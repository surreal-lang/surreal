-module(surreal_shell).

%% Shell interface for user_drv/group
%% Implements the same interface as Erlang's shell module

-export([start/0, start/1]).

%% Called by group:start - spawns the actual shell process
start() ->
    start(init).

start(_Args) ->
    %% Spawn a separate process for the shell loop
    %% The group process is the group leader, we can't do I/O from it directly
    spawn(fun() -> shell_init() end).

shell_init() ->
    io:format("Surreal REPL (BEAM native)~n"),
    io:format("Type :help for commands, :quit to exit~n~n"),
    loop(#{bindings => #{}, counter => 0}).

loop(State) ->
    case io:get_line("surreal> ") of
        eof ->
            io:format("~nGoodbye!~n"),
            ok;
        {error, terminated} ->
            %% Shell is being terminated
            ok;
        {error, Reason} ->
            io:format("Error reading input: ~p~n", [Reason]),
            loop(State);
        Line ->
            Input = string:trim(Line),
            case handle_input(Input, State) of
                {continue, NewState} ->
                    loop(NewState);
                quit ->
                    io:format("Goodbye!~n"),
                    ok
            end
    end.

%% Handle user input
handle_input("", State) ->
    {continue, State};

handle_input(":quit", _State) ->
    quit;

handle_input(":q", _State) ->
    quit;

handle_input(":help", State) ->
    print_help(),
    {continue, State};

handle_input(":h", State) ->
    print_help(),
    {continue, State};

handle_input(":clear", State) ->
    io:format("Bindings cleared.~n"),
    {continue, State#{bindings => #{}}};

handle_input(":bindings", State) ->
    print_bindings(maps:get(bindings, State, #{})),
    {continue, State};

handle_input(":b", State) ->
    print_bindings(maps:get(bindings, State, #{})),
    {continue, State};

handle_input(":type " ++ Expr, State) ->
    show_type(Expr, State),
    {continue, State};

handle_input(":core " ++ Expr, State) ->
    show_core(Expr, State),
    {continue, State};

handle_input(":" ++ Unknown, State) ->
    io:format("Unknown command: :~s~n", [Unknown]),
    io:format("Type :help for available commands.~n"),
    {continue, State};

handle_input("let " ++ Rest, State) ->
    handle_let(Rest, State);

handle_input("mod " ++ _ = Input, State) ->
    %% Module definition - compile and load directly
    define_module(Input, State);

handle_input(Input, State) ->
    eval_expr(Input, State).

%% Print help
print_help() ->
    io:format("Commands:~n"),
    io:format("  :help, :h       Show this help message~n"),
    io:format("  :quit, :q       Exit the shell~n"),
    io:format("  :clear          Clear all bindings~n"),
    io:format("  :bindings, :b   Show current bindings~n"),
    io:format("  :type <expr>    Show inferred type of expression~n"),
    io:format("  :core <expr>    Show generated Core Erlang~n"),
    io:format("~n"),
    io:format("Enter Surreal expressions to evaluate them.~n"),
    io:format("Use 'let x = expr' to create bindings.~n").

%% Print bindings
print_bindings(Bindings) when map_size(Bindings) == 0 ->
    io:format("No bindings.~n");
print_bindings(Bindings) ->
    maps:foreach(fun(Name, Value) ->
        Formatted = surreal_native:format_result(Value),
        io:format("  ~s = ~s~n", [Name, Formatted])
    end, Bindings).

%% Handle let binding
handle_let(Rest, State) ->
    case string:split(Rest, "=", leading) of
        [NamePart, ExprPart] ->
            Name = string:trim(NamePart),
            Expr = string:trim(ExprPart),
            Bindings = maps:get(bindings, State, #{}),
            case eval_expression(Expr, Bindings, State) of
                {ok, Value, NewState} ->
                    Formatted = surreal_native:format_result(Value),
                    io:format("~s~n", [Formatted]),
                    NewBindings = maps:put(Name, Value, Bindings),
                    {continue, NewState#{bindings => NewBindings}};
                {error, Error} ->
                    io:format("Error: ~s~n", [format_error(Error)]),
                    {continue, State}
            end;
        _ ->
            io:format("Error: Expected 'let name = expression'~n"),
            {continue, State}
    end.

%% Define a module - compile and load it
%% Transforms "mod foo { ... }" to "mod surreal::foo { ... }" for Surreal namespace
define_module(Source, State) ->
    PrefixedSource = prefix_module_name(Source),
    case surreal_native:generate_core_ast(list_to_binary(PrefixedSource)) of
        {ok, CoreAst} ->
            %% Extract exports from CoreAst: {c_module, _, _, Exports, _, _}
            Exports = try element(4, CoreAst) catch _:_ -> [] end,
            case compile:forms(CoreAst, [from_core, binary, return_errors]) of
                {ok, CompiledModName, Binary} ->
                    code:purge(CompiledModName),
                    case code:load_binary(CompiledModName, "repl", Binary) of
                        {module, CompiledModName} ->
                            %% Format exports from CoreAst
                            FunExports = [extract_export(E) || E <- Exports],
                            io:format("{:module, ~p, [~s]}~n",
                                [CompiledModName, format_exports(FunExports)]),
                            {continue, State};
                        {error, What} ->
                            io:format("Error loading module: ~p~n", [What]),
                            {continue, State}
                    end;
                {error, Errors, _Warnings} ->
                    io:format("Compile error: ~p~n", [Errors]),
                    {continue, State}
            end;
        {error, Error} ->
            io:format("Error: ~s~n", [format_error({parse_error, Error})]),
            {continue, State}
    end.

%% Prefix module name with surreal:: for REPL-defined modules
%% "mod foo { ... }" -> "mod surreal::foo { ... }"
prefix_module_name("mod " ++ Rest) ->
    %% Find the module name (ends at whitespace or {)
    case string:split(Rest, " ", leading) of
        [Name, Remaining] ->
            "mod surreal::" ++ Name ++ " " ++ Remaining;
        [NameAndBody] ->
            case string:split(NameAndBody, "{", leading) of
                [Name, Body] ->
                    "mod surreal::" ++ Name ++ "{" ++ Body;
                _ ->
                    "mod surreal::" ++ NameAndBody
            end
    end;
prefix_module_name(Other) ->
    Other.

%% Extract {Name, Arity} from c_var export
extract_export({c_var, _, {Name, Arity}}) -> {Name, Arity};
extract_export(_) -> {unknown, 0}.

format_exports([]) -> "";
format_exports(Exports) ->
    string:join([io_lib:format("~p/~p", [F, A]) || {F, A} <- Exports], ", ").

%% Evaluate an expression and print result
eval_expr(Input, State) ->
    Bindings = maps:get(bindings, State, #{}),
    case eval_expression(Input, Bindings, State) of
        {ok, Value, NewState} ->
            Formatted = surreal_native:format_result(Value),
            io:format("~s~n", [Formatted]),
            {continue, NewState};
        {error, Error} ->
            io:format("Error: ~s~n", [format_error(Error)]),
            {continue, State}
    end.

%% Core expression evaluation
eval_expression(Expr, Bindings, State) ->
    Counter = maps:get(counter, State, 0),
    ModName = list_to_atom("__repl_" ++ integer_to_list(Counter)),
    NewState = State#{counter => Counter + 1},

    %% Wrap expression in a module with an __eval__ function
    Source = wrap_expression(Expr, ModName, Bindings),

    case surreal_native:generate_core_ast(Source) of
        {ok, CoreAst} ->
            case compile:forms(CoreAst, [from_core, binary, return_errors]) of
                {ok, ModName, Binary} ->
                    code:purge(ModName),
                    case code:load_binary(ModName, "repl", Binary) of
                        {module, ModName} ->
                            try
                                Result = apply_with_bindings(ModName, Bindings),
                                code:purge(ModName),
                                code:delete(ModName),
                                {ok, Result, NewState}
                            catch
                                Class:Reason:Stack ->
                                    code:purge(ModName),
                                    code:delete(ModName),
                                    {error, {runtime, Class, Reason, Stack}}
                            end;
                        {error, What} ->
                            {error, {load_failed, What}}
                    end;
                {error, Errors, _Warnings} ->
                    {error, {compile_failed, Errors}}
            end;
        {error, ParseError} ->
            {error, {parse_error, ParseError}}
    end.

%% Wrap an expression in a module
wrap_expression(Expr, ModName, Bindings) ->
    ModNameStr = atom_to_list(ModName),
    BindingNames = maps:keys(Bindings),

    ParamList = case BindingNames of
        [] -> "";
        _ -> string:join([N ++ ": any" || N <- BindingNames], ", ")
    end,

    Source = io_lib:format(
        "mod ~s { pub fn __eval__(~s) -> any { ~s } }",
        [ModNameStr, ParamList, Expr]
    ),
    iolist_to_binary(Source).

%% Apply the __eval__ function with bindings as arguments
apply_with_bindings(ModName, Bindings) ->
    Args = [maps:get(K, Bindings) || K <- maps:keys(Bindings)],
    erlang:apply(ModName, '__eval__', Args).

%% Show inferred type
show_type(Expr, State) ->
    Counter = maps:get(counter, State, 0),
    ModName = list_to_atom("__repl_type_" ++ integer_to_list(Counter)),
    Bindings = maps:get(bindings, State, #{}),
    Source = wrap_expression(Expr, ModName, Bindings),

    case surreal_native:generate_core_ast(Source) of
        {ok, _CoreAst} ->
            io:format("Type inference not yet implemented in NIF~n"),
            io:format("Expression compiles successfully~n");
        {error, Error} ->
            io:format("Type error: ~s~n", [format_error({parse_error, Error})])
    end.

%% Show Core Erlang output
show_core(Expr, State) ->
    Counter = maps:get(counter, State, 0),
    ModName = list_to_atom("__repl_core_" ++ integer_to_list(Counter)),
    Bindings = maps:get(bindings, State, #{}),
    Source = wrap_expression(Expr, ModName, Bindings),

    case surreal_native:generate_core_ast(Source) of
        {ok, CoreAst} ->
            io:format("~p~n", [CoreAst]);
        {error, Error} ->
            io:format("Error: ~s~n", [format_error({parse_error, Error})])
    end.

%% Format errors for display
format_error({parse_error, Errors}) when is_list(Errors) ->
    string:join([binary_to_list(E) || E <- Errors], "\n");
format_error({parse_error, Error}) when is_binary(Error) ->
    binary_to_list(Error);
format_error({compile_failed, Errors}) ->
    io_lib:format("Compilation failed: ~p", [Errors]);
format_error({load_failed, What}) ->
    io_lib:format("Failed to load module: ~p", [What]);
format_error({runtime, Class, Reason, Stack}) ->
    io_lib:format("~p:~p~n~p", [Class, Reason, Stack]);
format_error(Other) ->
    io_lib:format("~p", [Other]).
