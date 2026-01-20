-module(surreal_repl).

-export([start/0, start/1]).

-record(state, {
    bindings = #{} :: map(),      % name => value
    counter = 0 :: non_neg_integer(),
    prompt = "surreal> " :: string()
}).

%% Start the REPL
-spec start() -> ok.
start() ->
    start([]).

-spec start(list()) -> ok.
start(_Opts) ->
    io:format("Surreal REPL (BEAM native)~n"),
    io:format("Type :help for commands, :quit to exit~n~n"),
    loop(#state{}).

%% Main REPL loop
loop(State) ->
    case io:get_line(State#state.prompt) of
        eof ->
            io:format("~nGoodbye!~n"),
            ok;
        {error, Reason} ->
            io:format("Error reading input: ~p~n", [Reason]),
            ok;
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
    {continue, State#state{bindings = #{}}};

handle_input(":bindings", State) ->
    print_bindings(State#state.bindings),
    {continue, State};

handle_input(":b", State) ->
    print_bindings(State#state.bindings),
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
            case eval_expression(Expr, State) of
                {ok, Value} ->
                    Formatted = surreal_native:format_result(Value),
                    io:format("~s~n", [Formatted]),
                    NewBindings = maps:put(Name, Value, State#state.bindings),
                    {continue, State#state{bindings = NewBindings}};
                {error, Error} ->
                    io:format("Error: ~s~n", [format_error(Error)]),
                    {continue, State}
            end;
        _ ->
            io:format("Error: Expected 'let name = expression'~n"),
            {continue, State}
    end.

%% Evaluate an expression and print result
eval_expr(Input, State) ->
    case eval_expression(Input, State) of
        {ok, Value} ->
            Formatted = surreal_native:format_result(Value),
            io:format("~s~n", [Formatted]),
            {continue, State};
        {error, Error} ->
            io:format("Error: ~s~n", [format_error(Error)]),
            {continue, State}
    end.

%% Core expression evaluation
eval_expression(Expr, State) ->
    Counter = State#state.counter,
    ModName = list_to_atom("__repl_" ++ integer_to_list(Counter)),

    %% Wrap expression in a module with an __eval__ function
    Source = wrap_expression(Expr, ModName, State#state.bindings),

    case surreal_native:generate_core_ast(Source) of
        {ok, CoreAst} ->
            case compile:forms(CoreAst, [from_core, binary, return_errors]) of
                {ok, ModName, Binary} ->
                    %% Load and execute
                    code:purge(ModName),
                    case code:load_binary(ModName, "repl", Binary) of
                        {module, ModName} ->
                            %% Execute with bindings
                            try
                                Result = apply_with_bindings(ModName, State#state.bindings),
                                code:purge(ModName),
                                code:delete(ModName),
                                {ok, Result}
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

    %% Generate parameter list for __eval__ function
    %% Binding names are strings, so use them directly
    ParamList = case BindingNames of
        [] -> "";
        _ -> string:join([N ++ ": any" || N <- BindingNames], ", ")
    end,

    %% Build the module source
    Source = io_lib:format(
        "mod ~s { pub fn __eval__(~s) -> any { ~s } }",
        [ModNameStr, ParamList, Expr]
    ),
    iolist_to_binary(Source).

%% Apply the __eval__ function with bindings as arguments
apply_with_bindings(ModName, Bindings) ->
    %% Get args in same order as keys (maps iterate in insertion order in OTP 26+)
    Args = [maps:get(K, Bindings) || K <- maps:keys(Bindings)],
    erlang:apply(ModName, '__eval__', Args).

%% Show inferred type (placeholder - needs NIF support)
show_type(Expr, State) ->
    %% For now, just try to compile and show success/failure
    Counter = State#state.counter,
    ModName = list_to_atom("__repl_type_" ++ integer_to_list(Counter)),
    Source = wrap_expression(Expr, ModName, State#state.bindings),

    case surreal_native:generate_core_ast(Source) of
        {ok, _CoreAst} ->
            %% TODO: Add a NIF that returns the inferred type
            io:format("Type inference not yet implemented in NIF~n"),
            io:format("Expression compiles successfully~n");
        {error, Error} ->
            io:format("Type error: ~s~n", [format_error({parse_error, Error})])
    end.

%% Show Core Erlang output
show_core(Expr, State) ->
    Counter = State#state.counter,
    ModName = list_to_atom("__repl_core_" ++ integer_to_list(Counter)),
    Source = wrap_expression(Expr, ModName, State#state.bindings),

    case surreal_native:generate_core_ast(Source) of
        {ok, CoreAst} ->
            %% Pretty print the Core Erlang AST
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
