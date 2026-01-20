-module(surreal).

%% Escript entry point
-export([main/1]).

%% API exports
-export([compile/1, compile_to_ast/1, format/1]).

%% Escript entry point
main([]) ->
    surreal_repl:start();
main(["repl"]) ->
    surreal_repl:start();
main(["compile", File]) ->
    case compile(File) of
        {ok, ModName, _Binary} ->
            io:format("Compiled: ~s~n", [ModName]);
        {error, Error} ->
            io:format("Error: ~p~n", [Error]),
            halt(1)
    end;
main(["help"]) ->
    print_usage();
main(["--help"]) ->
    print_usage();
main(["-h"]) ->
    print_usage();
main(_Args) ->
    print_usage(),
    halt(1).

print_usage() ->
    io:format("Surreal Compiler~n"),
    io:format("~n"),
    io:format("Usage:~n"),
    io:format("  surreal              Start the interactive REPL~n"),
    io:format("  surreal repl         Start the interactive REPL~n"),
    io:format("  surreal compile <file>  Compile a .surreal file~n"),
    io:format("  surreal help         Show this help message~n").

%% Compile Surreal source code from a file
-spec compile(string()) -> {ok, atom(), binary()} | {error, term()}.
compile(Filename) ->
    case file:read_file(Filename) of
        {ok, Source} ->
            compile_source(Source);
        {error, Reason} ->
            {error, {file_error, Reason}}
    end.

%% Compile Surreal source code to Core Erlang AST
-spec compile_to_ast(binary() | string()) -> {ok, term()} | {error, term()}.
compile_to_ast(Source) when is_list(Source) ->
    compile_to_ast(list_to_binary(Source));
compile_to_ast(Source) when is_binary(Source) ->
    surreal_native:generate_core_ast(Source).

%% Format an Erlang term as Surreal syntax
-spec format(term()) -> string().
format(Term) ->
    surreal_native:format_result(Term).

%% Internal: compile source to BEAM binary
compile_source(Source) ->
    case surreal_native:generate_core_ast(Source) of
        {ok, CoreAst} ->
            case compile:forms(CoreAst, [from_core, binary, return_errors]) of
                {ok, ModName, Binary} ->
                    {ok, ModName, Binary};
                {error, Errors, _Warnings} ->
                    {error, {compile_failed, Errors}}
            end;
        {error, Error} ->
            {error, {parse_error, Error}}
    end.
