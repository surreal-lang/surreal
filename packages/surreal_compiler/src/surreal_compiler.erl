%% @doc Surreal Compiler - High-level API for compiling Surreal source code.
%%
%% This module provides the main interface for compiling Surreal source files
%% to BEAM bytecode. It wraps the Rust-based NIF compiler and integrates with
%% the Erlang compiler toolchain.
%%
%% == Examples ==
%%
%% Compile a source string:
%% ```
%% {ok, Modules} = surreal_compiler:compile_source(<<"mod hello { pub fn greet() { :ok } }">>).
%% '''
%%
%% Compile a file:
%% ```
%% ok = surreal_compiler:compile_file("src/app.surreal").
%% '''
%%
%% Compile a project:
%% ```
%% ok = surreal_compiler:compile_project("my_project").
%% '''
-module(surreal_compiler).

-export([
    compile_source/1,
    compile_source/2,
    compile_file/1,
    compile_file/2,
    compile_project/1,
    compile_project/2,
    parse/1
]).

-type compile_option() ::
    {outdir, file:filename()} |
    return_binary |
    verbose |
    warnings_as_errors.

-type compile_result() ::
    ok |
    {ok, [{module(), binary()}]} |
    {error, term()}.

%% @doc Parse Surreal source and return module information.
%% Returns: {ok, [{ModuleName, [{FuncName, Arity}, ...]}]} | {error, Reason}
-spec parse(binary() | string()) -> {ok, term()} | {error, term()}.
parse(Source) when is_binary(Source) ->
    surreal_native:parse(Source);
parse(Source) when is_list(Source) ->
    surreal_native:parse(list_to_binary(Source)).

%% @doc Compile Surreal source code to BEAM modules.
%% Returns compiled module binaries.
-spec compile_source(binary() | string()) -> compile_result().
compile_source(Source) ->
    compile_source(Source, [return_binary]).

%% @doc Compile Surreal source code with options.
-spec compile_source(binary() | string(), [compile_option()]) -> compile_result().
compile_source(Source, Options) when is_list(Source) ->
    compile_source(list_to_binary(Source), Options);
compile_source(Source, Options) when is_binary(Source) ->
    case surreal_native:generate_core_ast(Source) of
        {ok, CoreAst} ->
            compile_core_ast(CoreAst, Options);
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Compile a Surreal source file.
-spec compile_file(file:filename()) -> compile_result().
compile_file(Filename) ->
    compile_file(Filename, []).

%% @doc Compile a Surreal source file with options.
-spec compile_file(file:filename(), [compile_option()]) -> compile_result().
compile_file(Filename, Options) ->
    case file:read_file(Filename) of
        {ok, Source} ->
            OutDir = proplists:get_value(outdir, Options, filename:dirname(Filename)),
            Options1 = [{outdir, OutDir} | proplists:delete(outdir, Options)],
            case compile_source(Source, Options1) of
                {ok, Modules} when is_list(Modules) ->
                    case proplists:get_bool(return_binary, Options) of
                        true ->
                            {ok, Modules};
                        false ->
                            write_beam_files(Modules, OutDir)
                    end;
                Other ->
                    Other
            end;
        {error, Reason} ->
            {error, {file_error, Filename, Reason}}
    end.

%% @doc Compile a Surreal project directory.
-spec compile_project(file:filename()) -> compile_result().
compile_project(ProjectDir) ->
    compile_project(ProjectDir, []).

%% @doc Compile a Surreal project directory with options.
-spec compile_project(file:filename(), [compile_option()]) -> compile_result().
compile_project(ProjectDir, Options) ->
    SrcDir = filename:join(ProjectDir, "src"),
    OutDir = proplists:get_value(outdir, Options, filename:join(ProjectDir, "ebin")),
    ok = filelib:ensure_dir(filename:join(OutDir, "dummy")),

    %% Find all .surreal files
    Pattern = filename:join(SrcDir, "**/*.surreal"),
    Files = filelib:wildcard(Pattern),

    case Files of
        [] ->
            {error, {no_source_files, SrcDir}};
        _ ->
            Options1 = [{outdir, OutDir} | proplists:delete(outdir, Options)],
            compile_files(Files, Options1, [])
    end.

%% Internal functions

compile_core_ast(CoreAst, Options) ->
    %% Compile Core Erlang AST to BEAM
    CompileOpts = [from_core, binary, return_errors, return_warnings],
    case compile:forms(CoreAst, CompileOpts) of
        {ok, ModuleName, Binary, Warnings} ->
            maybe_report_warnings(Warnings, Options),
            {ok, [{ModuleName, Binary}]};
        {ok, ModuleName, Binary} ->
            {ok, [{ModuleName, Binary}]};
        {error, Errors, Warnings} ->
            maybe_report_warnings(Warnings, Options),
            {error, {compile_errors, Errors}}
    end.

write_beam_files([], _OutDir) ->
    ok;
write_beam_files([{ModuleName, Binary} | Rest], OutDir) ->
    Filename = filename:join(OutDir, atom_to_list(ModuleName) ++ ".beam"),
    case file:write_file(Filename, Binary) of
        ok ->
            write_beam_files(Rest, OutDir);
        {error, Reason} ->
            {error, {write_error, Filename, Reason}}
    end.

compile_files([], _Options, Acc) ->
    {ok, lists:reverse(Acc)};
compile_files([File | Rest], Options, Acc) ->
    case compile_file(File, Options) of
        ok ->
            compile_files(Rest, Options, Acc);
        {ok, Modules} ->
            compile_files(Rest, Options, Modules ++ Acc);
        {error, _} = Error ->
            Error
    end.

maybe_report_warnings([], _Options) ->
    ok;
maybe_report_warnings(Warnings, Options) ->
    case proplists:get_bool(verbose, Options) of
        true ->
            io:format("Warnings: ~p~n", [Warnings]);
        false ->
            ok
    end,
    case proplists:get_bool(warnings_as_errors, Options) of
        true when Warnings =/= [] ->
            {error, {warnings, Warnings}};
        _ ->
            ok
    end.
