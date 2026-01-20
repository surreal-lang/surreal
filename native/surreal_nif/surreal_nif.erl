-module(surreal_nif).
-export([parse/1, generate_core_ast/1]).
-on_load(init/0).

%% NIF loading
init() ->
    SoName = find_nif_lib(),
    erlang:load_nif(SoName, 0).

find_nif_lib() ->
    %% Try various paths to find the NIF library
    Paths = [
        %% From priv dir if running as app
        priv_path(),
        %% From project root
        "native/surreal_nif/target/release/libsurreal_nif",
        "native/surreal_nif/target/debug/libsurreal_nif",
        %% Absolute path fallback
        filename:join([code:lib_dir(), "..", "..", "native", "surreal_nif", "target", "release", "libsurreal_nif"]),
        filename:join([code:lib_dir(), "..", "..", "native", "surreal_nif", "target", "debug", "libsurreal_nif"])
    ],
    find_first_existing(Paths).

priv_path() ->
    case code:priv_dir(surreal_nif) of
        {error, _} -> undefined;
        Dir -> filename:join(Dir, "libsurreal_nif")
    end.

find_first_existing([undefined | Rest]) ->
    find_first_existing(Rest);
find_first_existing([Path | Rest]) ->
    %% Check for .so or .dylib
    case filelib:is_file(Path ++ ".so") orelse filelib:is_file(Path ++ ".dylib") of
        true -> Path;
        false -> find_first_existing(Rest)
    end;
find_first_existing([]) ->
    %% Last resort - return a path and let load_nif fail with a clear error
    "libsurreal_nif".

%% Parse Surreal source code and return module information.
%% Returns: {ok, [{ModuleName, [{FuncName, Arity}, ...]}]} | {error, Reason}
-spec parse(binary() | string()) -> {ok, term()} | {error, term()}.
parse(_Source) ->
    erlang:nif_error(nif_not_loaded).

%% Generate Core Erlang AST from Surreal source code.
%% Returns: {ok, CoreErlangAST} | {error, Reason}
-spec generate_core_ast(binary() | string()) -> {ok, term()} | {error, term()}.
generate_core_ast(_Source) ->
    erlang:nif_error(nif_not_loaded).
