-module(surreal_nif_downloader).

-export([ensure_nif/0, nif_path/0]).

-define(GITHUB_ORG, "surreal-lang").
-define(GITHUB_REPO, "surreal").
-define(NIF_VERSION, "0.1.0").

%% @doc Ensures the NIF library is available, downloading it if necessary.
%% Returns {ok, Path} with the path to the NIF library (without extension),
%% or {error, Reason} if the NIF cannot be obtained.
-spec ensure_nif() -> {ok, string()} | {error, term()}.
ensure_nif() ->
    Path = nif_path(),
    case nif_exists(Path) of
        true ->
            {ok, Path};
        false ->
            case download_nif(Path) of
                ok -> {ok, Path};
                {error, Reason} -> {error, Reason}
            end
    end.

%% @doc Returns the path where the NIF library should be stored (without extension).
-spec nif_path() -> string().
nif_path() ->
    PrivDir = priv_dir(),
    filename:join(PrivDir, "libsurreal_native").

%% @doc Get the priv directory, creating it if necessary.
-spec priv_dir() -> string().
priv_dir() ->
    Dir = case code:priv_dir(surreal_compiler) of
        {error, bad_name} ->
            %% Not installed as app, use local priv dir
            "priv";
        PrivPath ->
            PrivPath
    end,
    ok = filelib:ensure_dir(filename:join(Dir, "dummy")),
    Dir.

%% @doc Check if the NIF library exists.
-spec nif_exists(string()) -> boolean().
nif_exists(BasePath) ->
    Ext = nif_extension(),
    filelib:is_file(BasePath ++ Ext).

%% @doc Download the NIF library from GitHub releases.
-spec download_nif(string()) -> ok | {error, term()}.
download_nif(BasePath) ->
    Target = detect_target(),
    case Target of
        {error, Reason} ->
            {error, {unsupported_platform, Reason}};
        _ ->
            Url = build_release_url(Target),
            io:format("Downloading NIF from: ~s~n", [Url]),
            download_and_extract(Url, BasePath)
    end.

%% @doc Detect the current platform target triple.
-spec detect_target() -> string() | {error, term()}.
detect_target() ->
    Arch = detect_arch(),
    Os = detect_os(),
    case {Arch, Os} of
        {{error, _} = E, _} -> E;
        {_, {error, _} = E} -> E;
        {A, O} -> A ++ "-" ++ O
    end.

%% @doc Detect CPU architecture.
-spec detect_arch() -> string() | {error, term()}.
detect_arch() ->
    case erlang:system_info(system_architecture) of
        "x86_64" ++ _ -> "x86_64";
        "aarch64" ++ _ -> "aarch64";
        "arm64" ++ _ -> "aarch64";  % macOS reports arm64
        Arch ->
            %% Try parsing more complex arch strings
            case string:find(Arch, "x86_64") of
                nomatch ->
                    case string:find(Arch, "aarch64") of
                        nomatch ->
                            case string:find(Arch, "arm64") of
                                nomatch -> {error, {unknown_arch, Arch}};
                                _ -> "aarch64"
                            end;
                        _ -> "aarch64"
                    end;
                _ -> "x86_64"
            end
    end.

%% @doc Detect operating system.
-spec detect_os() -> string() | {error, term()}.
detect_os() ->
    case os:type() of
        {unix, darwin} -> "apple-darwin";
        {unix, linux} -> "unknown-linux-gnu";
        {win32, _} -> "pc-windows-msvc";
        Other -> {error, {unknown_os, Other}}
    end.

%% @doc Build the GitHub release URL for the NIF.
-spec build_release_url(string()) -> string().
build_release_url(Target) ->
    ArtifactName = "libsurreal_native-" ++ Target ++ ".tar.gz",
    "https://github.com/" ++ ?GITHUB_ORG ++ "/" ++ ?GITHUB_REPO ++
        "/releases/download/v" ++ ?NIF_VERSION ++ "/" ++ ArtifactName.

%% @doc Download and extract the NIF tarball.
-spec download_and_extract(string(), string()) -> ok | {error, term()}.
download_and_extract(Url, BasePath) ->
    %% Start inets and ssl if not already started
    ensure_apps_started(),

    case httpc:request(get, {Url, []}, [{autoredirect, true}], [{body_format, binary}]) of
        {ok, {{_, 200, _}, _, Body}} ->
            extract_tarball(Body, BasePath);
        {ok, {{_, 404, _}, _, _}} ->
            {error, {not_found, Url}};
        {ok, {{_, StatusCode, _}, _, _}} ->
            {error, {http_error, StatusCode}};
        {error, Reason} ->
            {error, {download_failed, Reason}}
    end.

%% @doc Ensure required applications are started.
-spec ensure_apps_started() -> ok.
ensure_apps_started() ->
    _ = application:ensure_all_started(inets),
    _ = application:ensure_all_started(ssl),
    ok.

%% @doc Extract the NIF from a gzipped tarball.
-spec extract_tarball(binary(), string()) -> ok | {error, term()}.
extract_tarball(GzippedData, BasePath) ->
    try
        %% Decompress gzip
        Data = zlib:gunzip(GzippedData),
        %% Extract tar
        {ok, Files} = erl_tar:extract({binary, Data}, [memory]),
        %% Find the NIF library file
        Ext = nif_extension(),
        NifFileName = "libsurreal_native" ++ Ext,
        case lists:keyfind(NifFileName, 1, Files) of
            {NifFileName, NifData} ->
                DestPath = BasePath ++ Ext,
                ok = file:write_file(DestPath, NifData),
                %% Make executable on Unix
                case os:type() of
                    {unix, _} -> file:change_mode(DestPath, 8#755);
                    _ -> ok
                end,
                io:format("NIF installed to: ~s~n", [DestPath]),
                ok;
            false ->
                {error, {nif_not_in_archive, Files}}
        end
    catch
        error:Reason ->
            {error, {extract_failed, Reason}}
    end.

%% @doc Get the file extension for the NIF library on this platform.
-spec nif_extension() -> string().
nif_extension() ->
    case os:type() of
        {unix, _} -> ".so";
        {win32, _} -> ".dll"
    end.
