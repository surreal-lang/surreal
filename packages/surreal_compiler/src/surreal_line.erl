-module(surreal_line).

%% API
-export([start/0, stop/0, get_line/1]).

-record(state, {
    history = [] :: [string()],
    raw_mode = false :: boolean()
}).

%% Singleton state - stored in process dictionary
-define(STATE_KEY, surreal_line_state).

%% Start the line editor (initializes state)
-spec start() -> ok.
start() ->
    case get(?STATE_KEY) of
        undefined ->
            %% Try to enable raw mode
            RawMode = enable_raw_mode(),
            put(?STATE_KEY, #state{raw_mode = RawMode}),
            ok;
        _ ->
            ok
    end.

%% Stop the line editor
-spec stop() -> ok.
stop() ->
    case get(?STATE_KEY) of
        undefined -> ok;
        #state{raw_mode = true} ->
            disable_raw_mode(),
            erase(?STATE_KEY),
            ok;
        _ ->
            erase(?STATE_KEY),
            ok
    end.

%% Get a line of input with the given prompt
-spec get_line(string()) -> string() | eof | {error, term()}.
get_line(Prompt) ->
    State = case get(?STATE_KEY) of
        undefined -> #state{};
        S -> S
    end,
    case State#state.raw_mode of
        true ->
            get_line_raw(Prompt, State);
        false ->
            get_line_cooked(Prompt)
    end.

%% Enable raw terminal mode
enable_raw_mode() ->
    case os:type() of
        {unix, darwin} ->
            %% macOS: use -f flag or redirect from /dev/tty
            os:cmd("stty -f /dev/tty -echo -icanon min 1 time 0 2>/dev/null"),
            true;
        {unix, _} ->
            %% Linux: use -F flag
            os:cmd("stty -F /dev/tty -echo -icanon min 1 time 0 2>/dev/null"),
            true;
        _ ->
            false
    end.

%% Disable raw terminal mode
disable_raw_mode() ->
    case os:type() of
        {unix, darwin} ->
            os:cmd("stty -f /dev/tty echo icanon 2>/dev/null"),
            ok;
        {unix, _} ->
            os:cmd("stty -F /dev/tty echo icanon 2>/dev/null"),
            ok;
        _ ->
            ok
    end.

%% Cooked mode fallback - just use io:get_line
get_line_cooked(Prompt) ->
    case io:get_line(Prompt) of
        eof -> eof;
        {error, _} = E -> E;
        Line -> string:trim(Line, trailing, "\n")
    end.

%% Raw mode - character by character with editing
get_line_raw(Prompt, State) ->
    io:format("~s", [Prompt]),
    case read_line_loop("", State#state.history, 0) of
        {eof, _NewState} ->
            eof;
        {Line, NewHistory} ->
            put(?STATE_KEY, State#state{history = NewHistory}),
            Line
    end.

%% Main input loop
read_line_loop(Current, History, HistPos) ->
    case io:get_chars('', 1) of
        eof ->
            io:format("~n"),
            {eof, History};
        {error, _} = E ->
            io:format("~n"),
            {E, History};
        Data ->
            handle_char(Data, Current, History, HistPos)
    end.

%% Handle a single character or escape sequence
handle_char(<<"\n">>, Current, History, _HistPos) ->
    io:format("~n"),
    {Current, add_history(Current, History)};
handle_char(<<"\r">>, Current, History, _HistPos) ->
    io:format("~n"),
    {Current, add_history(Current, History)};
handle_char("\n", Current, History, _HistPos) ->
    io:format("~n"),
    {Current, add_history(Current, History)};
handle_char("\r", Current, History, _HistPos) ->
    io:format("~n"),
    {Current, add_history(Current, History)};

%% Ctrl+C
handle_char(<<3>>, _Current, History, _HistPos) ->
    io:format("^C~n"),
    {"", History};
handle_char([3], _Current, History, _HistPos) ->
    io:format("^C~n"),
    {"", History};

%% Ctrl+D on empty line = EOF
handle_char(<<4>>, "", History, _HistPos) ->
    {eof, History};
handle_char([4], "", History, _HistPos) ->
    {eof, History};
handle_char(<<4>>, Current, History, HistPos) ->
    read_line_loop(Current, History, HistPos);
handle_char([4], Current, History, HistPos) ->
    read_line_loop(Current, History, HistPos);

%% Backspace (127 or 8)
handle_char(<<127>>, Current, History, HistPos) ->
    do_backspace(Current, History, HistPos);
handle_char(<<8>>, Current, History, HistPos) ->
    do_backspace(Current, History, HistPos);
handle_char([127], Current, History, HistPos) ->
    do_backspace(Current, History, HistPos);
handle_char([8], Current, History, HistPos) ->
    do_backspace(Current, History, HistPos);

%% Escape - start of escape sequence
handle_char(<<27>>, Current, History, HistPos) ->
    read_escape_sequence(Current, History, HistPos);
handle_char([27], Current, History, HistPos) ->
    read_escape_sequence(Current, History, HistPos);

%% Regular printable character
handle_char(<<C>>, Current, History, HistPos) when C >= 32, C < 127 ->
    io:format("~c", [C]),
    read_line_loop(Current ++ [C], History, HistPos);
handle_char([C], Current, History, HistPos) when C >= 32, C < 127 ->
    io:format("~c", [C]),
    read_line_loop(Current ++ [C], History, HistPos);

%% Unknown - ignore and continue
handle_char(_, Current, History, HistPos) ->
    read_line_loop(Current, History, HistPos).

%% Read escape sequence (after ESC)
read_escape_sequence(Current, History, HistPos) ->
    case io:get_chars('', 1) of
        eof -> {eof, History};
        {error, _} -> {Current, History};
        <<"[">> -> read_csi_sequence(Current, History, HistPos);
        "[" -> read_csi_sequence(Current, History, HistPos);
        _ -> read_line_loop(Current, History, HistPos)
    end.

%% Read CSI sequence (after ESC [)
read_csi_sequence(Current, History, HistPos) ->
    case io:get_chars('', 1) of
        eof -> {eof, History};
        {error, _} -> {Current, History};
        <<"A">> -> handle_up(Current, History, HistPos);
        "A" -> handle_up(Current, History, HistPos);
        <<"B">> -> handle_down(Current, History, HistPos);
        "B" -> handle_down(Current, History, HistPos);
        <<"C">> -> read_line_loop(Current, History, HistPos);  % Right - ignore
        "C" -> read_line_loop(Current, History, HistPos);
        <<"D">> -> read_line_loop(Current, History, HistPos);  % Left - ignore
        "D" -> read_line_loop(Current, History, HistPos);
        _ -> read_line_loop(Current, History, HistPos)
    end.

%% Up arrow - previous history
handle_up(Current, History, HistPos) ->
    case get_history(History, HistPos + 1) of
        undefined ->
            read_line_loop(Current, History, HistPos);
        Entry ->
            clear_line(Current),
            io:format("~s", [Entry]),
            read_line_loop(Entry, History, HistPos + 1)
    end.

%% Down arrow - next history
handle_down(Current, History, HistPos) ->
    case HistPos of
        0 ->
            read_line_loop(Current, History, HistPos);
        1 ->
            clear_line(Current),
            read_line_loop("", History, 0);
        _ ->
            case get_history(History, HistPos - 1) of
                undefined ->
                    read_line_loop(Current, History, HistPos);
                Entry ->
                    clear_line(Current),
                    io:format("~s", [Entry]),
                    read_line_loop(Entry, History, HistPos - 1)
            end
    end.

%% Handle backspace
do_backspace("", History, HistPos) ->
    read_line_loop("", History, HistPos);
do_backspace(Current, History, HistPos) ->
    NewCurrent = string:slice(Current, 0, string:length(Current) - 1),
    io:format("\b \b"),
    read_line_loop(NewCurrent, History, HistPos).

%% Clear current line content (for history navigation)
clear_line(Current) ->
    Len = string:length(Current),
    Backs = lists:duplicate(Len, $\b),
    Spaces = lists:duplicate(Len, $ ),
    io:format("~s~s~s", [Backs, Spaces, Backs]).

%% Get history entry (1-indexed, 1 = most recent)
get_history(History, Pos) when Pos > 0, Pos =< length(History) ->
    lists:nth(Pos, History);
get_history(_, _) ->
    undefined.

%% Add to history (skip empty and consecutive duplicates)
add_history("", History) -> History;
add_history(Line, [Line | _] = History) -> History;
add_history(Line, History) -> [Line | History].
