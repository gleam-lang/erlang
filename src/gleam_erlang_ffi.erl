-module(gleam_erlang_ffi).
-export([
    atom_from_dynamic/1, rescue/1, atom_from_string/1, get_line/1,
    ensure_all_started/1, sleep/1, os_family/0, sleep_forever/0,
    get_all_env/0, get_env/1, set_env/2, unset_env/1, demonitor/1,
    new_selector/0, link/1, insert_selector_handler/3,
    remove_selector_handler/2, select/1, select/2, trap_exits/1,
    map_selector/2, merge_selector/2, flush_messages/0,
    priv_directory/1, connect_node/1, register_process/2, unregister_process/1,
    process_named/1, identity/1, pid_from_dynamic/1, reference_from_dynamic/1,
    port_from_dynamic/1
]).

-spec atom_from_string(binary()) -> {ok, atom()} | {error, atom_not_loaded}.
atom_from_string(S) ->
    try {ok, binary_to_existing_atom(S)}
    catch error:badarg -> {error, atom_not_loaded}
    end.

atom_from_dynamic(Data) when is_atom(Data) ->
    {ok, Data};
atom_from_dynamic(Data) ->
    {error, [{decode_error, <<"Atom">>, gleam@dynamic:classify(Data), []}]}.

pid_from_dynamic(Data) when is_pid(Data) ->
    {ok, Data};
pid_from_dynamic(Data) ->
    {error, [{decode_error, <<"Pid">>, gleam@dynamic:classify(Data), []}]}.

reference_from_dynamic(Data) when is_reference(Data) ->
    {ok, Data};
reference_from_dynamic(Data) ->
    {error, [{decode_error, <<"Reference">>, gleam@dynamic:classify(Data), []}]}.

port_from_dynamic(Data) when is_port(Data) ->
    {ok, Data};
port_from_dynamic(Data) ->
    {error, [{decode_error, <<"Port">>, gleam@dynamic:classify(Data), []}]}.

-spec get_line(io:prompt()) -> {ok, unicode:unicode_binary()} | {error, eof | no_data}.
get_line(Prompt) ->
    case io:get_line(Prompt) of
        eof -> {error, eof};
        {error, _} -> {error, no_data};
        Data when is_binary(Data) -> {ok, Data};
        Data when is_list(Data) -> {ok, unicode:characters_to_binary(Data)}
    end.

rescue(F) ->
    try {ok, F()}
    catch
        throw:X -> {error, {thrown, X}};
        error:X -> {error, {errored, X}};
        exit:X -> {error, {exited, X}}
    end.

ensure_all_started(Application) ->
    case application:ensure_all_started(Application) of
        {ok, _} = Ok -> Ok;

        {error, {ProblemApp, {"no such file or directory", _}}} ->
            {error, {unknown_application, ProblemApp}}
    end.

sleep(Microseconds) ->
    timer:sleep(Microseconds),
    nil.

sleep_forever() ->
    timer:sleep(infinity),
    nil.

get_all_env() ->
    BinVars = lists:map(fun(VarString) ->
        [VarName, VarVal] = string:split(VarString, "="),
        {list_to_binary(VarName), list_to_binary(VarVal)}
    end, os:getenv()),
    maps:from_list(BinVars).

get_env(Name) ->
    case os:getenv(binary_to_list(Name)) of
        false -> {error, nil};
        Value -> {ok, list_to_binary(Value)}
    end.

set_env(Name, Value) ->
    os:putenv(binary_to_list(Name), binary_to_list(Value)),
    nil.

unset_env(Name) ->
    os:unsetenv(binary_to_list(Name)),
    nil.

os_family() ->
    case os:type() of
        {win32, nt} ->
            windows_nt;
        {unix, linux} ->
            linux;
        {unix, darwin} ->
            darwin;
        {unix, freebsd} ->
            free_bsd;
        {_, Other} ->
            {other, atom_to_binary(Other, utf8)}
    end.

new_selector() ->
    {selector, #{}}.

map_selector({selector, Handlers}, Fn) ->
    MappedHandlers = maps:map(fun(_Tag, Handler) ->
        fun(Message) -> Fn(Handler(Message)) end
    end, Handlers),
    {selector, MappedHandlers}.

merge_selector({selector, HandlersA}, {selector, HandlersB}) ->
    {selector, maps:merge(HandlersA, HandlersB)}.

insert_selector_handler({selector, Handlers}, Tag, Fn) ->
    {selector, Handlers#{Tag => Fn}}.

remove_selector_handler({selector, Handlers}, Tag) ->
    {selector, maps:remove(Tag, Handlers)}.

select(Selector) ->
    {ok, Message} = select(Selector, infinity),
    Message.

select({selector, Handlers}, Timeout) ->
    AnythingHandler = maps:get(anything, Handlers, undefined),
    receive
        % Monitored process down messages.
        % This is special cased so we can selectively receive based on the
        % reference as well as the record tag.
        {'DOWN', Ref, process, Pid, Reason} when is_map_key(Ref, Handlers) ->
            Fn = maps:get(Ref, Handlers),
            {ok, Fn({process_down, Pid, Reason})};

        Msg when is_map_key({element(1, Msg), tuple_size(Msg)}, Handlers) ->
            Fn = maps:get({element(1, Msg), tuple_size(Msg)}, Handlers),
            {ok, Fn(Msg)};

        Msg when AnythingHandler =/= undefined ->
            {ok, AnythingHandler(Msg)}
    after Timeout ->
        {error, nil}
    end.

demonitor({_, Reference}) ->
    erlang:demonitor(Reference, [flush]).

link(Pid) ->
    try
        erlang:link(Pid)
    catch
        error:_ -> false
    end.

trap_exits(ShouldTrap) ->
    erlang:process_flag(trap_exit, ShouldTrap),
    nil.

flush_messages() ->
    receive _Message -> flush_messages()
    after 0 -> nil
    end.

priv_directory(Name) ->
    try erlang:binary_to_existing_atom(Name) of
        Atom ->
            case code:priv_dir(Atom) of
                {error, _} -> {error, nil};
                Path -> {ok, unicode:characters_to_binary(Path)}
            end
    catch
        error:badarg -> {error, nil}
    end.

connect_node(Node) ->
    case net_kernel:connect_node(Node) of
        true -> {ok, Node};
        false -> {error, failed_to_connect};
        ignored -> {error, local_node_is_not_alive}
    end.

register_process(Pid, Name) ->
    try
        true = erlang:register(Name, Pid),
        {ok, nil}
    catch
        error:badarg -> {error, nil}
    end.

unregister_process(Name) ->
    try
        true = erlang:unregister(Name),
        {ok, nil}
    catch
        error:badarg -> {error, nil}
    end.

process_named(Name) ->
    case erlang:whereis(Name) of
        Pid when is_pid(Pid) -> {ok, Pid};
        _ -> {error, nil}
    end.

identity(X) ->
    X.
