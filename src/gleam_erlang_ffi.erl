-module(gleam_erlang_ffi).
-export([
    atom_from_string/1, sleep/1, sleep_forever/0, demonitor/1, new_selector/0,
    link/1, insert_selector_handler/3, remove_selector_handler/2, select/1,
    select/2, trap_exits/1, map_selector/2, merge_selector/2, flush_messages/0,
    priv_directory/1, connect_node/1, register_process/2, unregister_process/1,
    process_named/1, identity/1, 'receive'/1, 'receive'/2, new_name/1,
    cast_down_message/1, cast_exit_reason/1
]).

-spec atom_from_string(binary()) -> {ok, atom()} | {error, nil}.
atom_from_string(S) ->
    try {ok, binary_to_existing_atom(S)}
    catch error:badarg -> {error, nil}
    end.

new_name(Prefix) ->
    Suffix = integer_to_binary(erlang:unique_integer([positive])),
    Name = <<Prefix/bits, "$"/utf8, Suffix/bits>>,
    binary_to_atom(Name).

sleep(Microseconds) ->
    timer:sleep(Microseconds),
    nil.

sleep_forever() ->
    timer:sleep(infinity),
    nil.

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
        {'DOWN', Ref, _, _, _} = Message when is_map_key(Ref, Handlers) ->
            Fn = maps:get(Ref, Handlers),
            {ok, Fn(cast_down_message(Message))};

        Msg when is_map_key({element(1, Msg), tuple_size(Msg)}, Handlers) ->
            Fn = maps:get({element(1, Msg), tuple_size(Msg)}, Handlers),
            {ok, Fn(Msg)};

        Msg when AnythingHandler =/= undefined ->
            {ok, AnythingHandler(Msg)}
    after Timeout ->
        {error, nil}
    end.

cast_exit_reason(normal) -> normal;
cast_exit_reason(killed) -> killed;
cast_exit_reason(Other) -> {abnormal, Other}.

cast_down_message({'DOWN', Ref, process, Pid, Reason}) ->
    {process_down, Ref, Pid, cast_exit_reason(Reason)};
cast_down_message({'DOWN', Ref, port, Pid, Reason}) ->
    {port_down, Ref, Pid, cast_exit_reason(Reason)}.
    

'receive'({subject, _Pid, Ref}) ->
    receive
        {Ref, Message} -> Message
    end;
'receive'({named_subject, Name}) ->
    receive
        {Name, Message} -> Message
    end.

'receive'({subject, _Pid, Ref}, Timeout) ->
    receive
        {Ref, Message} -> {ok, Message}
    after Timeout ->
        {error, nil}
    end;
'receive'({named_subject, Name}, Timeout) ->
    receive
        {Name, Message} -> {ok, Message}
    after Timeout ->
        {error, nil}
    end.

demonitor(Reference) ->
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
