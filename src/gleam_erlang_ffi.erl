-module(gleam_erlang_ffi).
-export([atom_from_dynamic/1, atom_create_from_string/1, atom_to_string/1,
         rescue/1, atom_from_string/1, get_line/1, ensure_all_started/1,
         sleep/1, sleep_forever/0, read_file/1, write_file/2, delete_file/1,
         delete_directory/1, recursive_delete/1, list_directory/1,
         make_directory/1, get_all_env/0, get_env/1, set_env/2, unset_env/1,
         os_family/0]).

-define(is_posix_error(Error),
    Error =:= eacces orelse Error =:= eagain orelse Error =:= ebadf orelse
    Error =:= ebadmsg orelse Error =:= ebusy orelse Error =:= edeadlk orelse
    Error =:= edeadlock orelse Error =:= edquot orelse Error =:= eexist orelse
    Error =:= efault orelse Error =:= efbig orelse Error =:= eftype orelse
    Error =:= eintr orelse Error =:= einval orelse Error =:= eio orelse
    Error =:= eisdir orelse Error =:= eloop orelse Error =:= emfile orelse
    Error =:= emlink orelse Error =:= emultihop orelse Error =:= enametoolong orelse
    Error =:= enfile orelse Error =:= enobufs orelse Error =:= enodev orelse
    Error =:= enolck orelse Error =:= enolink orelse Error =:= enoent orelse
    Error =:= enomem orelse Error =:= enospc orelse Error =:= enosr orelse
    Error =:= enostr orelse Error =:= enosys orelse Error =:= enotblk orelse
    Error =:= enotdir orelse Error =:= enotsup orelse Error =:= enxio orelse
    Error =:= eopnotsupp orelse Error =:= eoverflow orelse Error =:= eperm orelse
    Error =:= epipe orelse Error =:= erange orelse Error =:= erofs orelse
    Error =:= espipe orelse Error =:= esrch orelse Error =:= estale orelse
    Error =:= etxtbsy orelse Error =:= exdev
).

-spec atom_from_string(binary()) -> {ok, atom()} | {error, atom_not_loaded}.
atom_from_string(S) ->
    try {ok, binary_to_existing_atom(S, utf8)}
    catch error:badarg -> {error, atom_not_loaded}
    end.

-spec atom_create_from_string(binary()) -> atom().
atom_create_from_string(S) ->
    binary_to_atom(S, utf8).

-spec atom_to_string(atom()) -> binary().
atom_to_string(S) ->
    atom_to_binary(S, utf8).

% TODO: Improve error type
-spec atom_from_dynamic(any()) -> {ok, atom()} | {error, binary()}.
atom_from_dynamic(Data) when is_atom(Data) ->
    {ok, Data};
atom_from_dynamic(Data) ->
    {error, {decode_error, <<"Atom">>, gleam@dynamic:classify(Data)}}.

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

posix_result(Result) ->
    case Result of
        ok -> {ok, nil};
        {ok, Value} -> {ok, Value};
        {error, Reason} when ?is_posix_error(Reason) -> {error, Reason}
    end.

read_file(Filename) ->
    posix_result(file:read_file(Filename)).

write_file(Contents, Filename) ->
    posix_result(file:write_file(Filename, Contents)).

delete_file(Filename) ->
    posix_result(file:delete(Filename)).

make_directory(Dir) ->
    posix_result(file:make_dir(Dir)).

list_directory(Dir) ->
    case file:list_dir(Dir) of
        {ok, Filenames} ->
            {ok, [list_to_binary(Filename) || Filename <- Filenames]};
        {error, Reason} when ?is_posix_error(Reason) ->
            {error, Reason}
    end.

delete_directory(Dir) ->
    posix_result(file:del_dir(Dir)).

recursive_delete(Dir) ->
    posix_result(file:del_dir_r(Dir)).

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
            {other, atom_to_binary(Other)}
    end.
