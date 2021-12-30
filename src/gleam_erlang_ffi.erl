-module(gleam_erlang_ffi).
-export([atom_from_dynamic/1, atom_create_from_string/1, atom_to_string/1,
         rescue/1, atom_from_string/1, get_line/1, ensure_all_started/1,
         sleep/1, sleep_forever/0, read_file/1, write_file/2, delete_file/1,
         delete_directory/1, recursive_delete/1, list_directory/1, make_directory/1]).

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

read_file(Filename) ->
    case file:read_file(Filename) of
        {ok, Bitstring} -> {ok, Bitstring};
        {error, Reason} ->
            ensure_posix(Reason),
            {error, Reason}
    end.

write_file(Contents, Filename) ->
    case file:write_file(Filename, Contents) of
        ok -> {ok, nil};
        {error, Reason} ->
            ensure_posix(Reason),
            {error, Reason}
    end.

delete_file(Filename) ->
    case file:delete(Filename) of
        ok -> {ok, nil};
        {error, Reason} ->
            ensure_posix(Reason),
            {error, Reason}
    end.

make_directory(Dir) ->
    case file:make_dir(Dir) of
        ok ->
            {ok, nil};
        {error, Reason} ->
            ensure_posix(Reason),
            {error, Reason}
    end.

list_directory(Dir) ->
    case file:list_dir(Dir) of
        {ok, Filenames} ->
            {ok, [list_to_binary(Filename) || Filename <- Filenames]};
        {error, Reason} ->
            ensure_posix(Reason),
            {error, Reason}
    end.

delete_directory(Dir) ->
    case file:del_dir(Dir) of
        ok ->
            {ok, nil};
        {error, Reason} ->
            ensure_posix(Reason),
            {error, Reason}
    end.

recursive_delete(Dir) ->
    case file:del_dir_r(Dir) of
        ok ->
            {ok, nil};
        {error, Reason} ->
            ensure_posix(Reason),
            {error, Reason}
    end.

ensure_posix(Error) ->
    IsPosixCode = posix(Error),
    if IsPosixCode =:= true -> Error;
       true -> error(erlang_error)
    end.

posix(Error) ->
    lists:member(Error, [eacces, eagain, ebadf, ebadmsg, ebusy, edeadlk,
                         edeadlock, edquot, eexist, efault, efbig, eftype, eintr,
                         einval, eio, eisdir, eloop, emfile, emlink, emultihop,
                         enametoolong, enfile, enobufs, enodev, enolck, enolink,
                         enoent, enomem, enospc, enosr, enostr, enosys, enotblk,
                         enotdir, enotsup, enxio, eopnotsupp, eoverflow, eperm,
                         epipe, erange, erofs, espipe , esrch , estale, etxtbsy,
                         exdev]).
