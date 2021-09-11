-module(gleam_erlang_ffi).
-export([atom_from_dynamic/1, atom_create_from_string/1, atom_to_string/1,
         rescue/1, atom_from_string/1, get_line/1]).

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
