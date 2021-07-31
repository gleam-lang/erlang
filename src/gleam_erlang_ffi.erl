-module(gleam_erlang_ffi).
-export([atom_from_dynamic/1, atom_create_from_string/1, atom_to_string/1,
         atom_from_string/1]).

atom_from_string(S) ->
    try {ok, binary_to_existing_atom(S, utf8)}
    catch error:badarg -> {error, atom_not_loaded}
    end.

atom_create_from_string(S) ->
    binary_to_atom(S, utf8).

atom_to_string(S) ->
    atom_to_binary(S, utf8).

% TODO: Improve error type
atom_from_dynamic(Data) when is_atom(Data) -> 
    {ok, Data};
atom_from_dynamic(_) -> 
    {error, list_to_binary("expected an atom, got some other type")}.
