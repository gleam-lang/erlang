-module(gleam_erlang_test_ffi).
-export([assert_gleam_panic/1]).

assert_gleam_panic(F) ->
    try
        F(),
        erlang:error(expected_panic)
    catch
        error:#{gleam_error := _, message := M} -> M
    end.
