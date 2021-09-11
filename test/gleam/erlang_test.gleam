import gleam/dynamic
import gleam/erlang

pub fn term_to_binary_test() {
  let term = dynamic.from(#(1, "2", <<"hello":utf8>>))

  assert Ok(out) =
    term
    |> erlang.term_to_binary()
    |> erlang.binary_to_term()
  assert True = term == out

  assert Ok(out) =
    term
    |> erlang.term_to_binary()
    |> erlang.unsafe_binary_to_term()
  assert True = term == out

  assert Error(Nil) =
    <<>>
    |> erlang.unsafe_binary_to_term()
}
