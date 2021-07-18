import gleam/dynamic
import gleam/erlang
import gleam/should

pub fn term_to_binary_test() {
  let term = dynamic.from(#(1, "2", <<"hello":utf8>>))

  term
  |> erlang.term_to_binary()
  |> erlang.binary_to_term()
  |> should.equal(Ok(term))

  term
  |> erlang.term_to_binary()
  |> erlang.unsafe_binary_to_term()
  |> should.equal(Ok(term))

  <<>>
  |> erlang.unsafe_binary_to_term()
  |> should.equal(Error(Nil))
}
