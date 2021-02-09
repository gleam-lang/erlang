import gleam/dynamic
import gleam/beam
import gleam/should

pub fn term_to_binary_test() {
  let term = dynamic.from(tuple(1, "2", <<"hello":utf8>>))

  term
  |> beam.term_to_binary()
  |> beam.binary_to_term()
  |> should.equal(Ok(term))

  term
  |> beam.term_to_binary()
  |> beam.unsafe_binary_to_term()
  |> should.equal(Ok(term))

  <<>>
  |> beam.unsafe_binary_to_term()
  |> should.equal(Error(Nil))
}
