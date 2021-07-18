import gleam/dynamic
import gleam/erlang/charlist
import gleam/should

pub fn to_string_test() {
  "Hello, from erlang!"
  |> charlist.from_string()
  |> charlist.to_string()
  |> should.equal("Hello, from erlang!")
}

pub fn empty_string_test() {
  []
  |> dynamic.from
  |> dynamic.unsafe_coerce
  |> charlist.to_string
  |> should.equal("")

  ""
  |> charlist.from_string()
  |> dynamic.from
  |> dynamic.unsafe_coerce
  |> should.equal([])
}
