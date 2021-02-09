import gleam/dynamic
import gleam/beam/charlist
import gleam/should

pub fn to_string_test() {
  "Hello, from beam!"
  |> charlist.from_string()
  |> charlist.to_string()
  |> should.equal("Hello, from beam!")
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
