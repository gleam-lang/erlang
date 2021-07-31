import gleam/erlang/atom
import gleam/dynamic
import gleam/should

pub fn from_string_test() {
  atom.create_from_string("this is an existing atom")

  "this is an existing atom"
  |> atom.from_string
  |> should.be_ok

  "this is not an atom we have seen before"
  |> atom.from_string
  |> should.equal(Error(atom.AtomNotLoaded))
}

pub fn create_from_string_test() {
  "ok"
  |> atom.create_from_string
  |> Ok
  |> should.equal(atom.from_string("ok"))

  "expect"
  |> atom.create_from_string
  |> Ok
  |> should.equal(atom.from_string("expect"))

  "this is another atom we have not seen before"
  |> atom.create_from_string
  |> Ok
  |> should.equal(atom.from_string(
    "this is another atom we have not seen before",
  ))
}

pub fn to_string_test() {
  "ok"
  |> atom.create_from_string
  |> atom.to_string
  |> should.equal("ok")

  "expect"
  |> atom.create_from_string
  |> atom.to_string
  |> should.equal("expect")
}

pub fn from_dynamic_test() {
  ""
  |> atom.create_from_string
  |> dynamic.from
  |> atom.from_dynamic
  |> should.equal(Ok(atom.create_from_string("")))

  "ok"
  |> atom.create_from_string
  |> dynamic.from
  |> atom.from_dynamic
  |> should.equal(Ok(atom.create_from_string("ok")))

  1
  |> dynamic.from
  |> atom.from_dynamic
  |> should.be_error

  []
  |> dynamic.from
  |> atom.from_dynamic
  |> should.be_error
}
