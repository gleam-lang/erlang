import gleam/erlang/atom
import gleam/dynamic.{DecodeError}

pub fn from_string_test() {
  let assert Ok(_) = atom.from_string("ok")
  let assert Error(atom.AtomNotLoaded) =
    atom.from_string("the vm does not have an atom with this content")
}

pub fn create_from_string_test() {
  let result =
    "ok"
    |> atom.create_from_string
    |> Ok
  let assert True = result == atom.from_string("ok")

  let result =
    "expect"
    |> atom.create_from_string
    |> Ok
  let assert True = result == atom.from_string("expect")

  let result =
    "this is another atom we have not seen before"
    |> atom.create_from_string
    |> Ok
  let assert True =
    result == atom.from_string("this is another atom we have not seen before")
}

pub fn to_string_test() {
  let assert "ok" = atom.to_string(atom.create_from_string("ok"))

  let assert "expect" = atom.to_string(atom.create_from_string("expect"))
}

pub fn from_dynamic_test() {
  let result =
    ""
    |> atom.create_from_string
    |> dynamic.from
    |> atom.from_dynamic
  let assert True = result == Ok(atom.create_from_string(""))

  let result =
    "ok"
    |> atom.create_from_string
    |> dynamic.from
    |> atom.from_dynamic
  let assert True = result == Ok(atom.create_from_string("ok"))

  let assert Error([DecodeError(expected: "Atom", found: "Int", path: [])]) =
    1
    |> dynamic.from
    |> atom.from_dynamic

  let assert Error([DecodeError(expected: "Atom", found: "List", path: [])]) =
    []
    |> dynamic.from
    |> atom.from_dynamic
}
