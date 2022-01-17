import gleam/erlang/atom
import gleam/dynamic.{DecodeError}

pub fn from_string_test() {
  atom.create_from_string("this is an existing atom")

  assert Ok(_) = atom.from_string("this is an existing atom")

  assert Error(atom.AtomNotLoaded) =
    atom.from_string("this is not an atom we have seen before")
}

pub fn create_from_string_test() {
  let result =
    "ok"
    |> atom.create_from_string
    |> Ok
  assert True = result == atom.from_string("ok")

  let result =
    "expect"
    |> atom.create_from_string
    |> Ok
  assert True = result == atom.from_string("expect")

  let result =
    "this is another atom we have not seen before"
    |> atom.create_from_string
    |> Ok
  assert True =
    result == atom.from_string("this is another atom we have not seen before")
}

pub fn to_string_test() {
  assert "ok" = atom.to_string(atom.create_from_string("ok"))

  assert "expect" = atom.to_string(atom.create_from_string("expect"))
}

pub fn from_dynamic_test() {
  let result =
    ""
    |> atom.create_from_string
    |> dynamic.from
    |> atom.from_dynamic
  assert True = result == Ok(atom.create_from_string(""))

  let result =
    "ok"
    |> atom.create_from_string
    |> dynamic.from
    |> atom.from_dynamic
  assert True = result == Ok(atom.create_from_string("ok"))

  assert Error([DecodeError(expected: "Atom", found: "Int", path: [])]) =
    1
    |> dynamic.from
    |> atom.from_dynamic

  assert Error([DecodeError(expected: "Atom", found: "List", path: [])]) =
    []
    |> dynamic.from
    |> atom.from_dynamic
}
