import gleam/erlang/atom
import gleam/int

pub fn from_string_test() {
  let assert Ok(_) = atom.get("ok")
  let assert Error(Nil) =
    atom.get("the vm does not have an atom with this content")
}

pub fn create_from_string_test() {
  let assert True = atom.get("ok") == Ok(atom.create("ok"))

  // Generate the string at runtime to prevent erlc from optimising the atom
  // creation and doing it at compile time.
  let new = "this is a new atom " <> int.to_string(int.random(100))
  let assert Error(Nil) = atom.get(new)
  let created = atom.create(new)
  let assert True = atom.get(new) == Ok(created)
}

pub fn to_string_test() {
  let assert "ok" = atom.to_string(atom.create("ok"))
  let assert "expect" = atom.to_string(atom.create("expect"))
}
