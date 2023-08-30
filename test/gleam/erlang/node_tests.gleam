import gleam/erlang/node
import gleam/erlang/atom

// TODO: Improve these tests by spawning a peer node.

pub fn self_test() {
  let a = node.self()
  let b = node.self()
  let assert True = a == b
}

pub fn visible_test() {
  let assert [] = node.visible()
}

pub fn connect_not_alive_test() {
  let name = atom.create_from_string("not_found@localhost")
  let assert Error(node.LocalNodeIsNotAlive) = node.connect(name)
}

pub fn to_atom_test() {
  let assert "nonode@nohost" = atom.to_string(node.to_atom(node.self()))
}
