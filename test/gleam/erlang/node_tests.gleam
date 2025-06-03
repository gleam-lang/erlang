import gleam/erlang/atom
import gleam/erlang/node

// TODO: Improve these tests by spawning a peer node.

pub fn self_test() {
  let a = node.self()
  let b = node.self()
  assert a == b
}

pub fn visible_test() {
  let assert [] = node.visible()
}

pub fn connect_not_alive_test() {
  let name = atom.create("not_found@localhost")
  assert node.connect(name) == Error(node.LocalNodeIsNotAlive)
}

pub fn name_test() {
  assert node.name(node.self()) == atom.create("nonode@nohost")
}
