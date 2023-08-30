import gleam/erlang/atom.{Atom}

pub type Node

type DoNotLeak

// TODO: test
// TODO: document
@external(erlang, "erlang", "node")
pub fn self() -> Node

// TODO: test
// TODO: document
@external(erlang, "erlang", "nodes")
pub fn list() -> List(Node)

pub type ConnectError {
  /// Was unable to connect to the node.
  FailedToConnect
  /// The local node is not alive, so it is not possible to connect to the other
  /// node.
  LocalNodeIsNotAlive
}

// TODO: test
// TODO: document
@external(erlang, "gleam_erlang_ffi", "connect_node")
pub fn connect(node: Atom) -> Result(Node, ConnectError)

// TODO: test
// TODO: document
pub fn send(node: Node, name: Atom, message: message) -> Nil {
  raw_send(#(name, node), message)
  Nil
}

@external(erlang, "erlang", "send")
fn raw_send(receiver: #(Atom, Node), message: message) -> DoNotLeak

// TODO: test
// TODO: document
@external(erlang, "gleam_erlang_ffi", "identity")
pub fn to_atom(node: Node) -> Atom
