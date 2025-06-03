//// Multiple Erlang VM instances can form a cluster to make a distributed
//// Erlang system, talking directly to each other using messages rather than
//// other communication protocols like HTTP. In a distributed Erlang system
//// each virtual machine is called a _node_. This module provides Node related
//// types and functions to be used as a foundation by other packages providing
//// more specialised functionality.
////
//// For more information on distributed Erlang systems see the Erlang
//// documentation: <https://www.erlang.org/doc/system/distributed.html>.

import gleam/erlang/atom.{type Atom}

pub type Node

/// Return the current node.
///
@external(erlang, "erlang", "node")
pub fn self() -> Node

/// Return a list of all visible nodes in the cluster, not including the current
/// node.
///
/// The current node can be included by calling `self()` and prepending the
/// result.
///
/// ```gleam
/// let all_nodes = [node.self(), ..node.visible()]
/// ```
///
@external(erlang, "erlang", "nodes")
pub fn visible() -> List(Node)

pub type ConnectError {
  /// Was unable to connect to the node.
  FailedToConnect
  /// The local node is not alive, so it is not possible to connect to the other
  /// node.
  LocalNodeIsNotAlive
}

// TODO: test unknown node
// TODO: test successfully connecting
/// Establish a connection to a node, so the nodes can send messages to each
/// other and any other connected nodes.
///
/// Returns `Error(FailedToConnect)` if the node is not reachable.
///
/// Returns `Error(LocalNodeIsNotAlive)` if the local node is not alive, meaning
/// it is not running in distributed mode.
///
@external(erlang, "gleam_erlang_ffi", "connect_node")
pub fn connect(node: Atom) -> Result(Node, ConnectError)

/// Get the atom name of a node.
///
/// ## Examples
///
/// ```gleam
/// assert name(my_node) == atom.create("app1@localhost")
/// ```
///
@external(erlang, "gleam_erlang_ffi", "identity")
pub fn name(node: Node) -> Atom
