import gleam/erlang/atom.{type Atom}
import gleam/erlang/process.{type Name}

pub type Node

type DoNotLeak

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

// TODO: test
/// Send a message to a named process on a given node.
///
/// This function sends messages in the same format as the send function in the
/// process module, so messages sent using it can be received as normal using
/// subjects and selectors.
///
pub fn send(node: Node, name: Name(message), message: message) -> Nil {
  raw_send(#(name, node), #(name, message))
  Nil
}

@external(erlang, "erlang", "send")
fn raw_send(
  receiver: #(Name(message), Node),
  message: #(Name(message), message),
) -> DoNotLeak
