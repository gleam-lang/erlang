import gleam/dict
import gleam/dynamic.{type Dynamic}
import gleam/erlang/atom.{type Atom}
import gleam/erlang/process.{type Pid}

pub type Node

pub type StartOptions {
  StartOptions(
    name_domain: NameDomain,
    net_ticktime: Int,
    net_tickintensity: Int,
    dist_listen: Bool,
    hidden: Bool,
  )
}

pub type NameDomain {
  Shortnames
  Longnames
}

pub type NodeStartFailureReason

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
/// These messages are untyped, like regular Erlang messages.
///
pub fn send(node: Node, name: Atom, message: message) -> Nil {
  raw_send(#(name, node), message)
  Nil
}

@external(erlang, "erlang", "send")
fn raw_send(receiver: #(Atom, Node), message: message) -> DoNotLeak

/// Convert a node to the atom of its name.
///
@external(erlang, "gleam_erlang_ffi", "identity")
pub fn to_atom(node: Node) -> Atom

@external(erlang, "net_kernel", "start")
fn net_kernel_start(
  name: Atom,
  options: dict.Dict(Atom, Dynamic),
) -> Result(Pid, NodeStartFailureReason)

pub fn start(
  name: String,
  start_options: StartOptions,
) -> Result(Pid, NodeStartFailureReason) {
  let start_options_dict: dict.Dict(Atom, Dynamic) =
    dict.new()
    |> dict.insert(
      atom.create_from_string("name_domain"),
      dynamic.from(start_options.name_domain),
    )
    |> dict.insert(
      atom.create_from_string("net_ticktime"),
      dynamic.from(start_options.net_ticktime),
    )
    |> dict.insert(
      atom.create_from_string("net_tickintensity"),
      dynamic.from(start_options.net_tickintensity),
    )
    |> dict.insert(
      atom.create_from_string("dist_listen"),
      dynamic.from(start_options.dist_listen),
    )
    |> dict.insert(
      atom.create_from_string("hidden"),
      dynamic.from(start_options.hidden),
    )

  net_kernel_start(atom.create_from_string(name), start_options_dict)
}
