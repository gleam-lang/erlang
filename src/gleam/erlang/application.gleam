//// An Erlang application is a collection of code that can be loaded into the
//// Erlang virtual machine and even started and stopped if they define a
//// start module and supervision tree. Each Gleam package is an Erlang
//// application.

import gleam/erlang/node.{type Node}

/// The Erlang/OTP application `start` callback takes a start-type as an
/// argument, indicating the context in which the application is being started.
pub type StartType {
  /// A normal application start.
  Normal
  /// The application is distributed and started at the current node because of
  /// a takeover from Node, either because Erlang's `application:takeover/2`
  /// function has been called, or because the current node has higher priority
  /// than the previous node.
  Takeover(previous: Node)
  /// The application is distributed and started at the current node because of
  /// a failover from the previous node.
  Failover(previous: Node)
}

/// Returns the path of an application's `priv` directory, where extra non-Gleam
/// or Erlang files are typically kept. Each Gleam package is an Erlang
/// application.
///
/// Returns an error if no application was found with the given name.
///
/// # Example
///
/// ```gleam
/// application.priv_directory("my_app")
/// // -> Ok("/some/location/my_app/priv")
/// ```
///
@external(erlang, "gleam_erlang_ffi", "priv_directory")
pub fn priv_directory(name: String) -> Result(String, Nil)
