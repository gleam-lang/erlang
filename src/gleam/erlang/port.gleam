import gleam/dynamic.{type DecodeErrors, type Dynamic}

/// Ports are how code running on the Erlang virtual machine interacts with
/// the outside world. Bytes of data can be sent to and read from ports,
/// providing a form of message passing to an external program or resource.
///
/// For more information on ports see the [Erlang ports documentation][1].
///
/// [1]: https://erlang.org/doc/reference_manual/ports.html
///
pub type Port

/// Checks to see whether a `Dynamic` value is a port, and return the port if
/// it is.
///
/// ## Examples
///
///    > import gleam/dynamic
///    > from_dynamic(dynamic.from(process.self()))
///    Ok(process.self())
///
///    > from_dynamic(dynamic.from(123))
///    Error([DecodeError(expected: "Port", found: "Int", path: [])])
///
@external(erlang, "gleam_erlang_ffi", "port_from_dynamic")
pub fn port_from_dynamic(from from: Dynamic) -> Result(Port, DecodeErrors)
