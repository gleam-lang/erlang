import gleam/dynamic
import gleam/erlang/charlist.{type Charlist}

/// Ports are how code running on the Erlang virtual machine interacts with
/// the outside world. Bytes of data can be sent to and read from ports,
/// providing a form of message passing to an external program or resource.
///
/// For more information on ports see the [Erlang ports documentation][1].
///
/// [1]: https://erlang.org/doc/reference_manual/ports.html
///
pub type Port

pub type PortCommand {
  Spawn(String)
  SpawnDriver(String)
  SpawnExecutable(String)
  Fd(in: Int, out: Int)
}

pub fn spawn(command: String) -> PortCommand {
  Spawn(command)
}

pub fn spawn_driver(command: String) -> PortCommand {
  SpawnDriver(command)
}

pub fn spawn_executable(command: String) -> PortCommand {
  SpawnExecutable(command)
}


pub type PortOptions {
  Arg0(String)
  Args(List(String))
  Env(List(#(Charlist, Charlist)))
  Cd(String)
  Binary

  UseStdio
  NouseStdio

  Eof
  ExitStatus

  Stream
  Line(Int)
  Packet(Int) // only 1, 2, & 4
}

pub type PortError {
  Badarg
  SystemLimit
  Enomem
  Eagain
  Enametoolong
  Emfile
  Enfile
  Eaccess
  Enoent
}

@external(erlang, "gleam_erlang_ffi", "my_open_port")
pub fn open(
  name: PortCommand,
  settings: List(PortOptions),
) -> Result(Port, PortError)

@external(erlang, "erlang", "port_command")
pub fn port_command(
  port: Port,
  data: BitArray,
  options: List(PortOptions),
) -> Nil


@external(erlang, "gleam_erlang_ffi", "identity")
pub fn to_dynamic(a: Port) -> dynamic.Dynamic

@external(erlang, "erlang", "port_close")
pub fn close(port: Port) -> Nil
