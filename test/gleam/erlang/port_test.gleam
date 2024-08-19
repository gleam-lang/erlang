import gleam/dynamic.{DecodeError}
import gleam/erlang/port
import gleeunit/should

type PortName {
  Spawn(String)
}

type PortSetting

@external(erlang, "erlang", "open_port")
fn open_port(
  port_name: PortName,
  port_settings: List(PortSetting),
) -> dynamic.Dynamic

pub fn port_dynamic_test() {
  let msg = open_port(Spawn("echo \"hello world\""), [])

  msg
  |> port.port_from_dynamic
  |> should.be_ok()

  let assert Error([DecodeError(expected: "Port", found: "Int", path: [])]) =
    dynamic.from(1)
    |> port.port_from_dynamic
}
