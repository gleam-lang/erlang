import gleam/erlang/port
import gleam/erlang/port/receive.{Data, ExitStatus}
import gleam/erlang/process.{receive, port_open, port_connect, port_subject}
import gleam/erlang/charlist

@external(erlang, "gleam_erlang_test_ffi", "assert_gleam_panic")
fn assert_panic(f: fn() -> t) -> String

pub fn port_open_test() {
  let assert Ok(port) = port.open(
    port.spawn_executable("/bin/bash"),
    [port.Args(["-c", "echo hi | wc -c"]), port.UseStdio, port.ExitStatus, port.Binary]
  )

  let subject = port_subject(port)

  assert Ok(Data(<<"3\n">>)) == receive(subject, 100)
  assert Ok(ExitStatus(0)) == receive(subject, 100)
  assert Error(Nil) == receive(subject, 100)
}

pub fn port_env_open_test() {
  let assert Ok(port) = port.open(
    port.spawn_executable("/bin/bash"),
    [
      port.Args(["-c", "echo $VAR"]),
      port.Env([#(charlist.from_string("VAR"), charlist.from_string("test123"))]),
      port.UseStdio, port.ExitStatus, port.Binary
    ]
  )

  let subject = port_subject(port)

  assert Ok(Data(<<"test123\n">>)) == receive(subject, 100)
}

pub fn fail_port_open_test() {
  let assert Error(port.Enoent) = port.open(
    port.spawn_executable("/bin/doesntexist"),
    []
  )
}

pub fn port_subject_test() {
  let assert Ok(subject) = port_open(
    port.spawn_executable("/bin/bash"),
    [port.Args(["-c", "echo hello | wc -c"]), port.UseStdio, port.ExitStatus, port.Binary]
  )

  assert Ok(Data(<<"6\n">>)) == receive(subject, 100)
  assert Ok(ExitStatus(0)) == receive(subject, 100)
  assert Error(Nil) == receive(subject, 100)
}

pub fn close_port_test() {
  let assert Ok(port) = port.open(
    port.spawn_executable("/bin/bash"),
    [port.Args(["-c", "echo hi; sleep 0.25; echo yo 2> /dev/null"]), port.UseStdio, port.ExitStatus, port.Binary]
  )

  let subject = port_subject(port)

  assert Ok(Data(<<"hi\n">>)) == receive(subject, 100)

  port.close(port)

  assert Error(Nil) == receive(subject, 300)
}

pub fn delay_port_test() {
  let assert Ok(port) = port.open(
    port.spawn_executable("/bin/bash"),
    [port.Args(["-c", "echo hi; sleep 0.25; echo yo 2> /dev/null"]), port.UseStdio, port.ExitStatus, port.Binary]
  )

  let subject = port_subject(port)
  assert Ok(Data(<<"hi\n">>)) == receive(subject, 50)
  assert Error(Nil) == receive(subject, 100)
  assert Ok(Data(<<"yo\n">>)) == receive(subject, 300)
  assert Ok(ExitStatus(0)) == receive(subject, 100)
  assert Error(Nil) == receive(subject, 100)
}

// mirrors name_other_switchover_test in process_test a bit
pub fn port_switchover_test() {
  // create a new port
  let assert Ok(port) = port.open(
    port.spawn_executable("/bin/bash"),
    [port.Args(["-c", "cat"]), port.UseStdio, port.ExitStatus, port.Binary]
  )

  let subject = process.port_subject(port)

  // verify we can listen on it
  port.port_command(port, <<"wasd\n">>, [])
  assert Ok(Data(<<"wasd\n">>)) == receive(subject, 5)

  // switch the owner to some other erlang process
  let pid = process.spawn(fn() { process.sleep(20) })
  let newsubject = port_connect(port, pid)

  // try to listen on it again
  port.port_command(port, <<"test2\n">>, [])
  assert assert_panic(fn() { process.receive(newsubject, 0) })
    == "Cannot receive with a subject owned by another process"
  // test the original subject, too
  assert assert_panic(fn() { process.receive(subject, 0) })
    == "Cannot receive with a subject owned by another process"

  // let's switch back now
  let newnewsubject = port_connect(port, process.self())
  port.port_command(port, <<"test3\n">>, [])
  assert Ok(Data(<<"test3\n">>)) == receive(subject, 5)
  port.port_command(port, <<"test4\n">>, [])
  assert Ok(Data(<<"test4\n">>)) == receive(newsubject, 5)
  port.port_command(port, <<"test5\n">>, [])
  assert Ok(Data(<<"test5\n">>)) == receive(newnewsubject, 5)

  port.close(port)
}


pub fn port_subject_send_test() {
  let assert Ok(port) = port.open(
    port.spawn_executable("/bin/bash"),
    [port.Args(["-c", "cat"]), port.UseStdio, port.ExitStatus, port.Binary]
  )

  let subject = port_subject(port)


  assert assert_panic(fn() { process.send(subject, Data(<<"hello">>)) })
    == "Cannot send on PortSubject"

  port.close(port)
}
