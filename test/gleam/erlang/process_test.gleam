import gleam/dynamic
import gleam/dynamic/decode
import gleam/erlang/atom
import gleam/erlang/process.{ProcessDown}
import gleam/float
import gleam/function
import gleam/int
import gleam/list
import gleam/option.{Some}
import gleam/set

@external(erlang, "gleam_erlang_ffi", "identity")
fn unsafe_coerce(a: dynamic.Dynamic) -> anything

pub fn self_test() {
  let subject = process.new_subject()
  let pid = process.self()

  let assert True = pid == process.self()
  let assert False = pid == process.spawn(fn() { Nil })

  process.spawn(fn() { process.send(subject, process.self()) })
  let assert Ok(child_pid) = process.receive(subject, 5)
  let assert True = child_pid != process.self()
}

pub fn sleep_test() {
  // Exists just to ensure the function does not error
  process.sleep(1)
}

pub fn subject_owner_test() {
  let subject = process.new_subject()
  let assert True = process.subject_owner(subject) == Ok(process.self())
}

pub fn new_name_test() {
  let assert 1000 =
    list.range(1, 1000)
    |> list.map(fn(_) { process.new_name("name") })
    |> set.from_list
    |> set.size
}

pub fn subject_name_test() {
  let name = process.new_name("name")
  let subject = process.named_subject(name)
  assert process.subject_name(subject) == Ok(name)
  assert process.subject_name(process.new_subject()) == Error(Nil)
}

pub fn subject_owner_named_test() {
  let name = process.new_name("name")
  let subject = process.named_subject(name)
  let assert Ok(_) = process.register(process.self(), name)
  let assert True = process.subject_owner(subject) == Ok(process.self())
  let assert Ok(_) = process.unregister(name)
}

pub fn subject_owner_named_unregistered_test() {
  let name = process.new_name("name")
  let subject = process.named_subject(name)
  let assert Error(Nil) = process.subject_owner(subject)
}

pub fn receive_test() {
  let subject = process.new_subject()

  // Send message from self
  process.send(subject, 0)

  // Send message from another process
  process.spawn(fn() {
    process.send(subject, 1)
    process.send(subject, 2)
  })

  // Assert all the messages arrived
  let assert Ok(0) = process.receive(subject, 0)
  let assert Ok(1) = process.receive(subject, 50)
  let assert Ok(2) = process.receive(subject, 0)
  let assert Error(Nil) = process.receive(subject, 0)
}

@external(erlang, "gleam_erlang_test_ffi", "assert_gleam_panic")
fn assert_panic(f: fn() -> t) -> String

pub fn receive_other_test() {
  let subject = process.new_subject()
  process.spawn(fn() { process.send(subject, process.new_subject()) })
  let assert Ok(subject) = process.receive(subject, 20)

  assert assert_panic(fn() { process.receive(subject, 0) })
    == "Cannot receive with a subject owned by another process"
}

pub fn receive_forever_test() {
  let subject = process.new_subject()

  // Send message from self
  process.send(subject, 0)

  // Send message from another process
  process.spawn(fn() {
    process.send(subject, 1)
    process.send(subject, 2)
  })

  // Assert all the messages arrived
  let assert 0 = process.receive_forever(subject)
  let assert 1 = process.receive_forever(subject)
  let assert 2 = process.receive_forever(subject)
}

pub fn receive_forever_other_test() {
  let subject = process.new_subject()
  process.spawn(fn() { process.send(subject, process.new_subject()) })
  let subject = process.receive_forever(subject)

  assert assert_panic(fn() { process.receive_forever(subject) })
    == "Cannot receive with a subject owned by another process"
}

pub fn is_alive_test() {
  let pid = process.spawn_unlinked(fn() { Nil })
  process.sleep(5)
  let assert False = process.is_alive(pid)
}

pub fn sleep_forever_test() {
  let pid = process.spawn_unlinked(process.sleep_forever)
  process.sleep(5)
  let assert True = process.is_alive(pid)
}

pub fn selector_test() {
  let subject1 = process.new_subject()
  let subject2 = process.new_subject()
  let subject3 = process.new_subject()

  process.send(subject1, "1")
  process.send(subject2, 2)
  process.send(subject3, 3.0)

  let selector =
    process.new_selector()
    |> process.select_map(subject2, int.to_string)
    |> process.select_map(subject3, float.to_string)

  // We can selectively receive messages for subjects 2 and 3, skipping the one
  // from subject 1 even though it is first in the mailbox.
  let assert Ok("2") = process.selector_receive(selector, 0)
  let assert Ok("3.0") = process.selector_receive(selector, 0)
  let assert Error(Nil) = process.selector_receive(selector, 0)

  // More messages for subjects 2 and 3
  process.send(subject2, 2)
  process.send(subject3, 3.0)

  // Include subject 1 also
  let selector = process.select(selector, subject1)

  // Now we get the message for subject 1 first as it is first in the mailbox
  let assert Ok("1") = process.selector_receive(selector, 0)
  let assert Ok("2") = process.selector_receive(selector, 0)
  let assert Ok("3.0") = process.selector_receive(selector, 0)
  let assert Error(Nil) = process.selector_receive(selector, 0)
}

pub fn monitor_normal_exit_test() {
  assert monitor_process_exit(fn() { Nil }) == process.Normal
}

pub fn monitor_killed_test() {
  assert monitor_process_exit(fn() { process.kill(process.self()) })
    == process.Killed
}

pub fn monitor_abnormal_exit_test() {
  assert monitor_process_exit(fn() {
      process.send_abnormal_exit(process.self(), "reason")
    })
    == process.Abnormal(dynamic.string("reason"))
}

/// Spawns a child, monitors exits, runs `terminating_with` in the child,
/// checks that a `ProcessDown` is received, and finally returns the exit reason.
fn monitor_process_exit(terminating_with: fn() -> Nil) -> process.ExitReason {
  // Spawn child
  let parent_subject = process.new_subject()
  let pid =
    process.spawn_unlinked(fn() {
      let subject = process.new_subject()
      process.send(parent_subject, subject)
      // Wait for the parent to send a message before exiting
      let assert Ok(_) = process.receive(subject, 150)
      terminating_with()
    })

  // Monitor child
  let monitor = process.monitor(pid)
  let selector =
    process.new_selector()
    |> process.select_monitors(fn(x) { x })

  // There is no monitor message while the child is alive
  let assert Error(Nil) = process.selector_receive(selector, 0)

  // Terminate child to trigger monitor
  let assert Ok(child_subject) = process.receive(parent_subject, 50)
  process.send(child_subject, Nil)

  // We get a process down message!
  let assert Ok(ProcessDown(downed_monitor, downed_pid, reason)) =
    process.selector_receive(selector, 50)

  let assert True = downed_pid == pid
  let assert True = downed_monitor == monitor

  reason
}

pub fn monitor_specific_test() {
  let parent_subject = process.new_subject()
  let spawn = fn() {
    process.spawn_unlinked(fn() {
      let subject = process.new_subject()
      process.send(parent_subject, subject)
      // Wait for the parent to send a message before exiting
      process.receive(subject, 150)
    })
  }
  // Spawn child
  let pid1 = spawn()
  let pid2 = spawn()

  // Monitor children
  let monitor1 = process.monitor(pid1)
  let _monitor2 = process.monitor(pid2)
  let selector =
    process.new_selector()
    |> process.select_specific_monitor(monitor1, fn(x) { x })

  // There is no monitor message while the child is alive
  let assert Error(Nil) = process.selector_receive(selector, 0)

  // Shutdown child to trigger monitor
  let assert Ok(child_subject) = process.receive(parent_subject, 50)
  process.send(child_subject, Nil)

  // We get a process down message!
  let assert Ok(ProcessDown(downed_monitor, downed_pid, _reason)) =
    process.selector_receive(selector, 50)

  let assert True = downed_pid == pid1
  let assert True = downed_monitor == monitor1

  // We don't get the other one if we select again as the selector doesn't
  // include it
  let assert Error(Nil) = process.selector_receive(selector, 50)
}

pub fn demonitor_test() {
  // Spawn child
  let parent_subject = process.new_subject()
  let pid =
    process.spawn_unlinked(fn() {
      let subject = process.new_subject()
      process.send(parent_subject, subject)
      // Wait for the parent to send a message before exiting
      process.receive(subject, 150)
    })

  // Monitor child
  let monitor = process.monitor(pid)
  let empty_selector = process.new_selector()
  let selector =
    empty_selector
    |> process.select_specific_monitor(monitor, fn(x) { x })

  // Shutdown child to trigger monitor
  let assert Ok(child_subject) = process.receive(parent_subject, 50)
  process.send(child_subject, Nil)

  // Demonitor the child
  process.demonitor_process(monitor)

  // There is no down message
  let assert Error(Nil) = process.selector_receive(selector, 5)

  // Remove monitor from selector
  let assert True =
    empty_selector == selector |> process.deselect_specific_monitor(monitor)
}

pub fn call_test() {
  let parent_subject = process.new_subject()

  process.spawn(fn() {
    // Send the child subject to the parent so it can call the child
    let child_subject = process.new_subject()
    process.send(parent_subject, child_subject)
    // Wait for the subject to be messaged
    let assert Ok(#(x, reply)) = process.receive(child_subject, 50)
    // Reply
    process.send(reply, x + 1)
  })

  let assert Ok(call_subject) = process.receive(parent_subject, 50)

  // Call the child process and get a response.
  let assert 2 = process.call(call_subject, 50, fn(subject) { #(1, subject) })
}

pub fn call_forever_test() {
  let parent_subject = process.new_subject()

  process.spawn(fn() {
    // Send the child subject to the parent so it can call the child
    let child_subject = process.new_subject()
    process.send(parent_subject, child_subject)
    // Wait for the subject to be messaged
    let assert Ok(#(x, reply)) = process.receive(child_subject, 50)
    // Reply
    process.send(reply, x + 1)
  })

  let assert Ok(call_subject) = process.receive(parent_subject, 50)

  // Call the child process and get a response.
  let assert 2 =
    process.call_forever(call_subject, fn(subject) { #(1, subject) })
}

@external(erlang, "erlang", "send")
fn send(a: process.Pid, b: anything) -> Nil

pub fn select_record_test() {
  send(process.self(), #("a", 1))
  send(process.self(), #("a", 1, 2))
  send(process.self(), #("b", 2, 3))
  send(process.self(), #("c", 4, 5, 6))
  send(process.self(), #("d", 7, 8, 9, 10))
  send(process.self(), #("e", 11, 12, 13, 14, 15))
  send(process.self(), #("f", 16, 17, 18, 19, 20, 21))
  send(process.self(), #("g", 22, 23, 24, 25, 26, 27, 28))
  send(process.self(), "h")

  let assert Error(Nil) =
    process.new_selector()
    |> process.select_record("h", 1, unsafe_coerce)
    |> process.selector_receive(0)

  let assert Error(Nil) =
    process.new_selector()
    |> process.select_record("c", 1, unsafe_coerce)
    |> process.selector_receive(0)
  let assert Error(Nil) =
    process.new_selector()
    |> process.select_record("c", 1, unsafe_coerce)
    |> process.selector_receive(0)
  let assert Error(Nil) =
    process.new_selector()
    |> process.select_record("c", 2, unsafe_coerce)
    |> process.selector_receive(0)

  let assert Ok(#("g", 22, 23, 24, 25, 26, 27, 28)) =
    process.new_selector()
    |> process.select_record("g", 7, unsafe_coerce)
    |> process.selector_receive(0)

  let assert Ok(#("f", 16, 17, 18, 19, 20, 21)) =
    process.new_selector()
    |> process.select_record("f", 6, unsafe_coerce)
    |> process.selector_receive(0)

  let assert Ok(#("e", 11, 12, 13, 14, 15)) =
    process.new_selector()
    |> process.select_record("e", 5, unsafe_coerce)
    |> process.selector_receive(0)

  let assert Ok(#("d", 7, 8, 9, 10)) =
    process.new_selector()
    |> process.select_record("d", 4, unsafe_coerce)
    |> process.selector_receive(0)

  let assert Ok(#("c", 4, 5, 6)) =
    process.new_selector()
    |> process.select_record("c", 3, unsafe_coerce)
    |> process.selector_receive(0)

  let assert Ok(#("b", 2, 3)) =
    process.new_selector()
    |> process.select_record("b", 2, unsafe_coerce)
    |> process.selector_receive(0)

  let assert Ok(#("a", 1)) =
    process.new_selector()
    |> process.select_record("a", 1, unsafe_coerce)
    |> process.selector_receive(0)

  let assert Error(Nil) =
    process.new_selector()
    |> process.select_record("a", 1, unsafe_coerce)
    |> process.selector_receive(0)

  let assert Ok(#("a", 1, 2)) =
    process.new_selector()
    |> process.select_record("a", 2, unsafe_coerce)
    |> process.selector_receive(0)
}

pub fn select_anything_test() {
  process.flush_messages()
  send(process.self(), 1)
  send(process.self(), 2.0)

  let selector =
    process.new_selector()
    |> process.select_other(decode.run(_, decode.int))

  let assert Ok(Ok(1)) = process.selector_receive(selector, 0)
  let assert Ok(Error([
    decode.DecodeError(expected: "Int", found: "Float", path: []),
  ])) = process.selector_receive(selector, 0)
  let assert Error(Nil) = process.selector_receive(selector, 0)
}

pub fn linking_self_test() {
  let assert True = process.link(process.self())
}

pub fn linking_new_test() {
  let assert True =
    process.link(process.spawn_unlinked(fn() { process.sleep(100) }))
}

pub fn relinking_test() {
  let assert True = process.link(process.spawn(fn() { process.sleep(100) }))
}

pub fn linking_dead_test() {
  let pid = process.spawn(fn() { Nil })
  process.sleep(20)
  let assert False = process.link(pid)
}

pub fn unlink_unlinked_test() {
  process.unlink(process.spawn_unlinked(fn() { process.sleep(100) }))
}

pub fn unlink_linked_test() {
  process.unlink(process.spawn(fn() { process.sleep(100) }))
}

pub fn unlink_dead_test() {
  let pid = process.spawn(fn() { Nil })
  process.sleep(10)
  process.unlink(pid)
}

pub fn send_after_test() {
  let subject = process.new_subject()

  // 0 is received immediately, though asynchronously
  process.send_after(subject, 0, "a")
  let assert Ok("a") = process.receive(subject, 50)

  // With a delay it is sent later
  process.send_after(subject, 5, "b")
  let assert Error(Nil) = process.receive(subject, 0)
  let assert Ok("b") = process.receive(subject, 50)
}

pub fn cancel_timer_test() {
  let subject = process.new_subject()
  let timer = process.send_after(subject, 5, "a")
  let assert process.Cancelled(_) = process.cancel_timer(timer)
  let assert Error(Nil) = process.receive(subject, 5)
}

pub fn cancel_already_fired_timer_test() {
  let subject = process.new_subject()
  let timer = process.send_after(subject, 0, "a")
  let assert Ok(_) = process.receive(subject, 50)
  let assert process.TimerNotFound = process.cancel_timer(timer)
}

pub fn kill_test() {
  let pid = process.spawn_unlinked(fn() { process.sleep(100) })
  let assert True = process.is_alive(pid)
  process.kill(pid)
  let assert False = process.is_alive(pid)
}

pub fn kill_already_dead_test() {
  let pid = process.spawn(fn() { Nil })
  process.sleep(10)
  let assert False = process.is_alive(pid)
  process.kill(pid)
}

pub fn send_exit_test() {
  let pid = process.spawn_unlinked(fn() { process.sleep(100) })
  process.send_exit(pid)
}

pub fn send_exit_already_dead_test() {
  let pid = process.spawn(fn() { Nil })
  process.sleep(10)
  let assert False = process.is_alive(pid)
  process.send_exit(pid)
}

pub fn send_abnormal_exit_test() {
  let pid = process.spawn_unlinked(fn() { process.sleep(100) })
  process.send_abnormal_exit(pid, "Bye")
}

pub fn send_abnormal_exit_already_dead_test() {
  let pid = process.spawn(fn() { Nil })
  process.sleep(10)
  let assert False = process.is_alive(pid)
  process.send_abnormal_exit(pid, "Bye")
}

pub fn trap_exit_test() {
  process.trap_exits(True)
  let pid = process.spawn(fn() { process.sleep(100) })
  // This would cause an error if we were not trapping exits
  process.kill(pid)
}

pub fn select_forever_test() {
  let subject = process.new_subject()
  process.send(subject, 1)

  let assert 1 =
    process.new_selector()
    |> process.select(subject)
    |> process.selector_receive_forever
}

pub fn map_selector_test() {
  let subject1 = process.new_subject()
  let subject2 = process.new_subject()
  process.send(subject1, 1)
  process.send(subject2, 2.0)

  let selector =
    process.new_selector()
    |> process.select_map(subject1, int.to_string)
    |> process.select_map(subject2, float.to_string)
    |> process.map_selector(Some)

  let assert Some("1") = process.selector_receive_forever(selector)
  let assert Some("2.0") = process.selector_receive_forever(selector)
}

pub fn merge_selector_test() {
  let subject1 = process.new_subject()
  let subject2 = process.new_subject()
  process.send(subject1, 1)
  process.send(subject2, 2)

  let selector =
    process.new_selector()
    |> process.select_map(subject1, fn(a) { #("a", a) })
    |> process.select_map(subject2, fn(a) { #("a", a) })
    |> process.merge_selector(
      process.new_selector()
      |> process.select_map(subject2, fn(a) { #("b", a) }),
    )

  let assert #("a", 1) = process.selector_receive_forever(selector)
  let assert #("b", 2) = process.selector_receive_forever(selector)
}

pub fn select_trapped_exits_kill_test() {
  assert select_trapped_exits(fn() { process.kill(process.self()) })
    == process.Killed
}

pub fn select_trapped_exits_abnormal_test() {
  assert select_trapped_exits(fn() {
      process.send_abnormal_exit(process.self(), "reason")
    })
    == process.Abnormal(dynamic.string("reason"))
}

pub fn select_trapped_exits_normal_test() {
  assert select_trapped_exits(fn() { Nil }) == process.Normal
}

/// Traps exits, starts a linked child, runs `terminating_with` in the child,
/// expects an `ExitMessage`, and returns the exit reason
pub fn select_trapped_exits(terminating_with: fn() -> Nil) -> process.ExitReason {
  process.flush_messages()

  process.trap_exits(True)
  let pid = process.spawn(terminating_with)

  let assert Ok(process.ExitMessage(exited, reason)) =
    process.new_selector()
    |> process.select_trapped_exits(function.identity)
    |> process.selector_receive(10)

  let assert True = pid == exited
  process.trap_exits(False)
  reason
}

pub fn flush_messages_test() {
  let subject = process.new_subject()
  process.send(subject, 1)
  process.send(subject, 2)
  process.send(subject, 3)
  process.flush_messages()
  let assert Error(Nil) = process.receive(subject, 0)
}

@external(erlang, "gleam_erlang_ffi", "identity")
fn atom_to_dynamic(atom: atom.Atom) -> dynamic.Dynamic

pub fn register_name_taken_test() {
  let taken_name = unsafe_coerce(atom_to_dynamic(atom.create("code_server")))
  let assert Ok(a) = process.named(taken_name)
  let assert Error(Nil) = process.register(process.self(), taken_name)
  let assert Ok(b) = process.named(taken_name)
  let assert True = a == b
}

pub fn register_name_test() {
  let name = process.new_name("name")
  let _ = process.unregister(name)
  let assert Error(Nil) = process.named(name)
  let assert Ok(Nil) = process.register(process.self(), name)
  let assert Ok(pid) = process.named(name)
  let assert True = pid == process.self()
  let _ = process.unregister(name)
}

pub fn unregister_name_test() {
  let name = process.new_name("name")
  let _ = process.unregister(name)
  let assert Ok(Nil) = process.register(process.self(), name)
  let assert Ok(_) = process.named(name)
  let assert Ok(Nil) = process.unregister(name)
  let assert Error(Nil) = process.named(name)
  let _ = process.unregister(name)
}

pub fn deselect_test() {
  let subject1 = process.new_subject()
  let subject2 = process.new_subject()
  let selector0 = process.new_selector()
  let selector1 = selector0 |> process.select(subject1)
  let selector2 = selector1 |> process.select(subject2)

  assert process.deselect(selector2, subject2) == selector1

  assert process.deselect(selector1, subject1) == selector0

  assert selector2
    |> process.deselect(subject1)
    |> process.deselect(subject2)
    == selector0
}

pub fn name_test() {
  let name = process.new_name("name")
  let assert Ok(_) = process.register(process.self(), name)
  let subject = process.named_subject(name)
  process.send(subject, "Hello")
  let assert Ok("Hello") = process.receive(subject, 0)
  process.unregister(name)
}

pub fn name_other_test() {
  let name = process.new_name("name")
  let pid = process.spawn_unlinked(fn() { process.sleep(10) })
  let assert Ok(_) = process.register(pid, name)
  let subject = process.named_subject(name)
  process.send(subject, "Hello")
  assert assert_panic(fn() { process.receive(subject, 5) })
    == "Cannot receive with a subject owned by another process"
  process.unregister(name)
}


// tests that sending/receiving on names fail if the name gets unregistered
pub fn unregister_receive_test() {
  let name = process.new_name("name")
  let assert Ok(_) = process.register(process.self(), name)
  let subject = process.named_subject(name)

  process.send(subject, "Hello")
  let assert Ok("Hello") = process.receive(subject, 0)

  let assert Ok(_) = process.unregister(name)

  assert assert_panic(fn() { process.send(subject, "Hello") })
    == "Sending to unregistered name"
  assert assert_panic(fn() { process.receive(subject, 0) })
    == "Cannot receive with a subject owned by another process" // this error message could be better
}

// tests that receiving on names fail if the registration gets moved during execution
pub fn name_other_switchover_test() {
  let name = process.new_name("name")
  let self = process.self()
  let assert Ok(_) = process.register(self, name)
  let subject = process.named_subject(name)

  assert Ok(self) == process.subject_owner(subject)

  process.send(subject, "Hello")
  let assert Ok("Hello") = process.receive(subject, 0)

  let pid = process.spawn(fn() {
    process.sleep(10)
    let assert Ok(_) = process.unregister(name) // "undesirable behaviour"
    let assert Ok(_) = process.register(process.self(), name)

    process.sleep(20)
    let assert Ok("Peace") = process.receive(subject, 0)
  })

  assert Ok(self) == process.subject_owner(subject)

  process.send(subject, "World")
  let assert Ok("World") = process.receive(subject, 0)

  assert Ok(self) == process.subject_owner(subject)
  process.sleep(12)
  assert Ok(pid) == process.subject_owner(subject)

  process.send(subject, "Peace")
  assert assert_panic(fn() { process.receive(subject, 0) })
    == "Cannot receive with a subject owned by another process"
}
