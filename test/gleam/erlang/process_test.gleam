import gleam/dynamic.{DecodeError}
import gleam/erlang/atom
import gleam/erlang/process.{ProcessDown}
import gleam/float
import gleam/function
import gleam/int
import gleam/option.{Some}

pub fn self_test() {
  let subject = process.new_subject()
  let pid = process.self()

  let assert True = pid == process.self()
  let assert False = pid == process.start(fn() { Nil }, linked: True)

  process.start(fn() { process.send(subject, process.self()) }, linked: True)
  let assert Ok(child_pid) = process.receive(subject, 5)
  let assert True = child_pid != process.self()
}

pub fn sleep_test() {
  // Exists just to ensure the function does not error
  process.sleep(1)
}

pub fn subject_owner_test() {
  let subject = process.new_subject()
  let assert True = process.subject_owner(subject) == process.self()
}

pub fn receive_test() {
  let subject = process.new_subject()

  // Send message from self
  process.send(subject, 0)

  // Send message from another process
  process.start(
    fn() {
      process.send(subject, 1)
      process.send(subject, 2)
    },
    linked: True,
  )

  // Assert all the messages arrived
  let assert Ok(0) = process.receive(subject, 0)
  let assert Ok(1) = process.receive(subject, 50)
  let assert Ok(2) = process.receive(subject, 0)
  let assert Error(Nil) = process.receive(subject, 0)
}

pub fn is_alive_test() {
  let pid = process.start(fn() { Nil }, False)
  process.sleep(5)
  let assert False = process.is_alive(pid)
}

pub fn sleep_forever_test() {
  let pid = process.start(process.sleep_forever, False)
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
    |> process.selecting(subject2, int.to_string)
    |> process.selecting(subject3, float.to_string)

  // We can selectively receive messages for subjects 2 and 3, skipping the one
  // from subject 1 even though it is first in the mailbox.
  let assert Ok("2") = process.select(selector, 0)
  let assert Ok("3.0") = process.select(selector, 0)
  let assert Error(Nil) = process.select(selector, 0)

  // More messages for subjects 2 and 3
  process.send(subject2, 2)
  process.send(subject3, 3.0)

  // Include subject 1 also
  let selector = process.selecting(selector, subject1, fn(x) { x })

  // Now we get the message for subject 1 first as it is first in the mailbox
  let assert Ok("1") = process.select(selector, 0)
  let assert Ok("2") = process.select(selector, 0)
  let assert Ok("3.0") = process.select(selector, 0)
  let assert Error(Nil) = process.select(selector, 0)
}

pub fn monitor_test() {
  // Spawn child
  let parent_subject = process.new_subject()
  let pid =
    process.start(linked: False, running: fn() {
      let subject = process.new_subject()
      process.send(parent_subject, subject)
      // Wait for the parent to send a message before exiting
      process.receive(subject, 150)
    })

  // Monitor child
  let monitor = process.monitor_process(pid)
  let selector =
    process.new_selector()
    |> process.selecting_process_down(monitor, fn(x) { x })

  // There is no monitor message while the child is alive
  let assert Error(Nil) = process.select(selector, 0)

  // Shutdown child to trigger monitor
  let assert Ok(child_subject) = process.receive(parent_subject, 50)
  process.send(child_subject, Nil)

  // We get a process down message!
  let assert Ok(ProcessDown(downed_pid, _reason)) = process.select(selector, 50)

  let assert True = downed_pid == pid
}

pub fn demonitor_test() {
  // Spawn child
  let parent_subject = process.new_subject()
  let pid =
    process.start(linked: False, running: fn() {
      let subject = process.new_subject()
      process.send(parent_subject, subject)
      // Wait for the parent to send a message before exiting
      process.receive(subject, 150)
    })

  // Monitor child
  let monitor = process.monitor_process(pid)
  let selector =
    process.new_selector()
    |> process.selecting_process_down(monitor, fn(x) { x })

  // Shutdown child to trigger monitor
  let assert Ok(child_subject) = process.receive(parent_subject, 50)
  process.send(child_subject, Nil)

  // Demonitor the child
  process.demonitor_process(monitor)

  // There is no down message
  let assert Error(Nil) = process.select(selector, 5)
}

pub fn try_call_test() {
  let parent_subject = process.new_subject()

  process.start(linked: True, running: fn() {
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
  let assert Ok(2) =
    process.try_call(call_subject, fn(subject) { #(1, subject) }, 50)
}

pub fn try_call_timeout_test() {
  let parent_subject = process.new_subject()

  process.start(linked: True, running: fn() {
    // Send the call subject to the parent
    let child_subject = process.new_subject()
    process.send(parent_subject, child_subject)
    // Wait for the subject to be called
    let assert Ok(_) = process.receive(child_subject, 50)
    // Never reply
    process.sleep(100)
  })

  let assert Ok(call_subject) = process.receive(parent_subject, 50)

  // Call the child process over the subject
  let assert Error(process.CallTimeout) =
    process.try_call(call_subject, fn(x) { x }, 10)
}

pub fn call_test() {
  let parent_subject = process.new_subject()

  process.start(linked: True, running: fn() {
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
  let assert 2 = process.call(call_subject, fn(subject) { #(1, subject) }, 50)
}

@external(erlang, "erlang", "send")
fn send(a: process.Pid, b: anything) -> Nil

pub fn selecting_record_test() {
  send(process.self(), #("a", 1))
  send(process.self(), #("b", 2, 3))
  send(process.self(), #("c", 4, 5, 6))
  send(process.self(), #("d", 7, 8, 9, 10))
  send(process.self(), #("e", 11, 12, 13, 14, 15))
  send(process.self(), #("f", 16, 17, 18, 19, 20, 21))
  send(process.self(), #("g", 22, 23, 24, 25, 26, 27, 28))
  send(process.self(), "h")

  let assert Error(Nil) =
    process.new_selector()
    |> process.selecting_record2("h", dynamic.unsafe_coerce)
    |> process.select(0)

  let assert Error(Nil) =
    process.new_selector()
    |> process.selecting_record2("c", dynamic.unsafe_coerce)
    |> process.select(0)
  let assert Error(Nil) =
    process.new_selector()
    |> process.selecting_record2("c", dynamic.unsafe_coerce)
    |> process.select(0)
  let assert Error(Nil) =
    process.new_selector()
    |> process.selecting_record3("c", fn(a, b) {
      #(dynamic.unsafe_coerce(a), dynamic.unsafe_coerce(b))
    })
    |> process.select(0)

  let assert Ok(#(22, 23, 24, 25, 26, 27, 28)) =
    process.new_selector()
    |> process.selecting_record8("g", fn(a, b, c, d, e, f, g) {
      #(
        dynamic.unsafe_coerce(a),
        dynamic.unsafe_coerce(b),
        dynamic.unsafe_coerce(c),
        dynamic.unsafe_coerce(d),
        dynamic.unsafe_coerce(e),
        dynamic.unsafe_coerce(f),
        dynamic.unsafe_coerce(g),
      )
    })
    |> process.select(0)

  let assert Ok(#(16, 17, 18, 19, 20, 21)) =
    process.new_selector()
    |> process.selecting_record7("f", fn(a, b, c, d, e, f) {
      #(
        dynamic.unsafe_coerce(a),
        dynamic.unsafe_coerce(b),
        dynamic.unsafe_coerce(c),
        dynamic.unsafe_coerce(d),
        dynamic.unsafe_coerce(e),
        dynamic.unsafe_coerce(f),
      )
    })
    |> process.select(0)

  let assert Ok(#(11, 12, 13, 14, 15)) =
    process.new_selector()
    |> process.selecting_record6("e", fn(a, b, c, d, e) {
      #(
        dynamic.unsafe_coerce(a),
        dynamic.unsafe_coerce(b),
        dynamic.unsafe_coerce(c),
        dynamic.unsafe_coerce(d),
        dynamic.unsafe_coerce(e),
      )
    })
    |> process.select(0)

  let assert Ok(#(7, 8, 9, 10)) =
    process.new_selector()
    |> process.selecting_record5("d", fn(a, b, c, d) {
      #(
        dynamic.unsafe_coerce(a),
        dynamic.unsafe_coerce(b),
        dynamic.unsafe_coerce(c),
        dynamic.unsafe_coerce(d),
      )
    })
    |> process.select(0)

  let assert Ok(#(4, 5, 6)) =
    process.new_selector()
    |> process.selecting_record4("c", fn(a, b, c) {
      #(
        dynamic.unsafe_coerce(a),
        dynamic.unsafe_coerce(b),
        dynamic.unsafe_coerce(c),
      )
    })
    |> process.select(0)

  let assert Ok(#(2, 3)) =
    process.new_selector()
    |> process.selecting_record3("b", fn(a, b) {
      #(dynamic.unsafe_coerce(a), dynamic.unsafe_coerce(b))
    })
    |> process.select(0)

  let assert Ok(1) =
    process.new_selector()
    |> process.selecting_record2("a", dynamic.unsafe_coerce)
    |> process.select(0)
}

pub fn selecting_anything_test() {
  process.flush_messages()
  send(process.self(), 1)
  send(process.self(), 2.0)

  let selector =
    process.new_selector()
    |> process.selecting_anything(dynamic.int)

  let assert Ok(Ok(1)) = process.select(selector, 0)
  let assert Ok(Error([
    dynamic.DecodeError(expected: "Int", found: "Float", path: []),
  ])) = process.select(selector, 0)
  let assert Error(Nil) = process.select(selector, 0)
}

pub fn linking_self_test() {
  let assert True = process.link(process.self())
}

pub fn linking_new_test() {
  let assert True =
    process.link(
      process.start(linked: False, running: fn() { process.sleep(100) }),
    )
}

pub fn relinking_test() {
  let assert True =
    process.link(
      process.start(linked: True, running: fn() { process.sleep(100) }),
    )
}

pub fn linking_dead_test() {
  let pid = process.start(linked: True, running: fn() { Nil })
  process.sleep(20)
  let assert False = process.link(pid)
}

pub fn unlink_unlinked_test() {
  let assert Nil =
    process.unlink(
      process.start(linked: False, running: fn() { process.sleep(100) }),
    )
}

pub fn unlink_linked_test() {
  let assert Nil =
    process.unlink(
      process.start(linked: True, running: fn() { process.sleep(100) }),
    )
}

pub fn unlink_dead_test() {
  let pid = process.start(linked: True, running: fn() { Nil })
  process.sleep(10)
  let assert Nil = process.unlink(pid)
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
  let pid = process.start(linked: False, running: fn() { process.sleep(100) })
  let assert True = process.is_alive(pid)
  let assert Nil = process.kill(pid)
  let assert False = process.is_alive(pid)
}

pub fn kill_already_dead_test() {
  let pid = process.start(linked: True, running: fn() { Nil })
  process.sleep(10)
  let assert False = process.is_alive(pid)
  let assert Nil = process.kill(pid)
}

pub fn send_exit_test() {
  let pid = process.start(linked: False, running: fn() { process.sleep(100) })
  let assert Nil = process.send_exit(pid)
}

pub fn send_exit_already_dead_test() {
  let pid = process.start(linked: True, running: fn() { Nil })
  process.sleep(10)
  let assert False = process.is_alive(pid)
  let assert Nil = process.send_exit(pid)
}

pub fn send_abnormal_exit_test() {
  let pid = process.start(linked: False, running: fn() { process.sleep(100) })
  let assert Nil = process.send_abnormal_exit(pid, "Bye")
}

pub fn send_abnormal_exit_already_dead_test() {
  let pid = process.start(linked: True, running: fn() { Nil })
  process.sleep(10)
  let assert False = process.is_alive(pid)
  let assert Nil = process.send_abnormal_exit(pid, "Bye")
}

pub fn trap_exit_test() {
  let assert Nil = process.trap_exits(True)
  let pid = process.start(linked: True, running: fn() { process.sleep(100) })
  // This would cause an error if we were not trapping exits
  process.kill(pid)
}

pub fn select_forever_test() {
  let subject = process.new_subject()
  process.send(subject, 1)

  let assert 1 =
    process.new_selector()
    |> process.selecting(subject, function.identity)
    |> process.select_forever
}

pub fn map_selector_test() {
  let subject1 = process.new_subject()
  let subject2 = process.new_subject()
  process.send(subject1, 1)
  process.send(subject2, 2.0)

  let selector =
    process.new_selector()
    |> process.selecting(subject1, int.to_string)
    |> process.selecting(subject2, float.to_string)
    |> process.map_selector(Some)

  let assert Some("1") = process.select_forever(selector)
  let assert Some("2.0") = process.select_forever(selector)
}

pub fn merge_selector_test() {
  let subject1 = process.new_subject()
  let subject2 = process.new_subject()
  process.send(subject1, 1)
  process.send(subject2, 2)

  let selector =
    process.new_selector()
    |> process.selecting(subject1, fn(a) { #("a", a) })
    |> process.selecting(subject2, fn(a) { #("a", a) })
    |> process.merge_selector(
      process.new_selector()
      |> process.selecting(subject2, fn(a) { #("b", a) }),
    )

  let assert #("a", 1) = process.select_forever(selector)
  let assert #("b", 2) = process.select_forever(selector)
}

pub fn selecting_trapped_exits_test() {
  process.flush_messages()

  process.trap_exits(True)
  let pid = process.start(linked: True, running: fn() { process.sleep(100) })
  process.kill(pid)

  let assert Ok(process.ExitMessage(exited, process.Killed)) =
    process.new_selector()
    |> process.selecting_trapped_exits(function.identity)
    |> process.select(10)

  let assert True = pid == exited
}

pub fn flush_messages_test() {
  let subject = process.new_subject()
  process.send(subject, 1)
  process.send(subject, 2)
  process.send(subject, 3)
  process.flush_messages()
  let assert Error(Nil) = process.receive(subject, 0)
}

pub fn register_name_taken_test() {
  let taken_name = atom.create_from_string("code_server")
  let assert Ok(a) = process.named(taken_name)
  let assert Error(Nil) = process.register(process.self(), taken_name)
  let assert Ok(b) = process.named(taken_name)
  let assert True = a == b
}

pub fn register_name_test() {
  let name = atom.create_from_string("register_name_test_name")
  let _ = process.unregister(name)
  let assert Error(Nil) = process.named(name)
  let assert Ok(Nil) = process.register(process.self(), name)
  let assert Ok(pid) = process.named(name)
  let assert True = pid == process.self()
  let _ = process.unregister(name)
}

pub fn unregister_name_test() {
  let name = atom.create_from_string("unregister_name_test_name")
  let _ = process.unregister(name)
  let assert Ok(Nil) = process.register(process.self(), name)
  let assert Ok(_) = process.named(name)
  let assert Ok(Nil) = process.unregister(name)
  let assert Error(Nil) = process.named(name)
  let _ = process.unregister(name)
}

pub fn pid_from_dynamic_test() {
  let result =
    process.self()
    |> dynamic.from
    |> process.pid_from_dynamic
  let assert True = result == Ok(process.self())

  let assert Error([DecodeError(expected: "Pid", found: "Int", path: [])]) =
    1
    |> dynamic.from
    |> process.pid_from_dynamic

  let assert Error([DecodeError(expected: "Pid", found: "List", path: [])]) =
    []
    |> dynamic.from
    |> process.pid_from_dynamic
}
