import gleam/int
import gleam/float
import gleam/dynamic
import gleam/erlang/process.{ProcessDown}

pub fn self_test() {
  let subject = process.new_subject()
  let pid = process.self()

  assert True = pid == process.self()
  assert False = pid == process.start(fn() { Nil }, linked: True)

  process.start(fn() { process.send(subject, process.self()) }, linked: True)
  assert Ok(child_pid) = process.receive(subject, 5)
  assert True = child_pid != process.self()
}

pub fn sleep_test() {
  // Exists just to ensure the function does not error
  process.sleep(1)
}

pub fn subject_owner_test() {
  let subject = process.new_subject()
  assert True = process.subject_owner(subject) == process.self()
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
  assert Ok(0) = process.receive(subject, 0)
  assert Ok(1) = process.receive(subject, 50)
  assert Ok(2) = process.receive(subject, 0)
  assert Error(Nil) = process.receive(subject, 0)
}

pub fn is_alive_test() {
  let pid = process.start(fn() { Nil }, False)
  process.sleep(5)
  assert False = process.is_alive(pid)
}

pub fn sleep_forever_test() {
  let pid = process.start(process.sleep_forever, False)
  process.sleep(5)
  assert True = process.is_alive(pid)
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
  assert Ok("2") = process.select(selector, 0)
  assert Ok("3.0") = process.select(selector, 0)
  assert Error(Nil) = process.select(selector, 0)

  // More messages for subjects 2 and 3
  process.send(subject2, 2)
  process.send(subject3, 3.0)

  // Include subject 1 also
  let selector = process.selecting(selector, subject1, fn(x) { x })

  // Now we get the message for subject 1 first as it is first in the mailbox
  assert Ok("1") = process.select(selector, 0)
  assert Ok("2") = process.select(selector, 0)
  assert Ok("3.0") = process.select(selector, 0)
  assert Error(Nil) = process.select(selector, 0)
}

pub fn monitor_test() {
  // Spawn child
  let parent_subject = process.new_subject()
  let pid =
    process.start(
      linked: False,
      running: fn() {
        let subject = process.new_subject()
        process.send(parent_subject, subject)
        // Wait for the parent to send a message before exiting
        process.receive(subject, 150)
      },
    )

  // Monitor child
  let monitor = process.monitor_process(pid)
  let selector =
    process.new_selector()
    |> process.selecting_process_down(monitor, fn(x) { x })

  // There is no monitor message while the child is alive
  assert Error(Nil) = process.select(selector, 0)

  // Shutdown child to trigger monitor
  assert Ok(child_subject) = process.receive(parent_subject, 50)
  process.send(child_subject, Nil)

  // We get a process down message!
  assert Ok(ProcessDown(downed_pid, _reason)) = process.select(selector, 50)

  assert True = downed_pid == pid
}

pub fn demonitor_test() {
  // Spawn child
  let parent_subject = process.new_subject()
  let pid =
    process.start(
      linked: False,
      running: fn() {
        let subject = process.new_subject()
        process.send(parent_subject, subject)
        // Wait for the parent to send a message before exiting
        process.receive(subject, 150)
      },
    )

  // Monitor child
  let monitor = process.monitor_process(pid)
  let selector =
    process.new_selector()
    |> process.selecting_process_down(monitor, fn(x) { x })

  // Shutdown child to trigger monitor
  assert Ok(child_subject) = process.receive(parent_subject, 50)
  process.send(child_subject, Nil)

  // Demonitor the child
  process.demonitor_process(monitor)

  // There is no down message
  assert Error(Nil) = process.select(selector, 5)
}

pub fn try_call_test() {
  let parent_subject = process.new_subject()

  process.start(
    linked: True,
    running: fn() {
      // Send the child subject to the parent so it can call the child
      let child_subject = process.new_subject()
      process.send(parent_subject, child_subject)
      // Wait for the subject to be messaged
      assert Ok(#(x, reply)) = process.receive(child_subject, 50)
      // Reply
      process.send(reply, x + 1)
    },
  )

  assert Ok(call_subject) = process.receive(parent_subject, 50)

  // Call the child process and get a response.
  assert Ok(2) =
    process.try_call(call_subject, fn(subject) { #(1, subject) }, 50)
}

pub fn try_call_timeout_test() {
  let parent_subject = process.new_subject()

  process.start(
    linked: True,
    running: fn() {
      // Send the call subject to the parent
      let child_subject = process.new_subject()
      process.send(parent_subject, child_subject)
      // Wait for the subject to be called
      assert Ok(_) = process.receive(child_subject, 50)
      // Never reply
      process.sleep(100)
    },
  )

  assert Ok(call_subject) = process.receive(parent_subject, 50)

  // Call the child process over the subject
  assert Error(process.CallTimeout) =
    process.try_call(call_subject, fn(x) { x }, 10)
}

pub fn call_test() {
  let parent_subject = process.new_subject()

  process.start(
    linked: True,
    running: fn() {
      // Send the child subject to the parent so it can call the child
      let child_subject = process.new_subject()
      process.send(parent_subject, child_subject)
      // Wait for the subject to be messaged
      assert Ok(#(x, reply)) = process.receive(child_subject, 50)
      // Reply
      process.send(reply, x + 1)
    },
  )

  assert Ok(call_subject) = process.receive(parent_subject, 50)

  // Call the child process and get a response.
  assert 2 = process.call(call_subject, fn(subject) { #(1, subject) }, 50)
}

external fn send(process.Pid, anything) -> Nil =
  "erlang" "send"

fn subjectless_receive(tag, size) {
  process.new_selector()
  |> process.selecting_subjectless_record(tag, size, dynamic.unsafe_coerce)
  |> process.select(0)
}

pub fn selecting_subjectless_record_test() {
  send(process.self(), #("a", 1))
  send(process.self(), #("b", 2, 3))
  send(process.self(), #("c", 4, 5, 6))
  send(process.self(), "d")

  assert Error(Nil) = subjectless_receive("a", 0)
  assert Error(Nil) = subjectless_receive("d", 0)
  assert Error(Nil) = subjectless_receive("d", 1)

  assert Error(Nil) = subjectless_receive("c", 1)
  assert Error(Nil) = subjectless_receive("c", 2)
  assert Error(Nil) = subjectless_receive("c", 3)
  assert Ok(#("c", 4, 5, 6)) = subjectless_receive("c", 4)

  assert Ok(#("b", 2, 3)) = subjectless_receive("b", 3)

  assert Ok(#("a", 1)) = subjectless_receive("a", 2)
}
