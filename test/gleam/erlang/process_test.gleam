import gleam/int
import gleam/float
import gleam/erlang/process

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
