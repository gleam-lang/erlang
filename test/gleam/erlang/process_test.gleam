import gleam/erlang/process

pub fn self_test() {
  let pid = process.self()
  assert True = pid == process.self()
  assert False = pid == process.start(True, fn() { Nil })
  // TODO: call self within another process and test it is different
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
    True,
    fn() {
      process.send(subject, 1)
      process.send(subject, 2)
    },
  )

  // Assert all the messages arrived
  assert Ok(0) = process.receive(subject, 0)
  assert Ok(1) = process.receive(subject, 50)
  assert Ok(2) = process.receive(subject, 0)
  assert Error(Nil) = process.receive(subject, 0)
}
