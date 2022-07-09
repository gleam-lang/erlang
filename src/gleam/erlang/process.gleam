import gleam/erlang.{Reference}
import gleam/dynamic.{Dynamic}

/// A `Pid` (or Process identifier) is a reference to an Erlang process. Each
/// process has a `Pid` and it is one of the lowest level building blocks of
/// inter-process communication in the Erlang and Gleam OTP frameworks.
///
pub external type Pid

/// Get the `Pid` for the current process.
pub external fn self() -> Pid =
  "erlang" "self"

/// Create a new Erlang process that runs concurrently to the creator. In other
/// languages this might be called a fibre, a green thread, or a coroutine.
///
/// If `linked` is `True` then the created process is linked to the creator
/// process. When a process terminates an exit signal is sent to all other
/// processes that are linked to it, causing the process to either terminate or
/// have to handle the signal.
///
/// More can be read about processes and links in the [Erlang documentation][1].
///
/// [1]: https://www.erlang.org/doc/reference_manual/processes.html
///
pub fn start(running implementation: fn() -> anything, linked link: Bool) -> Pid {
  case link {
    True -> spawn_link(implementation)
    False -> spawn(implementation)
  }
}

external fn spawn(fn() -> anything) -> Pid =
  "erlang" "spawn"

external fn spawn_link(fn() -> anything) -> Pid =
  "erlang" "spawn_link"

/// A `Subject` is a value that processes can use to send and receive messages
/// to and from each other in a well typed way.
///
/// Each subject is "owned" by the process that created it. Any process can use
/// the `send` function to sent a message of the correct type to the process
/// that owns the subject, and the owner can use the `receive` function or the
/// `Selector` type to receive these messages.
///
/// The `Subject` type is similar to the "channel" types found in other
/// languages and the "topic" concept found in some pub-sub systems.
///
/// # Examples
///
/// ```gleam
/// let subject = new_subject()
///
/// // Send a message with the subject
/// send(subject, "Hello, Joe!")
///
/// // Receive the message
/// receive(subject, within: 10)
/// ```
///
pub opaque type Subject(message) {
  Subject(owner: Pid, tag: Reference)
}

/// Create a new `Subject` owned by the current process.
///
pub fn new_subject() -> Subject(message) {
  Subject(owner: self(), tag: erlang.make_reference())
}

/// Get the owner process for a `Subject`. This is the process that created the
/// `Subject` and will receive messages sent with it.
///
pub fn subject_owner(subject: Subject(message)) -> Pid {
  subject.owner
}

external type DoNotLeak

external fn raw_send(Pid, message) -> DoNotLeak =
  "erlang" "send"

/// Send a message to a process using a `Subject`. The message must be of the
/// type that the `Subject` accepts.
///
/// This function does not wait for the `Subject` owner process to call the
/// `receive` function, instead it returns once the message has been placed in
/// the process' mailbox.
///
/// # Ordering
///
/// If process P1 sends two messages to process P2 it is guarenteed that process
/// P1 will receive the messages in the order they were sent.
///
/// If you wish to receive the messages in a different order you can send them
/// on two different subjects and the receiver function can call the `receive`
/// function for each subject in the desired order, or you can write some Erlang
/// code to perform a selective receive.
///
/// # Examples
///
/// ```gleam
/// let subject = new_subject()
/// send(subject, "Hello, Joe!")
/// ```
///
pub fn send(subject: Subject(message), message: message) -> Nil {
  raw_send(subject.owner, #(subject.tag, message))
  Nil
}

/// Receive a message that has been sent to current process using the `Subject`.
///
/// If there is not an existing message for the `Subject` in the process'
/// mailbox or one does not arrive `within` the permitted timeout then the
/// `Error(Nil)` is returned.
///
/// Only the process that is owner of the `Subject` can receive a message using
/// it. If a process that does not own the `Subject` attempts to receive with it
/// then it will not receive a message.
///
/// To wait for messages from multiple `Subject`s at the same time see the
/// `Selector` type.
///
pub fn receive(
  from subject: Subject(message),
  within milliseconds: Int,
) -> Result(message, Nil) {
  new_selector()
  |> selecting(subject, fn(x) { x })
  |> select(within: milliseconds)
}

/// A type that enables a process to wait for messages from multiple `Subject`s
/// at the same time, returning whichever message arrives first.
///
/// Used with the `new_selector`, `selecting`, and `select` functions.
///
/// # Examples
///
/// ```gleam
/// > let int_subject = new_subject()
/// > let float_subject = new_subject()
/// > send(int_subject, 1)
/// >
/// > let selector =
/// >   new_selector()
/// >   |> selecting(int_subject, int.to_string)
/// >   |> selecting(float_subject, float.to_string)
/// >
/// > select(selector)
/// Ok("1")
/// ```
///
pub external type Selector(payload)

/// Create a new `Selector` which can be used to receive messages on multiple
/// `Subject`s at once.
///
pub external fn new_selector() -> Selector(payload) =
  "gleam_erlang_ffi" "new_selector"

/// Receive a message that has been sent to current process using any of the
/// `Subject`s that have been added to the `Selector` with the `selecting`
/// function.
///
/// If there is not an existing message for the `Selector` in the process'
/// mailbox or one does not arrive `within` the permitted timeout then the
/// `Error(Nil)` is returned.
///
/// Only the process that is owner of the `Subject`s can receive a message using
/// them. If a process that does not own the a `Subject` attempts to receive
/// with it then it will not receive a message.
///
pub external fn select(
  from: Selector(payload),
  within: Int,
) -> Result(payload, Nil) =
  "gleam_erlang_ffi" "select"

/// Add a new `Subject` to the `Selector` to that it's messages can be received.
///
/// The `mapping` function provided with the `Subject` can be used to convert
/// the type of messages received using this `Subject`. This is useful for when
/// you wish to add multiple `Subject`s to a `Seletor` when they have differing
/// message types. If you do not wish to transform the incoming messages in any
/// way then the `identity` function can be given.
///
pub fn selecting(
  selector: Selector(payload),
  for subject: Subject(message),
  mapping transform: fn(message) -> payload,
) -> Selector(payload) {
  insert_selector_handler(selector, subject.tag, transform)
}

external fn insert_selector_handler(
  Selector(payload),
  for: tag,
  mapping: fn(message) -> payload,
) -> Selector(payload) =
  "gleam_erlang_ffi" "insert_selector_handler"

/// Suspends the process calling this function for the specified number of
/// milliseconds.
///
pub external fn sleep(Int) -> Nil =
  "gleam_erlang_ffi" "sleep"

/// Suspends the process forever! This may be useful for suspending the main
/// process in a Gleam program when it has no more work to do but we want other
/// processes to continue to work.
///
pub external fn sleep_forever() -> Nil =
  "gleam_erlang_ffi" "sleep_forever"

/// Check to see whether the process for a given `Pid` is alive.
///
/// See the [Erlang documentation][1] for more information.
///
/// [1]: http://erlang.org/doc/man/erlang.html#is_process_alive-1
///
pub external fn is_alive(Pid) -> Bool =
  "erlang" "is_process_alive"

type ProcessMonitorFlag {
  Process
}

external fn erlang_monitor_process(ProcessMonitorFlag, Pid) -> Reference =
  "erlang" "monitor"

pub opaque type ProcessMonitor {
  ProcessMonitor(tag: Reference)
}

/// A message received when a monitored process exits.
///
pub type ProcessDown {
  ProcessDown(pid: Pid, reason: Dynamic)
}

/// Start monitoring a process so that when the monitored process exits a
/// message is to the monitoring process.
///
/// The message is only sent once, when the target process exits. If the
/// process was not alive when this function is called the message will never
/// be received.
///
/// The down message can be received with a `Selector` and the
/// `selecting_process_down` function.
///
/// The process can be demonitored with the `demonitor_process` function.
///
pub fn monitor_process(pid: Pid) -> ProcessMonitor {
  Process
  |> erlang_monitor_process(pid)
  |> ProcessMonitor
}

/// Add a `ProcessMonitor` to a `Selector` so that the `ProcessDown` message can
/// be received using the `Selector` and the `select` function.
///
pub fn selecting_process_down(
  selector: Selector(payload),
  monitor: ProcessMonitor,
  mapping: fn(ProcessDown) -> payload,
) -> Selector(payload) {
  insert_selector_handler(selector, monitor.tag, mapping)
}

/// Remove the monitor for a process so that when the monitor process exits a
/// `ProcessDown` message is not sent to the monitoring process.
///
/// If the message has already been sent it is removed from the monitoring
/// process' mailbox.
///
pub external fn demonitor_process(monitor: ProcessMonitor) -> Nil =
  "gleam_erlang_ffi" "demonitor"

/// An error returned when making a call to a process.
///
pub type CallError(msg) {
  /// The process being called exited before it sent a response.
  ///
  CalleeDown(reason: Dynamic)

  /// The process being called did not response within the permitted amount of
  /// time.
  ///
  CallTimeout
}

// This function is based off of Erlang's gen:do_call/4.
/// Send a message to a process and wait for a reply.
///
/// If the receiving process exits or does not reply within the allowed amount
/// of time then an error is returned.
///
pub fn try_call(
  subject: Subject(request),
  make_request: fn(Subject(response)) -> request,
  within timeout: Int,
) -> Result(response, CallError(response)) {
  let reply_subject = new_subject()

  // Monitor the callee process so we can tell if it goes down (meaning we
  // won't get a reply)
  let monitor = monitor_process(subject_owner(subject))

  // Send the request to the process over the channel
  send(subject, make_request(reply_subject))

  // Await a reply or handle failure modes (timeout, process down, etc)
  let result =
    new_selector()
    |> selecting(reply_subject, Ok)
    |> selecting_process_down(
      monitor,
      fn(down: ProcessDown) { Error(CalleeDown(reason: down.reason)) },
    )
    |> select(timeout)

  // Demonitor the process and close the channels as we're done
  demonitor_process(monitor)

  // Prepare an appropriate error (if present) for the caller
  case result {
    Error(Nil) -> Error(CallTimeout)
    Ok(res) -> res
  }
}

// TODO: changelog
/// Send a message to a process and wait for a reply.
///
/// If the receiving process exits or does not reply within the allowed amount
/// of time the calling process crashes. If you wish an error to be returned
/// instead see the `try_call` function.
///
pub fn call(
  subject: Subject(request),
  make_request: fn(Subject(response)) -> request,
  within timeout: Int,
) -> response {
  assert Ok(resp) = try_call(subject, make_request, timeout)
  resp
}
