import gleam/erlang.{Reference}

// TODO: changelog
/// A `Pid` (or Process identifier) is a reference to an Erlang process. Each
/// process has a `Pid` and it is one of the lowest level building blocks of
/// inter-process communication in the Erlang and Gleam OTP frameworks.
///
pub external type Pid

// TODO: changelog
/// Get the `Pid` for the current process.
pub external fn self() -> Pid =
  "erlang" "self"

// TODO: changelog
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

// TODO: changelog
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
/// // Send a message via the subject
/// send(subject, "Hello, Joe!")
///
/// // Receive the message
/// receive(subject, within: 10)
/// ```
///
pub opaque type Subject(message) {
  Subject(owner: Pid, tag: Reference)
}

// TODO: changelog
/// Create a new `Subject` owned by the current process.
///
pub fn new_subject() -> Subject(message) {
  Subject(owner: self(), tag: erlang.make_reference())
}

// TODO: changelog
/// Get the owner process for a `Subject`. This is the process that will receive
/// messages sent via the `Subject`.
///
pub fn subject_owner(subject: Subject(message)) -> Pid {
  subject.owner
}

external type DoNotLeak

external fn raw_send(Pid, message) -> DoNotLeak =
  "erlang" "send"

/// Send a message to a process via a `Subject`. The message must be of the type
/// that the subject accepts.
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

// TODO: changelog
// TODO: document
pub fn receive(
  from subject: Subject(message),
  within milliseconds: Int,
) -> Result(message, Nil) {
  new_selector()
  |> selecting(subject, fn(x) { x })
  |> select(within: milliseconds)
}

// TODO: changelog
// TODO: document
pub external type Selector(payload)

// TODO: changelog
// TODO: document
pub external fn new_selector() -> Selector(payload) =
  "gleam_erlang_ffi" "new_selector"

// TODO: changelog
// TODO: document
pub external fn select(
  from: Selector(payload),
  within: Int,
) -> Result(payload, Nil) =
  "gleam_erlang_ffi" "select"

// TODO: changelog
// TODO: document
pub external fn selecting(
  Selector(payload),
  for: Subject(message),
  mapping: fn(message) -> payload,
) -> Selector(payload) =
  "gleam_erlang_ffi" "selecting"

// TODO: changelog
/// Suspends the process calling this function for the specified number of
/// milliseconds.
///
pub external fn sleep(Int) -> Nil =
  "gleam_erlang_ffi" "sleep"

// TODO: changelog
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
