// TODO: test this module.
// Default setting is false for SASL compatible https://erlang.org/doc/man/kernel_app.html#logger_sasl_compatible
import gleam
import gleam/erlang/atom.{Atom}
import gleam/dynamic.{Dynamic}
import gleam/map.{Map}
import gleam/result
import gleam/io
import gleam/erlang.{Stacktrace}

// These are the log levels used in the erlang logger
// https://erlang.org/doc/apps/kernel/logger_chapter.html#log-level
// They match with a set defined in the Syslog Protocol, RFC 5424
pub type Level {
  Emergency
  Alert
  Critical
  Error
  Warning
  Notice
  Info
  Debug
}

pub fn log_level_from_dynamic(raw: Dynamic) {
  let emergency = dynamic.from(Emergency)
  let alert = dynamic.from(Alert)
  let critical = dynamic.from(Critical)
  let error = dynamic.from(Error)
  let warning = dynamic.from(Warning)
  let notice = dynamic.from(Notice)
  let info = dynamic.from(Info)
  let debug = dynamic.from(Debug)

  case raw {
    a if a == emergency -> Ok(Emergency)
    a if a == alert -> Ok(Alert)
    a if a == critical -> Ok(Critical)
    a if a == error -> Ok(Error)
    a if a == warning -> Ok(Warning)
    a if a == notice -> Ok(Notice)
    a if a == info -> Ok(Info)
    a if a == debug -> Ok(Debug)
    _ -> gleam.Error("Not a valid logger level")
  }
}

// Erl log event is this or string or format plus args
pub type Event {
  Event(level: Level, meta: Map(Atom, Dynamic), message: Message)
}

// https://erlang.org/doc/man/logger.html#HModule:log-2
// https://erlang.org/doc/man/logger.html#type-log_event
pub fn cast_log_event(raw) {
  try level = dynamic.field(raw, atom.create_from_string("level"))
  try level = log_level_from_dynamic(level)

  try meta = dynamic.field(raw, atom.create_from_string("meta"))
  // Documentation says this field must be in format Map(Atom, term)
  let metadata: Map(Atom, Dynamic) = dynamic.unsafe_coerce(meta)

  try msg = dynamic.field(raw, atom.create_from_string("msg"))
  try message = log_message_from_dynamic(msg)
  Event(level, metadata, message)
  |> Ok
}

pub type Message {
  Format(string: String, args: List(Dynamic))
  Report(Dynamic)
  String(String)
}

pub fn log_message_from_dynamic(raw: Dynamic) {
  try first = dynamic.element(raw, 0)
  try second = dynamic.element(raw, 1)

  let report = atom.create_from_string("report")
  let string = atom.create_from_string("string")

  case atom.from_dynamic(first) {
    Ok(k) if k == report -> Ok(Report(second))
    Ok(k) if k == string -> {
      try str = dynamic.string(second)
      Ok(String(str))
    }
    _ -> {
      try format_str = dynamic.string(first)
      try args = dynamic.list(second)
      Ok(Format(format_str, args))
    }
  }
}

external fn erl_add_handler(Atom, Atom, Map(Atom, Dynamic)) -> Dynamic =
  "logger" "add_handler"

pub fn add_handler(handler: fn(Dynamic, Stacktrace, Int) -> Nil) -> Nil {
  let handler_module = atom.create_from_string("gleam@erlang@logger@handler")
  let config =
    map.from_list([
      #(atom.create_from_string("handler"), dynamic.from(handler)),
      #(
        atom.create_from_string("level"),
        dynamic.from(atom.create_from_string("error")),
      ),
    ])
  erl_add_handler(handler_module, handler_module, config)
  |> io.debug
  Nil
}
