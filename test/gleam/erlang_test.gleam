import gleam/dynamic
import gleam/erlang.{UnknownApplication}
import gleam/erlang/atom

pub fn term_to_binary_test() {
  let term = dynamic.from(#(1, "2", <<"hello":utf8>>))

  assert Ok(out) =
    term
    |> erlang.term_to_binary()
    |> erlang.binary_to_term()
  assert True = term == out

  assert Ok(out) =
    term
    |> erlang.term_to_binary()
    |> erlang.unsafe_binary_to_term()
  assert True = term == out

  assert Error(Nil) =
    <<>>
    |> erlang.unsafe_binary_to_term()
}

pub fn ensure_all_started_ok_test() {
  let gleam_erlang = atom.create_from_string("gleam_erlang")
  let gleam_stdlib = atom.create_from_string("gleam_stdlib")
  assert Ok([app1, app2]) = erlang.ensure_all_started(gleam_erlang)
  assert True = app1 == gleam_stdlib
  assert True = app2 == gleam_erlang

  // If they are already started then empty list is returned
  assert Ok([]) = erlang.ensure_all_started(gleam_erlang)
}

pub fn ensure_all_started_unknown_test() {
  let unknown = atom.create_from_string("wibble_application")
  assert Error(UnknownApplication(problem)) = erlang.ensure_all_started(unknown)
  assert True = problem == unknown
}
