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
  let inets = atom.create_from_string("inets")
  assert Ok([app1]) = erlang.ensure_all_started(inets)
  assert True = app1 == inets

  // If they are already started then empty list is returned
  assert Ok([]) = erlang.ensure_all_started(inets)
}

pub fn ensure_all_started_unknown_test() {
  let unknown = atom.create_from_string("wibble_application")
  assert Error(UnknownApplication(problem)) = erlang.ensure_all_started(unknown)
  assert True = problem == unknown
}

pub fn sleep_test() {
  // Exists just to ensure the function does not error
  erlang.sleep(1)
}
