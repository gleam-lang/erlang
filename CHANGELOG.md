# Changelog

## Unreleased

- The `gleam/erlang` module gains the `Reference` type and `make_reference`
  function.
- The `gleam/erlang/process` module has been created with the `Pid`, `Subject`,
  `Selector`, and `CallError` types and the `self`, `start`, `new_subject`,
  `subject_owner`, `receive`, `new_selector`, `select`, `selecting`, `sleep`,
  `sleep_forever`, `monitoring_process`, `selecting_process_down`
  `demonitor_process`, `try_call`, `call`, `selecting_subjectless_record`, and
  `is_alive` functions.
- The `sleep` and `sleep_forever` functions have been moved to the
  `gleam/erlang/process` module.

## v0.9.3 - 2022-05-24

- Fixed some warnings with latest version of Gleam.

## v0.9.2 - 2022-02-07

- Switched from `atom_to_binary/1` to `atom_to_binary/2` to support older
  versions of Erlang.

## v0.9.1 - 2022-02-04

- Corrected the `NotUTF8` file error variant name to `NotUtf8`.

## v0.9.0 - 2022-01-26

- The `os` module gains the `family` function.
- Fixed a bug where the `erlang.sleep` function would fail.

## v0.8.0 - 2022-01-18

- Use `DecodeErrors` in the `atom` module for the `from_dynamic` function.

## v0.7.0 - 2022-01-13

- Add `is_directory`, `is_file`, `make_directory`, `list_directory`,
  `delete_directory`, `recursive_delete` functions to `file` module.
- Add `os` module with `get_all_env`, `get_env`, `set_env`, and `unset_env`
  functions.

## v0.6.0 - 2021-12-29

- Add `file` module featuring `read`, `read_bits`, `write`, `write_bits`, and
  `delete` functions.

## v0.5.0 - 2021-12-04

- The `erlang` module gains the `sleep` and `sleep_forever` functions.

## v0.4.0 - 2021-11-30

- The `erlang` module gains the `ensure_all_started` function.

## v0.3.0 - 2021-11-30

- The `erlang` module gains the `start_arguments` function.

## v0.2.0 - 2021-11-23

- Converted from rebar3 to the Gleam build tool.

## v0.1.0 - 2021-09-11

- Initial release
