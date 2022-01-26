# Changelog

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
