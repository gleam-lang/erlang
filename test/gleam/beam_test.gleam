import gleam/beam
import gleam/should

pub fn hello_world_test() {
  beam.hello_world()
  |> should.equal("Hello, from beam!")
}
