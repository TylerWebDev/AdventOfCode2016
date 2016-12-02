defmodule Mix.Tasks.Advent2.Run do
  use Mix.Task

  def run(_) do
    File.read!("input.txt") |> Advent.two |> IO.inspect
  end
end