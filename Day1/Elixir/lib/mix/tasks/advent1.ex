defmodule Mix.Tasks.Advent1.Run do
  use Mix.Task

  def run(_) do
    File.read!("input.txt") |> Advent.one |> IO.inspect
  end
end