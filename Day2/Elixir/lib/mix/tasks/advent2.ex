defmodule Mix.Tasks.Advent2.Run do
  use Mix.Task

  def run(_) do
    Advent.getCode(String.split(File.read!("input.txt"), "\n"), SecondKeypad) |> IO.inspect
  end
end