defmodule Mix.Tasks.Advent1.Run do
  use Mix.Task

  def run(_) do
    Advent.getCode(String.split(File.read!("input.txt"), "\n"), FirstKeypad) |> IO.inspect
  end
end