defmodule Mix.Tasks.Advent2.Run do
  use Mix.Task

  def run(_) do
    Advent.decompressDeep(File.read!("input.txt"))
      |> IO.inspect
  end
end