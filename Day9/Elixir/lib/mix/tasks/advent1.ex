defmodule Mix.Tasks.Advent1.Run do
  use Mix.Task

  def run(_) do
    Advent.decompress("", File.read!("input.txt"))
      |> String.length
      |> IO.inspect
  end
end