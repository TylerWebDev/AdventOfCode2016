defmodule Mix.Tasks.Advent1.Run do
  use Mix.Task

  def run(_) do
    File.read!("input.txt")
      |> String.split("\n")
      |> Enum.filter(&Advent.tls?/1)
      |> Enum.count
      |> IO.inspect
  end
end