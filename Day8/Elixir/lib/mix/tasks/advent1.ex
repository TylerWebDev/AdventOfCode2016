defmodule Mix.Tasks.Advent1.Run do
  use Mix.Task

  def run(_) do
    commands = File.read!("input.txt")
      |> String.split("\n")

    List.foldl(
      commands,
      Advent.screen,
      fn command, screen ->
        Advent.run(screen, command)
      end
    ) |> Map.values
      |> Enum.map(&Map.values/1)
      |> List.flatten
      |> Enum.filter(&(&1 === "#"))
      |> Enum.count
      |> IO.inspect
  end
end