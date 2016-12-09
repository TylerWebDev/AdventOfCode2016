defmodule Mix.Tasks.Advent2.Run do
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
    ) |> Advent.draw
  end
end