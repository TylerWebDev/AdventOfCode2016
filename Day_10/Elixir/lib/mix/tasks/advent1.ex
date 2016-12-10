defmodule Mix.Tasks.Advent1.Run do
  use Mix.Task

  def run(_) do
    List.foldl(
        File.read!("input.txt") |> String.split("\n"),
        {%{}, %{}},
        fn command, set ->
          Advent.run(set, command)
        end
      )
      |> Advent.consume
  end
end