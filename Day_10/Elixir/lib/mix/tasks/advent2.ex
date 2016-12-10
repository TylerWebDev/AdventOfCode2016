defmodule Mix.Tasks.Advent2.Run do
  use Mix.Task

  def run(_) do
    {_, outputs} = List.foldl(
        File.read!("input.txt") |> String.split("\n"),
        {%{}, %{}},
        fn command, set ->
          Advent.run(set, command)
        end
      )
      |> Advent.consume

    first = Map.get(outputs, 0) |> elem(1) |> List.first
    second = Map.get(outputs, 1) |> elem(1) |> List.first
    third = Map.get(outputs, 2) |> elem(1) |> List.first

    IO.inspect {first, second, third}
    IO.inspect (first * second) * third
  end
end