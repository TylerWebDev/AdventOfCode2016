defmodule Mix.Tasks.Advent1.Run do
  use Mix.Task

  def run(_) do
    File.read!("input.txt")
      |> String.split("\n")
      |> Enum.map(&String.trim/1)
      |> Enum.map(&String.split/1)
      |> Enum.map(fn [a, b, c] ->
        {String.to_integer(a), String.to_integer(b), String.to_integer(c)}
       end)
      |> Enum.filter(&Advent.valid?/1)
      |> Enum.count
      |> IO.inspect
  end
end