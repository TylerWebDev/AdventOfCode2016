defmodule Mix.Tasks.Advent1.Run do
  use Mix.Task

  def run(_) do
    valid = File.read!("input.txt")
      |> String.split("\n")
      |> Enum.map(&Advent.splitIntoParts/1)
      |> Enum.filter(fn({letters, _, checksum}) ->
        checksum === Advent.checksum(letters)
      end)

    List.foldl(
      valid,
      0,
      (fn({_, number, _}, sum) ->
        sum + number
      end)
    )
    |> IO.inspect
  end
end