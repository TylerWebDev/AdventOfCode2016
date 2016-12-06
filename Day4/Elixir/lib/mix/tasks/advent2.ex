defmodule Mix.Tasks.Advent2.Run do
  use Mix.Task

  def run(_) do
    json = File.read!("input.txt")
      |> String.split("\n")
      |> Enum.map(&Advent.splitIntoParts/1)
      |> Enum.filter(fn({letters, _, checksum, _}) ->
        checksum === Advent.checksum(letters)
      end)
      |> Enum.map(fn({_, number, checksum, message}) ->
        "#{number}-" <> Advent.decrypt "#{message}-#{number}[#{checksum}]}"
      end)
      |> Poison.encode!(pretty: true)

    File.write!("output.json", json)
  end
end