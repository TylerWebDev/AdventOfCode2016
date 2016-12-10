defmodule Advent do
  def decompress(decompressed, string) do
    try do
      [start, command, rest] = String.split(string, ~r{\(\d+x\d+\)}, parts: 2, include_captures: true)

      [grab, times] = String.split(command, "x", parts: 2)
         |> Enum.map(&String.replace(&1, "(", ""))
         |> Enum.map(&String.replace(&1, ")", ""))
         |> Enum.map(&String.to_integer/1)

      {repeater, tail} = String.split_at(rest, grab)

      decompress(
        decompressed <> start <> String.duplicate(repeater, times),
        tail
      )
    rescue _ in MatchError ->
        decompressed <> string
    end
  end

  def decompressDeep(string) do
    try do
      [start, command, rest] = String.split(string, ~r{\(\d+x\d+\)}, parts: 2, include_captures: true)

      [grab, times] = String.split(command, "x", parts: 2)
         |> Enum.map(&String.replace(&1, "(", ""))
         |> Enum.map(&String.replace(&1, ")", ""))
         |> Enum.map(&String.to_integer/1)

      {repeater, tail} = String.split_at(rest, grab)

      decompressDeep(tail) + (times * decompressDeep(repeater)) + String.length(start)
    rescue _ in MatchError ->
        String.length(string)
    end
  end
end
