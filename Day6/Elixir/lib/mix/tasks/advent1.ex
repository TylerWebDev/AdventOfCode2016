defmodule Mix.Tasks.Advent1.Run do
  use Mix.Task

  def run(_) do
    File.read!("input.txt")
      |> String.split("\n")
      |> Enum.map(&String.split(&1, "", trim: true))
      |> List.zip
      |> Enum.map(&Tuple.to_list/1)
      |> Enum.map(&Enum.join(&1))
      # here we want to fold it down to the most common letter
      |> Enum.map(fn letters ->
        List.foldl(
          String.split(letters, "", trim: true),
          %{},
          fn letter, letters ->Map.update(letters, letter, 1, &(&1 + 1)) end
        ) |> Map.to_list
          |> Enum.sort(fn {_, a}, {_, b} -> a > b end)
          |> List.first
          |> elem(0)
       end)
      |> Enum.join
      |> IO.inspect
  end
end