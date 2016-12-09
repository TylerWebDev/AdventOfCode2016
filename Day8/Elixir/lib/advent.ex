defmodule Advent do
  def run(screen, string) do
    cond do
      String.contains?(string, "rect ") ->
        args = String.slice(string, 5, String.length string)
          |> String.split("x", parts: 2)
          |> Enum.map(&String.to_integer/1)
        rect(screen, (Enum.at args, 0), (Enum.at args, 1))

      String.contains?(string, "rotate column ") ->
        args = String.slice(string, 16, String.length string)
          |> String.split(" by ", parts: 2)
          |> Enum.map(&String.to_integer/1)
        rotateColumn(screen, (Enum.at args, 0), (Enum.at args, 1))

      String.contains?(string, "rotate row ") ->
        args = String.slice(string, 13, String.length string)
          |> String.split(" by ", parts: 2)
          |> Enum.map(&String.to_integer/1)
        rotateRow(screen, (Enum.at args, 0), (Enum.at args, 1))
    end
  end

  def rotateRow(screen, y, by) do
    List.foldl(
      (Enum.into 0..49, []),
      screen,
      fn index, scr ->
        val = case index - by < 0 do
          true -> 50 - abs(index - by)
          false -> index - by
        end

        put_in scr[y][index], screen[y][val]
      end
    )
  end

  def rotateColumn(screen, x, by) do
    List.foldl(
      (Enum.into 0..5, []),
      screen,
      fn index, scr ->
        val = case index - by < 0 do
          true -> 6 - abs(index - by)
          false -> index - by
        end

        put_in scr[index][x], screen[val][x]
      end
    )
  end

  def rect(screen, a, b) do
    List.foldl(
      (Enum.into 0..5, []),
      screen,
      fn col, screen ->
        put_in screen[col], List.foldl(
          (Enum.into 0..49, []),
          screen,
          fn row, scr ->
            val = case row <= a - 1 && col <= b - 1 do
              true -> "#"
              false -> screen[col][row]
            end

            put_in scr[row], val
          end
        )
      end
    )
  end

  def draw(screen) do
    Map.to_list(screen)
      |> Enum.map(fn {_, values} ->
        values |> Map.to_list |> Enum.sort |> Enum.map(&elem(&1, 1)) |> Enum.join("") |> IO.puts
      end)

    screen
  end

  def screen do
    screen = %{}

    List.foldl(
      (Enum.into 0..5, []),
      screen,
      fn col, screen ->
        put_in screen[col], List.foldl(
          (Enum.into 0..49, []),
          screen,
          (&put_in &2[&1], " ")
        )
      end
    )

#    screen = %{
#      1 => %{-1 => "1", 0 => "2", 1 => "3"},
#      0 => %{-1 => "4", 0 => "5", 1 => "6"},
#      -1 => %{-1 => "7", 0 => "8", 1 => "9"},
#    }[y][x]
  end
end
