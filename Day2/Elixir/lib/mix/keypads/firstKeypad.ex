defmodule FirstKeypad do
  @behaviour Keypad

  def key({x, y}) do
    %{
      1 => %{-1 => "1", 0 => "2", 1 => "3"},
      0 => %{-1 => "4", 0 => "5", 1 => "6"},
      -1 => %{-1 => "7", 0 => "8", 1 => "9"},
    }[y][x]
  end
end