defmodule SecondKeypad do
  @behaviour Keypad

  def key({x, y}) do
    %{
      2 => %{0 => nil, 1 => nil, 2 => "1", 3 => nil, 4 => nil},
      1 => %{0 => nil, 1 => "2", 2 => "3", 3 => "4", 4 => nil},
      0 => %{0 => "5", 1 => "6", 2 => "7", 3 => "8", 4 => "9"},
      -1 => %{0 => nil, 1 => "A", 2 => "B", 3 => "C", 4 => nil},
      -2 => %{0 => nil, 1 => nil, 2 => "D", 3 => nil, 4 => nil},
    }[y][x]
  end
end