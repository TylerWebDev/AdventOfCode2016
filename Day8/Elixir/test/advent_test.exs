defmodule AdventTest do
  use ExUnit.Case
  doctest Advent

  test "can use commands and draw screen" do
#    Advent.screen
#      |> Advent.rect(3, 2)
#      |> Advent.rotateColumn(1, 1)
#      |> Advent.rotateRow(0, 4)
#      |> Advent.rotateColumn(1, 4)
#      |> Advent.draw

    Advent.screen
      |> Advent.run("rect 3x2")
      |> Advent.run("rotate column x=1 by 1")
      |> Advent.run("rotate row y=0 by 4")
      |> Advent.run("rotate column x=1 by 4")
      |> Advent.draw
  end
end
