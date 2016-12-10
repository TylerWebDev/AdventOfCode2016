defmodule AdventTest do
  use ExUnit.Case
  doctest Advent

  test "can decompress" do
    assert (Advent.decompress("", "ADVENT") === "ADVENT")
    assert (Advent.decompress("", "ADVENT") |> String.length === 6)

    assert (Advent.decompress("", "A(1x5)BC") === "ABBBBBC")
    assert (Advent.decompress("", "A(1x5)BC") |> String.length === 7)

    assert (Advent.decompress("", "A(2x2)BCD(2x2)EFG") === "ABCBCDEFEFG")
    assert (Advent.decompress("", "A(2x2)BCD(2x2)EFG") |> String.length === 11)

    assert (Advent.decompress("", "(6x1)(1x3)A") === "(1x3)A")
    assert (Advent.decompress("", "(6x1)(1x3)A") |> String.length === 6)

    assert (Advent.decompress("", "X(8x2)(3x3)ABCY") === "X(3x3)ABC(3x3)ABCY")
    assert (Advent.decompress("", "X(8x2)(3x3)ABCY") |> String.length === 18)
  end
end
