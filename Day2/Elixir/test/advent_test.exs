defmodule FirstKeypadTest do
  use ExUnit.Case
  doctest Advent

  test "can get new location from starting location and string directions for FirstKeypad" do
    assert (Advent.move({0, 0}, "ULL", FirstKeypad) === {-1, 1})
  end

  test "can get new location from starting location and string directions for SecondKeypad" do
    assert (Advent.move({0, 0}, "ULR", SecondKeypad) === {1, 0})
  end

  test "can convert coordinates to key for FirstKeypad" do
    assert (FirstKeypad.key({0, 0}) === "5")
    assert (FirstKeypad.key({1, 1}) === "3")
    assert (FirstKeypad.key({-1, 1}) === "1")
    assert (FirstKeypad.key({0, 1}) === "2")
    assert (FirstKeypad.key({1, -1}) === "9")
    assert (FirstKeypad.key({-3, -1}) === nil)
  end

  test "can convert coordinates to key for SecondKeypad" do
    assert (SecondKeypad.key({0, 0}) === "5")
    assert (SecondKeypad.key({1, 0}) === "6")
    assert (SecondKeypad.key({3, -2}) === nil)
    assert (SecondKeypad.key({2, 2}) === "1")
  end

  test "can take a list of string directions and get keys for FirstKeypad" do
     assert (Advent.getCode(["ULL", "RRDDD", "LURDL", "UUUUD"], FirstKeypad) === "1985")
  end

  test "can take a list of string directions and get keys for SecondKeypad" do
     assert (Advent.getCode(["ULL", "RRDDD", "LURDL", "UUUUD"], SecondKeypad) === "5DB3")
  end
end
