defmodule AdventTest do
  use ExUnit.Case
  doctest Advent

  test "can get moves from string directions" do
    assert (Advent.getMoves("R2") === [{"R", 2}])
    assert (Advent.getMoves("R2, L3") === [{"R", 2}, {"L", 3}])
    assert (Advent.getMoves("R2, R2, R2") === [{"R", 2}, {"R", 2}, {"R", 2}])
    assert (Advent.getMoves("R5, L5, R5, R3") === [{"R", 5}, {"L", 5}, {"R", 5}, {"R", 3}])
  end

  test "can get new direction and location from a move and a current direction and location" do
    assert (Advent.move({"R", 2}, {"N", 0, 0}) === {"E", 2, 0})
  end

  test "can get distance" do
    assert (Advent.one("R2") === 2)
    assert (Advent.one("R2, L3") === 5)
    assert (Advent.one("R2, R2, R2") === 2)
    assert (Advent.one("R5, L5, R5, R3") === 12)
  end

  test "can get first duplicate from a list of locations" do
    assert (Advent.getFirstDuplicate([{0, 0}, {1, -4}, {5, 5}, {1, -4}, {0, 0}]) === {1, -4})
  end

  test "can get distance from the first location visited twice" do
    assert (Advent.two("R8, R4, R4, R8") === 4)
  end
end
