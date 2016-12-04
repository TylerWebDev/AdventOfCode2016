defmodule FirstKeypadTest do
  use ExUnit.Case
  doctest Advent

  test "can check if valid triangle" do
    assert (Advent.valid?({1, 2, 3}) === false)
    assert (Advent.valid?({1, 5, 10}) === false)
    assert (Advent.valid?({3, 4, 5}) === true)
    assert (Advent.valid?({5, 10, 25}) === false)
  end
end
