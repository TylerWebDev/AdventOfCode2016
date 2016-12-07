defmodule AdventTest do
  use ExUnit.Case
  doctest Advent

  test "can get sixth position" do
    assert (Advent.sixth("123456") === "6")
  end

  test "can get seventh position" do
    assert (Advent.seventh("1234567") === "7")
  end

  test "can check if valid" do
    assert (Advent.valid?("123456") === false)
    assert (Advent.valid?("000006") === true)
  end

  test "can check if valid2" do
    assert (Advent.valid2?("1234567") === false)
    assert (Advent.valid2?("00000a7") === false)
    assert (Advent.valid2?("0000067") === true)
  end
end
