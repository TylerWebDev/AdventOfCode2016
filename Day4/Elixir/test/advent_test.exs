defmodule AdventTest do
  use ExUnit.Case
  doctest Advent

  test "can get parts" do
    assert (Advent.splitIntoParts("aaaaa-bbb-z-y-x-123[abxyz]") === {%{"a" => 5, "b" => 3, "z" => 1, "y" => 1, "x" => 1}, 123, "abxyz", "aaaaa-bbb-z-y-x"})
    assert (Advent.splitIntoParts("not-a-real-room-404[oarel]") === {%{"n" => 1, "o" => 3, "t" => 1, "a" => 2, "r" => 2, "e" => 1, "l" => 1, "m" => 1}, 404, "oarel", "not-a-real-room"})
  end

  test "can get checksum" do
    assert (Advent.checksum(%{"a" => 5, "b" => 3, "z" => 1, "y" => 1, "x" => 1}) === "abxyz")
    assert (Advent.checksum(%{"a" => 5, "b" => 3, "z" => 1, "y" => 1, "x" => 1, "c" => 1}) === "abcxy")
  end

  test "can rotate letters" do
    assert (Advent.rotateBy(1, "a") === "b")
    assert (Advent.rotateBy(1, "z") === "a")
  end

  test "can decrypt" do
    assert (Advent.decrypt("qzmt-zixmtkozy-ivhz-343[qzmti]") === "very encrypted name")
  end
end
