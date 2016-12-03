defmodule Keypad do
  @type coordinatePair :: {number, number}
  @callback key(coordinatePair) :: String.t
end