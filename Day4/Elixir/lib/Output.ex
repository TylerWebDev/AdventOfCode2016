defmodule Output do
  @derive [Poison.Encoder]
  defstruct [:decrypted]
end