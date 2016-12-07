defmodule Advent do
  def md5(string) do
    :erlang.md5(string)
      |> Base.encode16(case: :lower)
  end

  def valid?(string) do
    String.slice(string, 0..4) === "00000"
  end

  def valid2?(string) do
    zeros = String.slice(string, 0..4) === "00000"

    try do
      zeros && (String.to_integer(__MODULE__.sixth(string)) <= 7)
    rescue
      _ in ArgumentError -> false
    end
  end

  def sixth(string) do
    String.at(string, 5)
  end

  def seventh(string) do
    String.at(string, 6)
  end
end
