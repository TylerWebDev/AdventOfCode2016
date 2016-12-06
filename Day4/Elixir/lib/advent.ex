defmodule Advent do
  def splitIntoParts(string) do
    # aaaaa-bbb-z-y-x-123[abxyz]
    # {[{"a", 6}, {"b", 3}, {"z", 1}, {"y", 1}, {"x", 1}], 123, "abxyz", "aaaaa-bbb-z-y-x"}
    split = string
      |> String.split("-")

    [number, checksum] = (List.last split) |> String.split("[") |> (Enum.map(&String.trim_trailing(&1, "]")))

    List.foldl(
      split |> Enum.reverse |> fn [_ | tail] -> tail end.() |> Enum.reverse,
      {%{}, number, checksum},
      fn(str, {letters, number, checksum}) ->
        updatedLetters = List.foldl(
          String.split(str, "", trim: true),
          letters,
          (fn(letter, acc) ->
            Map.get_and_update(acc, letter, fn(current_value) ->
              {current_value, (if current_value, do: current_value + 1, else: 1)}
            end) |> elem(1)
          end)
        )

        {updatedLetters, number, checksum}
      end
    ) |> fn {letters, number, checksum} -> {
      letters,
      String.to_integer(number),
      checksum,
      List.first(String.split(string, ~r{-\d+}))
      } end.()
  end

  def checksum(letters) do
    Enum.sort(Map.to_list(letters), fn({letterA, countA}, {letterB, countB}) ->
      case countA === countB do
        true -> letterB > letterA
        false -> countA > countB
      end
    end) |> Enum.map(fn {letter, _} -> letter end) |> Enum.take(5) |> Enum.join("")
  end

  def rotateBy(rotations, stringLetter) do
    ascii = stringLetter |> String.to_charlist |> List.first

    to_add = rem rotations, 26

    case (ascii + to_add) > 122 do
      false -> <<ascii + to_add>>
      true -> <<97 + ((ascii + to_add) - 123)>>
    end
  end

  def decrypt(string) do
    {_, number, _, message} = __MODULE__.splitIntoParts(string)

    Enum.map(String.split(message, "", trim: true), fn(letter) ->
      decryptedLetter = __MODULE__.rotateBy(number, letter)
      integerValue = List.first (to_charlist decryptedLetter);
      case integerValue < 97 || integerValue > 122 do
        true -> " "
        false -> decryptedLetter
      end
    end) |> Enum.join
  end
end
