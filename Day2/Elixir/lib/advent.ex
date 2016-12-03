defmodule Advent do
  def getCode(moveStringList, keypad) do
    {_, numbers} = List.foldl(
      moveStringList,
      {{0, 0}, []},
      fn moveString, {location, numbers} ->
        newLocation = __MODULE__.move(location, moveString, keypad)

        {newLocation, [keypad.key(newLocation) | numbers]}
      end
    )

    numbers
      |> Enum.reverse
      |> Enum.join
  end

  def move({x, y}, moveString, keypad) do
    List.foldl(
      String.split(moveString, "", trim: true),
      {x, y},
      fn move, {x, y} ->
        next = case move do
          "U" -> {x, y + 1}
          "L" -> {x - 1, y}
          "R" -> {x + 1, y}
          "D" -> {x, y - 1}
        end

        nextKey = apply(keypad, :key, [next])

        case nextKey do
          nil -> {x, y}
          _ -> next
        end
      end
    )
  end
end
