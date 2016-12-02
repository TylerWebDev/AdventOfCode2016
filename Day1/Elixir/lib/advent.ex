defmodule Advent do
    def one(movelist) do
        {_, finalX, finalY} = List.foldl(
            Advent.getMoves(movelist),
            {"N", 0, 0},
            &move/2
        )

        abs(finalX) + abs(finalY)
    end

    def two(movelist) do
        {{_, finalX, finalY}, locations} = List.foldl(
            Advent.getMoves(movelist),
            {{"N", 0, 0}, []},
            (fn {leftOrRight, value}, {{direction, x, y}, locations} ->
              {Advent.move({leftOrRight, value}, {direction, x, y}), [{x, y} | locations]}
            end)
        )

        locations = Enum.reverse([{finalX, finalY} | locations])

        locations
          |> Advent.granularizeLocations
          |> Advent.getFirstDuplicate
          |> (fn {x, y} ->
            abs(x) + abs(y)
          end).()
    end

    def getFirstDuplicate(locations) do
        {_, duped} = List.foldl(
          locations,
          {[], nil},
          (fn location, {acc, duped} ->
            if duped do
              {acc, duped}
            else
              added = [location | acc]

              case Enum.count(Enum.uniq(added)) === Enum.count(acc) do
                true -> {acc, location}
                false -> {added, nil}
              end
            end
          end)
        )

        duped
    end

    def granularizeLocations([{firstX, firstY} | tail]) do
        {granular, _} = List.foldl(
          tail,
          {[], {firstX, firstY}},
          (fn {x, y}, {locations, {prevX, prevY}} ->
            [_ | granular] = case x === prevX do
              true -> Enum.zip(List.duplicate(x, abs(prevY - y) + 1), prevY..y)
              false -> Enum.zip(prevX..x, List.duplicate(y, abs(prevX - x) + 1))
            end

            {locations ++ granular, {x, y}}
          end)
        )

        [{firstX, firstY} | granular]
    end

    def getMoves(moves) do
       Enum.map(String.split(moves, ", "), &Advent.splitMove/1)
    end

    def splitMove(<< head::utf8, tail::binary >>) do
        {List.to_string([head]), elem(Integer.parse(tail), 0)}
    end

    def move({leftOrRight, value}, {direction, x, y}) do
      case {direction, leftOrRight} do
             {"N", "R"} -> {"E", x + value, y}
             {"S", "R"} -> {"W", x - value, y}
             {"E", "R"} -> {"S", x, y - value}
             {"W", "R"} -> {"N", x, y + value}
             {"N", "L"} -> {"W", x - value, y}
             {"S", "L"} -> {"E", x + value, y}
             {"E", "L"} -> {"N", x, y + value}
             {"W", "L"} -> {"S", x, y - value}
         end
     end
end
