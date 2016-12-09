defmodule Advent do
  def getABA(string) do
    {_, abas} = List.foldl(
      String.split(string, "", trim: true),
      {{nil, nil, nil}, []},
      fn nextCharacter, {{_, b, c}, abas} ->
        next = {b, c, nextCharacter}

        case b !== c && c !== nextCharacter && b === nextCharacter do
          true -> {next, [next | abas]}
          false -> {next, abas}
        end
      end
    )

    abas
  end

  def getOuterInner(string) do
    {outer, inner, last} = List.foldl(
        String.split(string, "", trim: true),
        {[], [], ""},
        fn character, {outer, inner, carry} ->
          case character do
            "[" -> {[carry | outer], inner, ""}
            "]" -> {outer, [carry | inner], ""}
            _ -> {outer, inner, carry <> character}
          end
        end
      )

      {[last | outer], inner}
  end

  def ssl?(string) do
    {outers, inners} = __MODULE__.getOuterInner(string)

    requiredBABS = Enum.map(outers, &__MODULE__.getABA/1)
      |> List.flatten
      |> Enum.map(fn {a, b, a} ->
       b <> a <> b
      end)

    List.foldl(
      inners,
      [],
      fn inner, passing ->
        found = Enum.filter(
          requiredBABS,
          &String.contains?(inner, &1)
        )

        [MapSet.intersection(MapSet.new(found), MapSet.new(requiredBABS)) |> MapSet.size >= 1 | passing]
      end
    ) |> Enum.filter(&(&1 === true)) |> Enum.count === 1
  end

  def isABBA?(string) do
    {isABBA, _} = List.foldl(
      String.split(string, "", trim: true),
      {false, {nil, nil, nil, nil}},
      fn nextCharacter, {abba, {a, b, c, d}} ->
        case abba === true do
          true ->
            {abba, {a, b, c, d}}
          false ->
            {b !== c && d !== nextCharacter && [b, c] === [nextCharacter, d], {b, c, d, nextCharacter}}
        end
      end
    )

    isABBA
  end

  def chunk character, {tls, str} do
    case tls do
      nil ->
        case character do
           "[" -> {(if __MODULE__.isABBA?(str), do: true, else: tls), ""}
           "]" -> {(if __MODULE__.isABBA?(str), do: false, else: tls), ""}
           _ -> {tls, str <> character}
        end
      true ->
        case character do
         "[" -> {tls, ""}
         "]" -> {(if __MODULE__.isABBA?(str), do: false, else: tls), ""}
         _ -> {tls, str <> character}
        end
      false -> {tls, ""}
    end
  end

  def tls?(string) do
    set = List.foldl(
      String.split(string, "", trim: true),
      {nil, ""},
      &__MODULE__.chunk/2
    )

    {tls, _} = __MODULE__.chunk("[", set)

    if tls === true, do: true, else: false
  end
end
