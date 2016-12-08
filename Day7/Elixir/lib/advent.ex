defmodule Advent do
  def isABBA?(string) do
    {abba, _} = List.foldl(
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

    abba
  end

  def tls?(string) do
    {tls, _} = List.foldl(
      String.split(string, "", trim: true),
      {nil, ""},
      fn character, {tls, str} ->
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
    )

    case tls do
      nil -> false
      _ -> tls
    end
  end
end
