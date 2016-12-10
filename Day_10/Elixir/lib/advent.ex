defmodule Advent do
  def run({bots, outputs}, command) do
    cond do
      String.slice(command, 0..4) === "value" ->
        [value, bot] = Regex.scan(~r/\d+/, command) |> List.flatten |> Enum.map(&String.to_integer/1)
        {
          Map.update(bots, bot, {nil, [value]}, fn {instructions, values} ->
            {instructions, [value | values]}
          end),
          outputs
        }

      String.contains?(command, "low to bot") && String.contains?(command, "and high to bot") ->
        [bot, low, high] = Regex.scan(~r/\d+/, command) |> List.flatten |> Enum.map(&String.to_integer/1)
        {
          Map.update(bots, bot, {{{:bot, low}, {:bot, high}}, []}, fn {_, values} ->
            {{{:bot, low}, {:bot, high}}, values}
          end),
          outputs
        }

      String.contains?(command, "high to bot") && String.contains?(command, "and low to bot") ->
        [bot, high, low] = Regex.scan(~r/\d+/, command) |> List.flatten |> Enum.map(&String.to_integer/1)
        {
          Map.update(bots, bot, {{{:bot, low}, {:bot, high}}, []}, fn {_, values} ->
            {{{:bot, low}, {:bot, high}}, values}
          end),
          outputs
        }

      String.contains?(command, "low to output") && String.contains?(command, "and high to output") ->
        [bot, low, high] = Regex.scan(~r/\d+/, command) |> List.flatten |> Enum.map(&String.to_integer/1)
        {
          Map.update(bots, bot, {{{:output, low}, {:output, high}}, []}, fn {_, values} ->
            {{{:output, low}, {:output, high}}, values}
          end),
          outputs
        }

      String.contains?(command, "high to output") && String.contains?(command, "and low to output") ->
        [bot, high, low] = Regex.scan(~r/\d+/, command) |> List.flatten |> Enum.map(&String.to_integer/1)
        {
          Map.update(bots, bot, {{{:output, low}, {:output, high}}, []}, fn {_, values} ->
            {{{:output, low}, {:output, high}}, values}
          end),
          outputs
        }

      String.contains?(command, "high to output") && String.contains?(command, "and low to bot") ->
        [bot, high, low] = Regex.scan(~r/\d+/, command) |> List.flatten |> Enum.map(&String.to_integer/1)
        {
          Map.update(bots, bot, {{{:bot, low}, {:output, high}}, []}, fn {_, values} ->
            {{{:bot, low}, {:output, high}}, values}
          end),
          outputs
        }

      String.contains?(command, "low to output") && String.contains?(command, "and high to bot") ->
        [bot, low, high] = Regex.scan(~r/\d+/, command) |> List.flatten |> Enum.map(&String.to_integer/1)
        {
          Map.update(bots, bot, {{{:output, low}, {:bot, high}}, []}, fn {_, values} ->
            {{{:output, low}, {:bot, high}}, values}
          end),
          outputs
        }
    end
  end

  def consume({bots, outputs}) do
    {bots, outputs} = List.foldl(
      Map.to_list(bots),
      {bots, outputs},
      fn({number, {{{bot_or_output_low, low}, {bot_or_output_high, high}}, vals}}, {bots, outputs}) ->
        case Enum.count(vals) === 2 do
          true ->
            # need to get high value and low value from vals
            low_value = Enum.min(vals)
            high_value = Enum.max(vals)

            case low_value === 17 && high_value === 61 do
              true -> IO.inspect "bot for #1: #{number}"
              _ -> nil
            end

            # remove values, give low, give high
            bots = Map.update!(bots, number, fn {instructions, _} ->
              {instructions, []}
            end)

            bots = case bot_or_output_low === :bot do
              true -> Map.update!(bots, low, fn {instructions, values} ->
                {instructions, [low_value | values]}
              end)
              false -> bots
            end

            bots = case bot_or_output_high === :bot do
              true -> Map.update!(bots, high, fn {instructions, values} ->
                {instructions, [high_value | values]}
              end)
              false -> bots
            end

            outputs = case bot_or_output_low === :output do
              true -> Map.update(outputs, low, {nil, [low_value]}, fn {instructions, values} ->
                {instructions, [low_value | values]}
              end)
              false -> outputs
            end

            outputs = case bot_or_output_high === :output do
              true -> Map.update(outputs, high, {nil, [high_value]}, fn {instructions, values} ->
                {instructions, [high_value | values]}
              end)
              false -> outputs
            end

            {bots, outputs}
           false ->
            {bots, outputs}
        end
      end
    )

    stillFat = bots |> Enum.filter(fn {_, {_, values}} ->
      Enum.count(values) === 2
    end) |> Enum.count > 0

    case stillFat do
      true -> consume({bots, outputs})
      false -> {bots, outputs}
    end
  end
end
