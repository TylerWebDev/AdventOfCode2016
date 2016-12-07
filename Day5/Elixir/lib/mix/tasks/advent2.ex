defmodule Mix.Tasks.Advent2.Run do
  use Mix.Task

  def attempt(doorId, index, to, agent, factor) do
    md5 = Advent.md5("#{doorId}#{index}")

    next = index + 1

    case Advent.valid2?(md5) do
      true ->
        # location
        sixth = Advent.sixth(md5)
        # character
        seventh = Advent.seventh(md5)

        location = String.to_integer(sixth)

        # add the valid value, only set to usable if the set is done
        Agent.update(agent, fn pw ->
          gathered = Map.put_new(pw, location, seventh)
          IO.inspect gathered
          IO.inspect "decrypting..."

          case Enum.count(Map.keys(gathered)) === 8 do
            true ->
              gathered
                |> Map.values
                |> Enum.join
                |> IO.inspect

              exit(:normal)
            false -> gathered
          end
        end)

        __MODULE__.attempt(doorId, next, to, agent, factor)
      false ->
        __MODULE__.attempt(doorId, next, to, agent, factor)
    end
  end

  def run(_) do
    {:ok, agent} = Agent.start_link fn -> %{} end

    # peak efficiency is ~10000
    factor = 10000
    # this needs to be a guess at the maximum needed
    # if you stall out, raise it. if you pass 150% back it up
    limit = 100000000

    Stream.take_every(0..limit, factor)
      |> Enum.map((fn(number) ->
         __MODULE__.attempt("ffykfhsq", number, number + factor, agent, factor)
      end))
  end
end