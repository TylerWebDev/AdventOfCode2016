defmodule Mix.Tasks.Advent1.Run do
  use Mix.Task

  def attempt(_, index, to, agent, factor) when (index === to) do
    Agent.update(agent, fn pw ->
      Enum.map(
        pw,
        fn {character, index, active} ->
          case index <= to && index >= to - factor do
            true -> {character, index, true}
            false -> {character, index, active}
          end
        end
      )
    end)
  end
  def attempt(doorId, index, to, agent, factor) do
    md5 = Advent.md5("#{doorId}#{index}")

    next = index + 1

    case Advent.valid?(md5) do
      true ->
        sixth = Advent.sixth(md5)

        # add the valid value, only set to usable if the set is done
        Agent.update(agent, fn pw ->
          gathered = [{sixth, index, index === to} | pw]
          IO.inspect "decrypting...#{(Enum.count(gathered) / 8) * 100}%"
          gathered
        end)

        __MODULE__.attempt(doorId, next, to, agent, factor)
      false ->
        __MODULE__.attempt(doorId, next, to, agent, factor)
    end
  end

  def check(agent) do
    gathered = Agent.get(agent, fn a -> a end)

    # check all true and length is 8
    case Enum.count(gathered) >= 8 && Enum.count(Enum.filter(gathered, fn {_, _, active} -> active end)) >= 8 do
      true ->
        gathered
          |> Enum.sort(fn {_, indexA, _}, {_, indexB, _}  -> indexA < indexB end)
          |> Enum.map(fn {character, _, _} -> character end)
          |> Enum.join
          |> String.slice(0..7)
          |> IO.inspect

        exit(:normal)
      false ->
        :timer.sleep(1000)
        Task.start(check(agent))
    end
  end

  def run(_) do
    {:ok, agent} = Agent.start_link fn -> [] end

    # peak efficiency is ~10000
    factor = 10000
    # this needs to be a guess at the maximum needed
    # if you stall out, raise it. if you pass 150% back it up
    limit = 10000000

    Stream.take_every(0..limit, factor)
      |> Enum.map((fn(number) ->
         Task.start(fn -> __MODULE__.attempt("ffykfhsq", number, number + factor, agent, factor) end)
      end))

    check(agent)
  end
end