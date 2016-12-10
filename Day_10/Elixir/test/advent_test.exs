defmodule AdventTest do
  use ExUnit.Case
  doctest Advent

  test "can set values" do
    assert (Advent.run({%{}, %{}}, "value 5 goes to bot 2") === {%{2 => {nil, [5]}}, %{}})
  end

  test "can assign instructions" do
    # bots only
    assert (Advent.run({%{2 => {nil, [5]}}, %{}}, "bot 2 gives low to bot 1 and high to bot 0") === {%{2 => {{{:bot, 1}, {:bot, 0}}, [5]}}, %{}})
    assert (Advent.run({%{2 => {nil, [5]}}, %{}}, "bot 2 gives high to bot 1 and low to bot 0") === {%{2 => {{{:bot, 0}, {:bot, 1}}, [5]}}, %{}})

    # outputs only
    assert (Advent.run({%{2 => {nil, [5]}}, %{}}, "bot 2 gives low to output 1 and high to output 0") === {%{2 => {{{:output, 1}, {:output, 0}}, [5]}}, %{}})
    assert (Advent.run({%{2 => {nil, [5]}}, %{}}, "bot 2 gives high to output 1 and low to output 0") === {%{2 => {{{:output, 0}, {:output, 1}}, [5]}}, %{}})
    assert (Advent.run({%{0 => {nil, [5]}}, %{}}, "bot 0 gives high to output 2 and low to output 0") === {%{0 => {{{:output, 0}, {:output, 2}}, [5]}}, %{}})

    # mix outputs and bots
    assert (Advent.run({%{2 => {nil, [5]}}, %{}}, "bot 2 gives high to output 1 and low to bot 0") === {%{2 => {{{:bot, 0}, {:output, 1}}, [5]}}, %{}})
    assert (Advent.run({%{2 => {nil, [5]}}, %{}}, "bot 2 gives low to output 1 and high to bot 0") === {%{2 => {{{:output, 1}, {:bot, 0}}, [5]}}, %{}})

    # example
    assert (
      Advent.run({%{}, %{}}, "value 5 goes to bot 2")
        |> Advent.run("bot 2 gives low to bot 1 and high to bot 0")
        |> Advent.run("value 3 goes to bot 1")
        |> Advent.run("bot 1 gives low to output 1 and high to bot 0")
        |> Advent.run("bot 0 gives low to output 2 and high to output 0")
        |> Advent.run("value 2 goes to bot 2")
      ===
      {
        # bots
        %{
          0 => {{{:output, 2}, {:output, 0}}, []},
          1 => {{{:output, 1}, {:bot, 0}}, [3]},
          2 => {{{:bot, 1}, {:bot, 0}}, [2, 5]}
        },
        # outputs
        %{}
      }
    )
  end

  test "can consume" do
    assert (
      Advent.consume({
        # bots
        %{
          0 => {{{:output, 2}, {:output, 0}}, []},
          1 => {{{:output, 1}, {:bot, 0}}, [3]},
          2 => {{{:bot, 1}, {:bot, 0}}, [2, 5]}
        },
        # outputs
        %{}
      })
      ===
      {
        # bots
        %{
          0 => {{{:output, 2}, {:output, 0}}, []},
          1 => {{{:output, 1}, {:bot, 0}}, []},
          2 => {{{:bot, 1}, {:bot, 0}}, []}
        },
        # outputs
        %{
          0 => {nil, [5]},
          1 => {nil, [2]},
          2 => {nil, [3]}
        }
      }
    )
  end
end
