defmodule Advent2025.Day01 do
  @moduledoc """
  Solution for Advent of Code 2025 - Day 1
  """

  @doc """
  Solves part 1 of the puzzle.
  """
  def part1(input) do
    dial_start = 50
    dial_steps = 100

    raw_moves = input |> String.split("\n", trim: true) |> Enum.map(&String.trim/1)

    Enum.scan(raw_moves, dial_start, fn raw_move, dial_position ->
      case raw_move do
        "L" <> distance -> dial_position - String.to_integer(distance)
        "R" <> distance -> dial_position + String.to_integer(distance)
      end
      |> Integer.mod(dial_steps)
      # |> IO.inspect(label: "new position")
    end)
    |> Enum.count(fn dial_position -> dial_position == 0 end)
  end

  @doc """
  Solves part 2 of the puzzle.
  """
  def part2(input) do
    # your logic here
    0
  end
end
