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

    raw_moves = input |> String.split("\n", trim: true)

    Enum.scan(raw_moves, dial_start, fn raw_move, dial_position ->
      case raw_move do
        "L" <> distance -> dial_position - String.to_integer(distance)
        "R" <> distance -> dial_position + String.to_integer(distance)
      end
      |> Integer.mod(dial_steps)
    end)
    |> Enum.count(fn dial_position -> dial_position == 0 end)
  end

  @doc """
  Solves part 2 of the puzzle.
  """
  def part2(input) do
    dial_start = 50
    dial_steps = 100

    raw_moves = input |> String.split("\n", trim: true)

    Enum.scan(raw_moves, {dial_start, 0}, fn raw_move, {dial_position, _extra_passes} ->
      {direction, moves} = case raw_move do
        "L" <> distance -> {"L", String.to_integer(distance)}
        "R" <> distance -> {"R", String.to_integer(distance)}
      end

      first_pass_count = if dial_position != 0, do: 1, else: 0
      passes_through_zero = div(moves, dial_steps) + case direction do
        "L" -> if dial_position - Integer.mod(moves, dial_steps) <= 0, do: first_pass_count, else: 0
        "R" -> if dial_position + Integer.mod(moves, dial_steps) >= dial_steps, do: first_pass_count, else: 0
      end

      new_dial_position = case direction do
        "L" -> dial_position - moves
        "R" -> dial_position + moves
      end |> Integer.mod(dial_steps)

      {new_dial_position, passes_through_zero}
    end)
    |> Enum.map(fn {_dial_position, passes_through_zero} -> passes_through_zero end)
    |> Enum.sum()
  end
end
