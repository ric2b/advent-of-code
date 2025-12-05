defmodule Advent2025.Day05 do
  @moduledoc """
  Solution for Advent of Code 2025 - Day 5
  """

  @doc """
  Returns which parts are available/implemented for this day.
  Returns a list like [], [1], or [1, 2].
  """
  def available_parts, do: []

  @doc """
  Returns the example input for the given part (1 or 2).
  """
  def example_input(_part) do
    """
    3-5
    10-14
    16-20
    12-18

    1
    5
    8
    11
    17
    32
    """
  end

  @doc """
  Solves part 1 of the puzzle.
  """
  def part1(input) do
    [raw_ranges, raw_ids] = input |> String.split("\n\n", trim: true)

    ranges = raw_ranges
    |> String.split("\n", trim: true)
    |> Enum.map(fn range ->
      [from, to] = String.split(range, "-") |> Enum.map(&String.to_integer/1)
      from..to
    end)

    ids = raw_ids
    |> String.split("\n", trim: true)
    |> Enum.map(&String.to_integer/1)


    Enum.filter(ids, fn id ->
      Enum.any?(ranges, fn range -> id in range end)
    end)
    |> Enum.count()
  end

  @doc """
  Solves part 2 of the puzzle.
  """
  def part2(input) do
    [raw_ranges, _raw_ids] = input |> String.split("\n\n", trim: true)

    ranges = raw_ranges
    |> String.split("\n", trim: true)
    |> Enum.map(fn range ->
      [from, to] = String.split(range, "-") |> Enum.map(&String.to_integer/1)
      from..to
    end)
    |> Enum.sort_by(&(&1.last))

    Enum.reduce(ranges, [List.first(ranges)], fn range, merged_ranges ->
      last_range = List.last(merged_ranges)

      if Range.disjoint?(range, last_range) do
        merged_ranges ++ [range]
      else
        merged_range = min(range.first, last_range.first)..max(range.last, last_range.last)
        (merged_ranges -- [last_range]) ++ [merged_range]
      end
    end)
    |> Enum.map(fn range -> Range.size(range) end)
    |> Enum.sum()
  end
end
