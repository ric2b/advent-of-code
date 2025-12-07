defmodule Advent2025.Day07 do
  use Memoize
  @moduledoc """
  Solution for Advent of Code 2025 - Day 7
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
    .......S.......
    ...............
    .......^.......
    ...............
    ......^.^......
    ...............
    .....^.^.^.....
    ...............
    ....^.^...^....
    ...............
    ...^.^...^.^...
    ...............
    ..^...^.....^..
    ...............
    .^.^.^.^.^...^.
    ...............
    """
  end

  @doc """
  Solves part 1 of the puzzle.
  """
  def part1(input) do
    [start_line | map] = input |> String.split("\n", trim: true) |> Enum.map(fn line -> String.graphemes(line) end)
    start_x = Enum.find_index(start_line, fn char -> char == "S" end)

    Enum.scan(map, [0, MapSet.new([start_x])], fn line, [_splits, beam_xs] ->
      new_beam_xs = Enum.flat_map(beam_xs, fn beam_x ->
        case Enum.at(line, beam_x) do
          "." -> [beam_x]
          "^" -> [beam_x - 1, beam_x + 1]
        end
      end)
      |> MapSet.new()

      [Enum.count(beam_xs, fn beam_x -> Enum.at(line, beam_x) == "^" end), new_beam_xs]
    end)
    |> Enum.map(fn [splits, _beam_xs] -> splits end)
    |> Enum.sum()
  end

  @doc """
  Solves part 2 of the puzzle.
  """
  def part2(input) do
    [start_line | map] = input |> String.split("\n", trim: true) |> Enum.map(fn line -> String.graphemes(line) end)
    start_x = Enum.find_index(start_line, fn char -> char == "S" end)

    timeline_counts(map, start_x)
  end

  defmemop timeline_counts([], _beam_x), do: 1
  defmemop timeline_counts(map, beam_x) do
    [current_line | rest_of_map] = map

    case Enum.at(current_line, beam_x) do
      "." -> timeline_counts(rest_of_map, beam_x)
      "^" -> timeline_counts(rest_of_map, beam_x - 1) + timeline_counts(rest_of_map, beam_x + 1)
    end
  end
end
