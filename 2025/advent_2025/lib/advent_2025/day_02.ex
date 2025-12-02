defmodule Advent2025.Day02 do
  @moduledoc """
  Solution for Advent of Code 2025 - Day 2
  """

  @doc """
  Returns the example input for the given part (1 or 2).
  """
  def example_input(_part) do
    "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124"
  end

  @doc """
  Solves part 1 of the puzzle.
  """
  def part1(input) do
    input
    |> String.trim()
    |> String.split(",")
    |> Enum.map(fn range ->
      [from, to] = String.split(range, "-") |> Enum.map(&String.to_integer/1)

      from..to |> Enum.filter(fn number ->
        as_string = Integer.to_string(number)
        {a, b} = String.split_at(as_string, div(String.length(as_string), 2))
        a == b
      end)
    end)
    |> List.flatten()
    |> Enum.sum()
  end

  @doc """
  Solves part 2 of the puzzle.
  """
  def part2(input) do
    input
    |> String.trim()
    |> String.split(",")
    |> Enum.map(fn range ->
      [from, to] = String.split(range, "-") |> Enum.map(&String.to_integer/1)

      from..to |> Enum.filter(fn number ->
        as_string = Integer.to_string(number)
        graphemes = String.graphemes(as_string)

        if length(graphemes) < 2 do
          false
        else
          1..max(1, div(length(graphemes), 2)) |> Enum.any?(fn chunk_size ->
            Enum.chunk_every(graphemes, chunk_size) |> Enum.uniq() |> length() == 1
          end)
        end
      end)
    end)
    |> List.flatten()
    |> Enum.sum()
  end
end
