defmodule Advent2025.Day03 do
  @moduledoc """
  Solution for Advent of Code 2025 - Day 3
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
    987654321111111
    811111111111119
    234234234234278
    818181911112111
    """
  end

  @doc """
  Solves part 1 of the puzzle.
  """
  def part1(input) do
    input
    |> String.split("\n", trim: true)
    |> Enum.map(fn battery_bank ->
      Enum.find(99..11//-1, fn number ->
        looking_for = Integer.to_string(number)
        batteries = String.graphemes(battery_bank)

        Enum.any?(Enum.with_index(batteries), fn {a, i} ->
            Enum.any?(Enum.slice(batteries, (i+1)..-1//1), fn b ->
              a <> b == looking_for
            end)
          end)
      end)
    end)
    |> Enum.sum()
  end

  @doc """
  Solves part 2 of the puzzle.
  """
  def part2(input) do
    input
    |> String.split("\n", trim: true)
    |> Enum.map(fn battery_bank ->
      batteries = String.graphemes(battery_bank)

      Enum.reduce(12..1//-1, {-1, []}, fn up_to_last_n, {current_index, prefix_so_far} ->
        {next_number, offset} = Enum.slice(batteries, current_index+1..-up_to_last_n//1)
        |> Enum.map(fn battery -> String.to_integer(battery) end)
        |> Enum.with_index()
        |> Enum.max_by(fn {number, _offset} -> number end)

        {current_index + offset + 1, prefix_so_far ++ [next_number]}
      end)
      |> elem(1)
      |> Enum.join("")
      |> String.to_integer()
    end)
    |> Enum.sum()
  end

  @doc """
  Returns all combinations of the given list of size.
  For example, combinations(["a", "b", "c"], 2) returns [["a", "b"], ["a", "c"], ["b", "c"]].
  """
  def combinations(list, size) when size == 1 do
    for item <- list, do: [item]
    |> Enum.uniq()
  end

  def combinations(list, size) do
    Enum.flat_map(Enum.with_index(list), fn {item, index}  ->
      combinations(Enum.slice(list, index+1..-1//1), size - 1)
      |> Enum.map(fn digits -> [item | digits] end)
    end)
    |> Enum.uniq()
  end
end
