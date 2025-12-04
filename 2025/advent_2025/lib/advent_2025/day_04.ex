defmodule Advent2025.Day04 do
  @moduledoc """
  Solution for Advent of Code 2025 - Day 4
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
    ..@@.@@@@.
    @@@.@.@.@@
    @@@@@.@.@@
    @.@@@@..@.
    @@.@@@@.@@
    .@@@@@@@.@
    .@.@.@.@@@
    @.@@@.@@@@
    .@@@@@@@@.
    @.@.@@@.@.
    """
  end

  @doc """
  Solves part 1 of the puzzle.
  """
  def part1(input) do
    map = input
    |> String.split("\n", trim: true)
    |> Enum.map(fn row -> String.graphemes(row) end)


    Enum.with_index(map)
    |> Enum.map(fn {row, y} ->
      Enum.with_index(row)
      |> Enum.filter(fn {cell, _x} -> cell == "@" end)
      |> Enum.count(fn {_cell, x} ->
        neighbours(map, {y, x})
        |> Enum.count(fn neighbour -> neighbour == "@" end) < 4
      end)
    end)
    |> Enum.sum()
  end

  @doc """
  Solves part 2 of the puzzle.
  """
  def part2(input) do
    map = input
    |> String.split("\n", trim: true)
    |> Enum.map(fn row -> String.graphemes(row) end)

    count_cells(map) - count_cells(remove_accessible(map))
  end

  defp count_cells(map) do
    Enum.map(map, fn row ->
      Enum.count(row, fn cell -> cell == "@" end)
    end)
   |> Enum.sum()
  end

  defp remove_accessible(map) do
    if !removable_cells?(map), do: map, else: remove_accessible(without_accessible(map))
  end

  defp without_accessible(map) do
    Enum.map(Enum.with_index(map), fn {row, y} ->
      Enum.map(Enum.with_index(row), fn {cell, x} ->
        case cell do
          "." -> "."
          "@" -> if neighbours_count(map, {y, x}) < 4, do: ".", else: "@"
        end
      end)
    end)
  end

  defp removable_cells?(map) do
    Enum.any?(Enum.with_index(map), fn {row, y} ->
      Enum.any?(Enum.with_index(row), fn {cell, x} ->
        cell == "@" and neighbours_count(map, {y, x}) < 4
      end)
    end)
  end

  defp neighbours_count(map, {y, x}) do
    neighbours(map, {y, x}) |> Enum.count(fn neighbour -> neighbour == "@" end)
  end

  defp neighbours(map, {y, x}) do
    [
      {y-1, x-1},
      {y-1, x},
      {y-1, x+1},
      {y, x-1},
      {y, x+1},
      {y+1, x-1},
      {y+1, x},
      {y+1, x+1}
    ]
    |> Enum.filter(fn {y, x} -> x >= 0 and y >= 0 and y < length(map) and x < length(Enum.at(map, y)) end)
    |> Enum.map(fn {y, x} -> Enum.at(map, y) |> Enum.at(x) end)
  end
end
