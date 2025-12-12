defmodule Advent2025.Day12 do
  @moduledoc """
  Solution for Advent of Code 2025 - Day 12
  """

  @doc """
  Returns which parts are available/implemented for this day.
  Returns a list like [], [1], or [1, 2].
  """
  def available_parts, do: [2]

  @doc """
  Returns the example input for the given part (1 or 2).
  """
  def example_input(_part) do
    """
    0:
    ###
    ##.
    ##.

    1:
    ###
    ##.
    .##

    2:
    .##
    ###
    ##.

    3:
    ##.
    ###
    ##.

    4:
    ###
    #..
    ###

    5:
    ###
    .#.
    ###

    4x4: 0 0 0 0 2 0
    12x5: 1 0 1 0 2 2
    12x5: 1 0 1 0 3 2
    """
  end

  @doc """
  Solves part 1 of the puzzle.
  """
  def part1(input) do
    sections = input
    |> String.split("\n\n", trim: true)

    present_sections = Enum.drop(sections, -1)
    tree_section = Enum.at(sections, -1)

    presents = present_sections
    |> Enum.map(fn raw_present ->
      [_name | grid] = String.split(raw_present, "\n", trim: true)
      grid |> Enum.map(fn row -> String.graphemes(row) end)
    end)

    trees = String.split(tree_section, "\n", trim: true) |> Enum.map(fn raw_tree ->
      [raw_dimensions | raw_counts] = String.split(raw_tree, " ", trim: true)
      [width, length] = String.split(String.slice(raw_dimensions, 0..-2//1), "x", trim: true) |> Enum.map(&String.to_integer/1)
      counts = raw_counts |> Enum.map(&String.to_integer/1)
      {{width, length}, counts}
    end)

    Enum.filter(trees, fn tree -> valid_tree?(tree, presents) end)
    |> Enum.count()
  end

  def part2(input) do
    "🎅 🎁🎄🎁"
  end

  defp valid_tree?(tree, presents) do
    {{width, length}, counts} = tree

    area_of_presents_to_include = Enum.zip(counts, presents)
    |> Enum.flat_map(fn {count, present} -> List.duplicate(present, count) end)
    |> Enum.map(fn present -> {tile_area(present), present_area(present)} end)

    tile_areas = Enum.map(area_of_presents_to_include, &elem(&1, 0)) |> Enum.sum()
    present_areas = Enum.map(area_of_presents_to_include, &elem(&1, 1)) |> Enum.sum()

    tree_area = width * length

    cond do
      tile_areas <= tree_area -> true
      present_areas > tree_area -> false
      true -> raise "Need packing algorithm"
    end
  end

  defp tile_area(present) do
    Enum.map(present, fn row -> length(row) end)
    |> Enum.sum()
  end

  defp present_area(present) do
    Enum.map(present, fn row -> Enum.count(row, fn cell -> cell == "#" end) end)
    |> Enum.sum()
  end
end
