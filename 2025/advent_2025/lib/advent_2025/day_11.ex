defmodule Advent2025.Day11 do
  @moduledoc """
  Solution for Advent of Code 2025 - Day 11
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
    aaa: you hhh
    you: bbb ccc
    bbb: ddd eee
    ccc: ddd eee fff
    ddd: ggg
    eee: out
    fff: out
    ggg: out
    hhh: ccc fff iii
    iii: out
    """
  end

  @doc """
  Solves part 1 of the puzzle.
  """
  def part1(input) do
    graph = input
    |> String.split("\n", trim: true)
    |> Enum.map(fn line ->
      [from, to] = String.split(line, ": ")
      {from, String.split(to, " ")}
    end)
    |> Map.new()

    count_paths_with_dfs(graph, "you", "out")
  end

  @doc """
  Solves part 2 of the puzzle.
  """
  def part2(input) do
    input
  end

  defp count_paths_with_dfs(_graph, start, target) when start == target, do: 1
  defp count_paths_with_dfs(graph, start, target) do
    graph[start]
    |> Enum.map(fn neighbor -> count_paths_with_dfs(graph, neighbor, target) end)
    |> Enum.sum()
  end
end
