defmodule Advent2025.Day11 do
  use Memoize

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
  def example_input(part) do
    case part do
      1 -> """
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
      2 -> """
           svr: aaa bbb
           aaa: fft
           fft: ccc
           bbb: tty
           tty: ccc
           ccc: ddd eee
           ddd: hub
           hub: fff
           eee: dac
           dac: fff
           fff: ggg hhh
           ggg: out
           hhh: out
           """
      end
  end

  @doc """
  Solves part 1 of the puzzle.
  """
  def part1(input) do
    graph = parse_graph(input)
    count_paths_with_dfs(graph, "you", "out")
  end

  @doc """
  Solves part 2 of the puzzle.
  """
  def part2(input) do
    graph = parse_graph(input)

    # [count_paths_with_dfs(graph, "dac", "fft"), count_paths_with_dfs(graph, "fft", "dac")]
    # (count_paths_with_dfs(graph, "svr", "dac") *
    # count_paths_with_dfs(graph, "dac", "fft") *
    # count_paths_with_dfs(graph, "fft", "out")) +
    (count_paths_with_dfs(graph, "svr", "fft") *
    count_paths_with_dfs(graph, "fft", "dac") *
    count_paths_with_dfs(graph, "dac", "out"))
  end

  defmemop count_paths_with_dfs(_graph, start, target) when start == target, do: 1
  defmemop count_paths_with_dfs(graph, start, target) do
    Map.get(graph, start, [])
    |> Enum.map(fn neighbor -> count_paths_with_dfs(graph, neighbor, target) end)
    |> Enum.reduce(0, fn count, acc -> count + acc end)
  end

  defp parse_graph(input) do
    input
    |> String.split("\n", trim: true)
    |> Enum.map(fn line ->
      [from, to] = String.split(line, ": ")
      {from, String.split(to, " ")}
    end)
    |> Map.new()
  end
end
