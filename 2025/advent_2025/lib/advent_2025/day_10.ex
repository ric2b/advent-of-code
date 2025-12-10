defmodule Advent2025.Day10 do
  @moduledoc """
  Solution for Advent of Code 2025 - Day 10
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
    [.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
    [...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}
    [.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}
    """
  end

  @doc """
  Solves part 1 of the puzzle.
  """
  def part1(input) do
    Enum.map(parse_input(input), fn {target_pattern, buttons, _target_joltage} ->
      current_pattern = Enum.map(target_pattern, fn _ -> false end)
      min_button_presses([current_pattern], target_pattern, &possible_patterns(buttons, &1))
    end)
    |> Enum.sum()
  end

  @doc """
  Solves part 2 of the puzzle.
  """
  def part2(input) do
    Enum.map(parse_input(input), fn {_target_pattern, buttons, target_joltage} ->
      current_joltage = Enum.map(target_joltage, fn _ -> 0 end)
      min_button_presses([current_joltage], target_joltage, &possible_joltages(buttons, target_joltage, &1))
    end)
    |> Enum.sum()
  end

  defp min_button_presses(current_level, target, neighbours_fn, visited \\ MapSet.new(), steps \\ 0)
  # defp min_button_presses(current_level, visited, target, neighbours_fn, steps)
  #  when not Kernel.is_list(current_level), do: min_button_presses([current_level], visited, target, neighbours_fn, steps)

  defp min_button_presses(current_level, target, neighbours_fn, visited, steps) do
    if target in current_level do
      steps
    else
      next_level =
        current_level
        |> Enum.flat_map(neighbours_fn)
        |> Enum.reject(&MapSet.member?(visited, &1))
        |> Enum.uniq()

      new_visited = Enum.reduce(next_level, visited, &MapSet.put(&2, &1))

      min_button_presses(next_level, target, neighbours_fn, new_visited,steps + 1)
    end
  end

  defp possible_patterns(buttons, current_pattern) do
    Enum.map(buttons, fn button ->
      Enum.map(Enum.with_index(current_pattern), fn {pattern_cell, pattern_index} ->
        if pattern_index in button, do: not pattern_cell, else: pattern_cell
      end)
    end)
    |> Enum.uniq()
  end

  defp possible_joltages(buttons, target_joltage, current_joltage) do
    Enum.map(buttons, fn button ->
      Enum.map(Enum.with_index(current_joltage), fn {joltage_cell, joltage_index} ->
        if joltage_index in button, do: joltage_cell + 1, else: joltage_cell
      end)
    end)
    |> Enum.reject(fn joltage ->
      Enum.zip(target_joltage, joltage)
      |> Enum.any?(fn {target, current} -> current > target end)
    end)
    |> Enum.uniq()
  end

  defp parse_input(input) do
    input
    |> String.split("\n", trim: true)
    |> Enum.map(fn line ->
      [raw_pattern | rest] = String.split(line, " ")

      pattern = String.graphemes(raw_pattern)
      |> Enum.slice(1..-2//1)
      |> Enum.map(fn char -> char == "#" end)

      [target_joltage | buttons] = Enum.reverse(rest)
      |> Enum.map(fn raw_button ->
        String.graphemes(raw_button)
        |> Enum.slice(1..-2//1)
        |> Enum.join("")
        |> String.split(",")
        |> Enum.map(&String.to_integer/1)
      end)

      {pattern, buttons, target_joltage}
    end)
  end
end
