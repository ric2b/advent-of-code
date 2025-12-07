defmodule Advent2025.Day06 do
  @moduledoc """
  Solution for Advent of Code 2025 - Day 6
  """

  @doc """
  Returns which parts are available/implemented for this day.
  Returns a list like [], [1], or [1, 2].
  """
  def available_parts, do: [1, 2]

  @doc """
  Returns the example input for the given part (1 or 2).
  """
  def example_input(_part) do
    """
    123 328  51 64
     45 64  387 23
      6 98  215 314
    *   +   *   +
    """
  end

  @doc """
  Solves part 1 of the puzzle.
  """
  def part1(input) do
    raw_values = input
    |> String.split("\n", trim: true)
    |> Enum.map(fn line -> String.split(line, " ", trim: true) end)

    Enum.map(transpose(raw_values), fn raw_problem ->
      [raw_operation | raw_numbers] = Enum.reverse(raw_problem)
      numbers = Enum.map(raw_numbers, &String.to_integer/1)
      case raw_operation do
        "+" -> Enum.sum(numbers)
        "*" -> Enum.product(numbers)
      end
    end)
    |> Enum.sum()
  end

  @doc """
  Solves part 2 of the puzzle.
  """
  def part2(input) do
    raw_values = input
    |> String.split("\n", trim: true)
    |> Enum.map(fn line -> String.graphemes(line) end)

    separator? = fn line -> Enum.all?(line, fn char -> char == " " end) end
    raw_problems = transpose(raw_values)
    |> Enum.chunk_by(separator?)
    |> Enum.reject(fn chunk -> Enum.all?(chunk, separator?) end)

    Enum.map(raw_problems, fn raw_problem ->
      operator = List.first(raw_problem) |> List.last()
      numbers = Enum.map(raw_problem, fn line ->
        Enum.drop(line, -1)
        |> Enum.join()
        |> String.trim()
        |> String.to_integer()
      end)

      case operator do
        "+" -> Enum.sum(numbers)
        "*" -> Enum.product(numbers)
      end
    end)
    |> Enum.sum()
  end

  @doc """
  Transposes a list of lists, handling lists of different lengths.
  Missing elements are filled with nil.

  ## Examples

      iex> ListTransposer.transpose([[1, 2, 3], [4, 5], [6, 7, 8, 9]])
      [[1, 4, 6], [2, 5, 7], [3, nil, 8], [nil, nil, 9]]

      iex> ListTransposer.transpose([[1, 2], [3], [4, 5, 6]])
      [[1, 3, 4], [2, nil, 5], [nil, nil, 6]]
  """
  def transpose([]), do: []
  def transpose([[] | _]), do: []

  def transpose(lists) do
    max_length = lists |> Enum.map(&length/1) |> Enum.max()

    Enum.map(0..(max_length - 1), fn index ->
      Enum.map(lists, fn list -> Enum.at(list, index) || " " end)
    end)
  end
end
