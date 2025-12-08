defmodule Advent2025.Day08 do
  @moduledoc """
  Solution for Advent of Code 2025 - Day 8
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
    162,817,812
    57,618,57
    906,360,560
    592,479,940
    352,342,300
    466,668,158
    542,29,236
    431,825,988
    739,650,466
    52,470,668
    216,146,977
    819,987,18
    117,168,530
    805,96,715
    346,949,466
    970,615,88
    941,993,340
    862,61,35
    984,92,344
    425,690,689
    """
  end

  @doc """
  Solves part 1 of the puzzle.
  """
  def part1(input, connections \\ 1000) do
    breaker_locations = input
    |> String.split("\n", trim: true)
    |> Enum.map(fn line -> String.split(line, ",") |> Enum.map(&String.to_integer/1) |> List.to_tuple() end)

    breaker_circuits = Map.new(Enum.zip(breaker_locations, breaker_locations))
    breaker_distances = for a <- breaker_locations, b <- breaker_locations, a < b do
      {{a, b}, euclidean_distance(a, b)}
    end |> Enum.sort_by(fn {_, distance} -> distance end)

    Enum.take(breaker_distances, connections)
    |> Enum.reduce(breaker_circuits, fn {{breaker_a, breaker_b}, _distance}, breaker_circuits ->
      breakers_to_join = Enum.filter(breaker_circuits, fn {_breaker, circuit} ->
        circuit == Map.fetch!(breaker_circuits, breaker_b)
      end)
      |> Enum.map(fn {breaker, _circuit} -> {breaker, Map.fetch!(breaker_circuits, breaker_a)} end)
      |> Map.new()

      Map.merge(breaker_circuits, breakers_to_join)
    end)
    |> Enum.group_by(fn {_breaker, circuit} -> circuit end)
    |> Enum.map(fn {_circuit, breakers} -> Enum.count(breakers) end)
    |> Enum.sort(:desc)
    |> Enum.take(3)
    |> Enum.product()
  end

  @doc """
  Solves part 2 of the puzzle.
  """
  def part2(input) do
    breaker_locations = input
    |> String.split("\n", trim: true)
    |> Enum.map(fn line -> String.split(line, ",") |> Enum.map(&String.to_integer/1) |> List.to_tuple() end)

    breaker_circuits = Map.new(Enum.zip(breaker_locations, breaker_locations))
    breaker_distances = for a <- breaker_locations, b <- breaker_locations, a < b do
      {{a, b}, euclidean_distance(a, b)}
    end |> Enum.sort_by(fn {_, distance} -> distance end)

    Enum.reduce_while(breaker_distances, breaker_circuits, fn {{breaker_a, breaker_b}, _distance}, breaker_circuits ->
      breakers_to_join = Enum.filter(breaker_circuits, fn {_breaker, circuit} ->
        circuit ==  Map.fetch!(breaker_circuits, breaker_b)
      end)
      |> Enum.map(fn {breaker, _circuit} -> {breaker, Map.fetch!(breaker_circuits, breaker_a)} end)
      |> Map.new()

      new_circuits = Map.merge(breaker_circuits, breakers_to_join)
      if Map.values(new_circuits) |> Enum.uniq() |> length() > 1 do
        {:cont, new_circuits}
      else
        {{x1, _y1, _z1}, {x2, _y2, _z2}} = {breaker_a, breaker_b}
        {:halt, x1 * x2}
      end
    end)
  end

  defp euclidean_distance({x1, y1, z1}, {x2, y2, z2}) do
    :math.sqrt(:math.pow(x1 - x2, 2) + :math.pow(y1 - y2, 2) + :math.pow(z1 - z2, 2))
  end
end
