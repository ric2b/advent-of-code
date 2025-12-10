defmodule Advent2025.Day09 do
  @moduledoc """
  Solution for Advent of Code 2025 - Day 9
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
    7,1
    11,1
    11,7
    9,7
    9,5
    2,5
    2,3
    7,3
    """
  end

  @doc """
  Solves part 1 of the puzzle.
  """
  def part1(input) do
    tile_locations = input
    |> String.split("\n", trim: true)
    |> Enum.map(fn line -> String.split(line, ",") |> Enum.map(&String.to_integer/1) |> List.to_tuple() end)

    (for a <- tile_locations, b <- tile_locations, a < b, do: rectangle_area({a, b}))
    |> Enum.max()
  end

  @doc """
  Solves part 2 of the puzzle.
  """
  def part2(input) do
    vertices = input
    |> String.split("\n", trim: true)
    |> Enum.map(fn line -> String.split(line, ",") |> Enum.map(&String.to_integer/1) |> List.to_tuple() end)

    polygon_edges = edges(vertices)

    (for a <- vertices, b <- vertices, a < b, do: {a, b})
    |> Enum.sort_by(fn rectangle -> rectangle_area(rectangle) end, :desc)
    |> Enum.find(fn rectangle -> not any_perimeter_points_inside_rectangle?(polygon_edges, rectangle) end)
    |> rectangle_area()
  end

  defp any_perimeter_points_inside_rectangle?(edges, rectangle) do
    Enum.any?(edges, fn edge -> edge_within_rectangle?(edge, rectangle) end)
  end

  defp edge_within_rectangle?(edge, rectangle) do
    {{xa1, ya1}, {xa2, ya2}} = edge
    {{xb1, yb1}, {xb2, yb2}} = rectangle

    {min_line_x, max_line_x} = {min(xa1, xa2), max(xa1, xa2)}
    {min_line_y, max_line_y} = {min(ya1, ya2), max(ya1, ya2)}
    {min_rect_x, max_rect_x} = {min(xb1, xb2), max(xb1, xb2)}
    {min_rect_y, max_rect_y} = {min(yb1, yb2), max(yb1, yb2)}

    max_line_x > min_rect_x and min_line_x < max_rect_x and max_line_y > min_rect_y and min_line_y < max_rect_y
  end

  defp edges(vertices) do
    Enum.zip(vertices, Enum.concat(Enum.drop(vertices, 1), [Enum.at(vertices, 0)]))
  end

  defp rectangle_area({{x1, y1}, {x2, y2}}) do
    (abs(x2 - x1) + 1) * (abs(y2 - y1) + 1)
  end
end
