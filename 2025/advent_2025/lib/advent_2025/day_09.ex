defmodule Advent2025.Day09 do
  @moduledoc """
  Solution for Advent of Code 2025 - Day 9
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

    # polygon = edges(vertices)

    # polygon_points = points_for_polygon(vertices)

    # puts_points(polygon_points)

    polygon_perimeter = edges(vertices) |> perimeter_points()

    for a <- vertices, b <- vertices, a < b do
      {a, b}
    end
    |> Enum.sort_by(fn rectangle -> rectangle_area(rectangle) end, :desc)
    |> Enum.filter(fn rectangle ->
      not Enum.any?(polygon_perimeter, fn point -> point_inside_rectangle?(point, rectangle) end)
    end)
    |> Enum.map(fn rectangle -> rectangle_area(rectangle) end)
    |> Enum.max()
  end

  # defp points_for_rectangle({{x1, y1}, {x2, y2}}) do
  #   for x <- min(x1, x2)..max(x1, x2), y <- min(y1, y2)..max(y1, y2), do: {x, y}
  # end

  defp puts_points(points) do
    IO.puts("\n")
    {max_x, max_y} = {
      points |> Enum.map(fn {x, _y} -> x end) |> Enum.max(),
      points |> Enum.map(fn {_x, y} -> y end) |> Enum.max(),
    }

    Enum.map_join(0..max_y, "\n", fn y ->
      Enum.map_join(0..max_x + 2, "", fn x -> if {x, y} in points, do: "#", else: "." end)
    end)
    |> IO.puts()
  end

  # defp rectangle_within_polygon?(rectangle, polygon) do
  #   {{min_x, max_x}, {min_y, max_y}} = {
  #     rectangle |> Enum.map(fn {x, _y} -> x end) |> Enum.min_max(),
  #     rectangle |> Enum.map(fn {_x, y} -> y end) |> Enum.min_max(),
  #   }

  #   rectangle_edges = edges(rectangle)

  #   not Enum.any?(polygon, fn {{x1, y1}, {x2, y2}} ->
  #     for x <

  #     x1 in min_x..max_x and y1 in min_y..max_y or x2 in min_x..max_x and y2 in min_y..max_y

  #     case {{x1, y1}, {x2, y2}} = edge do
  #       {x1, y1} in min_x..max_x and {x2, y2} in min_y..max_y -> true
  #       {x1, y1} in min_y..max_y and {x2, y2} in min_x..max_x -> true
  #       _ -> false
  #     end
  #   end)
  # end

  # 0 ..............
  # 1 .......#XXX#..
  # 2 .......XXXXX..
  # 3 ..OOOOOOOOXX..
  # 4 ..OOOOOOOOXX..
  # 5 ..OOOOOOOOXX..
  # 6 .........XXX..
  # 7 .........#X#..
  # 8 ..............

  # defp points_for_polygon(vertices) do
  #   {{min_x, max_x}, {min_y, max_y}} = {
  #     vertices |> Enum.map(fn {x, _y} -> x end) |> Enum.min_max(),
  #     vertices |> Enum.map(fn {_x, y} -> y end) |> Enum.min_max(),
  #   }

  #   vertical_edge_points = Enum.flat_map(edges(vertices) |> vertical_edges(), fn {a, b} -> points_for_rectangle({a, b}) end)
  #   horizontal_edge_points = Enum.flat_map(edges(vertices) |> horizontal_edges(), fn {a, b} -> points_for_rectangle({a, b}) end)
  #   # |> Enum.filter(fn point -> point not in vertical_edge_points end)


  #   puts_points(vertical_edge_points)
  #   puts_points(horizontal_edge_points)
  #   # IO.inspect(vertical_edge_points, label: "vertical edge points")

  #   relevant_grid_points = for y <- min_y..max_y, x <- min_x..max_x, do: {x, y}
  #   Enum.scan(relevant_grid_points, false, fn point, inside_polygon ->
  #     vertical_edge_point = point in vertical_edge_points

  #     cond do
  #       point in horizontal_edge_points -> true
  #       # inside_polygon and horizontal_edge_point and not vertical_edge_point -> true
  #       not inside_polygon and not vertical_edge_point -> false
  #       not inside_polygon and vertical_edge_point -> true
  #       inside_polygon and not vertical_edge_point -> true
  #       inside_polygon and vertical_edge_point -> false
  #     end

  #     # (7x) not inside_polygon and not vertical_edge_point -> not inside_polygon
  #     # (1x) not inside_polygon and vertical_edge_point -> inside_polygon
  #     # (3x) inside_polygon and not vertical_edge_point -> inside_polygon
  #     # (1x) inside_polygon and vertical_edge_point -> not inside_polygon
  #     # (9x) not inside_polygon and not vertical_edge_point -> not inside_polygon
  #     # (1x) not inside_polygon and vertical_edge_point -> inside_polygon


  #   end)
  #   |> Enum.zip(relevant_grid_points)
  #   |> Enum.filter(fn {inside_polygon, point} -> inside_polygon or point in vertices end)
  #   |> Enum.map(fn {_inside_polygon, point} -> point end)
  #   |> MapSet.new()
  # end

  # defp vertical_edges(edges) do
  #   Enum.filter(edges, fn {{x1, _y1}, {x2, _y2}} -> x1 == x2 end)
  # end

  # defp horizontal_edges(edges) do
  #   Enum.filter(edges, fn {{_x1, y1}, {_x2, y2}} -> y1 == y2 end)
  # end

  defp point_inside_rectangle?({x, y}, {{rx1, ry1}, {rx2, ry2}}) do
    {{min_x, max_x}, {min_y, max_y}} = {
      {min(rx1, rx2), max(rx1, rx2)},
      {min(ry1, ry2), max(ry1, ry2)}
    }
    x > min_x and x < max_x and y > min_y and y < max_y
  end

  defp perimeter_points(edges) do
    Enum.flat_map(edges, fn {{x1, y1}, {x2, y2}} ->
      for x <- min(x1, x2)..max(x1, x2), y <- min(y1, y2)..max(y1, y2), do: {x, y}
    end)
    |> MapSet.new()
  end

  defp edges(vertices) do
    Enum.zip(vertices, Enum.concat(Enum.drop(vertices, 1), [Enum.at(vertices, 0)]))
  end

  defp rectangle_area({{x1, y1}, {x2, y2}}) do
    (abs(x2 - x1) + 1) * (abs(y2 - y1) + 1)
  end
end
