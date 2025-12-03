defmodule Advent2025.AdventCase do
  @moduledoc """
  Provides common setup for Advent of Code test modules.

  Sets up:
  - `@day` - the day module (e.g., `Advent2025.Day04`)
  - `@actual_input` - the puzzle input (fetched from AoC if not cached)
  """

  defmacro __using__(_opts) do
    quote do
      padded_day_number =
        __MODULE__
        |> Module.split()
        |> List.last()
        |> String.replace(~r/[^\d]/, "")
        |> String.pad_leading(2, "0")

      @day Module.concat(Advent2025, :"Day#{padded_day_number}")

      input_file_path = "priv/static/inputs/day_#{padded_day_number}.txt"
      @actual_input (case File.read(input_file_path) do
        {:ok, input} -> input
        {:error, _reason} ->
          day = String.to_integer(padded_day_number)

          %{status_code: 200, body: body} = HTTPoison.get!(
            "https://adventofcode.com/2025/day/#{day}/input",
            [{"Cookie", Application.compile_env(:advent_2025, :aoc_token)}]
          )

          File.write!(input_file_path, body)
          body
      end)
    end
  end
end
