defmodule Advent2025.AdventCase do
  @moduledoc """
  Provides common setup for Advent of Code test modules.
  """

  defmacro __using__(_opts) do
    quote do
      day_number =
        __MODULE__
        |> Module.split()
        |> List.last()
        |> String.replace(~r/[^\d]/, "")
        |> String.pad_leading(2, "0")

      @actual_input File.read!("priv/static/inputs/day_#{day_number}.txt")
    end
  end
end
