defmodule Mix.Tasks.Gen.Day do
  @moduledoc """
  Generates implementation and test files for a given Advent of Code day.

  ## Usage

      mix gen.day DAY_NUMBER

  ## Example

      mix gen.day 4

  This will create:
  - `lib/advent_2025/day_04.ex`
  - `test/advent_2025/day_04_test.exs`
  """
  use Mix.Task

  @shortdoc "Generates implementation and test files for an Advent of Code day"

  @impl Mix.Task
  def run([day]) do
    day_number = String.to_integer(day)
    padded_day = String.pad_leading(to_string(day_number), 2, "0")

    generate_impl(day_number, padded_day)
    generate_test(padded_day)
  end

  def run(_) do
    Mix.shell().error("Usage: mix gen.day DAY_NUMBER")
  end

  defp generate_impl(day_number, padded_day) do
    impl_file_path = "lib/advent_2025/day_#{padded_day}.ex"

    if File.exists?(impl_file_path) do
      Mix.shell().error("Implementation file already exists: #{impl_file_path}")
    else
      content = generate_impl_content(day_number, padded_day)
      File.write!(impl_file_path, content)
      Mix.shell().info("Created #{impl_file_path}")
    end
  end

  defp generate_test(padded_day) do
    test_file_path = "test/advent_2025/day_#{padded_day}_test.exs"

    if File.exists?(test_file_path) do
      Mix.shell().error("Test file already exists: #{test_file_path}")
    else
      content = generate_test_content(padded_day)
      File.write!(test_file_path, content)
      Mix.shell().info("Created #{test_file_path}")
    end
  end

  defp generate_impl_content(day_number, padded_day) do
    """
    defmodule Advent2025.Day#{padded_day} do
      @moduledoc \"\"\"
      Solution for Advent of Code 2025 - Day #{day_number}
      \"\"\"

      @doc \"\"\"
      Returns which parts are available/implemented for this day.
      Returns a list like [], [1], or [1, 2].
      \"\"\"
      def available_parts, do: []

      @doc \"\"\"
      Returns the example input for the given part (1 or 2).
      \"\"\"
      def example_input(_part) do
        \"\"\"
        \"\"\"
      end

      @doc \"\"\"
      Solves part 1 of the puzzle.
      \"\"\"
      def part1(input) do
        input
      end

      @doc \"\"\"
      Solves part 2 of the puzzle.
      \"\"\"
      def part2(input) do
        input
      end
    end
    """
  end

  defp generate_test_content(padded_day) do
    """
    defmodule Advent2025.Day#{padded_day}Test do
      use ExUnit.Case, async: true
      use Advent2025.AdventCase

      describe "part1" do
        test "solves sample input" do
          assert @day.part1(@day.example_input(1)) == :todo
        end

        test "solves actual input" do
          assert @day.part1(@actual_input) == :todo
        end
      end

      describe "part2" do
        test "solves sample input" do
          assert @day.part2(@day.example_input(2)) == :todo
        end

        test "solves actual input" do
          assert @day.part2(@actual_input) == :todo
        end
      end
    end
    """
  end
end
