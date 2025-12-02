defmodule Advent2025.Day01Test do
  use ExUnit.Case, async: true
  use Advent2025.AdventCase

  alias Advent2025.Day01

  describe "part1" do
    test "solves sample input" do
      assert Day01.part1(Day01.example_input(1)) == 3
    end

    test "solves actual input" do
      assert Day01.part1(@actual_input) == 1100
    end
  end

  describe "part2" do
    test "solves sample input" do
      assert Day01.part2(Day01.example_input(2)) == 6
    end

    # @tag :skip
    test "solves actual input" do
      assert Day01.part2(@actual_input) == 6358
    end
  end
end
