defmodule Advent2025.Day01Test do
  use ExUnit.Case, async: true
  use Advent2025.AdventCase

  alias Advent2025.Day01

  @sample_input """
  L68
  L30
  R48
  L5
  R60
  L55
  L1
  L99
  R14
  L82
  """

  describe "part1" do
    test "solves sample input" do
      assert Day01.part1(@sample_input) == 3
    end

    test "solves actual input" do
      assert Day01.part1(@actual_input) == 1100
    end
  end

  describe "part2" do
    test "solves sample input" do
      assert Day01.part2(@sample_input) == 6
    end

    # @tag :skip
    test "solves actual input" do
      assert Day01.part2(@actual_input) == 6358
    end
  end
end
