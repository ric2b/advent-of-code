defmodule Advent2025.Day01Test do
  use ExUnit.Case, async: true

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
      input = File.read!("priv/static/inputs/day_01.txt")
      assert Day01.part1(input) == 1100
    end
  end

  describe "part2" do
    @tag :skip
    test "solves sample input" do
      assert Day01.part2(@sample_input) == 84
    end
  end
end
