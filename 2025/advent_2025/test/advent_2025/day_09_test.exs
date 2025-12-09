defmodule Advent2025.Day09Test do
  use ExUnit.Case, async: true
  use Advent2025.AdventCase

  describe "part1" do
    test "solves sample input" do
      assert @day.part1(@day.example_input(1)) == 50
    end

    test "solves actual input" do
      assert @day.part1(@actual_input) == 4767418746
    end
  end

  describe "part2" do
    test "solves sample input" do
      assert @day.part2(@day.example_input(2)) == 24
    end

    # Finished in 0.1 seconds
    # Finished in 204.9 seconds
    # Finished in 1152.6 seconds
    test "solves actual input" do
      assert @day.part2(@actual_input) == 1461987144
    end
  end
end
