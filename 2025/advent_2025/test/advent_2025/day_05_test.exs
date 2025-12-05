defmodule Advent2025.Day05Test do
  use ExUnit.Case, async: true
  use Advent2025.AdventCase

  describe "part1" do
    test "solves sample input" do
      assert @day.part1(@day.example_input(1)) == 3
    end

    test "solves actual input" do
      assert @day.part1(@actual_input) == 679
    end
  end

  describe "part2" do
    test "solves sample input" do
      assert @day.part2(@day.example_input(2)) == 14
    end

    test "solves actual input" do
      assert @day.part2(@actual_input) == 358155203664116
    end
  end
end
