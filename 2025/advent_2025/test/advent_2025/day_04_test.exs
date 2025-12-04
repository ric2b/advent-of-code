defmodule Advent2025.Day04Test do
  use ExUnit.Case, async: true
  use Advent2025.AdventCase

  describe "part1" do
    test "solves sample input" do
      assert @day.part1(@day.example_input(1)) == 13
    end

    test "solves actual input" do
      assert @day.part1(@actual_input) == 1424
    end
  end

  describe "part2" do
    test "solves sample input" do
      assert @day.part2(@day.example_input(2)) == 43
    end

    test "solves actual input" do
      assert @day.part2(@actual_input) == 8727
    end
  end
end
