defmodule Advent2025.Day07Test do
  use ExUnit.Case, async: true
  use Advent2025.AdventCase

  describe "part1" do
    test "solves sample input" do
      assert @day.part1(@day.example_input(1)) == 21
    end

    test "solves actual input" do
      assert @day.part1(@actual_input) == 1566
    end
  end

  describe "part2" do
    test "solves sample input" do
      assert @day.part2(@day.example_input(2)) == 40
    end

    test "solves actual input" do
      assert @day.part2(@actual_input) == 5921061943075
    end
  end
end
