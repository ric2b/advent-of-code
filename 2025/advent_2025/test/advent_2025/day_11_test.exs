defmodule Advent2025.Day11Test do
  use ExUnit.Case, async: true
  use Advent2025.AdventCase

  describe "part1" do
    test "solves sample input" do
      assert @day.part1(@day.example_input(1)) == 5
    end

    test "solves actual input" do
      assert @day.part1(@actual_input) == 539
    end
  end

  describe "part2" do
    @tag :skip
    test "solves sample input" do
      assert @day.part2(@day.example_input(2)) == :todo
    end

    @tag :skip
    test "solves actual input" do
      assert @day.part2(@actual_input) == :todo
    end
  end
end
