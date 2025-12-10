defmodule Advent2025.Day10Test do
  use ExUnit.Case, async: true
  use Advent2025.AdventCase

  describe "part1" do
    test "solves sample input" do
      assert @day.part1(@day.example_input(1)) == 7
    end

    test "solves actual input" do
      assert @day.part1(@actual_input) == 527
    end
  end

  describe "part2" do
    test "solves sample input" do
      assert @day.part2(@day.example_input(2)) == 33
    end

    @tag :skip
    @tag timeout: :infinity
    test "solves actual input" do
      assert @day.part2(@actual_input) == :todo
    end
  end
end
