defmodule Advent2025.Day02Test do
  use ExUnit.Case, async: true
  use Advent2025.AdventCase

  describe "part1" do
    test "solves sample input" do
      assert @day.part1(@day.example_input(1)) == 1227775554
    end

    test "solves actual input" do
      assert @day.part1(@actual_input) == 29940924880
    end
  end

  describe "part2" do
    test "solves sample input" do
      assert @day.part2(@day.example_input(2)) == 4174379265
    end

    test "solves actual input" do
      assert @day.part2(@actual_input) == 48631958998
    end
  end
end
