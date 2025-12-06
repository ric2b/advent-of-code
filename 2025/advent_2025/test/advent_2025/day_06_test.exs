defmodule Advent2025.Day06Test do
  use ExUnit.Case, async: true
  use Advent2025.AdventCase

  describe "part1" do
    test "solves sample input" do
      assert @day.part1(@day.example_input(1)) == 4277556
    end

    test "solves actual input" do
      assert @day.part1(@actual_input) == 3968933219902
    end
  end

  describe "part2" do
    test "solves sample input" do
      assert @day.part2(@day.example_input(2)) == 3263827
    end

    test "solves actual input" do
      assert @day.part2(@actual_input) == 6019576291014
    end
  end
end
