defmodule Advent2025.Day08Test do
  use ExUnit.Case, async: true
  use Advent2025.AdventCase

  describe "part1" do
    test "solves sample input" do
      assert @day.part1(@day.example_input(1), 10) == 40
    end

    test "solves actual input" do
      assert @day.part1(@actual_input) == 352584
    end
  end

  describe "part2" do
    test "solves sample input" do
      assert @day.part2(@day.example_input(2)) == 25272
    end

    test "solves actual input" do
      assert @day.part2(@actual_input) == 9617397716
    end
  end
end
