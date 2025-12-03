defmodule Advent2025.Day03Test do
  use ExUnit.Case, async: true
  use Advent2025.AdventCase

  alias Advent2025.Day03

  describe "part1" do
    test "solves sample input" do
      assert Day03.part1(Day03.example_input(1)) == 357
    end

    test "solves actual input" do
      assert Day03.part1(@actual_input) == 17359
    end
  end

  describe "part2" do
    test "solves sample input" do
      assert Day03.part2(Day03.example_input(2)) == 3121910778619
    end

    test "solves actual input" do
      assert Day03.part2(@actual_input) == 172787336861064
    end
  end
end
