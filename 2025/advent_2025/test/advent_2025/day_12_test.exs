defmodule Advent2025.Day12Test do
  use ExUnit.Case, async: true
  use Advent2025.AdventCase

  describe "part1" do
    @tag :skip
    test "solves sample input" do
      assert @day.part1(@day.example_input(1)) == 2
    end

    test "solves actual input" do
      assert @day.part1(@actual_input) == 492
    end
  end
end
