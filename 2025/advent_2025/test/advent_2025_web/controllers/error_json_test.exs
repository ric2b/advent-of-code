defmodule Advent2025Web.ErrorJSONTest do
  use Advent2025Web.ConnCase, async: true

  test "renders 404" do
    assert Advent2025Web.ErrorJSON.render("404.json", %{}) == %{errors: %{detail: "Not Found"}}
  end

  test "renders 500" do
    assert Advent2025Web.ErrorJSON.render("500.json", %{}) ==
             %{errors: %{detail: "Internal Server Error"}}
  end
end
