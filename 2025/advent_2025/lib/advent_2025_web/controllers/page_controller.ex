defmodule Advent2025Web.PageController do
  use Advent2025Web, :controller

  def home(conn, _params) do
    render(conn, :home)
  end
end
