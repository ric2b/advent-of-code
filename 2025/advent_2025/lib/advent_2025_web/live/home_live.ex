defmodule Advent2025Web.HomeLive do
  use Advent2025Web, :live_view

  @example_code """
  defmodule Advent2025.DayXX do
    def example_input(part) do
      # Returns example input for part 1 or 2
      # part: 1 or 2
      # returns: String
      \"\"\"
      Your example input here...
      \"\"\"
    end

    def part1(input) do
      # Your solution for part 1
      # input: String
      # returns: Integer or String
    end

    def part2(input) do
      # Your solution for part 2
      # input: String
      # returns: Integer or String
    end
  end
  """

  @impl true
  def mount(_params, _session, socket) do
    days = get_available_days()

    socket =
      socket
      |> assign(:page_title, "Home")
      |> assign(:days, days)

    {:ok, socket}
  end

  defp get_available_days do
    1..12
    |> Enum.map(fn day ->
      day_padded = String.pad_leading(to_string(day), 2, "0")
      module_name = Module.concat([Advent2025, "Day#{day_padded}"])

      available_parts =
        if Code.ensure_loaded?(module_name) do
          module_name.available_parts()
        else
          []
        end

      %{
        number: day,
        padded: day_padded,
        available: Code.ensure_loaded?(module_name),
        available_parts: available_parts,
        has_part1: 1 in available_parts,
        has_part2: 2 in available_parts
      }
    end)
  end

  @impl true
  def render(assigns) do
    ~H"""
    <Layouts.app flash={@flash}>
      <div class="min-h-screen bg-gradient-to-br from-slate-900 via-purple-900 to-slate-900 py-12 px-4 flex flex-col">
        <div class="max-w-7xl mx-auto w-full">
          <%!-- Header --%>
          <div class="text-center mb-12">
            <h1 class="text-6xl font-bold text-white mb-4">
              🎄 Advent of Code 2025 🎄
            </h1>
            <p class="text-gray-400 text-sm mt-2">
              Click on any available day to view and test the solution
            </p>
          </div>

          <%!-- Days Grid --%>
          <div class="grid grid-cols-2 sm:grid-cols-3 md:grid-cols-4 lg:grid-cols-5 gap-4">
            <%= for day <- @days do %>
              <%= if day.available do %>
                <.link
                  navigate={"/day/#{day.padded}"}
                  class="group bg-gradient-to-br from-green-600 to-green-800 hover:from-green-500 hover:to-green-700 rounded-lg p-6 border-2 border-green-400/50 hover:border-green-300 shadow-lg hover:shadow-2xl transition-all duration-200 transform hover:scale-105 hover:-translate-y-1"
                >
                  <div class="text-center">
                    <div class="text-sm font-semibold text-green-200 mb-1">DAY</div>
                    <div class="text-5xl font-bold text-white mb-2">{day.number}</div>
                    <div class="flex justify-center gap-1 mb-2">
                      <%= if day.has_part1 do %>
                        <.icon name="hero-star-solid" class="w-4 h-4 text-yellow-300" />
                      <% else %>
                        <.icon name="hero-star" class="w-4 h-4 text-yellow-300/50" />
                      <% end %>
                      <%= if day.has_part2 do %>
                        <.icon name="hero-star-solid" class="w-4 h-4 text-yellow-300" />
                      <% else %>
                        <.icon name="hero-star" class="w-4 h-4 text-yellow-300/50" />
                      <% end %>
                    </div>
                    <div class="text-xs text-green-100 opacity-0 group-hover:opacity-100 transition-opacity">
                      View Solution →
                    </div>
                  </div>
                </.link>
              <% else %>
                <div class="bg-slate-800/30 rounded-lg p-6 border-2 border-slate-700/50 shadow-lg">
                  <div class="text-center opacity-40">
                    <div class="text-sm font-semibold text-gray-500 mb-1">DAY</div>
                    <div class="text-5xl font-bold text-gray-600 mb-2">{day.number}</div>
                    <div class="flex justify-center gap-1 mb-2">
                      <.icon name="hero-lock-closed" class="w-4 h-4 text-gray-600" />
                    </div>
                    <div class="text-xs text-gray-600">Not available</div>
                  </div>
                </div>
              <% end %>
            <% end %>
          </div>
        </div>

        <%!-- Stats Section (pushed to bottom) --%>
        <% total_parts = 24 %>
        <% completed_parts = Enum.sum(Enum.map(@days, fn d -> length(d.available_parts) end)) %>
        <div class="mt-auto pt-12 max-w-2xl mx-auto w-full">
          <div class="bg-slate-800/50 backdrop-blur-sm rounded-lg p-6 border border-purple-500/30 shadow-xl">
            <h3 class="text-xl font-semibold text-white mb-4 text-center flex items-center justify-center gap-2">
              <.icon name="hero-chart-bar" class="w-6 h-6 text-purple-400" />
              Progress
            </h3>
            <div class="flex justify-around">
              <div class="text-center">
                <div class="text-4xl font-bold text-yellow-400">
                  {completed_parts}
                </div>
                <div class="text-gray-400 text-sm mt-1">Stars Earned</div>
              </div>
              <div class="text-center">
                <div class="text-4xl font-bold text-gray-500">
                  {total_parts - completed_parts}
                </div>
                <div class="text-gray-400 text-sm mt-1">Stars Remaining</div>
              </div>
              <div class="text-center">
                <div class="text-4xl font-bold text-purple-400">
                  {round(completed_parts / total_parts * 100)}%
                </div>
                <div class="text-gray-400 text-sm mt-1">Progress</div>
              </div>
            </div>
          </div>
        </div>
      </div>
    </Layouts.app>
    """
  end
end
