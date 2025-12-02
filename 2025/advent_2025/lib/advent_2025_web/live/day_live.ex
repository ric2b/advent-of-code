defmodule Advent2025Web.DayLive do
  use Advent2025Web, :live_view

  @default_input """
  Enter your puzzle input here...
  """

  @impl true
  def mount(%{"day" => day}, _session, socket) do
    day_number = String.to_integer(day)
    module = get_day_module(day_number)

    has_previous = day_number > 1 and day_exists?(day_number - 1)
    has_next = day_exists?(day_number + 1)

    case module do
      {:ok, day_module} ->
        available_parts = day_module.available_parts()

        socket =
          socket
          |> assign(:day, day)
          |> assign(:day_number, day_number)
          |> assign(:day_module, day_module)
          |> assign(:available_parts, available_parts)
          |> assign(:has_part1, 1 in available_parts)
          |> assign(:has_part2, 2 in available_parts)
          |> assign(:has_previous, has_previous)
          |> assign(:has_next, has_next)
          |> assign(:input, get_default_input(day_module))
          |> calculate_results()

        {:ok, socket}

      {:error, :not_found} ->
        socket =
          socket
          |> put_flash(:error, "Day #{day_number} not found")
          |> assign(:day, day)
          |> assign(:day_number, day_number)
          |> assign(:day_module, nil)
          |> assign(:available_parts, [])
          |> assign(:has_part1, false)
          |> assign(:has_part2, false)
          |> assign(:has_previous, has_previous)
          |> assign(:has_next, has_next)
          |> assign(:input, @default_input)
          |> assign(:part1_result, "N/A")
          |> assign(:part1_time, 0)
          |> assign(:part2_result, "N/A")
          |> assign(:part2_time, 0)

        {:ok, socket}
    end
  end

  @impl true
  def handle_event("update_input", %{"input" => input}, socket) do
    socket =
      socket
      |> assign(:input, input)
      |> calculate_results()

    {:noreply, socket}
  end

  @impl true
  def handle_event("reset_input", _params, socket) do
    socket =
      socket
      |> assign(:input, get_default_input(socket.assigns.day_module))
      |> calculate_results()

    {:noreply, socket}
  end

  defp calculate_results(socket) do
    case socket.assigns.day_module do
      nil ->
        socket

      day_module ->
        input = socket.assigns.input
        available_parts = socket.assigns.available_parts

        {part1_time, part1_result} =
          if 1 in available_parts do
            try do
              :timer.tc(fn -> day_module.part1(input) end)
            rescue
              error ->
                {0, "Error: #{Exception.message(error)}"}
            end
          else
            {0, "N/A"}
          end

        {part2_time, part2_result} =
          if 2 in available_parts do
            try do
              :timer.tc(fn -> day_module.part2(input) end)
            rescue
              error ->
                {0, "Error: #{Exception.message(error)}"}
            end
          else
            {0, "N/A"}
          end

        socket
        |> assign(:part1_result, part1_result)
        |> assign(:part1_time, part1_time / 1_000)
        |> assign(:part2_result, part2_result)
        |> assign(:part2_time, part2_time / 1_000)
    end
  end

  defp get_day_module(day_number) do
    module_name = Module.concat([Advent2025, "Day#{String.pad_leading(to_string(day_number), 2, "0")}"])

    if Code.ensure_loaded?(module_name) do
      {:ok, module_name}
    else
      {:error, :not_found}
    end
  end

  defp day_exists?(day_number) do
    module_name = Module.concat([Advent2025, "Day#{String.pad_leading(to_string(day_number), 2, "0")}"])
    Code.ensure_loaded?(module_name)
  end

  defp get_default_input(day_module) when is_atom(day_module) do
    day_module.example_input(1)
  end

  defp get_default_input(_), do: @default_input

  @impl true
  def render(assigns) do
    ~H"""
    <Layouts.app flash={@flash}>
      <div class="min-h-screen bg-gradient-to-br from-slate-900 via-purple-900 to-slate-900 py-12 px-4">
        <div class="max-w-6xl mx-auto">
          <%!-- Header --%>
          <div class="text-center mb-8">
            <h1 class="text-5xl font-bold text-white mb-4">
              🎄 Advent of Code 2025
            </h1>
            <h2 class="text-3xl font-semibold text-green-400 mb-2">
              Day {@day_number}
            </h2>
            <div class="flex justify-center gap-2 mb-2">
              <%= if @has_part1 do %>
                <.icon name="hero-star-solid" class="w-6 h-6 text-yellow-400" />
              <% else %>
                <.icon name="hero-star" class="w-6 h-6 text-yellow-400/40" />
              <% end %>
              <%= if @has_part2 do %>
                <.icon name="hero-star-solid" class="w-6 h-6 text-yellow-400" />
              <% else %>
                <.icon name="hero-star" class="w-6 h-6 text-yellow-400/40" />
              <% end %>
            </div>
          </div>

          <%!-- Day Navigation --%>
          <div class="flex items-center justify-center gap-4 mb-8">
            <%= if @has_previous do %>
              <.link
                navigate={~p"/day/#{@day_number - 1}"}
                class="flex items-center gap-2 px-5 py-2.5 bg-slate-800/70 hover:bg-slate-700/70 text-white rounded-lg border border-purple-500/30 hover:border-purple-500/60 transition-all duration-200 transform hover:scale-105 font-medium"
              >
                <.icon name="hero-chevron-left" class="w-5 h-5" />
                Day {@day_number - 1}
              </.link>
            <% else %>
              <span class="flex items-center gap-2 px-5 py-2.5 bg-slate-800/30 text-gray-500 rounded-lg border border-slate-700/30 cursor-not-allowed font-medium">
                <.icon name="hero-chevron-left" class="w-5 h-5" />
                Day {@day_number - 1}
              </span>
            <% end %>

            <.link
              navigate={~p"/"}
              class="flex items-center gap-2 px-5 py-2.5 bg-purple-600/80 hover:bg-purple-600 text-white rounded-lg transition-all duration-200 transform hover:scale-105 font-medium"
            >
              <.icon name="hero-home" class="w-5 h-5" />
              Home
            </.link>

            <%= if @has_next do %>
              <.link
                navigate={~p"/day/#{@day_number + 1}"}
                class="flex items-center gap-2 px-5 py-2.5 bg-slate-800/70 hover:bg-slate-700/70 text-white rounded-lg border border-purple-500/30 hover:border-purple-500/60 transition-all duration-200 transform hover:scale-105 font-medium"
              >
                Day {@day_number + 1}
                <.icon name="hero-chevron-right" class="w-5 h-5" />
              </.link>
            <% else %>
              <span class="flex items-center gap-2 px-5 py-2.5 bg-slate-800/30 text-gray-500 rounded-lg border border-slate-700/30 cursor-not-allowed font-medium">
                Day {@day_number + 1}
                <.icon name="hero-chevron-right" class="w-5 h-5" />
              </span>
            <% end %>
          </div>

          <%= if @day_module do %>
            <div class="grid grid-cols-1 lg:grid-cols-2 gap-8">
              <%!-- Input Section --%>
              <div class="space-y-4">
                <div class="bg-slate-800/50 backdrop-blur-sm rounded-lg p-6 border border-purple-500/30 shadow-xl">
                  <div class="flex items-center justify-between mb-4">
                    <h3 class="text-xl font-semibold text-white flex items-center gap-2">
                      <.icon name="hero-document-text" class="w-6 h-6 text-purple-400" />
                      Input
                    </h3>
                    <button
                      phx-click="reset_input"
                      class="px-4 py-2 bg-purple-600 hover:bg-purple-700 text-white rounded-lg transition-all duration-200 transform hover:scale-105 text-sm font-medium"
                    >
                      Reset to Example
                    </button>
                  </div>

                  <form phx-change="update_input">
                    <textarea
                      name="input"
                      class="w-full h-96 bg-slate-900/80 text-green-300 font-mono text-sm p-4 rounded-lg border border-slate-700 focus:border-purple-500 focus:ring-2 focus:ring-purple-500/50 outline-none resize-none"
                      placeholder="Enter your puzzle input here..."
                    >{@input}</textarea>
                  </form>
                </div>
              </div>

              <%!-- Results Section --%>
              <div class="space-y-4">
                <%!-- Part 1 Result --%>
                <div class={[
                  "bg-slate-800/50 backdrop-blur-sm rounded-lg p-6 shadow-xl",
                  if(@has_part1, do: "border border-green-500/30", else: "border border-slate-700/30 opacity-50")
                ]}>
                  <h3 class="text-xl font-semibold text-white mb-4 flex items-center gap-2">
                    Part 1
                  </h3>

                  <div class="bg-slate-900/80 rounded-lg p-6 border border-slate-700">
                    <div class="text-center">
                      <p class="text-gray-400 text-sm mb-2">Result</p>
                      <p class={[
                        "text-5xl font-bold mb-2 break-words",
                        if(@has_part1, do: "text-green-400", else: "text-gray-500")
                      ]}>
                        {@part1_result}
                      </p>
                      <%= if @has_part1 do %>
                        <p class="text-gray-500 text-xs">
                          Calculated in {format_time(@part1_time)}
                        </p>
                      <% else %>
                        <p class="text-gray-600 text-xs">
                          Not implemented yet
                        </p>
                      <% end %>
                    </div>
                  </div>
                </div>

                <%!-- Part 2 Result --%>
                <div class={[
                  "bg-slate-800/50 backdrop-blur-sm rounded-lg p-6 shadow-xl",
                  if(@has_part2, do: "border border-blue-500/30", else: "border border-slate-700/30 opacity-50")
                ]}>
                  <h3 class="text-xl font-semibold text-white mb-4 flex items-center gap-2">
                    Part 2
                  </h3>

                  <div class="bg-slate-900/80 rounded-lg p-6 border border-slate-700">
                    <div class="text-center">
                      <p class="text-gray-400 text-sm mb-2">Result</p>
                      <p class={[
                        "text-5xl font-bold mb-2 break-words",
                        if(@has_part2, do: "text-blue-400", else: "text-gray-500")
                      ]}>
                        {@part2_result}
                      </p>
                      <%= if @has_part2 do %>
                        <p class="text-gray-500 text-xs">
                          Calculated in {format_time(@part2_time)}
                        </p>
                      <% else %>
                        <p class="text-gray-600 text-xs">
                          Not implemented yet
                        </p>
                      <% end %>
                    </div>
                  </div>
                </div>
              </div>
            </div>
          <% else %>
            <div class="text-center py-12">
              <div class="bg-red-900/30 border border-red-500/50 rounded-lg p-8 max-w-2xl mx-auto">
                <.icon name="hero-exclamation-triangle" class="w-16 h-16 text-red-400 mx-auto mb-4" />
                <h3 class="text-2xl font-bold text-white mb-2">
                  Day Not Found
                </h3>
                <p class="text-gray-300">
                  The solution for Day {@day_number} hasn't been implemented yet.
                </p>
                <div class="mt-6">
                  <.link
                    navigate="/"
                    class="inline-block px-6 py-3 bg-purple-600 hover:bg-purple-700 text-white rounded-lg transition-all duration-200 transform hover:scale-105 font-medium"
                  >
                    Go Home
                  </.link>
                </div>
              </div>
            </div>
          <% end %>
        </div>
      </div>
    </Layouts.app>
    """
  end

  defp format_time(microseconds) when microseconds < 1 do
    "< 0.001ms"
  end

  defp format_time(microseconds) when microseconds < 1000 do
    "#{Float.round(microseconds, 3)}ms"
  end

  defp format_time(microseconds) do
    seconds = microseconds / 1000
    "#{Float.round(seconds, 3)}s"
  end
end
