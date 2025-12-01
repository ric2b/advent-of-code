defmodule Advent2025Web.Day01Live do
  use Advent2025Web, :live_view

  alias Advent2025.Day01

  @default_input """
  L68
  L30
  R48
  L5
  R60
  L55
  L1
  L99
  R14
  L82
  """

  @impl true
  def mount(_params, _session, socket) do
    socket =
      socket
      |> assign(:input, @default_input)
      |> calculate_results()

    {:ok, socket}
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
      |> assign(:input, @default_input)
      |> calculate_results()

    {:noreply, socket}
  end

  defp calculate_results(socket) do
    input = socket.assigns.input

    {part1_time, part1_result} = :timer.tc(fn -> Day01.part1(input) end)
    {part2_time, part2_result} = :timer.tc(fn -> Day01.part2(input) end)

    socket
    |> assign(:part1_result, part1_result)
    |> assign(:part1_time, part1_time / 1_000)
    |> assign(:part2_result, part2_result)
    |> assign(:part2_time, part2_time / 1_000)
  end

  @impl true
  def render(assigns) do
    ~H"""
    <Layouts.app flash={@flash}>
      <div class="min-h-screen bg-gradient-to-br from-slate-900 via-purple-900 to-slate-900 py-12 px-4">
        <div class="max-w-6xl mx-auto">
          <%!-- Header --%>
          <div class="text-center mb-12">
            <h1 class="text-5xl font-bold text-white mb-4">
              🎄 Advent of Code 2025
            </h1>
            <h2 class="text-3xl font-semibold text-green-400 mb-2">
              Day 01
            </h2>
            <p class="text-gray-300 text-lg">
              Solution Viewer
            </p>
          </div>

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
              <div class="bg-slate-800/50 backdrop-blur-sm rounded-lg p-6 border border-green-500/30 shadow-xl">
                <h3 class="text-xl font-semibold text-white mb-4 flex items-center gap-2">
                  <.icon name="hero-star-solid" class="w-6 h-6 text-yellow-400" />
                  Part 1
                </h3>

                <div class="bg-slate-900/80 rounded-lg p-6 border border-slate-700">
                  <div class="text-center">
                    <p class="text-gray-400 text-sm mb-2">Result</p>
                    <p class="text-5xl font-bold text-green-400 mb-2">
                      {@part1_result}
                    </p>
                    <p class="text-gray-500 text-xs">
                      Calculated in {format_time(@part1_time)}
                    </p>
                  </div>
                </div>
              </div>

              <%!-- Part 2 Result --%>
              <div class="bg-slate-800/50 backdrop-blur-sm rounded-lg p-6 border border-blue-500/30 shadow-xl">
                <h3 class="text-xl font-semibold text-white mb-4 flex items-center gap-2">
                  <.icon name="hero-star-solid" class="w-6 h-6 text-yellow-400" />
                  <.icon name="hero-star-solid" class="w-6 h-6 text-yellow-400" />
                  Part 2
                </h3>

                <div class="bg-slate-900/80 rounded-lg p-6 border border-slate-700">
                  <div class="text-center">
                    <p class="text-gray-400 text-sm mb-2">Result</p>
                    <p class="text-5xl font-bold text-blue-400 mb-2">
                      {@part2_result}
                    </p>
                    <p class="text-gray-500 text-xs">
                      Calculated in {format_time(@part2_time)}
                    </p>
                  </div>
                </div>
              </div>
            </div>
          </div>
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
