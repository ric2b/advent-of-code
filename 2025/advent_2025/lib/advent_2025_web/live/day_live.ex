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
          |> assign(:page_title, "Day #{day_number}")
          |> assign(:day, day)
          |> assign(:day_number, day_number)
          |> assign(:day_module, day_module)
          |> assign(:available_parts, available_parts)
          |> assign(:has_part1, 1 in available_parts)
          |> assign(:has_part2, 2 in available_parts)
          |> assign(:has_previous, has_previous)
          |> assign(:has_next, has_next)
          |> assign(:input, get_default_input(day_module))
          |> assign(:part1_result, nil)
          |> assign(:part1_loading, false)
          |> assign(:part2_result, nil)
          |> assign(:part2_loading, false)
          |> assign(:show_code, false)
          |> assign(:source_code, get_source_code(day_number))
          |> start_calculations()

        {:ok, socket}

      {:error, :not_found} ->
        socket =
          socket
          |> assign(:page_title, "Day #{day_number}")
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
          |> assign(:part1_loading, false)
          |> assign(:part2_result, "N/A")
          |> assign(:part2_loading, false)

        {:ok, socket}
    end
  end

  @impl true
  def handle_event("update_input", %{"input" => input}, socket) do
    socket =
      socket
      |> assign(:input, input)
      |> start_calculations()

    {:noreply, socket}
  end

  @impl true
  def handle_event("reset_input", _params, socket) do
    socket =
      socket
      |> assign(:input, get_default_input(socket.assigns.day_module))
      |> push_event("clear_saved_input", %{})
      |> start_calculations()

    {:noreply, socket}
  end

  @impl true
  def handle_event("load_saved_input", %{"input" => input}, socket) do
    socket =
      socket
      |> assign(:input, input)
      |> start_calculations()

    {:noreply, socket}
  end

  @impl true
  def handle_event("toggle_code", _params, socket) do
    {:noreply, assign(socket, show_code: !socket.assigns.show_code)}
  end

  defp start_calculations(socket) do
    case socket.assigns.day_module do
      nil ->
        socket

      day_module ->
        available_parts = socket.assigns.available_parts
        input = socket.assigns.input
        lv_pid = self()

        socket
        |> then(fn s ->
          if 1 in available_parts do
            Task.async(fn ->
              {time, result} =
                try do
                  :timer.tc(fn -> day_module.part1(input) end)
                rescue
                  error ->
                    {0, "Error: #{Exception.message(error)}"}
                end

              send(lv_pid, {:part1_result, result, time / 1_000})
            end)

            assign(s, :part1_loading, true)
          else
            assign(s, part1_result: "N/A", part1_time: 0, part1_loading: false)
          end
        end)
        |> then(fn s ->
          if 2 in available_parts do
            Task.async(fn ->
              {time, result} =
                try do
                  :timer.tc(fn -> day_module.part2(input) end)
                rescue
                  error ->
                    {0, "Error: #{Exception.message(error)}"}
                end

              send(lv_pid, {:part2_result, result, time / 1_000})
            end)

            assign(s, :part2_loading, true)
          else
            assign(s, part2_result: "N/A", part2_time: 0, part2_loading: false)
          end
        end)
    end
  end

  @impl true
  def handle_info({:part1_result, result, time}, socket) do
    {:noreply,
     socket
     |> assign(:part1_result, result)
     |> assign(:part1_time, time)
     |> assign(:part1_loading, false)}
  end

  @impl true
  def handle_info({:part2_result, result, time}, socket) do
    {:noreply,
     socket
     |> assign(:part2_result, result)
     |> assign(:part2_time, time)
     |> assign(:part2_loading, false)}
  end

  # Handle Task.async completion messages (we don't need the result here since we send our own message)
  @impl true
  def handle_info({ref, _result}, socket) when is_reference(ref) do
    Process.demonitor(ref, [:flush])
    {:noreply, socket}
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

  defp get_source_code(day_number) do
    day_padded = String.pad_leading(to_string(day_number), 2, "0")
    path = Path.join([:code.priv_dir(:advent_2025), "..", "lib", "advent_2025", "day_#{day_padded}.ex"])

    case File.read(path) do
      {:ok, content} ->
        content
        |> Makeup.Lexers.ElixirLexer.lex()
        |> Makeup.Formatters.HTML.HTMLFormatter.format_inner_as_binary([])

      {:error, _} ->
        "Source code not available"
    end
  end

  @impl true
  def render(assigns) do
    ~H"""
    <Layouts.app flash={@flash}>
      <div class="min-h-screen bg-gradient-to-br from-slate-900 via-purple-900 to-slate-900 py-12 px-4">
        <div class="max-w-6xl mx-auto">
          <%!-- Header --%>
          <div class="text-center mb-8">
            <h1 class="text-5xl font-bold text-white mb-4">
              🎄 Advent of Code 2025 🎄
            </h1>
            <a
              href={"https://adventofcode.com/2025/day/#{@day_number}"}
              target="_blank"
              rel="noopener noreferrer"
              class="inline-flex items-center gap-2 text-purple-300 hover:text-purple-100 text-sm transition-colors"
            >
              <.icon name="hero-arrow-top-right-on-square" class="w-4 h-4" />
              View Problem on Advent of Code
            </a>
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
            <.link
              :if={@has_previous}
              navigate={~p"/day/#{@day_number - 1}"}
              class="flex items-center gap-2 px-5 py-2.5 bg-slate-800/70 hover:bg-slate-700/70 text-white rounded-lg border border-purple-500/30 hover:border-purple-500/60 transition-all duration-200 transform hover:scale-105 font-medium"
            >
              <.icon name="hero-chevron-left" class="w-5 h-5" />
              Day {@day_number - 1}
            </.link>

            <.link
              navigate={~p"/"}
              class="flex items-center gap-2 px-5 py-2.5 bg-purple-600/80 hover:bg-purple-600 text-white rounded-lg transition-all duration-200 transform hover:scale-105 font-medium"
            >
              <.icon name="hero-home" class="w-5 h-5" />
              Home
            </.link>

            <.link
              :if={@has_next}
              navigate={~p"/day/#{@day_number + 1}"}
              class="flex items-center gap-2 px-5 py-2.5 bg-slate-800/70 hover:bg-slate-700/70 text-white rounded-lg border border-purple-500/30 hover:border-purple-500/60 transition-all duration-200 transform hover:scale-105 font-medium"
            >
              Day {@day_number + 1}
              <.icon name="hero-chevron-right" class="w-5 h-5" />
            </.link>
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
                      id={"input-day-#{@day_number}"}
                      name="input"
                      phx-hook=".PersistInput"
                      phx-debounce="200"
                      data-day={@day_number}
                      class="w-full h-96 bg-slate-900/80 text-green-300 font-mono text-sm p-4 rounded-lg border border-slate-700 focus:border-purple-500 focus:ring-2 focus:ring-purple-500/50 outline-none resize-none"
                      placeholder="Enter your puzzle input here..."
                    >{@input}</textarea>
                  </form>
                  <script :type={Phoenix.LiveView.ColocatedHook} name=".PersistInput">
                    export default {
                      mounted() {
                        const day = this.el.dataset.day;
                        const storageKey = `aoc2025_day_${day}_input`;

                        // Load saved input on mount
                        const savedInput = localStorage.getItem(storageKey);
                        if (savedInput) {
                          this.el.value = savedInput;
                          this.pushEvent("load_saved_input", { input: savedInput });
                        }

                        // Save input on change
                        this.el.addEventListener("input", (e) => {
                          localStorage.setItem(storageKey, e.target.value);
                        });

                        // Clear saved input when reset
                        this.handleEvent("clear_saved_input", () => {
                          localStorage.removeItem(storageKey);
                        });
                      }
                    }
                  </script>
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
                      <%= if @part1_loading do %>
                        <div class="flex flex-col items-center justify-center py-2">
                          <div class="w-12 h-12 border-4 border-green-400/30 border-t-green-400 rounded-full animate-spin mb-3"></div>
                          <p class="text-gray-400 text-sm animate-pulse">Calculating...</p>
                        </div>
                      <% else %>
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
                            Not available yet
                          </p>
                        <% end %>
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
                      <%= if @part2_loading do %>
                        <div class="flex flex-col items-center justify-center py-2">
                          <div class="w-12 h-12 border-4 border-blue-400/30 border-t-blue-400 rounded-full animate-spin mb-3"></div>
                          <p class="text-gray-400 text-sm animate-pulse">Calculating...</p>
                        </div>
                      <% else %>
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
                            Not available yet
                          </p>
                        <% end %>
                      <% end %>
                    </div>
                  </div>
                </div>
              </div>
            </div>

            <%!-- Solution Code Section --%>
            <div class="mt-8">
              <button
                phx-click="toggle_code"
                class="w-full flex items-center justify-between px-6 py-4 bg-slate-800/50 backdrop-blur-sm rounded-lg border border-purple-500/30 hover:border-purple-500/60 transition-all duration-200 text-white font-medium"
              >
                <span class="flex items-center gap-2">
                  <.icon name="hero-code-bracket" class="w-6 h-6 text-purple-400" />
                  View Solution Code
                </span>
                <.icon
                  name={if @show_code, do: "hero-chevron-up", else: "hero-chevron-down"}
                  class="w-5 h-5 text-purple-400"
                />
              </button>

              <%= if @show_code do %>
                <div class="mt-4 bg-slate-900/80 rounded-lg border border-slate-700 overflow-hidden">
                  <div class="flex items-center justify-between px-4 py-2 bg-slate-800/50 border-b border-slate-700">
                    <span class="text-sm text-gray-400 font-mono">
                      lib/advent_2025/day_{String.pad_leading(to_string(@day_number), 2, "0")}.ex
                    </span>
                  </div>
                  <pre class="p-4 overflow-x-auto text-sm"><code class="font-mono"><%= raw(@source_code) %></code></pre>
                </div>
              <% end %>
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
