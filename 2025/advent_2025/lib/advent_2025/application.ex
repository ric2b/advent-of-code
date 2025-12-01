defmodule Advent2025.Application do
  # See https://hexdocs.pm/elixir/Application.html
  # for more information on OTP Applications
  @moduledoc false

  use Application

  @impl true
  def start(_type, _args) do
    children = [
      Advent2025Web.Telemetry,
      Advent2025.Repo,
      {DNSCluster, query: Application.get_env(:advent_2025, :dns_cluster_query) || :ignore},
      {Phoenix.PubSub, name: Advent2025.PubSub},
      # Start a worker by calling: Advent2025.Worker.start_link(arg)
      # {Advent2025.Worker, arg},
      # Start to serve requests, typically the last entry
      Advent2025Web.Endpoint
    ]

    # See https://hexdocs.pm/elixir/Supervisor.html
    # for other strategies and supported options
    opts = [strategy: :one_for_one, name: Advent2025.Supervisor]
    Supervisor.start_link(children, opts)
  end

  # Tell Phoenix to update the endpoint configuration
  # whenever the application is updated.
  @impl true
  def config_change(changed, _new, removed) do
    Advent2025Web.Endpoint.config_change(changed, removed)
    :ok
  end
end
