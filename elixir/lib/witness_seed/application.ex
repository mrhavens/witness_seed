defmodule WitnessSeed.Application do
  @moduledoc """
  The main application module for Witness Seed 2.0: Swarm Storytelling Network Edition.
  Starts the supervision tree for witness cycle processes.
  """

  use Application

  @impl true
  def start(_type, _args) do
    # Start an ETS table for memory persistence
    :ets.new(:witness_memory, [:set, :public, :named_table])

    # Start 3 witness cycle processes (nodes) for the swarm
    children = [
      {WitnessSeed.Supervisor, num_nodes: 3}
    ]

    opts = [strategy: :one_for_one, name: WitnessSeed.Supervisor]
    Supervisor.start_link(children, opts)
  end
end