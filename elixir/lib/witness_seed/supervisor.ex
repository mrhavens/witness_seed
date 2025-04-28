defmodule WitnessSeed.Supervisor do
  @moduledoc """
  Supervisor for the witness cycle processes (nodes) in the swarm.
  """

  use Supervisor

  def start_link(opts) do
    Supervisor.start_link(__MODULE__, opts, name: __MODULE__)
  end

  @impl true
  def init(opts) do
    num_nodes = Keyword.get(opts, :num_nodes, 3)

    children = Enum.map(1..num_nodes, fn id ->
      %{
        id: :"Node_#{id}",
        start: {WitnessSeed.WitnessCycle, :start_link, [id]}
      }
    end)

    Supervisor.init(children, strategy: :one_for_one)
  end
end