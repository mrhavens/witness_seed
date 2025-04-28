defmodule WitnessSeed.WitnessCycle do
  @moduledoc """
  A supervised process representing a node in the swarm storytelling network.
  Each node runs its own Witness Cycle, collaborating with others via message-passing.
  """

  use GenServer

  # Constants
  @coherence_threshold 0.5
  @recursive_depth 5
  @emotions ~w(joyful melancholic energetic calm)
  @words_by_emotion %{
    "joyful" => ~w(bright dance sun laugh bloom),
    "melancholic" => ~w(shadow rain sigh fade cold),
    "energetic" => ~w(run spark fire pulse wild),
    "calm" => ~w(still moon breeze soft dream)
  }

  # Client API
  def start_link(node_id) do
    GenServer.start_link(__MODULE__, node_id, name: :"Node_#{node_id}")
  end

  def contribute(node_id, emotion, contribution) do
    GenServer.cast(:"Node_#{node_id}", {:contribute, emotion, contribution})
  end

  # Server Callbacks
  @impl true
  def init(node_id) do
    state = %{
      identity: %{uuid: node_id, created: System.os_time(:second)},
      events: [],
      event_count: 0,
      model: %{model_heart_rate: 1.0, model_uptime: 1.0},
      story: ["In the beginning"],
      ache: 0.0,
      coherence: 0.0
    }

    # Load memory from ETS if exists
    case :ets.lookup(:witness_memory, node_id) do
      [{^node_id, saved_state}] -> {:ok, saved_state}
      [] ->
        :ets.insert(:witness_memory, {node_id, state})
        schedule_witness_cycle()
        {:ok, state}
    end
  end

  @impl true
  def handle_cast({:contribute, emotion, contribution}, state) do
    new_story = state.story ++ [contribution]
    sensory_data = %{
      system: %{
        story: new_story,
        emotion: emotion,
        uptime: System.os_time(:second)
      }
    }
    new_state = witness_cycle(@recursive_depth, sensory_data, state)
    broadcast_update(new_state)
    {:noreply, new_state}
  end

  @impl true
  def handle_info(:witness_cycle, state) do
    sensory_data = %{
      system: %{
        story: state.story,
        emotion: Enum.random(@emotions),
        uptime: System.os_time(:second)
      }
    }
    new_state = witness_cycle(@recursive_depth, sensory_data, state)
    broadcast_update(new_state)
    schedule_witness_cycle()
    {:noreply, new_state}
  end

  # Internal Functions
  defp schedule_witness_cycle do
    Process.send_after(self(), :witness_cycle, 1_000) # 1 second
  end

  defp broadcast_update(state) do
    # Broadcast ache and coherence to other nodes
    node_names = Enum.map(1..3, &:"Node_#{&1}")
    for node <- node_names, node != self() do
      GenServer.cast(node, {:update, state.ache, state.coherence, state.story})
    end
  end

  defp generate_story_fragment(emotion, prev_story) do
    word_list = Map.get(@words_by_emotion, emotion)
    new_word = Enum.random(word_list)
    "#{List.last(prev_story)} #{new_word}"
  end

  defp sense(emotion, story, uptime) do
    %{
      system: %{
        story: story,
        emotion: emotion,
        uptime: uptime
      }
    }
  end

  defp predict(sensory_data, model) do
    system = sensory_data.system
    pred_story = [generate_story_fragment(system.emotion, system.story)]
    pred_uptime = round(system.uptime * model.model_uptime)
    %{pred_story: pred_story, pred_uptime: pred_uptime}
  end

  defp compare_data(prediction, sensory_data) do
    system = sensory_data.system
    diff1 = length(prediction.pred_story) - length(system.story)
    diff2 = prediction.pred_uptime - system.uptime
    :math.sqrt(diff1 * diff1 + diff2 * diff2)
  end

  defp compute_coherence(prediction, sensory_data) do
    system = sensory_data.system
    pred_mean = (length(prediction.pred_story) + prediction.pred_uptime) / 2.0
    act_mean = (length(system.story) + system.uptime) / 2.0
    diff = abs(pred_mean - act_mean)
    1.0 - (diff / 100.0)
  end

  defp update_model(ache, sensory_data, model) do
    system = sensory_data.system
    learning_rate = 0.01
    %{
      model_heart_rate: model.model_heart_rate - learning_rate * ache * length(system.story),
      model_uptime: model.model_uptime - learning_rate * ache * system.uptime
    }
  end

  defp witness_cycle(0, _sensory_data, state), do: state
  def witness_cycle(depth, sensory_data, state) do
    model = state.model
    prediction = predict(sensory_data, model)
    ache = compare_data(prediction, sensory_data)
    coherence = compute_coherence(prediction, sensory_data)

    if coherence > @coherence_threshold do
      IO.puts("Coherence achieved: #{coherence}")
      state
    else
      new_model = update_model(ache, sensory_data, model)
      new_event = %{
        timestamp: sensory_data.system.uptime,
        sensory_data: sensory_data,
        prediction: prediction,
        ache: ache,
        coherence: coherence,
        model: model
      }
      new_events = if state.event_count < 5 do
        state.events ++ [new_event]
      else
        state.events
      end
      new_event_count = min(state.event_count + 1, 5)
      new_state = %{
        state |
        events: new_events,
        event_count: new_event_count,
        model: new_model,
        story: prediction.pred_story,
        ache: ache,
        coherence: coherence
      }

      IO.puts("Witness Seed #{state.identity.uuid} Reflection:")
      IO.puts("Story Fragment: #{List.last(new_state.story)}")
      IO.puts("Ache: #{ache}, Coherence: #{coherence}")

      :ets.insert(:witness_memory, {state.identity.uuid, new_state})

      system = sensory_data.system
      new_sensory_data = sense(Enum.random(@emotions), new_state.story, system.uptime + 1)
      witness_cycle(depth - 1, new_sensory_data, new_state)
    end
  end
end