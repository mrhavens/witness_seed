defmodule WitnessSeedTest do
  use ExUnit.Case
  doctest WitnessSeed

  test "starts witness cycle processes" do
    assert Supervisor.count_children(WitnessSeed.Supervisor).workers == 3
  end
end