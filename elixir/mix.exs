defmodule WitnessSeed.MixProject do
  use Mix.Project

  def project do
    [
      app: :witness_seed,
      version: "0.1.0",
      elixir: "~> 1.15",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      description: "Witness Seed 2.0: Swarm Storytelling Network Edition",
      package: [
        licenses: ["CC BY-NC-SA 4.0"],
        links: %{
          "Patreon" => "https://www.patreon.com/c/markrandallhavens",
          "Whitepapers" => "https://linktr.ee/KAIROS.ADAMON"
        }
      ]
    ]
  end

  def application do
    [
      extra_applications: [:logger],
      mod: {WitnessSeed.Application, []}
    ]
  end

  defp deps do
    []
  end
end