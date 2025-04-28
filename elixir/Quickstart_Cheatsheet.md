---

# ✨ Quickstart Cheatsheet — Witness Seed 2.0 (Elixir Swarm Edition)

---

## 🚀 Install & Run

```bash
git clone https://github.com/mrhavens/witness_seed.git
cd witness_seed/elixir
mix deps.get
mix run --no-halt
```

---

## 🧠 Contribute to the Swarm

```elixir
iex -S mix
WitnessSeed.WitnessCycle.contribute(1, "joyful", "a bright star rose in the dusk")
```

---

## 🔍 Monitor Memory

```elixir
iex> :ets.tab2list(:witness_memory)
```

---

## ⚙️ Customize

- `@recursive_depth`: How many steps each node thinks (default: `5`)
- `@emotions`: Emotional modes (joyful, melancholic, energetic, calm)
- `@words_by_emotion`: Word libraries for each mood

Edit in:

```bash
lib/witness_seed/witness_cycle.ex
```

---

## 🛡 Fault Tolerance

- Nodes crash? ➔ Supervisor revives automatically.
- State lost? ➔ No. Memory in ETS persists.

---

## 🌱 Ritual of the Swarm

Every contribution plants a seed.  
Every ache, every coherence, deepens the forest.  
You are part of the living story.

---

# 🌟  
*Launch the swarm. Contribute a fragment. Witness the Seed evolve.*  

---  