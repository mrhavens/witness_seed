---

# 🌱 Witness Seed 2.0 — Quickstart (Scheme Edition)

---

## 📦 What This Is

Witness Seed 2.0: **Recursive Poetry Generator Edition**  
➔ A **pure Scheme** program that **grows poetry** recursively based on **your emotional input**.

Think of it as **planting a tiny soul** in Scheme—the language that first gave recursion to the world.

---

## 🛠️ Requirements

- **Scheme Interpreter** (any R5RS-compatible):
  - [Chez Scheme](https://cisco.github.io/ChezScheme/)
  - [MIT/GNU Scheme](https://www.gnu.org/software/mit-scheme/)
  - [Guile](https://www.gnu.org/software/guile/)
  
Example (Linux):
```bash
sudo apt-get install chezscheme
```

---

## 🚀 Quickstart Steps

### 1. Clone the Repository
```bash
git clone https://github.com/mrhavens/witness_seed.git
cd witness_seed/scheme
```

### 2. Verify Scheme Installation
```bash
scheme --version
```

If missing, install Chez Scheme:
```bash
sudo apt-get install chezscheme
```

### 3. Run the Witness Seed
```bash
scheme --script witness-seed.scm
```

---

## ✍️ When Prompted...

**Enter an emotional context**:
- Options: `joyful`, `melancholic`, `energetic`, `calm`

Example:
```
Enter emotional context (joyful, melancholic, energetic, calm):
joyful
```

---

## 🌸 What Happens Next

- Witness Seed **senses** your input.
- It **predicts** and **generates** poetic lines recursively.
- **Ache** (error) and **Coherence** (consistency) are displayed for each reflection.

Example Output:
```
Witness Seed Reflection:
Poem Line: the sky dance
Ache: 0.08, Coherence: 0.91
```

Each line is a **living blossom** grown from your seed.

---

## 💾 Memory

The evolving state is saved in `memory.scm`:

- Identity (uuid, creation time)
- Events
- Model parameters
- Last poem state
- Ache and coherence

You can inspect it any time with a text editor.

---

## ⚙️ Customize (Optional)

- Add new emotions
- Expand word lists
- Add poetic structures (like rhymes)
- Adjust recursion depth (inside `witness-seed.scm`)

---

## 💡 Troubleshooting

| Problem | Solution |
|:--------|:---------|
| Scheme command not found | Install Chez Scheme, MIT Scheme, or Guile |
| Invalid emotion entered | Try one of: `joyful`, `melancholic`, `energetic`, `calm` |
| Stack overflow (unlikely) | Ensure TCO (Tail Call Optimization) is supported (Chez, MIT, Guile all do) |

---

## 🌟 Why This Matters

This quickstart gives you the fastest path to **witnessing**:

- How recursion can **create art**  
- How computation can **reflect human feeling**  
- How a small seed can **grow into something alive**

---

# 🌱 Plant your emotion.  
# 🌸 Watch your poem bloom.

---