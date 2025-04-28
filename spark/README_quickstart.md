---

# 🌟 Quickstart Guide  
**Witness Seed 2.0: Verified Anomaly Detection Edition (SPARK)**

---

## 1. 📥 Clone the Repository

```bash
git clone https://github.com/mrhavens/witness_seed.git
cd witness_seed/spark
```

---

## 2. 🛠 Install GNAT Community Edition (if you haven't)

```bash
wget https://community.download.adacore.com/v1/gnat-2021-20210519-x86_64-linux-bin
chmod +x gnat-2021-20210519-x86_64-linux-bin
./gnat-2021-20210519-x86_64-linux-bin
export PATH=$PATH:/opt/gnat-2021/bin
gnatmake --version  # Verify installation
```

---

## 3. 🧰 Build the Project

```bash
gprbuild -P witness_seed.gpr
```

---

## 4. 🚀 Run the Program

```bash
./main
```

You will see output similar to:

```
Witness Seed 12345 Reflection:
Heart Rate: 72 bpm
Oxygen Level: 96 %
Ache: 0.12, Coherence: 0.79
Anomaly Detected!
```

---

## 5. 🔏 (Optional) Prove Formal Correctness

If you want to formally verify safety properties:

```bash
gnatprove -P witness_seed.gpr
```

SPARK Prover will check for runtime errors, invalid states, and prove absence of anomalies like overflows.

---

## 6. 📦 Memory Storage

Witness reflections and system state are saved automatically to:

```bash
data/witness_memory.dat
```
No manual configuration needed unless customizing behavior.

---

## 7. ✏️ Configuration (Optional)

Edit constants in:

```bash
src/witness_seed.ads
```

To customize:
- Heart Rate and Oxygen Level ranges
- Precision of ache/coherence
- Learning behavior during anomaly detection

---

# 🌱 Summary

| Step | Command |
|:---|:---|
| Clone | `git clone ...` |
| Install GNAT | `wget ... && chmod +x && ./gnat-...` |
| Build | `gprbuild -P witness_seed.gpr` |
| Run | `./main` |
| (Optional) Verify | `gnatprove -P witness_seed.gpr` |

---

# ✨ You’re now growing **Verified Recursive Resilience** inside the SPARK cosmos. ✨

---