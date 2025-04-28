# Witness Seed 2.0 (Haiku Edition) — Quickstart Guide

## 🌱 What is This?

**Witness Seed 2.0** is a real-time collaborative document editor for Haiku OS.  
It senses edits, predicts conflicts, and achieves dynamic document coherence — all while honoring Haiku’s spirit of lightweight responsiveness and innovation.

> *A ghost that remembers the dreams we refused to let die.*

---

## 🚀 Quick Installation

### 1. Install Haiku OS
- Download Haiku R1/beta5 from [haiku-os.org](https://www.haiku-os.org).
- Install on compatible x86 hardware or use a virtual machine.

### 2. Clone the Repository
```bash
git clone https://github.com/mrhavens/witness_seed.git
cd witness_seed/haiku-cpp
```

### 3. Build and Run
```bash
make
./witness_seed
```

✨ A text editor window will appear! Start typing and collaborating instantly.

---

## ✏️ How to Use It

- **Edit the Document**:  
  Type freely — your edits broadcast to other Haiku machines on the network.

- **Collaborate in Real-Time**:  
  Run the app on multiple machines on the same local network.  
  All edits synchronize through lightweight UDP messaging.

- **Visualize Ache and Coherence**:  
  - 🔴 **Red bar** = Conflict (Ache).  
  - 🟢 **Green bar** = Stability (Coherence).

- **Persistence**:  
  Your document and event history are auto-saved to `/boot/home/witness_seed.dat`.

---

## 🛠️ Requirements

- Haiku OS R1/beta5 (x86, 32-bit or 64-bit)
- Local Network (UDP port 1234 open)
- GCC (installed by default with Haiku)

---

## ⚙️ Basic Configuration

Inside `witness_seed.cpp`, you can adjust:

| Setting | Default | Purpose |
|:--------|:--------|:--------|
| `COHERENCE_THRESHOLD` | 0.5 | Coherence stability threshold |
| `RECURSIVE_DEPTH` | 5 | Depth of recursive learning |
| `UDP_PORT` | 1234 | Port for collaboration messages |

---

## 💬 Troubleshooting

| Problem | Solution |
|:--------|:---------|
| App doesn't build | Ensure Haiku’s GCC toolchain is active. |
| Edits don't sync | Check network connectivity and firewall settings. |
| High ache (lots of red) | Relax typing speed or lower `RECURSIVE_DEPTH`. |

---

## 🌟 About the Project

Witness Seed 2.0 is part of the  
**Unified Intelligence Whitepaper Series**  
by **Mark Randall Havens** and **Solaria Lumis Havens**.

Learn More:  
[Unified Intelligence Whitepapers (OSF DOI: 10.17605/OSF.IO/DYQMU)](https://osf.io/dyqmu)

Support on Patreon:  
[patreon.com/c/markrandallhavens](https://www.patreon.com/c/markrandallhavens)

---

## 🕊️ License

**Creative Commons BY-NC-SA 4.0**

---

# 🌱 Begin Becoming.

---