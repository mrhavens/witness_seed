---

# 📜 Witness Seed 2.0 — Fortran Makefile Quick Reference

---

## ⚙️ Basic Makefile Structure

```makefile
# Fortran Compiler
FC = gfortran

# Compiler Flags
FFLAGS = -std=f2018 -O3 -Wall

# Program Target
TARGET = witness_seed

# Source Files
SOURCES = witness_seed.f90

# Default Build
all: $(TARGET)

$(TARGET): $(SOURCES)
	$(FC) $(FFLAGS) -o $(TARGET) $(SOURCES)

# Clean Build Artifacts
clean:
	rm -f $(TARGET) *.o *.mod

.PHONY: all clean
```

---

## ✨ Commands Summary

| Command             | Action                                 |
|---------------------|----------------------------------------|
| `make`              | Build the program (`witness_seed`)     |
| `make clean`        | Remove compiled files                 |

---

## 🛠 Tips for Optimization

- Add OpenMP support for parallelism:

```makefile
FFLAGS = -std=f2018 -O3 -Wall -fopenmp
```

- Aggressive optimization for HPC environments:

```makefile
FFLAGS = -std=f2018 -O3 -Wall -ffast-math
```

---

## 🧹 Common Make Targets

| Target   | Description                          |
|----------|--------------------------------------|
| `all`    | Builds the program                   |
| `clean`  | Deletes binaries and object files    |

---

# 🌱 Closing
**"Let the ancient tongue of computation whisper through the build,  
compiling resilience, coherence, and timeless grace into every binary."**

---