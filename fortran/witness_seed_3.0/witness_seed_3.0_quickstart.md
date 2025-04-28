# Witness Seed 3.0 Quickstart

## Prerequisites
- Fortran compiler (e.g., gfortran, mpif90)
- MPI library (e.g., OpenMPI)
- Optional: NetCDF for climate data output

## Setup
1. Clone the repository: `git clone <repo-url>`
2. Navigate to `fortran/witness_seed_3.0`
3. Run `make` to compile: `make`

## Run
Execute the model: `mpirun -np 4 ./witness_seed_3.0`

## Output
Predictions are printed every 1000 steps. Future versions will write to `data/` in NetCDF format.

## Test
Run unit tests: `make test`

See [README.md](README.md) for the full vision.