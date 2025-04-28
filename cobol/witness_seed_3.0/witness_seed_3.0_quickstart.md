# Witness Seed 3.0 Quickstart

## Prerequisites
- COBOL compiler (e.g., GnuCOBOL, IBM Enterprise COBOL)
- Optional: VSAM or flat file support for data

## Setup
1. Clone the repository: `git clone <repo-url>`
2. Navigate to `cobol/witness_seed_3.0`
3. Run `make` to compile: `make`

## Run
Execute: `./witness_seed_3`

## Output
Predictions are displayed every 1000 steps. Future versions will write to `data/` in VSAM/flat files.

## Test
Run unit tests: `make test`

See [README.md](README.md) for the full vision.