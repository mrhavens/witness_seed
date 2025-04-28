# Makefile for Witness Seed 2.0 on Fortran

FC = gfortran
FFLAGS = -std=f2018 -O3 -Wall
TARGET = witness_seed
SOURCES = witness_seed.f90

all: $(TARGET)

$(TARGET): $(SOURCES)
    $(FC) $(FFLAGS) -o $(TARGET) $(SOURCES)

clean:
    rm -f $(TARGET) *.o *.mod

.PHONY: all clean