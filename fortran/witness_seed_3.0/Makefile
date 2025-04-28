FC = mpif90
FFLAGS = -O3 -Wall
SRC = src/rwd.f90 src/kairos.f90 src/io.f90 witness_seed_3.0.f90
OBJ = $(SRC:.f90=.o)
TARGET = witness_seed_3.0

all: $(TARGET)

$(TARGET): $(OBJ)
    $(FC) $(FFLAGS) -o $@ $^

%.o: %.f90
    $(FC) $(FFLAGS) -c $< -o $@

clean:
    rm -f $(OBJ) $(TARGET)

test:
    $(FC) $(FFLAGS) -o test_rwd tests/test_rwd.f90 src/rwd.f90
    $(FC) $(FFLAGS) -o test_kairos tests/test_kairos.f90 src/kairos.f90
    ./test_rwd
    ./test_kairos

.PHONY: all clean test