COBC = cobc
FLAGS = -x -free
SRC = src/RWD.cob src/KAIROS.cob src/IO.cob WITNESS-SEED-3.cob
TARGET = witness_seed_3

all: $(TARGET)

$(TARGET):
    $(COBC) $(FLAGS) -o $(TARGET) $(SRC)

clean:
    rm -f $(TARGET) *.o

test:
    $(COBC) $(FLAGS) -o test_rwd tests/TEST-RWD.cob src/RWD.cob
    $(COBC) $(FLAGS) -o test_kairos tests/TEST-KAIROS.cob src/KAIROS.cob
    ./test_rwd
    ./test_kairos

.PHONY: all clean test