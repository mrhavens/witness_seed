DCC = dcc32
FLAGS = -CC
SRC = src\RWD.pas src\Kairos.pas src\IO.pas WitnessSeed3.dpr
TARGET = WitnessSeed3.exe

all: $(TARGET)

$(TARGET):
    $(DCC) $(FLAGS) $(SRC)

clean:
    del *.dcu *.exe

test:
    $(DCC) $(FLAGS) tests\TestRWD.pas src\RWD.pas -E.
    $(DCC) $(FLAGS) tests\TestKairos.pas src\Kairos.pas -E.
    TestRWD.exe
    TestKairos.exe

.PHONY: all clean test