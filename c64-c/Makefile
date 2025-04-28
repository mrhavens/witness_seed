# Makefile for Witness Seed 2.0 on Commodore 64

CC = cc65
AS = ca65
LD = ld65
CFLAGS = -t c64 -Os --cpu 6502
LDFLAGS = -t c64 -o witness_seed.prg

TARGET = witness_seed.prg
SOURCES = witness_seed.c
OBJECTS = $(SOURCES:.c=.o)

all: $(TARGET)

$(TARGET).o: $(SOURCES)
    $(CC) $(CFLAGS) -c $< -o $@

$(TARGET): $(OBJECTS)
    $(LD) $(OBJECTS) $(LDFLAGS)

clean:
    rm -f $(OBJECTS) $(TARGET)

.PHONY: all clean