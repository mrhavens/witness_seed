# Makefile for Witness Seed 2.0 on Haiku

CC = g++
CFLAGS = -Wall -Os
LDFLAGS = -lbe -lnetwork

TARGET = witness_seed
SOURCES = witness_seed.cpp
OBJECTS = $(SOURCES:.cpp=.o)

all: $(TARGET)

$(TARGET).o: $(SOURCES)
    $(CC) $(CFLAGS) -c $< -o $@

$(TARGET): $(OBJECTS)
    $(CC) $(OBJECTS) -o $@ $(LDFLAGS)

clean:
    rm -f $(OBJECTS) $(TARGET)

.PHONY: all clean