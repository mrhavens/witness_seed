# Makefile for Witness Seed 2.0 on AVR

CC = avr-gcc
CFLAGS = -mmcu=atmega328p -DF_CPU=16000000UL -Os
OBJCOPY = avr-objcopy
AVRDUDE = avrdude

TARGET = witness_seed
SOURCES = witness_seed.c
OBJECTS = $(SOURCES:.c=.o)

all: $(TARGET).hex

$(TARGET).o: $(SOURCES)
    $(CC) $(CFLAGS) -c $< -o $@

$(TARGET).elf: $(OBJECTS)
    $(CC) $(CFLAGS) -o $@ $^

$(TARGET).hex: $(TARGET).elf
    $(OBJCOPY) -O ihex -R .eeprom $< $@

flash: $(TARGET).hex
    $(AVRDUDE) -F -V -c arduino -p ATMEGA328P -P /dev/ttyUSB0 -b 115200 -U flash:w:$<

clean:
    rm -f $(OBJECTS) $(TARGET).elf $(TARGET).hex

.PHONY: all flash clean