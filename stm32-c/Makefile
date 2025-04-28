# Makefile for Witness Seed 2.0 on STM32

CC = arm-none-eabi-gcc
CFLAGS = -mcpu=cortex-m3 -mthumb -Os -Wall -fdata-sections -ffunction-sections -I.
LDFLAGS = -mcpu=cortex-m3 -mthumb -specs=nosys.specs -Tstm32f1.ld -Wl,--gc-sections
OBJCOPY = arm-none-eabi-objcopy
STFLASH = st-flash

TARGET = witness_seed
SOURCES = witness_seed.c
OBJECTS = $(SOURCES:.c=.o)

all: $(TARGET).bin

$(TARGET).o: $(SOURCES)
    $(CC) $(CFLAGS) -c $< -o $@

$(TARGET).elf: $(OBJECTS)
    $(CC) $(OBJECTS) $(LDFLAGS) -o $@

$(TARGET).bin: $(TARGET).elf
    $(OBJCOPY) -O binary $< $@

flash: $(TARGET).bin
    $(STFLASH) write $< 0x8000000

clean:
    rm -f $(OBJECTS) $(TARGET).elf $(TARGET).bin

.PHONY: all flash clean