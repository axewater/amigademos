ASM = vasmm68k_mot
PYTHON = python
AFLAGS = -Fbin -m68000 -nosym

TARGET = dewdzki-fxp.adf
BOOTBIN = boot.bin
DEMOBIN = demo.bin
TABLES = tables.i

all: $(TARGET)

$(TARGET): $(BOOTBIN) $(DEMOBIN) makeadf.py
	$(PYTHON) makeadf.py $(BOOTBIN) $(DEMOBIN) $(TARGET)

$(BOOTBIN): boot.s
	$(ASM) $(AFLAGS) -o $(BOOTBIN) boot.s

$(DEMOBIN): demo.s $(TABLES) ptplayer_minimal.asm music.mod
	$(ASM) $(AFLAGS) -o $(DEMOBIN) demo.s

$(TABLES): gentables.py
	$(PYTHON) gentables.py $(TABLES)

clean:
	rm -f $(BOOTBIN) $(DEMOBIN) $(TARGET) $(TABLES)

.PHONY: all clean
