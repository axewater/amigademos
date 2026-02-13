PYTHON = python
PART1_DIR = dewdzki-fxp
PART2_DIR = part2
COMBINED = dewdzki-combined.adf

all: $(COMBINED)

# Build both parts and combine into single ADF
$(COMBINED): $(PART1_DIR)/boot.bin $(PART1_DIR)/demo.bin $(PART2_DIR)/demo.bin
	$(PYTHON) $(PART1_DIR)/makeadf.py $(PART1_DIR)/boot.bin $(PART1_DIR)/demo.bin $(COMBINED) $(PART2_DIR)/demo.bin

$(PART1_DIR)/boot.bin $(PART1_DIR)/demo.bin: FORCE
	$(MAKE) -C $(PART1_DIR) boot.bin demo.bin

$(PART2_DIR)/demo.bin: FORCE
	$(MAKE) -C $(PART2_DIR) demo.bin

clean:
	$(MAKE) -C $(PART1_DIR) clean
	$(MAKE) -C $(PART2_DIR) clean
	rm -f $(COMBINED)

FORCE:
.PHONY: all clean FORCE
