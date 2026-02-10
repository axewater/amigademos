"""Build a bootable Amiga ADF disk image from bootblock + demo binary."""
import struct
import sys

ADF_SIZE = 901120  # 80 tracks * 2 sides * 11 sectors * 512 bytes
BOOTBLOCK_SIZE = 1024  # 2 sectors


def compute_checksum(block):
    """Compute Amiga bootblock checksum (ones' complement with end-around carry)."""
    s = 0
    for i in range(0, BOOTBLOCK_SIZE, 4):
        if i == 4:  # skip the checksum longword itself
            continue
        val = struct.unpack(">I", block[i:i+4])[0]
        s += val
        if s >= (1 << 32):
            s = (s & 0xFFFFFFFF) + 1
    return (~s) & 0xFFFFFFFF


def main():
    if len(sys.argv) < 4:
        print(f"Usage: {sys.argv[0]} boot.bin demo.bin output.adf")
        sys.exit(1)

    boot_file = sys.argv[1]
    demo_file = sys.argv[2]
    adf_out = sys.argv[3]

    with open(boot_file, "rb") as f:
        boot_data = f.read()
    with open(demo_file, "rb") as f:
        demo_data = f.read()

    if len(boot_data) > BOOTBLOCK_SIZE:
        print(f"ERROR: bootblock is {len(boot_data)} bytes, max is {BOOTBLOCK_SIZE}")
        sys.exit(1)

    print(f"Bootblock code: {len(boot_data)} bytes ({BOOTBLOCK_SIZE - len(boot_data)} bytes free)")
    print(f"Demo code: {len(demo_data)} bytes")

    # Pad bootblock to exactly 1024 bytes
    block = bytearray(boot_data) + bytearray(BOOTBLOCK_SIZE - len(boot_data))

    # Calculate and patch checksum
    checksum = compute_checksum(bytes(block))
    struct.pack_into(">I", block, 4, checksum)
    print(f"Checksum: ${checksum:08X}")

    # Verify checksum
    verify = 0
    for i in range(0, BOOTBLOCK_SIZE, 4):
        val = struct.unpack(">I", block[i:i+4])[0]
        verify += val
        if verify >= (1 << 32):
            verify = (verify & 0xFFFFFFFF) + 1
    assert verify == 0xFFFFFFFF, f"Checksum verification failed: ${verify:08X}"
    print("Checksum verified OK")

    # Build ADF: bootblock at offset 0, demo at offset 1024
    adf = bytearray(ADF_SIZE)
    adf[:BOOTBLOCK_SIZE] = block
    adf[BOOTBLOCK_SIZE:BOOTBLOCK_SIZE + len(demo_data)] = demo_data

    with open(adf_out, "wb") as f:
        f.write(adf)

    print(f"Written {adf_out} ({ADF_SIZE} bytes, demo at offset {BOOTBLOCK_SIZE})")


if __name__ == "__main__":
    main()
