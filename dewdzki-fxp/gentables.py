"""Generate sine, gradient, and shape target tables for the demo as assembly includes."""
import math
import random
import sys

def gen_sine_table():
    """256-byte sine table, values 0-48 (max coarse scroll offset = 6 bytes).
    With 21-word DMA fetch (42 bytes) and 48-byte lines, coarse offset must be <= 6.
    Values 49+ would cause DMA to read past the end of each scanline."""
    lines = ["; Sine table: 256 bytes, values 0-48"]
    lines.append("sine_table:")
    for i in range(0, 256, 16):
        vals = []
        for j in range(16):
            v = int(24 + 24 * math.sin(2 * math.pi * (i + j) / 256))
            v = max(0, min(48, v))
            vals.append(f"${v:02X}")
        lines.append(f"\tdc.b\t{','.join(vals)}")
    return "\n".join(lines)

def gen_signed_sine_table():
    """256-word signed sine table, values -127..+127 for tunnel orbit."""
    lines = ["; Signed sine table: 256 words, values -127..+127"]
    lines.append("signed_sine_table:")
    for i in range(0, 256, 8):
        vals = []
        for j in range(8):
            v = int(round(127 * math.sin(2 * math.pi * (i + j) / 256)))
            v = max(-127, min(127, v))
            vals.append(f"${v & 0xFFFF:04X}")
        lines.append(f"\tdc.w\t{','.join(vals)}")
    return "\n".join(lines)

def gen_reciprocal_table():
    """256-word reciprocal table: recip[z] = round(16384/z), recip[0]=0."""
    lines = ["; Reciprocal table: 256 words, recip[z] = round(16384/z)"]
    lines.append("recip_table:")
    vals_all = []
    for z in range(256):
        if z == 0:
            vals_all.append(0)
        else:
            v = int(round(16384.0 / z))
            if v > 32767:
                v = 32767
            vals_all.append(v)
    for i in range(0, 256, 8):
        vals = [f"${vals_all[i+j]:04X}" for j in range(8)]
        lines.append(f"\tdc.w\t{','.join(vals)}")
    return "\n".join(lines)

def gen_yoffset_table():
    """256-word table: yoffset[y] = y * 48 (byte offset for scanline y)."""
    lines = ["; Y-offset table: 256 words, yoffset[y] = y * 48"]
    lines.append("yoffset_table:")
    for i in range(0, 256, 8):
        vals = [f"${(i+j)*48:04X}" for j in range(8)]
        lines.append(f"\tdc.w\t{','.join(vals)}")
    return "\n".join(lines)

def gen_gradient_table():
    """256-word gradient table: gold/silver flowing into each other, no black."""
    lines = ["; Gradient table: 256 words (gold/silver flow, no black)"]
    lines.append("gradient_table:")
    colors = []
    for i in range(256):
        # Smooth sine blend between gold and silver, never fading to black
        # t oscillates 0..1..0 over 256 entries (one full cycle)
        t = 0.5 + 0.5 * math.sin(2 * math.pi * i / 256)

        # Gold:   RGB ~ (1.0, 0.75, 0.2)  → Amiga $FB3
        # Silver: RGB ~ (0.85, 0.85, 0.95) → Amiga $DDF
        # Brightness modulation: gentle wave between 0.6 and 1.0
        bright = 0.7 + 0.3 * math.sin(2 * math.pi * i / 128)

        r = (1.0 * t + 0.85 * (1 - t)) * bright
        g = (0.75 * t + 0.85 * (1 - t)) * bright
        b = (0.2 * t + 0.95 * (1 - t)) * bright

        ri = min(15, max(1, int(r * 15 + 0.5)))
        gi = min(15, max(1, int(g * 15 + 0.5)))
        bi = min(15, max(1, int(b * 15 + 0.5)))
        colors.append((ri << 8) | (gi << 4) | bi)

    for i in range(0, 256, 8):
        vals = [f"${colors[i+j]:04X}" for j in range(8)]
        lines.append(f"\tdc.w\t{','.join(vals)}")
    return "\n".join(lines)

def _emit_targets(label, comment, coords):
    """Emit 120 target positions as dc.w x,y,z triples."""
    lines = [f"; {comment}"]
    lines.append(f"{label}:")
    for x, y, z in coords:
        lines.append(f"\tdc.w\t${x & 0xFFFF:04X},${y & 0xFFFF:04X},${z & 0xFFFF:04X}")
    return "\n".join(lines)

def gen_cube_targets():
    """120 stars on cube wireframe: 12 edges x 10 stars, coords +/-80."""
    HALF = 80
    STEP = 16
    edges = [
        (-HALF,-HALF,-HALF, STEP,0,0), (-HALF,-HALF,HALF, STEP,0,0),
        (-HALF,-HALF,-HALF, 0,0,STEP), (HALF,-HALF,-HALF, 0,0,STEP),
        (-HALF,HALF,-HALF, STEP,0,0),  (-HALF,HALF,HALF, STEP,0,0),
        (-HALF,HALF,-HALF, 0,0,STEP),  (HALF,HALF,-HALF, 0,0,STEP),
        (-HALF,-HALF,-HALF, 0,STEP,0), (HALF,-HALF,-HALF, 0,STEP,0),
        (-HALF,-HALF,HALF, 0,STEP,0),  (HALF,-HALF,HALF, 0,STEP,0),
    ]
    coords = []
    for sx, sy, sz, dx, dy, dz in edges:
        for i in range(10):
            coords.append((sx + dx * i, sy + dy * i, sz + dz * i))
    return _emit_targets("cube_targets", "Cube targets: 120 x (x,y,z) words", coords)

def gen_sphere_targets():
    """120 stars on sphere surface using Fibonacci lattice, radius 80."""
    n = 120
    r = 80
    golden_ratio = (1 + math.sqrt(5)) / 2
    coords = []
    for i in range(n):
        theta = math.acos(1 - 2 * (i + 0.5) / n)
        phi = 2 * math.pi * i / golden_ratio
        x = int(round(r * math.sin(theta) * math.cos(phi)))
        y = int(round(r * math.cos(theta)))
        z = int(round(r * math.sin(theta) * math.sin(phi)))
        coords.append((x, y, z))
    return _emit_targets("sphere_targets", "Sphere targets: 120 x (x,y,z) words", coords)

def gen_pyramid_targets():
    """120 stars on square pyramid wireframe: 8 edges x 15 stars."""
    HALF = 80
    apex = (0, HALF, 0)
    base = [(-HALF, -HALF, -HALF), (HALF, -HALF, -HALF),
            (HALF, -HALF, HALF), (-HALF, -HALF, HALF)]
    edges = []
    for i in range(4):
        edges.append((base[i], base[(i + 1) % 4]))
    for v in base:
        edges.append((v, apex))
    coords = []
    for (sx, sy, sz), (ex, ey, ez) in edges:
        for i in range(15):
            t = i / 14.0
            x = int(round(sx + (ex - sx) * t))
            y = int(round(sy + (ey - sy) * t))
            z = int(round(sz + (ez - sz) * t))
            coords.append((x, y, z))
    return _emit_targets("pyramid_targets", "Pyramid targets: 120 x (x,y,z) words", coords)

def gen_font_data():
    """16x32 font (8x8 source scaled 2x horiz, 4x vert). ASCII 32-90."""
    FONT = {
        ' ': [
            "........",
            "........",
            "........",
            "........",
            "........",
            "........",
            "........",
            "........",
        ],
        '-': [
            "........",
            "........",
            "........",
            ".######.",
            "........",
            "........",
            "........",
            "........",
        ],
        '.': [
            "........",
            "........",
            "........",
            "........",
            "........",
            "........",
            "..##....",
            "..##....",
        ],
        ',': [
            "........",
            "........",
            "........",
            "........",
            "........",
            "..##....",
            "..##....",
            ".##.....",
        ],
        "'": [
            "..##....",
            "..##....",
            ".##.....",
            "........",
            "........",
            "........",
            "........",
            "........",
        ],
        '0': [
            ".######.",
            "##....##",
            "##..####",
            "##.##.##",
            "####..##",
            "##....##",
            ".######.",
            "........",
        ],
        '1': [
            "...##...",
            "..###...",
            "...##...",
            "...##...",
            "...##...",
            "...##...",
            ".######.",
            "........",
        ],
        '2': [
            ".######.",
            "##....##",
            "......##",
            "...####.",
            ".##.....",
            "##......",
            "########",
            "........",
        ],
        '3': [
            ".######.",
            "##....##",
            "......##",
            "..#####.",
            "......##",
            "##....##",
            ".######.",
            "........",
        ],
        '4': [
            "....###.",
            "...####.",
            "..##.##.",
            ".##..##.",
            "########",
            ".....##.",
            ".....##.",
            "........",
        ],
        '5': [
            "########",
            "##......",
            "#######.",
            "......##",
            "......##",
            "##....##",
            ".######.",
            "........",
        ],
        '6': [
            ".######.",
            "##......",
            "#######.",
            "##....##",
            "##....##",
            "##....##",
            ".######.",
            "........",
        ],
        '7': [
            "########",
            "......##",
            ".....##.",
            "....##..",
            "...##...",
            "...##...",
            "...##...",
            "........",
        ],
        '8': [
            ".######.",
            "##....##",
            "##....##",
            ".######.",
            "##....##",
            "##....##",
            ".######.",
            "........",
        ],
        '9': [
            ".######.",
            "##....##",
            "##....##",
            ".#######",
            "......##",
            "......##",
            ".######.",
            "........",
        ],
        'A': [
            "..####..",
            ".##..##.",
            "##....##",
            "########",
            "##....##",
            "##....##",
            "##....##",
            "........",
        ],
        'B': [
            "#######.",
            "##....##",
            "##....##",
            "#######.",
            "##....##",
            "##....##",
            "#######.",
            "........",
        ],
        'C': [
            ".######.",
            "##....##",
            "##......",
            "##......",
            "##......",
            "##....##",
            ".######.",
            "........",
        ],
        'D': [
            "######..",
            "##...##.",
            "##....##",
            "##....##",
            "##....##",
            "##...##.",
            "######..",
            "........",
        ],
        'E': [
            "########",
            "##......",
            "##......",
            "######..",
            "##......",
            "##......",
            "########",
            "........",
        ],
        'F': [
            "########",
            "##......",
            "##......",
            "######..",
            "##......",
            "##......",
            "##......",
            "........",
        ],
        'G': [
            ".######.",
            "##....##",
            "##......",
            "##..####",
            "##....##",
            "##....##",
            ".######.",
            "........",
        ],
        'H': [
            "##....##",
            "##....##",
            "##....##",
            "########",
            "##....##",
            "##....##",
            "##....##",
            "........",
        ],
        'I': [
            ".######.",
            "...##...",
            "...##...",
            "...##...",
            "...##...",
            "...##...",
            ".######.",
            "........",
        ],
        'J': [
            "...#####",
            "......##",
            "......##",
            "......##",
            "##....##",
            "##....##",
            ".######.",
            "........",
        ],
        'K': [
            "##....##",
            "##...##.",
            "##..##..",
            "##.##...",
            "##..##..",
            "##...##.",
            "##....##",
            "........",
        ],
        'L': [
            "##......",
            "##......",
            "##......",
            "##......",
            "##......",
            "##......",
            "########",
            "........",
        ],
        'M': [
            "##....##",
            "###..###",
            "########",
            "##.##.##",
            "##....##",
            "##....##",
            "##....##",
            "........",
        ],
        'N': [
            "##....##",
            "###...##",
            "####..##",
            "##.##.##",
            "##..####",
            "##...###",
            "##....##",
            "........",
        ],
        'O': [
            ".######.",
            "##....##",
            "##....##",
            "##....##",
            "##....##",
            "##....##",
            ".######.",
            "........",
        ],
        'P': [
            "#######.",
            "##....##",
            "##....##",
            "#######.",
            "##......",
            "##......",
            "##......",
            "........",
        ],
        'Q': [
            ".######.",
            "##....##",
            "##....##",
            "##....##",
            "##..####",
            "##...##.",
            ".####.##",
            "........",
        ],
        'R': [
            "#######.",
            "##....##",
            "##....##",
            "#######.",
            "##..##..",
            "##...##.",
            "##....##",
            "........",
        ],
        'S': [
            ".######.",
            "##....##",
            "##......",
            ".######.",
            "......##",
            "##....##",
            ".######.",
            "........",
        ],
        'T': [
            "########",
            "...##...",
            "...##...",
            "...##...",
            "...##...",
            "...##...",
            "...##...",
            "........",
        ],
        'U': [
            "##....##",
            "##....##",
            "##....##",
            "##....##",
            "##....##",
            "##....##",
            ".######.",
            "........",
        ],
        'V': [
            "##....##",
            "##....##",
            "##....##",
            ".##..##.",
            ".##..##.",
            "..####..",
            "...##...",
            "........",
        ],
        'W': [
            "##....##",
            "##....##",
            "##....##",
            "##.##.##",
            "########",
            "###..###",
            "##....##",
            "........",
        ],
        'X': [
            "##....##",
            ".##..##.",
            "..####..",
            "...##...",
            "..####..",
            ".##..##.",
            "##....##",
            "........",
        ],
        'Y': [
            "##....##",
            ".##..##.",
            "..####..",
            "...##...",
            "...##...",
            "...##...",
            "...##...",
            "........",
        ],
        'Z': [
            "########",
            "......##",
            ".....##.",
            "...##...",
            "..##....",
            ".##.....",
            "########",
            "........",
        ],
    }

    def scale_row_2x(row_str):
        """Scale 8-pixel row to 16-pixel word (double each pixel)."""
        word = 0
        for i, ch in enumerate(row_str):
            if ch == '#':
                word |= (3 << (14 - i * 2))
        return word

    FIRST_CHAR = 32  # space
    LAST_CHAR = 90   # 'Z'
    VSCALE = 4       # vertical scale factor

    lines = [f"; Font data: 16x{8*VSCALE} per char, ASCII {FIRST_CHAR}-{LAST_CHAR}"]
    lines.append("font_data:")

    for code in range(FIRST_CHAR, LAST_CHAR + 1):
        ch = chr(code)
        if ch in FONT:
            rows = FONT[ch]
        else:
            rows = ["........"] * 8  # blank for undefined chars

        lines.append(f"\t; char {code} '{ch}'")
        for row_str in rows:
            word = scale_row_2x(row_str)
            for _ in range(VSCALE):
                lines.append(f"\tdc.w\t${word:04X}")

    return "\n".join(lines)


def gen_tunnel_targets():
    """120 pseudo-random scattered positions for tunnel re-entry."""
    rng = random.Random(42)
    coords = []
    for _ in range(120):
        x = rng.randint(-128, 127)
        y = rng.randint(-128, 127)
        z = rng.randint(20, 240)
        coords.append((x, y, z))
    return _emit_targets("tunnel_targets", "Tunnel targets: 120 x (x,y,z) words", coords)

def main():
    out = sys.argv[1] if len(sys.argv) > 1 else "tables.i"
    content = "; Auto-generated by gentables.py — do not edit\n\n"
    content += gen_sine_table() + "\n\n"
    content += gen_signed_sine_table() + "\n\n"
    content += gen_reciprocal_table() + "\n\n"
    content += gen_yoffset_table() + "\n\n"
    content += gen_gradient_table() + "\n\n"
    content += gen_tunnel_targets() + "\n\n"
    content += gen_cube_targets() + "\n\n"
    content += gen_sphere_targets() + "\n\n"
    content += gen_pyramid_targets() + "\n\n"
    content += gen_font_data() + "\n"
    with open(out, "w") as f:
        f.write(content)
    print(f"Written {out}")

if __name__ == "__main__":
    main()
