"""Generate all lookup tables for DEWDZKI-FXP Part 2 demo."""
import math
import random
import sys

# ── Sine table (256 bytes, 0-48) for horizontal scroll offset ──
def gen_sine_table():
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

# ── Signed sine table (256 words, -127..+127) ──
def gen_signed_sine_table():
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

# ── Reciprocal table (256 words) ──
def gen_reciprocal_table():
    lines = ["; Reciprocal table: 256 words, recip[z] = round(16384/z)"]
    lines.append("recip_table:")
    vals_all = []
    for z in range(256):
        if z == 0:
            vals_all.append(0)
        else:
            v = min(32767, int(round(16384.0 / z)))
            vals_all.append(v)
    for i in range(0, 256, 8):
        vals = [f"${vals_all[i+j]:04X}" for j in range(8)]
        lines.append(f"\tdc.w\t{','.join(vals)}")
    return "\n".join(lines)

# ── Y-offset table (256 words, y*48) ──
def gen_yoffset_table():
    lines = ["; Y-offset table: 256 words, yoffset[y] = y * 48"]
    lines.append("yoffset_table:")
    for i in range(0, 256, 8):
        vals = [f"${(i+j)*48:04X}" for j in range(8)]
        lines.append(f"\tdc.w\t{','.join(vals)}")
    return "\n".join(lines)

# ── Plasma color table (256 words) ──
def gen_plasma_table():
    lines = ["; Plasma table: 256 words, deep purple/blue/cyan/magenta"]
    lines.append("plasma_table:")
    colors = []
    for i in range(256):
        t = i / 256.0
        r = 0.5 + 0.5 * math.sin(2 * math.pi * t * 1.0)
        g = 0.5 + 0.5 * math.sin(2 * math.pi * t * 1.0 + 2.094)
        b = 0.5 + 0.5 * math.sin(2 * math.pi * t * 1.0 + 4.189)
        # Bias toward purple/blue/cyan
        r = r * 0.7
        b = min(1.0, b * 1.3)
        ri = min(15, max(0, int(r * 15 + 0.5)))
        gi = min(15, max(0, int(g * 15 + 0.5)))
        bi = min(15, max(0, int(b * 15 + 0.5)))
        colors.append((ri << 8) | (gi << 4) | bi)
    for i in range(0, 256, 8):
        vals = [f"${colors[i+j]:04X}" for j in range(8)]
        lines.append(f"\tdc.w\t{','.join(vals)}")
    return "\n".join(lines)

# ── Rainbow color table (256 words) ──
def gen_rainbow_table():
    lines = ["; Rainbow table: 256 words, full RGB rainbow for text"]
    lines.append("rainbow_table:")
    colors = []
    for i in range(256):
        h = i / 256.0  # hue 0-1
        # HSV to RGB with S=1, V=1
        h6 = h * 6.0
        sector = int(h6) % 6
        f = h6 - int(h6)
        if sector == 0:   r, g, b = 1.0, f, 0.0
        elif sector == 1: r, g, b = 1.0-f, 1.0, 0.0
        elif sector == 2: r, g, b = 0.0, 1.0, f
        elif sector == 3: r, g, b = 0.0, 1.0-f, 1.0
        elif sector == 4: r, g, b = f, 0.0, 1.0
        else:             r, g, b = 1.0, 0.0, 1.0-f
        ri = min(15, max(1, int(r * 15 + 0.5)))
        gi = min(15, max(1, int(g * 15 + 0.5)))
        bi = min(15, max(1, int(b * 15 + 0.5)))
        colors.append((ri << 8) | (gi << 4) | bi)
    for i in range(0, 256, 8):
        vals = [f"${colors[i+j]:04X}" for j in range(8)]
        lines.append(f"\tdc.w\t{','.join(vals)}")
    return "\n".join(lines)

# ── Raster bar gradient tables (4 bars × 16 words each) ──
def gen_rasterbar_tables():
    def make_gradient(r_peak, g_peak, b_peak, label, comment):
        lines = [f"; {comment}"]
        lines.append(f"{label}:")
        vals = []
        for i in range(16):
            # Distance from center (0..7..0)
            d = abs(i - 7.5) / 7.5  # 0 at center, 1 at edge
            bright = 1.0 - d * 0.8  # fade toward edges
            white_mix = max(0, 1.0 - d * 2.5)  # white center
            r = r_peak * bright + white_mix * (1.0 - r_peak)
            g = g_peak * bright + white_mix * (1.0 - g_peak)
            b = b_peak * bright + white_mix * (1.0 - b_peak)
            ri = min(15, max(0, int(r * 15 + 0.5)))
            gi = min(15, max(0, int(g * 15 + 0.5)))
            bi = min(15, max(0, int(b * 15 + 0.5)))
            vals.append(f"${(ri<<8)|(gi<<4)|bi:04X}")
        lines.append(f"\tdc.w\t{','.join(vals[:8])}")
        lines.append(f"\tdc.w\t{','.join(vals[8:])}")
        return "\n".join(lines)

    parts = []
    parts.append(make_gradient(1.0, 0.2, 0.2, "rasterbar_red", "Red raster bar: 16 words"))
    parts.append(make_gradient(0.2, 1.0, 1.0, "rasterbar_cyan", "Cyan raster bar: 16 words"))
    parts.append(make_gradient(0.2, 1.0, 0.2, "rasterbar_green", "Green raster bar: 16 words"))
    parts.append(make_gradient(1.0, 0.2, 1.0, "rasterbar_magenta", "Magenta raster bar: 16 words"))
    return "\n\n".join(parts)

# ── 8×8 font (96 chars, ASCII 32-127) ──
def gen_font():
    lines = ["; 8x8 font: 96 chars (ASCII 32-127), 768 bytes"]
    lines.append("font_data:")

    # Minimal 8x8 bitmap font - each char is 8 bytes (8 rows of 8 pixels)
    font = {}

    # Space
    font[32] = [0x00]*8

    # Punctuation and symbols
    font[33] = [0x18,0x18,0x18,0x18,0x18,0x00,0x18,0x00]  # !
    font[34] = [0x6C,0x6C,0x24,0x00,0x00,0x00,0x00,0x00]  # "
    font[35] = [0x6C,0xFE,0x6C,0x6C,0xFE,0x6C,0x00,0x00]  # #
    font[36] = [0x18,0x7E,0xC0,0x7C,0x06,0xFC,0x18,0x00]  # $
    font[37] = [0xC6,0xCC,0x18,0x30,0x66,0xC6,0x00,0x00]  # %
    font[38] = [0x38,0x6C,0x38,0x76,0xDC,0xCC,0x76,0x00]  # &
    font[39] = [0x18,0x18,0x30,0x00,0x00,0x00,0x00,0x00]  # '
    font[40] = [0x0C,0x18,0x30,0x30,0x30,0x18,0x0C,0x00]  # (
    font[41] = [0x30,0x18,0x0C,0x0C,0x0C,0x18,0x30,0x00]  # )
    font[42] = [0x00,0x66,0x3C,0xFF,0x3C,0x66,0x00,0x00]  # *
    font[43] = [0x00,0x18,0x18,0x7E,0x18,0x18,0x00,0x00]  # +
    font[44] = [0x00,0x00,0x00,0x00,0x00,0x18,0x18,0x30]  # ,
    font[45] = [0x00,0x00,0x00,0x7E,0x00,0x00,0x00,0x00]  # -
    font[46] = [0x00,0x00,0x00,0x00,0x00,0x18,0x18,0x00]  # .
    font[47] = [0x06,0x0C,0x18,0x30,0x60,0xC0,0x00,0x00]  # /

    # Digits 0-9
    font[48] = [0x7C,0xC6,0xCE,0xD6,0xE6,0xC6,0x7C,0x00]
    font[49] = [0x18,0x38,0x18,0x18,0x18,0x18,0x7E,0x00]
    font[50] = [0x7C,0xC6,0x06,0x3C,0x60,0xC0,0xFE,0x00]
    font[51] = [0x7C,0xC6,0x06,0x3C,0x06,0xC6,0x7C,0x00]
    font[52] = [0x1C,0x3C,0x6C,0xCC,0xFE,0x0C,0x0C,0x00]
    font[53] = [0xFE,0xC0,0xFC,0x06,0x06,0xC6,0x7C,0x00]
    font[54] = [0x3C,0x60,0xC0,0xFC,0xC6,0xC6,0x7C,0x00]
    font[55] = [0xFE,0x06,0x0C,0x18,0x30,0x30,0x30,0x00]
    font[56] = [0x7C,0xC6,0xC6,0x7C,0xC6,0xC6,0x7C,0x00]
    font[57] = [0x7C,0xC6,0xC6,0x7E,0x06,0x0C,0x78,0x00]

    # Punctuation : ; < = > ? @
    font[58] = [0x00,0x18,0x18,0x00,0x18,0x18,0x00,0x00]
    font[59] = [0x00,0x18,0x18,0x00,0x18,0x18,0x30,0x00]
    font[60] = [0x0C,0x18,0x30,0x60,0x30,0x18,0x0C,0x00]
    font[61] = [0x00,0x00,0x7E,0x00,0x7E,0x00,0x00,0x00]
    font[62] = [0x30,0x18,0x0C,0x06,0x0C,0x18,0x30,0x00]
    font[63] = [0x7C,0xC6,0x06,0x1C,0x18,0x00,0x18,0x00]
    font[64] = [0x7C,0xC6,0xDE,0xDE,0xDC,0xC0,0x7C,0x00]

    # Uppercase A-Z
    font[65] = [0x38,0x6C,0xC6,0xC6,0xFE,0xC6,0xC6,0x00]
    font[66] = [0xFC,0xC6,0xC6,0xFC,0xC6,0xC6,0xFC,0x00]
    font[67] = [0x7C,0xC6,0xC0,0xC0,0xC0,0xC6,0x7C,0x00]
    font[68] = [0xF8,0xCC,0xC6,0xC6,0xC6,0xCC,0xF8,0x00]
    font[69] = [0xFE,0xC0,0xC0,0xFC,0xC0,0xC0,0xFE,0x00]
    font[70] = [0xFE,0xC0,0xC0,0xFC,0xC0,0xC0,0xC0,0x00]
    font[71] = [0x7C,0xC6,0xC0,0xCE,0xC6,0xC6,0x7E,0x00]
    font[72] = [0xC6,0xC6,0xC6,0xFE,0xC6,0xC6,0xC6,0x00]
    font[73] = [0x7E,0x18,0x18,0x18,0x18,0x18,0x7E,0x00]
    font[74] = [0x1E,0x06,0x06,0x06,0xC6,0xC6,0x7C,0x00]
    font[75] = [0xC6,0xCC,0xD8,0xF0,0xD8,0xCC,0xC6,0x00]
    font[76] = [0xC0,0xC0,0xC0,0xC0,0xC0,0xC0,0xFE,0x00]
    font[77] = [0xC6,0xEE,0xFE,0xD6,0xC6,0xC6,0xC6,0x00]
    font[78] = [0xC6,0xE6,0xF6,0xDE,0xCE,0xC6,0xC6,0x00]
    font[79] = [0x7C,0xC6,0xC6,0xC6,0xC6,0xC6,0x7C,0x00]
    font[80] = [0xFC,0xC6,0xC6,0xFC,0xC0,0xC0,0xC0,0x00]
    font[81] = [0x7C,0xC6,0xC6,0xC6,0xD6,0xCC,0x76,0x00]
    font[82] = [0xFC,0xC6,0xC6,0xFC,0xD8,0xCC,0xC6,0x00]
    font[83] = [0x7C,0xC6,0xC0,0x7C,0x06,0xC6,0x7C,0x00]
    font[84] = [0xFE,0x18,0x18,0x18,0x18,0x18,0x18,0x00]
    font[85] = [0xC6,0xC6,0xC6,0xC6,0xC6,0xC6,0x7C,0x00]
    font[86] = [0xC6,0xC6,0xC6,0xC6,0x6C,0x38,0x10,0x00]
    font[87] = [0xC6,0xC6,0xC6,0xD6,0xFE,0xEE,0xC6,0x00]
    font[88] = [0xC6,0x6C,0x38,0x38,0x6C,0xC6,0xC6,0x00]  # X (was wrong)
    font[89] = [0xC6,0xC6,0x6C,0x38,0x18,0x18,0x18,0x00]
    font[90] = [0xFE,0x0C,0x18,0x30,0x60,0xC0,0xFE,0x00]

    # [ \ ] ^ _ `
    font[91] = [0x3C,0x30,0x30,0x30,0x30,0x30,0x3C,0x00]
    font[92] = [0xC0,0x60,0x30,0x18,0x0C,0x06,0x00,0x00]
    font[93] = [0x3C,0x0C,0x0C,0x0C,0x0C,0x0C,0x3C,0x00]
    font[94] = [0x10,0x38,0x6C,0xC6,0x00,0x00,0x00,0x00]
    font[95] = [0x00,0x00,0x00,0x00,0x00,0x00,0xFE,0x00]
    font[96] = [0x30,0x18,0x0C,0x00,0x00,0x00,0x00,0x00]

    # Lowercase a-z
    font[97]  = [0x00,0x00,0x7C,0x06,0x7E,0xC6,0x7E,0x00]
    font[98]  = [0xC0,0xC0,0xFC,0xC6,0xC6,0xC6,0xFC,0x00]
    font[99]  = [0x00,0x00,0x7C,0xC0,0xC0,0xC6,0x7C,0x00]
    font[100] = [0x06,0x06,0x7E,0xC6,0xC6,0xC6,0x7E,0x00]
    font[101] = [0x00,0x00,0x7C,0xC6,0xFE,0xC0,0x7C,0x00]
    font[102] = [0x1C,0x30,0x30,0x7C,0x30,0x30,0x30,0x00]
    font[103] = [0x00,0x00,0x7E,0xC6,0xC6,0x7E,0x06,0x7C]
    font[104] = [0xC0,0xC0,0xFC,0xC6,0xC6,0xC6,0xC6,0x00]
    font[105] = [0x18,0x00,0x38,0x18,0x18,0x18,0x3C,0x00]
    font[106] = [0x0C,0x00,0x1C,0x0C,0x0C,0x0C,0xCC,0x78]
    font[107] = [0xC0,0xC0,0xCC,0xD8,0xF0,0xD8,0xCC,0x00]
    font[108] = [0x38,0x18,0x18,0x18,0x18,0x18,0x3C,0x00]
    font[109] = [0x00,0x00,0xEC,0xFE,0xD6,0xC6,0xC6,0x00]
    font[110] = [0x00,0x00,0xFC,0xC6,0xC6,0xC6,0xC6,0x00]
    font[111] = [0x00,0x00,0x7C,0xC6,0xC6,0xC6,0x7C,0x00]
    font[112] = [0x00,0x00,0xFC,0xC6,0xC6,0xFC,0xC0,0xC0]
    font[113] = [0x00,0x00,0x7E,0xC6,0xC6,0x7E,0x06,0x06]
    font[114] = [0x00,0x00,0xDC,0xE0,0xC0,0xC0,0xC0,0x00]
    font[115] = [0x00,0x00,0x7E,0xC0,0x7C,0x06,0xFC,0x00]
    font[116] = [0x30,0x30,0x7C,0x30,0x30,0x30,0x1C,0x00]
    font[117] = [0x00,0x00,0xC6,0xC6,0xC6,0xC6,0x7E,0x00]
    font[118] = [0x00,0x00,0xC6,0xC6,0xC6,0x6C,0x38,0x00]
    font[119] = [0x00,0x00,0xC6,0xC6,0xD6,0xFE,0x6C,0x00]
    font[120] = [0x00,0x00,0xC6,0x6C,0x38,0x6C,0xC6,0x00]
    font[121] = [0x00,0x00,0xC6,0xC6,0xC6,0x7E,0x06,0x7C]
    font[122] = [0x00,0x00,0xFE,0x0C,0x38,0x60,0xFE,0x00]

    # { | } ~ DEL
    font[123] = [0x0E,0x18,0x18,0x70,0x18,0x18,0x0E,0x00]
    font[124] = [0x18,0x18,0x18,0x18,0x18,0x18,0x18,0x00]
    font[125] = [0x70,0x18,0x18,0x0E,0x18,0x18,0x70,0x00]
    font[126] = [0x76,0xDC,0x00,0x00,0x00,0x00,0x00,0x00]
    font[127] = [0x00,0x10,0x38,0x6C,0xC6,0xFE,0x00,0x00]

    # Fill any missing with blank
    for c in range(32, 128):
        if c not in font:
            font[c] = [0x00]*8

    for c in range(32, 128):
        vals = [f"${b:02X}" for b in font[c]]
        lines.append(f"\tdc.b\t{','.join(vals)}\t; {repr(chr(c))}")
    return "\n".join(lines)

# ── Logo bitmap: "DEWDZKI-FXP" rendered into 48×32 bytes ──
def gen_logo():
    WIDTH = 48  # bytes = 384 pixels
    HEIGHT = 32  # lines for logo area
    bitmap = bytearray(WIDTH * HEIGHT)

    # Use the font to render "DEWDZKI-FXP" centered, scaled 3x
    text = "DEWDZKI-FXP"
    # At 3x scale, each char is 24px wide. 11 chars = 264px
    # Center in 384px: offset = (384 - 264) / 2 = 60px
    start_x = 60
    start_y = 4  # a few lines down for vertical centering in 32 lines
    scale = 3

    # Build the same font dict inline
    font = {}
    font[68] = [0xF8,0xCC,0xC6,0xC6,0xC6,0xCC,0xF8,0x00]  # D
    font[69] = [0xFE,0xC0,0xC0,0xFC,0xC0,0xC0,0xFE,0x00]  # E
    font[87] = [0xC6,0xC6,0xC6,0xD6,0xFE,0xEE,0xC6,0x00]  # W
    font[90] = [0xFE,0x0C,0x18,0x30,0x60,0xC0,0xFE,0x00]  # Z
    font[75] = [0xC6,0xCC,0xD8,0xF0,0xD8,0xCC,0xC6,0x00]  # K
    font[73] = [0x7E,0x18,0x18,0x18,0x18,0x18,0x7E,0x00]  # I
    font[45] = [0x00,0x00,0x00,0x7E,0x00,0x00,0x00,0x00]  # -
    font[70] = [0xFE,0xC0,0xC0,0xFC,0xC0,0xC0,0xC0,0x00]  # F
    font[88] = [0xC6,0x6C,0x38,0x38,0x6C,0xC6,0xC6,0x00]  # X
    font[80] = [0xFC,0xC6,0xC6,0xFC,0xC0,0xC0,0xC0,0x00]  # P

    for ci, ch in enumerate(text):
        glyph = font.get(ord(ch), [0]*8)
        for row in range(8):
            for bit in range(8):
                if glyph[row] & (0x80 >> bit):
                    for sy in range(scale):
                        for sx in range(scale):
                            px = start_x + ci * (8 * scale) + bit * scale + sx
                            py = start_y + row * scale + sy
                            if 0 <= px < WIDTH*8 and 0 <= py < HEIGHT:
                                byte_idx = py * WIDTH + (px >> 3)
                                bit_idx = 7 - (px & 7)
                                bitmap[byte_idx] |= (1 << bit_idx)

    lines = [f"; Logo bitmap: {WIDTH}x{HEIGHT} bytes ({WIDTH*8}x{HEIGHT} pixels)"]
    lines.append(f"; Text: \"{text}\" at 3x scale, centered")
    lines.append("logo_data:")
    for row in range(HEIGHT):
        vals = []
        for col in range(0, WIDTH, 2):
            idx = row * WIDTH + col
            vals.append(f"${bitmap[idx]:02X},${bitmap[idx+1]:02X}")
        lines.append(f"\tdc.b\t{','.join(vals)}")
    return "\n".join(lines)

# ── Shape targets (120 × 3 words each) ──
def _emit_targets(label, comment, coords):
    lines = [f"; {comment}"]
    lines.append(f"{label}:")
    for x, y, z in coords:
        lines.append(f"\tdc.w\t${x & 0xFFFF:04X},${y & 0xFFFF:04X},${z & 0xFFFF:04X}")
    return "\n".join(lines)

def gen_tunnel_targets():
    rng = random.Random(42)
    coords = [(rng.randint(-128,127), rng.randint(-128,127), rng.randint(20,240)) for _ in range(120)]
    return _emit_targets("tunnel_targets", "Tunnel targets: 120 x (x,y,z)", coords)

def gen_cube_targets():
    HALF, STEP = 80, 16
    edges = [
        (-HALF,-HALF,-HALF, STEP,0,0), (-HALF,-HALF,HALF, STEP,0,0),
        (-HALF,-HALF,-HALF, 0,0,STEP), (HALF,-HALF,-HALF, 0,0,STEP),
        (-HALF,HALF,-HALF, STEP,0,0),  (-HALF,HALF,HALF, STEP,0,0),
        (-HALF,HALF,-HALF, 0,0,STEP),  (HALF,HALF,-HALF, 0,0,STEP),
        (-HALF,-HALF,-HALF, 0,STEP,0), (HALF,-HALF,-HALF, 0,STEP,0),
        (-HALF,-HALF,HALF, 0,STEP,0),  (HALF,-HALF,HALF, 0,STEP,0),
    ]
    coords = []
    for sx,sy,sz,dx,dy,dz in edges:
        for i in range(10):
            coords.append((sx+dx*i, sy+dy*i, sz+dz*i))
    return _emit_targets("cube_targets", "Cube targets: 120 x (x,y,z)", coords)

def gen_sphere_targets():
    n, r = 120, 80
    golden = (1 + math.sqrt(5)) / 2
    coords = []
    for i in range(n):
        theta = math.acos(1 - 2*(i+0.5)/n)
        phi = 2*math.pi*i/golden
        coords.append((int(round(r*math.sin(theta)*math.cos(phi))),
                       int(round(r*math.cos(theta))),
                       int(round(r*math.sin(theta)*math.sin(phi)))))
    return _emit_targets("sphere_targets", "Sphere targets: 120 x (x,y,z)", coords)

def gen_helix_targets():
    coords = []
    r = 60
    for i in range(120):
        angle = 2 * math.pi * i * 3 / 120  # 3 full turns
        y = int(round(-80 + 160 * i / 119))  # spread along Y axis
        x = int(round(r * math.cos(angle)))
        z = int(round(r * math.sin(angle)))
        coords.append((x, y, z))
    return _emit_targets("helix_targets", "Helix targets: 120 x (x,y,z)", coords)

# ── Bouncing sprite logo: 16x16 "D" glyph as sprite data words ──
def gen_bounce_sprite():
    """Generate a 16x16 'D' sprite glyph as two word-planes (OCS sprite format).
    Outputs 16 rows of (word_a, word_b) for color 3 (both planes set)."""
    # 16x16 bold 'D' glyph — hand-drawn
    rows = [
        0b1111111100000000,
        0b1111111111000000,
        0b1100001111100000,
        0b1100000011110000,
        0b1100000001110000,
        0b1100000001110000,
        0b1100000001110000,
        0b1100000001110000,
        0b1100000001110000,
        0b1100000001110000,
        0b1100000001110000,
        0b1100000011110000,
        0b1100001111100000,
        0b1111111111000000,
        0b1111111100000000,
        0b0000000000000000,
    ]
    lines = ["; Bounce sprite glyph: 16x16 'D', 16 rows of (wordA, wordB)"]
    lines.append("; Both planes set = color 3")
    lines.append("bounce_glyph:")
    for r in rows:
        lines.append(f"\tdc.w\t${r:04X},${r:04X}")
    return "\n".join(lines)

# ── Tunnel color table (256 words) — bright blue/purple ring bands with dark gaps ──
def gen_tunnel_color_table():
    lines = ["; Tunnel color table: 256 words, blue/purple rings with dark gaps"]
    lines.append("tunnel_color_table:")
    colors = []
    for i in range(256):
        t = i / 256.0
        # Create ring bands: bright peaks with dark gaps
        band = math.sin(2 * math.pi * t * 4) ** 2  # 4 bright bands
        if band < 0.3:
            band = 0  # dark gap
        else:
            band = (band - 0.3) / 0.7  # normalize bright part
        # Color: blue/purple gradient
        r = min(15, max(0, int(band * 8 + 0.5)))
        g = min(15, max(0, int(band * 2 + 0.5)))
        b = min(15, max(0, int(band * 15 + 0.5)))
        colors.append((r << 8) | (g << 4) | b)
    for i in range(0, 256, 8):
        vals = [f"${colors[i+j]:04X}" for j in range(8)]
        lines.append(f"\tdc.w\t{','.join(vals)}")
    return "\n".join(lines)

# ── Stripe pattern (48 bytes) — 4px on / 4px off vertical stripes ──
def gen_stripe_pattern():
    lines = ["; Stripe pattern: 48 bytes, 4px on / 4px off vertical stripes"]
    lines.append("stripe_pattern:")
    # 48 bytes = 384 pixels. Pattern: 4 on, 4 off = $F0 repeated
    vals = []
    for i in range(48):
        vals.append("$F0")
    for i in range(0, 48, 16):
        chunk = vals[i:i+16]
        lines.append(f"\tdc.b\t{','.join(chunk)}")
    return "\n".join(lines)

# ── Octahedron vertices (6 × 3 words) ──
def gen_octa_vertices():
    R = 50
    verts = [
        (0, -R, 0),   # top
        (0, R, 0),    # bottom
        (-R, 0, 0),   # left
        (R, 0, 0),    # right
        (0, 0, -R),   # front
        (0, 0, R),    # back
    ]
    lines = ["; Octahedron vertices: 6 x (x.w, y.w, z.w), radius 50"]
    lines.append("octa_vertices:")
    for x, y, z in verts:
        lines.append(f"\tdc.w\t{x},{y},{z}")
    return "\n".join(lines)

# ── Octahedron edges (12 × 2 bytes) ──
def gen_octa_edges():
    # Each non-polar vertex connects to both poles and to its 2 equatorial neighbors
    # Top=0, Bottom=1, Left=2, Right=3, Front=4, Back=5
    edges = [
        (0, 2), (0, 3), (0, 4), (0, 5),  # top to equator
        (1, 2), (1, 3), (1, 4), (1, 5),  # bottom to equator
        (2, 4), (4, 3), (3, 5), (5, 2),  # equatorial ring
    ]
    lines = ["; Octahedron edges: 12 x (v0.b, v1.b)"]
    lines.append("octa_edges:")
    vals = []
    for v0, v1 in edges:
        vals.append(f"${v0:02X},${v1:02X}")
    lines.append(f"\tdc.b\t{','.join(vals)}")
    return "\n".join(lines)

# ── Main ──
def main():
    out = sys.argv[1] if len(sys.argv) > 1 else "tables.i"
    parts = [
        "; Auto-generated by gentables.py - do not edit\n",
        gen_sine_table(),
        gen_signed_sine_table(),
        gen_reciprocal_table(),
        gen_yoffset_table(),
        gen_plasma_table(),
        gen_rainbow_table(),
        gen_rasterbar_tables(),
        gen_font(),
        gen_logo(),
        gen_tunnel_targets(),
        gen_cube_targets(),
        gen_sphere_targets(),
        gen_helix_targets(),
        gen_bounce_sprite(),
        gen_tunnel_color_table(),
        gen_stripe_pattern(),
        gen_octa_vertices(),
        gen_octa_edges(),
    ]
    with open(out, "w") as f:
        f.write("\n\n".join(parts) + "\n")
    print(f"Written {out}")

if __name__ == "__main__":
    main()
