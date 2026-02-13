#!/usr/bin/env python3
"""
Analyze a standard 4-channel Protracker MOD file.
Shows per-channel note triggers, volume commands, effects, and statistics.
"""

import sys
import struct
from collections import defaultdict

# Protracker period table (standard tuning)
PERIOD_TO_NOTE = {}
NOTE_NAMES = ['C-', 'C#', 'D-', 'D#', 'E-', 'F-', 'F#', 'G-', 'G#', 'A-', 'A#', 'B-']
PERIODS = [
    # Octave 1
    856, 808, 762, 720, 678, 640, 604, 570, 538, 508, 480, 453,
    # Octave 2
    428, 404, 381, 360, 339, 320, 302, 285, 269, 254, 240, 226,
    # Octave 3
    214, 202, 190, 180, 170, 160, 151, 143, 135, 127, 120, 113,
]

for i, p in enumerate(PERIODS):
    octave = (i // 12) + 1
    note = NOTE_NAMES[i % 12]
    PERIOD_TO_NOTE[p] = f"{note}{octave}"

EFFECT_NAMES = {
    0x0: "Arpeggio",
    0x1: "Slide Up",
    0x2: "Slide Down",
    0x3: "Tone Porta",
    0x4: "Vibrato",
    0x5: "Tone Porta+Vol Slide",
    0x6: "Vibrato+Vol Slide",
    0x7: "Tremolo",
    0x8: "Set Panning",
    0x9: "Sample Offset",
    0xA: "Volume Slide",
    0xB: "Position Jump",
    0xC: "Set Volume",
    0xD: "Pattern Break",
    0xE: "Extended",
    0xF: "Set Speed/Tempo",
}

EXTENDED_EFFECTS = {
    0x0: "Set Filter",
    0x1: "Fine Slide Up",
    0x2: "Fine Slide Down",
    0x3: "Glissando Ctrl",
    0x4: "Set Vibrato Waveform",
    0x5: "Set Finetune",
    0x6: "Pattern Loop",
    0x7: "Set Tremolo Waveform",
    0x8: "Set Panning (coarse)",
    0x9: "Retrigger Note",
    0xA: "Fine Vol Slide Up",
    0xB: "Fine Vol Slide Down",
    0xC: "Note Cut",
    0xD: "Note Delay",
    0xE: "Pattern Delay",
    0xF: "Invert Loop",
}

def period_to_note(period):
    if period == 0:
        return "..."
    if period in PERIOD_TO_NOTE:
        return PERIOD_TO_NOTE[period]
    # Find closest period
    closest = min(PERIODS, key=lambda p: abs(p - period))
    return PERIOD_TO_NOTE.get(closest, f"?{period:3d}")

def parse_mod(filename):
    with open(filename, 'rb') as f:
        data = f.read()

    # Title (20 bytes)
    title = data[0:20].decode('ascii', errors='replace').rstrip('\x00').strip()
    print(f"=== MOD File: {filename} ===")
    print(f"Title: '{title}'")
    print(f"File size: {len(data)} bytes")
    print()

    # Parse 31 sample headers (30 bytes each, starting at offset 20)
    samples = []
    offset = 20
    for i in range(31):
        name = data[offset:offset+22].decode('ascii', errors='replace').rstrip('\x00').strip()
        length = struct.unpack('>H', data[offset+22:offset+24])[0] * 2
        finetune = data[offset+24] & 0x0F
        if finetune > 7:
            finetune -= 16
        volume = data[offset+25]
        loop_start = struct.unpack('>H', data[offset+26:offset+28])[0] * 2
        loop_length = struct.unpack('>H', data[offset+28:offset+30])[0] * 2
        samples.append({
            'name': name,
            'length': length,
            'finetune': finetune,
            'volume': volume,
            'loop_start': loop_start,
            'loop_length': loop_length,
        })
        offset += 30

    # Print sample info
    print("=== SAMPLES ===")
    print(f"{'#':>2} {'Name':<22} {'Len':>6} {'Vol':>3} {'FT':>3} {'Loop':>6} {'LLen':>6}")
    for i, s in enumerate(samples):
        if s['length'] > 0:
            print(f"{i+1:2d} {s['name']:<22} {s['length']:6d} {s['volume']:3d} {s['finetune']:+3d} {s['loop_start']:6d} {s['loop_length']:6d}")
    print()

    # Song length and order table
    song_length = data[950]
    restart_pos = data[951]
    print(f"Song length: {song_length} patterns")
    print(f"Restart position: {restart_pos}")

    order_table = list(data[952:952+128])
    num_patterns = max(order_table[:song_length]) + 1
    print(f"Number of unique patterns: {num_patterns}")
    print(f"Order table (first {song_length}): {order_table[:song_length]}")
    print()

    # Check for M.K. signature
    sig = data[1080:1084].decode('ascii', errors='replace')
    print(f"Signature: '{sig}'")
    if sig not in ('M.K.', 'M!K!', '4CHN', 'FLT4'):
        print("WARNING: Non-standard signature!")
    print()

    # Parse patterns (starting at offset 1084)
    pattern_offset = 1084
    pattern_size = 64 * 4 * 4  # 64 rows * 4 channels * 4 bytes

    patterns = []
    for pat_num in range(num_patterns):
        pattern = []
        for row in range(64):
            channels = []
            for ch in range(4):
                idx = pattern_offset + pat_num * pattern_size + row * 16 + ch * 4
                b0, b1, b2, b3 = data[idx], data[idx+1], data[idx+2], data[idx+3]

                instrument = (b0 & 0xF0) | ((b2 & 0xF0) >> 4)
                period = ((b0 & 0x0F) << 8) | b1
                effect = b2 & 0x0F
                effect_param = b3

                channels.append({
                    'instrument': instrument,
                    'period': period,
                    'effect': effect,
                    'effect_param': effect_param,
                })
            pattern.append(channels)
        patterns.append(pattern)

    # ===== DETAILED PATTERN DISPLAY (first 3 patterns in song order) =====
    detail_count = min(3, song_length)
    print(f"=== DETAILED PATTERN DATA (first {detail_count} patterns in song order) ===")
    for order_idx in range(detail_count):
        pat_num = order_table[order_idx]
        pat = patterns[pat_num]
        print(f"\n--- Pattern {pat_num} (order position {order_idx}) ---")
        print(f"{'Row':>3} | {'--- Ch1 ---':^18} | {'--- Ch2 ---':^18} | {'--- Ch3 ---':^18} | {'--- Ch4 ---':^18}")
        print(f"    | {'Note Ins Eff':^18} | {'Note Ins Eff':^18} | {'Note Ins Eff':^18} | {'Note Ins Eff':^18}")
        print("-" * 85)
        for row_num, row in enumerate(pat):
            parts = []
            for ch_data in row:
                note_str = period_to_note(ch_data['period'])
                ins = ch_data['instrument']
                eff = ch_data['effect']
                param = ch_data['effect_param']

                ins_str = f"{ins:02d}" if ins > 0 else ".."
                if eff == 0 and param == 0:
                    eff_str = "..."
                elif eff == 0xE:
                    sub = (param >> 4) & 0xF
                    sub_param = param & 0xF
                    eff_str = f"E{sub:X}{sub_param:X}"
                else:
                    eff_str = f"{eff:X}{param:02X}"
                parts.append(f"{note_str} {ins_str} {eff_str}")
            print(f"{row_num:3d} | {parts[0]:^18} | {parts[1]:^18} | {parts[2]:^18} | {parts[3]:^18}")
    print()

    # ===== STATISTICS =====
    print("=" * 80)
    print("=== CHANNEL STATISTICS (across all patterns used in song) ===")
    print("=" * 80)

    # Gather stats per channel
    for ch in range(4):
        note_triggers = 0
        empty_rows = 0
        total_rows = 0
        volume_commands = []
        effects_used = defaultdict(int)
        periods_used = defaultdict(int)
        instruments_used = defaultdict(int)
        volume_affecting = 0  # effects that affect volume

        # Walk through song order
        for order_idx in range(song_length):
            pat_num = order_table[order_idx]
            pat = patterns[pat_num]
            for row in pat:
                ch_data = row[ch]
                total_rows += 1

                has_note = ch_data['period'] > 0
                has_ins = ch_data['instrument'] > 0
                has_effect = ch_data['effect'] != 0 or ch_data['effect_param'] != 0
                eff = ch_data['effect']
                param = ch_data['effect_param']

                if has_note:
                    note_triggers += 1
                    periods_used[ch_data['period']] += 1

                if has_ins:
                    instruments_used[ch_data['instrument']] += 1

                if not has_note and not has_ins and not has_effect:
                    empty_rows += 1

                if has_effect:
                    effects_used[eff] += 1

                # Track volume-related effects
                if eff == 0xC:  # Set Volume
                    volume_commands.append(param)
                    volume_affecting += 1
                elif eff == 0x5:  # Tone Porta + Vol Slide
                    volume_affecting += 1
                elif eff == 0x6:  # Vibrato + Vol Slide
                    volume_affecting += 1
                elif eff == 0x7:  # Tremolo
                    volume_affecting += 1
                elif eff == 0xA:  # Volume Slide
                    volume_affecting += 1
                elif eff == 0xE:
                    sub = (param >> 4) & 0xF
                    if sub == 0xA or sub == 0xB:  # Fine vol slide
                        volume_affecting += 1
                    elif sub == 0xC:  # Note Cut
                        volume_affecting += 1

        print(f"\n--- Channel {ch+1} ---")
        print(f"  Total rows (song): {total_rows}")
        print(f"  Note triggers:     {note_triggers} ({100*note_triggers/total_rows:.1f}%)")
        print(f"  Completely empty:  {empty_rows} ({100*empty_rows/total_rows:.1f}%)")
        print(f"  Volume-affecting:  {volume_affecting} rows ({100*volume_affecting/total_rows:.1f}%)")

        if instruments_used:
            print(f"  Instruments used:  {dict(instruments_used)}")

        if volume_commands:
            print(f"  Volume (Cxx) values: {sorted(set(volume_commands))}")
            print(f"  Volume command count: {len(volume_commands)}")

        if effects_used:
            print(f"  Effects used:")
            for eff_num in sorted(effects_used.keys()):
                name = EFFECT_NAMES.get(eff_num, f"Unknown({eff_num:X})")
                print(f"    {eff_num:X}xx ({name}): {effects_used[eff_num]} times")

        if periods_used:
            notes = [(period_to_note(p), count) for p, count in sorted(periods_used.items())]
            print(f"  Unique periods/notes: {len(periods_used)}")
            print(f"  Notes: {', '.join(f'{n}({c})' for n,c in notes)}")

    # ===== WHAT PTPLAYER EXPOSES PER-FRAME =====
    print()
    print("=" * 80)
    print("=== ANALYSIS: What changes frame-by-frame for EQ bars ===")
    print("=" * 80)
    print()
    print("ptplayer channel structures (n_xxx fields) are updated once per frame (VBlank).")
    print("Key fields for EQ visualization:")
    print("  n_volume  - current channel volume (0-64), set by Cxx or sample default")
    print("  n_period  - current period (pitch), set by note or slide effects")
    print("  n_trigger - NOT a standard field; must detect note-on yourself")
    print()

    # Analyze how often volume/period CHANGES (not just exists)
    print("=== ROW-BY-ROW CHANGE ANALYSIS (first 8 patterns in song order) ===")
    analyze_count = min(8, song_length)
    for ch in range(4):
        changes = 0
        total = 0
        prev_period = 0
        prev_volume = -1
        consecutive_empty = 0
        max_empty_streak = 0

        for order_idx in range(analyze_count):
            pat_num = order_table[order_idx]
            pat = patterns[pat_num]
            for row in pat:
                ch_data = row[ch]
                total += 1

                has_anything = ch_data['period'] > 0 or ch_data['instrument'] > 0 or \
                               ch_data['effect'] != 0 or ch_data['effect_param'] != 0

                if ch_data['period'] > 0 and ch_data['period'] != prev_period:
                    changes += 1
                    prev_period = ch_data['period']
                    consecutive_empty = 0
                elif ch_data['effect'] == 0xC:
                    if ch_data['effect_param'] != prev_volume:
                        changes += 1
                        prev_volume = ch_data['effect_param']
                    consecutive_empty = 0
                elif has_anything:
                    consecutive_empty = 0
                else:
                    consecutive_empty += 1
                    max_empty_streak = max(max_empty_streak, consecutive_empty)

        print(f"  Ch{ch+1}: {changes} note/vol changes in {total} rows "
              f"({100*changes/total:.1f}%), max empty streak: {max_empty_streak} rows")

    # Show speed/tempo info
    print()
    print("=== SPEED/TEMPO ANALYSIS ===")
    for order_idx in range(min(4, song_length)):
        pat_num = order_table[order_idx]
        pat = patterns[pat_num]
        for row_num, row in enumerate(pat):
            for ch_idx, ch_data in enumerate(row):
                if ch_data['effect'] == 0xF:
                    val = ch_data['effect_param']
                    if val <= 31:
                        print(f"  Order {order_idx}, Pattern {pat_num}, Row {row_num}, Ch{ch_idx+1}: Speed = {val}")
                    else:
                        print(f"  Order {order_idx}, Pattern {pat_num}, Row {row_num}, Ch{ch_idx+1}: Tempo = {val} BPM")

    print()
    print("=== FRAME TIMING ===")
    # Find initial speed/tempo
    initial_speed = 6
    initial_tempo = 125
    for order_idx in range(1):
        pat_num = order_table[order_idx]
        pat = patterns[pat_num]
        for row_num, row in enumerate(pat):
            for ch_data in row:
                if ch_data['effect'] == 0xF:
                    val = ch_data['effect_param']
                    if val <= 31 and row_num == 0:
                        initial_speed = val
                    elif val > 31 and row_num == 0:
                        initial_tempo = val
            if row_num == 0:
                break

    frames_per_row = initial_speed
    rows_per_second = initial_tempo * 2 / 5 / initial_speed
    print(f"  Initial speed: {initial_speed} (frames per row)")
    print(f"  Initial tempo: {initial_tempo} BPM")
    print(f"  Frames per row: {frames_per_row}")
    print(f"  Rows per second: {rows_per_second:.1f}")
    print(f"  This means each row lasts {frames_per_row} frames (~{frames_per_row/50*1000:.0f}ms at 50Hz PAL)")
    print(f"  A note trigger causes n_volume to jump to sample default volume,")
    print(f"  then n_volume stays CONSTANT until another Cxx or note trigger.")
    print(f"  Between rows (for {frames_per_row-1} frames), only slide/tremolo effects change values.")
    print()
    print("  ==> For EQ bars, you need to detect NEW note triggers (period > 0 with instrument)")
    print("  ==> and apply a decay envelope yourself, since ptplayer n_volume is mostly static!")

if __name__ == '__main__':
    filename = sys.argv[1] if len(sys.argv) > 1 else 'trance.mod'
    parse_mod(filename)
