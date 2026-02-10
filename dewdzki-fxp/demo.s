;----------------------------------------------------------
; DEWDZKI-FXP Main Demo
; Loaded at $49000 by bootblock trackloader
; OCS A500 PAL — 2 bitplanes, 384px wide
; Effects: copper raster bars, sine-wave logo, starfield
;----------------------------------------------------------

; Memory map (chip RAM)
COPPER_A	equ	$40000		; copper list front buffer (6 KB)
COPPER_B	equ	$41800		; copper list back buffer (6 KB)
BPLANE1		equ	$43000		; bitplane 1: logo (48*256 = 12 KB)
BPLANE2		equ	$46000		; bitplane 2: stars (48*256 = 12 KB)

; Display parameters
BPWIDTH		equ	48		; bytes per line (384 pixels)
BPHEIGHT	equ	256		; lines
BPSIZE		equ	BPWIDTH*BPHEIGHT

; Custom chip registers
CUSTOM		equ	$DFF000
VPOSR		equ	$004
VHPOSR		equ	$006
DMACON		equ	$096
INTENA		equ	$09A
INTREQ		equ	$09C
DIWSTRT		equ	$08E
DIWSTOP		equ	$090
DDFSTRT		equ	$092
DDFSTOP		equ	$094
BPLCON0		equ	$100
BPLCON1		equ	$102
BPLCON2		equ	$104
BPL1MOD		equ	$108
BPL2MOD		equ	$10A
BPL1PTH		equ	$0E0
BPL1PTL		equ	$0E2
BPL2PTH		equ	$0E4
BPL2PTL		equ	$0E6
COLOR00		equ	$180
COLOR01		equ	$182
COLOR02		equ	$184
COLOR03		equ	$186
COP1LCH		equ	$080
COP1LCL		equ	$082
COPJMP1		equ	$088

; Star parameters
NUM_STARS	equ	80
STAR_SIZE	equ	6		; x(word) + y(word) + speed(word)

	org	$49000			; loaded here by bootblock

;==========================================================
; Entry point — jumped to from bootblock
; OS is already killed, interrupts/DMA off
;==========================================================
Start:
	lea	CUSTOM,a6

	; Clear bitplane 1 (logo)
	lea	BPLANE1,a0
	move.w	#(BPSIZE/4)-1,d0
.clr1:	clr.l	(a0)+
	dbf	d0,.clr1

	; Clear bitplane 2 (stars)
	lea	BPLANE2,a0
	move.w	#(BPSIZE/4)-1,d0
.clr2:	clr.l	(a0)+
	dbf	d0,.clr2

	; Draw logo text into bitplane 1
	bsr.w	DrawLogo

	; Init star array with LFSR
	bsr.w	InitStars

	; Build initial copper list in buffer A
	lea	COPPER_A,a0
	bsr.w	BuildCopper

	; Install copper list A
	move.l	#COPPER_A,COP1LCH(a6)
	move.w	COPJMP1(a6),d0

	; Enable DMA: SET + DMAEN + BPLEN + COPEN
	move.w	#$83A0,DMACON(a6)

	; Init phase counters
	clr.w	gradient_phase
	clr.w	sine_phase

	; Init copper buffer pointers
	move.l	#COPPER_A,cop_front
	move.l	#COPPER_B,cop_back

;==========================================================
; Main loop
;==========================================================
MainLoop:
	bsr.w	WaitVBL

	; Swap copper buffers
	move.l	cop_front(pc),d0
	move.l	cop_back(pc),d1
	move.l	d1,cop_front
	move.l	d0,cop_back

	; Install new front buffer
	move.l	cop_front(pc),COP1LCH(a6)
	move.w	COPJMP1(a6),d0

	; Build next frame into back buffer
	move.l	cop_back(pc),a0
	bsr.w	BuildCopper

	; Update starfield
	bsr.w	UpdateStars

	; Advance animation phases
	addq.w	#1,gradient_phase
	and.w	#$FF,gradient_phase
	addq.w	#2,sine_phase
	and.w	#$FF,sine_phase

	bra.s	MainLoop

;==========================================================
; Variables (in code section, PC-relative accessible)
;==========================================================
cop_front:	dc.l	COPPER_A
cop_back:	dc.l	COPPER_B
gradient_phase:	dc.w	0
sine_phase:	dc.w	0

;==========================================================
; WaitVBL — wait for line 300, then wait for it to pass
;==========================================================
WaitVBL:
.wv1:	move.l	VPOSR(a6),d0
	and.l	#$1FF00,d0
	cmp.l	#300<<8,d0
	bne.s	.wv1
.wv2:	move.l	VPOSR(a6),d0
	and.l	#$1FF00,d0
	cmp.l	#300<<8,d0
	beq.s	.wv2
	rts

;==========================================================
; BuildCopper — build copper list at (a0)
; Uses gradient_phase and sine_phase for animation
;==========================================================
BuildCopper:
	movem.l	d2-d7/a2-a4,-(sp)

	; --- Header: display setup ---
	; DIWSTRT $2C81 (standard PAL)
	move.l	#(DIWSTRT<<16)|$2C81,(a0)+
	; DIWSTOP $2CC1
	move.l	#(DIWSTOP<<16)|$2CC1,(a0)+
	; DDFSTRT $0030 (one word earlier for scroll margin)
	move.l	#(DDFSTRT<<16)|$0030,(a0)+
	; DDFSTOP $00D0
	move.l	#(DDFSTOP<<16)|$00D0,(a0)+
	; BPLCON0: 2 bitplanes, color
	move.l	#(BPLCON0<<16)|$2200,(a0)+
	; BPLCON1: 0 (will be set per-line)
	move.l	#(BPLCON1<<16)|$0000,(a0)+
	; BPLCON2: 0
	move.l	#(BPLCON2<<16)|$0000,(a0)+
	; BPL1MOD: extra fetch = 48 - 42 = 6 (384px fetch is 24 words = 48 bytes,
	;   but display window fetches ~21 words; modulo compensates)
	; With DDFSTRT=$0030, DDFSTOP=$00D0: fetches (D0-30)/8+1 = 21 words = 42 bytes
	; We want 48-byte lines, so modulo = 48 - 42 = 6
	move.l	#(BPL1MOD<<16)|6,(a0)+
	; BPL2MOD: same
	move.l	#(BPL2MOD<<16)|6,(a0)+

	; BPL2PTH/PTL — stars bitplane (constant, not per-line)
	move.l	#BPLANE2,d0
	move.w	#BPL2PTH,(a0)+
	swap	d0
	move.w	d0,(a0)+
	swap	d0
	move.w	#BPL2PTL,(a0)+
	move.w	d0,(a0)+

	; COLOR01: white (logo text)
	move.l	#(COLOR01<<16)|$0FFF,(a0)+
	; COLOR02: light grey (stars)
	move.l	#(COLOR02<<16)|$0999,(a0)+
	; COLOR03: bright white (overlap)
	move.l	#(COLOR03<<16)|$0FFF,(a0)+

	; --- Per-scanline copper instructions ---
	; For each of 256 lines:
	;   WAIT(line, $07)
	;   COLOR00 = gradient[line + phase]
	;   BPL1PTH = hi word of bplane1 + sine_offset
	;   BPL1PTL = lo word of bplane1 + sine_offset
	;   BPLCON1 = fine scroll

	lea	sine_table(pc),a2
	lea	gradient_table(pc),a3
	move.w	gradient_phase(pc),d2	; gradient phase
	move.w	sine_phase(pc),d3	; sine phase

	moveq	#0,d4			; line counter
	move.w	#$2C,d5			; starting VPOS (first visible line = $2C)
	moveq	#0,d1			; V8 barrier flag (0 = not crossed yet)

.perLine:
	; Check if we need to cross the V8 barrier ($100 line)
	tst.w	d1
	bne.s	.pastBarrier
	cmp.w	#$100,d5
	blt.s	.noBarrier
	; Insert the V8 barrier crossing WAIT
	move.l	#$FFDFFFFE,(a0)+	; wait for end of line $FF
	moveq	#1,d1			; mark barrier crossed
.noBarrier:
.pastBarrier:
	; --- WAIT for this line ---
	move.w	d5,d0
	and.w	#$FF,d0			; low 8 bits of VPOS
	lsl.w	#8,d0
	or.w	#$07,d0			; H position = $07 (early in line)
	move.w	d0,(a0)+
	move.w	#$FFFE,(a0)+		; WAIT mask

.doColor:
	; COLOR00 = gradient[(line + gradient_phase) & 255]
	move.w	d4,d0
	add.w	d2,d0
	and.w	#$FF,d0
	add.w	d0,d0			; *2 for word index
	move.w	#COLOR00,(a0)+
	move.w	(a3,d0.w),(a0)+

	; Compute sine-based bitplane pointer offset
	; sine_val = sine_table[(line + sine_phase) & 255]
	move.w	d4,d0
	add.w	d3,d0
	and.w	#$FF,d0
	moveq	#0,d6
	move.b	(a2,d0.w),d6		; d6 = sine value 0-63

	; Convert sine value to pixel offset:
	; sine_val is 0-63, center=32. Pixel offset = sine_val - 32 + center_offset
	; The logo is drawn starting at byte offset 14 in the 48-byte line
	; We want it roughly centered. BPL pointer offset in bytes = sine_val / 2
	; (each byte = 8 pixels, so /2 gives word granularity = 16-pixel steps for coarse)
	; Actually: coarse = sine_val >> 4 bytes offset (0-3), fine = sine_val & 15

	; Coarse scroll: byte offset added to base bitplane pointer
	; Each line: base = BPLANE1 + line * BPWIDTH + coarse_byte_offset
	move.w	d6,d7
	lsr.w	#4,d7			; coarse: 0-3 (in units of 16 pixels)
	add.w	d7,d7			; *2 for byte pairs (word-aligned for 16px)

	move.w	d4,d0
	mulu	#BPWIDTH,d0		; line * 48
	add.l	#BPLANE1,d0
	ext.l	d7
	add.l	d7,d0			; add coarse offset

	; BPL1PTH/PTL
	move.w	#BPL1PTH,(a0)+
	swap	d0
	move.w	d0,(a0)+
	swap	d0
	move.w	#BPL1PTL,(a0)+
	move.w	d0,(a0)+

	; Fine scroll: BPLCON1 bits 0-3 = fine scroll for bitplane 1
	move.w	d6,d0
	and.w	#$0F,d0			; fine: 0-15 pixels
	move.w	#BPLCON1,(a0)+
	move.w	d0,(a0)+

	; Next line
	addq.w	#1,d4
	addq.w	#1,d5
	cmp.w	#BPHEIGHT,d4
	blt.w	.perLine

	; --- End copper list ---
	move.l	#$FFFFFFFE,(a0)+

	movem.l	(sp)+,d2-d7/a2-a4
	rts

;==========================================================
; DrawLogo — render "DEWDZKI-FXP" into bitplane 1 (48-byte wide)
; Centered vertically at line 124, horizontally centered
;==========================================================
DrawLogo:
	lea	TextString(pc),a0
	lea	Font8x8(pc),a1
	lea	BPLANE1,a2

	; Center: 11 chars * 8px = 88px
	; In 384px wide plane: (384-88)/2 = 148 px -> byte 18, bit 4
	; Simpler: byte 18 (pixel 144, close enough to center)
	; Y: line 124
	; Offset = 124 * 48 + 18 = 5970
	add.w	#124*BPWIDTH+18,a2

.nextChar:
	moveq	#0,d0
	move.b	(a0)+,d0
	beq.s	.done

	; Font index: (char - 32) * 8
	sub.b	#32,d0
	lsl.w	#3,d0
	lea	(a1,d0.w),a3

	; Draw 8 rows
	move.l	a2,a4
	moveq	#7,d1
.drawRow:
	move.b	(a3)+,(a4)
	lea	BPWIDTH(a4),a4		; next line (48 bytes)
	dbf	d1,.drawRow

	addq.l	#1,a2			; next char column
	bra.s	.nextChar
.done:
	rts

;==========================================================
; InitStars — initialize 80 stars with LFSR random positions
;==========================================================
InitStars:
	lea	star_array(pc),a0
	move.w	#$ACE1,d0		; LFSR seed
	moveq	#NUM_STARS-1,d1

.initStar:
	; Generate random X (0-383)
	bsr.s	.lfsr_step
	and.w	#$01FF,d0		; 0-511
	cmp.w	#384,d0
	blt.s	.xok
	sub.w	#384,d0
.xok:	move.w	d0,(a0)+		; x

	; Generate random Y (0-255)
	bsr.s	.lfsr_step
	and.w	#$FF,d0
	move.w	d0,(a0)+		; y

	; Speed based on star index: 0-29=1, 30-59=2, 60-79=3
	moveq	#NUM_STARS-1,d2
	sub.w	d1,d2			; d2 = star index (0-based)
	cmp.w	#30,d2
	blt.s	.speed1
	cmp.w	#60,d2
	blt.s	.speed2
	move.w	#3,(a0)+
	bra.s	.speedDone
.speed2:
	move.w	#2,(a0)+
	bra.s	.speedDone
.speed1:
	move.w	#1,(a0)+
.speedDone:
	dbf	d1,.initStar
	move.w	d0,lfsr_state		; save LFSR state
	rts

	; 16-bit Galois LFSR step (taps: 16,14,13,11 = $B400)
.lfsr_step:
	move.w	d0,d2
	and.w	#1,d2
	beq.s	.no_tap
	eor.w	#$B400,d0
.no_tap:
	lsr.w	#1,d0
	or.w	d2,d0			; rotate feedback bit if needed
	rts

lfsr_state:	dc.w	$ACE1

;==========================================================
; UpdateStars — erase old, move, draw new
;==========================================================
UpdateStars:
	movem.l	d2-d5/a2,-(sp)
	lea	star_array(pc),a2
	moveq	#NUM_STARS-1,d4

.starLoop:
	move.w	(a2),d0			; x
	move.w	2(a2),d1		; y
	move.w	4(a2),d2		; speed

	; Erase old pixel
	bsr.s	EraseStar

	; Move star left
	sub.w	d2,d0
	bpl.s	.noWrap
	add.w	#384,d0			; wrap around
.noWrap:
	move.w	d0,(a2)			; store new x

	; Draw new pixel
	move.w	(a2),d0
	move.w	2(a2),d1
	bsr.s	DrawStar

	lea	STAR_SIZE(a2),a2
	dbf	d4,.starLoop

	movem.l	(sp)+,d2-d5/a2
	rts

;----------------------------------------------------------
; EraseStar — clear pixel at (d0=x, d1=y) in bitplane 2
;----------------------------------------------------------
EraseStar:
	movem.l	d0-d1/a0,-(sp)
	lea	BPLANE2,a0

	; byte offset = y * 48 + x / 8
	mulu	#BPWIDTH,d1
	move.w	d0,d3
	lsr.w	#3,d3
	ext.l	d3
	add.l	d1,d3
	add.l	d3,a0

	; bit number = 7 - (x & 7)
	not.w	d0
	and.w	#7,d0
	bclr	d0,(a0)

	movem.l	(sp)+,d0-d1/a0
	rts

;----------------------------------------------------------
; DrawStar — set pixel at (d0=x, d1=y) in bitplane 2
;----------------------------------------------------------
DrawStar:
	movem.l	d0-d1/a0,-(sp)
	lea	BPLANE2,a0

	mulu	#BPWIDTH,d1
	move.w	d0,d3
	lsr.w	#3,d3
	ext.l	d3
	add.l	d1,d3
	add.l	d3,a0

	not.w	d0
	and.w	#7,d0
	bset	d0,(a0)

	movem.l	(sp)+,d0-d1/a0
	rts

;==========================================================
; Data
;==========================================================
TextString:
	dc.b	"DEWDZKI-FXP",0
	even

;==========================================================
; Star array: 80 * 6 bytes = 480 bytes
;==========================================================
star_array:
	dcb.b	NUM_STARS*STAR_SIZE,0

;==========================================================
; 8x8 bitmap font: ASCII 32 (space) through ASCII 90 (Z)
;==========================================================
Font8x8:
	; ASCII 32: SPACE
	dc.b	$00,$00,$00,$00,$00,$00,$00,$00
	; ASCII 33-44: unused
	dcb.b	8*12,0
	; ASCII 45: - (hyphen)
	dc.b	$00,$00,$00,$7E,$00,$00,$00,$00
	; ASCII 46-64: unused
	dcb.b	8*19,0
	; ASCII 65: A
	dc.b	$18,$3C,$66,$66,$7E,$66,$66,$00
	; ASCII 66: B
	dc.b	$7C,$66,$66,$7C,$66,$66,$7C,$00
	; ASCII 67: C
	dc.b	$3C,$66,$60,$60,$60,$66,$3C,$00
	; ASCII 68: D
	dc.b	$7C,$66,$66,$66,$66,$66,$7C,$00
	; ASCII 69: E
	dc.b	$7E,$60,$60,$7C,$60,$60,$7E,$00
	; ASCII 70: F
	dc.b	$7E,$60,$60,$7C,$60,$60,$60,$00
	; ASCII 71: G
	dc.b	$3C,$66,$60,$6E,$66,$66,$3E,$00
	; ASCII 72: H
	dc.b	$66,$66,$66,$7E,$66,$66,$66,$00
	; ASCII 73: I
	dc.b	$3C,$18,$18,$18,$18,$18,$3C,$00
	; ASCII 74: J
	dc.b	$0E,$06,$06,$06,$06,$66,$3C,$00
	; ASCII 75: K
	dc.b	$66,$6C,$78,$70,$78,$6C,$66,$00
	; ASCII 76: L
	dc.b	$60,$60,$60,$60,$60,$60,$7E,$00
	; ASCII 77: M
	dc.b	$C6,$EE,$FE,$D6,$C6,$C6,$C6,$00
	; ASCII 78: N
	dc.b	$C6,$E6,$F6,$DE,$CE,$C6,$C6,$00
	; ASCII 79: O
	dc.b	$3C,$66,$66,$66,$66,$66,$3C,$00
	; ASCII 80: P
	dc.b	$7C,$66,$66,$7C,$60,$60,$60,$00
	; ASCII 81: Q
	dc.b	$3C,$66,$66,$66,$6A,$6C,$36,$00
	; ASCII 82: R
	dc.b	$7C,$66,$66,$7C,$6C,$66,$66,$00
	; ASCII 83: S
	dc.b	$3C,$66,$60,$3C,$06,$66,$3C,$00
	; ASCII 84: T
	dc.b	$7E,$18,$18,$18,$18,$18,$18,$00
	; ASCII 85: U
	dc.b	$66,$66,$66,$66,$66,$66,$3C,$00
	; ASCII 86: V
	dc.b	$66,$66,$66,$66,$66,$3C,$18,$00
	; ASCII 87: W
	dc.b	$C6,$C6,$C6,$D6,$D6,$FE,$6C,$00
	; ASCII 88: X
	dc.b	$66,$66,$3C,$18,$3C,$66,$66,$00
	; ASCII 89: Y
	dc.b	$66,$66,$66,$3C,$18,$18,$18,$00
	; ASCII 90: Z
	dc.b	$7E,$06,$0C,$18,$30,$60,$7E,$00

;==========================================================
; Generated tables (sine + gradient)
;==========================================================
	include	"tables.i"
