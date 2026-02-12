;----------------------------------------------------------
; DEWDZKI-FXP Main Demo
; Loaded at $49000 by bootblock trackloader
; OCS A500 PAL — 2 bitplanes, 384px wide
; Effects: copper raster bars, sine-wave logo, tunnel starfield
;----------------------------------------------------------

; Memory map (chip RAM)
COPPER_A	equ	$40000		; copper list front buffer (6 KB)
COPPER_B	equ	$41800		; copper list back buffer (6 KB)
BPLANE1		equ	$43000		; bitplane 1: logo (48*256 = 12 KB)
BPLANE2		equ	$46000		; bitplane 2: stars (48*256 = 12 KB)

; Star array in free chip RAM before buffers
STAR_ARRAY	equ	$3FB50		; 120*10 = 1200 bytes

; Display parameters
BPWIDTH		equ	48		; bytes per line (384 pixels)
BPHEIGHT	equ	256		; lines
BPSIZE		equ	BPWIDTH*BPHEIGHT

; Star constants
NUM_STARS	equ	120
STAR_SIZE	equ	10		; bytes per star entry
MIN_Z		equ	5
MAX_Z		equ	255
Z_SPEED		equ	2
CENTER_X	equ	192		; screen center X
CENTER_Y	equ	128		; screen center Y
ORBIT_RADIUS	equ	31		; +/-31 pixels orbit

; Star struct offsets
STAR_SX		equ	0		; signed X offset
STAR_SY		equ	2		; signed Y offset
STAR_SZ		equ	4		; Z depth
STAR_OPX	equ	6		; old screen X ($FFFF=not drawn)
STAR_OPY	equ	8		; old screen Y

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

	org	$49000			; loaded here by bootblock

;==========================================================
; Entry point
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

	; Draw logo bitmap into bitplane 1
	bsr.w	DrawLogo

	; Init stars
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
	clr.w	orbit_phase

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

	; Update stars (before BuildCopper for timing)
	bsr.w	UpdateStars

	; Build next frame into back buffer
	move.l	cop_back(pc),a0
	bsr.w	BuildCopper

	; Advance animation phases
	addq.w	#1,gradient_phase
	and.w	#$FF,gradient_phase
	addq.w	#2,sine_phase
	and.w	#$FF,sine_phase
	addq.w	#2,orbit_phase
	and.w	#$FF,orbit_phase

	bra.s	MainLoop

;==========================================================
; Variables
;==========================================================
cop_front:	dc.l	COPPER_A
cop_back:	dc.l	COPPER_B
gradient_phase:	dc.w	0
sine_phase:	dc.w	0
orbit_phase:	dc.w	0
lfsr_state:	dc.w	$ACE1

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
; LFSRNext — 16-bit Galois LFSR, random result in d2
; Taps: $B400 (bits 15,13,12,10) — maximal period
; Preserves all regs except d2
;==========================================================
LFSRNext:
	move.w	lfsr_state(pc),d2
	lsr.w	#1,d2
	bcc.s	.noTap
	eor.w	#$B400,d2
.noTap:
	move.w	d2,lfsr_state
	rts

;==========================================================
; InitStars — populate STAR_ARRAY with NUM_STARS entries
;==========================================================
InitStars:
	movem.l	d2-d4/a2,-(sp)
	lea	STAR_ARRAY,a2
	move.w	#NUM_STARS-1,d4

.initLoop:
	; Random sx (-128..+127)
	bsr.w	LFSRNext
	move.b	d2,d0
	ext.w	d0
	move.w	d0,STAR_SX(a2)

	; Random sy (-128..+127)
	bsr.w	LFSRNext
	move.b	d2,d0
	ext.w	d0
	move.w	d0,STAR_SY(a2)

	; Random sz spread across MIN_Z..MAX_Z
	bsr.w	LFSRNext
	move.w	d2,d0
	and.w	#$FF,d0
	mulu	#MAX_Z-MIN_Z,d0
	lsr.l	#8,d0
	add.w	#MIN_Z,d0
	move.w	d0,STAR_SZ(a2)

	; Not drawn yet
	move.w	#$FFFF,STAR_OPX(a2)
	clr.w	STAR_OPY(a2)

	lea	STAR_SIZE(a2),a2
	dbf	d4,.initLoop

	movem.l	(sp)+,d2-d4/a2
	rts

;==========================================================
; UpdateStars — per-star erase, advance, project, plot
;
; Register usage inside the star loop:
;   a0 = BPLANE2 base
;   a2 = current star pointer (walks forward)
;   a3 = recip_table
;   a4 = yoffset_table
;   a5 = signed_sine_table
;   d5 = tunnel center X
;   d6 = tunnel center Y
;   d7 = loop counter
;   d0-d4 = scratch
;==========================================================
UpdateStars:
	movem.l	d2-d7/a2-a5,-(sp)

	lea	BPLANE2,a0
	lea	recip_table(pc),a3
	lea	yoffset_table(pc),a4
	lea	STAR_ARRAY,a2

	; Compute tunnel center (circular orbit)
	lea	signed_sine_table(pc),a5
	move.w	orbit_phase(pc),d0
	add.w	d0,d0			; word index
	move.w	(a5,d0.w),d5		; sine(-127..+127)
	muls	#ORBIT_RADIUS,d5
	asr.l	#7,d5
	add.w	#CENTER_X,d5		; d5 = center X

	move.w	orbit_phase(pc),d0
	add.w	#64,d0			; +90 degrees for cosine
	and.w	#$FF,d0
	add.w	d0,d0
	move.w	(a5,d0.w),d6
	muls	#ORBIT_RADIUS,d6
	asr.l	#7,d6
	add.w	#CENTER_Y,d6		; d6 = center Y

	move.w	#NUM_STARS-1,d7

.starLoop:
	; ---- 1. Erase old pixel (if drawn) ----
	move.w	STAR_OPX(a2),d0
	cmp.w	#$FFFF,d0
	beq.s	.skipErase

	move.w	STAR_OPY(a2),d1
	add.w	d1,d1			; word index
	move.w	0(a4,d1.w),d4		; d4 = old_py * 48
	move.w	d0,d2
	lsr.w	#3,d2			; byte in line
	add.w	d2,d4			; d4 = byte offset in plane
	move.w	d0,d2
	not.w	d2
	and.w	#7,d2			; bit number (7-(px&7))
	bclr	d2,0(a0,d4.w)

.skipErase:
	; ---- 2. Advance Z ----
	move.w	STAR_SZ(a2),d0
	sub.w	#Z_SPEED,d0
	cmp.w	#MIN_Z,d0
	ble.s	.respawn
	move.w	d0,STAR_SZ(a2)
	bra.s	.project

.respawn:
	bsr.w	LFSRNext
	move.b	d2,d0
	ext.w	d0
	move.w	d0,STAR_SX(a2)
	bsr.w	LFSRNext
	move.b	d2,d0
	ext.w	d0
	move.w	d0,STAR_SY(a2)
	move.w	#MAX_Z,STAR_SZ(a2)
	move.w	#$FFFF,STAR_OPX(a2)
	bra.w	.nextStar

.project:
	; ---- 3. Perspective projection ----
	move.w	STAR_SZ(a2),d0
	add.w	d0,d0			; word index into recip table
	move.w	0(a3,d0.w),d1		; d1 = recip[sz]

	; screen_x = (sx * recip[sz]) >> 8 + centerX
	move.w	d1,d3			; save recip for Y axis
	move.w	STAR_SX(a2),d0
	muls	d1,d0
	asr.l	#8,d0
	add.w	d5,d0			; d0 = screen X

	; screen_y = (sy * recip[sz]) >> 8 + centerY
	move.w	STAR_SY(a2),d1
	muls	d3,d1
	asr.l	#8,d1
	add.w	d6,d1			; d1 = screen Y

	; ---- 4. Bounds check ----
	tst.w	d0
	blt.s	.oob
	cmp.w	#383,d0
	bgt.s	.oob
	tst.w	d1
	blt.s	.oob
	cmp.w	#255,d1
	bgt.s	.oob
	bra.s	.doPlot

.oob:
	; Out of bounds — respawn at far Z
	bsr.w	LFSRNext
	move.b	d2,d0
	ext.w	d0
	move.w	d0,STAR_SX(a2)
	bsr.w	LFSRNext
	move.b	d2,d0
	ext.w	d0
	move.w	d0,STAR_SY(a2)
	move.w	#MAX_Z,STAR_SZ(a2)
	move.w	#$FFFF,STAR_OPX(a2)
	bra.s	.nextStar

.doPlot:
	; ---- 5. Plot new pixel ----
	move.w	d1,d4
	add.w	d4,d4			; word index
	move.w	0(a4,d4.w),d4		; d4 = screenY * 48
	move.w	d0,d2
	lsr.w	#3,d2			; byte in line
	add.w	d2,d4			; d4 = byte offset
	move.w	d0,d2
	not.w	d2
	and.w	#7,d2			; bit number
	bset	d2,0(a0,d4.w)

	; ---- 6. Store new old position ----
	move.w	d0,STAR_OPX(a2)
	move.w	d1,STAR_OPY(a2)

.nextStar:
	lea	STAR_SIZE(a2),a2
	dbf	d7,.starLoop

	movem.l	(sp)+,d2-d7/a2-a5
	rts

;==========================================================
; BuildCopper — build copper list at (a0)
;==========================================================
BuildCopper:
	movem.l	d2-d7/a2-a4,-(sp)

	; --- Header ---
	move.l	#(DIWSTRT<<16)|$2C81,(a0)+
	move.l	#(DIWSTOP<<16)|$2CC1,(a0)+
	move.l	#(DDFSTRT<<16)|$0030,(a0)+
	move.l	#(DDFSTOP<<16)|$00D0,(a0)+
	; BPLCON0: 2 bitplanes, color on
	move.l	#(BPLCON0<<16)|$2200,(a0)+
	move.l	#(BPLCON1<<16)|$0000,(a0)+
	move.l	#(BPLCON2<<16)|$0000,(a0)+
	; Modulos: 48 - 42 = 6 (21 words fetched, we want 48-byte lines)
	move.l	#(BPL1MOD<<16)|6,(a0)+
	move.l	#(BPL2MOD<<16)|6,(a0)+

	; BPL2 pointer (stars — static, set once)
	move.l	#(BPL2PTH<<16)|(BPLANE2>>16),(a0)+
	move.l	#(BPL2PTL<<16)|(BPLANE2&$FFFF),(a0)+

	; Colors
	move.l	#(COLOR00<<16)|$0000,(a0)+	; background = black
	move.l	#(COLOR02<<16)|$06FF,(a0)+	; stars only = cyan
	move.l	#(COLOR03<<16)|$0FFF,(a0)+	; stars+logo = white

	; --- Per-scanline ---
	lea	sine_table(pc),a2
	lea	gradient_table(pc),a3
	move.w	gradient_phase(pc),d2
	move.w	sine_phase(pc),d3

	moveq	#0,d4			; line counter
	move.w	#$2C,d5			; starting VPOS
	moveq	#0,d1			; V8 barrier flag
	move.l	#BPLANE1,d7		; running BPL1 address

.perLine:
	; V8 barrier crossing
	tst.w	d1
	bne.s	.pastBarrier
	cmp.w	#$100,d5
	blt.s	.noBarrier
	move.l	#$FFDFFFFE,(a0)+
	moveq	#1,d1
.noBarrier:
.pastBarrier:
	; WAIT this line
	move.w	d5,d0
	and.w	#$FF,d0
	lsl.w	#8,d0
	or.w	#$07,d0
	move.w	d0,(a0)+
	move.w	#$FFFE,(a0)+

	; COLOR01 = gradient[(line + phase) & 255]
	move.w	d4,d0
	add.w	d2,d0
	and.w	#$FF,d0
	add.w	d0,d0
	move.w	#COLOR01,(a0)+
	move.w	0(a3,d0.w),(a0)+

	; Sine scroll: compute fine/coarse from sine table
	; d3 = sine_phase (will be temporarily trashed, reloaded at end)
	move.w	d4,d0
	add.w	d3,d0
	and.w	#$FF,d0
	moveq	#0,d6
	move.b	0(a2,d0.w),d6		; d6 = sine value 0-63

	; Decompose into fine scroll + coarse byte offset
	move.w	d6,d0			; d0 = sine_val copy
	neg.w	d0
	and.w	#15,d0			; d0 = neg_fine = BPLCON1 value
	add.w	d0,d6			; round up to next multiple of 16
	lsr.w	#4,d6			; coarse words
	add.w	d6,d6			; coarse bytes
	ext.l	d6

	; BPL1PT = running base + coarse offset
	move.l	d7,d3			; d3 = copy of running BPL1 base
	add.l	d6,d3			; d3 = final BPL1PT

	move.w	#BPL1PTH,(a0)+
	swap	d3
	move.w	d3,(a0)+
	move.w	#BPL1PTL,(a0)+
	swap	d3
	move.w	d3,(a0)+

	; Fine scroll
	move.w	#BPLCON1,(a0)+
	move.w	d0,(a0)+

	; Restore d3 = sine_phase (was trashed above)
	move.w	sine_phase(pc),d3

	; Advance running BPL1 pointer
	add.l	#BPWIDTH,d7

	addq.w	#1,d4
	addq.w	#1,d5
	cmp.w	#BPHEIGHT,d4
	blt.w	.perLine

	; End
	move.l	#$FFFFFFFE,(a0)+

	movem.l	(sp)+,d2-d7/a2-a4
	rts

;==========================================================
; DrawLogo
;==========================================================
DrawLogo:
	lea	LogoBitmap(pc),a0
	lea	BPLANE1,a1
	move.w	#(BPSIZE/4)-1,d0
.copy:	move.l	(a0)+,(a1)+
	dbf	d0,.copy
	rts

;==========================================================
; Data
;==========================================================
LogoBitmap:
	incbin	"logo.bin"

	include	"tables.i"
