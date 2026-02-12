;----------------------------------------------------------
; DEWDZKI-FXP Main Demo
; Loaded at $46000 by bootblock trackloader
; OCS A500 PAL — 1 bitplane, 384px wide
; Effects: copper raster bars, sine-wave logo
;----------------------------------------------------------

; Memory map (chip RAM)
COPPER_A	equ	$40000		; copper list front buffer (6 KB)
COPPER_B	equ	$41800		; copper list back buffer (6 KB)
BPLANE1		equ	$43000		; bitplane 1: logo (48*256 = 12 KB)

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
BPL1PTH		equ	$0E0
BPL1PTL		equ	$0E2
COLOR00		equ	$180
COLOR01		equ	$182
COP1LCH		equ	$080
COP1LCL		equ	$082
COPJMP1		equ	$088

	org	$46000			; loaded here by bootblock

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

	; Draw logo text into bitplane 1
	bsr.w	DrawLogo

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
	; BPLCON0: 1 bitplane, color
	move.l	#(BPLCON0<<16)|$1200,(a0)+
	; BPLCON1: 0 (will be set per-line)
	move.l	#(BPLCON1<<16)|$0000,(a0)+
	; BPLCON2: 0
	move.l	#(BPLCON2<<16)|$0000,(a0)+
	; BPL1MOD: extra fetch = 48 - 42 = 6
	; With DDFSTRT=$0030, DDFSTOP=$00D0: fetches (D0-30)/8+1 = 21 words = 42 bytes
	; We want 48-byte lines, so modulo = 48 - 42 = 6
	move.l	#(BPL1MOD<<16)|6,(a0)+

	; COLOR00: black background
	move.l	#(COLOR00<<16)|$0000,(a0)+

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
	; COLOR01 = gradient[(line + gradient_phase) & 255]
	; Logo pixels cycle through the gradient colors
	move.w	d4,d0
	add.w	d2,d0
	and.w	#$FF,d0
	add.w	d0,d0			; *2 for word index
	move.w	#COLOR01,(a0)+
	move.w	(a3,d0.w),(a0)+

	; Compute sine-based bitplane pointer offset
	; sine_val = sine_table[(line + sine_phase) & 255]
	move.w	d4,d0
	add.w	d3,d0
	and.w	#$FF,d0
	moveq	#0,d6
	move.b	(a2,d0.w),d6		; d6 = sine value 0-63

	; Horizontal scroll: shift display LEFT by sine_val pixels
	; Adding to BPL1PT shifts LEFT (coarse, 16px steps)
	; BPLCON1 shifts RIGHT (fine, 0-15px)
	; To combine: invert fine scroll so both shift the same way
	;   neg_fine = (-sine_val) & 15   (0 when aligned, else 16-fine)
	;   coarse_bytes = ((sine_val + neg_fine) >> 4) * 2
	;   BPLCON1 = neg_fine
	moveq	#0,d7
	sub.w	d6,d7
	and.w	#15,d7			; d7 = neg_fine = BPLCON1 value

	move.w	d6,d0
	add.w	d7,d0			; sine_val + neg_fine (rounds up to next 16)
	lsr.w	#4,d0			; coarse_words
	add.w	d0,d0			; coarse_bytes

	; BPL1PT = BPLANE1 + line * BPWIDTH + coarse_bytes
	move.w	d0,d6			; save coarse_bytes
	move.w	d4,d0
	mulu	#BPWIDTH,d0		; line * 48
	add.l	#BPLANE1,d0
	ext.l	d6
	add.l	d6,d0			; add coarse offset

	; BPL1PTH/PTL
	move.w	#BPL1PTH,(a0)+
	swap	d0
	move.w	d0,(a0)+
	swap	d0
	move.w	#BPL1PTL,(a0)+
	move.w	d0,(a0)+

	; Fine scroll: BPLCON1 bits 0-3 (bitplane 1 only)
	move.w	#BPLCON1,(a0)+
	move.w	d7,(a0)+		; neg_fine

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
; DrawLogo — copy logo.bin bitmap into bitplane 1
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

;==========================================================
; Logo bitmap: 48 bytes/line * 256 lines = 12288 bytes
; 1 bitplane, 384px wide, black & white
;==========================================================
LogoBitmap:
	incbin	"logo.bin"

;==========================================================
; Generated tables (sine + gradient)
;==========================================================
	include	"tables.i"
