;----------------------------------------------------------
; DEWDZKI-FXP Part 2 — Main Demo
; Loaded at $49000 by bootblock trackloader
; OCS A500 PAL — 2 bitplanes, 384px wide
; 7 effects: copper plasma, rainbow logo, 3D starfield,
;            bouncing raster bars, sine scrolltext,
;            sprite EQ bars, beat flash
;----------------------------------------------------------

; Memory map (chip RAM)
COPPER_A	equ	$38000		; copper list front buffer (6 KB)
COPPER_B	equ	$39800		; copper list back buffer (6 KB)
SPR0_DATA	equ	$3B000		; sprite 0 data (272 bytes)
SPR2_DATA	equ	$3B110		; sprite 2 data
SPR4_DATA	equ	$3B220		; sprite 4 data
SPR6_DATA	equ	$3B330		; sprite 6 data
NULL_SPR	equ	$3B440		; 4 bytes null sprite
STAR_ARRAY	equ	$3B450		; 120*10 = 1200 bytes
BPLANE1		equ	$40000		; bitplane 1: logo + scroll (48*256 = 12 KB)
BPLANE2		equ	$43000		; bitplane 2: stars (48*256 = 12 KB)

; Display parameters
BPWIDTH		equ	48		; bytes per line (384 pixels)
BPHEIGHT	equ	256
BPSIZE		equ	BPWIDTH*BPHEIGHT

; Display register values
DIWSTRT_VAL	equ	$2C81
DIWSTOP_VAL	equ	$2CC1
DDFSTRT_VAL	equ	$0030
DDFSTOP_VAL	equ	$00D0
BPLCON0_VAL	equ	$2200		; 2 bitplanes, color on
BPL_MODULO	equ	BPWIDTH-42	; 48 - 21 words fetched = 6

; Star constants
NUM_STARS	equ	120
STAR_SIZE	equ	10
MIN_Z		equ	5
MAX_Z		equ	255
MIN_ZSPEED	equ	1
CENTER_X	equ	192
CENTER_Y	equ	128
ORBIT_RADIUS	equ	31

; Star struct offsets
STAR_SX		equ	0
STAR_SY		equ	2
STAR_SZ		equ	4
STAR_OPX	equ	6
STAR_OPY	equ	8

; Shape cycling
NUM_SHAPES	equ	4
SHAPE_DISPLAY_TIME equ	250
MORPH_DURATION	equ	128
SHAPE_BASE_Z	equ	160
EFFECT_TUNNEL	equ	0
EFFECT_MORPH	equ	1
EFFECT_DISPLAY	equ	2

; Sprite constants
SPR_MAX_H	equ	64
SPR_VBOT	equ	300
EQ_DECAY	equ	4
SPR_H0		equ	52		; left side
SPR_H1		equ	72
SPR_H2		equ	372		; right side
SPR_H3		equ	392
ENERGY_DECAY	equ	2

; Zone boundaries (PAL VPOS)
LOGO_START	equ	$2C		; line 44
LOGO_END	equ	$6C		; line 108 (64 lines)
MIDDLE_START	equ	$6C		; line 108
MIDDLE_END	equ	$C7		; line 199 (92 lines, but we use $D8 for scroll)
SCROLL_START	equ	$D8		; line 216
SCROLL_LINES	equ	40		; 5 char rows of 8

; Scrolltext
SCROLL_AREA	equ	BPLANE1+(SCROLL_START-LOGO_START)*BPWIDTH
SCROLL_SPEED	equ	1		; pixels per frame

; Beat flash
BEAT_THRESHOLD	equ	40
BEAT_JUMP	equ	20

; Custom chip registers
CUSTOM		equ	$DFF000
VPOSR		equ	$004
VHPOSR		equ	$006
DMACON		equ	$096
INTENA		equ	$09A
INTREQ		equ	$09C
INTREQR		equ	$01E
INTENAR		equ	$01C
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
SPR0PTH		equ	$120
SPR0PTL		equ	$122
SPR1PTH		equ	$124
SPR1PTL		equ	$126
SPR2PTH		equ	$128
SPR2PTL		equ	$12A
SPR3PTH		equ	$12C
SPR3PTL		equ	$12E
SPR4PTH		equ	$130
SPR4PTL		equ	$132
SPR5PTH		equ	$134
SPR5PTL		equ	$136
SPR6PTH		equ	$138
SPR6PTL		equ	$13A
SPR7PTH		equ	$13C
SPR7PTL		equ	$13E

	org	$49000

;==========================================================
; Entry point
;==========================================================
Start:
	lea	CUSTOM,a6

	; Clear bitplane 1
	lea	BPLANE1,a0
	move.w	#(BPSIZE/4)-1,d0
.clr1:	clr.l	(a0)+
	dbf	d0,.clr1

	; Clear bitplane 2
	lea	BPLANE2,a0
	move.w	#(BPSIZE/4)-1,d0
.clr2:	clr.l	(a0)+
	dbf	d0,.clr2

	; Draw logo bitmap into top of BPL1
	bsr.w	DrawLogo

	; Init stars
	bsr.w	InitStars

	; Init sprites
	bsr.w	InitSprites

	; Init scrolltext
	clr.w	scroll_charpos
	clr.w	scroll_bitcount
	lea	scroll_text(pc),a0
	move.l	a0,scroll_ptr

	; Init shape cycling
	clr.w	effect_state
	clr.w	shape_index
	clr.w	display_timer
	clr.w	morph_counter
	clr.w	angle_y
	clr.w	angle_x

	; Build initial copper lists
	lea	COPPER_A,a0
	bsr.w	BuildCopper
	lea	COPPER_B,a0
	bsr.w	BuildCopper

	; Install copper list A
	move.l	#COPPER_A,COP1LCH(a6)
	move.w	COPJMP1(a6),d0

	; Enable DMA: SET + DMAEN + BPLEN + COPEN + SPREN
	move.w	#$83E0,DMACON(a6)

	; Init phases
	clr.w	plasma_phase1
	clr.w	plasma_phase2
	clr.w	rainbow_phase
	clr.w	sine_phase
	clr.w	orbit_phase
	clr.w	flash_timer

	; Init copper buffer pointers
	move.l	#COPPER_A,cop_front
	move.l	#COPPER_B,cop_back

	; Install CIA-B interrupt for music playback
	sub.l	a0,a0
	moveq	#1,d0
	bsr	_mt_install_cia

	; Init module
	lea	moddata,a0
	sub.l	a1,a1
	moveq	#0,d0
	bsr	_mt_init

	; Enable playback
	st	_mt_Enable

	; Enable master interrupt
	move.w	#$C000,INTENA(a6)

;==========================================================
; Main loop
;==========================================================
MainLoop:
	bsr.w	WaitVBL
	bsr.w	UpdateAllChannelEnergy
	bsr.w	DetectBeat
	bsr.w	UpdateEqualizer

	; Swap copper buffers
	move.l	cop_front(pc),d0
	move.l	cop_back(pc),d1
	move.l	d1,cop_front
	move.l	d0,cop_back
	move.l	cop_front(pc),COP1LCH(a6)
	move.w	COPJMP1(a6),d0

	; State dispatch: tunnel / morph / display
	move.w	effect_state(pc),d0
	cmp.w	#EFFECT_MORPH,d0
	beq.w	.doMorph
	cmp.w	#EFFECT_DISPLAY,d0
	beq.w	.doDisplay

	; --- TUNNEL state ---
	bsr.w	UpdateStars
	addq.w	#1,display_timer
	move.w	display_timer(pc),d0
	cmp.w	#SHAPE_DISPLAY_TIME,d0
	blt.s	.stateEnd
	bsr.w	StartNextMorph
	bra.s	.stateEnd

.doMorph:
	bsr.w	ShapeStars
	bra.s	.stateEnd
.doDisplay:
	bsr.w	ShapeStars
	addq.w	#1,display_timer
	move.w	display_timer(pc),d0
	cmp.w	#SHAPE_DISPLAY_TIME,d0
	blt.s	.stateEnd
	bsr.w	StartNextMorph
.stateEnd:

	; Update scrolltext
	bsr.w	UpdateScrolltext

	; Build next frame into back buffer
	move.l	cop_back(pc),a0
	bsr.w	BuildCopper

	; Advance animation phases
	addq.w	#3,plasma_phase1
	and.w	#$FF,plasma_phase1
	addq.w	#1,plasma_phase2
	and.w	#$FF,plasma_phase2
	addq.w	#1,rainbow_phase
	and.w	#$FF,rainbow_phase
	addq.w	#2,sine_phase
	and.w	#$FF,sine_phase
	addq.w	#2,orbit_phase
	and.w	#$FF,orbit_phase

	; Decrement flash timer
	move.w	flash_timer(pc),d0
	beq.s	.noFlashDec
	subq.w	#1,d0
	move.w	d0,flash_timer
.noFlashDec:

	bra.w	MainLoop

;==========================================================
; Variables
;==========================================================
cop_front:	dc.l	COPPER_A
cop_back:	dc.l	COPPER_B
plasma_phase1:	dc.w	0
plasma_phase2:	dc.w	0
rainbow_phase:	dc.w	0
sine_phase:	dc.w	0
orbit_phase:	dc.w	0
lfsr_state:	dc.w	$ACE1
effect_state:	dc.w	0
morph_counter:	dc.w	0
angle_y:	dc.w	0
angle_x:	dc.w	0
shape_index:	dc.w	0
display_timer:	dc.w	0
current_targets: dc.l	0
current_zspeed:	dc.w	2
current_base_z:	dc.w	SHAPE_BASE_Z
flash_timer:	dc.w	0
eq_energy:	dc.w	0,0,0,0
combined_energy: dc.w	0
last_combined:	dc.w	0
scroll_charpos:	dc.w	0
scroll_bitcount: dc.w	0
scroll_ptr:	dc.l	0
rot_sin_y:	dc.w	0
rot_cos_y:	dc.w	0
rot_sin_x:	dc.w	0
rot_cos_x:	dc.w	0
bar_phase:	dc.w	0
bar_y:		dc.w	153,153,153,153

;==========================================================
; WaitVBL — wait for line 300
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
; UpdateAllChannelEnergy — read 4 channels, attack/decay
;==========================================================
UpdateAllChannelEnergy:
	movem.l	d2-d4/a2-a3,-(sp)
	lea	mt_data,a2
	lea	eq_energy(pc),a3
	lea	_mt_VUMeter,a0
	move.b	(a0),d4
	clr.b	(a0)

	; Channel volume offsets
	lea	.voloffs(pc),a0
	moveq	#0,d3			; bit index
	moveq	#3,d2			; loop counter

.chLoop:
	move.w	(a0)+,d0
	move.w	0(a2,d0.w),d0		; channel volume 0-64
	move.w	(a3),d1			; current energy

	btst	d3,d4
	beq.s	.noNote
	move.w	d0,d1			; attack: set energy = volume
.noNote:
	subq.w	#ENERGY_DECAY,d1
	bpl.s	.storeE
	moveq	#0,d1
.storeE:
	move.w	d1,(a3)+
	addq.w	#1,d3
	dbf	d2,.chLoop

	; Compute combined energy (ch0+ch1 = bass)
	lea	eq_energy(pc),a3
	move.w	(a3),d0
	add.w	2(a3),d0
	lsr.w	#1,d0			; average
	move.w	d0,combined_energy

	; Z speed = MIN_ZSPEED + (combined >> 3)
	move.w	d0,d1
	lsr.w	#3,d1
	addq.w	#MIN_ZSPEED,d1
	move.w	d1,current_zspeed

	; base_z = SHAPE_BASE_Z - combined
	move.w	#SHAPE_BASE_Z,d1
	sub.w	d0,d1
	cmp.w	#80,d1
	bge.s	.bzOk
	move.w	#80,d1
.bzOk:	move.w	d1,current_base_z

	; Update bar positions
	lea	signed_sine_table(pc),a0
	lea	bar_y(pc),a3
	lea	eq_energy(pc),a2
	move.w	bar_phase(pc),d3
	moveq	#3,d2
	moveq	#0,d4			; channel offset
.barLoop:
	move.w	d3,d0
	add.w	d4,d0
	and.w	#$FF,d0
	add.w	d0,d0
	move.w	(a0,d0.w),d0		; sin value -127..+127
	move.w	(a2)+,d1		; channel energy
	muls	d1,d0
	asr.l	#7,d0
	add.w	#153,d0			; center of middle zone
	cmp.w	#MIDDLE_START,d0
	bge.s	.barClipLo
	move.w	#MIDDLE_START,d0
.barClipLo:
	cmp.w	#$C0,d0
	ble.s	.barClipHi
	move.w	#$C0,d0
.barClipHi:
	move.w	d0,(a3)+
	add.w	#64,d4
	dbf	d2,.barLoop

	addq.w	#2,bar_phase
	and.w	#$FF,bar_phase

	movem.l	(sp)+,d2-d4/a2-a3
	rts

.voloffs:
	dc.w	mt_chan1+n_volume
	dc.w	mt_chan2+n_volume
	dc.w	mt_chan3+n_volume
	dc.w	mt_chan4+n_volume

;==========================================================
; DetectBeat — rising edge detection on bass energy
;==========================================================
DetectBeat:
	move.w	combined_energy(pc),d0
	move.w	last_combined(pc),d1
	move.w	d0,last_combined

	; Rising edge: current - last > BEAT_JUMP and current > threshold
	sub.w	d1,d0			; d0 = delta
	cmp.w	#BEAT_JUMP,d0
	blt.s	.noBeat
	move.w	combined_energy(pc),d0
	cmp.w	#BEAT_THRESHOLD,d0
	blt.s	.noBeat
	move.w	#3,flash_timer
.noBeat:
	rts

;==========================================================
; LFSRNext — 16-bit Galois LFSR
;==========================================================
LFSRNext:
	move.w	lfsr_state(pc),d2
	lsr.w	#1,d2
	bcc.s	.noTap
	eor.w	#$B400,d2
.noTap:	move.w	d2,lfsr_state
	rts

;==========================================================
; InitStars
;==========================================================
InitStars:
	movem.l	d2-d4/a2,-(sp)
	lea	STAR_ARRAY,a2
	move.w	#NUM_STARS-1,d4
.initLoop:
	bsr.w	LFSRNext
	move.b	d2,d0
	ext.w	d0
	move.w	d0,STAR_SX(a2)
	bsr.w	LFSRNext
	move.b	d2,d0
	ext.w	d0
	move.w	d0,STAR_SY(a2)
	bsr.w	LFSRNext
	move.w	d2,d0
	and.w	#$FF,d0
	mulu	#MAX_Z-MIN_Z,d0
	lsr.l	#8,d0
	add.w	#MIN_Z,d0
	move.w	d0,STAR_SZ(a2)
	move.w	#$FFFF,STAR_OPX(a2)
	clr.w	STAR_OPY(a2)
	lea	STAR_SIZE(a2),a2
	dbf	d4,.initLoop
	movem.l	(sp)+,d2-d4/a2
	rts

;==========================================================
; EraseStar
;==========================================================
EraseStar:
	move.w	STAR_OPX(a2),d0
	cmp.w	#$FFFF,d0
	beq.s	.done
	move.w	STAR_OPY(a2),d1
	add.w	d1,d1
	move.w	0(a4,d1.w),d4
	move.w	d0,d2
	lsr.w	#3,d2
	add.w	d2,d4
	move.w	d0,d2
	not.w	d2
	and.w	#7,d2
	bclr	d2,0(a0,d4.w)
.done:	rts

;==========================================================
; ProjectAndPlot
;==========================================================
ProjectAndPlot:
	tst.w	d0
	blt.s	.oob
	cmp.w	#383,d0
	bgt.s	.oob
	tst.w	d1
	blt.s	.oob
	cmp.w	#255,d1
	bgt.s	.oob
	move.w	d1,d4
	add.w	d4,d4
	move.w	0(a4,d4.w),d4
	move.w	d0,d2
	lsr.w	#3,d2
	add.w	d2,d4
	move.w	d0,d2
	not.w	d2
	and.w	#7,d2
	bset	d2,0(a0,d4.w)
	move.w	d0,STAR_OPX(a2)
	move.w	d1,STAR_OPY(a2)
	moveq	#1,d0
	rts
.oob:	move.w	#$FFFF,STAR_OPX(a2)
	moveq	#0,d0
	rts

;==========================================================
; UpdateStars — tunnel mode
;==========================================================
UpdateStars:
	movem.l	d2-d7/a2-a5,-(sp)
	lea	BPLANE2,a0
	lea	recip_table(pc),a3
	lea	yoffset_table(pc),a4
	lea	STAR_ARRAY,a2
	lea	signed_sine_table(pc),a5

	; Compute tunnel center
	move.w	orbit_phase(pc),d0
	add.w	d0,d0
	move.w	(a5,d0.w),d5
	muls	#ORBIT_RADIUS,d5
	asr.l	#7,d5
	add.w	#CENTER_X,d5

	move.w	orbit_phase(pc),d0
	add.w	#64,d0
	and.w	#$FF,d0
	add.w	d0,d0
	move.w	(a5,d0.w),d6
	muls	#ORBIT_RADIUS,d6
	asr.l	#7,d6
	add.w	#CENTER_Y,d6

	move.w	#NUM_STARS-1,d7
.starLoop:
	bsr.w	EraseStar
	move.w	STAR_SZ(a2),d0
	sub.w	current_zspeed(pc),d0
	cmp.w	#MIN_Z,d0
	ble.s	.respawn
	move.w	d0,STAR_SZ(a2)
	move.w	STAR_SZ(a2),d0
	add.w	d0,d0
	move.w	0(a3,d0.w),d1
	move.w	d1,d3
	move.w	STAR_SX(a2),d0
	muls	d1,d0
	asr.l	#8,d0
	add.w	d5,d0
	move.w	STAR_SY(a2),d1
	muls	d3,d1
	asr.l	#8,d1
	add.w	d6,d1
	bsr.w	ProjectAndPlot
	tst.w	d0
	bne.s	.nextStar
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
.nextStar:
	lea	STAR_SIZE(a2),a2
	dbf	d7,.starLoop
	movem.l	(sp)+,d2-d7/a2-a5
	rts

;==========================================================
; StartNextMorph
;==========================================================
StartNextMorph:
	move.w	shape_index(pc),d0
	addq.w	#1,d0
	cmp.w	#NUM_SHAPES,d0
	blt.s	.noWrap
	clr.w	d0
.noWrap:
	move.w	d0,shape_index
	add.w	d0,d0
	add.w	d0,d0
	lea	TargetPtrTable(pc),a0
	move.l	0(a0,d0.w),current_targets
	move.w	#EFFECT_MORPH,effect_state
	clr.w	morph_counter
	clr.w	display_timer
	rts

TargetPtrTable:
	dc.l	tunnel_targets
	dc.l	cube_targets
	dc.l	sphere_targets
	dc.l	helix_targets

;==========================================================
; ShapeStars — rotate+morph toward shape targets
;==========================================================
ShapeStars:
	movem.l	d2-d7/a2-a5,-(sp)
	lea	BPLANE2,a0
	lea	recip_table(pc),a3
	lea	yoffset_table(pc),a4
	lea	STAR_ARRAY,a2
	lea	signed_sine_table(pc),a5

	; sin/cos Y
	move.w	angle_y(pc),d0
	and.w	#$FF,d0
	add.w	d0,d0
	move.w	0(a5,d0.w),rot_sin_y
	move.w	angle_y(pc),d0
	add.w	#64,d0
	and.w	#$FF,d0
	add.w	d0,d0
	move.w	0(a5,d0.w),rot_cos_y

	; sin/cos X
	move.w	angle_x(pc),d0
	and.w	#$FF,d0
	add.w	d0,d0
	move.w	0(a5,d0.w),rot_sin_x
	move.w	angle_x(pc),d0
	add.w	#64,d0
	and.w	#$FF,d0
	add.w	d0,d0
	move.w	0(a5,d0.w),rot_cos_x

	move.l	current_targets(pc),a1
	move.w	#NUM_STARS-1,d7

.starLoop:
	bsr.w	EraseStar
	move.w	effect_state(pc),d0
	cmp.w	#EFFECT_MORPH,d0
	beq.s	.ease

	; Direct read (display mode)
	move.w	(a1),d0
	move.w	2(a1),d1
	move.w	4(a1),d2
	bra.s	.rotate

.ease:
	; Exponential ease
	move.w	(a1),d0
	sub.w	STAR_SX(a2),d0
	asr.w	#3,d0
	add.w	d0,STAR_SX(a2)
	move.w	2(a1),d0
	sub.w	STAR_SY(a2),d0
	asr.w	#3,d0
	add.w	d0,STAR_SY(a2)
	move.w	4(a1),d0
	sub.w	STAR_SZ(a2),d0
	asr.w	#3,d0
	add.w	d0,STAR_SZ(a2)
	move.w	STAR_SX(a2),d0
	move.w	STAR_SY(a2),d1
	move.w	STAR_SZ(a2),d2

.rotate:
	; Y rotation
	move.w	d0,d3
	move.w	d2,d4
	muls	rot_cos_y(pc),d0
	muls	rot_sin_y(pc),d4
	add.l	d4,d0
	asr.l	#7,d0
	move.w	d2,d4
	muls	rot_cos_y(pc),d4
	muls	rot_sin_y(pc),d3
	sub.l	d3,d4
	asr.l	#7,d4

	; X rotation
	move.w	d1,d3
	move.w	d4,d5
	muls	rot_cos_x(pc),d1
	muls	rot_sin_x(pc),d5
	sub.l	d5,d1
	asr.l	#7,d1
	move.w	d3,d5
	muls	rot_sin_x(pc),d5
	muls	rot_cos_x(pc),d4
	add.l	d5,d4
	asr.l	#7,d4

	add.w	current_base_z(pc),d4
	cmp.w	#MIN_Z,d4
	ble.s	.skipPlot
	cmp.w	#255,d4
	ble.s	.zOk
	move.w	#255,d4
.zOk:
	move.w	d4,d5
	add.w	d5,d5
	move.w	0(a3,d5.w),d5
	muls	d5,d0
	asr.l	#8,d0
	add.w	#CENTER_X,d0
	muls	d5,d1
	asr.l	#8,d1
	add.w	#CENTER_Y,d1
	bsr.w	ProjectAndPlot
	bra.s	.nextStar

.skipPlot:
	move.w	#$FFFF,STAR_OPX(a2)
.nextStar:
	lea	6(a1),a1
	lea	STAR_SIZE(a2),a2
	dbf	d7,.starLoop

	addq.w	#2,angle_y
	and.w	#$FF,angle_y
	addq.w	#1,angle_x
	and.w	#$FF,angle_x

	; Check morph completion
	move.w	effect_state(pc),d0
	cmp.w	#EFFECT_MORPH,d0
	bne.s	.done
	addq.w	#1,morph_counter
	move.w	morph_counter(pc),d0
	cmp.w	#MORPH_DURATION,d0
	blt.s	.done
	move.w	shape_index(pc),d0
	tst.w	d0
	bne.s	.toDisplay
	move.w	#EFFECT_TUNNEL,effect_state
	clr.w	display_timer
	bra.s	.done
.toDisplay:
	move.w	#EFFECT_DISPLAY,effect_state
	clr.w	display_timer
.done:
	movem.l	(sp)+,d2-d7/a2-a5
	rts

;==========================================================
; UpdateScrolltext — shift left 1px/frame, render chars
;==========================================================
UpdateScrolltext:
	movem.l	d2-d5/a2-a3,-(sp)

	; Shift all 40 lines of scroll area left by 1 pixel
	lea	SCROLL_AREA,a0
	moveq	#SCROLL_LINES-1,d5
.shiftLine:
	; Shift 24 words left by 1 pixel via ROXL chain
	; Walk from rightmost word to leftmost, carry propagates
	lea	BPWIDTH-2(a0),a2	; point to last word
	moveq	#23,d3
	andi	#$FE,ccr		; clear X flag
.shiftWord:
	roxl.w	(a2)
	subq.l	#2,a2
	dbf	d3,.shiftWord

	lea	BPWIDTH(a0),a0
	dbf	d5,.shiftLine

	; Every 8 frames, render next char at right edge
	move.w	scroll_bitcount(pc),d0
	addq.w	#1,d0
	cmp.w	#8,d0
	blt.s	.noChar
	clr.w	d0
	bsr.w	DrawScrollChar
.noChar:
	move.w	d0,scroll_bitcount

	movem.l	(sp)+,d2-d5/a2-a3
	rts

;==========================================================
; DrawScrollChar — render 8x8 char at right edge of scroll
;==========================================================
DrawScrollChar:
	movem.l	d2/a2-a3,-(sp)

	; Get next char
	move.l	scroll_ptr(pc),a2
	moveq	#0,d0
	move.b	(a2)+,d0
	cmp.b	#$FF,d0			; end marker?
	bne.s	.notEnd
	lea	scroll_text(pc),a2	; wrap around
	move.b	(a2)+,d0
.notEnd:
	move.l	a2,scroll_ptr

	; Font lookup: (char - 32) * 8
	sub.w	#32,d0
	bpl.s	.charOk
	moveq	#0,d0			; below space = space
.charOk:
	cmp.w	#96,d0
	blt.s	.charInRange
	moveq	#0,d0
.charInRange:
	lsl.w	#3,d0			; *8
	lea	font_data(pc),a3
	add.w	d0,a3			; a3 = glyph data

	; Write 8 rows into rightmost byte of each scroll line
	lea	SCROLL_AREA,a0
	add.w	#BPWIDTH-1,a0		; rightmost byte
	moveq	#7,d2
.charRow:
	move.b	(a3)+,(a0)
	lea	BPWIDTH(a0),a0
	dbf	d2,.charRow

	movem.l	(sp)+,d2/a2-a3
	rts

;==========================================================
; InitSprites
;==========================================================
InitSprites:
	lea	NULL_SPR,a0
	clr.l	(a0)

	lea	SprInitTab(pc),a1
	moveq	#3,d7
.sprInitLoop:
	move.l	(a1)+,a0
	move.w	(a1)+,d0

	move.w	#(SPR_VBOT&$FF)<<8,d1
	move.w	d0,d2
	lsr.w	#1,d2
	or.w	d2,d1
	move.w	d1,(a0)+

	move.w	#((SPR_VBOT&$FF)<<8)|$0006,d1
	move.w	d1,(a0)+

	move.w	#SPR_MAX_H-1,d1
.sprFill:
	move.l	#$FFFFFFFF,(a0)+
	dbf	d1,.sprFill

	clr.l	(a0)+
	move.l	#$FFFFFFFF,(a0)

	dbf	d7,.sprInitLoop
	rts

SprInitTab:
	dc.l	SPR0_DATA
	dc.w	SPR_H0
	dc.l	SPR2_DATA
	dc.w	SPR_H1
	dc.l	SPR4_DATA
	dc.w	SPR_H2
	dc.l	SPR6_DATA
	dc.w	SPR_H3

;==========================================================
; UpdateEqualizer — update sprite POS/CTL from energy
;==========================================================
UpdateEqualizer:
	movem.l	d2-d5/a2-a4,-(sp)
	lea	mt_data,a2
	lea	eq_energy(pc),a3
	lea	EQSpriteCtrl(pc),a4

	moveq	#3,d5
.eqLoop:
	move.w	(a3)+,d1		; energy for this channel

	move.w	#SPR_VBOT,d0
	sub.w	d1,d0			; VSTART

	move.l	(a4)+,a0		; sprite data address
	move.w	(a4)+,d2		; HSTART>>1

	move.w	d0,d3
	and.w	#$FF,d3
	lsl.w	#8,d3
	or.w	d2,d3
	move.w	d3,(a0)			; POS

	move.w	#((SPR_VBOT&$FF)<<8)|$0002,d3
	btst	#8,d0
	beq.s	.noV8
	or.w	#$0004,d3
.noV8:	move.w	d3,2(a0)		; CTL

	dbf	d5,.eqLoop
	movem.l	(sp)+,d2-d5/a2-a4
	rts

EQSpriteCtrl:
	dc.l	SPR0_DATA
	dc.w	SPR_H0>>1
	dc.l	SPR2_DATA
	dc.w	SPR_H1>>1
	dc.l	SPR4_DATA
	dc.w	SPR_H2>>1
	dc.l	SPR6_DATA
	dc.w	SPR_H3>>1

;==========================================================
; DrawLogo — copy logo_data into top of BPL1
;==========================================================
DrawLogo:
	lea	logo_data(pc),a0
	lea	BPLANE1,a1
	; Logo is 48*32 = 1536 bytes, copy as longwords
	move.w	#(48*32/4)-1,d0
.copy:	move.l	(a0)+,(a1)+
	dbf	d0,.copy
	rts

;==========================================================
; BuildCopper — the heart of the visuals
; Input: a0 = copper buffer pointer
;==========================================================
BuildCopper:
	movem.l	d2-d7/a2-a5,-(sp)

	; --- Header ---
	move.l	#(DIWSTRT<<16)|DIWSTRT_VAL,(a0)+
	move.l	#(DIWSTOP<<16)|DIWSTOP_VAL,(a0)+
	move.l	#(DDFSTRT<<16)|DDFSTRT_VAL,(a0)+
	move.l	#(DDFSTOP<<16)|DDFSTOP_VAL,(a0)+
	move.l	#(BPLCON0<<16)|BPLCON0_VAL,(a0)+
	move.l	#(BPLCON1<<16)|$0000,(a0)+
	move.l	#(BPLCON2<<16)|$0000,(a0)+
	move.l	#(BPL1MOD<<16)|BPL_MODULO,(a0)+
	move.l	#(BPL2MOD<<16)|BPL_MODULO,(a0)+

	; BPL2 pointer (stars — set once)
	move.l	#(BPL2PTH<<16)|(BPLANE2>>16),(a0)+
	move.l	#(BPL2PTL<<16)|(BPLANE2&$FFFF),(a0)+

	; Base colors
	move.l	#(COLOR00<<16)|$0000,(a0)+
	move.l	#(COLOR02<<16)|$06FF,(a0)+	; stars = cyan
	move.l	#(COLOR03<<16)|$0FFF,(a0)+	; overlap = white

	; --- Sprite pointers ---
	move.l	#(SPR0PTH<<16)|(SPR0_DATA>>16),(a0)+
	move.l	#(SPR0PTL<<16)|(SPR0_DATA&$FFFF),(a0)+
	move.l	#(SPR1PTH<<16)|(NULL_SPR>>16),(a0)+
	move.l	#(SPR1PTL<<16)|(NULL_SPR&$FFFF),(a0)+
	move.l	#(SPR2PTH<<16)|(SPR2_DATA>>16),(a0)+
	move.l	#(SPR2PTL<<16)|(SPR2_DATA&$FFFF),(a0)+
	move.l	#(SPR3PTH<<16)|(NULL_SPR>>16),(a0)+
	move.l	#(SPR3PTL<<16)|(NULL_SPR&$FFFF),(a0)+
	move.l	#(SPR4PTH<<16)|(SPR4_DATA>>16),(a0)+
	move.l	#(SPR4PTL<<16)|(SPR4_DATA&$FFFF),(a0)+
	move.l	#(SPR5PTH<<16)|(NULL_SPR>>16),(a0)+
	move.l	#(SPR5PTL<<16)|(NULL_SPR&$FFFF),(a0)+
	move.l	#(SPR6PTH<<16)|(SPR6_DATA>>16),(a0)+
	move.l	#(SPR6PTL<<16)|(SPR6_DATA&$FFFF),(a0)+
	move.l	#(SPR7PTH<<16)|(NULL_SPR>>16),(a0)+
	move.l	#(SPR7PTL<<16)|(NULL_SPR&$FFFF),(a0)+

	; Sprite colors (one color per pair)
	move.l	#($01A6<<16)|$0F44,(a0)+	; spr0 col3 = red
	move.l	#($01AE<<16)|$04F4,(a0)+	; spr2 col3 = green
	move.l	#($01B6<<16)|$048F,(a0)+	; spr4 col3 = blue
	move.l	#($01BE<<16)|$0FF4,(a0)+	; spr6 col3 = yellow

	; Load table pointers
	lea	sine_table(pc),a2
	lea	plasma_table(pc),a3
	lea	rainbow_table(pc),a4
	lea	signed_sine_table(pc),a5

	; Load phases
	move.w	plasma_phase1(pc),d2
	move.w	rainbow_phase(pc),d3
	move.w	sine_phase(pc),d6

	; Flash bias: d7.w = 0 or flash per-channel amount (0-6)
	move.w	flash_timer(pc),d7
	tst.w	d7
	beq.s	.noFlashBias
	add.w	d7,d7			; timer*2 = flash amount per nibble (2,4,6)
.noFlashBias:

	; ============================================
	; LOGO ZONE: lines $2C - $6B (64 lines)
	; Per line: WAIT + COLOR00(plasma) + COLOR01(rainbow) + BPL1PT(sine wave)
	; ============================================
	move.l	#BPLANE1,d4		; running BPL1 address
	moveq	#0,d5			; line counter within zone

.logoLine:
	move.w	#LOGO_START,d0
	add.w	d5,d0			; VPOS

	; WAIT
	move.w	d0,d1
	and.w	#$FF,d1
	lsl.w	#8,d1
	or.w	#$07,d1
	move.w	d1,(a0)+
	move.w	#$FFFE,(a0)+

	; COLOR00 = plasma
	move.w	d5,d0
	add.w	d5,d0			; y*2
	add.w	d2,d0			; + phase1
	and.w	#$FF,d0
	add.w	d0,d0			; word index
	move.w	#COLOR00,(a0)+
	move.w	0(a3,d0.w),d0
	; Apply flash bias
	tst.w	d7
	beq.s	.lNoFlash1
	bsr.w	ApplyFlash
.lNoFlash1:
	move.w	d0,(a0)+

	; COLOR01 = rainbow
	move.w	d5,d0
	add.w	d3,d0			; + rainbow_phase
	and.w	#$FF,d0
	add.w	d0,d0
	move.w	#COLOR01,(a0)+
	move.w	0(a4,d0.w),d0
	tst.w	d7
	beq.s	.lNoFlash2
	bsr.w	ApplyFlash
.lNoFlash2:
	move.w	d0,(a0)+

	; BPL1PT sine wave (logo wave distortion)
	; Amplitude modulated by bass energy
	move.w	d5,d0
	add.w	d6,d0			; + sine_phase
	and.w	#$FF,d0
	moveq	#0,d1
	move.b	0(a2,d0.w),d1		; sine 0-48
	; Scale by bass energy: (sine * combined_energy) >> 6
	move.w	combined_energy(pc),d0
	mulu	d1,d0
	lsr.l	#6,d0
	move.w	d0,d1

	; Decompose into fine + coarse
	move.w	d1,d0
	neg.w	d0
	and.w	#15,d0			; fine scroll
	add.w	d0,d1			; round up
	lsr.w	#4,d1			; coarse words
	add.w	d1,d1			; coarse bytes
	ext.l	d1
	move.l	d4,d1			; BPL1 base for this line
	; Actually just use simple sine offset
	moveq	#0,d1
	move.b	0(a2,d0.w),d1
	; Simpler approach: just offset BPL1PT by sine value bytes
	move.w	d5,d0
	add.w	d6,d0
	and.w	#$FF,d0
	moveq	#0,d1
	move.b	0(a2,d0.w),d1
	; Scale amplitude by energy
	move.w	combined_energy(pc),d0
	mulu	d1,d0
	lsr.l	#7,d0			; scaled offset 0..~24
	and.w	#$FFFE,d0		; word-align
	ext.l	d0
	move.l	d4,d1
	add.l	d0,d1			; d1 = adjusted BPL1PT

	move.w	#BPL1PTH,(a0)+
	swap	d1
	move.w	d1,(a0)+
	move.w	#BPL1PTL,(a0)+
	swap	d1
	move.w	d1,(a0)+

	; Advance running BPL1 pointer
	add.l	#BPWIDTH,d4

	addq.w	#1,d5
	cmp.w	#64,d5			; 64 logo lines
	blt.w	.logoLine

	; ============================================
	; MIDDLE ZONE start ($6C): disable BPL1 text
	; Set BPL1PT to a blank area (after BPL2) and COLOR01=$000
	; ============================================
	; WAIT for middle start
	move.w	#(MIDDLE_START<<8)|$07,(a0)+
	move.w	#$FFFE,(a0)+
	; Point BPL1 to end of BPL2 (zeroed area in initial clear)
	move.l	#(BPL1PTH<<16)|((BPLANE2+BPSIZE)>>16),(a0)+
	move.l	#(BPL1PTL<<16)|((BPLANE2+BPSIZE)&$FFFF),(a0)+
	move.l	#(COLOR01<<16)|$0000,(a0)+

	; ============================================
	; MIDDLE ZONE: lines $6C - $D7 (108 lines)
	; Per line: WAIT + COLOR00(plasma or raster bar)
	; ============================================
	moveq	#0,d5			; line counter

.middleLine:
	move.w	#MIDDLE_START,d0
	add.w	d5,d0			; VPOS

	; V8 barrier (insert once at $100)
	cmp.w	#$100,d0
	bne.s	.mNoBarrier
	move.l	#$FFDFFFFE,(a0)+
.mNoBarrier:

	; WAIT
	move.w	d0,d1
	and.w	#$FF,d1
	lsl.w	#8,d1
	or.w	#$07,d1
	move.w	d1,(a0)+
	move.w	#$FFFE,(a0)+

	; Check raster bars: is this line within any bar?
	move.w	#MIDDLE_START,d0
	add.w	d5,d0			; d0 = current VPOS
	moveq	#3,d3			; bar index (3..0)
.barCheck:
	move.w	d3,d1
	add.w	d1,d1			; word offset into bar_y
	lea	bar_y(pc),a1
	move.w	0(a1,d1.w),d1		; bar center Y
	sub.w	d0,d1			; signed dist = center - current
	bpl.s	.barNoNeg
	neg.w	d1
.barNoNeg:
	cmp.w	#8,d1
	blt.s	.barHit
	dbf	d3,.barCheck
	bra.s	.barMiss

.barHit:
	; d3 = bar index, compute gradient index
	; signed distance: center - current
	move.w	d3,d1
	add.w	d1,d1
	lea	bar_y(pc),a1
	move.w	0(a1,d1.w),d1		; center
	sub.w	d1,d0			; d0 = current - center (-7..+7)
	add.w	#8,d0			; index 0-15
	; Clamp
	bpl.s	.bIdxOk
	moveq	#0,d0
.bIdxOk:
	cmp.w	#15,d0
	ble.s	.bIdxOk2
	moveq	#15,d0
.bIdxOk2:
	add.w	d0,d0			; word offset into gradient
	; Get gradient table for bar d3
	move.w	d3,d1
	lsl.w	#5,d1			; *32 bytes (16 words)
	lea	rasterbar_red(pc),a1
	add.w	d1,a1
	move.w	0(a1,d0.w),d0		; gradient color
	tst.w	d7
	beq.s	.barNoFlash
	bsr.w	ApplyFlash
.barNoFlash:
	move.w	#COLOR00,(a0)+
	move.w	d0,(a0)+
	bra.s	.middleNext

.barMiss:
	; No bar hit — use plasma
	move.w	d5,d0
	add.w	#64,d0
	add.w	d0,d0
	add.w	d2,d0
	and.w	#$1FE,d0
	move.w	#COLOR00,(a0)+
	move.w	0(a3,d0.w),d0
	tst.w	d7
	beq.s	.mNoFlash
	bsr.w	ApplyFlash
.mNoFlash:
	move.w	d0,(a0)+

.middleNext:
	addq.w	#1,d5
	cmp.w	#108,d5
	blt.w	.middleLine

	; ============================================
	; SCROLL ZONE: lines $D8 - $FF + barrier + $00-$0B
	; Re-enable BPL1, apply sine wave + rainbow
	; ============================================
	; Compute BPL1 base for scroll area
	move.l	#SCROLL_AREA,d4

	moveq	#0,d5
	moveq	#0,d1			; barrier flag

.scrollLine:
	move.w	#SCROLL_START,d0
	add.w	d5,d0

	; V8 barrier
	tst.w	d1
	bne.s	.sPastBarrier
	cmp.w	#$100,d0
	blt.s	.sNoBarrier
	move.l	#$FFDFFFFE,(a0)+
	moveq	#1,d1
.sNoBarrier:
.sPastBarrier:

	; WAIT
	move.w	d0,d1
	and.w	#$FF,d1
	lsl.w	#8,d1
	or.w	#$07,d1
	move.w	d1,(a0)+
	move.w	#$FFFE,(a0)+
	; Reload barrier flag
	move.w	#SCROLL_START,d0
	add.w	d5,d0
	cmp.w	#$100,d0
	sge	d1
	ext.w	d1
	neg.w	d1

	; COLOR00 = plasma
	move.w	d5,d0
	add.w	#172,d0			; offset past logo+middle
	add.w	d0,d0
	add.w	d2,d0
	and.w	#$1FE,d0
	move.w	#COLOR00,(a0)+
	move.w	0(a3,d0.w),d0
	tst.w	d7
	beq.s	.sNoFlash1
	bsr.w	ApplyFlash
.sNoFlash1:
	move.w	d0,(a0)+

	; COLOR01 = rainbow
	move.w	d5,d0
	add.w	#172,d0
	add.w	d3,d0
	and.w	#$FF,d0
	add.w	d0,d0
	move.w	#COLOR01,(a0)+
	move.w	0(a4,d0.w),d0
	tst.w	d7
	beq.s	.sNoFlash2
	bsr.w	ApplyFlash
.sNoFlash2:
	move.w	d0,(a0)+

	; BPL1PT with sine offset
	move.w	d5,d0
	add.w	d6,d0
	and.w	#$FF,d0
	moveq	#0,d1
	move.b	0(a2,d0.w),d1
	and.w	#$FFFE,d1		; word-align
	ext.l	d1
	move.l	d4,d0
	add.l	d1,d0

	move.w	#BPL1PTH,(a0)+
	swap	d0
	move.w	d0,(a0)+
	move.w	#BPL1PTL,(a0)+
	swap	d0
	move.w	d0,(a0)+

	add.l	#BPWIDTH,d4

	addq.w	#1,d5
	cmp.w	#SCROLL_LINES,d5
	blt.w	.scrollLine

	; End copper list
	move.l	#$FFFFFFFE,(a0)+

	movem.l	(sp)+,d2-d7/a2-a5
	rts

;==========================================================
; ApplyFlash — bias color in d0 toward white
; d7 = flash amount (low nibble used as per-channel add)
; d0 = color in/out (Amiga $0RGB)
; Trashes: d1
;==========================================================
ApplyFlash:
	; Extract and add each nibble separately to avoid carries
	; d7 = $222 * timer, so each nibble is the same value
	move.w	d0,d1		; save original

	; Red nibble
	move.w	d1,d0
	lsr.w	#8,d0
	and.w	#$F,d0
	add.w	d7,d0		; d7 low nibble = flash per channel
	cmp.w	#$F,d0
	ble.s	.rOk
	moveq	#$F,d0
.rOk:	lsl.w	#8,d0
	move.w	d0,-(sp)	; save R on stack

	; Green nibble
	move.w	d1,d0
	lsr.w	#4,d0
	and.w	#$F,d0
	add.w	d7,d0
	cmp.w	#$F,d0
	ble.s	.gOk
	moveq	#$F,d0
.gOk:	lsl.w	#4,d0
	or.w	(sp)+,d0
	move.w	d0,-(sp)	; save R+G

	; Blue nibble
	move.w	d1,d0
	and.w	#$F,d0
	add.w	d7,d0
	cmp.w	#$F,d0
	ble.s	.bOk
	moveq	#$F,d0
.bOk:	or.w	(sp)+,d0
	rts

;==========================================================
; Data includes
;==========================================================
	include	"tables.i"

;==========================================================
; Scroll text
;==========================================================
scroll_text:
	dc.b	"    DEWDZKI-FXP  ...  PART II  ...  "
	dc.b	"MUSIC: STUPEFACIENT BY XTD / MYSTIC & TRSI  ...  "
	dc.b	"CODE: DEWDZKI  ...  "
	dc.b	"GREETINGS TO ALL AMIGA SCENERS  ...  "
	dc.b	"KEEPING THE DREAM ALIVE  ...       "
	dc.b	$FF
	even

;==========================================================
; MOD player
;==========================================================
ENABLE_VUMETER	equ	1
	include	"ptplayer_minimal.asm"

	even
moddata:
	incbin	"part2.mod"
