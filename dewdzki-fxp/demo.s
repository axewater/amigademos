;----------------------------------------------------------
; DEWDZKI-FXP Main Demo
; Loaded at $49000 by bootblock trackloader
; OCS A500 PAL — 2 bitplanes, 384px wide
; Effects: copper raster bars, sine-wave logo, shape-cycling starfield
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

; Shape rotation
SHAPE_BASE_Z	equ	160		; Z offset for projection

; Effect states
EFFECT_TUNNEL	equ	0
EFFECT_MORPH	equ	1
EFFECT_DISPLAY	equ	2

; Shape cycling
NUM_SHAPES	equ	4
SHAPE_DISPLAY_TIME equ	250		; 5 seconds at 50fps
MORPH_DURATION	equ	128		; frames to morph

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

	; Init shape cycling state
	clr.w	effect_state
	clr.w	shape_index
	clr.w	display_timer
	clr.w	morph_counter
	clr.w	angle_y
	clr.w	angle_x

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
	bsr.w	MorphStars
	bra.s	.stateEnd
.doDisplay:
	bsr.w	DisplayShape
	addq.w	#1,display_timer
	move.w	display_timer(pc),d0
	cmp.w	#SHAPE_DISPLAY_TIME,d0
	blt.s	.stateEnd
	bsr.w	StartNextMorph
.stateEnd:

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

	bra.w	MainLoop

;==========================================================
; Variables
;==========================================================
cop_front:	dc.l	COPPER_A
cop_back:	dc.l	COPPER_B
gradient_phase:	dc.w	0
sine_phase:	dc.w	0
orbit_phase:	dc.w	0
lfsr_state:	dc.w	$ACE1
effect_state:	dc.w	0
morph_counter:	dc.w	0
angle_y:	dc.w	0
angle_x:	dc.w	0
shape_index:	dc.w	0		; 0=tunnel, 1=cube, 2=sphere, 3=pyramid
display_timer:	dc.w	0		; frames in current display/tunnel
current_targets: dc.l	0		; pointer to active target array

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
; StartNextMorph — advance to next shape and begin morph
;==========================================================
StartNextMorph:
	; Advance shape_index cyclically
	move.w	shape_index(pc),d0
	addq.w	#1,d0
	cmp.w	#NUM_SHAPES,d0
	blt.s	.noWrap
	clr.w	d0
.noWrap:
	move.w	d0,shape_index
	; Look up target pointer
	add.w	d0,d0
	add.w	d0,d0			; x4 for longword index
	lea	TargetPtrTable(pc),a0
	move.l	0(a0,d0.w),current_targets
	; Enter morph state
	move.w	#EFFECT_MORPH,effect_state
	clr.w	morph_counter
	clr.w	display_timer
	rts

;==========================================================
; Target pointer lookup table (shape_index 0..3)
;==========================================================
TargetPtrTable:
	dc.l	tunnel_targets		; shape 0 = tunnel
	dc.l	cube_targets		; shape 1 = cube
	dc.l	sphere_targets		; shape 2 = sphere
	dc.l	pyramid_targets		; shape 3 = pyramid

;==========================================================
; MorphStars — ease stars toward current targets, rotate, project
; Uses exponential ease: pos += (target - pos) >> 3
;==========================================================
MorphStars:
	movem.l	d2-d7/a2-a5,-(sp)

	lea	BPLANE2,a0
	lea	recip_table(pc),a3
	lea	yoffset_table(pc),a4
	lea	STAR_ARRAY,a2
	lea	signed_sine_table(pc),a5

	; Cache sin/cos for Y and X rotation on stack
	move.w	angle_y(pc),d0
	and.w	#$FF,d0
	add.w	d0,d0
	move.w	0(a5,d0.w),d1		; sin_y
	move.w	angle_y(pc),d0
	add.w	#64,d0
	and.w	#$FF,d0
	add.w	d0,d0
	move.w	0(a5,d0.w),d2		; cos_y

	move.w	angle_x(pc),d0
	and.w	#$FF,d0
	add.w	d0,d0
	move.w	0(a5,d0.w),d3		; sin_x
	move.w	angle_x(pc),d0
	add.w	#64,d0
	and.w	#$FF,d0
	add.w	d0,d0
	move.w	0(a5,d0.w),d4		; cos_x

	; Push sin/cos values: sin_y, cos_y, sin_x, cos_x
	move.w	d4,-(sp)		; cos_x [0(sp)]
	move.w	d3,-(sp)		; sin_x [2(sp)]
	move.w	d2,-(sp)		; cos_y [4(sp)]
	move.w	d1,-(sp)		; sin_y [6(sp)]

	move.l	current_targets(pc),a1

	move.w	#NUM_STARS-1,d7

.mstarLoop:
	; ---- 1. Erase old pixel ----
	move.w	STAR_OPX(a2),d0
	cmp.w	#$FFFF,d0
	beq.s	.mskipErase
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
.mskipErase:

	; ---- 2. Ease toward target ----
	; sx += (target_x - sx) >> 3
	move.w	(a1),d0			; target_x
	sub.w	STAR_SX(a2),d0
	asr.w	#3,d0
	add.w	d0,STAR_SX(a2)

	move.w	2(a1),d0		; target_y
	sub.w	STAR_SY(a2),d0
	asr.w	#3,d0
	add.w	d0,STAR_SY(a2)

	move.w	4(a1),d0		; target_z
	sub.w	STAR_SZ(a2),d0
	asr.w	#3,d0
	add.w	d0,STAR_SZ(a2)

	; ---- 3. 3D rotate (Y then X) and project ----
	move.w	STAR_SX(a2),d0		; x
	move.w	STAR_SY(a2),d1		; y
	move.w	STAR_SZ(a2),d2		; z

	; Y rotation: x' = (x*cos_y + z*sin_y) >> 7
	;             z' = (z*cos_y - x*sin_y) >> 7
	move.w	d0,d3			; save x
	move.w	d2,d4			; save z
	muls	4(sp),d0		; x * cos_y
	muls	6(sp),d4		; z * sin_y
	add.l	d4,d0
	asr.l	#7,d0			; d0 = x'
	move.w	d2,d4			; z
	muls	4(sp),d4		; z * cos_y
	muls	6(sp),d3		; x * sin_y
	sub.l	d3,d4
	asr.l	#7,d4			; d4 = z'

	; X rotation: y' = (y*cos_x - z'*sin_x) >> 7
	;             z'' = (y*sin_x + z'*cos_x) >> 7
	move.w	d1,d3			; save y
	move.w	d4,d5			; save z'
	muls	0(sp),d1		; y * cos_x
	muls	2(sp),d5		; z' * sin_x
	sub.l	d5,d1
	asr.l	#7,d1			; d1 = y'
	move.w	d3,d5			; y
	muls	2(sp),d5		; y * sin_x
	muls	0(sp),d4		; z' * cos_x
	add.l	d5,d4
	asr.l	#7,d4			; d4 = z''

	; Add base Z for projection
	add.w	#SHAPE_BASE_Z,d4

	; Project: recip lookup
	cmp.w	#MIN_Z,d4
	ble.w	.mskipPlot
	cmp.w	#255,d4
	ble.s	.mzOk
	move.w	#255,d4
.mzOk:
	move.w	d4,d5
	add.w	d5,d5
	move.w	0(a3,d5.w),d5		; recip[z]

	; screen_x = (x' * recip) >> 8 + CENTER_X
	muls	d5,d0
	asr.l	#8,d0
	add.w	#CENTER_X,d0

	; screen_y = (y' * recip) >> 8 + CENTER_Y
	muls	d5,d1
	asr.l	#8,d1
	add.w	#CENTER_Y,d1

	; Bounds check
	tst.w	d0
	blt.s	.mskipPlot
	cmp.w	#383,d0
	bgt.s	.mskipPlot
	tst.w	d1
	blt.s	.mskipPlot
	cmp.w	#255,d1
	bgt.s	.mskipPlot

	; ---- 4. Plot pixel ----
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
	bra.s	.mnextStar

.mskipPlot:
	move.w	#$FFFF,STAR_OPX(a2)

.mnextStar:
	lea	6(a1),a1		; next cube target
	lea	STAR_SIZE(a2),a2
	dbf	d7,.mstarLoop

	; Clean up stack (4 words)
	addq.l	#8,sp

	; Advance angles
	addq.w	#2,angle_y
	and.w	#$FF,angle_y
	addq.w	#1,angle_x
	and.w	#$FF,angle_x

	; Check morph completion
	addq.w	#1,morph_counter
	move.w	morph_counter(pc),d0
	cmp.w	#MORPH_DURATION,d0
	blt.s	.morphNotDone
	; Morph complete — decide next state
	move.w	shape_index(pc),d0
	tst.w	d0
	bne.s	.toDisplay
	; shape_index 0 = tunnel
	move.w	#EFFECT_TUNNEL,effect_state
	clr.w	display_timer
	bra.s	.morphNotDone
.toDisplay:
	move.w	#EFFECT_DISPLAY,effect_state
	clr.w	display_timer
.morphNotDone:

	movem.l	(sp)+,d2-d7/a2-a5
	rts

;==========================================================
; DisplayShape — rotate shape targets, project, plot
; Pure rotation mode (no easing, positions = current targets)
;==========================================================
DisplayShape:
	movem.l	d2-d7/a2-a5,-(sp)

	lea	BPLANE2,a0
	lea	recip_table(pc),a3
	lea	yoffset_table(pc),a4
	lea	STAR_ARRAY,a2
	lea	signed_sine_table(pc),a5

	; Cache sin/cos for Y and X rotation on stack
	move.w	angle_y(pc),d0
	and.w	#$FF,d0
	add.w	d0,d0
	move.w	0(a5,d0.w),d1		; sin_y
	move.w	angle_y(pc),d0
	add.w	#64,d0
	and.w	#$FF,d0
	add.w	d0,d0
	move.w	0(a5,d0.w),d2		; cos_y

	move.w	angle_x(pc),d0
	and.w	#$FF,d0
	add.w	d0,d0
	move.w	0(a5,d0.w),d3		; sin_x
	move.w	angle_x(pc),d0
	add.w	#64,d0
	and.w	#$FF,d0
	add.w	d0,d0
	move.w	0(a5,d0.w),d4		; cos_x

	; Push sin/cos: sin_y, cos_y, sin_x, cos_x
	move.w	d4,-(sp)		; cos_x [0(sp)]
	move.w	d3,-(sp)		; sin_x [2(sp)]
	move.w	d2,-(sp)		; cos_y [4(sp)]
	move.w	d1,-(sp)		; sin_y [6(sp)]

	move.l	current_targets(pc),a1

	move.w	#NUM_STARS-1,d7

.cstarLoop:
	; ---- 1. Erase old pixel ----
	move.w	STAR_OPX(a2),d0
	cmp.w	#$FFFF,d0
	beq.s	.cskipErase
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
.cskipErase:

	; ---- 2. Read cube target directly ----
	move.w	(a1),d0			; x
	move.w	2(a1),d1		; y
	move.w	4(a1),d2		; z

	; ---- 3. 3D rotate (Y then X) and project ----
	; Y rotation
	move.w	d0,d3
	move.w	d2,d4
	muls	4(sp),d0		; x * cos_y
	muls	6(sp),d4		; z * sin_y
	add.l	d4,d0
	asr.l	#7,d0			; x'
	move.w	d2,d4
	muls	4(sp),d4		; z * cos_y
	muls	6(sp),d3		; x * sin_y
	sub.l	d3,d4
	asr.l	#7,d4			; z'

	; X rotation
	move.w	d1,d3
	move.w	d4,d5
	muls	0(sp),d1		; y * cos_x
	muls	2(sp),d5		; z' * sin_x
	sub.l	d5,d1
	asr.l	#7,d1			; y'
	move.w	d3,d5
	muls	2(sp),d5		; y * sin_x
	muls	0(sp),d4		; z' * cos_x
	add.l	d5,d4
	asr.l	#7,d4			; z''

	; Add base Z
	add.w	#SHAPE_BASE_Z,d4

	; Project
	cmp.w	#MIN_Z,d4
	ble.w	.cskipPlot
	cmp.w	#255,d4
	ble.s	.czOk
	move.w	#255,d4
.czOk:
	move.w	d4,d5
	add.w	d5,d5
	move.w	0(a3,d5.w),d5		; recip[z]

	muls	d5,d0
	asr.l	#8,d0
	add.w	#CENTER_X,d0

	muls	d5,d1
	asr.l	#8,d1
	add.w	#CENTER_Y,d1

	; Bounds check
	tst.w	d0
	blt.s	.cskipPlot
	cmp.w	#383,d0
	bgt.s	.cskipPlot
	tst.w	d1
	blt.s	.cskipPlot
	cmp.w	#255,d1
	bgt.s	.cskipPlot

	; ---- 4. Plot pixel ----
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
	bra.s	.cnextStar

.cskipPlot:
	move.w	#$FFFF,STAR_OPX(a2)

.cnextStar:
	lea	6(a1),a1
	lea	STAR_SIZE(a2),a2
	dbf	d7,.cstarLoop

	; Clean up stack
	addq.l	#8,sp

	; Advance angles
	addq.w	#2,angle_y
	and.w	#$FF,angle_y
	addq.w	#1,angle_x
	and.w	#$FF,angle_x

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
