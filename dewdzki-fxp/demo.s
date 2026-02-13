;----------------------------------------------------------
; DEWDZKI-FXP Main Demo
; Loaded at $49000 by bootblock trackloader
; OCS A500 PAL — 2 bitplanes, 384px wide
; Effects: copper terrain logo, raster bars, sine-wave scroll, starfield
;----------------------------------------------------------

; Memory map (chip RAM)
COPPER_A	equ	$40000		; copper list front buffer (6 KB)
COPPER_B	equ	$41800		; copper list back buffer (6 KB)
BPLANE1		equ	$43000		; bitplane 1: logo (48*256 = 12 KB)
BPLANE2		equ	$46000		; bitplane 2: stars (48*256 = 12 KB)

; Scroll buffer in free chip RAM
SCROLL_BUF	equ	$3E800		; scroll text buffer (48*64 = 3072 bytes)
; Star array in free chip RAM before buffers
STAR_ARRAY	equ	$3FB50		; 120*10 = 1200 bytes

; Display parameters
BPWIDTH		equ	48		; bytes per line (384 pixels)
BPHEIGHT	equ	256		; lines
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
STAR_SIZE	equ	10		; bytes per star entry
MIN_Z		equ	5
MAX_Z		equ	255
Z_SPEED		equ	2
ENERGY_DECAY	equ	2		; decay per frame (~0.64s full decay)
MIN_ZSPEED	equ	1		; Z speed when silent
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

; Scroll text constants
SCROLL_ZONE_Y	equ	192		; first scanline of scroll zone
SCROLL_ZONE_H	equ	64		; lines in scroll zone
SCROLL_BUF_W	equ	48		; bytes per line (same as main)
FONT_CHAR_H	equ	32		; pixels tall per character
FONT_CHAR_SIZE	equ	2*FONT_CHAR_H	; 64 bytes per character
FONT_FIRST	equ	32		; first ASCII code in font table
FONT_LAST	equ	90		; last ASCII code ('Z')
SINE_AMP	equ	16		; ±16 pixels vertical bounce
SCROLL_SPEED	equ	2		; pixels per frame

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

	; Clear scroll buffer
	lea	SCROLL_BUF,a0
	move.w	#(SCROLL_BUF_W*SCROLL_ZONE_H/4)-1,d0
.clrs:	clr.l	(a0)+
	dbf	d0,.clrs

	; Init stars
	bsr.w	InitStars
	bsr.w	InitScroll

	; Init shape cycling state
	clr.w	effect_state
	clr.w	shape_index
	clr.w	display_timer
	clr.w	morph_counter
	clr.w	angle_y
	clr.w	angle_x

	; Build initial copper lists in both buffers
	lea	COPPER_A,a0
	bsr.w	BuildCopper
	lea	COPPER_B,a0
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

	; Install CIA-B interrupt for music playback
	sub.l	a0,a0			; VectorBase = 0 (68000, no VBR)
	moveq	#1,d0			; PAL flag
	bsr	_mt_install_cia

	; Init module
	lea	moddata,a0		; absolute (too far for PC-relative)
	sub.l	a1,a1			; samples embedded in module
	moveq	#0,d0			; start at song position 0
	bsr	_mt_init

	; Enable playback (mt_init falls through to mt_end which clears this)
	st	_mt_Enable		; set to $FF (non-zero = enabled)

	; Enable master interrupt (CIA handler needs it)
	move.w	#$C000,INTENA(a6)	; SET + INTEN

;==========================================================
; Main loop
;==========================================================
MainLoop:
	bsr.w	WaitVBL
	bsr.w	UpdateMusicEnergy
	bsr.w	CheckKeyboard

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

	; Render scroll text into buffer
	bsr.w	RenderScrollText

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
	addq.w	#1,terrain_phase1
	and.w	#$FF,terrain_phase1
	addq.w	#3,terrain_phase2
	and.w	#$FF,terrain_phase2

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
music_energy:	dc.w	0		; attack/decay envelope tracking channel (0-64)
current_zspeed:	dc.w	2		; computed Z speed this frame (1-9)
current_base_z:	dc.w	SHAPE_BASE_Z	; dynamic base Z for shape modes
active_channel:	dc.w	0		; which MOD channel to track (0-3)
channel_vol_off: dc.w	mt_chan1+n_volume ; precomputed offset into mt_data
last_rawkey:	dc.b	0		; last raw keyboard byte (edge detect)
	even
rot_sin_y:	dc.w	0
rot_cos_y:	dc.w	0
rot_sin_x:	dc.w	0
rot_cos_x:	dc.w	0
channel_energies: dc.w	0,0,0,0		; per-channel attack/decay (0-64)
terrain_phase1:	dc.w	0		; slow vertical wave phase
terrain_phase2:	dc.w	0		; fast vertical ripple phase
scroll_offset:	dc.w	0		; pixel scroll position
scroll_phase:	dc.w	0		; sine wave phase for bounce
scroll_text_len: dc.w	0		; computed at init

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
; UpdateMusicEnergy — read all 4 channel volumes, attack/decay
; Drives terrain (ch0/ch1), starfield from active_channel
; Trashes: d0, d1, d3, a0-a2
;==========================================================
UpdateMusicEnergy:
	; --- Update all 4 channel energies ---
	lea	mt_data,a0
	lea	channel_energies(pc),a1
	lea	ChannelVolumeOffsets(pc),a2
	moveq	#3,d3
.chLoop:
	move.w	(a2)+,d0
	move.w	0(a0,d0.w),d0		; channel volume (0-64)
	move.w	(a1),d1			; current energy
	cmp.w	d1,d0
	bgt.s	.chAtt
	subq.w	#ENERGY_DECAY,d1
	bpl.s	.chSto
	moveq	#0,d1
	bra.s	.chSto
.chAtt:	move.w	d0,d1
.chSto:	move.w	d1,(a1)+
	dbf	d3,.chLoop
	; --- Starfield drive from active channel ---
	move.w	active_channel(pc),d0
	add.w	d0,d0
	lea	channel_energies(pc),a1
	move.w	0(a1,d0.w),d1
	move.w	d1,music_energy
	; z_speed = MIN_ZSPEED + (energy >> 3)  ->  1..9
	move.w	d1,d0
	lsr.w	#3,d0
	addq.w	#MIN_ZSPEED,d0
	move.w	d0,current_zspeed
	; base_z = SHAPE_BASE_Z - energy  ->  160..96
	move.w	#SHAPE_BASE_Z,d0
	sub.w	d1,d0
	move.w	d0,current_base_z
	rts

;==========================================================
; CheckKeyboard — spacebar cycles active MOD channel (0-3)
; Trashes: d0, a0
;==========================================================
CIAA_SDR	equ	$BFEC01
CIAA_CRA	equ	$BFEE01
KEY_SPACE	equ	$40
KEY_S		equ	$21

CheckKeyboard:
	move.b	CIAA_SDR,d0
	cmp.b	last_rawkey(pc),d0
	beq.s	.ckDone
	move.b	d0,last_rawkey
	; Decode: invert and rotate
	not.b	d0
	ror.b	#1,d0
	btst	#7,d0			; key-up?
	bne.s	.ckHandshake		; ignore releases, just handshake
	cmp.b	#KEY_S,d0
	bne.s	.notS
	; S pressed — cycle to next shape
	bsr.w	StartNextMorph
	bra.s	.ckHandshake
.notS:
	cmp.b	#KEY_SPACE,d0
	bne.s	.ckHandshake
	; Spacebar pressed — cycle channel
	move.w	active_channel(pc),d0
	addq.w	#1,d0
	and.w	#3,d0
	move.w	d0,active_channel
	add.w	d0,d0
	lea	ChannelVolumeOffsets(pc),a0
	move.w	0(a0,d0.w),d0
	move.w	d0,channel_vol_off
.ckHandshake:
	or.b	#$40,CIAA_CRA		; start handshake
	or.b	#$40,CIAA_CRA		; redundant write = delay
	and.b	#$BF,CIAA_CRA		; end handshake
.ckDone:
	rts

ChannelVolumeOffsets:
	dc.w	mt_chan1+n_volume
	dc.w	mt_chan2+n_volume
	dc.w	mt_chan3+n_volume
	dc.w	mt_chan4+n_volume

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
; EraseStar — clear old pixel for star at (a2)
; In:  a0=BPLANE2, a2=star ptr, a4=yoffset_table
; Out: —
; Trashes: d0-d2, d4
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
; ProjectAndPlot — bounds check, plot pixel, store OPX/OPY
; In:  d0=screen_x, d1=screen_y
;      a0=BPLANE2, a2=star ptr, a4=yoffset_table
; Out: d0 = 0 if OOB, non-zero if plotted
; Trashes: d0, d2, d4
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
	; In bounds — plot
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
	bsr.w	EraseStar

	; Advance Z
	move.w	STAR_SZ(a2),d0
	sub.w	current_zspeed(pc),d0
	cmp.w	#MIN_Z,d0
	ble.s	.respawn
	move.w	d0,STAR_SZ(a2)

	; Perspective projection
	move.w	STAR_SZ(a2),d0
	add.w	d0,d0			; word index into recip table
	move.w	0(a3,d0.w),d1		; d1 = recip[sz]

	move.w	d1,d3			; save recip for Y axis
	move.w	STAR_SX(a2),d0
	muls	d1,d0
	asr.l	#8,d0
	add.w	d5,d0			; d0 = screen X

	move.w	STAR_SY(a2),d1
	muls	d3,d1
	asr.l	#8,d1
	add.w	d6,d1			; d1 = screen Y

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
; ShapeStars — rotate stars toward/at shape targets
; Handles both EFFECT_MORPH (easing) and EFFECT_DISPLAY (direct)
; Uses rot_sin/cos variables instead of stack for sin/cos cache
;==========================================================
ShapeStars:
	movem.l	d2-d7/a2-a5,-(sp)

	lea	BPLANE2,a0
	lea	recip_table(pc),a3
	lea	yoffset_table(pc),a4
	lea	STAR_ARRAY,a2
	lea	signed_sine_table(pc),a5

	; Compute sin/cos for Y rotation → variables
	move.w	angle_y(pc),d0
	and.w	#$FF,d0
	add.w	d0,d0
	move.w	0(a5,d0.w),rot_sin_y
	move.w	angle_y(pc),d0
	add.w	#64,d0
	and.w	#$FF,d0
	add.w	d0,d0
	move.w	0(a5,d0.w),rot_cos_y

	; Compute sin/cos for X rotation → variables
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

	; Branch: morph (ease) or display (direct read)
	move.w	effect_state(pc),d0
	cmp.w	#EFFECT_MORPH,d0
	beq.s	.ease

	; --- Direct read (display mode) ---
	move.w	(a1),d0
	move.w	2(a1),d1
	move.w	4(a1),d2
	bra.s	.rotate

.ease:
	; --- Exponential ease (morph mode) ---
	; sx += (target_x - sx) >> 3
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
	; 3D rotation: Y then X
	; Y rotation: x' = (x*cos_y + z*sin_y) >> 7
	;             z' = (z*cos_y - x*sin_y) >> 7
	move.w	d0,d3			; save x
	move.w	d2,d4			; save z
	muls	rot_cos_y(pc),d0	; x * cos_y
	muls	rot_sin_y(pc),d4	; z * sin_y
	add.l	d4,d0
	asr.l	#7,d0			; d0 = x'
	move.w	d2,d4			; z
	muls	rot_cos_y(pc),d4	; z * cos_y
	muls	rot_sin_y(pc),d3	; x * sin_y
	sub.l	d3,d4
	asr.l	#7,d4			; d4 = z'

	; X rotation: y' = (y*cos_x - z'*sin_x) >> 7
	;             z'' = (y*sin_x + z'*cos_x) >> 7
	move.w	d1,d3			; save y
	move.w	d4,d5			; save z'
	muls	rot_cos_x(pc),d1	; y * cos_x
	muls	rot_sin_x(pc),d5	; z' * sin_x
	sub.l	d5,d1
	asr.l	#7,d1			; d1 = y'
	move.w	d3,d5			; y
	muls	rot_sin_x(pc),d5	; y * sin_x
	muls	rot_cos_x(pc),d4	; z' * cos_x
	add.l	d5,d4
	asr.l	#7,d4			; d4 = z''

	; Add base Z for projection
	add.w	current_base_z(pc),d4

	; Project: recip lookup
	cmp.w	#MIN_Z,d4
	ble.s	.skipPlot
	cmp.w	#255,d4
	ble.s	.zOk
	move.w	#255,d4
.zOk:
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

	bsr.w	ProjectAndPlot
	bra.s	.nextStar

.skipPlot:
	move.w	#$FFFF,STAR_OPX(a2)

.nextStar:
	lea	6(a1),a1
	lea	STAR_SIZE(a2),a2
	dbf	d7,.starLoop

	; Advance angles
	addq.w	#2,angle_y
	and.w	#$FF,angle_y
	addq.w	#1,angle_x
	and.w	#$FF,angle_x

	; Check morph completion (only in morph state)
	move.w	effect_state(pc),d0
	cmp.w	#EFFECT_MORPH,d0
	bne.s	.done
	addq.w	#1,morph_counter
	move.w	morph_counter(pc),d0
	cmp.w	#MORPH_DURATION,d0
	blt.s	.done
	; Morph complete — decide next state
	move.w	shape_index(pc),d0
	tst.w	d0
	bne.s	.toDisplay
	; shape_index 0 = tunnel
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
; InitScroll — compute scroll text length
;==========================================================
InitScroll:
	lea	scroll_text(pc),a0
	moveq	#0,d0
.len:	tst.b	(a0)+
	beq.s	.lenDone
	addq.w	#1,d0
	bra.s	.len
.lenDone:
	move.w	d0,scroll_text_len
	clr.w	scroll_offset
	clr.w	scroll_phase
	rts

;==========================================================
; RenderScrollText — draw sine-bouncing scroll text
; Clears SCROLL_BUF, renders 24 characters with sine Y offset
; Advances scroll_offset and scroll_phase each frame
; Trashes: d0-d7, a0-a5
;==========================================================
RenderScrollText:
	movem.l	d2-d7/a2-a5,-(sp)

	; Clear scroll buffer (48*64 = 3072 bytes = 768 longs)
	lea	SCROLL_BUF,a0
	move.w	#(SCROLL_BUF_W*SCROLL_ZONE_H/4)-1,d0
.clr:	clr.l	(a0)+
	dbf	d0,.clr

	; Compute first visible character index
	move.w	scroll_offset(pc),d7
	lsr.w	#4,d7			; d7 = scroll_char (first visible)

	lea	signed_sine_table(pc),a5
	lea	yoffset_table(pc),a4

	moveq	#23,d5			; 24 character positions
	moveq	#0,d4			; position counter (0..23)

.charLoop:
	; Compute text index with wrapping
	move.w	d7,d0
	add.w	d4,d0			; char_index = scroll_char + position
.modWrap:
	cmp.w	scroll_text_len(pc),d0
	blt.s	.modDone
	sub.w	scroll_text_len(pc),d0
	bra.s	.modWrap
.modDone:

	; Fetch ASCII character
	lea	scroll_text(pc),a1
	moveq	#0,d6
	move.b	0(a1,d0.w),d6		; d6 = ASCII code
	beq.s	.skipChar		; should not happen after wrap, but safety

	; Font lookup: (ascii - FONT_FIRST) * FONT_CHAR_SIZE
	sub.w	#FONT_FIRST,d6
	bmi.s	.skipChar
	cmp.w	#FONT_LAST-FONT_FIRST,d6
	bgt.s	.skipChar
	lsl.w	#6,d6			; * 64 = FONT_CHAR_SIZE
	lea	font_data,a2
	add.w	d6,a2			; a2 = font bitmap for this char

	; Compute sine Y offset for this character position
	move.w	d4,d0
	lsl.w	#3,d0			; * 8 (spread sine across chars)
	add.w	scroll_phase(pc),d0
	and.w	#$FF,d0
	add.w	d0,d0			; word index
	move.w	0(a5,d0.w),d0		; signed sine -127..+127
	muls	#SINE_AMP,d0
	asr.l	#7,d0
	add.w	#SINE_AMP,d0		; 0..2*SINE_AMP = 0..32

	; Destination = SCROLL_BUF + y_off * 48 + char_pos * 2
	add.w	d0,d0			; word index for yoffset table
	move.w	0(a4,d0.w),d3		; y * 48
	move.w	d4,d0
	add.w	d0,d0			; char_pos * 2 bytes
	add.w	d0,d3			; d3 = byte offset in buffer
	lea	SCROLL_BUF,a1
	add.w	d3,a1			; a1 = destination ptr

	; Copy 32 rows of 2 bytes (16 pixels wide)
	moveq	#FONT_CHAR_H-1,d3
.rowCopy:
	move.w	(a2)+,(a1)
	lea	SCROLL_BUF_W(a1),a1	; next row in buffer
	dbf	d3,.rowCopy

.skipChar:
	addq.w	#1,d4
	dbf	d5,.charLoop

	; Advance scroll offset
	move.w	scroll_offset(pc),d0
	add.w	#SCROLL_SPEED,d0
	; Wrap when past text length in pixels
	move.w	scroll_text_len(pc),d1
	lsl.w	#4,d1			; text_len * 16 pixels
	cmp.w	d1,d0
	blt.s	.noWrap
	sub.w	d1,d0
.noWrap:
	move.w	d0,scroll_offset
	; Advance sine phase
	addq.w	#3,scroll_phase
	and.w	#$FF,scroll_phase

	movem.l	(sp)+,d2-d7/a2-a5
	rts

;==========================================================
; BuildCopper — build copper list at (a0)
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
	lea	yoffset_table(pc),a4
	lea	signed_sine_table(pc),a5
	move.w	gradient_phase(pc),d2
	move.w	sine_phase(pc),d3

	moveq	#0,d4			; line counter
	move.w	#$2C,d5			; starting VPOS
	moveq	#0,d1			; V8 barrier flag

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

	; --- Vertical terrain displacement ---
	; Wave 1: signed_sine[(line + phase1) & 255] * ch_energy[0] >> 8
	move.w	d4,d0
	add.w	terrain_phase1(pc),d0
	and.w	#$FF,d0
	add.w	d0,d0			; word index
	move.w	0(a5,d0.w),d0		; signed sine (-127..+127)
	muls	channel_energies(pc),d0	; * ch0 energy (0-64)
	asr.l	#8,d0			; ±31 max
	move.w	d0,d6			; d6 = wave1

	; Wave 2: signed_sine[(line*2 + phase2) & 255] * ch_energy[1] >> 9
	move.w	d4,d0
	add.w	d4,d0			; line * 2
	add.w	terrain_phase2(pc),d0
	and.w	#$FF,d0
	add.w	d0,d0
	move.w	0(a5,d0.w),d0
	muls	channel_energies+2(pc),d0 ; * ch1 energy
	asr.l	#8,d0
	asr.w	#1,d0			; ±15 max
	add.w	d0,d6			; d6 = total vert_offset

	; Source row = clamp(line + vert_offset, 0, 255)
	move.w	d4,d0
	add.w	d6,d0
	bpl.s	.vNotNeg
	moveq	#0,d0
	bra.s	.vClamped
.vNotNeg:
	cmp.w	#255,d0
	ble.s	.vClamped
	move.w	#255,d0
.vClamped:
	; BPL1 row base from yoffset table
	add.w	d0,d0			; word index
	moveq	#0,d7
	move.w	0(a4,d0.w),d7		; src_row * BPWIDTH
	add.l	#BPLANE1,d7		; d7 = BPL1 base for displaced row

	; --- Horizontal sine scroll ---
	; d3 = sine_phase (will be temporarily trashed, reloaded at end)
	move.w	d4,d0
	add.w	d3,d0
	and.w	#$FF,d0
	moveq	#0,d6
	move.b	0(a2,d0.w),d6		; d6 = sine value 0-48

	; Decompose into fine scroll + coarse byte offset
	move.w	d6,d0			; d0 = sine_val copy
	neg.w	d0
	and.w	#15,d0			; d0 = neg_fine = BPLCON1 value
	add.w	d0,d6			; round up to next multiple of 16
	lsr.w	#4,d6			; coarse words
	add.w	d6,d6			; coarse bytes
	ext.l	d6

	; BPL1PT = displaced row base + coarse horizontal offset
	move.l	d7,d3			; d3 = displaced row base
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

	addq.w	#1,d4
	addq.w	#1,d5
	cmp.w	#SCROLL_ZONE_Y,d4
	blt.w	.perLine

	; ============================================
	; Scroll zone transition at line 192 (VPOS $EC)
	; ============================================

	; WAIT for scroll zone start
	move.w	d5,d0
	and.w	#$FF,d0
	lsl.w	#8,d0
	or.w	#$07,d0
	move.w	d0,(a0)+
	move.w	#$FFFE,(a0)+

	; Switch to 1 bitplane (hides stars in scroll zone)
	move.l	#(BPLCON0<<16)|$1200,(a0)+

	; BPL1PT = SCROLL_BUF + coarse scroll offset
	move.w	scroll_offset(pc),d0
	and.w	#15,d0			; fine pixel
	move.w	d0,d6			; save fine_pixel
	neg.w	d0
	and.w	#15,d0			; bplcon1 value
	move.w	d0,d7			; save bplcon1 val

	; coarse = 0 when fine_pixel==0, else 2
	moveq	#0,d3
	tst.w	d6
	beq.s	.noCoarse
	moveq	#2,d3
.noCoarse:
	move.l	#SCROLL_BUF,d0
	add.l	d3,d0			; BPL1PT = SCROLL_BUF + coarse

	move.w	#BPL1PTH,(a0)+
	swap	d0
	move.w	d0,(a0)+
	move.w	#BPL1PTL,(a0)+
	swap	d0
	move.w	d0,(a0)+

	; BPLCON1 = scroll fine value
	move.w	#BPLCON1,(a0)+
	move.w	d7,(a0)+

	; Scroll zone background color
	move.l	#(COLOR00<<16)|$0112,(a0)+

	; Advance past transition line
	addq.w	#1,d4
	addq.w	#1,d5

	; --- Scroll zone per-line: WAIT + COLOR01 gradient ---
.scrollLine:
	; V8 barrier (VPOS $100 = line 212)
	tst.w	d1
	bne.s	.sPastBar
	cmp.w	#$100,d5
	blt.s	.sNoBar
	move.l	#$FFDFFFFE,(a0)+
	moveq	#1,d1
.sNoBar:
.sPastBar:
	; WAIT
	move.w	d5,d0
	and.w	#$FF,d0
	lsl.w	#8,d0
	or.w	#$07,d0
	move.w	d0,(a0)+
	move.w	#$FFFE,(a0)+

	; COLOR01 = gradient for scroll text (offset +128 for contrast)
	move.w	d4,d0
	add.w	d2,d0
	add.w	#128,d0			; phase offset from logo gradient
	and.w	#$FF,d0
	add.w	d0,d0
	move.w	#COLOR01,(a0)+
	move.w	0(a3,d0.w),(a0)+

	addq.w	#1,d4
	addq.w	#1,d5
	cmp.w	#BPHEIGHT,d4
	blt.w	.scrollLine

	; End
	move.l	#$FFFFFFFE,(a0)+

	movem.l	(sp)+,d2-d7/a2-a5
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
scroll_text:
	dc.b	"DEWDZKI WILL NEVER DIE - GREETZ GO OUT TO"
	dc.b	" - SICK0 - ZZZ - RIZZ - ZOCH - FERRE070"
	dc.b	" - TEDY - REDBARON -- WE WILL NEVER FORGET"
	dc.b	" KOPYKATZ, ROSEVALLEY AND OTHER FXP"
	dc.b	" ADVENTURES, I'LL UPLOAD THIS TO THE"
	dc.b	" INTERNET ARCHIVE - DEWZKI WILL NEVER"
	dc.b	" DIE.......          "
	dc.b	0
	even

LogoBitmap:
	incbin	"logo.bin"

	include	"tables.i"

;==========================================================
; MOD player and music data
;==========================================================
	include	"ptplayer_minimal.asm"

	even
moddata:
	incbin	"trance.mod"
