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
; Bouncing logo sprites (2 attached pairs = 32px wide logo)
; Attached pair: spr1+spr0 share, spr3+spr2 share
; We use sprites 1,3 as independent bouncing logos
LOGO_SPR1	equ	$3B450		; sprite 1 data (left half of logo)
LOGO_SPR3	equ	$3B560		; sprite 3 data (right half of logo)
STAR_ARRAY	equ	$3B680		; 120*10 = 1200 bytes (moved down)
; Scratch buffers (in gap $3B900-$40000)
TUNNEL_COLORS	equ	$3BC00		; 108 words (216 bytes)
STAR_COLORS	equ	$3BCD8		; 216 words (432 bytes)
GLITCH_OFFSETS	equ	$3BE88		; 108 longs (432 bytes)
PROJECTED_VERTS	equ	$3C038		; 12 words (24 bytes)
BPL1_MIDDLE	equ	$40C00		; BPLANE1 + 64*48 (start of middle zone in BPL1)

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
SCROLL_BUF	equ	$46000		; after BPLANE2, before demo code ($49000)
SCROLL_BUF_STRIDE equ	256		; power of 2: row offset = font_row << 8
SCROLL_TEXT_LEN	equ	177		; chars in scroll_text (excluding $FF)
SCROLL_TOTAL_PX	equ	SCROLL_TEXT_LEN*8 ; 1416 pixels before wrap
SCROLL_SINE_SHIFT equ	1		; halve sine amplitude (max ~24 bytes)
SCROLL_SPEED	equ	1		; pixels per frame

; Beat flash
BEAT_THRESHOLD	equ	40
BEAT_JUMP	equ	20

; Bouncing logo sprite
BOUNCE_W	equ	16		; pixels wide per sprite
BOUNCE_H	equ	16		; pixels tall
BOUNCE_XMIN	equ	$41		; hardware H min (left border)
BOUNCE_XMAX	equ	$1B1		; hardware H max - 16px
BOUNCE_YMIN	equ	$2C		; top of display
BOUNCE_YMAX	equ	$FC		; bottom - 16 lines (stay above scroll)
BOUNCE_PULSE_DECAY equ	3	; how fast pulsation decays

; Middle zone BPL1 effect cycling
MID_EFFECT_TIME	equ	300		; frames per mid effect (6 sec)
MID_WAVEFORM	equ	0
MID_MOIRE	equ	1
MID_WIREFRAME	equ	2
NUM_MID_EFFECTS	equ	3
MIDDLE_LINES	equ	108		; lines in middle zone

; Blitter registers
DMACONR		equ	$002
BLTCON0		equ	$040
BLTCON1		equ	$042
BLTAFWM		equ	$044
BLTALWM		equ	$046
BLTAPTH		equ	$050
BLTAPTL		equ	$052
BLTDPTH		equ	$054
BLTDPTL		equ	$056
BLTSIZE		equ	$058
BLTBPTH		equ	$04C
BLTBPTL		equ	$04E
BLTAMOD		equ	$064
BLTBMOD		equ	$062
BLTDMOD		equ	$066

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

	; Init bouncing logo sprites
	bsr.w	InitBounceLogo

	; Init scrolltext (pre-render into SCROLL_BUF)
	bsr.w	PreRenderScroll
	clr.w	scroll_pixel_pos

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
; Main loop — new blitter pipeline with 6 effects
;==========================================================
MainLoop:
	bsr.w	WaitVBL

	; === Step 1: Start BPL2 clear (blitter runs in parallel) ===
	bsr.w	BlitClearBPL2

	; === Step 2: CPU work while BPL2 clears ===
	bsr.w	UpdateAllChannelEnergy
	bsr.w	DetectBeat
	bsr.w	UpdateEqualizer
	bsr.w	UpdateTunnelZoom
	bsr.w	UpdateRainbowStarColors
	bsr.w	PrepareGlitchOffsets

	; === Step 3: Wait for BPL2 clear ===
	bsr.w	WaitBlitter

	; === Step 4: Start BPL1 middle clear (blitter runs in parallel) ===
	bsr.w	BlitClearBPL1Middle

	; === Step 5: CPU work while BPL1 middle clears ===
	; Swap copper buffers
	move.l	cop_front(pc),d0
	move.l	cop_back(pc),d1
	move.l	d1,cop_front
	move.l	d0,cop_back
	move.l	cop_front(pc),COP1LCH(a6)
	move.w	COPJMP1(a6),d0

	; State dispatch: tunnel / morph / display (star plotting on BPL2)
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

	; === Step 6: Wait for BPL1 middle clear ===
	bsr.w	WaitBlitter

	; === Step 7: Middle zone BPL1 effect (waveform/moiré/wireframe) ===
	move.w	mid_effect(pc),d0
	cmp.w	#MID_MOIRE,d0
	beq.s	.doMoire
	cmp.w	#MID_WIREFRAME,d0
	beq.s	.doWire
	; Default: waveform (CPU)
	bsr.w	DrawWaveform
	bra.s	.midDone
.doMoire:
	bsr.w	BlitMoirePass1
	bsr.w	BlitMoirePass2
	bsr.w	WaitBlitter		; wait for moiré to finish
	bra.s	.midDone
.doWire:
	bsr.w	DrawWireframe
.midDone:

	; === Step 8: Start scroll blit (runs in parallel) ===
	; (BlitScrollLeft called from UpdateScrolltext)

	; Update bouncing logo sprites
	bsr.w	UpdateBounceLogo

	; Update scrolltext (includes blitter scroll)
	bsr.w	UpdateScrolltext

	; Build next frame into back buffer
	move.l	cop_back(pc),a0
	bsr.w	BuildCopper

	; === Advance animation phases ===
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

	; Advance new effect phases
	addq.w	#2,ring_phase
	and.w	#$FF,ring_phase

	; rainbow_star_phase speed driven by treble energy (ch2+ch3)
	lea	eq_energy(pc),a0
	move.w	4(a0),d0		; ch2
	add.w	6(a0),d0		; ch3
	lsr.w	#5,d0
	addq.w	#1,d0			; min speed 1
	add.w	d0,rainbow_star_phase
	and.w	#$FF,rainbow_star_phase

	; Advance wave phases (each channel at different speed)
	lea	wave_phases(pc),a0
	addq.w	#4,(a0)+
	addq.w	#3,(a0)+
	addq.w	#2,(a0)+
	addq.w	#1,(a0)+
	lea	wave_phases(pc),a0
	and.w	#$FF,(a0)+
	and.w	#$FF,(a0)+
	and.w	#$FF,(a0)+
	and.w	#$FF,(a0)+

	; Advance moiré shift (oscillate 1..15)
	move.w	moire_shift(pc),d0
	add.w	moire_dir(pc),d0
	cmp.w	#15,d0
	blt.s	.mshOk1
	move.w	#-1,moire_dir
	move.w	#15,d0
.mshOk1:
	cmp.w	#1,d0
	bge.s	.mshOk2
	move.w	#1,moire_dir
	moveq	#1,d0
.mshOk2:
	move.w	d0,moire_shift

	; Advance wireframe angle (speed driven by combined_energy)
	move.w	combined_energy(pc),d0
	lsr.w	#4,d0
	addq.w	#1,d0
	add.w	d0,wire_angle
	and.w	#$FF,wire_angle

	; Mid effect cycling (every MID_EFFECT_TIME frames)
	addq.w	#1,mid_effect_timer
	move.w	mid_effect_timer(pc),d0
	cmp.w	#MID_EFFECT_TIME,d0
	blt.s	.noMidSwitch
	clr.w	mid_effect_timer
	move.w	mid_effect(pc),d0
	addq.w	#1,d0
	cmp.w	#NUM_MID_EFFECTS,d0
	blt.s	.midNoWrap
	moveq	#0,d0
.midNoWrap:
	move.w	d0,mid_effect
.noMidSwitch:

	; Decrement flash timer
	move.w	flash_timer(pc),d0
	beq.s	.noFlashDec
	subq.w	#1,d0
	move.w	d0,flash_timer
.noFlashDec:

	; --- Check for demo end ---
	tst.b	_mt_SongEnd		; song looped?
	bne.w	EndDemo
	btst	#6,$BFE001		; left mouse button (active low)
	beq.w	EndDemo

	bra.w	MainLoop

;==========================================================
; EndDemo — fade to black and halt
;==========================================================
EndDemo:
	; Stop music
	bsr	_mt_end

	; Fade out over 16 frames
	moveq	#15,d7
.fadeLoop:
	bsr.w	WaitVBL
	move.l	cop_front(pc),a0
	bsr.w	FadeCopper
	move.l	cop_back(pc),a0
	bsr.w	FadeCopper
	dbf	d7,.fadeLoop

	; Kill everything
	move.w	#$7FFF,INTENA(a6)
	move.w	#$7FFF,INTREQ(a6)
	move.w	#$7FFF,DMACON(a6)
	move.w	#$0000,COLOR00(a6)

.halt:	bra.s	.halt

;==========================================================
; FadeCopper — decrement all color register values in copper list
; a0 = copper list pointer, a6 = CUSTOM base
;==========================================================
FadeCopper:
.scan:	move.w	(a0)+,d0		; register
	move.w	(a0)+,d1		; value
	cmp.w	#$FFFF,d0		; end of copper list?
	beq.s	.fdone
	btst	#0,d1			; WAIT instruction? (bit 0 of 2nd word)
	bne.s	.scan
	; Check if this is a color register ($0180-$01BE)
	cmp.w	#$0180,d0
	blt.s	.scan
	cmp.w	#$01BE,d0
	bgt.s	.scan
	; Fade: decrement each RGB nibble toward 0
	move.w	d1,d2
	and.w	#$0F00,d2
	beq.s	.rOk
	sub.w	#$0100,d1
.rOk:	move.w	d1,d2
	and.w	#$00F0,d2
	beq.s	.gOk
	sub.w	#$0010,d1
.gOk:	move.w	d1,d2
	and.w	#$000F,d2
	beq.s	.bOk
	subq.w	#1,d1
.bOk:	move.w	d1,-2(a0)		; write back faded value
	bra.s	.scan
.fdone:	rts

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
scroll_pixel_pos: dc.w	0
rot_sin_y:	dc.w	0
rot_cos_y:	dc.w	0
rot_sin_x:	dc.w	0
rot_cos_x:	dc.w	0
bar_phase:	dc.w	0
bar_y:		dc.w	153,153,153,153

; Bouncing logo 1 variables (hardware coords)
bounce_x:	dc.w	$80		; current X (hardware sprite H)
bounce_y:	dc.w	$60		; current Y (hardware sprite V)
bounce_dx:	dc.w	1		; X velocity (+1 or -1)
bounce_dy:	dc.w	1		; Y velocity (+1 or -1)
bounce_pulse:	dc.w	0		; pulsation intensity (0-15, decays)
; Bouncing logo 2 (sprite 3) — independent movement
bounce2_x:	dc.w	$120
bounce2_y:	dc.w	$90
bounce2_dx:	dc.w	-1
bounce2_dy:	dc.w	1

; New effect variables
mid_effect:	dc.w	0		; 0=waveform, 1=moiré, 2=wireframe
mid_effect_timer: dc.w	0		; frames in current effect
ring_phase:	dc.w	0		; tunnel zoom animation phase
rainbow_star_phase: dc.w 0		; per-line star color phase
wave_phases:	dc.w	0,0,0,0		; per-channel waveform phase
moire_shift:	dc.w	1		; current XOR shift (1-15)
moire_dir:	dc.w	1		; +1 or -1
wire_angle:	dc.w	0		; wireframe rotation angle

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
; WaitBlitter — wait for blitter to finish
;==========================================================
WaitBlitter:
.wb:	btst	#6,DMACONR(a6)
	bne.s	.wb
	rts

;==========================================================
; BlitClearBPL2 — clear entire starfield bitplane via blitter
; Does NOT wait for completion (caller should WaitBlitter later)
;==========================================================
BlitClearBPL2:
	bsr.s	WaitBlitter		; ensure previous blit done
	clr.w	BLTDMOD(a6)		; no modulo
	move.l	#$01000000,BLTCON0(a6)	; D only, minterm=0 (clear)
	move.l	#BPLANE2,BLTDPTH(a6)
	move.w	#(256<<6)|24,BLTSIZE(a6) ; 256 lines × 24 words → starts blit
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
	; (EraseStar removed — BPL2 cleared by blitter before star loop)
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
	; (EraseStar removed — BPL2 cleared by blitter before star loop)
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
; UpdateScrolltext — advance pixel counter (copper handles display)
;==========================================================
UpdateScrolltext:
	move.w	scroll_pixel_pos(pc),d0
	addq.w	#SCROLL_SPEED,d0
	cmp.w	#SCROLL_TOTAL_PX,d0
	blt.s	.noWrap
	sub.w	#SCROLL_TOTAL_PX,d0
.noWrap:
	move.w	d0,scroll_pixel_pos
	rts

;==========================================================
; PreRenderScroll — render entire scroll text into wide bitmap
; Buffer layout: 256 bytes/row × 8 rows, 1 byte per char column
;==========================================================
PreRenderScroll:
	movem.l	d2-d4/a2-a3,-(sp)

	; Clear buffer (256 × 8 = 2048 bytes)
	lea	SCROLL_BUF,a0
	move.w	#(2048/4)-1,d0
.clrBuf:
	clr.l	(a0)+
	dbf	d0,.clrBuf

	; For each char in scroll_text: write font bytes into buffer columns
	lea	scroll_text(pc),a2	; source text
	lea	font_data(pc),a3	; font base
	moveq	#0,d2			; char index (column)

.charLoop:
	moveq	#0,d0
	move.b	(a2)+,d0
	cmp.b	#$FF,d0			; end marker?
	beq.s	.doneChars

	; Font lookup: (char - 32) * 8
	sub.w	#32,d0
	bpl.s	.cOk
	moveq	#0,d0
.cOk:
	cmp.w	#96,d0
	blt.s	.cInRange
	moveq	#0,d0
.cInRange:
	lsl.w	#3,d0			; *8 bytes per glyph
	lea	0(a3,d0.w),a0		; glyph pointer

	; Write 8 rows: row R gets byte at SCROLL_BUF + R*256 + char_index
	lea	SCROLL_BUF,a1
	add.w	d2,a1			; column offset
	moveq	#7,d1
.rowLoop:
	move.b	(a0)+,(a1)
	lea	SCROLL_BUF_STRIDE(a1),a1
	dbf	d1,.rowLoop

	addq.w	#1,d2
	bra.s	.charLoop

.doneChars:
	; Copy first 78 bytes of each row to positions TEXT_LEN..TEXT_LEN+77
	; for seamless wrap
	moveq	#7,d3			; 8 rows
	lea	SCROLL_BUF,a0
.wrapRow:
	move.w	d2,d4			; dest offset = TEXT_LEN
	moveq	#77,d1			; 78 bytes to copy (0..77)
	moveq	#0,d0			; source offset
.wrapByte:
	move.b	0(a0,d0.w),0(a0,d4.w)
	addq.w	#1,d0
	addq.w	#1,d4
	dbf	d1,.wrapByte
	lea	SCROLL_BUF_STRIDE(a0),a0
	dbf	d3,.wrapRow

	movem.l	(sp)+,d2-d4/a2-a3
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
; InitBounceLogo — fill LOGO_SPR1 with "D" glyph data
; Sprite format: POS word, CTL word, then 16 rows of (dataA, dataB)
;==========================================================
InitBounceLogo:
	; Fill sprite 1 (bouncing D)
	lea	LOGO_SPR1,a0
	move.w	bounce_y(pc),d0
	move.w	bounce_x(pc),d1

	; POS: VSTART[7:0]<<8 | HSTART[8:1]
	move.w	d0,d2
	and.w	#$FF,d2
	lsl.w	#8,d2
	move.w	d1,d3
	lsr.w	#1,d3
	or.w	d3,d2
	move.w	d2,(a0)+		; POS

	; CTL: VSTOP[7:0]<<8 | VSTART[8]<<2 | VSTOP[8]<<1 | HSTART[0]
	move.w	d0,d2
	add.w	#BOUNCE_H,d2		; VSTOP
	move.w	d2,d3
	and.w	#$FF,d3
	lsl.w	#8,d3
	btst	#8,d0			; VSTART bit 8
	beq.s	.noVS8
	or.w	#$04,d3
.noVS8:	btst	#8,d2			; VSTOP bit 8
	beq.s	.noVE8
	or.w	#$02,d3
.noVE8:	btst	#0,d1			; HSTART bit 0
	beq.s	.noH0
	or.w	#$01,d3
.noH0:	move.w	d3,(a0)+		; CTL

	; Copy 16 rows of glyph data
	lea	bounce_glyph(pc),a1
	moveq	#15,d0
.glyphCopy:
	move.l	(a1)+,(a0)+
	dbf	d0,.glyphCopy

	; Terminator
	clr.l	(a0)

	; Also init sprite 3 with same data (second bouncer)
	lea	LOGO_SPR3,a0
	; Start at different position
	move.w	#$90,d0			; Y
	move.w	#$120,d1		; X
	move.w	d0,d2
	and.w	#$FF,d2
	lsl.w	#8,d2
	move.w	d1,d3
	lsr.w	#1,d3
	or.w	d3,d2
	move.w	d2,(a0)+
	move.w	d0,d2
	add.w	#BOUNCE_H,d2
	move.w	d2,d3
	and.w	#$FF,d3
	lsl.w	#8,d3
	btst	#8,d0
	beq.s	.noVS8b
	or.w	#$04,d3
.noVS8b: btst	#8,d2
	beq.s	.noVE8b
	or.w	#$02,d3
.noVE8b: btst	#0,d1
	beq.s	.noH0b
	or.w	#$01,d3
.noH0b:	move.w	d3,(a0)+
	lea	bounce_glyph(pc),a1
	moveq	#15,d0
.glyphCopy2:
	move.l	(a1)+,(a0)+
	dbf	d0,.glyphCopy2
	clr.l	(a0)
	rts

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
; UpdateBounceLogo — DVD-style bounce + music pulsation
; Updates POS/CTL words of LOGO_SPR1 and LOGO_SPR3
;==========================================================
UpdateBounceLogo:
	; --- Pulsation: on beat, spike pulse; otherwise decay ---
	move.w	bounce_pulse(pc),d0
	move.w	flash_timer(pc),d1
	tst.w	d1
	beq.s	.noPulse
	move.w	#15,d0
.noPulse:
	tst.w	d0
	beq.s	.pulseOk
	subq.w	#1,d0
.pulseOk:
	move.w	d0,bounce_pulse

	; Speed = 1 + (combined_energy >> 5)
	move.w	combined_energy(pc),d4
	lsr.w	#5,d4
	addq.w	#1,d4

	; --- Bounce sprite 1 ---
	lea	bounce_x(pc),a1
	lea	LOGO_SPR1,a0
	bsr.s	.bounceOne

	; --- Bounce sprite 3 (independent) ---
	lea	bounce2_x(pc),a1
	lea	LOGO_SPR3,a0
	bsr.s	.bounceOne
	rts

	; --- Bounce one sprite ---
	; In: a0=sprite data, a1=ptr to (x,y,dx,dy), d4=speed
.bounceOne:
	move.w	(a1),d0			; X
	move.w	2(a1),d1		; Y
	move.w	4(a1),d2		; dX
	move.w	6(a1),d3		; dY

	; Apply velocity * speed
	move.w	d2,d5
	muls	d4,d5
	add.w	d5,d0
	move.w	d3,d5
	muls	d4,d5
	add.w	d5,d1

	; Bounce X
	cmp.w	#BOUNCE_XMIN,d0
	bge.s	.xMinOk
	move.w	#BOUNCE_XMIN,d0
	neg.w	d2
.xMinOk:
	cmp.w	#BOUNCE_XMAX,d0
	ble.s	.xMaxOk
	move.w	#BOUNCE_XMAX,d0
	neg.w	d2
.xMaxOk:
	; Bounce Y
	cmp.w	#BOUNCE_YMIN,d1
	bge.s	.yMinOk
	move.w	#BOUNCE_YMIN,d1
	neg.w	d3
.yMinOk:
	cmp.w	#BOUNCE_YMAX,d1
	ble.s	.yMaxOk
	move.w	#BOUNCE_YMAX,d1
	neg.w	d3
.yMaxOk:
	; Store
	move.w	d0,(a1)
	move.w	d1,2(a1)
	move.w	d2,4(a1)
	move.w	d3,6(a1)

	; Fall through to write POS/CTL
	; --- write POS/CTL at (a0) for pos d0=H, d1=V ---
	move.w	d1,d5			; VSTART
	move.w	d1,d6
	add.w	#BOUNCE_H,d6		; VSTOP

	; POS: VSTART[7:0]<<8 | HSTART[8:1]
	move.w	d5,d7
	and.w	#$FF,d7
	lsl.w	#8,d7
	move.w	d0,d3
	lsr.w	#1,d3
	or.w	d3,d7
	move.w	d7,(a0)

	; CTL: VSTOP[7:0]<<8 | VSTART[8]<<2 | VSTOP[8]<<1 | HSTART[0]
	move.w	d6,d7
	and.w	#$FF,d7
	lsl.w	#8,d7
	btst	#8,d5
	beq.s	.wNoVS8
	or.w	#$04,d7
.wNoVS8: btst	#8,d6
	beq.s	.wNoVE8
	or.w	#$02,d7
.wNoVE8: btst	#0,d0
	beq.s	.wNoH0
	or.w	#$01,d7
.wNoH0:	move.w	d7,2(a0)
	rts

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
; BlitClearBPL1Middle — clear middle zone of BPL1 via blitter
; 108 lines × 24 words (48 bytes/line)
; Does NOT wait for completion
;==========================================================
BlitClearBPL1Middle:
	bsr.w	WaitBlitter
	clr.w	BLTDMOD(a6)
	move.l	#$01000000,BLTCON0(a6)	; D only, minterm=0
	move.l	#BPL1_MIDDLE,BLTDPTH(a6)
	move.w	#(MIDDLE_LINES<<6)|24,BLTSIZE(a6)
	rts

;==========================================================
; UpdateTunnelZoom — compute 108 ring colors
; Writes to TUNNEL_COLORS buffer (108 words)
;==========================================================
UpdateTunnelZoom:
	movem.l	d2-d5/a2-a3,-(sp)
	lea	TUNNEL_COLORS,a0
	lea	tunnel_color_table(pc),a2
	lea	signed_sine_table(pc),a3

	; center_y orbits via sine
	move.w	ring_phase(pc),d0
	and.w	#$FF,d0
	add.w	d0,d0
	move.w	(a3,d0.w),d2		; signed sine -127..+127
	asr.w	#2,d2			; scale to -31..+31
	add.w	#54,d2			; center at line 54 of 108

	; spacing shrinks with bass energy: 4 - (combined>>5), min 1
	move.w	combined_energy(pc),d3
	lsr.w	#5,d3
	move.w	#4,d4
	sub.w	d3,d4
	cmp.w	#1,d4
	bge.s	.spacOk
	moveq	#1,d4
.spacOk:

	; ring_phase offset for animation
	move.w	ring_phase(pc),d5

	moveq	#0,d3			; line counter
.tLoop:
	; dist = abs(line - center_y)
	move.w	d3,d0
	sub.w	d2,d0
	bpl.s	.tNoNeg
	neg.w	d0
.tNoNeg:
	; index = (dist * spacing + ring_phase) & $FF
	mulu	d4,d0
	add.w	d5,d0
	and.w	#$FF,d0
	add.w	d0,d0			; word index
	move.w	(a2,d0.w),(a0)+

	addq.w	#1,d3
	cmp.w	#MIDDLE_LINES,d3
	blt.s	.tLoop

	movem.l	(sp)+,d2-d5/a2-a3
	rts

;==========================================================
; UpdateRainbowStarColors — compute 108 COLOR02/03 pairs
; Writes to STAR_COLORS buffer (108 × 2 words = 216 words)
;==========================================================
UpdateRainbowStarColors:
	movem.l	d2-d3/a2,-(sp)
	lea	STAR_COLORS,a0
	lea	rainbow_table(pc),a2

	move.w	rainbow_star_phase(pc),d2

	moveq	#0,d3
.rsLoop:
	move.w	d3,d0
	add.w	d2,d0
	and.w	#$FF,d0
	add.w	d0,d0
	move.w	(a2,d0.w),d0		; COLOR02 = rainbow
	move.w	d0,(a0)+		; store COLOR02
	or.w	#$888,d0		; brighter overlap
	move.w	d0,(a0)+		; store COLOR03

	addq.w	#1,d3
	cmp.w	#MIDDLE_LINES,d3
	blt.s	.rsLoop

	movem.l	(sp)+,d2-d3/a2
	rts

;==========================================================
; PrepareGlitchOffsets — LFSR random BPL2 addresses
; Only when flash_timer > 0. Writes to GLITCH_OFFSETS (108 longs)
;==========================================================
PrepareGlitchOffsets:
	move.w	flash_timer(pc),d0
	beq.s	.noGlitch
	movem.l	d2-d3,-(sp)
	lea	GLITCH_OFFSETS,a0
	move.w	#MIDDLE_LINES-1,d3
.gLoop:
	bsr.w	LFSRNext
	; Random offset within BPLANE2 (12KB = $3000)
	move.w	d2,d0
	and.w	#$1FFF,d0		; 0-8191
	; Force even
	and.w	#$FFFE,d0
	ext.l	d0
	add.l	#BPLANE2,d0
	move.l	d0,(a0)+
	dbf	d3,.gLoop
	movem.l	(sp)+,d2-d3
.noGlitch:
	rts

;==========================================================
; DrawWaveform — plot 4-channel sine waves into BPL1 middle
; 108 x-positions, 4 channels overlaid
;==========================================================
DrawWaveform:
	movem.l	d2-d7/a2-a4,-(sp)
	lea	BPL1_MIDDLE,a0
	lea	yoffset_table(pc),a4
	lea	signed_sine_table(pc),a3
	lea	eq_energy(pc),a2

	moveq	#3,d7			; channel counter
.wfChLoop:
	move.w	d7,d0
	add.w	d0,d0
	move.w	(a2,d0.w),d6		; channel energy (amplitude)
	lsr.w	#1,d6			; scale down
	cmp.w	#50,d6
	ble.s	.ampOk
	move.w	#50,d6
.ampOk:
	tst.w	d6
	beq.s	.wfChSkip

	; Frequency: channel 0=fast, 3=slow
	move.w	d7,d0
	add.w	d0,d0
	lea	wave_phases(pc),a1
	move.w	(a1,d0.w),d5		; phase for this channel

	moveq	#0,d4			; x counter (0..107)
.wfXLoop:
	; y = 54 + (sin(x*freq + phase) * amplitude) >> 7
	move.w	d4,d0
	move.w	#3,d1
	sub.w	d7,d1			; freq multiplier: ch0=3, ch1=2, ch2=1, ch3=0
	addq.w	#1,d1			; ch0=4, ch1=3, ch2=2, ch3=1
	mulu	d1,d0
	add.w	d5,d0
	and.w	#$FF,d0
	add.w	d0,d0
	move.w	(a3,d0.w),d0		; signed sine -127..+127
	muls	d6,d0
	asr.l	#7,d0
	add.w	#54,d0			; center Y in middle zone

	; Clamp to 0..107
	bpl.s	.wyOk1
	moveq	#0,d0
.wyOk1:	cmp.w	#107,d0
	ble.s	.wyOk2
	move.w	#107,d0
.wyOk2:
	; Plot pixel at (d4, d0) in BPL1 middle
	; byte_offset = y*48 + (x>>3)
	add.w	d0,d0
	move.w	(a4,d0.w),d1		; y*48 from yoffset table
	move.w	d4,d0
	lsr.w	#3,d0
	add.w	d0,d1			; byte offset

	move.w	d4,d0
	not.w	d0
	and.w	#7,d0			; bit index
	bset	d0,(a0,d1.w)

	addq.w	#1,d4
	cmp.w	#MIDDLE_LINES,d4
	blt.s	.wfXLoop

.wfChSkip:
	dbf	d7,.wfChLoop

	movem.l	(sp)+,d2-d7/a2-a4
	rts

;==========================================================
; BlitMoirePass1 — fill BPL1 middle with stripe pattern (A→D)
;==========================================================
BlitMoirePass1:
	bsr.w	WaitBlitter
	move.w	#$FFFF,BLTAFWM(a6)
	move.w	#$FFFF,BLTALWM(a6)
	clr.w	BLTAMOD(a6)
	clr.w	BLTDMOD(a6)
	move.l	#$09F00000,BLTCON0(a6)	; A→D, minterm=$F0 (copy A)
	lea	stripe_pattern(pc),a1
	move.l	a1,BLTAPTH(a6)
	move.l	#BPL1_MIDDLE,BLTDPTH(a6)
	; 108 lines × 24 words — but source is only 48 bytes
	; We repeat by setting AMOD = -48 (wrap every line)
	move.w	#-48,BLTAMOD(a6)	; source wraps each line
	move.w	#(MIDDLE_LINES<<6)|24,BLTSIZE(a6)
	rts

;==========================================================
; BlitMoirePass2 — XOR shifted copy (A XOR B → D)
;==========================================================
BlitMoirePass2:
	bsr.w	WaitBlitter
	; Shift amount from moire_shift
	move.w	moire_shift(pc),d0
	and.w	#$F,d0
	ror.w	#4,d0			; shift into ASH position (bits 15-12)
	or.w	#$0DE0,d0		; A+B→D, minterm=$60 (A XOR B)
	move.w	d0,BLTCON0(a6)
	clr.w	BLTCON1(a6)
	move.w	#$FFFF,BLTAFWM(a6)
	move.w	#$FFFF,BLTALWM(a6)
	clr.w	BLTAMOD(a6)
	clr.w	BLTBMOD(a6)
	clr.w	BLTDMOD(a6)
	move.l	#BPL1_MIDDLE,BLTAPTH(a6)
	move.l	#BPL1_MIDDLE,BLTBPTH(a6)
	move.l	#BPL1_MIDDLE,BLTDPTH(a6)
	move.w	#(MIDDLE_LINES<<6)|24,BLTSIZE(a6)
	rts

;==========================================================
; DrawWireframe — transform octahedron + draw 12 edges
;==========================================================
DrawWireframe:
	movem.l	d2-d7/a2-a5,-(sp)
	lea	signed_sine_table(pc),a3
	lea	octa_vertices(pc),a2
	lea	PROJECTED_VERTS,a4

	; Compute sin/cos of wire_angle for Y rotation
	move.w	wire_angle(pc),d0
	and.w	#$FF,d0
	add.w	d0,d0
	move.w	(a3,d0.w),d6		; sin
	move.w	wire_angle(pc),d0
	add.w	#64,d0
	and.w	#$FF,d0
	add.w	d0,d0
	move.w	(a3,d0.w),d7		; cos

	; Transform 6 vertices → projected 2D (x,y) words
	moveq	#5,d5
.xformLoop:
	move.w	(a2)+,d0		; x
	move.w	(a2)+,d1		; y
	move.w	(a2)+,d2		; z

	; Y-axis rotation: x' = x*cos + z*sin, z' = z*cos - x*sin
	move.w	d0,d3
	move.w	d2,d4
	muls	d7,d0			; x*cos
	muls	d6,d4			; z*sin
	add.l	d4,d0
	asr.l	#7,d0			; x'
	muls	d7,d2			; z*cos
	muls	d6,d3			; x*sin
	sub.l	d3,d2
	asr.l	#7,d2			; z'

	; Simple perspective: project with z offset
	add.w	#150,d2			; push back
	cmp.w	#10,d2
	bge.s	.zPosOk
	move.w	#10,d2
.zPosOk:
	; screen_x = x' * 128 / z' + 54 (center of 108px width in middle area)
	; screen_y = y * 128 / z' + 54
	ext.l	d0
	lsl.l	#7,d0
	divs	d2,d0
	add.w	#54,d0			; x in 0..107 range (roughly)

	ext.l	d1
	lsl.l	#7,d1
	divs	d2,d1
	add.w	#54,d1

	move.w	d0,(a4)+		; projected x
	move.w	d1,(a4)+		; projected y
	dbf	d5,.xformLoop

	; Draw 12 edges
	lea	octa_edges(pc),a2
	lea	PROJECTED_VERTS,a4
	moveq	#11,d5
.edgeLoop:
	move.w	d5,-(sp)		; save loop counter

	; Read edge vertex indices
	moveq	#0,d0
	move.b	(a2)+,d0		; v0 index
	moveq	#0,d1
	move.b	(a2)+,d1		; v1 index
	move.l	a2,-(sp)		; save edge pointer

	; Fetch v0 projected coords
	lsl.w	#2,d0			; *4 bytes per vertex
	move.w	(a4,d0.w),d2		; v0.x → temp d2
	move.w	2(a4,d0.w),d3		; v0.y → temp d3

	; Fetch v1 projected coords
	lsl.w	#2,d1
	move.w	(a4,d1.w),d4		; v1.x → temp d4
	move.w	2(a4,d1.w),d5		; v1.y → temp d5

	; Set up DrawLine args: d0=x0, d1=y0, d2=x1, d3=y1
	move.w	d2,d0			; x0
	move.w	d3,d1			; y0
	move.w	d4,d2			; x1
	move.w	d5,d3			; y1
	bsr.w	DrawLine

	move.l	(sp)+,a2		; restore edge pointer
	move.w	(sp)+,d5		; restore loop counter
	dbf	d5,.edgeLoop

	movem.l	(sp)+,d2-d7/a2-a5
	rts

;==========================================================
; DrawLine — Bresenham line (d0,d1)→(d2,d3) into BPL1 middle
; d0=x0, d1=y0, d2=x1, d3=y1
; Clips to 0..107 range for both axes
; Uses: d4-d7, a0, a2 (saved/restored)
;==========================================================
DrawLine:
	movem.l	d4-d7/a2,-(sp)
	lea	BPL1_MIDDLE,a0
	lea	yoffset_table(pc),a2

	; Clip all coords to 0..107
	tst.w	d0
	bpl.s	.cx0ok
	moveq	#0,d0
.cx0ok:	cmp.w	#107,d0
	ble.s	.cx0ok2
	move.w	#107,d0
.cx0ok2:
	tst.w	d1
	bpl.s	.cy0ok
	moveq	#0,d1
.cy0ok:	cmp.w	#107,d1
	ble.s	.cy0ok2
	move.w	#107,d1
.cy0ok2:
	tst.w	d2
	bpl.s	.cx1ok
	moveq	#0,d2
.cx1ok:	cmp.w	#107,d2
	ble.s	.cx1ok2
	move.w	#107,d2
.cx1ok2:
	tst.w	d3
	bpl.s	.cy1ok
	moveq	#0,d3
.cy1ok:	cmp.w	#107,d3
	ble.s	.cy1ok2
	move.w	#107,d3
.cy1ok2:

	; dx = abs(x1-x0), sx = sign → d6
	move.w	d2,d4
	sub.w	d0,d4
	moveq	#1,d6
	tst.w	d4
	bpl.s	.dxPos
	neg.w	d4
	moveq	#-1,d6
.dxPos:
	; dy = -abs(y1-y0), sy = sign → d7
	move.w	d3,d5
	sub.w	d1,d5
	moveq	#1,d7
	tst.w	d5
	bmi.s	.dyNeg
	neg.w	d5
	bra.s	.dyDone
.dyNeg:	neg.w	d7
.dyDone:
	; Push dx, dy onto stack for loop reference
	; Stack layout: [sp+0]=dy, [sp+2]=dx
	move.w	d4,-(sp)		; dx
	move.w	d5,-(sp)		; dy
	; d5 = error = dx + dy
	add.w	d4,d5

.lineLoop:
	; --- Plot pixel at (d0=x, d1=y) ---
	; Save error on stack temporarily, use d4/d5 for plot
	move.w	d5,-(sp)		; save error
	move.w	d1,d4
	add.w	d4,d4
	move.w	(a2,d4.w),d4		; d4 = y * 48
	move.w	d0,d5
	lsr.w	#3,d5
	add.w	d5,d4			; d4 = byte offset
	move.w	d0,d5
	not.w	d5
	and.w	#7,d5
	bset	d5,(a0,d4.w)		; set pixel
	move.w	(sp)+,d5		; restore error

	; Check if at endpoint
	cmp.w	d2,d0
	bne.s	.notDone
	cmp.w	d3,d1
	beq.s	.lineDone
.notDone:
	; e2 = 2 * error
	move.w	d5,d4
	add.w	d4,d4			; d4 = e2

	; if e2 >= dy: error += dy, x += sx
	cmp.w	(sp),d4			; e2 vs dy [sp+0]
	blt.s	.noXstep
	add.w	(sp),d5
	add.w	d6,d0
.noXstep:
	; if e2 <= dx: error += dx, y += sy
	cmp.w	2(sp),d4		; e2 vs dx [sp+2]
	bgt.s	.noYstep
	add.w	2(sp),d5
	add.w	d7,d1
.noYstep:
	bra.s	.lineLoop

.lineDone:
	addq.l	#4,sp			; pop dy, dx
	movem.l	(sp)+,d4-d7/a2
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
	move.l	#(SPR1PTH<<16)|(LOGO_SPR1>>16),(a0)+
	move.l	#(SPR1PTL<<16)|(LOGO_SPR1&$FFFF),(a0)+
	move.l	#(SPR2PTH<<16)|(SPR2_DATA>>16),(a0)+
	move.l	#(SPR2PTL<<16)|(SPR2_DATA&$FFFF),(a0)+
	move.l	#(SPR3PTH<<16)|(LOGO_SPR3>>16),(a0)+
	move.l	#(SPR3PTL<<16)|(LOGO_SPR3&$FFFF),(a0)+
	move.l	#(SPR4PTH<<16)|(SPR4_DATA>>16),(a0)+
	move.l	#(SPR4PTL<<16)|(SPR4_DATA&$FFFF),(a0)+
	move.l	#(SPR5PTH<<16)|(NULL_SPR>>16),(a0)+
	move.l	#(SPR5PTL<<16)|(NULL_SPR&$FFFF),(a0)+
	move.l	#(SPR6PTH<<16)|(SPR6_DATA>>16),(a0)+
	move.l	#(SPR6PTL<<16)|(SPR6_DATA&$FFFF),(a0)+
	move.l	#(SPR7PTH<<16)|(NULL_SPR>>16),(a0)+
	move.l	#(SPR7PTL<<16)|(NULL_SPR&$FFFF),(a0)+

	; Sprite colors — pairs 0+1 and 2+3 pulse with bounce_pulse
	; Base: spr0+1 = red ($F44), spr2+3 = green ($4F4)
	; Pulse adds brightness per nibble
	move.w	bounce_pulse(pc),d0	; 0-15
	lsr.w	#1,d0			; 0-7

	; Pair 0+1 color 3: red + pulse
	move.w	#$0F44,d1
	add.w	d0,d1			; boost blue
	move.w	d0,-(sp)
	lsl.w	#4,d0
	add.w	d0,d1			; boost green
	move.w	(sp)+,d0
	move.w	#$01A6,(a0)+
	move.w	d1,(a0)+

	; Pair 2+3 color 3: green + pulse
	move.w	#$04F4,d1
	add.w	d0,d1			; boost blue
	move.w	d0,-(sp)
	lsl.w	#8,d0
	add.w	d0,d1			; boost red
	move.w	(sp)+,d0
	move.w	#$01AE,(a0)+
	move.w	d1,(a0)+

	; Pair 4+5: blue (EQ only, no bounce)
	move.l	#($01B6<<16)|$048F,(a0)+
	; Pair 6+7: yellow (EQ only)
	move.l	#($01BE<<16)|$0FF4,(a0)+

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
	; MIDDLE ZONE start ($6C): enable BPL1 for effects
	; Set BPL1PT = BPL1_MIDDLE, COLOR01=$FFF
	; ============================================
	; WAIT for middle start
	move.w	#(MIDDLE_START<<8)|$07,(a0)+
	move.w	#$FFFE,(a0)+
	; Point BPL1 to middle zone area
	move.l	#(BPL1PTH<<16)|(BPL1_MIDDLE>>16),(a0)+
	move.l	#(BPL1PTL<<16)|(BPL1_MIDDLE&$FFFF),(a0)+
	move.l	#(COLOR01<<16)|$0FFF,(a0)+

	; ============================================
	; MIDDLE ZONE: lines $6C - $D7 (108 lines)
	; Per line: WAIT + COLOR00(tunnel/rasterbar) + COLOR02(rainbow) + COLOR03(rainbow)
	; Conditional: + BPL2PTH/L (glitch) when flash_timer > 0
	; ============================================
	moveq	#0,d5			; line counter
	lea	TUNNEL_COLORS,a1
	move.l	a1,-(sp)		; save tunnel colors ptr
	lea	STAR_COLORS,a1
	move.l	a1,-(sp)		; save star colors ptr
	lea	GLITCH_OFFSETS,a1
	move.l	a1,-(sp)		; save glitch ptr

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

	; COLOR00: check raster bars first, else use tunnel color
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
	; d3 = bar index, compute gradient color
	move.w	d3,d1
	add.w	d1,d1
	lea	bar_y(pc),a1
	move.w	0(a1,d1.w),d1		; center
	sub.w	d1,d0			; d0 = current - center (-7..+7)
	add.w	#8,d0			; index 0-15
	bpl.s	.bIdxOk
	moveq	#0,d0
.bIdxOk:
	cmp.w	#15,d0
	ble.s	.bIdxOk2
	moveq	#15,d0
.bIdxOk2:
	add.w	d0,d0
	move.w	d3,d1
	lsl.w	#5,d1
	lea	rasterbar_red(pc),a1
	add.w	d1,a1
	move.w	0(a1,d0.w),d0
	tst.w	d7
	beq.s	.barNoFlash
	bsr.w	ApplyFlash
.barNoFlash:
	move.w	#COLOR00,(a0)+
	move.w	d0,(a0)+
	bra.s	.middleCol23

.barMiss:
	; No bar hit — use tunnel color from pre-computed buffer
	move.l	8(sp),a1		; tunnel colors ptr
	move.w	d5,d0
	add.w	d0,d0
	move.w	(a1,d0.w),d0		; tunnel color for this line
	tst.w	d7
	beq.s	.mNoFlash
	bsr.w	ApplyFlash
.mNoFlash:
	move.w	#COLOR00,(a0)+
	move.w	d0,(a0)+

.middleCol23:
	; COLOR02 + COLOR03 from rainbow star colors buffer
	move.l	4(sp),a1		; star colors ptr
	move.w	d5,d0
	lsl.w	#2,d0			; *4 (2 words per line)
	move.w	#COLOR02,(a0)+
	move.w	(a1,d0.w),(a0)+		; COLOR02
	move.w	#COLOR03,(a0)+
	move.w	2(a1,d0.w),(a0)+	; COLOR03

	; Conditional glitch: BPL2PTH/L
	move.w	flash_timer(pc),d0
	beq.s	.noGlitch
	move.l	(sp),a1			; glitch offsets ptr
	move.w	d5,d0
	lsl.w	#2,d0			; *4 (longword per line)
	move.l	(a1,d0.w),d0		; random BPL2 address
	move.w	#BPL2PTH,(a0)+
	swap	d0
	move.w	d0,(a0)+
	move.w	#BPL2PTL,(a0)+
	swap	d0
	move.w	d0,(a0)+
.noGlitch:

	addq.w	#1,d5
	cmp.w	#MIDDLE_LINES,d5
	blt.w	.middleLine

	lea	12(sp),sp		; pop 3 saved pointers

	; ============================================
	; SCROLL ZONE transition: reset BPL2PT (undo glitch)
	; ============================================
	move.l	#(BPL2PTH<<16)|(BPLANE2>>16),(a0)+
	move.l	#(BPL2PTL<<16)|(BPLANE2&$FFFF),(a0)+
	move.l	#(COLOR01<<16)|$0000,(a0)+	; disable BPL1 text color

	; ============================================
	; SCROLL ZONE: lines $D8 - $FF (40 lines)
	; Pre-rendered copper-only sine scroller
	; Per line: WAIT + COLOR00 + COLOR01 + BPL1PT
	; 5x vertical scaling (8 font rows × 5 scanlines)
	; No V8 barrier needed ($D8-$FF < $100)
	; ============================================

	; Compute scroll parameters
	move.w	scroll_pixel_pos(pc),d0
	move.w	d0,d1
	and.w	#15,d1			; fine = pixel_pos & 15

	; Set BPLCON1: PF1H = fine, PF2H = 0 (stars unaffected)
	move.w	#BPLCON1,(a0)+
	move.w	d1,(a0)+

	; Compute base pointer: SCROLL_BUF + coarse
	lsr.w	#4,d0
	add.w	d0,d0			; coarse = (pixel_pos >> 4) * 2
	ext.l	d0
	add.l	#SCROLL_BUF,d0
	move.l	d0,-(sp)		; [sp] = base_ptr (constant)

	moveq	#0,d4			; font_row_base = 0 (high word also 0)
	move.w	#4,a5			; sub-line counter (4..0 = 5 lines/row)
	moveq	#0,d5			; scanline counter

.scrollLine:
	move.w	#SCROLL_START,d0
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
	add.w	#172,d0			; offset past logo+middle
	add.w	d0,d0
	add.w	d2,d0			; + plasma_phase1
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
	add.w	d3,d0			; + rainbow_phase
	and.w	#$FF,d0
	add.w	d0,d0
	move.w	#COLOR01,(a0)+
	move.w	0(a4,d0.w),d0
	tst.w	d7
	beq.s	.sNoFlash2
	bsr.w	ApplyFlash
.sNoFlash2:
	move.w	d0,(a0)+

	; BPL1PT = base_ptr + font_row_base + sine_offset
	move.w	d5,d0
	add.w	d6,d0			; + sine_phase
	and.w	#$FF,d0
	moveq	#0,d1
	move.b	0(a2,d0.w),d1		; sine 0-48
	lsr.w	#SCROLL_SINE_SHIFT,d1	; halve → 0-24
	and.w	#$FFFE,d1		; word-align
	ext.l	d1
	add.l	(sp),d1			; + base_ptr
	add.l	d4,d1			; + font_row_base (high word = 0)

	move.w	#BPL1PTH,(a0)+
	swap	d1
	move.w	d1,(a0)+
	move.w	#BPL1PTL,(a0)+
	swap	d1
	move.w	d1,(a0)+

	; Advance sub-line / font row (5x vertical scaling)
	move.w	a5,d0
	subq.w	#1,d0
	bpl.s	.subOk
	moveq	#4,d0			; reset sub-counter
	add.w	#SCROLL_BUF_STRIDE,d4	; next font row
.subOk:
	move.w	d0,a5

	addq.w	#1,d5
	cmp.w	#SCROLL_LINES,d5
	blt.w	.scrollLine

	; Pop base_ptr from stack
	addq.l	#4,sp

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
ENABLE_SONGEND	equ	1
	include	"ptplayer_minimal.asm"

	even
moddata:
	incbin	"part2.mod"
