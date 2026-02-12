;----------------------------------------------------------
; DEWDZKI-FXP Bootblock Trackloader
; Loads demo code from disk offset 1024 into DEMO_ADDR
; Uses AmigaOS DoIO via Kickstart-provided IORequest
;----------------------------------------------------------

DEMO_ADDR	equ	$49000
LOAD_SIZE	equ	16384		; must be >= demo.bin size
STACK_ADDR	equ	$7FFF0
CUSTOM		equ	$DFF000
DMACON		equ	$096
INTENA		equ	$09A
INTREQ		equ	$09C

; exec.library LVO offsets
LVO_DoIO	equ	-456

; trackdisk IORequest field offsets
IO_COMMAND	equ	28
IO_LENGTH	equ	36
IO_DATA		equ	40
IO_OFFSET	equ	44
CMD_READ	equ	2

;==========================================================
; Bootblock header (12 bytes)
;==========================================================
	dc.b	"DOS",0			; disk type
	dc.l	0			; checksum (patched by makeadf.py)
	dc.l	880			; root block

;==========================================================
; Entry: A6=ExecBase, A1=IORequest (from Kickstart)
;==========================================================
Boot:
	move.l	a6,a5			; save ExecBase in A5
	move.l	a1,a4			; save IORequest in A4

	; Set up trackdisk read: 8192 bytes from offset 1024 into $49000
	move.w	#CMD_READ,IO_COMMAND(a4)
	move.l	#LOAD_SIZE,IO_LENGTH(a4)
	move.l	#DEMO_ADDR,IO_DATA(a4)
	move.l	#1024,IO_OFFSET(a4)

	; Call DoIO(IORequest)
	move.l	a4,a1
	move.l	a5,a6
	jsr	LVO_DoIO(a6)

	; Kill the OS
	lea	CUSTOM,a6
	move.w	#$7FFF,INTENA(a6)
	move.w	#$7FFF,INTREQ(a6)
	move.w	#$7FFF,DMACON(a6)

	; Set stack and jump to demo
	lea	STACK_ADDR,a7
	jmp	DEMO_ADDR
