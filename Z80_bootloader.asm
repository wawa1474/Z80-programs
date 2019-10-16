;//00 = execute, 01 = set addr, 02 = read, 03 = write
    jp nextByte
;.org 100
jumpTable:
.dw execute,getAddr,plusRead,plusWrite
jTableSize equ 4;https://www.asm80.com/index.html
#define jTableSize 4;http://clrhome.org/asm/
addrBuild:
.dw $ABCD
nextByte:
	ld hl, nextByte;
	push hl;
	
	call getByte
	ld a, l
	
	cp jTableSize;make the user do bounds checking?
	ccf
	ret c

	ld hl, jumpTable;10-3
	
	;ld b, 0;7-2
	ld b, h;4-1;h must be 0
	
	add a, a;4-1
	ld c, a;4-1
	add hl, bc;11-1

	ld b, (hl);7-1
	inc hl;6-1
	ld h, (hl);7-1
	ld l, b;4-1
	jp (hl);4-1
	;;64-13
	;61-12
	
	
;	ld hl, jumpTable;10-3
;	add a, a;4-1
;	add a, l;4-1
;	ld l, a;4-1
;	ld a, 0;7-2
;	adc a, h;4-1
;	ld h, a;4-1
;	
;	ld a, (hl);7-1
;	inc hl;6-1
;	ld h, (hl);7-1
;	ld l, a;4-1
;	jp (hl);4-1
;	;65-15
	
	
;	ld hl, jumpTable;10-3
;	ld de, jumpAddr;10-3
;	
;	add a, a;4-1
;	ld b, 0;7-2
;	ld c, a;4-1
;	add hl, bc;11-1
;	
;	ld a, (hl);7-1
;	inc hl;6-1
;	ld (de), a;7-1
;	inc de;6-1
;	ld a, (hl);7-1
;	ld (de), a;7-1
;	
;	.db $C3
;jumpAddr:
;	.dw jumpAddr;10-3
;	;96-20
	
	ret;
	
execute:
	;call getAddr;must use the getAddr cmd before using the execute cmd
	push de
	ret;

;read:
;	;call getAddr;
;	;ld a, (de)
;	;call sendByte
;	ld hl, addrBuild
;	ld a, (hl)
;	push af
;	call makeByte
;	pop af
;	ld (hl), a
;	
;	ld b, (hl)
;	inc hl
;	ld c, (hl)
;	push bc
;	dec hl
;	call makeAddr
;	pop bc
;	ld (hl), c
;	dec hl
;	ld (hl), b
;	ret

;write:
;	call getAddr;
;	call receiveByte
;	ld (de), a
;	ret
	
plusRead:
	ld a, (de)
	ld hl, addrBuild
	ld (hl), a
	call makeByte
	call sendByte
	inc de;
    ret

plusWrite:
	call getByte
	ld (de), a
	inc de;
    ret

readLarge:
	ld b, 0;loop counter
	ld hl, addrBuild
	
	ld a, e
    ld c, $0F
	and c;16 byte boundary
	ld e, a
	
	ld a, (de)
	ld (hl), a
	
	call makeByte
	call sendByte
	
	ret

receiveByte:
    ret

sendByte:
    ret

makeByte:
	;call receiveByte
    rld
    call nibbleToHex
    
    ;ld h, a
    ld b, a
    
    ;call receiveByte
    rld
    call nibbleToHex
    
    ;ld l, a
    ld c, a
	ret

makeAddr:
    ld hl, addrBuild
    call makeByte
    ;ex de, hl
    ld d, b
    ld e, c
    inc hl
    
    call makeByte
	ret

getByte:
    call receiveByte
    call hexToNibble
    
    add a, a
    add a, a
    add a, a
    add a, a
    
    ld l, a
    
    call receiveByte
    call hexToNibble
    
    or l
    ld l, a
    
	ret

getAddr:
    call getByte
    ld h, l
    
    call getByte
    ex de, hl
    
    ret

hexToNibble:;regA = input, regA low nibble = output
	ld c, 10
	sub '0'
	cp c
	jp m, hex09
	
	sub 7
	
hex09:
	ret

nibbleToHex:
	and $0F
	ld c, 10
	cp c
	jp m, hex09n
	
	add a, 7
	
hex09n:
	add a, '0'
	ret