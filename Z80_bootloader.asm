;//00 = execute, 01 = set addr, 02 = read, 03 = write
    jp nextByte
.org 100
jumpTable:
.dw execute,getAddr,plusRead,plusWrite
addrBuild:
.dw $ABCD
nextByte:
	ld hl, nextByte;
	push hl;
	
	call receiveByte;

	ld hl, jumpTable;
	ld b, 0;
	add a, a
	ld c, a;
	add hl, bc;

	ld b, (hl)
	inc hl;
	ld h, (hl)
	ld l, b
	jp (hl);//jumps to hl not (hl)
	
	ret;
	
execute:
	;push hl;
	call getAddr
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
    inc de;
    ld a, (de)
	ld hl, addrBuild
	ld (hl), a
	call makeByte
    call sendByte
    ret

plusWrite:
    inc de;
    call getByte
	ld (de), a
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