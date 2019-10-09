what the bootstrap loader needs to work.
It needs to:

    initialize a memory pointer
    read a byte coming from port 00?
    load it to memory
    change the memory address
    loop back and repeat
    break out of this loop to execute the code



.org        $0000;program start at 00h,,4A4152 01FF00DB00BA28FB02570D20F6
	ld c,d;ASCII 'J'
    ld b,c;ASCII 'A'
	ld d,d;ASCII 'R'
	
    ld bc,$00FF;load bc with 00FFh (255)
start:
	in a,($00);load a with byte from port 0
    cp d;subtract d from a set flags (a is not changed)
    jr z,start;jump to start if zero flag set
    ld (bc),a;store a into memory location bc
    ld d,a;load d with a
    dec c;decrement c
    jr nz, start;if not zero jump to start
	
;13 bytes ^


.org        $0000;program start at 00h,,4A41524152 01DB00BA28F602570D20F1
	ld c,d;ASCII 'J'
    ld b,c;ASCII 'A'
	ld d,d;ASCII 'R'
    ld b,c;ASCII 'A'
	ld d,d;ASCII 'R'
	
    ld bc,$00DB;load bc with 00DBh (219)
;start:	  ^
	;in a,($00);load a with byte from port 0 (DB00)
    cp d;subtract d from a set flags (a is not changed)
    jr z, 6;jump to start if zero flag set
    ld (bc),a;store a into memory location bc
    ld d,a;load d with a
    dec c;decrement c
    jr nz, 6;if not zero jump to start ;2nd stage bootloader overwrites destination byte
	
;11 bytes ^


.org        $0000;program start at 00h,,4A41524152 01DB00BA28FB02570D20F6
	ld c,d;ASCII 'J'
    ld b,c;ASCII 'A'
	ld d,d;ASCII 'R'
    ld b,c;ASCII 'A'
	ld d,d;ASCII 'R'

	.db $01;ld bc,$00DB;load bc with 00DBh (219)
start:
	.dw $00DB;in a,($00);load a with byte from port 0 (DB00)
    cp d;subtract d from a set flags (a is not changed)
    jr z, start;jump to start if zero flag set
    ld (bc),a;store a into memory location bc
    ld d,a;load d with a
    dec c;decrement c
    jr nz, start;if not zero jump to start ;2nd stage bootloader overwrites destination byte

;same as previous ^


.org        $0000;program start at 00h,,110F0001DB00BA28FB02577B0DB920F4

    ld de,$000F;load d with known value(00)
    ;ld e,end;load e with final bootloader address
    ld bc,$00DB;load bc with 00DBh (219)
;start:	  ^
	;in a,($00);load a with byte from port 0
    cp d;subtract d from a set flags (a is not changed)
    jr z, 4;jump to start if zero flag set
    ld (bc),a;store a into memory location bc
    ld d,a;load d with a
    ld a,e;load a with e
    dec c;decrement c
    cp c;compare c to a(e)
    jr nz, 4;if not zero jump to start
;end:

;16 bytes ^ but more robust


.org        $0000;program start at 00h,,160001DB00BA28FB02570D20F6

    ld d,$00;load d with known value(00)
	.db $01;ld bc,$00DB;load bc with 00DBh (219)
start:
	.dw $00DB;in a,($00);load a with byte from port 0 (DB00)
    cp d;subtract d from a set flags (a is not changed)
    jr z, start;jump to start if zero flag set
    ld (bc),a;store a into memory location bc
    ld d,a;load d with a
    dec c;decrement c
    jr nz, start;if not zero jump to start
;end:

;13 bytes ^


#define		ld_bc_00DB $01
#define		in_a_00 $00DB

.org        $0000;program start at 00h,,160001DB00BA28FB02570D20F6

    ld d,$00;load d with known value(00)
	.db ld_bc_00DB;load bc with 00DBh (219)
start:
	.dw in_a_00;load a with byte from port 0 (DB00)
    cp d;subtract d from a set flags (a is not changed)
    jr z, start;jump to start if zero flag set
    ld (bc),a;store a into memory location bc
    ld d,a;load d with a
    dec c;decrement c
    jr nz, start;if not zero jump to start
;end:

;13 bytes ^


.org        $0000;program start at 00h,,21FF00454CEDAA20FC

	ld hl,$00FF;set h to 00, l to ff
    ld b,l;set b to l(ff)
    ld c,h;set c to h(00)
start:
    ind;16 cycles | Reads the (C) port and writes the result to (HL), then decrements HL and decrements B
    jr nz,start;12/7 cycles

;9 bytes ^ not at all robust (max 23 cycles to get next byte ready for reading)



#define		PROGSTART $8000;start of program in ram
;fill unused bytes with $003F OR $3F00
.org        $0000;program start at 00h,,21FF8055DB00BA28FB77572D20F6E9

    ld hl,PROGSTART + $FF;load bc with 80FFh (33023)
    ld d,l;load d with known value l (FF)
start:
    in a,($00);load a with byte from port 0
    cp d;subtract d from a set flags (a is not changed)
    jr z, start;jump to start if zero flag set
    ld (hl),a;store a into memory location (hl)
    ld d,a;load d with a
    dec l;decrement l
    jr nz, start;if not zero jump to start
    jp (hl);jump to start of program (8000h)
;end:

;15 bytes ^ fairly robust
	
	
.org $0000
INIT:
	in a,($01);
    cp $01;
    jr z, INIT;
LENGTH:
	in a, ($01);
    cp $00;
    jr z, LENGTH;
    in a,($00);
    ld b,a;
LENGTH2:
	in a, ($01);
    cp $00;
    jr z, LENGTH2;
    in a,($00);
    ld c,a;
	;ld bc, $0000;
LOOP:
    in a, ($01);
    cp $00;
    jr z, LOOP;
    in a, ($00);
    ld (bc),a;
    dec bc;
    jr nz, LOOP;
	
	
	
	
	







//E = execute, W = write, R = read, + = auto-increment read/write
regSP = endOfProgram + ?;
setupSerial();
setupBanks();
while(true){
	regA = getByte();
	switch(regA){
		case '+'://+ / +dd
			switch(regD){
				case 'W':
					regHL++;
					regA = getByte();
					ld (regHL), regA;//address, data
					break;
				case 'R':
					regHL++;
					ld regA, (regHL);//address
					sendByte(regA);
					break;
			}
		case 'W'://Waaaadd
			regHL = getAddr();
			regA = getByte();
			ld (regHL), regA;//address, data
			regD = 'W';
			break;
		case 'R'://Raaaa
			regHL = getAddr();
			ld regA, (regHL);//address
			sendByte(regA);
			regD = 'R';
			break;
		case 'E'://Eaaaa
			regHL = getAddr();
			jump(regHL);
	}
}

byte flipByte(byte b){
	b = (b & 0xF0) >> 4 | (b & 0x0F) << 4;
	b = (b & 0xCC) >> 4 | (b & 0x33) << 4;
	b = (b & 0xAA) >> 4 | (b & 0x55) << 4;
	return b;
}

:nextByte
	regHL = nextByte;
	push regHL;
	
	regA = getByte();

	regE = '+';
	cmp regE;
	jmp nz notPlus;

	regA = regD;
	
	inc regHL;

	regE = 'W';
	cmp regE;
	jmp z plusWrite;

	regE = 'R';
	cmp regE;
	jmp z plusRead;

:notPlus
	push regAF;
	call getAddr();
	pop regAF;

	regE = 'W';
	cmp regE;
	jmp z write;

	regE = 'R';
	cmp regE;
	jmp z read;

	regE = 'E';
	cmp regE;
	jmp z execute;

	//jmp nextByte;
	rtn;

:plusWrite

:plusRead

:write

:read

:execute
	push regHL;
	rtn;













;//00 = execute, 01 = read, 02 = write, 03 = plusRead, 04 = plusWrite
    jp nextByte
.org 100
jumpTable:
.dw execute,read,write,plusRead,plusWrite
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

;	ld (jumpTableIndex + 1), hl;//self modifiying code to the rescue, only works when loaded in ram!
;jumpTableIndex:
;	;//whats the order of the addr bytes here? will i have to swap them for the previous instruction?
;	ld hl, ($0000);//dummy addr value to be loaded with correct value by previous instruction
;	push hl;
;	ret;

;	ld c, (hl)
;	inc hl
;	ld b, (hl)
;	push bc
;	ret;

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

read:
	;call getAddr;
	;ld a, (de)
	;call sendByte
	ld hl, addrBuild
	ld a, (hl)
	push af
	call makeByte
	pop af
	ld (hl), a
	
	ld b, (hl)
	inc hl
	ld c, (hl)
	push bc
	dec hl
	call makeAddr
	pop bc
	ld (hl), c
	dec hl
	ld (hl), b
	ret

write:
	call getAddr;
	call receiveByte
	ld (de), a
	ret
	
plusRead:
    inc de;
    ld a, (de)
    call sendByte
    ret

plusWrite:
    inc de;
    call receiveByte
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

;getAddrOLD:
;	ld hl, 0
;	ld b, l
;	
;nextDigit:
;	call receiveByte
;	call hexToNibble
;	
;	or l
;	ld l, a
;	inc b
;	ld a, 4
;	cp b
;	jp z, end
;	
;	add hl,hl
;	add hl,hl
;	add hl,hl
;	add hl,hl
;	jp nextDigit
;	
;end:
;	ex de, hl
;	ret

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












byte getByte(){
	
}

void sendByte(byte){
	
}

char getAddr(){
	regHL = 0;
	regD = 0;
	
	:nextDigit
	regA = getByte();
	call hexToNibble;
	//regE = 10;
	//sub '0';
	//cmp regE;
	//jmp m, hex09;
	
	//sub 7;
	
	//:hex09
	regA |= regL;
	regL = regA;
	regD++;
	regA = 4;
	cmp regD;
	jmp z end;
	
	regHL += regHL;
	regHL += regHL;
	regHL += regHL;
	regHL += regHL;
	jmp nextDigit;
	
	:end
	return;
}

double makeAddr(){//HL = input addr, BCDE = hex output
	regA = regL;
	call nibbleToHex;
	regB = regA;
	
	regA = regL;
	call nextNibble;
	call nibbleToHex;
	regC = regA;
	
	regA = regH;
	call nibbleToHex;
	regD = regA;
	
	regA = regH;
	call nextNibble;
	call nibbleToHex;
	regE = regA;

	return;
}

char makeByte(){//(HL) = input byte, BC = hex output
	rrd;
	call nibbleToHex;
	regB = regA;
	
	rrd;
	call nibbleToHex;
	regC = regA;
	
	return;
}

void nextNibble(){
	srl regA;
	srl regA;
	srl regA;
	srl regA;
	return;
}

void prevNibble(){
	sla regA;
	sla regA;
	sla regA;
	sla regA;
	return;
}

byte hexToNibble(){//regA = input, regA low nibble = output
	regE = 10;
	sub '0';
	cmp regE;
	jmp m, end;
	
	sub 7;
	
	:end
	return;
}

byte nibbleToHex(){//regA low nibble = input, regA = output
	and 0x0F;
	regE = 10;
	cmp regE;
	jmp p, hex09;
	
	add 7;
	
	:hex09
	add '0';
	return;
}

char getAddr(){
	regD = 0;
	
	while(true){
		regA = getByte();
		
		regA -= '0';
		if(regA < 10){
			goto hex;
		}else{
			regA -= 7;
		}
		
		:hex
		if(regD == 0 || regD == 2){
			regL = nibblesL[regA];
		}else if(regD == 1){
			regL |= nibblesH[regA];
			regH = regL;
		}else{
			regL |= nibblesH[regA];
			return regHL;
		}
	}
}

char getAddr(){
	regD = 0;
	
	while(true){
		regA = getByte();
		
		regA -= '0';
		if(regA < 10){
			goto hex;
		}else{
			regA -= 7;
		}
		
		:hex
		if(regD == 0 || regD == 2){
			regHL = nibblesL;
			regHL += regA;
			regE = [regHL];
		}else if(regD == 1){
			regHL = nibblesH;
			regHL += regA;
			regA = [regHL];
			regA += regE;
			regD = regA;
		}else{
			regHL = nibblesH;
			regHL += regA;
			regA = [regHL];
			regA += regE;
			return regHL;
		}
	}
}

char getAddr(){
	regD = 0;
	
	:nextDigit
	regA = getByte();
	
	regA -= '0';
	
	regL = 10;
	cmp regL;
	jmp m, hex;
	
	regA -= 7;
	
	:hex
	push(regAF);
	regA = regD;
	regL = 3;
	regB = 0;
	cmp regL;
	//jmp z highAddrHighNibble
	jmp nz pass;
	regB = 1;
	:pass
	regL = 1;
	cmp regL;
	jmp z highNibble
	
	pop(regAF);
	regHL = nibblesL;
	regHL += regA;
	regE = [regHL];
	jmp out;
	
	:highNibble
	pop(regAF);
	regHL = nibblesH;
	regHL += regA;
	regA = [regHL];
	regA += regE;
	regC = regA;
	regA = regB;
	or regA;
	regA = regC;
	jmp nz highAddrHighNibble;//flags are messed up
	
	:lowAddrHighNibble
	//pop(regAF);
	//regHL = nibblesH;
	//regHL += regA;
	//regA = [regHL];
	//regA += regE;
	addrH = regA;
	jmp out;
	
	:highAddrHighNibble
	//pop(regAF);
	//regHL = nibblesH;
	//regHL += regA;
	//regA = [regHL];
	//regA += regE;
	regL = regA;
	regH = addrH;
	return;
	
	:out
	inc regD;
	jmp nextDigit
}

char getAddr(){
	regD = 0;
	
	:nextDigit
	regA = getByte();
	
	regA -= '0';
	
	regL = 10;
	cmp regL;
	jmp m, hex;
	
	regA -= 7;
	
	:hex
	push(regAF);
	regA = regD;
	regL = 3;
	cmp regL;
	jmp z highAddrHighNibble
	regL = 1;
	cmp regL;
	jmp z lowAddrHighNibble
	
	pop(regAF);
	regHL = nibblesL;
	regHL += regA;
	regE = [regHL];
	jmp out;
	
	:highAddrHighNibble
	pop(regAF);
	regHL = nibblesH;
	regHL += regA;
	regA = [regHL];
	regA += regE;
	regC = regA;
	regHL = addrH;
	regB = [regHL];
	return;
	
	:lowAddrHighNibble
	pop(regAF);
	regHL = nibblesH;
	regHL += regA;
	regA = [regHL];
	regA += regE;
	regHL = addrH;
	[regHL] = regA;
	
	:out
	inc regD;
	jmp nextDigit
}

char getAddr(){//regD = nibble counter, regE = nibble dupe, regB = cmp reg, regBC = address output
	regD = 0;
	
	:nextDigit
	regA = getByte();
	
	regA -= '0';
	
	regL = 10;
	cmp regL;
	jmp m, hex;
	
	regA -= 7;
	
	:hex
	regE = regA;//dupe nibble
	
	regHL = nibblesH;//array start
	regHL += regA;//array index
	regA = [regHL];//get value
	regA += regE;//addres byte low/high
	
	regC = regA;//lower address byte
	
	regA = regD;//nibble counter
	regB = 3;
	cmp regB;
	jmp z highAddrHighNibble
	regB = 1;
	cmp regB;
	jmp z lowAddrHighNibble
	
	jmp out;
	
	:highAddrHighNibble
	regHL = addrH;
	regB = [regHL];
	return;
	
	:lowAddrHighNibble
	regHL = addrH;
	[regHL] = regE;
	
	:out
	inc regD;
	jmp nextDigit
}

byte addrH;
byte nibblesH[] = {0x00,0x10,0x20,0x30,0x40,0x50,0x60,0x70,0x80,0x90,0xA0,0xB0,0xC0,0xD0,0xE0,0xF0};
byte nibblesL[] = {0x00,0x01,0x02,0x03,0x04,0x05,0x06,0x07,0x08,0x09,0x0A,0x0B,0x0C,0x0D,0x0E,0x0F};





getAddr:
	ld hl, 0
	ld d, 0
	
nextDigit:
	;regA = getByte()
	call hexToNibble
	
	or l
	ld l, a
	inc d
	ld a, 4
	cp d
	jp z, end
	
	add hl,hl
    add hl,hl
    add hl,hl
	add hl,hl
	jp nextDigit
	
end:
	ret

hexToNibble:;regA = input, regA low nibble = output
	ld e, 10
	sub '0'
	cp e
	jp m, hex09
	
	sub 7
	
hex09:
	ret

nibbleToHex:
	and $0F
	ld e, 10
	cp e
	jp p, hex09n
	
	add a, 7
	
hex09n:
	add a, '0'
	ret
