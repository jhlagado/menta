; *************************************************************************
;
;      MINT Minimal Interpreter for the Z80 
;
;      Ken Boak, John Hardy and Craig Jones. 
;
;      GNU GENERAL PUBLIC LICENSE                   Version 3, 29 June 2007
;
;      see the LICENSE file in this repo for more information 
;
; *****************************************************************************

        DSIZE       EQU $80
        RSIZE       EQU $80
        TIBSIZE     EQU $100
        TRUE        EQU 1
        FALSE       EQU 0

        NUMGRPS     EQU 5
        GRPSIZE     EQU $40
        
        varsOfs     EQU ((VARS - mintVars)/2) - "a"
        sysvarsOfs  EQU ((sysVars - mintVars)/2) -"a" 


; **************************************************************************
; Page 0  Initialisation
; **************************************************************************		

		.ORG ROMSTART + $180		

start:
        LD SP,DSTACK
        CALL initialize
        CALL printStr
        .cstr "MINT V1.1\r\n"
        JP interpret

; ***********************************************************************
; Initial values for user mintVars		
; ***********************************************************************		
iSysVars:
        DW dStack               ; a vS0
        DW FALSE                ; b vBase16
        DW 0                    ; c vTIBPtr
        DW DEFS                 ; d vDEFS
        DW 0                    ; e vEdited the last command to be edited
        DW 0                    ; f 
        DW 0                    ; g 
        DW HEAP                 ; h vHeapPtr

initialize:
        LD HL,iSysVars
        LD DE,sysVars
        LD BC,8 * 2
        LDIR
        
        LD HL,DEFS
        LD B,GRPSIZE/2 * NUMGRPS
init1:
        LD (HL),lsb(empty_)
        INC HL
        LD (HL),msb(empty_)
        INC HL
        DJNZ init1
        LD IX,RSTACK
        LD IY,NEXT			    ; IY provides a faster jump to NEXT
        RET

macro:                          ; 25
        PUSH DE
        LD (vTIBPtr),BC
        LD HL,ctrlCodes
        ADD A,L
        LD L,A
        LD E,(HL)
        LD D,msb(macros)
        PUSH DE                 ; give GO something to consume
        CALL enter
        .cstr "\\G"
        LD BC,(vTIBPtr)
        POP DE
        JR interpret2

interpret:
        PUSH DE
        call prompt
        LD BC,0                 ; load BC with offset into TIB         
        LD (vTIBPtr),BC

interpret2:                     ; calc nesting (a macro might have changed it)
        LD E,0                  ; initilize nesting value
        PUSH BC                 ; save offset into TIB, 
                                ; BC is also the count of chars in TIB
        LD HL,TIB               ; HL is start of TIB
        JR interpret4

interpret3:
        LD A,(HL)               ; A = char in TIB
        INC HL                  ; inc pointer into TIB
        DEC BC                  ; dec count of chars in TIB
        call nesting            ; update nesting value

interpret4:
        LD A,C                  ; is count zero?
        OR B
        JR NZ, interpret3       ; if not loop
        POP BC                  ; restore offset into TIB

; *******************************************************************         
; Wait for a character from the serial input (keyboard) 
; and store it in the text buffer. Keep accepting characters,
; increasing the instruction pointer BC - until a newline received.
; *******************************************************************

waitchar:   
        CALL getchar            ; loop around waiting for character
        CP $20
        JR NC,waitchar1
        CP $0                   ; is it end of string?
        JR Z,waitchar4
        CP '\r'                 ; carriage return?
        JR Z,waitchar3
        JR macro    

waitchar1:
        LD HL,TIB
        ADD HL,BC
        LD (HL),A               ; store the character in textbuf
        INC BC
        CALL putchar            ; echo character to screen
        CALL nesting
        JR  waitchar            ; wait for next character

waitchar3:
        LD HL,TIB
        ADD HL,BC
        LD (HL),"\r"            ; store the crlf in textbuf
        INC HL
        LD (HL),"\n"            
        INC HL                  
        INC BC
        INC BC
        CALL crlf               ; echo character to screen
        LD A,E                  ; if zero nesting append an ETX after \r
        OR A
        JR NZ,waitchar
        LD (HL),$03             ; store end of text ETX in text buffer 
        INC BC

waitchar4:
        LD (vTIBPtr),BC
        LD BC,TIB               ; Instructions stored on heap at address HERE
        DEC BC
        POP DE                  ; restore TOS
                                ; Drop into the NEXT and dispatch routines

; ********************************************************************************
;
; Dispatch Routine.
;
; Get the next character and form a 1 byte jump address
;
; This target jump address is loaded into HL, and using JP (HL) to quickly 
; jump to the selected function.
;
; Individual handler routines will deal with each category:
;
; 1. Detect characters A-Z and jump to the User Command handler routine
;
; 2. Detect characters a-z and jump to the variable handler routine
;
; 3. All other characters are punctuation and cause a jump to the associated
; primitive code.
;
; Instruction Pointer IP BC is incremented
;
; *********************************************************************************
NEXT:                               ; 9 
        INC BC                      ; Increment the IP
        LD A,(BC)                   ; Get the next character and dispatch
        LD L,A                      ; Index into table
        LD H,msb(opcodes)           ; Start address of jump table         
        LD L,(HL)                   ; get low jump address
        LD H,msb(codePage)             ; Load H with the 1st page address
        JP (HL)                     ; Jump to routine

enter:                              ;=9
        LD HL,BC
        CALL rpush                  ; save Instruction Pointer
        POP BC
        DEC BC
        JP  (IY)                    ; Execute code in user command

; ARRAY compilation routine
compNEXT:                       ; 19
        LD HL,(vHeapPtr)        ; load heap ptr
        LD (HL),E               ; store lsb
        LD A,(vByteMode)
        INC HL          
        OR A
        JR NZ,compNext1
        LD (HL),D
        INC HL
compNext1:
        POP DE
        LD (vHeapPtr),HL        ; save heap ptr
        JR NEXT

nesting:                            ;=46 
        BIT 7,E             
        JR NZ,nesting1
        CP '`'
        JR Z, nesting0
        JR nesting1a
nesting1:
        CP '`'
        JR NZ,nesting1a
nesting0:
        LD A,$80
        XOR E                       ; flip bit 7
        LD E,A
        RET 
nesting1a:
        CP ':'
        JR Z,nesting2
        CP '['
        JR Z,nesting2
        CP '('
        JR NZ,nesting3
nesting2:
        INC E
        RET 
nesting3:
        CP ';'
        JR Z,nesting4
        CP ']'
        JR Z,nesting4
        CP ')'
        RET NZ
nesting4:
        DEC E
        RET 

prompt:                             ;=9
        call printStr
        .cstr "\r\n> "
        RET
        

; **************************************************************************
; Macros must be written in Mint and end with ; 
; this code must not span pages
; **************************************************************************
macros:

.include "MINT-macros.asm"

; **************************************************************************
; Page 2  Jump Tables
; **************************************************************************
        .align $100
opcodes:
; ***********************************************************************
; Initial values for user mintVars		
; ***********************************************************************		
        DB    lsb(exit_)        ;   NUL 
        DB    lsb(nop_)         ;   SOH 
        DB    lsb(nop_)         ;   STX 
        DB    lsb(etx_)         ;   ETX 
        DB    lsb(nop_)         ;   EOT 
        DB    lsb(nop_)         ;   ENQ 
        DB    lsb(nop_)         ;   ACK 
        DB    lsb(nop_)         ;   BEL 
        DB    lsb(nop_)         ;   BS  
        DB    lsb(nop_)         ;   TAB 
        DB    lsb(nop_)         ;   LF  
        DB    lsb(nop_)         ;   VT  
        DB    lsb(nop_)         ;   FF  
        DB    lsb(nop_)         ;   CR  
        DB    lsb(nop_)         ;   SO  
        DB    lsb(nop_)         ;   SI  
        DB    lsb(nop_)         ;   DLE 
        DB    lsb(nop_)         ;   DC1 
        DB    lsb(nop_)         ;   DC2 
        DB    lsb(nop_)         ;   DC3 
        DB    lsb(nop_)         ;   DC4 
        DB    lsb(nop_)         ;   NAK 
        DB    lsb(nop_)         ;   SYN 
        DB    lsb(nop_)         ;   ETB 
        DB    lsb(nop_)         ;   CAN 
        DB    lsb(nop_)         ;   EM  
        DB    lsb(nop_)         ;   SUB 
        DB    lsb(nop_)         ;   ESC 
        DB    lsb(nop_)         ;   FS  
        DB    lsb(nop_)         ;   GS  
        DB    lsb(nop_)         ;   RS  
        DB    lsb(nop_)         ;   US  
        DB    lsb(nop_)         ;   SP
        DB    lsb(store_)       ;   !   ( val adr -- ) store          
        DB    lsb(dup_)         ;   "   ( n -- n n ) duplicate
        DB    lsb(hex_)         ;   #   hexadecimal number
        DB    lsb(swap_)        ;   $   ( a b -- b a ) swap         
        DB    lsb(over_)        ;   %   ( a b -- a b a ) over        
        DB    lsb(and_)         ;   &   ( n n -- n ) and
        DB    lsb(drop_)        ;   '   ( n -- ) drop
        DB    lsb(begin_)       ;   (   ( n -- ) loop n times    
        DB    lsb(again_)       ;   )   end of loop
        DB    lsb(mul_)         ;   *   multiply         
        DB    lsb(add_)         ;   +   add
        DB    lsb(hdot_)        ;   ,   print hex         
        DB    lsb(sub_)         ;   -   subtract
        DB    lsb(dot_)         ;   .   print
        DB    lsb(div_)         ;   /   divide
        DB    lsb(num_)         ;   0   number         
        DB    lsb(num_)         ;   1   number        
        DB    lsb(num_)         ;   2   number            
        DB    lsb(num_)         ;   3   number
        DB    lsb(num_)         ;   4   number            
        DB    lsb(num_)         ;   5   number            
        DB    lsb(num_)         ;   6   number            
        DB    lsb(num_)         ;   7   number
        DB    lsb(num_)         ;   8   number            
        DB    lsb(num_)         ;   9   number        
        DB    lsb(def_)         ;   :   start of command definition       
        DB    lsb(ret_)         ;   ;   end of command definition
        DB    lsb(lt_)          ;   <   less than
        DB    lsb(eq_)          ;   =   equal to         
        DB    lsb(gt_)          ;   >   greater than         
        DB    lsb(getRef_)      ;   ?   get reference to command
        DB    lsb(fetch_)       ;   @   fetch 
        DB    lsb(call_)        ;   A   call command 
        DB    lsb(call_)        ;   B
        DB    lsb(call_)        ;   C
        DB    lsb(call_)        ;   D    
        DB    lsb(call_)        ;   E
        DB    lsb(call_)        ;   F
        DB    lsb(call_)        ;   G
        DB    lsb(call_)        ;   H
        DB    lsb(call_)        ;   I
        DB    lsb(call_)        ;   J
        DB    lsb(call_)        ;   K
        DB    lsb(call_)        ;   L
        DB    lsb(call_)        ;   M
        DB    lsb(call_)        ;   N
        DB    lsb(call_)        ;   O
        DB    lsb(call_)        ;   P
        DB    lsb(call_)        ;   Q
        DB    lsb(call_)        ;   R
        DB    lsb(call_)        ;   S
        DB    lsb(call_)        ;   T
        DB    lsb(call_)        ;   U
        DB    lsb(call_)        ;   V
        DB    lsb(call_)        ;   W
        DB    lsb(call_)        ;   X
        DB    lsb(call_)        ;   Y
        DB    lsb(call_)        ;   Z   call command
        DB    lsb(arrDef_)      ;   [   start an array
        DB    lsb(alt_)         ;   \   alternate code
        DB    lsb(arrEnd_)      ;   ]   end of array
        DB    lsb(xor_)         ;   ^   code of char
        DB    lsb(neg_)         ;   _   negate
        DB    lsb(str_)         ;   `   print string         
        DB    lsb(var_)         ;   a   variable reference
        DB    lsb(var_)         ;   b
        DB    lsb(var_)         ;   c
        DB    lsb(var_)         ;   d
        DB    lsb(var_)         ;   e
        DB    lsb(var_)         ;   f
        DB    lsb(var_)         ;   g
        DB    lsb(var_)         ;   h
        DB    lsb(var_)         ;   i            
        DB    lsb(var_)         ;   j
        DB    lsb(var_)         ;   k
        DB    lsb(var_)         ;   l
        DB    lsb(var_)         ;   m
        DB    lsb(var_)         ;   n
        DB    lsb(var_)         ;   o
        DB    lsb(var_)         ;   p
        DB    lsb(var_)         ;   q            
        DB    lsb(var_)         ;   r
        DB    lsb(var_)         ;   s    
        DB    lsb(var_)         ;   t
        DB    lsb(var_)         ;   u
        DB    lsb(var_)         ;   v
        DB    lsb(var_)         ;   w
        DB    lsb(var_)         ;   x
        DB    lsb(var_)         ;   y
        DB    lsb(var_)         ;   z   variable reference
        DB    lsb(shl_)         ;   {   shift left
        DB    lsb(or_)          ;   |   or         
        DB    lsb(shr_)         ;   }   shift right         
        DB    lsb(inv_)         ;   ~   invert         
        DB    lsb(nop_)         ;   backspace
                                
        
; ***********************************************************************
; Alternate function codes		
; ***********************************************************************		
ctrlCodes:
altCodes:
        DB     lsb(empty_)      ;   NUL ^@
        DB     lsb(empty_)      ;   SOH ^A
        DB     lsb(toggleBase_) ;   STX ^B
        DB     lsb(empty_)      ;   ETX ^C
        DB     lsb(empty_)      ;   EOT ^D
        DB     lsb(edit_)       ;   ENQ ^E
        DB     lsb(empty_)      ;   ACK ^F
        DB     lsb(empty_)      ;   BEL ^G
        DB     lsb(backsp_)     ;   BS  ^H
        DB     lsb(empty_)      ;   TAB ^I
        DB     lsb(reedit_)     ;   LF  ^J
        DB     lsb(empty_)      ;   VT  ^K
        DB     lsb(list_)       ;   FF  ^L
        DB     lsb(empty_)      ;   CR  ^M
        DB     lsb(empty_)      ;   SO  ^N
        DB     lsb(empty_)      ;   SI  ^O
        DB     lsb(printStack_) ;   DLE ^P
        DB     lsb(empty_)      ;   DC1 ^Q
        DB     lsb(empty_)      ;   DC2 ^R
        DB     lsb(empty_)      ;   DC3 ^S
        DB     lsb(empty_)      ;   DC4 ^T
        DB     lsb(empty_)      ;   NAK ^U
        DB     lsb(empty_)      ;   SYN ^V
        DB     lsb(empty_)      ;   ETB ^W
        DB     lsb(empty_)      ;   CAN ^X
        DB     lsb(empty_)      ;   EM  ^Y
        DB     lsb(empty_)      ;   SUB ^Z
        DB     lsb(empty_)      ;   ESC ^[
        DB     lsb(empty_)      ;   FS  ^\
        DB     lsb(empty_)      ;   GS  ^]
        DB     lsb(empty_)      ;   RS  ^^
        DB     lsb(empty_)      ;   US  ^_)
        DB     lsb(aNop_)       ;   SP  ^`
        DB     lsb(cStore_)     ;   !   byte store          
        DB     lsb(aNop_)       ;   "
        DB     lsb(aNop_)       ;   #
        DB     lsb(aNop_)       ;   $   ( -- adr ) text input ptr           
        DB     lsb(aNop_)       ;   %            
        DB     lsb(aNop_)       ;   &
        DB     lsb(aNop_)       ;   '
        DB     lsb(ifte_)       ;   (   ( b -- ) if...then...else             
        DB     lsb(aNop_)       ;   )
        DB     lsb(aNop_)       ;   *            
        DB     lsb(incr_)       ;   +   ( adr -- ) decrements variable at address
        DB     lsb(aNop_)       ;   ,            
        DB     lsb(aNop_)       ;   -  
        DB     lsb(aNop_)       ;   .  
        DB     lsb(aNop_)       ;   /
        DB     lsb(aNop_)       ;   0           
        DB     lsb(aNop_)       ;   1  
        DB     lsb(aNop_)       ;   2            
        DB     lsb(aNop_)       ;   3  
        DB     lsb(aNop_)       ;   4            
        DB     lsb(aNop_)       ;   5            
        DB     lsb(aNop_)       ;   6            
        DB     lsb(aNop_)       ;   7
        DB     lsb(aNop_)       ;   8            
        DB     lsb(aNop_)       ;   9        
        DB     lsb(aNop_)       ;   :   start defining a macro        
        DB     lsb(aNop_)       ;   ;  
        DB     lsb(aNop_)       ;   <
        DB     lsb(aNop_)       ;   =            
        DB     lsb(aNop_)       ;   >            
        DB     lsb(aNop_)       ;   ?
        DB     lsb(cFetch_)     ;   @   byte fetch     
        DB     lsb(aNop_)       ;   A    
        DB     lsb(break_)      ;   B   break loop
        DB     lsb(nop_)        ;   C
        DB     lsb(depth_)      ;   D   ( -- val ) depth of data stack  
        DB     lsb(emit_)       ;   E   ( val -- ) emits a char to output
        DB     lsb(aNop_)       ;   F
        DB     lsb(go_)         ;   G   ( adr -- ) execute mint definition
        DB     lsb(aNop_)       ;   H  
        DB     lsb(inPort_)     ;   I   ( port -- val ) inout from port  
        DB     lsb(aNop_)       ;   J
        DB     lsb(key_)        ;   K   ( -- val )  read a char from input
        DB     lsb(aNop_)       ;   L  
        DB     lsb(aNop_)       ;   M  
        DB     lsb(newln_)      ;   N   prints a newline to output
        DB     lsb(outPort_)    ;   O   ( val port -- )
        DB     lsb(printStk_)   ;   P   ( -- ) non-destructively prints stack
        DB     lsb(aNop_)       ;   Q   quits from Mint REPL
        DB     lsb(rot_)        ;   R   ( a b c -- b c a )
        DB     lsb(aNop_)       ;   S
        DB     lsb(typeStr_)    ;   T   ( adr -- ) prints a null terminated string
        DB     lsb(aNop_)       ;   U
        DB     lsb(aNop_)       ;   V
        DB     lsb(aNop_)       ;   W   ( b -- ) if false, skip to end of loop
        DB     lsb(exec_)       ;   X
        DB     lsb(aNop_)       ;   Y
        DB     lsb(editDef_)    ;   Z
        DB     lsb(cArrDef_)    ;   [
        DB     lsb(comment_)    ;   \   comment text, skips reading until end of line
        DB     lsb(aNop_)       ;   ]
        DB     lsb(charCode_)   ;   ^
        DB     lsb(aNop_)       ;   _ 
        DB     lsb(strDef_)     ;   `   ( -- adr ) allocates a null terminated string          
        DB     lsb(sysVar_)     ;   a   sys var: start of data stack variable
        DB     lsb(sysVar_)     ;   b   sys var: base16 variable
        DB     lsb(sysVar_)     ;   c   sys var: TIBPtr variable
        DB     lsb(sysVar_)     ;   d  
        DB     lsb(sysVar_)     ;   e  
        DB     lsb(sysVar_)     ;   f
        DB     lsb(sysVar_)     ;   g  
        DB     lsb(sysVar_)     ;   h   sys var: heap ptr variable
        DB     lsb(i_)          ;   i   sys var: returns index variable of current loop          
        DB     lsb(j_)          ;   j   sys var: returns index variable of outer loop
        DB     lsb(sysVar_)     ;   k  
        DB     lsb(sysVar_)     ;   l
        DB     lsb(sysVar_)     ;   m   
        DB     lsb(sysVar_)     ;   n  
        DB     lsb(sysVar_)     ;   o
        DB     lsb(sysVar_)     ;   p  
        DB     lsb(sysVar_)     ;   q           
        DB     lsb(sysVar_)     ;   r
        DB     lsb(sysVar_)     ;   s 
        DB     lsb(sysVar_)     ;   t
        DB     lsb(sysVar_)     ;   u
        DB     lsb(sysVar_)     ;   v   
        DB     lsb(sysVar_)     ;   w   
        DB     lsb(sysVar_)     ;   x
        DB     lsb(sysVar_)     ;   y
        DB     lsb(sysVar_)     ;   z   sys var: 
        DB     lsb(group_)      ;   {
        DB     lsb(aNop_)       ;   |            
        DB     lsb(endGroup_)   ;   }            
        DB     lsb(aNop_)       ;   ~           
        DB     lsb(aNop_)       ;   BS		


; **********************************************************************			 
; code page primitive routines 
; **********************************************************************
        .align $100
codePage:

alt_:        
        JP alt
and_:        
        POP HL                      ; HL=NOS DE=TOS
        LD A,E         
        AND L           
        LD E,A         
        LD A,D         
        AND H           
        LD D,A         
        JP (IY)        
                            
or_: 		 
        POP HL                      ; HL=NOS DE=TOS
        LD A,E         
        OR L           
        LD E,A         
        LD A,D         
        OR H           
        LD D,A         
        JP (IY)        

inv_:						        
        LD HL,$FFFF                 
        JR xor1        

xor_:		 
        POP HL                      ; HL=NOS DE=TOS
xor1:
        LD A,E
        XOR L
        LD E,A
        LD A,D
        XOR D
        LD D,A         
        JP (IY)        

add_:                          
        POP HL                      ; HL=NOS DE=TOS            
        ADD HL,DE                   ; NOS+TOS
        EX DE,HL                    
        JP (IY)           

arrDef_:    
arrDef:                             ;= 18
        LD A,FALSE
arrDef1:      
        LD IY,compNEXT
        LD (vByteMode),A
        LD HL,(vHeapPtr)            ; HL = heap ptr
        CALL rpush                  ; save start of array \[  \]
        JP NEXT                     ; hardwired to NEXT

arrEnd_:    
        JP arrEnd        

begin_:     
        JP begin        

call_:
        LD HL,BC
        CALL rpush                  ; save Instruction Pointer
        LD A,(BC)
        CALL lookupDef1
        LD C,(HL)
        INC HL
        LD B,(HL)
        DEC BC
        JP  (IY)                    ; Execute code from User def

def_:   
        JP def        

hdot_:                              ; print hexadecimal
        EX DE,HL
        CALL printhex
        JR dot2
dot_:       
        EX DE,HL
        CALL printdec
dot2:
        POP DE                      ; consume TOS
        LD A," "                    ; print space
        CALL putchar
        JP (IY)

drop_:                      
        POP DE
        JP (IY)

dup_:        
        PUSH DE
        JP (IY)

etx_:
etx:
        LD HL,-DSTACK
        ADD HL,SP
        JP NC,etx1
        LD SP,DSTACK
etx1:
        JP interpret

exit_:
        INC BC
        PUSH BC
        CALL rpop               
        LD BC,HL
        RET
        
fetch_:                           
        EX DE,HL
fetch1:
        LD E,(HL)      
        INC HL          
        LD D,(HL)      
        JP (IY)        

hex_:   
        JP hex        

nop_:   
        JP NEXT                     ; hardwire white space to always go to NEXT (important for arrays)

num_:   
        JP num        

over_:  
        POP HL                      ; HL=NOS
        PUSH HL
        PUSH DE
        EX DE,HL                
        JP (IY)        
    
ret_:
        CALL rpop                   ; Restore Instruction pointer
        LD BC,HL                
        JP (IY)             

store_:                     
        EX DE,HL                    ; HL = TOS
        POP DE                      ; DE = NOS
        LD (HL),E       
        INC HL           
        LD (HL),D       
        POP DE                      ; DE = TOS
        JP (IY)         

swap_:        
        EX DE,HL
        EX (SP),HL
        EX DE,HL
        JP (IY)

shl_:   
        EX DE,HL                
        ADD HL,HL                   ; shift left
        EX DE,HL                
        JP (IY)                 
    
shr_:    
        EX DE,HL                
shr1:
        SRL H                       ; div HL by 2
        RR L
        EX DE,HL                    ; result in TOS
        JP (IY)                 

neg_:   
        LD HL,0    		        
        JR sub2                 

sub_:       				     
sub1:  
        POP HL                  
sub2:  
        OR A                    
        SBC HL,DE               
        EX DE,HL                    
        JP (IY)                 
                                
eq_:    
        POP HL                      ; DE=TOS HL=NOS
        OR A                        ; reset the carry flag
        SBC HL,DE                   ; NOS-TOS if equal HL=0
        EX DE,HL
        JR Z,eq1
eq0:                                ; if false
        LD DE,0
        JP (IY)
eq1:                                ; if true
        LD DE,1
        JP (IY)

getRef_:    
        JP getRef        

gt_:    
        POP HL
        EX DE,HL
        JR lt1

lt_:    
        POP HL
lt1:
        OR A                        ; reset the carry flag
        SBC HL,DE                   ; only equality sets HL=0 here
        JR C,eq1
        JR eq0

var_:
        LD A,varsOfs  
var1:
        LD H,B
        LD L,C
        ADD A,(HL)
        ADD A,A
        PUSH DE                     ; push TOS
        LD E,A                      ; TOS = ptr to var
        LD D,msb(mintVars)
        JP (IY)
        
div_:   
        JP div        

mul_:   
        JP mul        

again_:     
        JP again        

str_:                       
str:                                ;= 17
        INC BC
        
str1:            
        LD A, (BC)
        INC BC
        CP "`"                      ; ` is the string terminator
        JR Z,str2
        CALL putchar
        JR str1

str2:  
        DEC BC
        JP (IY) 

;*******************************************************************
; Code commands Commands continued
;*******************************************************************

again:                              ;=57
        PUSH DE                     ; save TOS
        LD E,(IX+0)                 ; peek loop var
        LD D,(IX+1)                 
        LD A,E                      ; check if IFTEMode (-1)
        AND D
        INC A
        JR NZ,again1
        INC IX                      ; drop IFTEMode
        INC IX
        LD DE,0                     ; TOS=FALSE
        JP (IY)

again1:
        LD L,(IX+2)                 ; peek loopLimit
        LD H,(IX+3)                 
        DEC HL                      ; loopLimit-1
        OR A
        SBC HL,DE                   ; loopVar == loopLimit-1 ?
        JR Z,again2
        INC DE                      ; no  so update frame and loop back
        LD (IX+0),E                 ; poke loop var
        LD (IX+1),D                 
        LD C,(IX+4)                 ; peek loop address
        LD B,(IX+5)  
        POP DE                      ; restore TOS
        JP (IY)

again2:
        LD DE,6                     ; yes drop loop frame
        ADD IX,DE
        POP DE                      ; restore TOS
        JP (IY)

arrEnd:                             ;=29
        PUSH DE                     ; save TOS
        CALL rpop                   ; HL=start of array
        EX DE,HL                    ; DE=start of array 
        PUSH DE                     ; save start of array
        LD HL,(vHeapPtr)            ; HL = heap ptr
        OR A
        SBC HL,DE                   ; bytes on heap 
        LD A,(vByteMode)
        OR A
        JR NZ,arrEnd2               ; if not byte mode div by 2
        SRL H                       ; BC = m words
        RR L
arrEnd2:
        EX DE,HL                    ; TOS=length 
        LD IY,NEXT                  ; restore IY
        JP (IY)                     ; hardwired to NEXT

                                    ;=28
begin:                              ;=43
        LD A,E                      ; is TOS zero?
        OR D
        JR NZ,begin3                ; if zero skip to past closing paren
begin1:
        LD E,1                      ; we have already seen an opening paren
begin2:
        INC BC
        LD A,(BC)
        CALL nesting                ; keep nesting balanced
        XOR A
        OR E
        JR NZ,begin2
        JR begin4
begin3:
        EX DE,HL                    ; HL=loop limit
        LD DE,-6
        ADD IX,DE
        LD (IX+0),0                 ; loopVar
        LD (IX+1),0                 
        LD (IX+2),L                 ; loopLimit
        LD (IX+3),H                 
        LD (IX+4),C                 ; loopAddress
        LD (IX+5),B                 
begin4:
        POP DE                      ; consume
        JP (IY)

def:                                ; Create a colon definition
        INC BC
        LD  A,(BC)                  ; Get the next character
        CALL lookupDef
        PUSH DE                     ; save return SP
        LD DE,(vHeapPtr)            ; start of defintion  
        LD (HL),E                   ; Save low byte of address in CFA
        INC HL              
        LD (HL),D                   ; Save high byte of address in CFA+1
        EX DE,HL                    ; HL=HeapPtr
        POP DE                      ; restore return SP
def1:                               ; Skip to end of definition   
        INC BC                      ; Point to next character
        LD A,(BC)                   ; Get the next character
        LD (HL),A                   ; write to definition
        INC HL
        CP ";"                      ; Is it a semicolon 
        JR NZ, def1                 ; end the definition
        LD (vHeapPtr),HL            ; bump heap ptr to after definiton
        JP (IY)       

div:                                ;=33
        POP HL                      ; HL=NOS DE=TOS
        PUSH BC                     ; Preserve the IP
        LD BC,HL                    ; BC = 2nd value
        LD HL,0    	                ; zero the remainder
        LD A,16    	                ; loop counter
div1:		                        ; shift the bits from BC (numerator) into HL (accumulator)
        SLA C
        RL B
        ADC HL,HL
        SBC HL,DE			        ; check if remainder >= denominator (HL>=DE)
        JR C,div2
        INC C
        JR div3
div2:		                        ; remainder is not >= denominator, so we have to add DE back to HL
        ADD HL,DE
div3:
        DEC A
        JR NZ,div1
        LD DE,BC                    ; result from BC to DE
        POP BC
        PUSH DE                     ; push Result
        EX DE,HL                    ; TOS=remainder             
        JP (IY)

hex:                                ;= 30
        PUSH DE                     ; push down TOS
	    LD HL,0		    		    ; Clear HL to accept the number
hex1:
        INC BC
        LD A,(BC)				    ; Get the character which is a numeral
        BIT 6,A                     ; is it uppercase alpha?
        JR Z, hex2                  ; no a decimal
        SUB 7                       ; sub 7  to make $A - $F
hex2:
        SUB $30                     ; Form decimal digit
        JP C,num2
        CP $0F+1
        JP NC,num2
        ADD HL,HL                   ; 2X ; Multiply digit(s) in HL by 16
        ADD HL,HL                   ; 4X
        ADD HL,HL                   ; 8X
        ADD HL,HL                   ; 16X     
        ADD A,L                     ; Add into bottom of HL
        LD  L,A             
        JR  hex1

mul:                                ;=27
        POP HL                      ; HL=NOS DE=TOS
        PUSH BC                     ; Preserve the IP
        LD BC,HL                    ; BC = 2nd value
        LD HL,0
        LD A,16
mul1:
        ADD HL,HL
        RL E
        RL D
        JR NC,mul2          
        ADD HL,BC
        JR NC,mul2          
        INC DE
mul2:
        DEC A
        JR NZ,mul1
        EX DE,HL
        POP BC
		JP (IY)

; **************************************************************************
; Alt code primitives
; **************************************************************************
        .align $100
altCodePage:

break_:
        LD A,E                      ; zero?
        OR D
        JR NZ,break1
        POP DE                      ; consume
        JP (IY)
break1:
        LD DE,6                     ; drop loop frame
        ADD IX,DE
        POP DE                      ; consume
        JP begin1                   ; skip to end of loop        

cArrDef_:                   
        LD A,TRUE
        JP arrDef1

cFetch_:
        EX DE,HL
        LD D,0         
        LD E,(HL)      
aNop_:
        JP (IY)             
                             
charCode_:
        INC BC
        LD A,(BC)
        PUSH DE
        LD D,0
        LD E,A
        JP (IY)

comment_:
        JP (IY)        

cStore_:	  
        EX DE,HL           
        POP DE           
        LD (HL),E
        POP DE
        JP (IY)         
                            
depth_:
        LD HL,0
        ADD HL,SP
        PUSH DE                     ; push down TOS
        EX DE,HL                    ; DE=SP
        LD HL,DSTACK                ; HL=SP0
        OR A
        SBC HL,DE                   ; SP0 - SP
        JP shr1

emit_:
        LD A,E
        POP DE                      ; consume
        CALL putchar
        JP (IY)

ifte_:
        LD A,E
        OR D
        JP NZ,ifte1
        INC DE
        JP begin1                   ; skip to closing ) works with \) too 
ifte1:
        LD HL,-1                    ; push -1 on return stack to indicate IFTEMode
        CALL rpush
        POP DE                      ; consume                             
        JP (IY)

exec_:
        CALL exec1
        JP (IY)
exec1:
        POP HL                      ; HL = RET address    
        EX (SP),HL                  ; HL = NOS, (SP) = RET
        EX DE,HL                    ; HL = TOS, DE = NOS
        JP (HL)                     ; JP to machine code, RET will return to exec_

go_:
        LD HL,BC
        CALL rpush                  ; save Instruction Pointer
        LD BC,DE                    ; pop TOS
        POP DE                      ; consume
        DEC BC                      ; decrement to just before 
        JP  (IY)                    ; Execute code from User def

endGroup_:
        CALL rpop
        LD (vDEFS),HL
        JP (IY)

group_:
        LD HL,(vDEFS)
        CALL rpush
        LD D,E                      ; TOS * 64    
        LD E,0
        SRL D
        RR E
        SRL D
        RR E
        LD HL,DEFS                  
        ADD HL,DE                   ; HL=DEFS + TOS * 64
        LD (vDEFS),HL               ; store in vDEFS
        JP  (IY)                

sysVar_:
        LD A,sysvarsOfs  
        JP var1

i_:
        PUSH DE                     ; save TOS
        PUSH IX
        POP DE                      ; HL=RSP
        JP (IY)

incr_:
        POP HL                      ; DE=addr HL=incr
        EX DE,HL                    ; HL=addr DE=incr
        LD A,E
        ADD A,(HL)
        LD (HL),A
        INC HL
        LD A,D
        ADC A,(HL)
        LD (HL),A
        JP (IY)

inPort_:
        LD C,E
        IN E,(C)
        LD D,0
        JP (IY)        

j_:
        PUSH DE                     ; save TOS
        PUSH IX
        POP HL                      ; HL=RSP
        LD DE,6                        
        ADD HL,DE                   ; HL+=6
        EX DE,HL                    ; TOS=RSP+6
        JP (IY)

key_:
        PUSH DE                     ; push down TOS
        CALL getchar
        LD E,A
        LD D,0
        JP (IY)

newln_:
        call crlf
        JP (IY)        

outPort_:
        LD C,E
        POP HL
        OUT (C),L
        JP (IY)        

rot_:                               ; a b c -- b c a
        POP HL                      ; (SP)=a HL=b DE=c
        EX (SP),HL                  ; (SP)=b HL=a DE=c
        EX DE,HL                    ; (SP)=b HL=c DE=a
        PUSH HL                                  
        JP (IY)

strDef_:
        JP strDef

typeStr_:
        EX DE,HL
        POP DE
        CALL printStr1
        JP (IY)

printStk_:
printStk:                   ;= 32
        CALL enter
        ; .cstr "\\a@2-\\D1-(",$22,"@\\b@\\(,)(.)2-)'"             
        .cstr "\\a@2-\\D1-(",$22,"@.2-)'"             
        JP (IY)

editDef_:
                            ;= 
;*******************************************************************
; Alt Code commands Commands continued
;*******************************************************************
                                    ;=54
editDef:                            ; lookup up def based on number
        LD A,"A"
        ADD A,E
        EX AF,AF'
        LD HL,(vDEFS)
        ADD HL,DE
        ADD HL,DE
        LD E,(HL)
        INC HL
        LD D,(HL)
        EX DE,HL
        LD A,(HL)
        CP ";"
        LD DE,TIB
        JR Z,editDef3
        LD A,":"
        CALL writeChar
        EX AF,AF'
        CALL writeChar
        JR editDef2
editDef1:
        INC HL
editDef2:        
        LD A,(HL)
        CALL writeChar
        CP ";"
        JR NZ,editDef1
editDef3:        
        LD HL,TIB
        EX DE,HL
        OR A
        SBC HL,DE
        LD (vTIBPtr),HL
        POP DE                      ; consume
        JP (IY)

strDef:                             ;=20
        PUSH DE                     ; save TOS
        LD HL,(vHeapPtr)            ; HL = heap ptr
        LD DE,HL                    ; TOS=start of string
        JR strDef2
strDef1:
        LD (HL),A                   ; write char to string
        INC HL                      
strDef2:
        INC BC                      ; point to next char
        LD A,(BC)
        CP "`"                      ; ` is the string terminator
        JR NZ,strDef1
        XOR A                       ; write null to end of string
        LD (HL),A
        JP (IY)

;*******************************************************************
; Misc Commands continued
;*******************************************************************
alt:                                ;=11               
        INC BC
        LD A,(BC)
        LD HL,altCodes
        ADD A,L
        LD L,A
        LD L,(HL)                   
        LD H, msb(altCodePage)            
        JP (HL)     

getRef:                             ;= 8
        INC BC
        LD A,(BC)
        CALL lookupDef
        JP fetch1

num:                                ;=35 
		PUSH DE                     ; push down TOS
		LD HL,$0000				    ; Clear HL to accept the number
		LD A,(BC)				    ; Get the character which is a numeral
num1:                               ; corrected KB 24/11/21

        SUB $30                     ; Form decimal digit
        ADD A,L                     ; Add into bottom of HL
        LD L,A                      
        LD A,0                      ; Clear A
        ADC A,H	                    ; Add with carry H-reg
	    LD H,A	                    ; Put result in H-reg
        INC BC                      ; Increment IP
        LD A, (BC)                  ; and get the next character
        CP $30                      ; Less than $30
        JR C, num2                  ; Not a number / end of number
        CP $3A                      ; Greater or equal to $3A
        JR NC, num2                 ; Not a number / end of number
        ADD HL,HL                   ; 2X
        LD DE,HL                       
        ADD HL,HL                   ; 4X
        ADD HL,HL                   ; 8X
        ADD HL,DE                   ; 2X  + 8X  = 10X
        JR num1
num2:
        DEC BC
        EX DE,HL                    ; Put the number in TOS
        JP (IY)                     ; and process the next character


;*******************************************************************
; Subroutines
;*******************************************************************

crlf:                               ;=7
        call printStr
        .cstr "\r\n"
        RET

lookupDef:                          ;=20
        SUB "A"  
        LD (vEdited),A      
        JR lookupDef2
lookupDef1:
        SUB "A"  
lookupDef2:
        ADD A,A
        LD HL,(vDEFS)
        ADD A,L
        LD L,A
        LD A,0
        ADC A,H
        LD H,A
        RET

printStr:                           ;=
        EX (SP),HL                  ; use return address to locate string
        CALL printStr1              ; print until null
        INC HL                      ; skip past null
        EX (SP),HL                  ; HL contains real return address
        RET
printStr1:
        LD A,(HL)                   ; A=char
        OR A
        RET Z                       ; return if null found
        CALL putchar
        INC HL                      ; next char
        JR printStr1

printdec:                           ;=36
        LD DE,-10000
        CALL printdec1
        LD DE,-1000
        CALL printdec1
        LD DE,-100
        CALL printdec1
        LD E,-10
        CALL printdec1
        LD E,-1
printdec1:
        LD A,'0'-1
printdec2:
        INC A
        ADD	HL,DE
        JR C,printdec2
        SBC HL,DE
        JP putchar

printhex:                           ;=29   
        LD A,H
        CALL printhex2
        LD A,L
        CALL printhex2
        RET

printhex2:		                    
        LD	C,A
		RRA 
		RRA 
		RRA 
		RRA 
	    CALL printhex3
	    LD A,C

printhex3:		
        AND	0x0F
		ADD	A,0x90
		DAA
		ADC	A,0x40
		DAA
		JP putchar

rpush:                              ; 11
        DEC IX                  
        LD (IX+0),H
        DEC IX
        LD (IX+0),L
        RET

rpop:                               ; 11
        LD L,(IX+0)         
        INC IX              
        LD H,(IX+0)
        INC IX                  
        RET

writeChar:                          ;=5
        LD (DE),A
        INC DE
        JP putchar
        
