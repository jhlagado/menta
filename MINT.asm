; *************************************************************************
;
;       MINT Minimal Interpreter for the Z80 
;
;       Ken Boak, John Hardy and Craig Jones. 
;
;       GNU GENERAL PUBLIC LICENSE                   Version 3, 29 June 2007
;
;       see the LICENSE file in this repo for more information 
;
; *****************************************************************************

        DSIZE       EQU $80
        RSIZE       EQU $80
        TIBSIZE     EQU $100
        TRUE        EQU 1
        FALSE       EQU 0

        NUMGRPS     EQU 5
        GRPSIZE     EQU $40
        
        varsOfs   EQU ((VARS - mintVars)/2) + "a"
        sysvarsOfs   EQU ((sysVars - mintVars)/2) + "a"


; **************************************************************************
; Page 0  Initialisation
; **************************************************************************		

		.ORG ROMSTART + $180		

start:
        LD SP,DSTACK
        CALL initialize
        CALL printStr
        .cstr "MINT V1.0\r\n"
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
        EXX
        LD DE,RSTACK
        EXX
        LD IY,NEXT			    ; IY provides a faster jump to NEXT
        RET

macro:                          ; 25
        LD (vTIBPtr),BC
        LD HL,ctrlCodes
        ADD A,L
        LD L,A
        LD E,(HL)
        LD D,msb(macros)
        PUSH DE
        call ENTER
        .cstr "\\G"
        LD BC,(vTIBPtr)
        JR interpret2

interpret:
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
        LD D,0
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
        LD A,E                  ; if zero nesting append and ETX after \r
        OR A
        JR NZ,waitchar
        LD (HL),$03             ; store end of text ETX in text buffer 
        INC BC

waitchar4:    
        LD (vTIBPtr),BC
        EXX
        LD BC,TIB               ; Instructions stored on heap at address HERE
        DEC BC
        EXX
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
        EXX
        INC BC                      ; 6t    Increment the IP
        LD A,(BC)                   ; 7t    Get the next character and dispatch
        EXX
        LD L,A                      ; 4t    Index into table
        LD H,msb(opcodes)           ; 7t    Start address of jump table         
        LD L,(HL)                   ; 7t    get low jump address
        LD H,msb(page4)             ; 7t    Load H with the 1st page address
        JP (HL)                     ; 4t    Jump to routine

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
        DB    lsb(exit_)    ;   NUL 
        DB    lsb(nop_)     ;   SOH 
        DB    lsb(nop_)     ;   STX 
        DB    lsb(etx_)     ;   ETX 
        DB    lsb(nop_)     ;   EOT 
        DB    lsb(nop_)     ;   ENQ 
        DB    lsb(nop_)     ;   ACK 
        DB    lsb(nop_)     ;   BEL 
        DB    lsb(nop_)     ;   BS  
        DB    lsb(nop_)     ;   TAB 
        DB    lsb(nop_)     ;   LF  
        DB    lsb(nop_)     ;   VT  
        DB    lsb(nop_)     ;   FF  
        DB    lsb(nop_)     ;   CR  
        DB    lsb(nop_)     ;   SO  
        DB    lsb(nop_)     ;   SI  
        DB    lsb(nop_)     ;   DLE 
        DB    lsb(nop_)     ;   DC1 
        DB    lsb(nop_)     ;   DC2 
        DB    lsb(nop_)     ;   DC3 
        DB    lsb(nop_)     ;   DC4 
        DB    lsb(nop_)     ;   NAK 
        DB    lsb(nop_)     ;   SYN 
        DB    lsb(nop_)     ;   ETB 
        DB    lsb(nop_)     ;   CAN 
        DB    lsb(nop_)     ;   EM  
        DB    lsb(nop_)     ;   SUB 
        DB    lsb(nop_)     ;   ESC 
        DB    lsb(nop_)     ;   FS  
        DB    lsb(nop_)     ;   GS  
        DB    lsb(nop_)     ;   RS  
        DB    lsb(nop_)     ;   US  
        DB    lsb(nop_)     ;   SP
        DB    lsb(store_)   ;   !            
        DB    lsb(dup_)     ;   "
        DB    lsb(hex_)    ;    #
        DB    lsb(swap_)   ;    $            
        DB    lsb(over_)   ;    %            
        DB    lsb(and_)    ;    &
        DB    lsb(drop_)   ;    '
        DB    lsb(begin_)  ;    (        
        DB    lsb(again_)  ;    )
        DB    lsb(mul_)    ;    *            
        DB    lsb(add_)    ;    +
        DB    lsb(hdot_)   ;    ,            
        DB    lsb(sub_)    ;    -
        DB    lsb(dot_)    ;    .
        DB    lsb(div_)    ;    /
        DB    lsb(num_)    ;    0            
        DB    lsb(num_)    ;    1        
        DB    lsb(num_)    ;    2            
        DB    lsb(num_)    ;    3
        DB    lsb(num_)    ;    4            
        DB    lsb(num_)    ;    5            
        DB    lsb(num_)    ;    6            
        DB    lsb(num_)    ;    7
        DB    lsb(num_)    ;    8            
        DB    lsb(num_)    ;    9        
        DB    lsb(def_)    ;    :        
        DB    lsb(ret_)    ;    ;
        DB    lsb(lt_)     ;    <
        DB    lsb(eq_)     ;    =            
        DB    lsb(gt_)     ;    >            
        DB    lsb(getRef_) ;    ?
        DB    lsb(fetch_)  ;    @    
        DB    lsb(call_)   ;    A    
        DB    lsb(call_)   ;    B
        DB    lsb(call_)   ;    C
        DB    lsb(call_)   ;    D    
        DB    lsb(call_)   ;    E
        DB    lsb(call_)   ;    F
        DB    lsb(call_)   ;    G
        DB    lsb(call_)   ;    H
        DB    lsb(call_)   ;    I
        DB    lsb(call_)   ;    J
        DB    lsb(call_)   ;    K
        DB    lsb(call_)   ;    L
        DB    lsb(call_)   ;    M
        DB    lsb(call_)   ;    N
        DB    lsb(call_)   ;    O
        DB    lsb(call_)   ;    P
        DB    lsb(call_)   ;    Q
        DB    lsb(call_)   ;    R
        DB    lsb(call_)   ;    S
        DB    lsb(call_)   ;    T
        DB    lsb(call_)   ;    U
        DB    lsb(call_)   ;    V
        DB    lsb(call_)   ;    W
        DB    lsb(call_)   ;    X
        DB    lsb(call_)   ;    Y
        DB    lsb(call_)   ;    Z
        DB    lsb(arrDef_) ;    [
        DB    lsb(alt_)    ;    \
        DB    lsb(arrEnd_) ;    ]
        DB    lsb(xor_)    ;    ^
        DB    lsb(neg_)    ;    _
        DB    lsb(str_)    ;    `            
        DB    lsb(var_)    ;    a
        DB    lsb(var_)    ;    b
        DB    lsb(var_)    ;    c
        DB    lsb(var_)    ;    d
        DB    lsb(var_)    ;    e
        DB    lsb(var_)    ;    f
        DB    lsb(var_)    ;    g
        DB    lsb(var_)    ;    h
        DB    lsb(var_)    ;    i            
        DB    lsb(var_)    ;    j
        DB    lsb(var_)    ;    k
        DB    lsb(var_)    ;    l
        DB    lsb(var_)    ;    m
        DB    lsb(var_)    ;    n
        DB    lsb(var_)    ;    o
        DB    lsb(var_)    ;    p
        DB    lsb(var_)    ;    q            
        DB    lsb(var_)    ;    r
        DB    lsb(var_)    ;    s    
        DB    lsb(var_)    ;    t
        DB    lsb(var_)    ;    u
        DB    lsb(var_)    ;    v
        DB    lsb(var_)    ;    w
        DB    lsb(var_)    ;    x
        DB    lsb(var_)    ;    y
        DB    lsb(var_)    ;    z
        DB    lsb(shl_)    ;    {
        DB    lsb(or_)     ;    |            
        DB    lsb(shr_)    ;    }            
        DB    lsb(inv_)    ;    ~            
        DB    lsb(nop_)    ;    backspace

        
; ***********************************************************************
; Alternate function codes		
; ***********************************************************************		
ctrlCodes:
altCodes:
        DB     lsb(empty_)      ; NUL ^@
        DB     lsb(empty_)      ; SOH ^A
        DB     lsb(toggleBase_) ; STX ^B
        DB     lsb(empty_)      ; ETX ^C
        DB     lsb(empty_)      ; EOT ^D
        DB     lsb(edit_)       ; ENQ ^E
        DB     lsb(empty_)      ; ACK ^F
        DB     lsb(empty_)      ; BEL ^G
        DB     lsb(backsp_)     ; BS  ^H
        DB     lsb(empty_)      ; TAB ^I
        DB     lsb(reedit_)     ; LF  ^J
        DB     lsb(empty_)      ; VT  ^K
        DB     lsb(list_)       ; FF  ^L
        DB     lsb(empty_)      ; CR  ^M
        DB     lsb(empty_)      ; SO  ^N
        DB     lsb(empty_)      ; SI  ^O
        DB     lsb(printStack_) ; DLE ^P
        DB     lsb(empty_)      ; DC1 ^Q
        DB     lsb(empty_)      ; DC2 ^R
        DB     lsb(empty_)      ; DC3 ^S
        DB     lsb(empty_)      ; DC4 ^T
        DB     lsb(empty_)      ; NAK ^U
        DB     lsb(empty_)      ; SYN ^V
        DB     lsb(empty_)      ; ETB ^W
        DB     lsb(empty_)      ; CAN ^X
        DB     lsb(empty_)      ; EM  ^Y
        DB     lsb(empty_)      ; SUB ^Z
        DB     lsb(empty_)      ; ESC ^[
        DB     lsb(empty_)      ; FS  ^\
        DB     lsb(empty_)      ; GS  ^]
        DB     lsb(empty_)      ; RS  ^^
        DB     lsb(empty_)      ; US  ^_)
        DB     lsb(aNop_)       ; SP  ^`
        DB     lsb(cStore_)     ;    !            
        DB     lsb(aNop_)       ;    "
        DB     lsb(aNop_)       ;    #
        DB     lsb(aNop_)       ;    $  ( -- adr ) text input ptr           
        DB     lsb(aNop_)       ;    %            
        DB     lsb(aNop_)       ;    &
        DB     lsb(aNop_)       ;    '
        DB     lsb(ifte_)       ;    (  ( b -- )              
        DB     lsb(aNop_)       ;    )
        DB     lsb(aNop_)       ;    *            
        DB     lsb(incr_)       ;    +  ( adr -- ) decrements variable at address
        DB     lsb(aNop_)       ;    ,            
        DB     lsb(aNop_)       ;    -  
        DB     lsb(aNop_)       ;    .  
        DB     lsb(aNop_)       ;    /
        DB     lsb(aNop_)       ;    0           
        DB     lsb(aNop_)       ;    1  
        DB     lsb(aNop_)       ;    2            
        DB     lsb(aNop_)       ;    3  
        DB     lsb(aNop_)       ;    4            
        DB     lsb(aNop_)       ;    5            
        DB     lsb(aNop_)       ;    6            
        DB     lsb(aNop_)       ;    7
        DB     lsb(aNop_)       ;    8            
        DB     lsb(aNop_)       ;    9        
        DB     lsb(aNop_)       ;    :  start defining a macro        
        DB     lsb(aNop_)       ;    ;  
        DB     lsb(aNop_)       ;    <
        DB     lsb(aNop_)       ;    =            
        DB     lsb(aNop_)       ;    >            
        DB     lsb(aNop_)       ;    ?
        DB     lsb(cFetch_)     ;    @      
        DB     lsb(aNop_)       ;    A    
        DB     lsb(break_)      ;    B
        DB     lsb(nop_)        ;    C
        DB     lsb(depth_)      ;    D  ( -- val ) depth of data stack  
        DB     lsb(emit_)       ;    E   ( val -- ) emits a char to output
        DB     lsb(aNop_)       ;    F
        DB     lsb(go_)         ;    G   ( -- ? ) execute mint definition
        DB     lsb(aNop_)       ;    H  
        DB     lsb(inPort_)     ;    I  ( port -- val )   
        DB     lsb(aNop_)       ;    J
        DB     lsb(key_)        ;    K  ( -- val )  read a char from input
        DB     lsb(aNop_)       ;    L  
        DB     lsb(aNop_)       ;    M  
        DB     lsb(newln_)      ;    N   ; prints a newline to output
        DB     lsb(outPort_)    ;    O  ( val port -- )
        DB     lsb(printStk_)   ;    P  ( -- ) non-destructively prints stack
        DB     lsb(aNop_)       ;    Q  quits from Mint REPL
        DB     lsb(rot_)        ;    R  ( a b c -- b c a )
        DB     lsb(aNop_)       ;    S
        DB     lsb(aNop_)       ;    T
        DB     lsb(aNop_)       ;    U
        DB     lsb(aNop_)       ;    V
        DB     lsb(aNop_)       ;    W   ; ( b -- ) if false, skip to end of loop
        DB     lsb(exec_)       ;    X
        DB     lsb(aNop_)       ;    Y
        DB     lsb(editDef_)    ;    Z
        DB     lsb(cArrDef_)    ;    [
        DB     lsb(comment_)    ;    \  comment text, skips reading until end of line
        DB     lsb(aNop_)       ;    ]
        DB     lsb(charCode_)   ;    ^
        DB     lsb(sign_)       ;    _)  ( n -- b ) returns true if -ve 
        DB     lsb(aNop_)       ;    `            
        DB     lsb(sysVar_)     ;    a  ; start of data stack variable
        DB     lsb(sysVar_)     ;    b  ; base16 variable
        DB     lsb(sysVar_)     ;    c  ; TIBPtr variable
        DB     lsb(sysVar_)     ;    d  
        DB     lsb(sysVar_)     ;    e  
        DB     lsb(sysVar_)     ;    f
        DB     lsb(sysVar_)     ;    g  
        DB     lsb(sysVar_)     ;    h  ; heap ptr variable
        DB     lsb(i_)          ;    i  ; returns index variable of current loop          
        DB     lsb(j_)          ;    j  ; returns index variable of outer loop
        DB     lsb(sysVar_)     ;    k  
        DB     lsb(sysVar_)     ;    l
        DB     lsb(sysVar_)     ;    m  ( a b -- c ) return the minimum value
        DB     lsb(sysVar_)     ;    n  
        DB     lsb(sysVar_)     ;    o
        DB     lsb(sysVar_)     ;    p  
        DB     lsb(sysVar_)     ;    q           
        DB     lsb(sysVar_)     ;    r
        DB     lsb(sysVar_)     ;    s 
        DB     lsb(sysVar_)     ;    t
        DB     lsb(sysVar_)     ;    u
        DB     lsb(sysVar_)     ;    v   
        DB     lsb(sysVar_)     ;    w   
        DB     lsb(sysVar_)     ;    x
        DB     lsb(sysVar_)     ;    y
        DB     lsb(sysVar_)     ;    z
        DB     lsb(group_)      ;    {
        DB     lsb(aNop_)       ;    |            
        DB     lsb(endGroup_)   ;    }            
        DB     lsb(aNop_)       ;    ~           
        DB     lsb(aNop_)       ;    BS		


; **********************************************************************			 
; Page 4 primitive routines 
; **********************************************************************
        .align $100
page4:

alt_:        
alt:                                ;= 11
        EXX
        INC BC
        LD A,(BC)
        EXX
        LD HL,altCodes
        ADD A,L
        LD L,A
        LD L,(HL)                   ; 7t    get low jump address
        LD H, msb(page6)            ; Load H with the 5th page address
        JP  (HL)                    ; 4t    Jump to routine

and_:        
        JP (IY)        
                            ; 63t
or_: 		 
        JP (IY)        

xor_:		 
        JP (IY)        

inv_:						    
        JP (IY)        
   
add_:                          ; Add the top 2 members of the stack
        JP (IY)        

arrDef_:    
        JP (IY)        

arrEnd_:    
        JP (IY)        

begin_:     
        JP (IY)        

call_:
        JP (IY)        

def_:   
        JP (IY)        

hdot_:                              ; print hexadecimal
        JP (IY)        

dot_:       
        JP (IY)        

drop_:                      ; Discard the top member of the stack
        JP (IY)        

dup_:        
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
        EXX
        INC BC
        PUSH BC
        CALL rpop               
        LD BC,HL
        EXX
        RET
        
fetch_:                           
        EX DE,HL
        LD E,(HL)      
        INC HL          
        LD D,(HL)      
        JP (IY)        

hex_:   
        JP (IY)        

nop_:   
        JP NEXT                 ; hardwire white space to always go to NEXT (important for arrays)

num_:   
        JP num        

over_:  
        JP (IY)        
    
ret_:
        EXX
        CALL rpop               ; Restore Instruction pointer
        LD BC,HL                
        EXX
        JP (IY)             

store_:                     
        EX DE,HL                ; HL = TOS
        POP DE                  ; DE = NOS
        LD (HL),E       
        INC HL           
        LD (HL),D       
        POP DE                  ; DE = TOS
        JP (IY)         

swap_:        
        JP (IY)        

shl_:   
        JP (IY)        
    
shr_:    
        JP (IY)        

neg_:   
        JP (IY)        
    
sub_:       				    ; Subtract the value 2nd on stack from top of stack 
        JP (IY)        
                                ; 58t
eq_:    
        JP (IY)        

getRef_:    
        JP (IY)        

gt_:    
        JP (IY)        
        
lt_:    
        JP (IY)        
        
var_:
        LD A,varsOfs  
var1:
        EXX
        LD H,B
        LD L,C
        SUB (HL)
        ADD A,A
        EXX
        PUSH DE                 ; push TOS
        LD E,A                  ; TOS = ptr to var
        LD D,msb(mintVars)
        JP (IY)
        
div_:   
        JP (IY)        

mul_:   
        JP (IY)        

again_:     
        JP (IY)        

str_:                       
        JP (IY)        

;*******************************************************************
; Page 5 primitive routines 
;*******************************************************************
        ;falls through 

; **************************************************************************
; Page 6 Alt primitives
; **************************************************************************
        .align $100
page6:

cArrDef_:                   ; define a byte array
        JP (IY)        

cFetch_:
        JP (IY)        
anop_:
        JP (IY)        ; 8t
                            ; 49t 
charCode_:
        JP (IY)        

comment_:
        JP (IY)        

cStore_:	  
        JP (IY)        
                            ; 48t
depth_:
        JP (IY)        

emit_:
        JP (IY)        

ifte_:
        JP (IY)        

exec_:
        CALL exec1
        JP (IY)
exec1:
        POP HL                  ; HL = RET address    
        EX (SP),HL              ; HL = NOS, (SP) = RET
        EX DE,HL                ; HL = TOS, DE = NOS
        JP (HL)                 ; JP to machine code, RET will return to exec_

go_:
        PUSH DE                 ; push TOS
        EXX
        LD HL,BC
        CALL rpush              ; save Instruction Pointer
        POP BC                  ; pop TOS
        DEC BC                  ; decrement to just before 
        EXX
        JP  (IY)                ; Execute code from User def

endGroup_:
        JP (IY)        

group_:
        JP (IY)        

sysVar_:
        LD A,sysvarsOfs  
        JP var1

i_:
        JP (IY)        

incr_:
        JP (IY)        

inPort_:
        JP (IY)        

j_:
        JP (IY)        

key_:
        JP (IY)        

newln_:
        JP (IY)        

outPort_:
        JP (IY)        

rot_:                               
        JP (IY)        

sign_:
        JP (IY)        

break_:
        JP (IY)        

printStk_:
        JP (IY)        

editDef_:
        JP (IY)        


;*******************************************************************
; Page 5 primitive routines continued
;*******************************************************************

crlf:                               ; 18
        call printStr
        .cstr "\r\n"
        RET

prompt:
        call printStr
        .cstr "\r\n> "
        RET
        
printStr:
        EX (SP),HL
        JR printStr2

printStr1:
        CALL putchar
        INC HL

printStr2:
        LD A,(HL)
        OR A
        JR NZ,printStr1
        INC HL
        EX (SP),HL
        RET
        
rpush:                              ; 11
        EX DE,HL
        DEC HL                  
        LD (HL),D
        DEC HL
        LD (HL),E
        EX DE,HL
        RET

rpop:                               ; 11
        EX DE,HL
        LD E,(HL)         
        INC HL              
        LD D,(HL)
        INC HL                  
        EX DE,HL
        RET

enter:                          ;= 9
        EXX
        LD HL,BC
        CALL rpush              ; save Instruction Pointer
        POP BC
        DEC BC
        EXX
        JP  (IY)                ; Execute code from User def


; ********************************************************************************
; Number Handling Routine - converts numeric ascii string to a 16-bit number in HL
; Read the first character. 
;			
; Number characters ($30 to $39) are converted to digits by subtracting $30
; and then added into the L register. (HL forms a 16-bit accumulator)
; Fetch the next character, if it is a number, multiply contents of HL by 10
; and then add in the next digit. Repeat this until a non-number character is 
; detected. Add in the final digit so that HL contains the converted number.
; Push HL onto the stack and proceed to the dispatch routine.
; ********************************************************************************
         
num:                            ;= 
        EXX
		LD HL,0			    	; Clear HL to accept the number
		LD A,(BC)				; Get the character which is a numeral
num1:                           ; corrected KB 24/11/21

        SUB $30                 ; Form decimal digit
        ADD A,L                 ; Add into bottom of HL
        LD  L,A                 ; 
        XOR A                   ; Clear A
        ADC	A,H	                ; Add with carry H-reg
	    LD	H,A	                ; Put result in H-reg
      
        INC BC                  ; Increment IP
        LD A, (BC)              ; and get the next character
        CP $30                  ; Less than $30
        JR C,num2               ; Not a number / end of number
        CP $3A                  ; Greater or equal to $3A
        JR NC,num2              ; Not a number / end of number
                                ; Multiply digit(s) in HL by 10
        ADD HL,HL               ; 2X
        LD  E,L                 ; LD DE,HL
        LD  D,H                 ; 
        ADD HL,HL               ; 4X
        ADD HL,HL               ; 8X
        ADD HL,DE               ; 2X  + 8X  = 10X
        JR  num1
num2:
        DEC BC
        PUSH HL                 ; Put the number on the stack
        EXX
        EX DE,HL                ; NOS in HL
        EX (SP),HL              ; TOS in HL
        EX DE,HL                ; TOS in DE    
        JP (IY)                 ; and process the next character

; **************************************************************************             
; calculate nesting value
; A is char to be tested, 
; E is the nesting value (initially 0)
; E is increased by ( and [ 
; E is decreased by ) and ]
; E has its bit 7 toggled by `
; limited to 127 levels
; **************************************************************************             

nesting:                        ;= 44
        CP '`'
        JR NZ,nesting1
        BIT 7,E
        JR Z,nesting1a
        RES 7,E
        RET
nesting1a: 
        SET 7,E
        RET
nesting1:
        BIT 7,E             
        RET NZ             
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
        
