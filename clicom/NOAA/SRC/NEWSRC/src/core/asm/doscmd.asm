PAGE ,132
;******************************************************************************
;                       PROCEDURE DOSCMD
;
;       Calling Language: MICROSOFT FORTRAN
;
;       Entry Conditions: arguments passed are CMDNAM, CMDARG, EFLAG
;
;       DESCRIPTION: Uses DOS Interrupt 21H Function 4B00H to run a DOS command
;******************************************************************************
;
DATA    SEGMENT PUBLIC 'DATA'
DATA    ENDS
DGROUP  GROUP DATA

STACKF  STRUC                           ;Define Stack Addresses
   SAVEBP       DW ?
   RTNADR       DD ?
   EFLAG        DD ?
   NCHARG       DD ?
   CMDARG       DD ?
   CMDNAM       DD ?
STACKF  ENDS

LOADEXEC  STRUC                           ;Define parameters for EXEC function
   leEnvironment       DW  2 DUP(00H)
   leCommandTail       DD 80 DUP(?)
   leFCB_1             DD 11 DUP(20H),5 DUP(00H)
   leFCB_2             DD 11 DUP(20H),5 DUP(00H)
LOADEXEC  ENDS

CODE    SEGMENT 'CODE'
ASSUME  CS:CODE, DS:DGROUP, SS:DGROUP

PUBLIC  PRTSCR
PRTSCR  PROC FAR

        PUSH    BP                      ;Save FORTRAN registers on the stack
        MOV     BP,SP
        PUSH    DS
        PUSH    ES
        PUSH    SI
        PUSH    DI

;******************************************************************************
;       Load adresses into registers to set up for move
;******************************************************************************
            
            LES       BX,DWORD PTR [BP].NCHARG  ;ADDRESS OF NCHARG
            MOV       CX,ES:[BX]                ;CHARACTER COUNT TO CX
            
            LES       SI,DWORD PTR [BP].CMDARG  ;ADDRESS OF STRING CMDARG
            MOV       BX,ES:[SI]
            MOV       DS,BX                     ;SAVE ADDRESS OF A FOR MOVE
            LES       BX,DWORD PTR [BP].L       ;ADDRESS OF L
            MOV       BX,ES:[BX]                ;L TO BX
            SUB       BX,1
            ADD       AX,BX                     ;AX=A(L)
            MOV       SI,AX                     ;PUT IN SOURCE INDEX FOR MOVE
            LES       AX,DWORD PTR [BP].B       ;ADDRESS OF B ARRAY
            LES       BX,DWORD PTR [BP].M       ;ADDRESS OF M
            MOV       BX,ES:[BX]                ;M TO BX
            SUB       BX,1
            ADD       AX,BX                     ;AX=B(M)
            MOV       DI,AX             ;PUT IN DESTINATION INDEX FOR MOVE

;******************************************************************************
;                               Move the data
;******************************************************************************

            CLD                 ;SET FLAG TO MOVE IN ASCENDING ORDER
            REP       MOVSB     ;MOVE K BYTES FROM A(L) TO B(M)

;******************************************************************************
;       Set up registers and call DOS Interrupt 05H. 
;******************************************************************************

        MOV     DX, SEG CMDNAM
        MOV     DS, DX
        MOV     DX, OFFSET CMDNAM       ; DS:DX POINTS TO DOS COMMAND NAME
        
        MOV     BX, SEG LOADEXEC
        MOV     EX, BX 
        MOV     BX, OFFSET LOADEXEC     ; ES:BX POINTS TO LOADEXEC STRUCTURE
        
        MOV     AX,4B00H                ; LOAD AND EXECUTE PROGRAM
        INT     21H
        
        JC      ERR                     ; JUMP TO ERROR PROCESSING IF ERROR 
                                        ; FLAG IS SET
                                        
        JMP     EXIT                    ; JUMP TO EXIT PROCESSING -- NO ERROR                                  


;******************************************************************************
ERR:    LES     AX,EFLAG

EXIT:   POP     DI                      ;Restore FORTRAN registers from stack
        POP     SI
        POP     ES
        POP     DS
        POP     BP

        RET     12H                     ;Return to calling program

PRTSCR  ENDP
CODE    ENDS
END
