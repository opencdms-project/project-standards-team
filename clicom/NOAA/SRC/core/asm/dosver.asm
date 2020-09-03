PAGE ,132
;******************************************************************************
;                       PROCEDURE DOSVER
;
;       Calling Language: MICROSOFT FORTRAN
;
;       Parameters: INTEGER*2  MAJVER,MINVER
;
;       DESCRIPTION: Uses DOS Function Number 30h to retrieve the current DOS
;                    version and passes the version number back to the calling 
;                    program as two integers.
;******************************************************************************
;
DATA    SEGMENT PUBLIC 'DATA'
DATA    ENDS
DGROUP  GROUP DATA

STACKF  STRUC                           ;Define Stack Addresses
   SAVEBP       DW ?
   RTNADR       DD ?
   MINVER       DD ?
   MAJVER       DD ?
STACKF  ENDS

CODE    SEGMENT 'CODE'
ASSUME  CS:CODE, DS:DGROUP, SS:DGROUP

PUBLIC  DOSVER
DOSVER  PROC FAR

        PUSH    BP                      ;Save FORTRAN registers on the stack
        MOV     BP,SP
        PUSH    DI
        PUSH    ES

;******************************************************************************
;                               Get DOS Version
;******************************************************************************

        MOV     AH,30H
        INT     21H
        LES     DI,DWORD PTR [BP].MAJVER
        MOV     BYTE PTR ES:[DI],AL
        LES     DI,DWORD PTR [BP].MINVER
        MOV     BYTE PTR ES:[DI],AH
;******************************************************************************

        POP     ES                      ;Restore FORTRAN registers from stack
        POP     DI
        POP     BP

        RET     08H                     ;Return to calling program

DOSVER  ENDP
CODE    ENDS
END
