PAGE ,132
;******************************************************************************
;                       PROCEDURE GETPRT
;
;       Calling Language: MICROSOFT FORTRAN
;
;       Parameters: INTEGER*2  ERRFLG
;
;	Entry Conditions: None
;
;       DESCRIPTION: Uses DOS Function Number 0100h to get PRINT.EXE installed
;                    status.  Interrupt 2Fh Function number 0100h determines
;                    whether the resident portion of the PRINT command has been
;                    loaded.  An ERRFLG of one (1) is return if PRINT command 
;                    is loaded, and an ERRFLG of zero (0) if not.
;******************************************************************************
;
DATA    SEGMENT PUBLIC 'DATA'
DATA    ENDS
DGROUP  GROUP DATA

STACKF  STRUC                           ;Define Stack Addresses
   SAVEBP       DW ?
   RTNADR       DD ?
   ERRFLG       DD ?
STACKF  ENDS

CODE    SEGMENT 'CODE'
ASSUME  CS:CODE, DS:DGROUP, SS:DGROUP

PUBLIC  GETPRT
GETPRT  PROC FAR

        PUSH    BP                      ;Save FORTRAN registers on the stack
        MOV     BP,SP
        PUSH    DI
        PUSH    ES

;******************************************************************************
;       Get PRINT installed status
;******************************************************************************

        MOV     AX,0100H
        INT     2FH
 
        CMP     AL,0FFH
        JNE     NOTLOADED

        LES     DI,DWORD PTR [BP].ERRFLG
        MOV     BYTE PTR ES:[DI],01
        JMP     EXIT

NOTLOADED:
        LES     DI,DWORD PTR [BP].ERRFLG
        MOV     BYTE PTR ES:[DI],00

EXIT:
        POP     ES                      ;Restore FORTRAN registers from stack
        POP     DI
        POP     BP

        RET     04H                     ;Return to calling program

GETPRT  ENDP
CODE    ENDS
END
