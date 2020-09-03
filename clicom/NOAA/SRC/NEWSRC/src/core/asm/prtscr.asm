PAGE ,132
;******************************************************************************
;                       PROCEDURE PRTSCR
;
;       Calling Language: MICROSOFT FORTRAN
;
;       Entry Conditions: None.
;
;       DESCRIPTION: Uses DOS Interrupt 05H to send the current screen contents
;                    to the printer.
;******************************************************************************
;
DATA    SEGMENT PUBLIC 'DATA'
DATA    ENDS
DGROUP  GROUP DATA

STACKF  STRUC                           ;Define Stack Addresses
   SAVEBP       DW ?
   RTNADR       DD ?
STACKF  ENDS

CODE    SEGMENT 'CODE'
ASSUME  CS:CODE, DS:DGROUP, SS:DGROUP

PUBLIC  PRTSCR
PRTSCR  PROC FAR

        PUSH    BP                      ;Save FORTRAN registers on the stack
        MOV     BP,SP
        PUSH    DI
        PUSH    ES

;******************************************************************************
;       Set up registers and call DOS Interrupt 05H. 
;******************************************************************************

        INT     05H                     ;to retrieve the system date

;******************************************************************************

EXIT:   POP     ES                      ;Restore FORTRAN registers from stack
        POP     DI
        POP     BP

        RET     08H                     ;Return to calling program

PRTSCR  ENDP
CODE    ENDS
END
