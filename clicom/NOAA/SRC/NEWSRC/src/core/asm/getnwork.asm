PAGE ,132
;******************************************************************************
;                       PROCEDURE GETNWORK
;
;       Calling Language: MICROSOFT FORTRAN
;
;       Parameters: INTEGER*2  ERRFLG
;
;	Entry Conditions: None
;
;       DESCRIPTION: Uses DOS Function Number 1100h to get NETWORK Installed
;                    status. Interrupt 2FH Function number 1100H determines
;                    whether the resident portion of the network software has
;                    installed.  An ERRFLG of ONE (1) is return if Network
;                    is loaded, and an ERRFLG of ZERO (0) if not.
;******************************************************************************
;
DATA    SEGMENT PUBLIC 'DATA'
DATA    ENDS
DGROUP  GROUP DATA

STACKF   STRUC                           ;Define Stack Addresses
   SAVEBP       DW ?
   RTNADR       DD ?
   ERRFLG       DD ?
STACKF   ENDS

CODE     SEGMENT 'CODE'
ASSUME   CS:CODE, DS:DGROUP, SS:DGROUP

PUBLIC   GETNWORK
GETNWORK PROC FAR

         PUSH    BP                      ;Save FORTRAN registers on the stack
         MOV     BP,SP
         PUSH    DI
         PUSH    ES

;******************************************************************************
;       Get NET WORK installed status
;******************************************************************************

         MOV     AX,1100H
         INT     2FH
         CMP     AL,0FFH
         JNE     NOTLOADED

         MOV     AL,01          ;Set to 1, If the Network software is installed
         JMP     EXIT

NOTLOADED:
         MOV     AL,00          ;Set to 0, If the Network software has not.

EXIT:
         LES     DI,DWORD PTR [BP].ERRFLG
         MOV     BYTE PTR ES:[DI],AL

         POP     ES                      ;Restore FORTRAN registers from stack
         POP     DI
         POP     BP

         RET     04H                     ;Return to calling program

GETNWORK ENDP
CODE     ENDS
END
