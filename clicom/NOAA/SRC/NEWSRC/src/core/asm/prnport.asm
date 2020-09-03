PAGE ,132
;******************************************************************************
;			PROCEDURE PRNPORT
;
;	Calling Language: MICROSOFT FORTRAN
;
;	Parameters: INTEGER*2  PRTERR
;
;	Entry Conditions: None
;
;	DESCRIPTION: Uses DOS Function 17H to determine the printer status and
;                    reported in the AH register by services 02H. If the
;                    program set:
;                       PRTERR to zero  (0) to indicate printer is READY
;                       PRTERR to one   (1) to indicate printer TURNED OFF
;                       PRTERR to two   (2) to indicate printer OFF LINE
;                       PRTERR to three (3) to indicate printer is BUSY
;                       PRTERR to four  (4) to indicate printer OUT OF PAPER
;******************************************************************************
;
DATA	SEGMENT PUBLIC 'DATA'
DATA	ENDS
DGROUP	GROUP DATA

STACKF	STRUC				;Define Stack Addresses
   SAVEBP	DW ?
   RTNADR	DD ?
   PRTERR	DD ?
STACKF	ENDS

CODE	SEGMENT 'CODE'
ASSUME	CS:CODE, DS:DGROUP, SS:DGROUP

PUBLIC	PRNPORT
PRNPORT	PROC FAR

	PUSH	BP			;Save FORTRAN registers on the stack
	MOV	BP,SP
	PUSH	DI
	PUSH	ES

;******************************************************************************
;	Call DOS Function 17H to determine the printer status
;******************************************************************************

        MOV     DX,00H           ;Default LPT1 port for printer
        MOV     AH,02H           ;Function #2 int 17H to read printer status
	INT	17H	         ;and report in the AH register

        CMP     AH,0C8H           ;printer turn off ?
        JE      PRT_TURNED_OFF
        CMP     AH,18H            ;print off line ?
        JE      PRT_OFF_LINE
        CMP     AH,0A1H           ;print is busy ?
        JE      PRT_IS_BUSY
        CMP     AH,38H            ;Printer out of paper ?
        JE      PRT_OUT_PAPER
        CMP     AH,90H            ;Printer is ready
        JE      PRT_READY
        CMP     AH,16H            ;Printer is selected
        JE      PRT_READY

PRT_TURNED_OFF:
        MOV     AL,01H
        JMP     EXIT

PRT_OFF_LINE:
        MOV     AL,02H
        JMP     EXIT

PRT_IS_BUSY:
        MOV     AL,03H
        JMP     EXIT

PRT_OUT_PAPER:
        MOV     AL,04H
        JMP     EXIT

PRT_READY:
        MOV     AL,00H
        JMP     EXIT

;******************************************************************************
;	Move error number from register into FORTRAN variable
;******************************************************************************

EXIT:
	LES	DI,DWORD PTR [BP].PRTERR	;Move number to 
	MOV	BYTE PTR ES:[DI],AL		;FORTRAN variable

;******************************************************************************

	POP	ES			;Restore FORTRAN registers from stack
	POP	DI
	POP	BP

	RET	04H			;Return to calling program

PRNPORT	ENDP
CODE	ENDS
END
