PAGE ,132
;***************************************************************************
;			PROCEDURE INKEY
;
;	Calling Language: MICROSOFT FORTRAN
;
;	Parameters: CHARACTER*1 KEY
;
;	Entry Conditions: None
;
;	DESCRIPTION: Uses DOS service to read a character from the 
;		     keyboard buffer and returns it in KEY.  Non-zero
;		     values returned in KEY indicate standard ASCII
;		     characters.  A zero value returned in KEY indicates
;		     that a special key (e.g. a Function Key) has been 
;		     pressed.  The standard DOS coded value for this key
;		     will be returned upon a subsequent call to this routine.
;****************************************************************************
;
DATA	SEGMENT PUBLIC 'DATA'
DATA	ENDS
DGROUP	GROUP DATA

STACKF	STRUC				;Define Stack Addresses
   SAVEBP	DW ?
   RTNADR	DD ?
   KEY		DD ?
STACKF	ENDS

CODE	SEGMENT 'CODE'
ASSUME	CS:CODE, DS:DGROUP, SS:DGROUP

PUBLIC	INKEY
INKEY	PROC FAR

	PUSH	BP			;Save FORTRAN registers on the stack
	MOV	BP,SP
	PUSH	DI
	PUSH	ES

	MOV	AH,07H			;Call DOS keyboard Service #7
	INT	21H
	
	LES	DI,[BP].KEY		;Load character into variable passed
	MOV	ES:[DI],AL		;from calling FORTRAN program

	POP	ES			;Restore FORTRAN registers from stack
	POP	DI
	POP	BP

	RET	04H			;Return to calling program

INKEY	ENDP
CODE	ENDS
END
