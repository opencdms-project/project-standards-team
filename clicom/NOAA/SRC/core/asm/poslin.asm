PAGE ,132
;******************************************************************************
;			PROCEDURE POSLIN
;
;	Calling Language: MICROSOFT FORTRAN
;
;	Parameters: INTEGER*2  ROW,COL
;
;	Entry Conditions: None
;
;	DESCRIPTION: Uses ROM BIOS service to return the current location of
;		     of the cursor in ROW,COL.  The values will be 0 ó ROW ó 24
;		     and 0 ó COL ó 79. 
;******************************************************************************
;
DATA	SEGMENT PUBLIC 'DATA'
DATA	ENDS
DGROUP	GROUP DATA

STACKF	STRUC				;Define Stack Addresses
   SAVEBP	DW ?
   RTNADR	DD ?
   COL          DD ?
   ROW		DD ?
STACKF	ENDS

CODE	SEGMENT 'CODE'
ASSUME	CS:CODE, DS:DGROUP, SS:DGROUP

PUBLIC	POSLIN
POSLIN	PROC FAR

	PUSH	BP			;Save FORTRAN registers on the stack
	MOV	BP,SP
	PUSH	DI
	PUSH	ES

;******************************************************************************
;	Find currently active video page and retrieve cursor position
;******************************************************************************


	MOV	AH,0FH			;Use ROM BIOS service #15 to get 
	INT	10H			;currently active video page in BH

	MOV	AH,03H			;Use ROM BIOS service #3 to retrieve 
	INT	10H			;the current cursor position

;******************************************************************************
;		Return the postion of the cursor
;******************************************************************************

	LES	DI,DWORD PTR [BP].ROW	;Move ROW coordinate from DH into 
	MOV	BYTE PTR ES:[DI],DH     ;FORTRAN parameter

	LES	DI,DWORD PTR [BP].COL	;Move COL coordinate from DL into
	MOV	BYTE PTR ES:[DI],DL     ;FORTRAN parameter

;******************************************************************************

EXIT:	POP	ES			;Restore FORTRAN registers from stack
	POP	DI
	POP	BP

	RET	08H			;Return to calling program

POSLIN	ENDP
CODE	ENDS
END
