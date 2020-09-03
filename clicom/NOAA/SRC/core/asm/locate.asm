PAGE ,132
;******************************************************************************
;			PROCEDURE LOCATE
;
;	Calling Language: MICROSOFT FORTRAN
;
;	Parameters: INTEGER*2  ROW,COL,EFLAG
;
;	Entry Conditions: 0 ó ROW ó 24
;			  0 ó COL ó 79
;
;	DESCRIPTION: Uses ROM BIOS service to move the cursor to the specified
;		     position on the screen using 80-column mode coordinates.
;		     (0,0) is the upper left corner of the screen, and (24,79)
;		     is the lower right corner of the screen.  The coordinates
;		     passed from the FORTRAN program are checked to see if they
;		     are within bounds.  If the coordinates are correct, the 
;                    cursor is moved, and an EFLAG of one (1) is returned.  If
;                    the coordinates are in error, an EFLAG of zero (0) is
;		     returned.
;******************************************************************************
;
DATA	SEGMENT PUBLIC 'DATA'

EMSG1	DB	'Invalid row coordinate passed to LOCATE$'
EPTR1	DD	EMSG1
EMSG2	DB	'Invalid column coordinate passed to LOCATE$'
EPTR2	DD	EMSG2

DATA	ENDS
DGROUP	GROUP DATA

STACKF	STRUC				;Define Stack Addresses
   SAVEBP	DW ?
   RTNADR	DD ?
   EFLAG	DD ?
   COL          DD ?
   ROW		DD ?
STACKF	ENDS

CODE	SEGMENT 'CODE'
ASSUME	CS:CODE, DS:DGROUP, SS:DGROUP

PUBLIC	LOCATE
LOCATE	PROC FAR

	PUSH	BP			;Save FORTRAN registers on the stack
	MOV	BP,SP
	PUSH	DI
	PUSH	ES
	
INCLUDE WTERR.INC

;******************************************************************************
;	Set up registers for ROM BIOS call and check coordinate bounds
;******************************************************************************

	LES	DI,DWORD PTR [BP].ROW	;Move ROW coordinate into DH
	MOV	DH,ES:[DI]

	CMP	DH,0H			;Check ROW coordinate bounds
	JL	ERR1
	CMP	DH,18H
	JG	ERR1

	LES	DI,DWORD PTR [BP].COL	;Move COL coordinate into DL
	MOV	DL,ES:[DI]

	CMP	DL,0H			;Check COL coordinate bounds
	JL	ERR2
	CMP	DL,4FH
	JG	ERR2


	MOV	AH,0FH			;Set current Video Page in BH register
	INT	10H			;by calling ROM BIOS video service #15

;******************************************************************************
;			Move the cursor
;******************************************************************************
	MOV	AH,02H			;Call ROM BIOS service #2
	INT	10H

	LES	DI,DWORD PTR [BP].EFLAG	;Set EFLAG = 1 to indicate success
	MOV	BYTE PTR ES:[DI],01H
	JMP	SHORT EXIT
;******************************************************************************
;			Set the error flag
;******************************************************************************
;

ERR1:	LES	DI,EPTR1		;Point to error message
	_WTERR				;Enter _WTERR to write out message
	JMP	SHORT ERROR
	
	
ERR2:	LES	DI,EPTR2		;Point to error message	
	_WTERR				;Enter _WTERR to write out message
	

ERROR:	LES	DI,DWORD PTR [BP].EFLAG
	MOV	BYTE PTR ES:[DI],00H

;******************************************************************************

EXIT:	POP	ES			;Restore FORTRAN registers from stack
	POP	DI
	POP	BP

	RET	0CH			;Return to calling program

LOCATE	ENDP
CODE	ENDS
END
