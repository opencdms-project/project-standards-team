PAGE ,132
;******************************************************************************
;			PROCEDURE RNAME2
;
;	Calling Language: MICROSOFT FORTRAN
;
;	Parameters: CHARACTER*n OLDNAM,NEWNAM (Where n is the length of the 
;					       longest name)
;		    INTEGER*2  EFLAG
;
;	Entry Conditions: OLDNAM, NEWNAM are ASCIIZ strings (i.e. the last
;			  character in the name is an ASCII NUL character).
;
;	DESCRIPTION: Uses DOS Function number 56 to rename the file OLDNAM to 
;		     NEWNAM.  OLDNAM and NEWNAM can be full filenames 
;		     (including path), but must be on the same disk drive.
;		     EFLAG is set to one (1) to indicate success in creating 
;		     the file.  Otherwise, the EFLAG is set to zero (0) to
;		     indicate an error.
;
;******************************************************************************
;
DATA	SEGMENT PUBLIC 'DATA'

EMSG1	DB	'File or Path not found by RNAME2$'
EPTR1	DD	EMSG1
EMSG2	DB	'Access to file by RNAME2 denied$'
EPTR2	DD	EMSG2

DATA	ENDS
DGROUP	GROUP DATA

STACKF	STRUC				;Define Stack Addresses
   SAVEBP	DW ?
   RTNADR	DD ?
   EFLAG	DD ?
   NEWNAM       DD ?
   OLDNAM	DD ?
STACKF	ENDS

CODE	SEGMENT 'CODE'
ASSUME	CS:CODE, DS:DGROUP, SS:DGROUP

PUBLIC	RNAME2
RNAME2	PROC FAR

	PUSH	BP			;Save FORTRAN registers on the stack
	MOV	BP,SP
	PUSH	DI
	PUSH	ES
	PUSH	DS
	
INCLUDE	WTERR.INC

;******************************************************************************
;	Set up registers and call DOS Funtion #86. Set EFLAG if successful
;******************************************************************************

	LDS	DX,DWORD PTR [BP].OLDNAM  ;Load pointer to OLDNAM into DS:DX
	LES	DI,DWORD PTR [BP].NEWNAM  ;Load pointer to NEWNAM into ES:DI
	
	MOV	AH,56H			;Call DOS Function #86
	INT	21H			;to rename the file

	
	CMP	AL,02H		;Check for errors, and determine which error
	JE 	ERR1		;message to display
	CMP	AL,03H
	JE	ERR1
	CMP	AL,05H 
	JE	ERR2
	CMP	AL,11H
	JE	ERR1

;******************************************************************************
;		Set EFLAG = 1 to indicate success in creating file
;******************************************************************************

OKAY:	LES 	DI,DWORD PTR [BP].EFLAG	;Set up pointer to EFLAG
	MOV	BYTE PTR ES:[DI],01H	;Set EFLAG = 1 for success
	JMP	SHORT EXIT

;******************************************************************************
;		Set EFLAG = 0 to indicate failure to create file
;******************************************************************************

ERR1:	LES 	DI,EPTR1		;Point to EMSG1
	_WTERR				;Enter Macro to write EMSG1
	JMP	SHORT ERROR
	
ERR2:	LES	DI,EPTR2		;Point to EMSG2
	_WTERR				;Enter Macro to write EMSG2
	
	
ERROR:	LES 	DI,DWORD PTR [BP].EFLAG	;Set up pointer to EFLAG
	MOV	BYTE PTR ES:[DI],00H

;******************************************************************************

EXIT:	POP	DS			;Restore FORTRAN registers from stack
	POP	ES
	POP	DI
	POP	BP

	RET	0CH			;Return to calling program

RNAME2	ENDP
CODE	ENDS
END
