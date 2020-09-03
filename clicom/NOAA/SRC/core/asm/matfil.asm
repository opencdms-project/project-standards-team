PAGE ,132
;******************************************************************************
;			PROCEDURE MATFIL
;
;	Calling Language: MICROSOFT FORTRAN
;
;	Parameters: CHARACTER*22 FLNAM1,FLNAM2
;		    INTEGER*2  MATCH,EFLAG
;
;	Entry Conditions: FLNAM1, FLNAM2 are ASCIIZ strings containing the
;			  names of the two files whose dates are to be compared
;
;	DESCRIPTION: Uses DOS Functions to determine if FLNAM1 has been changed
;		     since FLNAM2 was changed. If not, MATCH is set to one (1).
;		     Otherwise, MATCH is set to zero (0).  EFLAG is set to one
;		     (1) to indicate success in comparing the files.  Other-
;		     wise, EFLAG is set to zero (0) to indicate an error.
;******************************************************************************
;
DATA	SEGMENT PUBLIC 'DATA'

EMSG1	DB	'Incorrect DOS Version for MATFIL$'
EPTR1	DD	EMSG1
EMSG2 	DB	'MATFIL unable to accesss the file(s)$'
EPTR2	DD	EMSG2
HOLD1	DW	?
HOLD2	DW	?

DATA	ENDS
DGROUP	GROUP DATA

STACKF	STRUC				;Define Stack Addresses
   SAVEBP	DW ?
   RTNADR	DD ?
   EFLAG	DD ?
   MATCH	DD ?
   FLNAM2	DD ?
   FLNAM1       DD ?
STACKF	ENDS

CODE	SEGMENT 'CODE'
ASSUME	CS:CODE, DS:DGROUP, SS:DGROUP

PUBLIC	MATFIL
MATFIL	PROC FAR

	PUSH	BP			;Save FORTRAN registers on the stack
	MOV	BP,SP
	PUSH	DI
	PUSH	ES
	PUSH	DS

INCLUDE WTERR.INC

;******************************************************************************
;			Test for correct DOS Version
;******************************************************************************

	MOV	AH,30H
	INT	21H
	CMP	AL,03H
	JL	ERR1

;******************************************************************************
;	Set up registers and call DOS Funtion #61 to open file #1
;******************************************************************************

	LDS	DX,DWORD PTR [BP].FLNAM1	;Load ptr to FLNAM1 into DS:DX
	MOV	AL,20H			;Set open mode to read-only
	
	MOV	AH,3DH			;Call DOS Function #61
	INT	21H			;to open the file
	JC	ERR2			;Open Failed

;******************************************************************************
;		Call Dos Function #87 to get file date and time for file #1
;******************************************************************************

	MOV	BX,AX		;Move filehandle to BX register
	MOV	AL,00H		;Retrieve date / time
	MOV	AH,57H		;Call DOS Function #87 to get date/time
	INT 	21H
	
	PUSH	CX			;Save time for FLNAM1
	PUSH	DX			;Save date for FLNAM2

;******************************************************************************
;		Call Dos Function #62 to close file #1
;******************************************************************************

	MOV	AH,3EH			;Call Dos Function #62
	INT	21H			;to close the file
	

;******************************************************************************
;	Set up registers and call DOS Funtion #61 to open file #2
;******************************************************************************

	LDS	DX,DWORD PTR [BP].FLNAM2	;Load ptr to FLNAM2 into DS:DX
	MOV	AL,20H			;Set open mode to read-only
	
	MOV	AH,3DH			;Call DOS Function #61
	INT	21H			;to open the file
	JC	ERR2			;Open Failed

;******************************************************************************
;		Call Dos Function #87 to get file date and time for file #2
;******************************************************************************

	MOV	BX,AX		;Move filehandle to BX register
	MOV	AL,00H		;Retrieve date / time
	MOV	AH,57H		;Call DOS Function #87 to get date/time
	INT 	21H


;******************************************************************************
;		Call Dos Function #62 to close file #2
;******************************************************************************

	MOV	AH,3EH			;Call Dos Function #62
	INT	21H			;to close the file
	

;******************************************************************************
;		Compare date/time for the two files
;******************************************************************************

	POP	AX		;Retrieve date of first file from the stack
	POP	BX		;Retrieve time of first file from the stack
	CMP	AX,DX		;Compare the dates of the two files
	JO	DCDDAT		;Compare overflows, so break out yr,mo,day
	JG	NOMATC
	JE	CKTIME		;Dates match, so check the times.
	JMP	SHORT OKAY
	
CKTIME:	CMP	BX,CX		;Compare the times of the two files
	JG	NOMATC
	JNO	OKAY		;No overflow, so files match
	JMP	DCDTIM		;Compare overflows, so break out hour, minute
	
OKAY:	LES	DI,DWORD PTR [BP].MATCH		;FILES MATCH!
	MOV	BYTE PTR ES:[DI],01H

	LES	DI,DWORD PTR [BP].EFLAG
	MOV	BYTE PTR ES:[DI],01H	;Set EFLAG = 1 for success
	JMP	EXIT
	
;******************************************************************************
;		Set EFLAG = 0 to indicate failure to match files
;******************************************************************************

ERR1:	LES	DI,EPTR1
	_WTERR
	JMP	SHORT ERROR
	
ERR2:	LES	DI,EPTR2
	_WTERR

	
ERROR:	LES	DI,DWORD PTR [BP].EFLAG
	MOV	BYTE PTR ES:[DI],00H


;******************************************************************************
;				Files don't Match
;******************************************************************************

NOMATC:	LES	DI,DWORD PTR [BP].MATCH
	MOV	BYTE PTR ES:[DI],00H
	LES	DI,DWORD PTR [BP].EFLAG
	MOV	BYTE PTR ES:[DI],01H	;Set EFLAG = 1 for success
	JMP	SHORT EXIT
	
;******************************************************************************
;	Decode the DOS date codes into year, month, day and compare.   This 
;	code is executed only if and overflow occurs when the DOS date codes
;	are directly compared.
;******************************************************************************

DCDDAT:	MOV	HOLD1,DX	;Save dates in local storage
	MOV	HOLD2,AX
	PUSH	CX		;Save times on the stack
	PUSH	BX
	SHR	DH,01H		;Decode the year for the files in DH and AH
	SHR	AH,01H
	CMP	DH,AH		;Compare the years
	JG	CLRSF		;Go clear the stack, and then continue at NOMATC
	
; Years match, so decode the months

	MOV	CL,04H		;Set up number of times to shift
	MOV	DX,HOLD1	;Load the DOS date codes from temp storage
	MOV	AX,HOLD2
	SHR	DX,1		;Decode month in AL, DL
	SHR	AX,1
	SHR	DL,CL		
	SHR	AL,CL
	CMP	DL,AL		;Compare the months
	JG	CLRSF		;Go clear the stack, and then continue at NOMATC
	
; Months match, so decode the days


	MOV	DX,HOLD1	;Load the DOS date codes from temp storage
	MOV	AX,HOLD2
	AND	DL,1FH		;Decode day in DH, AH
	AND	AL,1FH

; DATES MATCH !
	
	POP	BX	;Dates match - clear stack and go check times
	POP	CX
	JMP	CKTIME
	
;******************************************************************************
;	Dates match, so clear stack and go set return values
;******************************************************************************

CLRSF:	ADD	SP,04H
	JMP	NOMATC

;******************************************************************************
;	Decode the DOS time codes into Hour and Minute, and compare.  This
;	code is executed only if the direct compare of the DOS time codes
;	overflows.
;******************************************************************************

DCDTIM:	MOV	HOLD1,BX	;Save the DOS time codes in temp storage
	MOV	HOLD2,CX
	MOV	AX,CX		;Use AX for FLNAM2 time
	MOV	CL,03H		;Set number of times to shift
	SHR	BH,CL		;Decode hour in BH and AH
	SHR	AH,CL
	CMP	BH,AH		;Compare the hour for the files
	JG	NOMATC		
	
; Hours match, so decode the minutes

	MOV	BX,HOLD1	;Reload the DOS time codes from temp storage
	MOV	AX,HOLD2
	MOV	CL,05H		;Set number of times to shift
	SHL	BX,CL		;Decode the minute in BH and AH
	SHL	AX,CL
	MOV	CL,02H		;Set number of times to shift
	SHR	BH,CL
	SHR	AH,CL
	CMP	BH,AH		;Compare the minute for the files
	JG	NOMATC
	JMP	OKAY		;FILES MATCH !
	


;******************************************************************************

EXIT:	POP	DS			;Restore FORTRAN registers from stack
	POP	ES
	POP	DI
	POP	BP

	RET	10H			;Return to calling program

MATFIL	ENDP
CODE	ENDS
END
