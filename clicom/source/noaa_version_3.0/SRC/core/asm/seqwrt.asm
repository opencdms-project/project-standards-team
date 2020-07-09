PAGE ,132
;******************************************************************************
;			PROCEDURE SEQWRT
;
;	Calling Language: MICROSOFT FORTRAN
;
;	Parameters: CHARACTER*1 FCB(44)
;		    INTEGER*2  EFLAG
;
;	Entry Conditions: FCB contains information necessay for DOS file
;		 	  control block.  See DOS Programmer's Reference Manual
;
;	DESCRIPTION: Uses DOS Function number 20 to write the next sequential
;		     record to a file based on the information contained in the
;		     FCB.  The record is writen from the default Disk Transfer
;		     Area, which can be set by using "STXFRA".  EFLAG is set to
;		     one (1) to indicate success in writing the record. Other-
;		     wise, the EFLAG is set to zero (0) to indicate an error.
;******************************************************************************
;
DATA	SEGMENT PUBLIC 'DATA'

EMSG1	DB	'SEQWRT attempted to write to a full disk$'
EPTR1	DD	EMSG1
EMSG2	DB	'SEQWRT attempted to write a record that is too large'
	DB	' for the DOS transfer seqment$'
EPTR2	DD	EMSG2

DATA	ENDS
DGROUP	GROUP DATA

STACKF	STRUC				;Define Stack Addresses
   SAVEBP	DW ?
   RTNADR	DD ?
   EFLAG	DD ?
   FCBPTR       DD ?
STACKF	ENDS

CODE	SEGMENT 'CODE'
ASSUME	CS:CODE, DS:DGROUP, SS:DGROUP

PUBLIC	SEQWRT
SEQWRT	PROC FAR

	PUSH	BP			;Save FORTRAN registers on the stack
	MOV	BP,SP
	PUSH	DI
	PUSH	ES
	PUSH	DS


INCLUDE WTERR.INC

;******************************************************************************
;	Set up registers and call DOS Funtion #21. Set EFLAG if successful
;******************************************************************************

	LDS	DX,DWORD PTR [BP].FCBPTR	;Load pointer to FCB into DS:DX
	
	MOV	AH,15H			;Call DOS Function #21
	INT	21H			;to write the file

	
	CMP 	AL,00H			;Test for success in writing file
	JNE	CHKERR

	MOV	BYTE PTR ES:[DI],01H	;Set EFLAG = 1 for success
	JMP	SHORT EXIT

;******************************************************************************
;		Set EFLAG = 0 to indicate failure to write file
;******************************************************************************

CHKERR:	CMP	AL,02H
	JE	ERR2
	
ERR1:	LES	DI,EPTR1		;Point to error message
	_WTERR				;Enter _WTERR to write out message
	JMP	SHORT ERROR
	
	
ERR2:	LES	DI,EPTR2		;Point to error message	
	_WTERR				;Enter _WTERR to write out message
	

ERROR:	LES 	DI,DWORD PTR [BP].EFLAG	;Set up pointer to EFLAG
	MOV	BYTE PTR ES:[DI],00H

;******************************************************************************

EXIT:	POP	DS			;Restore FORTRAN registers from stack
	POP	ES
	POP	DI
	POP	BP

	RET	08H			;Return to calling program

SEQWRT	ENDP
CODE	ENDS
END
