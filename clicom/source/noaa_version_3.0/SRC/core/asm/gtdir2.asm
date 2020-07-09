PAGE ,132
;******************************************************************************
;			PROCEDURE GTDIR2
;
;	Calling Language: MICROSOFT FORTRAN
;
;	Parameters: CHARATER*1 STRING(64)
;		    INTEGER*2  DSKNUM, EFLAG
;
;	Entry Conditions: DSKNUM must be the number of a currently installed
;		          disk drive on the system
;
;	DESCRIPTION: Uses DOS Function #71 to determine the currently active 
;		     path on the disk drive specified by DRVNUM.  Drive is 
;		     determined by the numbering system 0 = default drvie, 
;		     1 = A:, 2 = B:, 3 = C:, etc.  The path is returned in
;		     STRING as an ASCIIZ string (the text is terminated with
;		     an ASCII NUL character).  The drive letter and the back-
;		     slash (\) for the root directory are not returned.  An 
;		     EFLAG of one (1) is returned if successful, and an EFLAG
;		     of zero (0) if not.
;******************************************************************************
;
DATA	SEGMENT PUBLIC 'DATA'

EMSG1	DB	'A bad Drive Number was passed to GTDIR2$'
EPTR1	DD	EMSG1
	
DATA	ENDS
DGROUP	GROUP DATA

STACKF	STRUC				;Define Stack Addresses
   SAVEBP	DW ?
   RTNADR	DD ?
   EFLAG	DD ?
   DSKNUM	DD ?
   STRING	DD ?
STACKF	ENDS

CODE	SEGMENT 'CODE'
ASSUME	CS:CODE, DS:DGROUP, SS:DGROUP

PUBLIC	GTDIR2
GTDIR2	PROC FAR

	PUSH	BP			;Save FORTRAN registers on the stack
	MOV	BP,SP
	PUSH	DI
	PUSH	ES
	PUSH	DS
	PUSH	SI
	
INCLUDE WTERR.INC



	LES	DI,[BP].DSKNUM		;Get paramater DRVNUM from stack and 
	MOV	DL,ES:[DI]		;and load into register for call
	
;******************************************************************************
;			Check for valid DSKNUM
;******************************************************************************

	CMP	DL,00H
	JL	ERROR
	CMP	DL,01AH
	JG	ERROR
	
;******************************************************************************
;		Call DOS Function 71 to retrieve the current path
;******************************************************************************

	LDS	SI,DWORD PTR [BP].STRING	;Set ptr to variable for the 
						;path name
	MOV	AH,47H				;Call DOS function 71 to
	INT	21H				;load the path to DS:SI

;******************************************************************************
;		Set EFLAG = 1 to indicate success
;******************************************************************************

	LES	DI,DWORD PTR [BP].EFLAG
	MOV	BYTE PTR ES:[DI],01H
	JMP	SHORT EXIT

;******************************************************************************
;	Write out error message and set EFLAG = 0 to indicate error
;******************************************************************************

ERROR:	LES	DI,DWORD PTR [BP].EFLAG
	MOV	BYTE PTR ES:[DI],00H
	LES	DI,EPTR1		;Write out error message
	_WTERR
	


;******************************************************************************

EXIT:	POP	SI			;Restore FORTRAN registers from stack
	POP	DS
	POP	ES			
	POP	DI
	POP	BP

	RET	0CH			;Return to calling program

GTDIR2	ENDP
CODE	ENDS
END
