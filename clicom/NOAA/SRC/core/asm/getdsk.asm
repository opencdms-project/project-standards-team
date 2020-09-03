PAGE ,132
;******************************************************************************
;			PROCEDURE GETDSK
;
;	Calling Language: MICROSOFT FORTRAN
;
;	Parameters: INTEGER*2  DSKNUM
;
;	Entry Conditions: None
;
;	DESCRIPTION: Uses DOS Function #25 to determine the number of the 
;		     current default disk drive, based on the  numbering system
;		     0 = A:, 1 = B:, 2 = C:, etc.
;******************************************************************************
;
DATA	SEGMENT PUBLIC 'DATA'
DATA	ENDS
DGROUP	GROUP DATA

STACKF	STRUC				;Define Stack Addresses
   SAVEBP	DW ?
   RTNADR	DD ?
   DSKNUM	DD ?
STACKF	ENDS

CODE	SEGMENT 'CODE'
ASSUME	CS:CODE, DS:DGROUP, SS:DGROUP

PUBLIC	GETDSK
GETDSK	PROC FAR

	PUSH	BP			;Save FORTRAN registers on the stack
	MOV	BP,SP
	PUSH	DI
	PUSH	ES

;******************************************************************************
;	Call DOS Function #25 to retrieve current disk drive
;******************************************************************************

	MOV	AH,19H			
	INT	21H			

;******************************************************************************
;	Move number of disk drive from register into FORTRAN variable
;******************************************************************************

	LES	DI,DWORD PTR [BP].DSKNUM	;Move disk drive number to 
	MOV	BYTE PTR ES:[DI],AL		;FORTRAN variable

		
;******************************************************************************
	POP	ES			;Restore FORTRAN registers from stack
	POP	DI
	POP	BP

	RET	04H			;Return to calling program

GETDSK	ENDP
CODE	ENDS
END
