PAGE ,132
;******************************************************************************
;			PROCEDURE STXFRA
;
;	Calling Language: MICROSOFT FORTRAN
;
;	Parameters: INTEGER*2  SEGADR,OFFST
;
;	Entry Conditions: SEGADR, OFFST contain HEXADECIMAL segment address
;			  and offset (respectively) of the area of memory to
;			  be used for the default Data Transfer Area.
;
;	DESCRIPTION: Uses DOS Function number 26 to set the default Disk 
;		     Transfer Area for data being read/written from/to a file
;		     using "SEQRED" and "SEQWRT".  No error checking is done in
;		     this routine, so the user should take care in specifying
;		     the address.
;******************************************************************************
;
DATA	SEGMENT PUBLIC 'DATA'
DATA	ENDS
DGROUP	GROUP DATA

STACKF	STRUC				;Define Stack Addresses
   SAVEBP	DW ?
   RTNADR	DD ?
   OFFST	DD ?
   SEGADR       DD ?
STACKF	ENDS

CODE	SEGMENT 'CODE'
ASSUME	CS:CODE, DS:DGROUP, SS:DGROUP

PUBLIC	STXFRA
STXFRA	PROC FAR

	PUSH	BP			;Save FORTRAN registers on the stack
	MOV	BP,SP
	PUSH	DI
	PUSH	ES
	PUSH	DS

;******************************************************************************
;			Set up registers and call DOS Funtion #26. 
;******************************************************************************

	LES	DI,DWORD PTR [BP].SEGADR	;Load pointer to DTA into DS:DX
	MOV	AX,ES:[DI]
	MOV	DS,AX
	LES	DI,DWORD PTR [BP].OFFST
	MOV	AX,ES:[DI]
	MOV	DX,AX
	
	MOV	AH,1AH			;Call DOS Function #26
	INT	21H			;to set the DTA

;******************************************************************************

EXIT:	POP	DS			;Restore FORTRAN registers from stack
	POP	ES
	POP	DI
	POP	BP

	RET	08H			;Return to calling program

STXFRA	ENDP
CODE	ENDS
END
