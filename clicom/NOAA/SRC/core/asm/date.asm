PAGE ,132
;******************************************************************************
;			PROCEDURE DATE
;
;	Calling Language: MICROSOFT FORTRAN
;
;	Parameters: INTEGER*2  YEAR,DATCOM
;
;	Entry Conditions: None.
;
;	DESCRIPTION: Uses DOS Function number 42 to return the current system 
;		     date.  YEAR will be set to the current system year (1899 -
;		     2099).  DATCOM contains a numerical value which is a com-
;		     bination of the month and day.  The current system month
;		     may be obtained by dividing DATCOM by 256.  The current 
;		     system day can be obtained from MOD (DATCOM,256).
;******************************************************************************
;
DATA	SEGMENT PUBLIC 'DATA'
DATA	ENDS
DGROUP	GROUP DATA

STACKF	STRUC				;Define Stack Addresses
   SAVEBP	DW ?
   RTNADR	DD ?
   DATCOM	DD ?
   YEAR         DD ?
STACKF	ENDS

CODE	SEGMENT 'CODE'
ASSUME	CS:CODE, DS:DGROUP, SS:DGROUP

PUBLIC	DATE
DATE	PROC FAR

	PUSH	BP			;Save FORTRAN registers on the stack
	MOV	BP,SP
	PUSH	DI
	PUSH	ES

;******************************************************************************
;	Set up registers and call DOS Funtion #42. 
;******************************************************************************

	MOV	AH,2AH			;Call DOS Function #42
	INT	21H			;to retrieve the system date

	LES 	DI,DWORD PTR [BP].YEAR	;Set up pointer to YEAR
	MOV	WORD PTR ES:[DI],CX	;Move year value to FORTRAN parameter

	LES 	DI,DWORD PTR [BP].DATCOM	;Set up pointer to DATCOM
	MOV	WORD PTR ES:[DI],DX	;Move month-day  value to FORTRAN 
					;parameter

;******************************************************************************

EXIT:	POP	ES			;Restore FORTRAN registers from stack
	POP	DI
	POP	BP

	RET	08H			;Return to calling program

DATE	ENDP
CODE	ENDS
END
