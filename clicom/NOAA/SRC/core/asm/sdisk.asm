PAGE ,132
;******************************************************************************
;			PROCEDURE SDISK
;
;	Calling Language: MICROSOFT FORTRAN
;
;	Parameters: INTEGER*2  DSKNUM,NUMDRV
;
;	Entry Conditions: DSKNUM must be coded as follows: 0 = A:, 1 = B:, etc.
;
;	DESCRIPTION: Uses DOS Function number 14 to set the default system 
;		     drive.  The number of disk drives on the system is
;		     returned in NUMDRV.  The user should note that this number
;		     is based on the LASTDRIVE command in the CONFIG.SYS  file,
; 		     and not the actual number of drives on the system.
;******************************************************************************
;
DATA	SEGMENT PUBLIC 'DATA'
DATA	ENDS
DGROUP	GROUP DATA

STACKF	STRUC				;Define Stack Addresses
   SAVEBP	DW ?
   RTNADR	DD ?
   NUMDRV       DD ?
   DSKNUM	DD ?
STACKF	ENDS

CODE	SEGMENT 'CODE'
ASSUME	CS:CODE, DS:DGROUP, SS:DGROUP

PUBLIC	SDISK
SDISK	PROC FAR

	PUSH	BP			;Save FORTRAN registers on the stack
	MOV	BP,SP
	PUSH	DI
	PUSH	ES

;******************************************************************************
;	Set up registers and call DOS Funtion #14.
;******************************************************************************

	LES	DI,DWORD PTR [BP].DSKNUM	;Load number of drive to make
	MOV	DL,ES:[DI]			;the default drive
	
	MOV	AH,0EH			;Call DOS Function #14
	INT	21H			;to set the drive

;******************************************************************************
;		Move number of drives to FORTRAN parameter
;******************************************************************************

	LES 	DI,DWORD PTR [BP].NUMDRV	;Set up pointer to NUMDRV
	MOV	BYTE PTR ES:[DI],AL

;******************************************************************************

EXIT:	POP	ES			;Restore FORTRAN registers from stack
	POP	DI
	POP	BP

	RET	08H			;Return to calling program

SDISK	ENDP
CODE	ENDS
END
