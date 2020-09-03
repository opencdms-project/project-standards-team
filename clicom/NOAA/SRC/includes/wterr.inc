;******************************************************************************
;			MACRO _WTERR
;
;	Entry Conditions: ES:DI --> Error Message to be written.
;
;	Exit Conditions: AX,DX is destroyed
;
;	DESCRIPTION:  This MACRO writes uses DOS Function #9 to write out the
;		      error message pointed to by ES:DI.  Any message to be
;		      written by this macro must end with the $ sign. 
;******************************************************************************

_WTERR	MACRO

	ECOORD	EQU	1814H		;Location to write error message at



	MOV	DX,ECOORD	;Use ROM BIOS service #2 to position the cursor
	MOV	AH,02H
	INT	10H


	MOV 	AX,DI		;Move ES:DI values to DS:DX
	MOV	DX,AX
	PUSH 	DS
	MOV	AX,ES
	MOV	DS,AX
	MOV	AH,09		;Call DOS Function #9 (Write String)
	INT	21H
	POP	DS

ENDM				;End of _WTERR Macro
