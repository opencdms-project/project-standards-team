PAGE ,132
;******************************************************************************
;			PROCEDURE KEYCHK
;
;	Calling Language: MICROSOFT FORTRAN
;
;	Parameters: INTEGER*2  KEYHIT
;
;	Entry Conditions: None.
;
;	DESCRIPTION:  This routine uses ROM BIOS Keyboard service #1 to 
;		      determine if a character has been entered from the 
;		      keyboard into the keyboard buffer.  KEYHIT is set to
;		      one (1) if a character is available, and to zero (0) if
;		      a character is not available. 
;******************************************************************************

DATA      SEGMENT PUBLIC 'DATA'
DATA      ENDS
DGROUP    GROUP   DATA

STACKF	STRUC				;Define Stack Addresses
   SAVEBP	DW ?
   RTNADR	DD ?
   KEYHIT	DD ?
STACKF	ENDS

CODE      SEGMENT 'CODE'
          ASSUME  CS:CODE,DS:DGROUP,SS:DGROUP
	  
PUBLIC    KEYCHK
KEYCHK    PROC    FAR

          PUSH    BP			;Save FORTRAN registers
          MOV     BP,SP
          PUSH    SI
	  PUSH    ES

;******************************************************************************
;	Set up registers and call ROM BIOS Keyboard Service #1 	  
;******************************************************************************

          LES     SI,DWORD PTR [BP].KEYHIT	;Load address of FORTRAN param

          MOV     AH,1        			;Call ROM BIOS Keyboard Svc
          INT     16H
	  
          JZ      EMPTY			;Keyboard buffer is empty
	  
;******************************************************************************
;	 Key has been struck, so set KEYHIT to 1
;******************************************************************************
;	  
          MOV     BYTE PTR ES:[SI],01H
          JMP     SHORT EXIT
;******************************************************************************
;		No key hit, so set KEYHIT to 0
;******************************************************************************
 
EMPTY:    MOV     BYTE PTR ES:[SI],00H

;******************************************************************************

EXIT:     POP     ES
	  POP     SI		;Restore FORTRAN Registers from stack
          MOV     SP,BP
          POP     BP
	  
          RET     04H		;Return to calling program
	  
KEYCHK    ENDP
CODE      ENDS
END
