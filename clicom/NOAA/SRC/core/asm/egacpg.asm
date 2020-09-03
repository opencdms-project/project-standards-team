PAGE ,132
;******************************************************************************
;			PROCEDURE EGACPG
;
;	Calling Language: MICROSOFT FORTRAN
;
;	Parameters: INTEGER*2  PAGADR
;
;	Entry Conditions: PAGADR is a valid page address.
;
;	DESCRIPTION: THIS ROUTINE USES ROM BIOS SERVICE #5 TO SET THE ACTIVE
;		     PAGE WHEN USING AN IBM COMPATIBLE EGA BOARD.  THE GRAFLIB
;		     SUBROUTINES CAN PERFORM THIS FUNCTION EXCEPT WHEN IN
;		     GRAPHICS MODE. THEY SHOULD NORMALLY BE USED EXCEPT FOR
;		     THIS ONE CASE.
;******************************************************************************

DATA      SEGMENT PUBLIC 'DATA'
DATA      ENDS
DGROUP    GROUP   DATA

STACKF	STRUC				;Define Stack Addresses
   SAVEBP	DW ?
   RTNADR	DD ?
   PAGADR	DD ?
STACKF	ENDS

CODE        SEGMENT   'CODE'
            ASSUME CS:CODE,DS:DGROUP,SS:DGROUP
	    
PUBLIC      EGACPG
EGACPG      PROC FAR

            PUSH      BP  	;SAVE REGISTERS
            MOV       BP,SP
            PUSH      DS
	    PUSH      ES
;******************************************************************************
;		Retrieve the PAGADR, and call ROM BIOS Service #5
;******************************************************************************

            LES      BX,DWORD PTR [BP].PAGADR 	;LOAD ADDRESS OF PAGE
            MOV      AX,[BX] 			;PAGE INTO AX  REGISTER
            MOV      AH,5    			;CALL ROM BIOS TO SET
            INT      10H     			;THE ACTIVE PAGE.

;******************************************************************************

	    POP	      ES		;Restore FORTRAN Registers
            POP       DS		
            POP       BP
	    
            RET      04H 		;RETURN TO CALLING PROGRAM

EGACPG      ENDP
CODE        ENDS
END
