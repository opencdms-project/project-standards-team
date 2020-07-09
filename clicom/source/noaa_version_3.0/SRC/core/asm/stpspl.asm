PAGE ,132
;******************************************************************************
;			PROCEDURE STPSPL
;
;	Calling Language: MICROSOFT FORTRAN
;
;	Parameters: INTEGER*2 EFLAG
;
;	Entry Conditions: None.
;
;	DESCRIPTION: Uses IBM PC-LAN PROGRAM Interrupt Service 06H to
;	 	     terminate a spool to the LAN printer.  If successful,
;		     an EFLAG value of one (1) is returned.  If the spool
;		     terminate fails, an EFLAG of zero (0) is returned.
; 
;	      ****THIS ROUTINE WILL NOT WORK ON DOS SYSTEMS BELOW 3.1****
;******************************************************************************

DATA      SEGMENT PUBLIC 'DATA'
DATA      ENDS
DGROUP    GROUP   DATA

STACKF	STRUC				;Define Stack Addresses
   SAVEBP	DW ?
   RTNADR	DD ?
   EFLAG	DD ?
STACKF	ENDS

CODE        SEGMENT   'CODE'
ASSUME      CS:CODE, DS:DGROUP, SS:DGROUP
	    
PUBLIC      STPSPL
STPSPL      PROC FAR

            PUSH      BP  	;SAVE FORTRAN REGISTERS ON STACK
            MOV       BP,SP
	    PUSH      DI
	    PUSH      ES
	    
;******************************************************************************
;		Set up registers and do PC-LAN Program Interrupt
;******************************************************************************
	    
            MOV      AH,06H   		
            MOV      AL,03H
            INT      2AH    	 	
            JC       ERROR      	 ;IF CARRY SET, THEN ERROR
	    
            LES      DI,DWORD PTR [BP].EFLAG     ;SET EFLAG=1 IF SUCCESSFUL
            MOV      BYTE PTR ES:[DI],01H
            JMP      SHORT EXIT

;******************************************************************************
;		SET THE ERROR FLAG
;******************************************************************************

ERROR:      LES      DI,DWORD PTR [BP].EFLAG     
            MOV      BYTE PTR ES:[DI],00H

;******************************************************************************

EXIT:       POP	      ES 		;RESTORE REGISTERS
            POP       DI
	    POP       BP
	    
	    
	    
            RET       04H 		;RETURN TO CALLING PROGRAM

STPSPL      ENDP
CODE        ENDS
END
