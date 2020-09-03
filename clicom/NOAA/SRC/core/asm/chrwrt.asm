PAGE ,132
;******************************************************************************
;			PROCEDURE CHRWRT
;
;	Calling Language: MICROSOFT FORTRAN
;
;	Parameters: INTEGER*2  JCHAR,BGCOL,FGCOL,NTIME
;
;	Entry Conditions: BGCOL,FGCOL are valid colors (No checking performed).
;			  JCHAR is ASCII value of character to be written.
;
;	DESCRIPTION: Uses ROM BIOS service #9 to write JCHAR to the screen in 
;		     FGCOLor on BGCOLor.  NTIME specifies the number of times
;		     to write the character.
;******************************************************************************

DATA      SEGMENT PUBLIC 'DATA'
DATA      ENDS
DGROUP    GROUP   DATA

STACKF	STRUC				;Define Stack Addresses
   SAVEBP	DW ?
   RTNADR	DD ?
   NTIME	DD ?
   FGCOL        DD ?
   BGCOL	DD ?
   JCHAR	DD ?
STACKF	ENDS

CODE      SEGMENT 'CODE'
          ASSUME  CS:CODE,DS:DGROUP,SS:DGROUP
	  
PUBLIC    CHRWRT
CHRWRT    PROC    FAR

          PUSH    BP                                
          MOV     BP,SP                              
          PUSH    SI
	  PUSH    ES
	  
;******************************************************************************
;		Compute color attribute based on FGCOL and BGCOL
;******************************************************************************
;
          LES     SI,DWORD PTR [BP].FGCOL
          MOV     BX,ES:[SI]                            
          LES     SI,DWORD PTR [BP].BGCOL
          MOV     AX,ES:[SI]
          MOV     CL,4
          SHL     AX,CL
          OR      BX,AX
	  
          MOV     AH,15		;Call ROM BIOS Service #15 to set the active
          INT     10H           ;Video page in BH register.
	  
;******************************************************************************
;	Load character and number of times to write, then call ROM BIOS
;	Service #9 to write the character.
;******************************************************************************

          LES     SI,DWORD PTR [BP].NTIME
          MOV     CX,ES:[SI]                            
          LES     SI,DWORD PTR [BP].JCHAR
          MOV     AX,ES:[SI]                            
          MOV     AH,09                              
          INT     010H

;******************************************************************************

	  POP	  ES		;Restore FORTRAN registers
          POP     SI            
          MOV     SP,BP
          POP     BP                                 
	  
          RET     010H          ;Return to calling program

CHRWRT    ENDP
CODE      ENDS
END                               

