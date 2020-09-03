PAGE ,132
;******************************************************************************
;			PROCEDURE RDCHAR
;
;	Calling Language: MICROSOFT FORTRAN
;
;	Parameters: INTEGER*2  JCHAR,BGCOL,FGCOL
;
;	Entry Conditions: None
;
;	DESCRIPTION: Uses ROM BIOS service #8 to read the character (JCHAR)
;                    and its attribute (FGCOLOR and BGCOLOR) at the current
;                    cursor position.
;
;******************************************************************************

DATA      SEGMENT PUBLIC 'DATA'
DATA      ENDS
DGROUP    GROUP   DATA

STACKF	STRUC				;Define Stack Addresses
   SAVEBP	DW ?
   RTNADR	DD ?
   FGCOL        DD ?
   BGCOL	DD ?
   JCHAR	DD ?
STACKF	ENDS

CODE      SEGMENT 'CODE'
          ASSUME  CS:CODE,DS:DGROUP,SS:DGROUP
	  
PUBLIC    RDCHAR
RDCHAR    PROC    FAR

          PUSH    BP                                
          MOV     BP,SP                              
          PUSH    DI
	  PUSH    ES
	  
;******************************************************************************
;         Set the active Video page in BX register
;******************************************************************************
;
          MOV     AH,15		;Call ROM BIOS Service #15 to set the active
          INT     10H           ;Video page in BH register.

;******************************************************************************
;	Read the character and its attribute at the current cursor position
;******************************************************************************

          MOV     AH,08                              
          INT     010H

          LES     DI,DWORD PTR [BP].JCHAR
          MOV     BYTE PTR ES:[DI],AL                            

;******************************************************************************
;         Retrieve FGCOL and BGCOL based on attribute in AX register
;******************************************************************************
;
          MOV     BL,AH                            
          MOV     CL,4
          SHR     BL,CL

          LES     DI,DWORD PTR [BP].FGCOL
          MOV     BYTE PTR ES:[DI],AH                            
          LES     DI,DWORD PTR [BP].BGCOL
          MOV     BYTE PTR ES:[DI],BL                            

;******************************************************************************

	  POP	  ES		;Restore FORTRAN registers
          POP     DI            
          MOV     SP,BP
          POP     BP                                 
	  
          RET     0CH          ;Return to calling program

RDCHAR    ENDP
CODE      ENDS
END                               

