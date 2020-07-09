PAGE ,132
;******************************************************************************
;			PROCEDURE MOVBYT
;
;	Calling Language: MICROSOFT FORTRAN
;
;	Parameters: INTEGER*2  K,A,L,B,M
;
;	Entry Conditions: None.
;
;	DESCRIPTION: THIS SUBROUTINE MOVES K BYTES FROM ARRAY A STARTING WITH
;		     THE LTH BYTE TO ARRAY B STARTING WITH THE MTH BYTE.  THE
;		     CALLING AND RETURN LINKAGE IS COMPATIBLE WITH MICRO-SOFT
;		     FORTRAN. INTEGER VALUES K,L AND M CAN BE EITHER 2 OR 4 
;		     BYTES.
;******************************************************************************

DATA      SEGMENT PUBLIC 'DATA'
DATA      ENDS
DGROUP    GROUP   DATA

STACKF	STRUC				;Define Stack Addresses
   SAVEBP	DW ?
   RTNADR	DD ?
   M		DD ?
   B	        DD ?
   L 		DD ?
   A		DD ?
   K		DD ?
STACKF	ENDS

CODE        SEGMENT   'CODE'
            ASSUME CS:CODE,DS:DGROUP,SS:DGROUP
	    
PUBLIC      MOVBYT
MOVBYT      PROC FAR

            PUSH      BP                   ; SAVE FORTRAN REGISTERS
            MOV       BP,SP
	    PUSH      DS
	    PUSH      ES
	    PUSH      SI
	    PUSH      DI

;******************************************************************************
;	Load adresses into registers to set up for move
;******************************************************************************
	    
            LES       BX,DWORD PTR [BP].K 	;ADDRESS OF K
            MOV       CX,ES:[BX]           	;CHARACTER COUNT TO CX
            LES       AX,DWORD PTR [BP].A 	;ADDRESS OF ARRAY A
            MOV       BX,ES
            MOV       DS,BX         		;SAVE ADDRESS OF A FOR MOVE
            LES       BX,DWORD PTR [BP].L 	;ADDRESS OF L
            MOV       BX,ES:[BX]    		;L TO BX
            SUB       BX,1
            ADD       AX,BX  			;AX=A(L)
            MOV       SI,AX  			;PUT IN SOURCE INDEX FOR MOVE
            LES       AX,DWORD PTR [BP].B 	;ADDRESS OF B ARRAY
            LES       BX,DWORD PTR [BP].M 	;ADDRESS OF M
            MOV       BX,ES:[BX] 		;M TO BX
            SUB       BX,1
            ADD       AX,BX  			;AX=B(M)
            MOV       DI,AX  		;PUT IN DESTINATION INDEX FOR MOVE

;******************************************************************************
;				Move the data
;******************************************************************************

            CLD       		;SET FLAG TO MOVE IN ASCENDING ORDER
	    REP       MOVSB     ;MOVE K BYTES FROM A(L) TO B(M)

;******************************************************************************

	    POP       DI	;RESTORE REGISTERS
	    POP       SI
	    POP       ES
            POP       DS
            POP       BP
            RET      014H   ;RETURN TO CALLING PROGRAM
	    
MOVBYT       ENDP
CODE        ENDS
END
