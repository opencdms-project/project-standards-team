PAGE ,132
;******************************************************************************
;			PROCEDURE RS232
;
;	Calling Language: MICROSOFT FORTRAN
;
;	Parameters: INTEGER*2  RDWTSW,NCHARS,RTNCOD
;		    CHARACTER*1 BUFFER(N), WHERE N IS USER-DEFINED
;		    CHARACTER*1 SRCHCR
;
;	Entry Conditions: 1 ó RDWTSW ó 2
;
;	DESCRIPTION: Uses ROM BIOS Serial Communication ServiceS #1 and #2 to
;		     read/write Serial Port 2 to/from BUFFER, based on the
;		     value of RDWTSW (1 = write, 2 = read).  When reading, the
;		     routine will also scan the input looking for SRCHCR. 
;		     RTNCODE is set to two (2) if the search character is
;		     found.  RTNCODE is set to one (1) if RDWTSW is invalid.
;******************************************************************************

DATA      SEGMENT PUBLIC 'DATA'
DATA      ENDS
DGROUP    GROUP   DATA

STACKF	STRUC				;Define Stack Addresses
   SAVEBP	DW ?
   RTNADR	DD ?
   RTNCOD	DD ?
   SRCHCR       DD ?
   NCHARS	DD ?
   BUFFER	DD ?
   RDWTSW       DD ?
STACKF	ENDS



code        segment   'code'
            assume cs:code,ds:dgroup,ss:dgroup

public      RS232
RS232       proc far

            push      bp		;Save FORTRAN registers
            mov       bp,sp
            push      ds
	    push      di
	    push      es

;******************************************************************************
;	Retrieve FORTRAN parameters, and set up registers for BIOS call
;******************************************************************************

            mov       dx,01H
            LES       DI,DWORD PTR [BP].NCHARS
            MOV       CX,ES:[DI]     		;# OF CHARACTERS TO XFER
            LES       DI,DWORD PTR [BP].SRCHCR
            MOV       BL,ES:[DI]     		;SEARCH CHARACTER
            LES       DI,DWORD PTR [BP].RDWTSW
            MOV       AH,ES:[DI]     		;READ/WRITE FUNCTION
            CMP       AH,1           		;TEST WHICH
            JE        WRITE
            CMP       AH,2          
            JE        READ
            MOV       DX,1
            JMP       SHORT EXIT       		;IMPROPER FUNCTION
	    
;******************************************************************************
;			Read data from port	    
;******************************************************************************


READ:       LES       DI,DWORD PTR [BP].BUFFER
READLOOP:   INT       14H            		;READ ONE CHARACTER
	    CMP	      AH,00H			;CHECK FOR READ ERROR
	    JE	      OKAY
	    MOV       AH,02H			;READ FROM PORT
	    JMP	      READLOOP                  ;TRY TO READ AGAIN
OKAY:       MOV       ES:[DI],AL      		;PUT IN CALLERS BUFFER
            CMP       BL,0           		;SEARCH CHARACTER?
            JE        NEXT           		;NO
            CMP       AL,BL          		;YES, CHECK FOR IT
            JE        EXIT           		;FOUND
NEXT:       ADD       DI,1           		;NO SET TO LOOP
            MOV       AH,2
            LOOP      READLOOP       		;READ UNTIL CX=0
            CMP       BL,0           		;LOOKING FOR SEARCH CHAR?
            JE        EXIT           		;NO
            MOV       DX,2           		;YES INDICATE NOT FOUND
            JMP       SHORT EXIT

;******************************************************************************
;			Write characters to port
;******************************************************************************

WRITE:      LES       DI,DWORD PTR [BP].BUFFER
WRITELOOP:  MOV       AL,ES:[DI]     		;CHARACTER TO AL
            INT       14H            		;SEND TO PORT
            ADD       DI,1
            MOV       AH,1           		;SET TO LOOP
            LOOP      WRITELOOP      		;WRITE UNTIL CX=0

;******************************************************************************

EXIT:       LES       DI,DWORD PTR [BP].RTNCOD
            MOV       ES:[DI],DX
	    pop       es
	    pop       di
            pop       ds
            pop       bp
            RET      014H			;Return to calling program
RS232       endp
CODE        ENDS
end
