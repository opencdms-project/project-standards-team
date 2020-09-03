PAGE ,132
;******************************************************************************
;                       PROCEDURE STOPPRT
;
;       Calling Language: MICROSOFT FORTRAN
;
;       Parameters: None
;
;       Entry Conditions: None
;
;       DESCRIPTION: Uses DOS Function Number 0103h to cancel all files in
;                    Print Queue.  Interrupt 2Fh Function number 0103h stops
;                    the current print job and removes all files from the
;                    print queue.
;******************************************************************************
;
DATA     SEGMENT PUBLIC 'DATA'
DATA     ENDS
DGROUP   GROUP DATA

STACKF   STRUC                           ;Define Stack Addresses
   SAVEBP       DW ?
   RTNADR       DD ?
STACKF   ENDS

CODE     SEGMENT 'CODE'
ASSUME   CS:CODE, DS:DGROUP, SS:DGROUP

PUBLIC   STOPPRT
STOPPRT  PROC FAR

         PUSH    BP                      ;Save FORTRAN registers on the stack
         MOV     BP,SP

;******************************************************************************
;     Stops the current print job and remove all files from the print queue
;******************************************************************************
 
         MOV     AX,0103H
         INT     2FH

;******************************************************************************

         POP     BP                      ;Restore FORTRAN registers from stack

         RET                             ;Return to calling program

STOPPRT  ENDP
CODE     ENDS
END
