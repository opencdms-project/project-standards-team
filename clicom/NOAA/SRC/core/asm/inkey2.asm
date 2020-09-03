PAGE ,80
;***************************************************************************
;                       PROCEDURE INKEY2
;
;       Calling Language: MICROSOFT FORTRAN
;
;       Parameters: CHARACTER*1 KEY
;
;       Entry Conditions: None
;
;       DESCRIPTION: Uses DOS service to read a character from the 
;                    keyboard buffer and returns it in KEY.  Non-zero
;                    values returned in KEY indicate standard ASCII
;                    characters.  A zero value returned in KEY indicates
;                    that a special key (e.g. a Function Key) has been 
;                    pressed.  The standard DOS coded value for this key
;                    will be returned upon a subsequent call to this routine.
;****************************************************************************
;
DATA    SEGMENT PUBLIC 'DATA'

DATA    ENDS
DGROUP  GROUP DATA

STACKF  STRUC                           ;Define Stack Addresses
   SAVEBP       DW ?
   RTNADR       DD ?
   EKEY         DD ?
   KEY          DD ?
STACKF  ENDS

CODE    SEGMENT 'CODE'
ASSUME  CS:CODE, DS:DGROUP, SS:DGROUP

PUBLIC  INKEY2
INKEY2  PROC FAR

        PUSH    BP                      ;Save FORTRAN registers on the stack
        MOV     BP,SP
        PUSH    DI
        PUSH    DS
        PUSH    ES
        

        MOV     AH,07H                  ;Call DOS keyboard Service #7
        INT     21H
        
        LES     DI,[BP].KEY             ;Load character into variable passed
        MOV     ES:[DI],AL              ;from calling FORTRAN program
        
        CMP     AL,0E0H                  ;CHECK FOR GRAYPAD EXTENDED CHARACTER
        JE      GET2                    ;NORMAL ASCII CHARACTER, SO RETURN
        
        CMP     AL,00H                  ;CHECK FOR OTHER EXTENDED CHARACTER
        JNE     EXIT
        
GET2:   MOV     AH,07H                  ;MAKE SECOND DOS CALL FOR EXTENDED CODE
        INT     21H

        LES     DI,[BP].EKEY            ;STORE EXTENDED CHARACTER IN FORTRAN VARIABLE
        MOV     ES:[DI],AL
        
EXIT:   POP     ES                      ;Restore FORTRAN registers from stack
        POP     DS
        POP     DI
        POP     BP

        RET     08H                     ;Return to calling program

INKEY2  ENDP
CODE    ENDS
END
