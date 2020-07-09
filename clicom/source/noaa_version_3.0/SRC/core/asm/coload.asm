PAGE ,132
;****************************************************************
; THIS PROGRAM HAS BEEN MODIFIED TO BE CALLABLE FROM MICRO-SOFT
; FORTRAN LEVEL 3.3.  IT WILL NOT WORK FOR PREVIOUS LEVELS OF
; MICRO-SOFT FORTRAN.  THE MODIFICATIONS ARE DELINEATED WITH
; ASTERISKS.  EWK 11/19/85
;*****************************************************************
;   COLOADER loads COSORT into memory and provides access to it.  The
;user modifies the parameters below and assembles to get COLOADER.OBJ.
;COLOADER.OBJ is linked to the calling program which (1) far calls 
;to COSORT_LOAD to load; and (2) far calls repeatedly to COSORT.
;
;The following parameters in COLOADER may be altered:
;
;  (1) OFFSET_COSORT offset part of address  (see PL/M-86 call stds)
;       0000h for MEDIUM calls:  four offsets passed, segments are DS
;       0028h for LARGE calls:   four segment,offset pairs are passed
;  (2) SEGMENT_COSORT segment part of address
;       0000h informs COLOADER to place COSORT right above COLOADER
;       xxxxh user specifies the segment where COSORT is to begin
;  (3) LENGTH_COSORT length, in paragraphs, of COSORT and workspace
;       0000h informs COLOADER to use all subsequent space
;       xxxxh user specifies length (must not be less than C80h)
;
;                                Information Resources, Inc.  June 1984
;
DSEG SEGMENT
public  COSORT         ; entry to COSORT after it is loaded

COSORT         db 00EAH; Public calling address to COSORT routine
OFFSET_COSORT  dw 0028h; Offset; 0000h for MEDIUM / 0028h for LARGE
SEGMENT_COSORT dw 00000h; Segment part. 0000 => right after TOPOF_COSOR
LENGTH_COSORT  dw 00000H; Length (in par.s) geq 0C80h  0000 => maximum.
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
PSPAD          DW 0
CURRENT_DIR    DB '\',64 DUP(0)
SORT_DIR       DB '\COSORT',0
READ_COSORT    DB 7,10,10,13,'READ COSORT RECORD          ',10,13,'$'
MISS_COSORT    db 7,10,10,13,'Missing/Bad COSORT.COR file.',10,13,'$'
NOMEM_COSORT   db 7,10,10,13,'Need more memory for COSORT.',10,13,'$'
FCB_COSORT db 0,'COSORT  COR',0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
          UDS  dw 0  ; User's DS
          USP  dw 0  ;        SP
          USS  dw 0  ;        SS
               dw 20 dup (0) ; Loader's Stack
        STACK  equ   $
TOPOF_COSORT EQU     $
DSEG ENDS
 
CSEG SEGMENT  ; Loader Subroutine and COSORT address calculation
public  COLOAD         ; loads COSORT into memory at segment & address
     assume cs:CSEG, ds:DSEG, SS:DSEG
     MEMLOC_COSORT =26h; Code address to insert total paragraphs
COLOAD      proc far   ; COSORT Loading Procedure - - - - - - - - 
     mov ax,DSEG
     push DS
     mov ds,ax
     pop UDS
     mov USS,ss
     mov USP,sp
     mov ss,ax
     mov sp,offset STACK
     MOV DL,0
;******************************************************************
     MOV SI,OFFSET CURRENT_DIR+1  ; RETREIVE CURRENT DIRECTORY
     MOV AH,047H                  ; PATH
     INT 21H
     MOV DX,OFFSET SORT_DIR       ; SET CURRENT DIRECTORY TO \UTIL
     MOV AH,03BH                  ; TO READ IN COSORT.COR
     INT 21H
;******************************************************************
     mov dx,offset FCB_COSORT  ; Open COSORT code file
     mov byte ptr FCB_COSORT+32,0
     mov ah,15
     int 33
     cmp al,255
     jnz L0_COSORT
     mov dx,offset MISS_COSORT
     jmp ERROR_COSORT
L0_COSORT:cmp SEGMENT_COSORT,0 ; User Entry?
     jnz L1_COSORT
;*****************************************************************
; THIS BLOCK OF CODE REPLACES THE ORIGINAL COLOADER LOGIC TO
; COMPUTE SEGMENT_COSORT BASED ON TOPOF_COSORT.  THIS NEW LOGIC
; RETREIVES THE END OF THE FORTRAN PROGRAM BY PICKING UP BYTES
; 2 AND 3 FROM THE PROGRAM PREFIX SEGMENT.  THE END OF THE 
; FORTRAN PROGRAM IS MOVED TO SEGMENT_COSRT AND BECOMES THE
; THE START ADDRESS THAT THE COLOADER USES TO LOAD IN COSORT.
; COLOAD MUST BE CALLED FROM THE MAIN PROGRAM IN ORDER FOR THIS
; LOGIC TO WORK
;*****************************************************************
     LES SI,DWORD PTR USP ; A(USP) TO SI  A(USS) TO ES
     MOV AX,ES:2[SI]      ; USERS CALLING SEGMENT ADR TO AX
     SUB AX,10H           ; COMPUTE ADRESS OF PROG SEG PREFIX
     MOV ES,AX            ; MOVE PSP ADR TO ES
     MOV SI,2             ; SET TO PICK UP BYTES 2&3 OF PSP
     MOV BX,ES:[SI]       ; MOVE END OF FORTRAN PROGRAM SEGMENT
     MOV  ES,BX           ; THIS CODE CORRECTS A FORTRAN 4.0
     MOV SI,0
     MOV AX,0
     MOV AL,ES:[SI]
     CMP AX,0005AH
     JE  ENDOK
     MOV  SI,3            ; PROBLEM BY ADDING AN EXTRA MEMORY
     ADD BX,5             ; BLOCK + 5 TO WHAT FORTRAN COMPUTES
     ADD BX,ES:[SI]       ; AS THE END OF MEMORY.
ENDOK:MOV SEGMENT_COSORT,BX ; ADR TO SEGMENT_COSORT
L1_COSORT:cmp LENGTH_COSORT,0 ; User Entry?
     jnz L2_COSORT
     les si,dword ptr USP ; to pick up users cs
     mov ax,es:2[si]
     sub ax,10h           ; address of Users block
     mov LENGTH_COSORT,ax ; hold temp
     MOV PSPAD,AX         ; SAVE PROG SEG PREFIX ADDRESS
     mov bx,0ffffh        ; impossible reduction
     mov es,ax
     mov ah,4ah           ; set block
     int 33
      MOV AX,SEGMENT_COSORT
      SUB AX,PSPAD
      SUB BX,AX
      SUB BX,0600H
      MOV LENGTH_COSORT,BX
      MOV AX,PSPAD          ; GET ADDRESS OF PROG SEG PREFIX
      MOV ES,AX
       MOV SI,2             ; POINT TO OFFSET 2 OF PSP
       MOV AX,SEGMENT_COSORT  ; NEW END OF FORTRAN PROGRAM IS
       ADD AX,LENGTH_COSORT   ; SEGMENT_COSORT+LENGTH_COSORT
       MOV ES:[SI],AX         ; STORE NEW END IN PSP+2
       SUB AX,PSPAD       ; DEALLOCATE UPPER MEMORY FOR C
       MOV BX,AX          ; ROUTINES TO USE
      MOV AH,4AH
      INT 33
;*******************************************************************
L2_COSORT:cmp LENGTH_COSORT,0C80h  ; Length check
     jnc L3_COSORT
L25_COSORT:     mov dx,offset NOMEM_COSORT
     jmp short ERROR_COSORT
L3_COSORT:push OFFSET_COSORT  ; Loop to read COSORT code
           mov OFFSET_COSORT,0
L4_COSORT:push ds
;*******************************************************************
; DOS DEBUG LOGIC.  IF ACTIVATED, CAN BE USED RUNNNING UNDER 
; DEBUG TO HALT COLOAD WITH A CTRL C.  THIS CAN BE USEFUL TO
; EXAMINE REGISTERS,CODE AND DATA.
;*******************************************************************
;     MOV DX,OFFSET READ_COSORT
;     MOV AH,9
;     INT 33
     lds dx,dword ptr OFFSET_COSORT
     mov ah,26  ; set DTA
     int 33
     pop ds
     mov dx,offset FCB_COSORT   ; do the read
     mov ah,20  ; read @ ds:FCB
     int 33
     add OFFSET_COSORT,128
     cmp al,0
     jz L4_COSORT
     pop OFFSET_COSORT
     mov dx,offset FCB_COSORT
     mov ah,16
     int 33
;**************************************************************
      LEA DX,CURRENT_DIR     ; RESTORE CURRENT DIRECTORY PATH
      MOV AH,03BH
      INT 21H
;***************************************************************
     push SEGMENT_COSORT
     pop es
     push LENGTH_COSORT
     pop es:MEMLOC_COSORT
     mov ax,UDS
     mov ss,USS
     mov sp,USP
     mov ds,ax
     ret
ERROR_COSORT:mov ah,9
     int 33
     mov ah,4ch
     int 33
COLOAD      endp
; ================== End of Loader Subroutine =========================
CSEG ends
     end
