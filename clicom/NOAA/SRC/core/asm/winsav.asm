PAGE ,132
;******************************************************************************
;                       PROCEDURE WINSAV
;
;       Calling Language: MICROSOFT FORTRAN
;
;       Parameters: INTEGER*2  BUFFER(BSIZE),BSIZE,OPCODE,STROW,STCOL,ENDROW,
;                              ENDCOL,EFLAG
;
;       Entry Conditions: 0 ó OPCODE ó 1
;                         BSIZE = (ENDROW - STROW + 1)*(ENDCOL - STCOL + 1)
;                         0 ó STROW,ENDROW ó 24
;                         0 ó STCOL,ENDCOL ó 79
;
;       DESCRIPTION: Uses ROM BIOS services to save or restore an area of video
;                    memory defined by (STROW,STCOL) to (ENDROW,ENDCOL) to and
;                    from BUFFER.  If OPCODE is zero (0), the window is saved
;                    to BUFFER, and if it is one (1), the window is restored
;                    from BUFFER.  EFLAG will be set to one (1) to indicate
;                    success, and zero (0) to indicate an error.
;******************************************************************************

DATA      SEGMENT PUBLIC 'DATA'
        VIDEO_BASE      DW 0B800H
        ROW_LEN         DW 0000H
        BUFF_CT         DW 0000H
        EMSG1           DB 'Invalid OPCODE passed to WINSAV$'
        EPTR1           DD EMSG1
        EMSG2           DB 'BUFFER passed to WINSAV is too small to store '
                        DB 'specified window$'
        EPTR2           DD EMSG2
        EMSG3           DB 'BUFFER passed to WINSAV is to small to restore'
                        DB ' specified window$'
        EPTR3           DD EMSG3
        EMSG4           DB 'Invalid window coordinate(s) passed to WINSAV$'
        EPTR4           DD EMSG4
DATA      ENDS
DGROUP    GROUP   DATA

STACKF  STRUC                           ;Define Stack Addresses
   SAVEBP       DW ?
   RTNADR       DD ?
   EFLAG        DD ?
   ENDCOL       DD ?
   ENDROW       DD ?
   STCOL        DD ?
   STROW        DD ?
   OPCODE       DD ?
   BSIZE        DD ?
   BUFFER       DD ?
STACKF  ENDS

CODE    SEGMENT 'CODE'
        ASSUME  CS:CODE,DS:DGROUP,SS:DGROUP,ES:DGROUP

INCLUDE WTERR.INC


;******************************************************************************
;                       MACRO   Page_Offset
;
;       Entry Conditions: AX = current video page number.
;
;       Exit Conditions: DI = byte offset of the start of that video page.
;
;       Description:    This MACRO calculates the byte offset of the start of
;                       the specified video page from the video memory base
;                       address. The formula for this is:
;                                offset = (page #) * (# of bytes per page)
;******************************************************************************
  Page_Offset MACRO
        MOV   DI,AX           ; DI = page number
        SHL   DI,1            ; * 2
        SHL   DI,1            ; * 4
        SHL   DI,1            ; * 8
        SHL   DI,1            ; * 16
        SHL   DI,1            ; * 32
        SHL   DI,1            ; * 64
        SHL   DI,1            ; * 128
        SHL   DI,1            ; * 256
        SHL   DI,1            ; * 512
        SHL   DI,1            ; * 1024
        SHL   DI,1            ; * 2048
        SHL   DI,1            ; * 4096 = true # of bytes per page
                              ; (80*25*2) with 96 extra
  ENDM

;******************************************************************************
;                       MACRO   Offset_Within_Page
;
;       Entry Conditions: AH = row of starting position (0 ó AH ó 24)
;                         AL = column of starting position (0 ó AL ó 79)
;
;       Exit Conditions: BX = byte offset from start of video page 
;
;       Description:    This MACRO computes the offset of (ROW,COL) from the
;                       start of the video page.  Since one screen position
;                       consists of two bytes (character and attribute), one
;                       row contains 160 bytes.  Thus the offset can be
;                       computed as:
;                               offset = (160 * row) + (2 * column)
;******************************************************************************
  Offset_Within_Page MACRO
        PUSH  AX                ; save column

        MOV   AL,AH             ; move row into AL
        XOR   AH,AH
        SHL   AX,1              ; * 2
        SHL   AX,1              ; * 4
        SHL   AX,1              ; * 8
        SHL   AX,1              ; * 16
        SHL   AX,1              ; * 32
        MOV   BX,AX             ; BX = row*32
        SHL   AX,1              ; * 64
        SHL   AX,1              ; * 128
        ADD   BX,AX             ; BX = row*32 + row*128 = row*160

        POP   AX                ; restore column
        PUSH  AX
        XOR   AH,AH             ; clear row
        SHL   AL,1              ; AL = 2 * columns
        ADD   BX,AX             ; BX = offset
        POP   AX
  ENDM

          
PUBLIC    WINSAV
WINSAV    PROC    FAR

          PUSH  BP                                
          MOV   BP,SP                              
          PUSH  SI
          PUSH  ES
          PUSH  DI

;******************************************************************************
;       Initialize counter
;******************************************************************************

        XOR     AX,AX
        MOV     BUFF_CT,AX
        
;******************************************************************************
;       Call ROM BIOS to retrieve current video status
;******************************************************************************

        MOV     AH,0FH                  ;Call ROM BIOS to retrieve the current
        INT     10H                     ;video status using ROM BIOS video
                                        ;service #15

;******************************************************************************
;       Determine the video base address using the current video mode
;******************************************************************************

        CMP     AL,07H          ;Check for monochrome display
        JNE     COMPUT
        MOV     VIDEO_BASE,0B000H       ;Load address of video_base variable
                                        ;and set for monochrome monitor

;******************************************************************************
;       Compute the number of words to move for each row - (ENDCOL - STCOL + 1)
;******************************************************************************

COMPUT: LES     SI,DWORD PTR [BP].ENDCOL
        MOV     CH,ES:[SI]
        CMP     CH,00H
        JL      ERR4
        CMP     CH,4FH
        JG      ERR4
        LES     SI,DWORD PTR [BP].STCOL
        MOV     CL,ES:[SI]
        CMP     CL,00H
        JL      ERR4
        CMP     CL,4FH
        JG      ERR4
        CMP     CH,CL
        JL      ERR4
        SUB     CH,CL
        INC     CH
        XCHG    CH,CL
        XOR     CH,CH
        MOV     ROW_LEN,CX      ;ROW_LEN contains number of words to save

;******************************************************************************
;       Compute the starting position to save or restore
;******************************************************************************

        XCHG    AL,BH
        XOR     AH,AH
        PAGE_OFFSET             ;DI = offset of current page from video base 
        ADD     VIDEO_BASE,DI   ;Change video_base to point to current page
        
        
;******************************************************************************
;       Load window coordinates to save
;******************************************************************************

        LES     SI,DWORD PTR [BP].ENDROW
        MOV     DH,ES:[SI]
        CMP     DH,00H
        JL      ERR4
        CMP     DH,18H
        JG      ERR4
        LES     SI,DWORD PTR [BP].STROW
        MOV     AH,ES:[SI]      
        CMP     AH,00H
        JL      ERR4
        CMP     AH,18H
        JG      ERR4
        CMP     DH,AH
        JL      ERR4
        LES     SI,DWORD PTR [BP].STCOL
        MOV     AL,ES:[SI]
        JMP     SHORT CHKOP

;******************************************************************************
;       Error routine for bad window coordinates
;******************************************************************************
        
ERR4:   LES     DI,EPTR4
        _WTERR
        JMP     ERROR

;******************************************************************************
;       Determine whether to save or restore
;******************************************************************************
        
CHKOP:  LES     SI,DWORD PTR [BP].OPCODE
        MOV     CH,ES:[SI]
        CMP     CH,00H
        JE      SAVE
        CMP     CH,01H
        JE      RESTOR
        JMP     ERR1
        
;******************************************************************************
;       Loop to save each row of window in buffer
;******************************************************************************
        
SAVE:   LES     DI,DWORD PTR [BP].BUFFER
SAVELP: OFFSET_WITHIN_PAGE              ;BX NOW HAS OFFSET FROM VIDEO_BASE
        MOV     SI,BX
        MOV     CX,WORD PTR ROW_LEN
        ADD     WORD PTR BUFF_CT,CX
        PUSH    DS                      ;Save pointer to DATA segment
        MOV     DS,WORD PTR VIDEO_BASE
        CLD
        REP     MOVSW           ;Copy one screen row to BUFFER
        POP     DS                      ;Restore pointer to DATA segment
        PUSH    ES
        LES     SI,DWORD PTR [BP].BSIZE
        MOV     CX,WORD PTR BUFF_CT
        CMP     CX,ES:[SI]      ;BUFFER size exceeded?
        JG      ERR2
        POP     ES
        INC     AH
        CMP     DH,AH           ;Entire window saved?
        JGE     SAVELP
        JMP     SHORT OKAY
                        
        


;******************************************************************************
;       Loop to restore each row of window from BUFFER
;******************************************************************************
        
RESTOR: MOV     CX,DS
        MOV     ES,CX
        PUSH    DS
        LDS     SI,DWORD PTR [BP].BUFFER
STORLP: OFFSET_WITHIN_PAGE              ;BX NOW HAS OFFSET FROM VIDEO_BASE
        MOV     DI,BX
        MOV     CX,ES:[ROW_LEN]
        ADD     ES:[BUFF_CT],CX
        PUSH    ES                      ;Save pointer to DATA segment
        MOV     ES,WORD PTR ES:[VIDEO_BASE]
        CLD
        REP     MOVSW           ;Restore one row of screen from BUFFER
        LES     DI,DWORD PTR [BP].BSIZE
        MOV     CX,ES:[BUFF_CT]
        CMP     CX,ES:[DI]      ;BUFFER size exceeded?
        JG      ERR3
        POP     ES                      ;Restore pointer to DATA segment
        INC     AH
        CMP     DH,AH           ;Entire window restored?
        JGE     STORLP
        POP     DS

;******************************************************************************
;       Set EFLAG to indicate success
;******************************************************************************
                        
OKAY:   LES     SI,DWORD PTR [BP].EFLAG
        MOV     BYTE PTR ES:[SI],01H
        JMP     SHORT EXIT
        
                
;******************************************************************************
;                       ERROR ROUTINES
;******************************************************************************

ERR1:   LES     DI,EPTR1
        _WTERR
        JMP     SHORT ERROR
        
        
ERR2:   ADD     SP,02H
        LES     DI,EPTR2
        _WTERR
        JMP     SHORT ERROR
        
ERR3:   ADD     SP,02H
        POP     DS
        LES     DI,EPTR3
        _WTERR


ERROR:  LES     SI,DWORD PTR [BP].EFLAG
        MOV     WORD PTR ES:[SI],00H


;******************************************************************************

EXIT:   POP     DI              ;Restore FORTRAN registers
        POP     ES              
        POP     SI            
        POP     BP                                 
          
        RET     20H          ;Return to calling program

WINSAV    ENDP
CODE      ENDS
END                               

