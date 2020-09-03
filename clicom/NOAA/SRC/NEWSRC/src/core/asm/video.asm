PAGE ,132

DATA    SEGMENT PUBLIC 'DATA'

HLDCLR  DW 70H
EMSG1   DB      'Invalid video page passed to ACTPAG$'
EPTR1   DD      EMSG1
EMSG3A  DB      'Call to CLTEXT while in graphics mode illegal$'
EPTR3A  DD      EMSG3A
EMSG3B  DB      'Invalid border color passed to CLTEXT$'
EPTR3B  DD      EMSG3B
EMSG3C  DB      'Invalid background color passed to CLTEXT$'
EPTR3C  DD      EMSG3C
EMSG4A  DB      'Invalid graphics color passed to CWRITE$'
EPTR4A  DD      EMSG4A
EMSG4B  DB      'Invalid text color passed to CWRITE$'
EPTR4B  DD      EMSG4B
EMSG4C  DB      'String passed to CWRITE is too long$'
EPTR4C  DD      EMSG4C
EMSG6A  DB      'SETMOD received invalid screen mode for monochrome monitor$'
EPTR6A  DD      EMSG6A
EMSG6B  DB      'SETMOD received invalid screen mode for color monitor$'
EPTR6B  DD      EMSG6B
EMSG8A  DB      'Call to CLRCLS while in graphics mode illegal$'
EPTR8A  DD      EMSG8A
EMSG8B  DB      'Invalid foreground color passed to CLRCLS$'
EPTR8B  DD      EMSG8B
EMSG8C  DB      'Invalid background color passed to CLRCLS$'
EPTR8C  DD      EMSG8C

DATA    ENDS
DGROUP  GROUP DATA

CODE    SEGMENT 'CODE'
ASSUME  CS:CODE, DS:DGROUP, SS:DGROUP


INCLUDE WTERR.INC


;******************************************************************************
;                       PROCEDURE ACTPAG
;
;       Calling Language: MICROSOFT FORTRAN
;
;       Parameters: INTEGER*2 IPAGE,EFLAG
;
;       Entry Conditions: 0 ó IPAGE ó 3
;
;       DESCRIPTION: Uses ROM BIOS services to set the active video page, as
;                    specified in IPAGE.  An EFLAG of one (1) is returned if 
;                    successful, and an EFLAG of zero (0) if not.
;******************************************************************************
;

STACKF1 STRUC                           ;Define Stack Addresses
   SAVEBP1      DW ?
   RTNADR1      DD ?
   EFLAG1       DD ?
   IPAGE        DD ?
STACKF1 ENDS


PUBLIC  ACTPAG
ACTPAG  PROC FAR

        PUSH    BP                      ;Save FORTRAN registers on the stack
        MOV     BP,SP
        PUSH    ES
        PUSH    DI


;******************************************************************************
;  Determine current video status and check if correct for current screen mode
;******************************************************************************

        MOV     AH,0FH                  ;Use ROM BIOS service to determine
        INT     10H                     ;current video status


        CMP     AL,03H                  ;Check to see if screen is in 80 column
        JE      COL80                   ;mode
        CMP     AL,02H
        JE      COL80

        CMP     AL,01H                  ;Check to see if screen is in 40 column
        JE      COL40                   ;mode
        CMP     AL,0H
        JE      COL40
        JMP     SHORT ERROR1

;******************************************************************************
;       Check to see if requested page is valid for current video mode
;******************************************************************************

COL80:  LES     DI,DWORD PTR [BP].IPAGE         ;Check for 80 column mode
        MOV     AL,ES:[DI]
        CMP     AL,0H
        JL      ERROR1
        CMP     AL,03H
        JG      ERROR1
        JMP     SHORT OKAY1


COL40:  LES     DI,DWORD PTR [BP].IPAGE         ;Check for 40 column mode
        MOV     AL,ES:[DI]
        CMP     AL,0H
        JL      ERROR1
        CMP     AL,07H
        JG      ERROR1

;******************************************************************************
;       Passes tests, so make requested page active, and set EFLAG = 1
;******************************************************************************

OKAY1:  MOV     AH,05H
        INT     10H

        LES     DI,DWORD PTR [BP].EFLAG1
        MOV     BYTE PTR ES:[DI],01H
        JMP     SHORT EXIT1

;******************************************************************************
;                       Requested page is in error
;******************************************************************************

ERROR1: LES     DI,DWORD PTR [BP].EFLAG1
        MOV     BYTE PTR ES:[DI],0H
        LES     DI,EPTR1                ;Write out error message
        _WTERR
        

;******************************************************************************

EXIT1:  POP     DI                      ;Restore FORTRAN registers from stack
        POP     ES
        POP     BP


        RET     08H                     ;Return to calling program

ACTPAG  ENDP



;******************************************************************************
;                       PROCEDURE CLS
;
;       Calling Language: MICROSOFT FORTRAN
;
;       Parameters: None
;
;       Entry Conditions: None
;
;       DESCRIPTION: Uses ROM BIOS services to clear the screen (using the
;                    currently active video page).  Screen is cleared by
;                    defining the entire screen as a window and scrolling the
;                    entire window up.  The lines that are inserted are set to
;                    use a screen attribute byte of zero (blank black lines).
;                    After screen is cleared, the cursor is set to the home 
;                    (0,0) position.
;******************************************************************************
;

STACKF2 STRUC                           ;Define Stack Addresses
   SAVEBP2      DW ?
   RTNADR2      DD ?
STACKF2 ENDS


PUBLIC  CLS
CLS     PROC FAR

        PUSH    BP                      ;Save FORTRAN registers on the stack
        MOV     BP,SP

;******************************************************************************
;               Set Window Corners and other BIOS parameters
;******************************************************************************

        XOR     CX,CX                   ;Set upper left window corner (0,0)
        MOV     DX,184FH                ;Set lower right window corner (23,79)

        XOR     AX,AX                   ; Scroll the entire window

        MOV     BH,07H                  ;Set blank lines display attribute

;******************************************************************************
;               Clear screen by scrolling
;******************************************************************************

        MOV     AH,06H                  ;Call ROM BIOS Video Service #6 to 
        INT     10H                     ;clear the screen

;******************************************************************************
;               Set the cursor to the home position
;******************************************************************************

        MOV     AH,0FH                  ;Use ROM BIOS service #15 to get the
        INT     10H                     ;currently active display page in BH

        XOR     DX,DX                   ;Set cursor coordinates to (0,0)

        MOV     AH,02H                  ;Call ROM BIOS Video Service #2 to set
        INT     10H                     ;the cursor postion


;******************************************************************************

        POP     BP                      ;Restore FORTRAN registers from stack

        RET                             ;Return to calling program

CLS     ENDP



;******************************************************************************
;                       PROCEDURE CLTEXT
;
;       Calling Language: MICROSOFT FORTRAN
;
;       Parameters: INTEGER*2  BKGDCL,BRDRCL,EFLAG
;
;       Entry Conditions: 0 ó BKGDCL ó 7
;                         0 ó BRDRCL ó 15
;
;       DESCRIPTION: Uses ROM BIOS service to set the background and border
;                    colors in text mode.  If an invalid color is requested, 
;                    EFLAG will be set to zero (0). EFLAG will be set to
;                    one (1) to indicate success in setting the colors.
;******************************************************************************
;

STACKF3 STRUC                           ;Define Stack Addresses
   SAVEBP3      DW ?
   RTNADR3      DD ?
   EFLAG3       DD ?
   BRDRCL       DD ?
   BKGDCL       DD ?
STACKF3 ENDS


PUBLIC  CLTEXT
CLTEXT  PROC FAR

        PUSH    BP                      ;Save FORTRAN registers on the stack
        MOV     BP,SP
        PUSH    SI
        PUSH    DI
        PUSH    ES

;******************************************************************************
;       Call ROM BIOS to retrieve current video status
;******************************************************************************

        MOV     AH,0FH                  ;Call ROM BIOS to retrieve the current
        INT     10H                     ;video status using ROM BIOS video
                                        ;service #15

        CMP     AL,04H                  ;Check for graphics or text mode
        JL      TEXTMD
        CMP     AL,07H
        JE      TEXTMD
        JMP     SHORT ERR3A             ;Not in text mode, so error



;******************************************************************************
;       Set up registers for ROM BIOS call and check for valid color
;******************************************************************************

TEXTMD: XOR     BH,BH           ;Specify set BORDER color

        LES     DI,DWORD PTR [BP].BRDRCL        ;Move BORDER color parameter 
        MOV     BL,BYTE PTR ES:[DI]             ;from stack into BL

        CMP     BL,00H          ;Check for valid BORDER color
        JL      ERR3B
        CMP     BL,15H
        JG      ERR3B

        MOV     AH,0BH          ;Set up for call to ROM BIOS service #11
        INT     10H             ;Set the BORDER color

;******************************************************************************
;               Save BACKGROUND color in memory
;******************************************************************************

        LES     DI,DWORD PTR [BP].BKGDCL        ;Load background color from
        MOV     BL,BYTE PTR ES:[DI]             ; FORTRAN parameter

        CMP     BL,00H          ;Test for valid background color
        JL      ERR3C
        CMP     BL,07H  
        JG      ERR3C

        XOR     BH,BH
        MOV     CL,04H
        SHL     BL,CL
        XCHG    BL,CL

        PUSH    DS              ;Save color in memory
        MOV     AX,DATA
        MOV     DS,AX
        MOV     SI,OFFSET HLDCLR
        MOV     BYTE PTR DS:[SI],CL
        POP     DS



;******************************************************************************

DONE3:  LES     DI,DWORD PTR [BP].EFLAG3  ;Set EFLAG = 1 to indicate success
        MOV     BYTE PTR ES:[DI],01H
        JMP     SHORT EXIT3

;******************************************************************************
;                       Set the error flag
;******************************************************************************
;

ERR3A:  LES     DI,EPTR3A
        _WTERR
        JMP     SHORT ERROR3

ERR3B:  LES     DI,EPTR3B
        _WTERR
        JMP     SHORT ERROR3

ERR3C:  LES     DI,EPTR3C
        _WTERR                                  
        

ERROR3: LES     DI,DWORD PTR [BP].EFLAG3
        MOV     BYTE PTR ES:[DI],00H

;******************************************************************************

EXIT3:  POP     ES                      ;Restore FORTRAN registers from stack
        POP     DI
        POP     SI
        POP     BP

        RET     0CH                     ;Return to calling program

CLTEXT  ENDP



;******************************************************************************
;                       PROCEDURE CWRITE
;
;       Calling Language: MICROSOFT FORTRAN
;
;       Parameters: CHARACTER*1 TEXT(N)
;                   INTEGER*2  COLOR,EFLAG
;
;       Entry Conditions: TEXT(N) = NUL (ASCII NUL or 00h)
;                         Text Mode: 0 < COLOR ó 15
;                         Graphics Mode: 0 < COLOR  ó 3
;
;       DESCRIPTION: Uses ROM BIOS service to write the characters contained 
;                    in TEXT to the screen in the specified COLOR.  The last
;                    character in TEXT must be the ASCII NUL character.  If an
;                    invalid color is requested, EFLAG will be set to 
;                    zero (0). EFLAG will be set to one (1) to indicate
;                    success in writing the  characters.
;******************************************************************************
;

STACKF4 STRUC                           ;Define Stack Addresses
   SAVEBP4      DW ?
   RTNADR4      DD ?
   EFLAG4       DD ?
   COLOR        DD ?
   TEXT         DD ?
STACKF4 ENDS


PUBLIC  CWRITE
CWRITE  PROC FAR

        PUSH    BP                      ;Save FORTRAN registers on the stack
        MOV     BP,SP
        PUSH    SI
        PUSH    DI
        PUSH    ES

;******************************************************************************
;       Set up registers for ROM BIOS call and check for valid color
;******************************************************************************

        MOV     AH,0FH          ;Set current Video Page in BH register by
        INT     10H             ;calling ROM BIOS video service #15.  
                                ;Also, get the current video mode in AL
                                ;register.



        LES     DI,DWORD PTR [BP].COLOR         ;Move color parameter from 
        MOV     BL,BYTE PTR ES:[DI]             ;stack into BL

        

        CMP     AL,04H                  ;Check for graphics or text mode
        JL      TXTMOD
        CMP     AL,06H
        JG      TXTMOD

GRPHCS: CMP     BL,00H          ;Check for valid graphics color
        JL      ERR4A
        CMP     BL,03H
        JG      ERR4A
        JMP     SHORT OKAY4

TXTMOD: MOV     CL,BL           ;Check for valid text color
        SHL     CL,01H
        JC      CHKBLK
        CMP     BL,00H  
        JL      ERR4B
        CMP     BL,0FH
        JG      ERR4B
        JMP     SHORT OKAY4
        
CHKBLK: SUB     BL,080H         ;Check for valid blinking color
        CMP     BL,00H
        JL      ERR4B
        CMP     BL,0FH
        JG      ERR4B
        ADD     BL,080H

OKAY4:  PUSH    DS                      ;Set blank lines display attribute
        MOV     AX,DATA                 ;by retrieving background color from
        MOV     DS,AX                   ;memory
        MOV     SI,OFFSET HLDCLR
        MOV     AL,DS:[SI]
        OR      BL,AL
        POP     DS

        LES     DI,DWORD PTR [BP].TEXT  ;Set starting address of first
                                        ;character of array TEXT 


;******************************************************************************
;       Loop to write out each character of the array TEXT.  Stop when ASCII
;       NUL encountered (CHAR (0)).
;******************************************************************************

LDCHAR: MOV     AL,ES:[DI]              ;Load next character of array TEXT

        CMP     AL,00H          ;Check if next character is ASCII NUL
        JE      DONE4

        MOV     CX,01H          ;Set  number of times to write out each 
                                ;character

        MOV     AH,09H          ;Set up for call to ROM BIOS service #9
        INT     10H             ;Write out this character using ROM BIOS service

;******************************************************************************
;       Position the cursor to the next position on the screen
;******************************************************************************

        MOV     AH,03H          ;Read the current cursor position using ROM
        INT     10H             ;BIOS service #3

        INC     DL              ;Move cursor one column to the right

POSCUR: MOV     AH,02H          ;Set the cursor position using ROM BIOS service
        INT     10H             ;#2

;******************************************************************************
;       Move to the next character in the array, and loop back
;******************************************************************************

        INC     DI              ;Posistion to next character

        JZ      ERR4C           ;String is to long to write out
        JMP     SHORT LDCHAR    ;Loop back to write next character
;******************************************************************************
;               END OF CHARACTER WRITING LOOP
;******************************************************************************


;******************************************************************************

DONE4:  LES     DI,DWORD PTR [BP].EFLAG4  ;Set EFLAG = 1 to indicate success
        MOV     BYTE PTR ES:[DI],01H
        JMP     SHORT EXIT4
;******************************************************************************
;                       Set the error flag
;******************************************************************************
;

ERR4A:  LES     DI,EPTR4A
        _WTERR
        JMP     SHORT ERROR4
        
ERR4B:  LES     DI,EPTR4B
        _WTERR
        JMP     SHORT ERROR4
        
ERR4C:  LES     DI,EPTR4C
        _WTERR
                                

ERROR4: LES     DI,DWORD PTR [BP].EFLAG4
        MOV     BYTE PTR ES:[DI],00H

;******************************************************************************

EXIT4:  POP     ES                      ;Restore FORTRAN registers from stack
        POP     DI
        POP     SI
        POP     BP

        RET     0CH                     ;Return to calling program

CWRITE  ENDP


;******************************************************************************
;                       PROCEDURE SCROLL
;
;       Calling Language: MICROSOFT FORTRAN
;
;       Parameters: INTEGER*2 SCRDIR,NUMLIN,LROW,LCOL,HROW,HCOL
;
;       Entry Conditions: SCRDIR = 0 OR 1
;                         NUMLIN > 0
;                         0 ó LROW,HROW ó 24
;                         0 ó LCOL,HCOL ó 79
;
;       DESCRIPTION: Uses ROM BIOS services to scroll NUMLIN lines in the 
;                    direction indicated by SCRDIR, in the window defined by
;                    (LROW,LCOL) to (HROW,HCOL).  The lines that are inserted
;                    are set to use a screen attribute byte of zero (blank
;                    black lines).
;******************************************************************************
;

STACKF5 STRUC                           ;Define Stack Addresses
   SAVEBP5      DW ?
   RTNADR5      DD ?
   HCOL         DD ?
   HROW         DD ?
   LCOL         DD ?
   LROW         DD ?
   NUMLIN       DD ?
   SCRDIR       DD ?
STACKF5 ENDS


PUBLIC  SCROLL
SCROLL  PROC FAR

        PUSH    BP                      ;Save FORTRAN registers on the stack
        MOV     BP,SP
        PUSH    SI
        PUSH    ES
        PUSH    DI


;******************************************************************************
;       Retrieve parameters from the stack and set up for BIOS call
;******************************************************************************

        LES     DI,DWORD PTR [BP].NUMLIN        ;Retrieve number of lines to
        MOV     AL,ES:[DI]                      ;scroll

        LES     DI,DWORD PTR [BP].LROW          ;Retrieve upper left window
        MOV     CH,ES:[DI]                      ;corner

        LES     DI,DWORD PTR [BP].LCOL          
        MOV     CL,ES:[DI]              

        LES     DI,DWORD PTR [BP].HROW          ;Retrieve lower right window
        MOV     DH,ES:[DI]                      ;corner

        LES     DI,DWORD PTR [BP].HCOL          
        MOV     DL,ES:[DI]              

;******************************************************************************
;               Set color attribute for scrolled lines
;******************************************************************************

        PUSH    DS                      ;Set blank lines display attribute
        PUSH    AX                      ;by retrieving background color from
        MOV     AX,DATA                 ;memory
        MOV     DS,AX                   
        MOV     SI,OFFSET HLDCLR
        MOV     BH,DS:[SI]
        POP     AX
        POP     DS


;******************************************************************************
;       Determine direction to scroll and call ROM BIOS service #6 or #7
;******************************************************************************

        LES     DI,DWORD PTR [BP].SCRDIR        ;Retrieve direction to scroll
        MOV     BL,ES:[DI]              

        CMP     BL,01H                  ;Determine which BIOS call to use
        JL      SCRLDN

        MOV     AH,06H                  ;Scroll up
        INT     10H
        JMP     SHORT EXIT5

SCRLDN: MOV     AH,07H                  ;Scroll down
        INT     10H

;******************************************************************************

EXIT5:  POP     DI                      ;Restore FORTRAN registers from stack
        POP     ES
        POP     SI
        POP     BP

        RET     18H                     ;Return to calling program

SCROLL  ENDP



;******************************************************************************
;                       PROCEDURE SETMOD
;
;       Calling Language: MICROSOFT FORTRAN
;
;       Parameters: INTEGER*2  SCRMOD,EFLAG
;
;       Entry Conditions: 0 ó SCRMOD ó 15, SCRMOD <> 11,12
;
;       DESCRIPTION: Uses ROM BIOS service to set the video operation mode.
;                    Video mode is set based on the value passed in the SCRMOD
;                    parameter.  This parameter has 7 valid values, as
;                    described in the table below.  If the SCRMOD parameter has
;                    a valid value, the video mode is set, and EFLAG is set to 
;                    one (1) to indicate success.  Otherwise, the EFLAG is set
;                    to zero (0) to indicate an error.
;
;       Mode    Type            Colors  Resolution      Dimensions      Adapter
;       =======================================================================
;
;         0     text            b/w     medium          40 X 25         CGA
;         1     text            16      medium          40 X 25         CGA
;         2     text            b/w     high            80 X 25         CGA
;         3     text            16      high            80 X 25         CGA
;         4     graphics        4       medium          320 X 200       CGA
;         5     graphics        4 grey  medium          320 X 200       CGA
;         6     graphics        b/w     high            640 X 200       CGA
;         7     text            b/w     high            80 X 25         MA
;       =======================================================================
;
;******************************************************************************
;

STACKF6 STRUC                           ;Define Stack Addresses
   SAVEBP6      DW ?
   RTNADR6      DD ?
   EFLAG6       DD ?
   SCRMD6       DD ?
STACKF6 ENDS


PUBLIC  SETMOD
SETMOD  PROC FAR

        PUSH    BP                      ;Save FORTRAN registers on the stack
        MOV     BP,SP
        PUSH    SI
        PUSH    DI
        PUSH    ES


;******************************************************************************
;               Check if monochrome or color video equipment installed
;******************************************************************************

        INT     11H                     ;Call ROM BIOS equipment list service

        AND     AX,30H                  ;Isolate bits 4,5 which describe video
        CMP     AX,30H                  ;Check for monochrome monitor
        JNE     CLR


MONO:   LES     DI,DWORD PTR [BP].SCRMD6        ;Load screen mode parameter 
        MOV     AL,ES:[DI]                      ;into register
        CMP     AL,07H                  ;7 IS ONLY VALID MONOCHROME CODE
        JNE     ERR6A
        JMP     SHORT SET

;******************************************************************************
;       Check SCRMOD range, and load register for BIOS call
;******************************************************************************

CLR:    LES     DI,DWORD PTR [BP].SCRMD6        ;Load screen mode parameter 
        MOV     AL,ES:[DI]                      ;into register
        CMP     AL,0H                   ;Check to see if parameter SCRMOD is
        JL      ERR6B                   ;a valid code for color monitors
        CMP     AL,07H
        JG      ERR6B

;******************************************************************************
;       Make BIOS call and set EFLAG = 1 to indicate success
;******************************************************************************

SET:    MOV     AH,0H                   ;Call ROM BIOS to set the current
        INT     10H                     ;video mode using ROM BIOS video
                                        ;service #0

        LES     DI,DWORD PTR [BP].EFLAG6        ;Set EFLAG to 1
        MOV     BYTE PTR ES:[DI],01H
        JMP     SHORT EXIT6

;******************************************************************************
;       Set EFLAG = 0 to indicate SCRMOD out of bounds
;******************************************************************************

ERR6A:  LES     DI,EPTR6A
        _WTERR
        JMP     SHORT ERROR6
        
ERR6B:  LES     DI,EPTR6B
        _WTERR
        

ERROR6: LES     DI,DWORD PTR [BP].EFLAG6
        MOV     BYTE PTR ES:[DI],0H

;******************************************************************************

EXIT6:  POP     ES
        POP     DI
        POP     SI
        POP     BP

        RET     08H                     ;Return to calling program

SETMOD  ENDP



;******************************************************************************
;                       PROCEDURE STATUS
;
;       Calling Language: MICROSOFT FORTRAN
;
;       Parameters: INTEGER*2  SCRMOD,CLTYPE,VPAGE
;
;       Entry Conditions: None
;
;       DESCRIPTION: Uses ROM BIOS service to determine the current video mode,
;                    the current number of screen display columns, and the
;                    currently active  video page.  The current video mode is
;                    returned in SCRMOD, and is coded as follows:
;
;       Mode    Type            Colors  Resolution      Dimensions      Adapter
;       =======================================================================
;
;         0     text            b/w     medium          40 X 25         CGA
;         1     text            16      medium          40 X 25         CGA
;         2     text            b/w     high            80 X 25         CGA
;         3     text            16      high            80 X 25         CGA
;         4     graphics        4       medium          320 X 200       CGA
;         5     graphics        4 grey  medium          320 X 200       CGA
;         6     graphics        b/w     high            640 X 200       CGA
;         7     text            b/w     high            80 X 25         MA
;       =======================================================================
;
;                    The current number of display characters (columns) is
;                    returned in CLTYPE as 20, 40, or 80.  The number of the
;                    currently active video page is returned in VPAGE.
;******************************************************************************
;

STACKF7 STRUC                           ;Define Stack Addresses
   SAVEBP7      DW ?
   RTNADR7      DD ?
   VPAGE        DD ?
   CLTYPE       DD ?
   SCRMD7       DD ?
STACKF7 ENDS


PUBLIC  STATUS
STATUS  PROC FAR

        PUSH    BP                      ;Save FORTRAN registers on the stack
        MOV     BP,SP
        PUSH    SI
        PUSH    DI
        PUSH    ES

;******************************************************************************
;       Call ROM BIOS to retrieve current video status
;******************************************************************************

        MOV     AH,0FH                  ;Call ROM BIOS to retrieve the current
        INT     10H                     ;video status using ROM BIOS video
                                        ;service #15

;******************************************************************************
;       Move values from registers into FORTRAN variables
;******************************************************************************

        LES     DI,DWORD PTR [BP].SCRMD7        ;Move screen mode to FORTRAN
        MOV     BYTE PTR ES:[DI],AL             ;variable

                
        LES     DI,DWORD PTR [BP].CLTYPE        ;Move number of columns to
        MOV     BYTE PTR ES:[DI],AH             ;FORTRAN variable


        LES     DI,DWORD PTR [BP].VPAGE         ;Move active video page number
        MOV     BYTE PTR ES:[DI],BH             ;to FORTRAN variable
;******************************************************************************

EXIT7:  POP     ES                      ;Restore FORTRAN registers from stack
        POP     DI
        POP     SI
        POP     BP

        RET     0CH                     ;Return to calling program

STATUS  ENDP

;******************************************************************************
;                       PROCEDURE CLRCLS
;
;       Calling Language: MICROSOFT FORTRAN
;
;       Parameters: INTEGER*2 BCOL,FCOL
;
;       Entry Conditions: 0 ó BACKCL ó 7
;                         0 ó FORECL ó 15
;
;       DESCRIPTION: Uses ROM BIOS services to clear the screen and set the
;                    default background and foreground colors (using the
;                    currently active video page).  Screen is cleared by
;                    defining the entire screen as a window and scrolling the
;                    entire window up.  The lines that are inserted are set to
;                    use a screen attribute byte built from the specified
;                    colors.  After screen is cleared, the cursor is set to the
;                    home (0,0) position.
;******************************************************************************
;

STACKF8 STRUC                           ;Define Stack Addresses
   SAVEBP8      DW ?
   RTNADR8      DD ?
   FORECL       DD ?
   BACKCL       DD ?
STACKF8 ENDS


PUBLIC  CLRCLS
CLRCLS  PROC FAR

        PUSH    BP                      ;Save FORTRAN registers on the stack
        MOV     BP,SP
        PUSH    SI
        PUSH    ES
        PUSH    DI

;******************************************************************************
;       Call ROM BIOS to retrieve current video status
;******************************************************************************

        MOV     AH,0FH                  ;Call ROM BIOS to retrieve the current
        INT     10H                     ;video status using ROM BIOS video
                                        ;service #15

        CMP     AL,04H                  ;Check for graphics or text mode
        JL      BACKGD
        CMP     AL,07H
        JE      BACKGD
        JMP     SHORT ERR8A             ;Not in text mode, so error

;******************************************************************************
;               Check BACKGROUND color for validity
;******************************************************************************

BACKGD: LES     DI,DWORD PTR [BP].BACKCL        ;Load background color from
        MOV     BL,BYTE PTR ES:[DI]             ; FORTRAN parameter

        CMP     BL,00H          ;Test for valid background color
        JL      ERR8C
        CMP     BL,07H  
        JG      ERR8C

        XOR     BH,BH           ;Set background color attributes and save in CH
        MOV     CL,04H
        SHL     BL,CL
        XCHG    BL,CH

        PUSH    DS              ;Save color in memory
        MOV     AX,DATA
        MOV     DS,AX
        MOV     SI,OFFSET HLDCLR
        MOV     BYTE PTR DS:[SI],CH
        POP     DS

;******************************************************************************
;               Check FOREGROUND color for validity
;******************************************************************************

        LES     DI,DWORD PTR [BP].FORECL        ;Move color parameter from 
        MOV     BL,BYTE PTR ES:[DI]             ;stack into BL
        MOV     CL,BL           ;Check for valid text color
        SHL     CL,01H
        JC      CHKBK8
        CMP     BL,00H  
        JL      ERR8B
        CMP     BL,0FH
        JG      ERR8B
        JMP     SHORT OKAY8
        
CHKBK8: SUB     BL,080H         ;Check for valid blinking color
        CMP     BL,00H
        JL      ERR8B
        CMP     BL,0FH
        JG      ERR8B
        ADD     BL,080H

OKAY8:  OR      BL,CH           ;Set blank lines display attribute
        XCHG    BH,BL           ;combining the background and foreground
                                ;colors


;******************************************************************************
;               Set Window Corners and other BIOS parameters
;******************************************************************************

        XOR     CX,CX                   ;Set upper left window corner (0,0)
        MOV     DX,184FH                ;Set lower right window corner (23,79)

        XOR     AX,AX                   ; Scroll the entire window

;******************************************************************************
;               Clear screen by scrolling
;******************************************************************************

        MOV     AH,06H                  ;Call ROM BIOS Video Service #6 to 
        INT     10H                     ;clear the screen

;******************************************************************************
;               Set the cursor to the home position
;******************************************************************************

        MOV     AH,0FH                  ;Use ROM BIOS service #15 to get the
        INT     10H                     ;currently active display page in BH

        XOR     DX,DX                   ;Set cursor coordinates to (0,0)

        MOV     AH,02H                  ;Call ROM BIOS Video Service #2 to set
        INT     10H                     ;the cursor postion
        JMP     SHORT EXIT8

ERR8A:  LES     DI,EPTR8A
        _WTERR
        JMP     SHORT EXIT8

ERR8C:  LES     DI,EPTR8C
        _WTERR                                  
        JMP     SHORT EXIT8
        
ERR8B:  LES     DI,EPTR8B
        _WTERR

;******************************************************************************

EXIT8:  POP     DI                      ;Restore FORTRAN registers from stack
        POP     ES
        POP     SI
        POP     BP

        RET     08H                     ;Return to calling program

CLRCLS  ENDP

CODE    ENDS
END
