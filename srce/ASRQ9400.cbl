      *************************
       IDENTIFICATION DIVISION.
      *************************
 
       PROGRAM-ID. ASRQ9400.
 
       COPY XCWWCRHT.
 
      *****************************************************************
      **  MEMBER :  ASRQ9400                                         **
      **  REMARKS:  BATCH I/O PROGRAM FOR PROCESSING THE             **
      **            APPLICATION ID'S FROM ILLUSTRATION/APPLICATION   **
      **            SYSTEM                                           **\
      **                                                             **
      *****************************************************************
      **  DATE     AUTH.  DESCRIPTION                                **
      **                                                             **
      **  31AUG01  DPK    CREATED FOR '9400' FILE PROCESSING         **
NWLXML**  28JUL09  CTS    INCLUDED CHANNEL TYPE IN THE AS9400 LAYOUT **
      *****************************************************************
 
      /
      **********************
       ENVIRONMENT DIVISION.
      **********************
 
       INPUT-OUTPUT SECTION.
 
       FILE-CONTROL.
 
       COPY ACSS9400.
      /
      ***************
       DATA DIVISION.
      ***************
 
       FILE SECTION.
 
       COPY ACSD9400.
 
       COPY ACSR9400.
      /
       WORKING-STORAGE SECTION.
 
       COPY XCWWPGWS REPLACING '$VAR1' BY 'ASRQ9400'.
 
       COPY SQLCA.
 
       01  WS-WORKING-STORAGE.
           05  WS-FILE-OPEN-SWITCH          PIC X(01)  VALUE 'N'.
               88  WS-FILE-IS-OPEN                     VALUE 'Y'.
               88  WS-FILE-IS-CLOSED                   VALUE 'N'.
      /
       COPY XCWTFCMD.
      /
      *****************
       LINKAGE SECTION.
      *****************
 
       01  WGLOB-GLOBAL-AREA.
       COPY XCWWGLOB.
      /
       COPY ACSW9400.
 
NWLXML*       01  W9400-LINK-RECORD         PIC X(15).
NWLXML 01  W9400-LINK-RECORD         PIC X(16).
      /
       PROCEDURE DIVISION  USING WGLOB-GLOBAL-AREA
                                 W9400-SEQ-IO-WORK-AREA
                                 W9400-LINK-RECORD.
 
      *--------------
       0000-MAINLINE.
      *--------------
 
           IF  WS-FILE-IS-OPEN
           AND (W9400-SEQ-IO-COMMAND = TFCMD-WRITE-RECORD
           OR  W9400-SEQ-IO-COMMAND  = TFCMD-REWRITE-RECORD)
               MOVE  W9400-LINK-RECORD TO R9400-SEQ-REC-INFO
           END-IF.
 
           EVALUATE W9400-SEQ-IO-COMMAND
 
               WHEN TFCMD-READ-RECORD
                    PERFORM  9400-1000-READ
                        THRU 9400-1000-READ-X
 
               WHEN TFCMD-WRITE-RECORD
                    PERFORM  9400-2000-WRITE
                        THRU 9400-2000-WRITE-X
 
               WHEN TFCMD-OPEN-I-FILE
                    PERFORM  9400-3000-OPEN-INPUT
                        THRU 9400-3000-OPEN-INPUT-X
                    MOVE 'Y'           TO WS-FILE-OPEN-SWITCH
 
               WHEN TFCMD-OPEN-O-FILE
                    PERFORM  9400-4000-OPEN-OUTPUT
                        THRU 9400-4000-OPEN-OUTPUT-X
                    MOVE 'Y'           TO WS-FILE-OPEN-SWITCH
 
               WHEN TFCMD-CLOSE-FILE
                    PERFORM  9400-5000-CLOSE
                        THRU 9400-5000-CLOSE-X
                    MOVE 'N'           TO WS-FILE-OPEN-SWITCH
 
               WHEN TFCMD-OPEN-EXTEND-FILE
                    PERFORM  9400-6000-OPEN-EXTEND
                        THRU 9400-6000-OPEN-EXTEND-X
                    MOVE 'Y'           TO WS-FILE-OPEN-SWITCH
 
           END-EVALUATE.
 
           IF  WS-FILE-IS-OPEN
           AND (W9400-SEQ-IO-COMMAND = TFCMD-READ-RECORD
           OR   W9400-SEQ-IO-COMMAND = TFCMD-READ-RECORD-FOR-UPDATE
           OR   W9400-SEQ-IO-COMMAND = TFCMD-READNEXT-RECORD)
                MOVE R9400-SEQ-REC-INFO    TO W9400-LINK-RECORD
           END-IF.
 
           GOBACK.
 
       0000-MAINLINE-X.
           EXIT.
      /
      *****************************************************************
      * PROCESSING COPYBOOKS                                          *
      *****************************************************************
 
       COPY ACPI9400.
      /
       COPY XCPL0030.
 
      *****************************************************************
      **                 END OF PROGRAM ASRQ9400                     **
      *****************************************************************
