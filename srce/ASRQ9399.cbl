      *************************
       IDENTIFICATION DIVISION.
      *************************
 
       PROGRAM-ID. ASRQ9399.
 
       COPY XCWWCRHT.
 
      *****************************************************************
      **  MEMBER :  ASRQ9399                                         **
      **  REMARKS:  BATCH I/O PROGRAM FOR PROCESSING THE             **
      **            APPLICATION ID'S FROM ILLUSTRATION/APPLICATION   **
      **            SYSTEM                                           **
      **                                                             **
      *****************************************************************
      **  DATE     AUTH.  DESCRIPTION                                **
      **                                                             **
P00697**  25OCT02  DPK    CREATED FOR '9399' FILE PROCESSING         **
      *****************************************************************
 
      /
      **********************
       ENVIRONMENT DIVISION.
      **********************
 
       INPUT-OUTPUT SECTION.
 
       FILE-CONTROL.
 
       COPY ACSS9399.
      /
      ***************
       DATA DIVISION.
      ***************
 
       FILE SECTION.
 
       COPY ACSD9399.
 
       COPY ACSR9399.
      /
       WORKING-STORAGE SECTION.
 
       COPY XCWWPGWS REPLACING '$VAR1' BY 'ASRQ9399'.
 
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
       COPY ACSW9399.
 
       01  W9399-LINK-RECORD         PIC X(15).
      /
       PROCEDURE DIVISION  USING WGLOB-GLOBAL-AREA
                                 W9399-SEQ-IO-WORK-AREA
                                 W9399-LINK-RECORD.
 
      *--------------
       0000-MAINLINE.
      *--------------
 
           IF  WS-FILE-IS-OPEN
           AND (W9399-SEQ-IO-COMMAND = TFCMD-WRITE-RECORD
           OR  W9399-SEQ-IO-COMMAND  = TFCMD-REWRITE-RECORD)
               MOVE  W9399-LINK-RECORD TO R9399-SEQ-REC-INFO
           END-IF.
 
           EVALUATE W9399-SEQ-IO-COMMAND
 
               WHEN TFCMD-READ-RECORD
                    PERFORM  9399-1000-READ
                        THRU 9399-1000-READ-X
 
               WHEN TFCMD-WRITE-RECORD
                    PERFORM  9399-2000-WRITE
                        THRU 9399-2000-WRITE-X
 
               WHEN TFCMD-OPEN-I-FILE
                    PERFORM  9399-3000-OPEN-INPUT
                        THRU 9399-3000-OPEN-INPUT-X
                    MOVE 'Y'           TO WS-FILE-OPEN-SWITCH
 
               WHEN TFCMD-OPEN-O-FILE
                    PERFORM  9399-4000-OPEN-OUTPUT
                        THRU 9399-4000-OPEN-OUTPUT-X
                    MOVE 'Y'           TO WS-FILE-OPEN-SWITCH
 
               WHEN TFCMD-CLOSE-FILE
                    PERFORM  9399-5000-CLOSE
                        THRU 9399-5000-CLOSE-X
                    MOVE 'N'           TO WS-FILE-OPEN-SWITCH
 
               WHEN TFCMD-OPEN-EXTEND-FILE
                    PERFORM  9399-6000-OPEN-EXTEND
                        THRU 9399-6000-OPEN-EXTEND-X
                    MOVE 'Y'           TO WS-FILE-OPEN-SWITCH
 
           END-EVALUATE.
 
           IF  WS-FILE-IS-OPEN
           AND (W9399-SEQ-IO-COMMAND = TFCMD-READ-RECORD
           OR   W9399-SEQ-IO-COMMAND = TFCMD-READ-RECORD-FOR-UPDATE
           OR   W9399-SEQ-IO-COMMAND = TFCMD-READNEXT-RECORD)
                MOVE R9399-SEQ-REC-INFO    TO W9399-LINK-RECORD
           END-IF.
 
           GOBACK.
 
       0000-MAINLINE-X.
           EXIT.
      /
      *****************************************************************
      * PROCESSING COPYBOOKS                                          *
      *****************************************************************
 
       COPY ACPI9399.
      /
       COPY XCPL0030.
 
      *****************************************************************
      **                 END OF PROGRAM ASRQ9399                     **
      *****************************************************************
