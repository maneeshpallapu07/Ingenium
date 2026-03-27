      *************************
       IDENTIFICATION DIVISION.
      *************************
 
       PROGRAM-ID. ASRQ2100.
 
       COPY XCWWCRHT.
 
      *****************************************************************
      **  MEMBER :  ASRQ2100                                         **
      **  REMARKS:  BATCH I/O PROGRAM FOR THE                        **
      **            APEX UPLOAD DATA FILE                            **
      **                                                             **
      **  DOMAIN :  PO                                               **
      **  CLASS  :  FD                                               **
      *****************************************************************
      **  DATE     AUTH.  DESCRIPTION                                **
      **                                                             **
      **  28FEB95  DEV    CREATED FOR '2100' FILE PROCESSING         **
APEX53**  30NOV95  JJS    UPGRADE TO INGENIUM 5.3 & WINAPEX 1.0,     **
APEX53**                  ADD WORKING STORAGE COPYBOOK XCWWPGWS,     **
APEX53**                  CHANGES TO SUPPORT I/O PROGRAMS            **
APEX54**  31DEC96  TJS    MODIFICATIONS FOR MAINTAINABILITY          **
007684**  30OCT98  56     CHECKPOINT/RESTART                         **
014590**  15DEC99  60     ARCHITECTURAL CHANGES                      **
      *****************************************************************
      /
      **********************
       ENVIRONMENT DIVISION.
      **********************
 
       INPUT-OUTPUT SECTION.
 
       FILE-CONTROL.
 
       COPY ACSS2100.
      /
      ***************
       DATA DIVISION.
      ***************
 
       FILE SECTION.
 
       COPY ACSD2100.
 
       COPY ACSR2100.
      /
 
       WORKING-STORAGE SECTION.
 
APEX53 COPY XCWWPGWS REPLACING '$VAR1' BY 'ASRQ2100'.
 
       COPY SQLCA.
 
       01  WS-WORKING-STORAGE.
           05  WS-FILE-OPEN-SWITCH          PIC X(01)  VALUE 'N'.
               88  WS-FILE-IS-OPEN                     VALUE 'Y'.
               88  WS-FILE-IS-CLOSED                   VALUE 'N'.
      /
014590*COPY XCWL0030.
      /
       COPY XCWTFCMD.
      /
      *****************
       LINKAGE SECTION.
      *****************
 
       01  WGLOB-GLOBAL-AREA.
       COPY XCWWGLOB.
      /
       COPY ACSW2100.
 
       01  W2100-LINK-RECORD         PIC X(3000).
      /
       PROCEDURE DIVISION  USING WGLOB-GLOBAL-AREA
007684                           W2100-SEQ-IO-WORK-AREA
                                 W2100-LINK-RECORD.
 
      *--------------
       0000-MAINLINE.
      *--------------
 
           IF  WS-FILE-IS-OPEN
007684     AND (W2100-SEQ-IO-COMMAND = TFCMD-WRITE-RECORD
007684     OR  W2100-SEQ-IO-COMMAND  = TFCMD-REWRITE-RECORD)
007684         MOVE  W2100-LINK-RECORD TO R2100-SEQ-REC-INFO
           END-IF.
 
007684     EVALUATE W2100-SEQ-IO-COMMAND
 
APEX54         WHEN TFCMD-READ-RECORD
                    PERFORM  2100-1000-READ
                        THRU 2100-1000-READ-X
 
APEX54         WHEN TFCMD-WRITE-RECORD
                    PERFORM  2100-2000-WRITE
                        THRU 2100-2000-WRITE-X
 
APEX54         WHEN TFCMD-OPEN-I-FILE
                    PERFORM  2100-3000-OPEN-INPUT
                        THRU 2100-3000-OPEN-INPUT-X
                    MOVE 'Y'           TO WS-FILE-OPEN-SWITCH
 
APEX54         WHEN TFCMD-OPEN-O-FILE
                    PERFORM  2100-4000-OPEN-OUTPUT
                        THRU 2100-4000-OPEN-OUTPUT-X
                    MOVE 'Y'           TO WS-FILE-OPEN-SWITCH
 
APEX54         WHEN TFCMD-CLOSE-FILE
                    PERFORM  2100-5000-CLOSE
                        THRU 2100-5000-CLOSE-X
                    MOVE 'N'           TO WS-FILE-OPEN-SWITCH
 
APEX54         WHEN TFCMD-OPEN-EXTEND-FILE
                    PERFORM  2100-6000-OPEN-EXTEND
                        THRU 2100-6000-OPEN-EXTEND-X
                    MOVE 'Y'           TO WS-FILE-OPEN-SWITCH
 
APEX54     END-EVALUATE.
 
           IF  WS-FILE-IS-OPEN
007684     AND (W2100-SEQ-IO-COMMAND = TFCMD-READ-RECORD
007684     OR  W2100-SEQ-IO-COMMAND  = TFCMD-READ-RECORD-FOR-UPDATE
007684     OR  W2100-SEQ-IO-COMMAND  = TFCMD-READNEXT-RECORD)
007684         MOVE R2100-SEQ-REC-INFO    TO W2100-LINK-RECORD
APEX54     END-IF.
 
           GOBACK.
 
       0000-MAINLINE-X.
           EXIT.
      /
      *****************************************************************
      * PROCESSING COPYBOOKS                                          *
      *****************************************************************
 
       COPY ACPI2100.
      /
       COPY XCPL0030.
 
      *****************************************************************
      **                 END OF PROGRAM ASRQ2100                     **
      *****************************************************************
