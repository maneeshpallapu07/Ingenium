      *************************
       IDENTIFICATION DIVISION.
      *************************
 
       PROGRAM-ID. ASRQADUP.
 
       COPY XCWWCRHT.
 
      *****************************************************************
      **  MEMBER :  ASRQADUP                                         **
      **  REMARKS:  BATCH I/O PROGRAM FOR PROCESSING THE             **
      **            APPLICATION ID'S FROM ILLUSTRATION/APPLICATION   **
      **            SYSTEM                                           **
      **                                                             **
      *****************************************************************
      **  DATE     AUTH.  DESCRIPTION                                **
      **                                                             **
P00697**  25OCT02  DPK    CREATED FOR 'ADUP' FILE PROCESSING         **
      *****************************************************************
 
      /
      **********************
       ENVIRONMENT DIVISION.
      **********************
 
       INPUT-OUTPUT SECTION.
 
       FILE-CONTROL.
 
       COPY ACSSADUP.
      /
      ***************
       DATA DIVISION.
      ***************
 
       FILE SECTION.
 
       COPY ACSDADUP.
 
       COPY ACSRADUP.
      /
       WORKING-STORAGE SECTION.
 
       COPY XCWWPGWS REPLACING '$VAR1' BY 'ASRQADUP'.
 
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
       COPY ACSWADUP.
 
       01  WADUP-LINK-RECORD         PIC X(80).
      /
       PROCEDURE DIVISION  USING WGLOB-GLOBAL-AREA
                                 WADUP-SEQ-IO-WORK-AREA
                                 WADUP-LINK-RECORD.
 
      *--------------
       0000-MAINLINE.
      *--------------
 
           IF  WS-FILE-IS-OPEN
           AND (WADUP-SEQ-IO-COMMAND = TFCMD-WRITE-RECORD
           OR  WADUP-SEQ-IO-COMMAND  = TFCMD-REWRITE-RECORD)
               MOVE  WADUP-LINK-RECORD TO RADUP-SEQ-REC-INFO
           END-IF.
 
           EVALUATE WADUP-SEQ-IO-COMMAND
 
               WHEN TFCMD-READ-RECORD
                    PERFORM  ADUP-1000-READ
                        THRU ADUP-1000-READ-X
 
               WHEN TFCMD-WRITE-RECORD
                    PERFORM  ADUP-2000-WRITE
                        THRU ADUP-2000-WRITE-X
 
               WHEN TFCMD-OPEN-I-FILE
                    PERFORM  ADUP-3000-OPEN-INPUT
                        THRU ADUP-3000-OPEN-INPUT-X
                    MOVE 'Y'           TO WS-FILE-OPEN-SWITCH
 
               WHEN TFCMD-OPEN-O-FILE
                    PERFORM  ADUP-4000-OPEN-OUTPUT
                        THRU ADUP-4000-OPEN-OUTPUT-X
                    MOVE 'Y'           TO WS-FILE-OPEN-SWITCH
 
               WHEN TFCMD-CLOSE-FILE
                    PERFORM  ADUP-5000-CLOSE
                        THRU ADUP-5000-CLOSE-X
                    MOVE 'N'           TO WS-FILE-OPEN-SWITCH
 
               WHEN TFCMD-OPEN-EXTEND-FILE
                    PERFORM  ADUP-6000-OPEN-EXTEND
                        THRU ADUP-6000-OPEN-EXTEND-X
                    MOVE 'Y'           TO WS-FILE-OPEN-SWITCH
 
           END-EVALUATE.
 
           IF  WS-FILE-IS-OPEN
           AND (WADUP-SEQ-IO-COMMAND = TFCMD-READ-RECORD
           OR   WADUP-SEQ-IO-COMMAND = TFCMD-READ-RECORD-FOR-UPDATE
           OR   WADUP-SEQ-IO-COMMAND = TFCMD-READNEXT-RECORD)
                MOVE RADUP-SEQ-REC-INFO    TO WADUP-LINK-RECORD
           END-IF.
 
           GOBACK.
 
       0000-MAINLINE-X.
           EXIT.
      /
      *****************************************************************
      * PROCESSING COPYBOOKS                                          *
      *****************************************************************
 
       COPY ACPIADUP.
      /
       COPY XCPL0030.
 
      *****************************************************************
      **                 END OF PROGRAM ASRQADUP                     **
      *****************************************************************
