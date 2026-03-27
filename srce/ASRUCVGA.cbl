      *************************
       IDENTIFICATION DIVISION.
      *************************
 
       PROGRAM-ID.  ASRUCVGA.
 
       COPY XCWWCRHT.
 
      *****************************************************************
      **  MEMBER :  ASRUCVGA                                         **
      **  REMARKS:  APEX UPLOAD CVGA TABLE PROCESSING                **
      **                                                             **
      **  DOMAIN :  CV                                               **
      **  CLASS  :  FD                                               **
      *****************************************************************
      **  DATE     AUTH.  DESCRIPTION                                **
      **                                                             **
      **  31JAN94  APEX   NBS/APEX REDESIGN                          **
APEX52**  30NOV94  JJS    UPGRADE TO RELEASE 5.2                     **
APEX53**  30NOV95  JJS    UPGRADE TO INGENIUM 5.3 & WINAPEX 1.0,     **
APEX53**                  NAME CHANGES NOT TAGGED,                   **
APEX53**                  ADD WORKING STORAGE COPYBOOK XCWWPGWS,     **
APEX53**                  CHANGES TO SUPPORT I/O PROGRAMS            **
APEX54**  31DEC96  TJS    MODIFICATIONS FOR MAINTAINABILITY          **
557700**  30SEP97  TJS    APEX UPLOAD 5.5                            **
014590**  15DEC99  60     ARCHITECTURAL CHANGES                      **
      *****************************************************************
 
       ENVIRONMENT DIVISION.
 
       DATA DIVISION.
 
       WORKING-STORAGE SECTION.
 
APEX53 COPY XCWWPGWS REPLACING '$VAR1' BY 'ASRUCVGA'.
 
       COPY SQLCA.
 
014590*COPY XCWL0030.
 
       01  WS-WORK-AREA.
           05  WS-UPLD-FLD-APEX-NM.
               10  FILLER                        PIC X(04).
               10  WS-CODE-AGNT-NO               PIC 9(01).
               10  FILLER                        PIC X(15).
           05  WS-UPLD-FLD-APEX-NM-R             REDEFINES
               WS-UPLD-FLD-APEX-NM.
               10  FILLER                        PIC X(07).
               10  WS-PCT-AGNT-NO                PIC 9(01).
               10  FILLER                        PIC X(12).
      /
       COPY ACWWAPUP.
      /
       COPY XCWTFCMD.
      /
       COPY XCWWWKDT.
      /
       COPY ACFWUTTB.
       COPY ACFRUTTB.
      /
       COPY XCWL0280.
      /
      *****************
       LINKAGE SECTION.
      *****************
 
       01  WGLOB-GLOBAL-AREA.
       COPY XCWWGLOB.
 
       COPY ACWLAPUP.
 
       COPY ACFRUFLD.
 
       COPY CCFRCVGA.
      /
       PROCEDURE DIVISION  USING  WGLOB-GLOBAL-AREA
                                  LAPUP-PARM-AREA
                                  RUFLD-REC-INFO
                                  RCVGA-REC-INFO.
 
      *--------------
       0000-MAINLINE.
      *--------------
 
           PERFORM  1000-INITIALIZE
               THRU 1000-INITIALIZE-X.
 
APEX54     EVALUATE TRUE
 
557700*        WHEN RUFLD-UPLD-FLD-TYP-CHAR
557700         WHEN RUFLD-UPLD-FLD-TYP-MIX-CASE
557700         WHEN RUFLD-UPLD-FLD-TYP-UPPER-CASE
                    PERFORM  2000-PROCESS-CHAR-FIELD
                        THRU 2000-PROCESS-CHAR-FIELD-X
 
APEX54         WHEN RUFLD-UPLD-FLD-TYP-DATE
                    PERFORM  3000-PROCESS-DATE-FIELD
                        THRU 3000-PROCESS-DATE-FIELD-X
 
APEX54         WHEN RUFLD-UPLD-FLD-TYP-NUMERIC
                    PERFORM  4000-PROCESS-NUMERIC-FIELD
                        THRU 4000-PROCESS-NUMERIC-FIELD-X
 
APEX54         WHEN RUFLD-UPLD-FLD-TYP-DOLLAR-AMT
APEX54              PERFORM  4000-PROCESS-NUMERIC-FIELD
APEX54                  THRU 4000-PROCESS-NUMERIC-FIELD-X
 
557700*        WHEN RUFLD-UPLD-FLD-TYP-TRANS
557700         WHEN RUFLD-UPLD-FLD-TYP-TRANS-VALU
                    PERFORM  5000-PROCESS-TRANS-FIELD
                        THRU 5000-PROCESS-TRANS-FIELD-X
 
APEX54         WHEN RUFLD-UPLD-FLD-TYP-COMPLEX
                    PERFORM  6000-PROCESS-COMPLEX-FIELD
                        THRU 6000-PROCESS-COMPLEX-FIELD-X
 
557700*        WHEN RUFLD-UPLD-FLD-TYP-FIELD
557700         WHEN RUFLD-UPLD-FLD-TYP-TRANS-NAME
                    PERFORM  7000-PROCESS-FIELD-FIELD
                        THRU 7000-PROCESS-FIELD-FIELD-X
 
APEX54         WHEN RUFLD-UPLD-FLD-TYP-UNUSED
APEX54             CONTINUE
 
APEX54         WHEN OTHER
                   MOVE WAPUP-C-UNKNOWN-FIELD-TYPE TO LAPUP-RETURN-CD
 
APEX54     END-EVALUATE.
 
           GOBACK.
 
       0000-MAINLINE-X.
           EXIT.
      /
      *----------------
       1000-INITIALIZE.
      *----------------
 
           MOVE WAPUP-C-GOOD-RETURN-CD        TO LAPUP-RETURN-CD.
           MOVE WAPUP-C-GOOD-RETURN-CD        TO LAPUP-SUB-RETURN-CD.
           MOVE WAPUP-C-NO                    TO LAPUP-REC-CHANGED-SW.
 
       1000-INITIALIZE-X.
           EXIT.
      /
      *------------------------
       2000-PROCESS-CHAR-FIELD.
      *------------------------
 
           MOVE WAPUP-C-UNKNOWN-FIELD-ERR     TO LAPUP-RETURN-CD.
 
       2000-PROCESS-CHAR-FIELD-X.
           EXIT.
      /
      *------------------------
       3000-PROCESS-DATE-FIELD.
      *------------------------
 
           MOVE WAPUP-C-UNKNOWN-FIELD-ERR     TO LAPUP-RETURN-CD.
 
       3000-PROCESS-DATE-FIELD-X.
           EXIT.
      /
      *---------------------------
       4000-PROCESS-NUMERIC-FIELD.
      *---------------------------
 
           MOVE WAPUP-C-UNKNOWN-FIELD-ERR     TO LAPUP-RETURN-CD.
 
       4000-PROCESS-NUMERIC-FIELD-X.
           EXIT.
      /
      *-------------------------
       5000-PROCESS-TRANS-FIELD.
      *-------------------------
 
           MOVE WAPUP-C-UNKNOWN-FIELD-ERR     TO LAPUP-RETURN-CD.
 
       5000-PROCESS-TRANS-FIELD-X.
           EXIT.
      /
      *---------------------------
       6000-PROCESS-COMPLEX-FIELD.
      *---------------------------
 
           IF  RUFLD-UPLD-FLD-NM = 'AGT_ID'
               MOVE RUFLD-UPLD-FLD-APEX-NM    TO WS-UPLD-FLD-APEX-NM
               MOVE LAPUP-INPUT-DATA          TO
                    LAPUP-CVGA-AGT-ID (WS-CODE-AGNT-NO)
               MOVE WAPUP-C-YES               TO LAPUP-REC-CHANGED-SW
               GO TO 6000-PROCESS-COMPLEX-FIELD-X
           END-IF.
 
           IF  RUFLD-UPLD-FLD-NM = 'CVG_AGT_SHR_PCT'
               MOVE 3                         TO L0280-LENGTH
               MOVE 2                         TO L0280-PRECISION
               MOVE LAPUP-INPUT-DATA          TO L0280-INPUT-DATA
               PERFORM  0280-1000-NUMERIC-EDIT
                   THRU 0280-1000-NUMERIC-EDIT-X
               IF  L0280-OK
                   MOVE RUFLD-UPLD-FLD-APEX-NM TO WS-UPLD-FLD-APEX-NM
                   COMPUTE LAPUP-CVGA-CVG-AGT-SHR-PCT (WS-PCT-AGNT-NO)
                       = L0280-OUTPUT / (10 ** L0280-PRECISION)
                   MOVE WAPUP-C-YES           TO LAPUP-REC-CHANGED-SW
                   GO TO 6000-PROCESS-COMPLEX-FIELD-X
               ELSE
                   MOVE WAPUP-C-NUM-CONV-ERR  TO LAPUP-RETURN-CD
                   MOVE L0280-STATUS          TO LAPUP-SUB-RETURN-CD
                   GO TO 6000-PROCESS-COMPLEX-FIELD-X
               END-IF
           END-IF.
 
           MOVE WAPUP-C-UNKNOWN-FIELD-ERR     TO LAPUP-RETURN-CD.
 
       6000-PROCESS-COMPLEX-FIELD-X.
           EXIT.
      /
      *-------------------------
       7000-PROCESS-FIELD-FIELD.
      *-------------------------
 
APEX54     IF  LAPUP-INPUT-DATA = '1'
APEX54         MOVE WAPUP-C-UNKNOWN-FIELD-ERR TO LAPUP-RETURN-CD
APEX54     END-IF.
 
       7000-PROCESS-FIELD-FIELD-X.
           EXIT.
      /
      *****************************************************************
      *   LINKAGE PROCESSING COPYBOOKS                                *
      *****************************************************************
       COPY XCPL0280.
      /
       COPY ACPNUTTB.
      /
       COPY XCPL0030.
 
      *****************************************************************
      **                 END OF PROGRAM ASRUCVGA                     **
      *****************************************************************
