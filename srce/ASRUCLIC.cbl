      *************************
       IDENTIFICATION DIVISION.
      *************************
 
       PROGRAM-ID.  ASRUCLIC.
 
       COPY XCWWCRHT.
 
      *****************************************************************
      **  MEMBER :  ASRUCLIC                                         **
      **  REMARKS:  APEX UPLOAD CLIC TABLE PROCESSING                **
      **                                                             **
      **  DOMAIN :  CL                                               **
      **  CLASS  :  FD                                               **
      *****************************************************************
      **  DATE     AUTH.  DESCRIPTION                                **
      **                                                             **
557700**  30SEP97  KLE    APEX UPLOAD 5.5                            **
014590**  15DEC99  60     ARCHITECTURAL CHANGES                      **
      *****************************************************************
 
      **********************
       ENVIRONMENT DIVISION.
      **********************
 
      ***************
       DATA DIVISION.
      ***************
 
       WORKING-STORAGE SECTION.
 
       COPY XCWWPGWS REPLACING '$VAR1' BY 'ASRUCLIC'.
 
       01  WS-WORK-AREAS.
           05  WS-PHONE-NUMBER-IN.
               10  WS-PNUM-IN-AREA          PIC X(03).
               10  WS-PNUM-IN-EXCHANGE      PIC X(03).
               10  WS-PNUM-IN-LAST4         PIC X(04).
               10  WS-PNUM-IN-EXTENSION     PIC X(04).
           05  WS-PHONE-NUMBER-OUT.
               10  WS-PNUM-OUT-AREA         PIC X(03).
               10  FILLER                   PIC X(01) VALUE '-'.
               10  WS-PNUM-OUT-EXCHANGE     PIC X(03).
               10  FILLER                   PIC X(01) VALUE '-'.
               10  WS-PNUM-OUT-LAST4        PIC X(04).
               10  FILLER                   PIC X(01) VALUE SPACES.
               10  WS-PNUM-OUT-EXTENSION    PIC X(04).
 
       COPY SQLCA.
 
014590*COPY XCWL0030.
      /
      *****************************************************************
      *  COMMON COPYBOOKS
      *****************************************************************
       COPY ACWWAPUP.
      /
      *****************************************************************
      *  CALLED MODULE PARAMETER INFORMATION
      *****************************************************************
      /
      *****************
       LINKAGE SECTION.
      *****************
       01  WGLOB-GLOBAL-AREA.
       COPY XCWWGLOB.
      /
       COPY ACWLAPUP.
       COPY ACFRUFLD.
       COPY CCFRCLIC.
      /
       PROCEDURE DIVISION  USING  WGLOB-GLOBAL-AREA
                                  LAPUP-PARM-AREA
                                  RUFLD-REC-INFO
                                  RCLIC-REC-INFO.
 
      *--------------
       0000-MAINLINE.
      *--------------
 
           PERFORM  1000-INITIALIZE
               THRU 1000-INITIALIZE-X.
 
           EVALUATE TRUE
 
               WHEN RUFLD-UPLD-FLD-TYP-MIX-CASE
               WHEN RUFLD-UPLD-FLD-TYP-UPPER-CASE
                    PERFORM  1000-PROCESS-CHAR-FIELD
                        THRU 1000-PROCESS-CHAR-FIELD-X
 
               WHEN RUFLD-UPLD-FLD-TYP-COMPLEX
                    PERFORM  2000-PROCESS-COMPLEX-FIELD
                        THRU 2000-PROCESS-COMPLEX-FIELD-X
 
               WHEN RUFLD-UPLD-FLD-TYP-NUMERIC
               WHEN RUFLD-UPLD-FLD-TYP-TRANS-NAME
               WHEN RUFLD-UPLD-FLD-TYP-DATE
               WHEN RUFLD-UPLD-FLD-TYP-DOLLAR-AMT
               WHEN RUFLD-UPLD-FLD-TYP-TRANS-VALU
                    SET  LAPUP-UNKNOWN-FIELD-ERR
                                            TO TRUE
 
               WHEN RUFLD-UPLD-FLD-TYP-UNUSED
                    CONTINUE
 
               WHEN OTHER
                    SET  LAPUP-UNKNOWN-FIELD-TYPE
                                            TO TRUE
 
           END-EVALUATE.
 
       0000-MAINLINE-X.
           GOBACK.
      /
      *----------------
       1000-INITIALIZE.
      *----------------
 
           MOVE WAPUP-C-GOOD-RETURN-CD      TO LAPUP-SUB-RETURN-CD.
           SET  LAPUP-GOOD-RETURN           TO TRUE.
           SET  LAPUP-REC-NOT-CHANGED       TO TRUE.
 
       1000-INITIALIZE-X.
           EXIT.
      /
      *------------------------
       1000-PROCESS-CHAR-FIELD.
      *------------------------
 
           IF  RUFLD-UPLD-FLD-NM = 'CLI_CNTCT_ID_TXT'
               MOVE LAPUP-INPUT-DATA         TO WS-PHONE-NUMBER-IN
               MOVE WS-PNUM-IN-AREA          TO WS-PNUM-OUT-AREA
               MOVE WS-PNUM-IN-EXCHANGE      TO WS-PNUM-OUT-EXCHANGE
               MOVE WS-PNUM-IN-LAST4         TO WS-PNUM-OUT-LAST4
               IF  WS-PNUM-IN-EXTENSION = SPACES
                   MOVE SPACES               TO WS-PNUM-OUT-EXTENSION
               ELSE
                   MOVE WS-PNUM-IN-EXTENSION TO WS-PNUM-OUT-EXTENSION
               END-IF
               MOVE WS-PHONE-NUMBER-OUT      TO RCLIC-CLI-CNTCT-ID-TXT
               GO TO 1000-PROCESS-CHAR-FIELD-X
           END-IF.
 
           MOVE WAPUP-C-UNKNOWN-FIELD-ERR    TO LAPUP-RETURN-CD.
           MOVE SPACES                       TO RCLIC-CLI-CNTCT-ID-CD.
 
       1000-PROCESS-CHAR-FIELD-X.
           EXIT.
      /
      *---------------------------
       2000-PROCESS-COMPLEX-FIELD.
      *---------------------------
 
           IF  RUFLD-UPLD-FLD-NM = 'CLI_CNTCT_ID_TXT'
               MOVE LAPUP-INPUT-DATA         TO WS-PHONE-NUMBER-IN
               MOVE WS-PNUM-IN-AREA          TO WS-PNUM-OUT-AREA
               MOVE WS-PNUM-IN-EXCHANGE      TO WS-PNUM-OUT-EXCHANGE
               MOVE WS-PNUM-IN-LAST4         TO WS-PNUM-OUT-LAST4
               IF  WS-PNUM-IN-EXTENSION = SPACES
                   MOVE SPACES               TO WS-PNUM-OUT-EXTENSION
               ELSE
                   MOVE WS-PNUM-IN-EXTENSION TO WS-PNUM-OUT-EXTENSION
               END-IF
               MOVE WS-PHONE-NUMBER-OUT      TO RCLIC-CLI-CNTCT-ID-TXT
               GO TO 2000-PROCESS-COMPLEX-FIELD-X
           END-IF.
 
           MOVE WAPUP-C-UNKNOWN-FIELD-ERR    TO LAPUP-RETURN-CD.
           MOVE SPACES                       TO RCLIC-CLI-CNTCT-ID-CD.
 
       2000-PROCESS-COMPLEX-FIELD-X.
           EXIT.
      /
      *****************************************************************
      *  PROCESSING COPYBOOKS
      *****************************************************************
      /
      *****************************************************************
      *  LINKAGE PROCESSING COPYBOOKS
      *****************************************************************
      /
      *****************************************************************
      *  ERROR HANDLING ROUTINES
      *****************************************************************
       COPY XCPL0030.
 
      *****************************************************************
      **                 END OF PROGRAM ASRUCLIC                     **
      *****************************************************************
