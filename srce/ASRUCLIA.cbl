      *************************
       IDENTIFICATION DIVISION.
      *************************
 
       PROGRAM-ID.  ASRUCLIA.
 
       COPY XCWWCRHT.
 
      *****************************************************************
      **  MEMBER :  ASRUCLIA                                         **
      **  REMARKS:  APEX UPLOAD CLIA TABLE PROCESSING                **
      **                                                             **
      **  DOMAIN :  CL                                               **
      **  CLASS  :  FD                                               **
      *****************************************************************
      **  DATE     AUTH.  DESCRIPTION                                **
      **                                                             **
APEX54**  31DEC96  TJS    MODIFICATIONS FOR MAINTAINABILITY          **
557700**  30SEP97  TJS    APEX UPLOAD 5.5                            **
014590**  15DEC99  60     ARCHITECTURAL CHANGES                      **
      *****************************************************************
 
      **********************
       ENVIRONMENT DIVISION.
      **********************
 
      ***************
       DATA DIVISION.
      ***************
 
       WORKING-STORAGE SECTION.
 
       COPY XCWWPGWS REPLACING '$VAR1' BY 'ASRUCLIA'.
 
       COPY SQLCA.
 
014590*COPY XCWL0030.
 
       01  WS-WORK-AREAS.
           05  WS-POSTAL-CODE-IN.
               10  WS-POSTAL-CODE-IN-FIRST3  PIC X(03).
               10  WS-POSTAL-CODE-IN-LAST3   PIC X(03).
           05  WS-POSTAL-CODE-OUT.
               10  WS-POSTAL-CODE-OUT-FIRST3 PIC X(03).
               10  FILLER                    PIC X(01) VALUE SPACE.
               10  WS-POSTAL-CODE-OUT-LAST3  PIC X(03).
      /
      *****************************************************************
      *  COMMON COPYBOOKS
      *****************************************************************
       COPY ACWWAPUP.
      /
      *****************************************************************
      *  I/O COPYBOOKS
      *****************************************************************
       COPY ACFRUTTB.
       COPY ACFWUTTB.
      /
      *****************************************************************
      *  CALLED MODULE PARAMETER INFORMATION
      *****************************************************************
       COPY XCWL0280.
      /
      *****************
       LINKAGE SECTION.
      *****************
       01  WGLOB-GLOBAL-AREA.
       COPY XCWWGLOB.
      /
       COPY ACWLAPUP.
       COPY ACFRUFLD.
       COPY CCFRCLIA.
      /
       PROCEDURE DIVISION  USING  WGLOB-GLOBAL-AREA
                                  LAPUP-PARM-AREA
                                  RUFLD-REC-INFO
                                  RCLIA-REC-INFO.
 
      *--------------
       0000-MAINLINE.
      *--------------
 
           PERFORM  1000-INITIALIZE
               THRU 1000-INITIALIZE-X.
 
           EVALUATE TRUE
 
557700*        WHEN RUFLD-UPLD-FLD-TYP-CHAR
557700         WHEN RUFLD-UPLD-FLD-TYP-MIX-CASE
557700         WHEN RUFLD-UPLD-FLD-TYP-UPPER-CASE
                    PERFORM  2000-PROCESS-CHAR-FIELD
                        THRU 2000-PROCESS-CHAR-FIELD-X
 
               WHEN RUFLD-UPLD-FLD-TYP-DATE
                    PERFORM  3000-PROCESS-DATE-FIELD
                        THRU 3000-PROCESS-DATE-FIELD-X
 
               WHEN RUFLD-UPLD-FLD-TYP-NUMERIC
               WHEN RUFLD-UPLD-FLD-TYP-DOLLAR-AMT
                    PERFORM  4000-PROCESS-NUMERIC-FIELD
                        THRU 4000-PROCESS-NUMERIC-FIELD-X
 
557700*        WHEN RUFLD-UPLD-FLD-TYP-TRANS
557700         WHEN RUFLD-UPLD-FLD-TYP-TRANS-VALU
                    PERFORM  5000-PROCESS-TRANS-FIELD
                        THRU 5000-PROCESS-TRANS-FIELD-X
 
               WHEN RUFLD-UPLD-FLD-TYP-COMPLEX
                    PERFORM  6000-PROCESS-COMPLEX-FIELD
                        THRU 6000-PROCESS-COMPLEX-FIELD-X
 
557700*        WHEN RUFLD-UPLD-FLD-TYP-FIELD
557700         WHEN RUFLD-UPLD-FLD-TYP-TRANS-NAME
                    PERFORM  7000-PROCESS-FIELD-FIELD
                        THRU 7000-PROCESS-FIELD-FIELD-X
 
               WHEN RUFLD-UPLD-FLD-TYP-UNUSED
                    CONTINUE
 
               WHEN OTHER
                    MOVE WAPUP-C-UNKNOWN-FIELD-TYPE
                                            TO LAPUP-RETURN-CD
 
           END-EVALUATE.
 
       0000-MAINLINE-X.
           GOBACK.
      /
      *----------------
       1000-INITIALIZE.
      *----------------
 
           MOVE WAPUP-C-GOOD-RETURN-CD      TO LAPUP-RETURN-CD.
           MOVE WAPUP-C-GOOD-RETURN-CD      TO LAPUP-SUB-RETURN-CD.
           MOVE WAPUP-C-NO                  TO LAPUP-REC-CHANGED-SW.
 
       1000-INITIALIZE-X.
           EXIT.
      /
      *------------------------
       2000-PROCESS-CHAR-FIELD.
      *------------------------
 
           IF  RUFLD-UPLD-FLD-NM = 'ADDR_EFF_DT_NUM'
               MOVE LAPUP-INPUT-DATA        TO RCLIA-ADDR-EFF-DT-NUM
               MOVE WAPUP-C-YES             TO LAPUP-REC-CHANGED-SW
               GO TO 2000-PROCESS-CHAR-FIELD-X
           END-IF.
 
           IF  RUFLD-UPLD-FLD-NM = 'ADDR_END_DT_NUM'
               MOVE LAPUP-INPUT-DATA        TO RCLIA-ADDR-END-DT-NUM
               MOVE WAPUP-C-YES             TO LAPUP-REC-CHANGED-SW
               GO TO 2000-PROCESS-CHAR-FIELD-X
           END-IF.
 
           IF  RUFLD-UPLD-FLD-NM = 'CLI_ADDR_LN_1_TXT'
               MOVE LAPUP-INPUT-DATA        TO RCLIA-CLI-ADDR-LN-1-TXT
               MOVE WAPUP-C-YES             TO LAPUP-REC-CHANGED-SW
               GO TO 2000-PROCESS-CHAR-FIELD-X
           END-IF.
 
           IF  RUFLD-UPLD-FLD-NM = 'CLI_ADDR_LN_2_TXT'
               MOVE LAPUP-INPUT-DATA        TO RCLIA-CLI-ADDR-LN-2-TXT
               MOVE WAPUP-C-YES             TO LAPUP-REC-CHANGED-SW
               GO TO 2000-PROCESS-CHAR-FIELD-X
           END-IF.
 
           IF  RUFLD-UPLD-FLD-NM = 'CLI_ADDR_LN_3_TXT'
               MOVE LAPUP-INPUT-DATA        TO RCLIA-CLI-ADDR-LN-3-TXT
               MOVE WAPUP-C-YES             TO LAPUP-REC-CHANGED-SW
               GO TO 2000-PROCESS-CHAR-FIELD-X
           END-IF.
 
           IF  RUFLD-UPLD-FLD-NM = 'CLI_CITY_NM_TXT'
               MOVE LAPUP-INPUT-DATA        TO RCLIA-CLI-CITY-NM-TXT
               MOVE WAPUP-C-YES             TO LAPUP-REC-CHANGED-SW
               GO TO 2000-PROCESS-CHAR-FIELD-X
           END-IF.
 
           IF  RUFLD-UPLD-FLD-NM = 'CLI_CTRY_CD'
               MOVE LAPUP-INPUT-DATA        TO RCLIA-CLI-CTRY-CD
               MOVE WAPUP-C-YES             TO LAPUP-REC-CHANGED-SW
               GO TO 2000-PROCESS-CHAR-FIELD-X
           END-IF.
 
           IF  RUFLD-UPLD-FLD-NM = 'CLI_ID'
               MOVE LAPUP-INPUT-DATA        TO RCLIA-CLI-ID
               MOVE WAPUP-C-YES             TO LAPUP-REC-CHANGED-SW
               GO TO 2000-PROCESS-CHAR-FIELD-X
           END-IF.
 
           IF  RUFLD-UPLD-FLD-NM = 'CLI_PSTL_CD'
               MOVE LAPUP-INPUT-DATA        TO RCLIA-CLI-PSTL-CD
               MOVE WAPUP-C-YES             TO LAPUP-REC-CHANGED-SW
               GO TO 2000-PROCESS-CHAR-FIELD-X
           END-IF.
 
           IF  RUFLD-UPLD-FLD-NM = 'CLI_RES_NUM'
               MOVE LAPUP-INPUT-DATA        TO RCLIA-CLI-RES-NUM
               MOVE 'A'                     TO RCLIA-CLI-RES-TYP-CD
               MOVE WAPUP-C-YES             TO LAPUP-REC-CHANGED-SW
               GO TO 2000-PROCESS-CHAR-FIELD-X
           END-IF.
 
           MOVE WAPUP-C-UNKNOWN-FIELD-ERR    TO LAPUP-RETURN-CD.
 
       2000-PROCESS-CHAR-FIELD-X.
           EXIT.
      /
      *------------------------
       3000-PROCESS-DATE-FIELD.
      *------------------------
 
           IF  RUFLD-UPLD-FLD-NM = 'ADDR_STAT_CHNG_DT'
               MOVE LAPUP-INPUT-DT-YR       TO WAPUP-CONV-DT-YR
               MOVE LAPUP-INPUT-DT-MON      TO WAPUP-CONV-DT-MON
               MOVE LAPUP-INPUT-DT-DAY      TO WAPUP-CONV-DT-DAY
               MOVE WAPUP-CONV-DT           TO RCLIA-ADDR-STAT-CHNG-DT
               MOVE WAPUP-C-YES             TO LAPUP-REC-CHANGED-SW
               GO TO 3000-PROCESS-DATE-FIELD-X
           END-IF.
 
           IF  RUFLD-UPLD-FLD-NM = 'CLI_ADDR_CHNG_DT'
               MOVE LAPUP-INPUT-DT-YR       TO WAPUP-CONV-DT-YR
               MOVE LAPUP-INPUT-DT-MON      TO WAPUP-CONV-DT-MON
               MOVE LAPUP-INPUT-DT-DAY      TO WAPUP-CONV-DT-DAY
               MOVE WAPUP-CONV-DT           TO RCLIA-CLI-ADDR-CHNG-DT
               MOVE WAPUP-C-YES             TO LAPUP-REC-CHANGED-SW
               GO TO 3000-PROCESS-DATE-FIELD-X
           END-IF.
 
           MOVE WAPUP-C-UNKNOWN-FIELD-ERR   TO LAPUP-RETURN-CD.
 
       3000-PROCESS-DATE-FIELD-X.
           EXIT.
      /
      *---------------------------
       4000-PROCESS-NUMERIC-FIELD.
      *---------------------------
 
           MOVE WAPUP-C-UNKNOWN-FIELD-ERR   TO LAPUP-RETURN-CD.
 
       4000-PROCESS-NUMERIC-FIELD-X.
           EXIT.
      /
      *-------------------------
       5000-PROCESS-TRANS-FIELD.
      *-------------------------
 
           IF  RUFLD-UPLD-FLD-NM = 'CLI_CRNT_LOC_CD'
               MOVE RUFLD-UPLD-TTBL-TYP-ID  TO WUTTB-UPLD-TTBL-TYP-ID
               MOVE LAPUP-INPUT-DATA        TO WUTTB-UPLD-TTBL-VALU-ID
               PERFORM  UTTB-1000-LOOKUP-UTTB
                   THRU UTTB-1000-LOOKUP-UTTB-X
 
               IF  WUTTB-IO-OK
                   MOVE RUTTB-UPLD-TTBL-VALU-TXT
                                            TO RCLIA-CLI-CRNT-LOC-CD
                   MOVE WAPUP-C-YES         TO LAPUP-REC-CHANGED-SW
                   GO TO 5000-PROCESS-TRANS-FIELD-X
               ELSE
                   MOVE WAPUP-C-TRAN-CONV-ERR
                                            TO LAPUP-RETURN-CD
                   GO TO 5000-PROCESS-TRANS-FIELD-X
               END-IF
           END-IF.
 
           IF  RUFLD-UPLD-FLD-NM = 'CLI_CTRY_CD'
               MOVE RUFLD-UPLD-TTBL-TYP-ID  TO WUTTB-UPLD-TTBL-TYP-ID
               MOVE LAPUP-INPUT-DATA        TO WUTTB-UPLD-TTBL-VALU-ID
               PERFORM  UTTB-1000-LOOKUP-UTTB
                   THRU UTTB-1000-LOOKUP-UTTB-X
 
               IF  WUTTB-IO-OK
                   MOVE RUTTB-UPLD-TTBL-VALU-TXT
                                            TO RCLIA-CLI-CTRY-CD
                   MOVE WAPUP-C-YES         TO LAPUP-REC-CHANGED-SW
                   GO TO 5000-PROCESS-TRANS-FIELD-X
               ELSE
                   MOVE WAPUP-C-TRAN-CONV-ERR
                                            TO LAPUP-RETURN-CD
                   GO TO 5000-PROCESS-TRANS-FIELD-X
               END-IF
           END-IF.
 
           MOVE WAPUP-C-UNKNOWN-FIELD-ERR   TO LAPUP-RETURN-CD.
 
       5000-PROCESS-TRANS-FIELD-X.
           EXIT.
      /
      *---------------------------
       6000-PROCESS-COMPLEX-FIELD.
      *---------------------------
 
           IF  RUFLD-UPLD-FLD-NM = 'CLI_ADDR_YR_DUR'
               MOVE 3                      TO L0280-LENGTH
               MOVE 0                      TO L0280-PRECISION
               MOVE LAPUP-INPUT-DATA       TO L0280-INPUT-DATA
               PERFORM  0280-1000-NUMERIC-EDIT
                   THRU 0280-1000-NUMERIC-EDIT-X
 
               IF  L0280-OK
                   COMPUTE RCLIA-CLI-ADDR-YR-DUR = L0280-OUTPUT / 12
                   MOVE WAPUP-C-YES        TO LAPUP-REC-CHANGED-SW
                   GO TO 6000-PROCESS-COMPLEX-FIELD-X
               ELSE
                   MOVE WAPUP-C-NUM-CONV-ERR
                                           TO LAPUP-RETURN-CD
                   MOVE L0280-STATUS       TO LAPUP-SUB-RETURN-CD
                   GO TO 6000-PROCESS-COMPLEX-FIELD-X
               END-IF
           END-IF.
 
           IF  RUFLD-UPLD-FLD-NM = 'CLI_PSTL_CD'
               MOVE LAPUP-INPUT-DATA       TO WS-POSTAL-CODE-IN
               MOVE WS-POSTAL-CODE-IN-FIRST3
                                           TO WS-POSTAL-CODE-OUT-FIRST3
               MOVE WS-POSTAL-CODE-IN-LAST3
                                           TO WS-POSTAL-CODE-OUT-LAST3
               MOVE WS-POSTAL-CODE-OUT     TO RCLIA-CLI-PSTL-CD
               MOVE WAPUP-C-YES            TO LAPUP-REC-CHANGED-SW
               GO TO 6000-PROCESS-COMPLEX-FIELD-X
           END-IF.
 
           MOVE WAPUP-C-UNKNOWN-FIELD-ERR  TO LAPUP-RETURN-CD.
 
       6000-PROCESS-COMPLEX-FIELD-X.
           EXIT.
      /
      *-------------------------
       7000-PROCESS-FIELD-FIELD.
      *-------------------------
 
           IF  LAPUP-INPUT-DATA = '1'
               MOVE WAPUP-C-UNKNOWN-FIELD-ERR
                                            TO LAPUP-RETURN-CD
           END-IF.
 
 
       7000-PROCESS-FIELD-FIELD-X.
           EXIT.
      /
      *****************************************************************
      *  PROCESSING COPYBOOKS
      *****************************************************************
       COPY ACPPUTTB.
      /
      *****************************************************************
      *  LINKAGE PROCESSING COPYBOOKS
      *****************************************************************
       COPY XCPL0280.
      /
      *****************************************************************
      *  FILE I/O PROCESS MODULES
      *****************************************************************
       COPY ACPNUTTB.
      /
      *****************************************************************
      *  ERROR HANDLING ROUTINES
      *****************************************************************
       COPY XCPL0030.
 
      *****************************************************************
      **                 END OF PROGRAM ASRUCLIA                     **
      *****************************************************************
