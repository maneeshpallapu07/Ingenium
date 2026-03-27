      *************************
       IDENTIFICATION DIVISION.
      *************************
 
       PROGRAM-ID.  ASRUCLIO.
 
       COPY XCWWCRHT.
 
      *****************************************************************
      **  MEMBER :  ASRUCLIO                                         **
      **  REMARKS:  APEX UPLOAD CLIO TABLE PROCESSING                **
      **                                                             **
      **  DOMAIN :  CL                                               **
      **  CLASS  :  FD                                               **
      *****************************************************************
      **  DATE     AUTH.  DESCRIPTION                                **
      **                                                             **
APEX52**  30NOV94  JJS    APEX UPLOAD UPGRADE TO RELEASE 5.2         **
APEX53**  30NOV95  JJS    UPGRADE TO RELEASE 5.3, WINAPEX 1.0,       **
APEX53**                  NAME CHANGES NOT TAGGED,                   **
APEX53**                  ADD WORKING STORAGE COPYBOOK XCWWPGWS,     **
APEX53**                  CHANGES TO SUPPORT I/O PROGRAMS            **
APEX54**  31DEC96  TJS    MODIFICATIONS FOR MAINTAINABILITY          **
557700**  30SEP97  TJS    APEX UPLOAD 5.5                            **
014590**  15DEC99  60     ARCHITECTURAL CHANGES                      **
015543**  15DEC99  60     CODE CLEANUP                               **
      *****************************************************************
      /
      **********************
       ENVIRONMENT DIVISION.
      **********************
 
      ***************
       DATA DIVISION.
      ***************
 
      *
       WORKING-STORAGE SECTION.
      *
APEX53 COPY XCWWPGWS REPLACING '$VAR1' BY 'ASRUCLIO'.
 
       COPY SQLCA.
 
014590*COPY XCWL0030.
 
       01  WS-CONSTANTS.
           05  WS-C-FLD-APEX-NM-LEN         PIC S9(04) COMP VALUE +20.
           05  WS-C-MAX-INFC-PEND           PIC S9(04) COMP VALUE +6.
 
       01  WS-COUNTERS.
           05  WS-SUB                       PIC S9(04) COMP.
 
       01  WS-SWITCHES.
           05  WS-BYPASS-FIELD-SW           PIC X(01).
               88  WS-BYPASS-FIELD          VALUE 'Y'.
 
       01  WS-WORK-AREA.
           05  WS-UPLD-FLD-STRUCT-NM        PIC X(20).
               88  WS-INFC-PEND-STRUCT      VALUE
                                            'PEND                '
                                            'PENDH               '
                                            'PENDJUV             '
                                            'OTHERAPPSSUBJUV     '
                                            'OTHERAPPSSUB        '
                                            'INFORCEH            '.
               88  WS-INFC-PEND-DI-STRUCT   VALUE
                                            'PENDH               '
                                            'INFORCEH            '.
               88  WS-PEND-NC-STRUCTURE     VALUE
                                            'PEND1NC             '
                                            'PEND2NC             '
                                            'PEND3NC             '
                                            'PEND4NC             '
                                            'PEND5NC             '.
               88  WS-PEND-JUV-NC-STRUCTURE VALUE
                                            'PENDJ1NC            '
                                            'PENDJ2NC            '
                                            'PENDJ3NC            '
                                            'PENDJ4NC            '
                                            'PENDJ5NC            '.
               88  WS-OAS-NC-STRUCTURE      VALUE
                                            'OAS1NC              '
                                            'OAS2NC              '
                                            'OAS3NC              '
                                            'OAS4NC              '
                                            'OAS5NC              '.
               88  WS-OAS-JUV-NC-STRUCTURE  VALUE
                                            'OASJ1NC             '
                                            'OASJ2NC             '
                                            'OASJ3NC             '
                                            'OASJ4NC             '
                                            'OASJ5NC             '.
               88  WS-BYPASS-FOR-JUVENILE   VALUE
                                            'PEND                '
                                            'PENDH               '
                                            'OTHERAPPSSUB        '
                                            'INFORCEH            '
                                            'PEND1NC             '
                                            'PEND2NC             '
                                            'PEND3NC             '
                                            'PEND4NC             '
                                            'PEND5NC             '
                                            'OAS1NC              '
                                            'OAS2NC              '
                                            'OAS3NC              '
                                            'OAS4NC              '
                                            'OAS5NC              '.
           05  WS-UPLD-FLD-STRUCT-NM-CHAR   REDEFINES
               WS-UPLD-FLD-STRUCT-NM        PIC X(01) OCCURS 20 TIMES.
           05  WS-FLD-APEX-NM               PIC X(20).
           05  WS-FLD-APEX-NM-CHAR          REDEFINES
               WS-FLD-APEX-NM               PIC X(01) OCCURS 20 TIMES.
           05  WS-NEW-INFC-PEND-ID.
               10  WS-NEW-INFC-PEND-TYPE    PIC X(04).
               10  WS-NEW-INFC-PEND-NO      PIC X(01).
      /
       COPY ACWWAPUP.
      /
       COPY XCWL0280.
      /
       COPY XCWTFCMD.
      /
       COPY XCWWWKDT.
      /
       COPY ACFWUTTB.
       COPY ACFRUTTB.
      /
      *****************
       LINKAGE SECTION.
      *****************
 
       01  WGLOB-GLOBAL-AREA.
       COPY XCWWGLOB.
 
       COPY ACWLAPUP.
 
       COPY ACWLCLIO.
 
       COPY ACFRUFLD.
 
APEX54 COPY NCWL5850.
      /
       PROCEDURE DIVISION  USING  WGLOB-GLOBAL-AREA
                                  LAPUP-PARM-AREA
                                  LCLIO-PARM-AREA
                                  RUFLD-REC-INFO
APEX54                            L5850-PARM-INFO.
 
      *--------------
       0000-MAINLINE.
      *--------------
 
           PERFORM  1000-INITIALIZE
               THRU 1000-INITIALIZE-X.
 
           MOVE WAPUP-C-NO                 TO WS-BYPASS-FIELD-SW.
           MOVE RUFLD-UPLD-FLD-STRUCT-NM   TO WS-UPLD-FLD-STRUCT-NM.
 
           IF  WS-INFC-PEND-STRUCT
               PERFORM  8000-CHECK-INFC-PEND
                   THRU 8000-CHECK-INFC-PEND-X
           END-IF.
 
APEX54     IF  WS-BYPASS-FIELD
APEX54         GO TO 0000-MAINLINE-X
APEX54     END-IF.
 
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
APEX54           OR RUFLD-UPLD-FLD-TYP-DOLLAR-AMT
                    PERFORM  4000-PROCESS-NUMERIC-FIELD
                        THRU 4000-PROCESS-NUMERIC-FIELD-X
 
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
                    CONTINUE
 
APEX54         WHEN OTHER
                    MOVE WAPUP-C-UNKNOWN-FIELD-TYPE
APEX54                                     TO LAPUP-RETURN-CD
 
APEX54     END-EVALUATE.
 
       0000-MAINLINE-X.
           GOBACK.
      /
      *----------------
       1000-INITIALIZE.
      *----------------
 
           MOVE WAPUP-C-GOOD-RETURN-CD       TO LAPUP-RETURN-CD.
           MOVE WAPUP-C-GOOD-RETURN-CD       TO LAPUP-SUB-RETURN-CD.
           MOVE WAPUP-C-NO                   TO LAPUP-REC-CHANGED-SW.
 
       1000-INITIALIZE-X.
           EXIT.
      /
      *------------------------
       2000-PROCESS-CHAR-FIELD.
      *------------------------
 
           IF  RUFLD-UPLD-FLD-NM = 'CLI_OINS_CO_NM'
               MOVE LAPUP-INPUT-DATA         TO
APEX54                 L5850-CLI-OINS-CO-NM (WS-SUB)
               MOVE WAPUP-C-YES              TO LAPUP-REC-CHANGED-SW
APEX54         SET L5850-CLIO-DATA-UPDATE (WS-SUB)
APEX54                                       TO TRUE
               GO TO 2000-PROCESS-CHAR-FIELD-X
           END-IF.
 
           IF  RUFLD-UPLD-FLD-NM = 'DI_OINS_BP_DUR'
               MOVE LAPUP-INPUT-DATA         TO
APEX54                 L5850-DI-OINS-BP-DUR (WS-SUB)
               MOVE WAPUP-C-YES              TO LAPUP-REC-CHANGED-SW
APEX54         SET L5850-CLIO-DATA-UPDATE (WS-SUB)
APEX54                                       TO TRUE
               GO TO 2000-PROCESS-CHAR-FIELD-X
           END-IF.
 
           IF  RUFLD-UPLD-FLD-NM = 'DI_OINS_EP_DUR'
               MOVE LAPUP-INPUT-DATA         TO
APEX54                 L5850-DI-OINS-EP-DUR (WS-SUB)
               MOVE WAPUP-C-YES              TO LAPUP-REC-CHANGED-SW
APEX54         SET L5850-CLIO-DATA-UPDATE (WS-SUB)
APEX54                                       TO TRUE
               GO TO 2000-PROCESS-CHAR-FIELD-X
           END-IF.
 
557700*    IF  RUFLD-UPLD-FLD-NM = 'OINS_ISS_YR_QTY'
557700     IF  RUFLD-UPLD-FLD-NM = 'CLI_OINS_ISS_YR'
               MOVE LAPUP-INPUT-DT-YR        TO
557700*                L5850-OINS-ISS-YR-QTY (WS-SUB)
557700                 L5850-CLI-OINS-ISS-YR (WS-SUB)
               MOVE WAPUP-C-YES              TO LAPUP-REC-CHANGED-SW
APEX54         SET L5850-CLIO-DATA-UPDATE (WS-SUB)
APEX54                                       TO TRUE
               GO TO 2000-PROCESS-CHAR-FIELD-X
           END-IF.
 
           MOVE WAPUP-C-UNKNOWN-FIELD-ERR    TO LAPUP-RETURN-CD.
 
       2000-PROCESS-CHAR-FIELD-X.
           EXIT.
      /
      *------------------------
       3000-PROCESS-DATE-FIELD.
      *------------------------
 
           MOVE WAPUP-C-UNKNOWN-FIELD-ERR    TO LAPUP-RETURN-CD.
 
       3000-PROCESS-DATE-FIELD-X.
           EXIT.
      /
      *---------------------------
       4000-PROCESS-NUMERIC-FIELD.
      *---------------------------
 
           IF  RUFLD-UPLD-FLD-NM = 'CLI_OINS_ADB_AMT'
               MOVE 9                        TO L0280-LENGTH
               MOVE 2                        TO L0280-PRECISION
               MOVE LAPUP-INPUT-DATA         TO L0280-INPUT-DATA
               PERFORM  0280-1000-NUMERIC-EDIT
                   THRU 0280-1000-NUMERIC-EDIT-X
               IF  L0280-OK
APEX54             COMPUTE L5850-CLI-OINS-ADB-AMT (WS-SUB) =
                       L0280-OUTPUT / (10 ** L0280-PRECISION)
                   MOVE WAPUP-C-YES          TO LAPUP-REC-CHANGED-SW
APEX54             SET L5850-CLIO-DATA-UPDATE (WS-SUB)
APEX54                                       TO TRUE
                   GO TO 4000-PROCESS-NUMERIC-FIELD-X
               ELSE
                   MOVE WAPUP-C-NUM-CONV-ERR TO LAPUP-RETURN-CD
                   MOVE L0280-STATUS         TO LAPUP-SUB-RETURN-CD
                   GO TO 4000-PROCESS-NUMERIC-FIELD-X
               END-IF
           END-IF.
 
 
           IF  RUFLD-UPLD-FLD-NM = 'CLI_OINS_TOT_AMT'
               MOVE 9                        TO L0280-LENGTH
               MOVE 2                        TO L0280-PRECISION
               MOVE LAPUP-INPUT-DATA         TO L0280-INPUT-DATA
               PERFORM  0280-1000-NUMERIC-EDIT
                   THRU 0280-1000-NUMERIC-EDIT-X
               IF  L0280-OK
APEX54             COMPUTE L5850-CLI-OINS-TOT-AMT (WS-SUB) =
                       L0280-OUTPUT / (10 ** L0280-PRECISION)
                   MOVE WAPUP-C-YES          TO LAPUP-REC-CHANGED-SW
APEX54             SET L5850-CLIO-DATA-UPDATE (WS-SUB)
APEX54                                       TO TRUE
                   GO TO 4000-PROCESS-NUMERIC-FIELD-X
               ELSE
                   MOVE WAPUP-C-NUM-CONV-ERR TO LAPUP-RETURN-CD
                   MOVE L0280-STATUS         TO LAPUP-SUB-RETURN-CD
                   GO TO 4000-PROCESS-NUMERIC-FIELD-X
               END-IF
           END-IF.
 
           MOVE WAPUP-C-UNKNOWN-FIELD-ERR    TO LAPUP-RETURN-CD.
 
       4000-PROCESS-NUMERIC-FIELD-X.
           EXIT.
      /
      *-------------------------
       5000-PROCESS-TRANS-FIELD.
      *-------------------------
 
           IF  RUFLD-UPLD-FLD-NM = 'DI_OINS_TAXBL_CD'
               MOVE RUFLD-UPLD-TTBL-TYP-ID   TO WUTTB-UPLD-TTBL-TYP-ID
               MOVE LAPUP-INPUT-DATA         TO WUTTB-UPLD-TTBL-VALU-ID
               PERFORM  UTTB-1000-LOOKUP-UTTB
                   THRU UTTB-1000-LOOKUP-UTTB-X
               IF  WUTTB-IO-OK
                   MOVE RUTTB-UPLD-TTBL-VALU-TXT TO
APEX54                     L5850-DI-OINS-TAXBL-CD (WS-SUB)
                   MOVE WAPUP-C-YES          TO LAPUP-REC-CHANGED-SW
APEX54             SET L5850-CLIO-DATA-UPDATE (WS-SUB)
APEX54                                       TO TRUE
                   GO TO 5000-PROCESS-TRANS-FIELD-X
               ELSE
                   MOVE WAPUP-C-TRAN-CONV-ERR TO LAPUP-RETURN-CD
                   GO TO 5000-PROCESS-TRANS-FIELD-X
               END-IF
           END-IF.
 
 
           IF  RUFLD-UPLD-FLD-NM = 'OINS_BUS_PERS_CD'
               MOVE RUFLD-UPLD-TTBL-TYP-ID   TO WUTTB-UPLD-TTBL-TYP-ID
               MOVE LAPUP-INPUT-DATA         TO WUTTB-UPLD-TTBL-VALU-ID
               PERFORM  UTTB-1000-LOOKUP-UTTB
                   THRU UTTB-1000-LOOKUP-UTTB-X
               IF  WUTTB-IO-OK
                   MOVE RUTTB-UPLD-TTBL-VALU-TXT TO
APEX54                     L5850-OINS-BUS-PERS-CD (WS-SUB)
                   MOVE WAPUP-C-YES          TO LAPUP-REC-CHANGED-SW
APEX54             SET L5850-CLIO-DATA-UPDATE (WS-SUB)
APEX54                                       TO TRUE
                   GO TO 5000-PROCESS-TRANS-FIELD-X
               ELSE
                   MOVE WAPUP-C-TRAN-CONV-ERR TO LAPUP-RETURN-CD
                   GO TO 5000-PROCESS-TRANS-FIELD-X
               END-IF
           END-IF.
 
           MOVE WAPUP-C-UNKNOWN-FIELD-ERR    TO LAPUP-RETURN-CD.
 
       5000-PROCESS-TRANS-FIELD-X.
           EXIT.
      /
      *---------------------------
       6000-PROCESS-COMPLEX-FIELD.
      *---------------------------
 
           IF  RUFLD-UPLD-FLD-NM = 'OINS_THIS_CO_IND'
               PERFORM  6100-PROCESS-OINS-CO-IND
                   THRU 6100-PROCESS-OINS-CO-IND-X
               GO TO 6000-PROCESS-COMPLEX-FIELD-X
           END-IF.
 
           MOVE WAPUP-C-UNKNOWN-FIELD-ERR    TO LAPUP-RETURN-CD.
 
       6000-PROCESS-COMPLEX-FIELD-X.
           EXIT.
      /
      *-------------------------
       6100-PROCESS-OINS-CO-IND.
      *-------------------------
 
           IF  LAPUP-INPUT-DATA NOT = '1'
               GO TO 6100-PROCESS-OINS-CO-IND-X
           END-IF.
 
      *
      * ON A JUVENILE APP, PARENTS PENDING/INFORCE IS ALSO
      * COLLECTED AND SHOULD BE BYPASSED
      *
 
           IF  LCLIO-JUVENILE-APP
           AND WS-BYPASS-FOR-JUVENILE
               GO TO 6100-PROCESS-OINS-CO-IND-X
           END-IF.
 
      *
      * FIND CORRECT OCCURANCE IN OINS ARRAY
      *
 
           IF  WS-PEND-NC-STRUCTURE
               MOVE RUFLD-UPLD-FLD-STRUCT-NM TO WS-NEW-INFC-PEND-TYPE
               MOVE WS-UPLD-FLD-STRUCT-NM-CHAR (5)
                                             TO WS-NEW-INFC-PEND-NO
           ELSE
               IF  WS-PEND-JUV-NC-STRUCTURE
                   MOVE RUFLD-UPLD-FLD-STRUCT-NM
                                         TO WS-NEW-INFC-PEND-TYPE
                   MOVE WS-UPLD-FLD-STRUCT-NM-CHAR (6)
                                         TO WS-NEW-INFC-PEND-NO
 
               ELSE
                   PERFORM  6101-OAS-NC-STRUCTURE
                       THRU 6101-OAS-NC-STRUCTURE-X
               END-IF
           END-IF.
 
 
APEX54     PERFORM
               VARYING WS-SUB FROM 1 BY 1
               UNTIL WS-SUB > LCLIO-INFC-PEND-CNT (LCLIO-INS-TYP-NO)
               OR WS-NEW-INFC-PEND-ID =
                     LCLIO-INFC-PEND-ID (LCLIO-INS-TYP-NO, WS-SUB)
015543         CONTINUE
APEX54     END-PERFORM.
 
 
           IF  WS-SUB > LCLIO-INFC-PEND-CNT (LCLIO-INS-TYP-NO)
           OR  WS-NEW-INFC-PEND-ID NOT =
               LCLIO-INFC-PEND-ID (LCLIO-INS-TYP-NO, WS-SUB)
               GO TO 6100-PROCESS-OINS-CO-IND-X
           END-IF.
 
           MOVE RUFLD-UPLD-TTBL-TYP-ID      TO WUTTB-UPLD-TTBL-TYP-ID.
           MOVE RUFLD-UPLD-FLD-APEX-NM      TO WUTTB-UPLD-TTBL-VALU-ID.
 
           PERFORM  UTTB-1000-LOOKUP-UTTB
               THRU UTTB-1000-LOOKUP-UTTB-X.
 
           IF  NOT WUTTB-IO-OK
               MOVE WAPUP-C-TRAN-CONV-ERR   TO LAPUP-RETURN-CD
               GO TO 6100-PROCESS-OINS-CO-IND-X
           END-IF.
 
           MOVE WAPUP-C-YES                 TO LAPUP-REC-CHANGED-SW.
APEX54     SET L5850-CLIO-DATA-UPDATE (WS-SUB)
APEX54                                      TO TRUE.
 
           MOVE RUTTB-UPLD-TTBL-VALU-TXT    TO
APEX54             L5850-OINS-THIS-CO-IND (WS-SUB).
 
       6100-PROCESS-OINS-CO-IND-X.
           EXIT.
      /
      *----------------------
APEX54 6101-OAS-NC-STRUCTURE.
      *----------------------
 
           IF  WS-OAS-NC-STRUCTURE
               MOVE 'OTHE'                   TO WS-NEW-INFC-PEND-TYPE
               MOVE WS-UPLD-FLD-STRUCT-NM-CHAR (4)
                                             TO WS-NEW-INFC-PEND-NO
           ELSE
               IF  WS-OAS-JUV-NC-STRUCTURE
                   MOVE 'OTHE'               TO WS-NEW-INFC-PEND-TYPE
                   MOVE WS-UPLD-FLD-STRUCT-NM-CHAR (5)
                                             TO WS-NEW-INFC-PEND-NO
               END-IF
           END-IF.
 
APEX54 6101-OAS-NC-STRUCTURE-X.
           EXIT.
      /
      *-------------------------
       7000-PROCESS-FIELD-FIELD.
      *-------------------------
 
           IF  LAPUP-INPUT-DATA NOT = '1'
               GO TO 7000-PROCESS-FIELD-FIELD-X
APEX54     END-IF.
 
           MOVE WAPUP-C-UNKNOWN-FIELD-ERR    TO LAPUP-RETURN-CD.
 
       7000-PROCESS-FIELD-FIELD-X.
           EXIT.
      /
      *---------------------
       8000-CHECK-INFC-PEND.
      *---------------------
 
           IF  WS-INFC-PEND-DI-STRUCT
               MOVE +2                     TO LCLIO-INS-TYP-NO
           ELSE
               MOVE +1                     TO LCLIO-INS-TYP-NO
           END-IF.
 
           IF  LAPUP-INPUT-DATA = '0'
           OR  LAPUP-INPUT-DATA = '0.00'
               MOVE WAPUP-C-YES            TO WS-BYPASS-FIELD-SW
               GO TO 8000-CHECK-INFC-PEND-X
           END-IF.
 
      *
      * ON A JUVENILE APP, PARENTS PENDING/INFORCE IS ALSO
      * COLLECTED AND SHOULD BE BYPASSED
      *
 
           IF  LCLIO-JUVENILE-APP
           AND WS-BYPASS-FOR-JUVENILE
               MOVE WAPUP-C-YES            TO WS-BYPASS-FIELD-SW
               GO TO 8000-CHECK-INFC-PEND-X
           END-IF.
 
      *
      * LAST CHARACTER OF APEX FIELD NAME SHOULD BE OCCURANCE #
      *
 
           MOVE RUFLD-UPLD-FLD-APEX-NM     TO WS-FLD-APEX-NM.
 
APEX54     PERFORM
               VARYING WS-SUB FROM WS-C-FLD-APEX-NM-LEN BY -1
               UNTIL WS-SUB = ZERO
               OR WS-FLD-APEX-NM-CHAR (WS-SUB) NOT = SPACES
015543         CONTINUE
APEX54     END-PERFORM.
 
 
           IF  WS-SUB = ZERO
               MOVE WAPUP-C-YES            TO WS-BYPASS-FIELD-SW
               GO TO 8000-CHECK-INFC-PEND-X
           END-IF.
 
           IF  WS-FLD-APEX-NM-CHAR (WS-SUB) NOT NUMERIC
               MOVE WAPUP-C-YES            TO WS-BYPASS-FIELD-SW
               GO TO 8000-CHECK-INFC-PEND-X
           END-IF.
 
      *
      * FIRST 4 CHARS OF STRUCTURE NAME IS PEND/OTHE/INFO.  WHEN
      * TAKEN WITH OCCURANCE NUMBER, WILL UNIQUELY ID INFO
      *
 
           IF  RUFLD-UPLD-FLD-STRUCT-NM = 'INFORCEH'
               MOVE 'OTHE'                   TO WS-NEW-INFC-PEND-TYPE
           ELSE
               MOVE RUFLD-UPLD-FLD-STRUCT-NM TO WS-NEW-INFC-PEND-TYPE
           END-IF.
 
           MOVE WS-FLD-APEX-NM-CHAR (WS-SUB) TO WS-NEW-INFC-PEND-NO.
 
APEX54     PERFORM
               VARYING WS-SUB FROM 1 BY 1
               UNTIL WS-SUB > LCLIO-INFC-PEND-CNT (LCLIO-INS-TYP-NO)
               OR WS-NEW-INFC-PEND-ID =
                     LCLIO-INFC-PEND-ID (LCLIO-INS-TYP-NO, WS-SUB)
015543         CONTINUE
APEX54     END-PERFORM.
 
 
           IF  WS-SUB > LCLIO-INFC-PEND-CNT (LCLIO-INS-TYP-NO)
APEX54         CONTINUE
           ELSE
               GO TO 8000-CHECK-INFC-PEND-X
           END-IF.
 
      *
      * TRY TO ADD A NEW INFC PEND ENTRY
      *
 
           IF  LCLIO-INFC-PEND-CNT (LCLIO-INS-TYP-NO) =
               WS-C-MAX-INFC-PEND
               MOVE WAPUP-C-MAX-INFC-PEND-ERR TO LAPUP-RETURN-CD
               MOVE WAPUP-C-YES               TO WS-BYPASS-FIELD-SW
               GO TO 8000-CHECK-INFC-PEND-X
           END-IF.
 
           ADD +1 TO LCLIO-INFC-PEND-CNT (LCLIO-INS-TYP-NO).
 
           MOVE WS-NEW-INFC-PEND-ID      TO
                LCLIO-INFC-PEND-ID (LCLIO-INS-TYP-NO, WS-SUB).
 
           MOVE 'INFPD'                  TO WUTTB-UPLD-TTBL-TYP-ID.
           MOVE RUFLD-UPLD-FLD-STRUCT-NM TO WUTTB-UPLD-TTBL-VALU-ID.
 
           PERFORM  UTTB-1000-LOOKUP-UTTB
               THRU UTTB-1000-LOOKUP-UTTB-X.
 
           IF  WUTTB-IO-OK
               MOVE WAPUP-C-YES      TO LAPUP-REC-CHANGED-SW
APEX54         SET L5850-CLIO-DATA-UPDATE (WS-SUB)
APEX54                               TO TRUE
               MOVE RUTTB-UPLD-TTBL-VALU-TXT
APEX54                               TO L5850-OINS-INFC-PEND-CD (WS-SUB)
           ELSE
               MOVE WAPUP-C-TRAN-CONV-ERR TO LAPUP-RETURN-CD
           END-IF.
 
       8000-CHECK-INFC-PEND-X.
           EXIT.
      /
      *****************************************************************
      * PROCESSING COPYBOOKS                                          *
      *****************************************************************
 
       COPY ACPPUTTB.
      /
       COPY XCPL0280.
      /
       COPY ACPNUTTB.
      /
       COPY XCPL0030.
 
      *****************************************************************
      **                 END OF PROGRAM ASRUCLIO                     **
      *****************************************************************
