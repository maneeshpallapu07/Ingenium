      *************************
       IDENTIFICATION DIVISION.
      *************************

       PROGRAM-ID.  ASRUCLI.

       COPY XCWWCRHT.

      *****************************************************************
      **  MEMBER :  ASRUCLI                                          **
      **  REMARKS:  APEX UPLOAD CLI TABLE PROCESSING                 **
      **                                                             **
      **  DOMAIN :  CL                                               **
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
557659**  30SEP97  KLE    DATA ARCHITECTURE MODIFICATION             **
557695**  30SEP97  KLE    TAXATION ID ENHANCEMENTS                   **
557698**  30SEP97  TJS    MIXED CASE DATA                            **
557700**  30SEP97  KLE    APEX UPLOAD                                **
015508**  15DEC99  60     CLIENT ENHANCEMENT FOR JAPAN               **
015509**  15DEC99  60     ADDRESS ENHANCEMENT FOR JAPAN              **
014590**  15DEC99  60     ARCHITECTURAL CHANGES                      **
016103**  29SEP00  611J   SEARCH ENHANCEMENTS FOR JAPANESE           **
      *****************************************************************
      /
      **********************
       ENVIRONMENT DIVISION.
      **********************

      ***************
       DATA DIVISION.
      ***************

       WORKING-STORAGE SECTION.

APEX53 COPY XCWWPGWS REPLACING '$VAR1' BY 'ASRUCLI'.

       COPY SQLCA.

014590*COPY XCWL0030.
      /
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
016103     05  WS-PHNT-TXT                   PIC X(08).
016103     05  WS-PHNT-NUM REDEFINES WS-PHNT-TXT PIC 9(8).
      /
      *****************************************************************
      *  COMMON COPYBOOKS                                             *
      *****************************************************************
       COPY ACWWAPUP.

015508 COPY CCFRCLNM.
      /
      *****************************************************************
      *  I/O COPYBOOKS                                                *
      *****************************************************************
      /
       COPY ACFWUTTB.
      /
       COPY ACFRUTTB.
      /
      *****************************************************************
      *  CALLED MODULE PARAMETER INFORMATION                          *
      *****************************************************************

       COPY XCWL0280.
557698 COPY XCWL0005.
      /
      *****************
       LINKAGE SECTION.
      *****************

       01  WGLOB-GLOBAL-AREA.
       COPY XCWWGLOB.

       COPY ACWLAPUP.

       COPY ACFRUFLD.

       COPY CCFRCLI.

015508 COPY CCFRCLNC.

015508 COPY ACWLCLI.
      /
       PROCEDURE DIVISION  USING  WGLOB-GLOBAL-AREA
                                  LAPUP-PARM-AREA
                                  RUFLD-REC-INFO
015508*                           RCLI-REC-INFO.
015508                            RCLI-REC-INFO
015508                            RCLNC-REC-INFO
015508                            LCLI-PARM-AREA.

      *--------------
       0000-MAINLINE.
      *--------------

           PERFORM  1000-INITIALIZE
               THRU 1000-INITIALIZE-X.

015508     MOVE LCLI-CRNT-CLNM-REC       TO RCLNM-REC-INFO.

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
APEX54         WHEN RUFLD-UPLD-FLD-TYP-DOLLAR-AMT
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
APEX54              CONTINUE

APEX54         WHEN OTHER
                    MOVE WAPUP-C-UNKNOWN-FIELD-TYPE
                                         TO LAPUP-RETURN-CD
APEX54     END-EVALUATE.

015508     MOVE RCLNM-REC-INFO           TO LCLI-CRNT-CLNM-REC.

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

           IF  RUFLD-UPLD-FLD-NM = 'BEST_TIME_CALL_TXT'
               MOVE LAPUP-INPUT-DATA         TO RCLI-BEST-TIME-CALL-TXT
               MOVE WAPUP-C-YES              TO LAPUP-REC-CHANGED-SW
               GO TO 2000-PROCESS-CHAR-FIELD-X
           END-IF.

           IF  RUFLD-UPLD-FLD-NM = 'CLI_AGE_PROOF_IND'
               MOVE LAPUP-INPUT-DATA         TO RCLI-CLI-AGE-PROOF-IND
               MOVE WAPUP-C-YES              TO LAPUP-REC-CHANGED-SW
               GO TO 2000-PROCESS-CHAR-FIELD-X
           END-IF.

           IF  RUFLD-UPLD-FLD-NM = 'CLI_CCAS_LOCK_IND'
               MOVE LAPUP-INPUT-DATA         TO RCLI-CLI-CCAS-LOCK-IND
               MOVE WAPUP-C-YES              TO LAPUP-REC-CHANGED-SW
               GO TO 2000-PROCESS-CHAR-FIELD-X
           END-IF.

           IF  RUFLD-UPLD-FLD-NM = 'CLI_CHRTY_IND'
               MOVE LAPUP-INPUT-DATA         TO RCLI-CLI-CHRTY-IND
               MOVE WAPUP-C-YES              TO LAPUP-REC-CHANGED-SW
               GO TO 2000-PROCESS-CHAR-FIELD-X
           END-IF.

           IF  RUFLD-UPLD-FLD-NM = 'CLI_CMPLT_CCAS_IND'
               MOVE LAPUP-INPUT-DATA         TO RCLI-CLI-CMPLT-CCAS-IND
               MOVE WAPUP-C-YES              TO LAPUP-REC-CHANGED-SW
               GO TO 2000-PROCESS-CHAR-FIELD-X
           END-IF.

           IF  RUFLD-UPLD-FLD-NM = 'CLI_CNFD_IND'
               MOVE LAPUP-INPUT-DATA         TO RCLI-CLI-CNFD-IND
               MOVE WAPUP-C-YES              TO LAPUP-REC-CHANGED-SW
               GO TO 2000-PROCESS-CHAR-FIELD-X
           END-IF.

           IF  RUFLD-UPLD-FLD-NM = 'CLI_CNSLDT_IND'
               MOVE LAPUP-INPUT-DATA         TO RCLI-CLI-CNSLDT-IND
               MOVE WAPUP-C-YES              TO LAPUP-REC-CHANGED-SW
               GO TO 2000-PROCESS-CHAR-FIELD-X
           END-IF.

           IF  RUFLD-UPLD-FLD-NM = 'CLI_CO_NM'
015508*        MOVE LAPUP-INPUT-DATA         TO RCLI-CLI-ENTR-CO-NM
015508         MOVE LAPUP-INPUT-DATA         TO RCLNC-CLI-CO-ENTR-NM
557698         PERFORM  8000-TRANSLATE-UPPER-CASE
557698             THRU 8000-TRANSLATE-UPPER-CASE-X
015508*        MOVE LAPUP-INPUT-DATA         TO RCLI-CLI-CO-NM
015508         MOVE LAPUP-INPUT-DATA         TO RCLNC-CLI-CO-NM
               MOVE WAPUP-C-YES              TO LAPUP-REC-CHANGED-SW
015508*        MOVE WGLOB-SYSTEM-DATE-INT    TO RCLI-CLI-CO-NM-CHNG-DT
               GO TO 2000-PROCESS-CHAR-FIELD-X
           END-IF.

           IF  RUFLD-UPLD-FLD-NM = 'CLI_CR_CARD_NUM'
               MOVE LAPUP-INPUT-DATA         TO RCLI-CLI-CR-CARD-NUM
               MOVE WAPUP-C-YES              TO LAPUP-REC-CHANGED-SW
               GO TO 2000-PROCESS-CHAR-FIELD-X
           END-IF.

           IF  RUFLD-UPLD-FLD-NM = 'CLI_CR_CARD_TYP_CD'
               MOVE LAPUP-INPUT-DATA         TO RCLI-CLI-CR-CARD-TYP-CD
               MOVE WAPUP-C-YES              TO LAPUP-REC-CHANGED-SW
               GO TO 2000-PROCESS-CHAR-FIELD-X
           END-IF.

           IF  RUFLD-UPLD-FLD-NM = 'CLI_EMPLR_NM'
015508*        MOVE LAPUP-INPUT-DATA         TO RCLI-CLI-EMPLR-NM
015508         MOVE LAPUP-INPUT-DATA         TO LCLI-CLI-EMPLR-NM
               MOVE WAPUP-C-YES              TO LAPUP-REC-CHANGED-SW
               GO TO 2000-PROCESS-CHAR-FIELD-X
           END-IF.

           IF  RUFLD-UPLD-FLD-NM = 'CLI_GIV_NM'
015508*        MOVE LAPUP-INPUT-DATA         TO RCLI-CLI-ENTR-GIV-NM
015508         MOVE LAPUP-INPUT-DATA         TO RCLNM-ENTR-GIV-NM
557698         PERFORM  8000-TRANSLATE-UPPER-CASE
557698             THRU 8000-TRANSLATE-UPPER-CASE-X
015508*        MOVE LAPUP-INPUT-DATA         TO RCLI-CLI-GIV-NM
015508         MOVE LAPUP-INPUT-DATA         TO RCLNM-CLI-INDV-GIV-NM
               MOVE WAPUP-C-YES              TO LAPUP-REC-CHANGED-SW
APEX52         MOVE WGLOB-SYSTEM-DATE-INT    TO RCLI-CLI-NM-CHNG-DT
               GO TO 2000-PROCESS-CHAR-FIELD-X
           END-IF.

           IF  RUFLD-UPLD-FLD-NM = 'CLI_INCM_EARN_CD'
               MOVE LAPUP-INPUT-DATA         TO RCLI-CLI-INCM-EARN-CD
               MOVE WAPUP-C-YES              TO LAPUP-REC-CHANGED-SW
               GO TO 2000-PROCESS-CHAR-FIELD-X
           END-IF.

           IF  RUFLD-UPLD-FLD-NM = 'CLI_LANG_CD'
               MOVE LAPUP-INPUT-DATA         TO RCLI-CLI-LANG-CD
               MOVE WAPUP-C-YES              TO LAPUP-REC-CHANGED-SW
               GO TO 2000-PROCESS-CHAR-FIELD-X
           END-IF.

           IF  RUFLD-UPLD-FLD-NM = 'CLI_LEGIT_DUP_IND'
               MOVE LAPUP-INPUT-DATA         TO RCLI-CLI-LEGIT-DUP-IND
               MOVE WAPUP-C-YES              TO LAPUP-REC-CHANGED-SW
               GO TO 2000-PROCESS-CHAR-FIELD-X
           END-IF.

           IF  RUFLD-UPLD-FLD-NM = 'CLI_MIB_IND_CD'
               MOVE LAPUP-INPUT-DATA         TO RCLI-CLI-MIB-IND-CD
               MOVE WAPUP-C-YES              TO LAPUP-REC-CHANGED-SW
               GO TO 2000-PROCESS-CHAR-FIELD-X
           END-IF.

           IF  RUFLD-UPLD-FLD-NM = 'CLI_MID_INIT_NM'
015508*        MOVE LAPUP-INPUT-DATA         TO RCLI-CLI-MID-INIT-NM
015508         MOVE LAPUP-INPUT-DATA         TO RCLNM-CLI-INDV-MID-NM
               MOVE WAPUP-C-YES              TO LAPUP-REC-CHANGED-SW
APEX52         MOVE WGLOB-SYSTEM-DATE-INT    TO RCLI-CLI-NM-CHNG-DT
               GO TO 2000-PROCESS-CHAR-FIELD-X
           END-IF.

           IF  RUFLD-UPLD-FLD-NM = 'CLI_PREV_DCLN_IND'
               MOVE LAPUP-INPUT-DATA         TO RCLI-CLI-PREV-DCLN-IND
               MOVE WAPUP-C-YES              TO LAPUP-REC-CHANGED-SW
               GO TO 2000-PROCESS-CHAR-FIELD-X
           END-IF.

           IF  RUFLD-UPLD-FLD-NM = 'CLI_PREV_EMPLR_NM'
015508*        MOVE LAPUP-INPUT-DATA         TO RCLI-CLI-PREV-EMPLR-NM
015508         MOVE LAPUP-INPUT-DATA         TO LCLI-CLI-PREV-EMPLR-NM
               MOVE WAPUP-C-YES              TO LAPUP-REC-CHANGED-SW
               GO TO 2000-PROCESS-CHAR-FIELD-X
           END-IF.

           IF  RUFLD-UPLD-FLD-NM = 'CLI_SFX_NM'
015508*        MOVE LAPUP-INPUT-DATA         TO RCLI-CLI-SFX-NM
015508         MOVE LAPUP-INPUT-DATA         TO RCLNM-CLI-INDV-SFX-NM
               MOVE WAPUP-C-YES              TO LAPUP-REC-CHANGED-SW
APEX52         MOVE WGLOB-SYSTEM-DATE-INT    TO RCLI-CLI-NM-CHNG-DT
               GO TO 2000-PROCESS-CHAR-FIELD-X
           END-IF.

557695*    IF  RUFLD-UPLD-FLD-NM = 'CLI_SIN_ID'
557695     IF  RUFLD-UPLD-FLD-NM = 'CLI_TAX_ID'
557695*        MOVE LAPUP-INPUT-DATA         TO RCLI-CLI-SIN-ID
557695         MOVE LAPUP-INPUT-DATA         TO RCLI-CLI-TAX-ID
               MOVE WAPUP-C-YES              TO LAPUP-REC-CHANGED-SW
               GO TO 2000-PROCESS-CHAR-FIELD-X
           END-IF.

           IF  RUFLD-UPLD-FLD-NM = 'CLI_SMKR_CD'
               MOVE LAPUP-INPUT-DATA         TO RCLI-CLI-SMKR-CD
               MOVE WAPUP-C-YES              TO LAPUP-REC-CHANGED-SW
               GO TO 2000-PROCESS-CHAR-FIELD-X
           END-IF.

           IF  RUFLD-UPLD-FLD-NM = 'CLI_CLAS_CD'
               MOVE LAPUP-INPUT-DATA         TO RCLI-CLI-CLAS-CD
               MOVE WAPUP-C-YES              TO LAPUP-REC-CHANGED-SW
               GO TO 2000-PROCESS-CHAR-FIELD-X
           END-IF.

           IF  RUFLD-UPLD-FLD-NM = 'CLI_TXEMP_CD'
               MOVE LAPUP-INPUT-DATA         TO RCLI-CLI-TXEMP-CD
               MOVE WAPUP-C-YES              TO LAPUP-REC-CHANGED-SW
               GO TO 2000-PROCESS-CHAR-FIELD-X
           END-IF.

           IF  RUFLD-UPLD-FLD-NM = 'CLI_UWGDECN_CD'
               MOVE LAPUP-INPUT-DATA         TO RCLI-CLI-UWGDECN-CD
               MOVE WAPUP-C-YES              TO LAPUP-REC-CHANGED-SW
               GO TO 2000-PROCESS-CHAR-FIELD-X
           END-IF.

           IF  RUFLD-UPLD-FLD-NM = 'CLI_UWGDECN_TYP_CD'
               MOVE LAPUP-INPUT-DATA         TO RCLI-CLI-UWGDECN-TYP-CD
               MOVE WAPUP-C-YES              TO LAPUP-REC-CHANGED-SW
               GO TO 2000-PROCESS-CHAR-FIELD-X
           END-IF.

           IF  RUFLD-UPLD-FLD-NM = 'CLI_WRK_CD'
               MOVE LAPUP-INPUT-DATA         TO RCLI-CLI-WRK-CD
               MOVE WAPUP-C-YES              TO LAPUP-REC-CHANGED-SW
               GO TO 2000-PROCESS-CHAR-FIELD-X
           END-IF.

           IF  RUFLD-UPLD-FLD-NM = 'CLI_WRK_DUR_CD'
               MOVE LAPUP-INPUT-DATA         TO RCLI-CLI-WRK-DUR-CD
               MOVE WAPUP-C-YES              TO LAPUP-REC-CHANGED-SW
               GO TO 2000-PROCESS-CHAR-FIELD-X
           END-IF.

           IF  RUFLD-UPLD-FLD-NM = 'CLI_WRK_RES_IND'
               MOVE LAPUP-INPUT-DATA         TO RCLI-CLI-WRK-RES-IND
               MOVE WAPUP-C-YES              TO LAPUP-REC-CHANGED-SW
               GO TO 2000-PROCESS-CHAR-FIELD-X
           END-IF.

           IF  RUFLD-UPLD-FLD-NM = 'EMPLR_ADDR_1_TXT'
015509*        MOVE LAPUP-INPUT-DATA         TO RCLI-EMPLR-ADDR-1-TXT
015509         MOVE LAPUP-INPUT-DATA         TO LCLI-EMPLR-ADDR-1-TXT
               MOVE WAPUP-C-YES              TO LAPUP-REC-CHANGED-SW
               GO TO 2000-PROCESS-CHAR-FIELD-X
           END-IF.

           IF  RUFLD-UPLD-FLD-NM = 'EMPLR_ADDR_2_TXT'
015509*        MOVE LAPUP-INPUT-DATA         TO RCLI-EMPLR-ADDR-2-TXT
015509         MOVE LAPUP-INPUT-DATA         TO LCLI-EMPLR-ADDR-2-TXT
               MOVE WAPUP-C-YES              TO LAPUP-REC-CHANGED-SW
               GO TO 2000-PROCESS-CHAR-FIELD-X
           END-IF.

557700*    IF  RUFLD-UPLD-FLD-NM = 'INCM_EARN_MODE_CD'
557700*        MOVE LAPUP-INPUT-DATA         TO RCLI-INCM-EARN-MODE-CD
557700*        MOVE WAPUP-C-YES              TO LAPUP-REC-CHANGED-SW
557700*        GO TO 2000-PROCESS-CHAR-FIELD-X
557700*    END-IF.

           IF  RUFLD-UPLD-FLD-NM = 'PREV_CNSLDT_CLI_ID'
               MOVE LAPUP-INPUT-DATA         TO RCLI-PREV-CNSLDT-CLI-ID
               MOVE WAPUP-C-YES              TO LAPUP-REC-CHANGED-SW
               GO TO 2000-PROCESS-CHAR-FIELD-X
           END-IF.

           IF  RUFLD-UPLD-FLD-NM = 'PREV_INFO_CD'
               MOVE LAPUP-INPUT-DATA         TO RCLI-PREV-INFO-CD
               MOVE WAPUP-C-YES              TO LAPUP-REC-CHANGED-SW
               GO TO 2000-PROCESS-CHAR-FIELD-X
           END-IF.

           IF  RUFLD-UPLD-FLD-NM = 'UNMTCH_MAIL_IND'
               MOVE LAPUP-INPUT-DATA         TO RCLI-UNMTCH-MAIL-IND
               MOVE WAPUP-C-YES              TO LAPUP-REC-CHANGED-SW
               GO TO 2000-PROCESS-CHAR-FIELD-X
           END-IF.

           IF  RUFLD-UPLD-FLD-NM = 'UW_USER_1_ID'
               MOVE LAPUP-INPUT-DATA         TO RCLI-UW-USER-1-ID
               MOVE WAPUP-C-YES              TO LAPUP-REC-CHANGED-SW
               GO TO 2000-PROCESS-CHAR-FIELD-X
           END-IF.

           IF  RUFLD-UPLD-FLD-NM = 'UW_USER_2_ID'
               MOVE LAPUP-INPUT-DATA         TO RCLI-UW-USER-2-ID
               MOVE WAPUP-C-YES              TO LAPUP-REC-CHANGED-SW
               GO TO 2000-PROCESS-CHAR-FIELD-X
           END-IF.

           MOVE WAPUP-C-UNKNOWN-FIELD-ERR    TO LAPUP-RETURN-CD.

       2000-PROCESS-CHAR-FIELD-X.
           EXIT.
      /
      *------------------------
       3000-PROCESS-DATE-FIELD.
      *------------------------

           IF  RUFLD-UPLD-FLD-NM = 'CLI_BTH_DT'
               MOVE LAPUP-INPUT-DT-YR        TO WAPUP-CONV-DT-YR
               MOVE LAPUP-INPUT-DT-MON       TO WAPUP-CONV-DT-MON
               MOVE LAPUP-INPUT-DT-DAY       TO WAPUP-CONV-DT-DAY
               MOVE WAPUP-CONV-DT            TO RCLI-CLI-BTH-DT
               MOVE WAPUP-C-YES              TO LAPUP-REC-CHANGED-SW
               GO TO 3000-PROCESS-DATE-FIELD-X
           END-IF.

           IF  RUFLD-UPLD-FLD-NM = 'CLI_CNSLDT_DT'
               MOVE LAPUP-INPUT-DT-YR        TO WAPUP-CONV-DT-YR
               MOVE LAPUP-INPUT-DT-MON       TO WAPUP-CONV-DT-MON
               MOVE LAPUP-INPUT-DT-DAY       TO WAPUP-CONV-DT-DAY
               MOVE WAPUP-CONV-DT            TO RCLI-CLI-CNSLDT-DT
               MOVE WAPUP-C-YES              TO LAPUP-REC-CHANGED-SW
               GO TO 3000-PROCESS-DATE-FIELD-X
           END-IF.

           IF  RUFLD-UPLD-FLD-NM = 'CLI_PREV_DCLN_DT'
               MOVE LAPUP-INPUT-DT-YR        TO WAPUP-CONV-DT-YR
               MOVE LAPUP-INPUT-DT-MON       TO WAPUP-CONV-DT-MON
               MOVE LAPUP-INPUT-DT-DAY       TO WAPUP-CONV-DT-DAY
               MOVE WAPUP-CONV-DT            TO RCLI-CLI-PREV-DCLN-DT
               MOVE WAPUP-C-YES              TO LAPUP-REC-CHANGED-SW
               GO TO 3000-PROCESS-DATE-FIELD-X
           END-IF.

           IF  RUFLD-UPLD-FLD-NM = 'CLI_STMT_DT'
               MOVE LAPUP-INPUT-DT-YR        TO WAPUP-CONV-DT-YR
               MOVE LAPUP-INPUT-DT-MON       TO WAPUP-CONV-DT-MON
               MOVE LAPUP-INPUT-DT-DAY       TO WAPUP-CONV-DT-DAY
               MOVE WAPUP-CONV-DT            TO RCLI-CLI-STMT-DT
               MOVE WAPUP-C-YES              TO LAPUP-REC-CHANGED-SW
               GO TO 3000-PROCESS-DATE-FIELD-X
           END-IF.

           IF  RUFLD-UPLD-FLD-NM = 'CLI_UWGDECN_DT'
               MOVE LAPUP-INPUT-DT-YR        TO WAPUP-CONV-DT-YR
               MOVE LAPUP-INPUT-DT-MON       TO WAPUP-CONV-DT-MON
               MOVE LAPUP-INPUT-DT-DAY       TO WAPUP-CONV-DT-DAY
               MOVE WAPUP-CONV-DT            TO RCLI-CLI-UWGDECN-DT
               MOVE WAPUP-C-YES              TO LAPUP-REC-CHANGED-SW
               GO TO 3000-PROCESS-DATE-FIELD-X
           END-IF.

           IF  RUFLD-UPLD-FLD-NM = 'PREV_UWG_ACTV_DT'
               MOVE LAPUP-INPUT-DT-YR        TO WAPUP-CONV-DT-YR
               MOVE LAPUP-INPUT-DT-MON       TO WAPUP-CONV-DT-MON
               MOVE LAPUP-INPUT-DT-DAY       TO WAPUP-CONV-DT-DAY
               MOVE WAPUP-CONV-DT            TO RCLI-PREV-UWG-ACTV-DT
               MOVE WAPUP-C-YES              TO LAPUP-REC-CHANGED-SW
               GO TO 3000-PROCESS-DATE-FIELD-X
           END-IF.

           MOVE WAPUP-C-UNKNOWN-FIELD-ERR    TO LAPUP-RETURN-CD.

       3000-PROCESS-DATE-FIELD-X.
           EXIT.
      /
      *---------------------------
       4000-PROCESS-NUMERIC-FIELD.
      *---------------------------

557700*    IF  RUFLD-UPLD-FLD-NM = 'CLI_INCM_EARN_AMT'
557700*        MOVE 7                        TO L0280-LENGTH
557700*        MOVE 2                        TO L0280-PRECISION
557700*        MOVE LAPUP-INPUT-DATA         TO L0280-INPUT-DATA
557700*        PERFORM  0280-1000-NUMERIC-EDIT
557700*            THRU 0280-1000-NUMERIC-EDIT-X
557700*        IF  L0280-OK
557700*            COMPUTE RCLI-CLI-INCM-EARN-AMT = L0280-OUTPUT /
557700*                (10 ** L0280-PRECISION)
557700*            MOVE WAPUP-C-YES          TO LAPUP-REC-CHANGED-SW
557700*            GO TO 4000-PROCESS-NUMERIC-FIELD-X
557700*        ELSE
557700*            MOVE WAPUP-C-NUM-CONV-ERR TO LAPUP-RETURN-CD
557700*            MOVE L0280-STATUS         TO LAPUP-SUB-RETURN-CD
557700*            GO TO 4000-PROCESS-NUMERIC-FIELD-X
557700*        END-IF
557700*    END-IF.

           IF  RUFLD-UPLD-FLD-NM = 'CLI_MIB_RQST_CTR'
               MOVE 4                        TO L0280-LENGTH
               MOVE 0                        TO L0280-PRECISION
               MOVE LAPUP-INPUT-DATA         TO L0280-INPUT-DATA
               PERFORM  0280-1000-NUMERIC-EDIT
                   THRU 0280-1000-NUMERIC-EDIT-X
               IF  L0280-OK
                   COMPUTE RCLI-CLI-MIB-RQST-CTR = L0280-OUTPUT /
                       (10 ** L0280-PRECISION)
                   MOVE WAPUP-C-YES          TO LAPUP-REC-CHANGED-SW
                   GO TO 4000-PROCESS-NUMERIC-FIELD-X
               ELSE
                   MOVE WAPUP-C-NUM-CONV-ERR TO LAPUP-RETURN-CD
                   MOVE L0280-STATUS         TO LAPUP-SUB-RETURN-CD
                   GO TO 4000-PROCESS-NUMERIC-FIELD-X
               END-IF
           END-IF.

557700*    IF  RUFLD-UPLD-FLD-NM = 'CLI_NET_WRTH_AMT'
557700*        MOVE 9                        TO L0280-LENGTH
557700*        MOVE 2                        TO L0280-PRECISION
557700*        MOVE LAPUP-INPUT-DATA         TO L0280-INPUT-DATA
557700*        PERFORM  0280-1000-NUMERIC-EDIT
557700*            THRU 0280-1000-NUMERIC-EDIT-X
557700*        IF  L0280-OK
557700*            COMPUTE RCLI-CLI-NET-WRTH-AMT = L0280-OUTPUT /
557700*                (10 ** L0280-PRECISION)
557700*            MOVE WAPUP-C-YES          TO LAPUP-REC-CHANGED-SW
557700*            GO TO 4000-PROCESS-NUMERIC-FIELD-X
557700*        ELSE
557700*            MOVE WAPUP-C-NUM-CONV-ERR TO LAPUP-RETURN-CD
557700*            MOVE L0280-STATUS         TO LAPUP-SUB-RETURN-CD
557700*            GO TO 4000-PROCESS-NUMERIC-FIELD-X
557700*        END-IF
557700*    END-IF.

557700*    IF  RUFLD-UPLD-FLD-NM = 'CLI_OTHR_INCM_AMT'
557700*        MOVE 7                        TO L0280-LENGTH
557700*        MOVE 2                        TO L0280-PRECISION
557700*        MOVE LAPUP-INPUT-DATA         TO L0280-INPUT-DATA
557700*        PERFORM  0280-1000-NUMERIC-EDIT
557700*            THRU 0280-1000-NUMERIC-EDIT-X
557700*        IF  L0280-OK
557700*            COMPUTE RCLI-CLI-OTHR-INCM-AMT = L0280-OUTPUT /
557700*                (10 ** L0280-PRECISION)
557700*            MOVE WAPUP-C-YES          TO LAPUP-REC-CHANGED-SW
557700*            GO TO 4000-PROCESS-NUMERIC-FIELD-X
557700*        ELSE
557700*            MOVE WAPUP-C-NUM-CONV-ERR TO LAPUP-RETURN-CD
557700*            MOVE L0280-STATUS         TO LAPUP-SUB-RETURN-CD
557700*            GO TO 4000-PROCESS-NUMERIC-FIELD-X
557700*        END-IF
557700*    END-IF.

           IF  RUFLD-UPLD-FLD-NM = 'CLI_PRST_RT'
               MOVE 3                        TO L0280-LENGTH
               MOVE 0                        TO L0280-PRECISION
               MOVE LAPUP-INPUT-DATA         TO L0280-INPUT-DATA
               PERFORM  0280-1000-NUMERIC-EDIT
                   THRU 0280-1000-NUMERIC-EDIT-X
               IF  L0280-OK
                   COMPUTE RCLI-CLI-PRST-RT = L0280-OUTPUT /
                       (10 ** L0280-PRECISION)
                   MOVE WAPUP-C-YES          TO LAPUP-REC-CHANGED-SW
                   GO TO 4000-PROCESS-NUMERIC-FIELD-X
               ELSE
                   MOVE WAPUP-C-NUM-CONV-ERR TO LAPUP-RETURN-CD
                   MOVE L0280-STATUS         TO LAPUP-SUB-RETURN-CD
                   GO TO 4000-PROCESS-NUMERIC-FIELD-X
               END-IF
           END-IF.

           IF  RUFLD-UPLD-FLD-NM = 'GIV_NM_PHNT_NUM'
               MOVE 9                        TO L0280-LENGTH
               MOVE 0                        TO L0280-PRECISION
               MOVE LAPUP-INPUT-DATA         TO L0280-INPUT-DATA
               PERFORM  0280-1000-NUMERIC-EDIT
                   THRU 0280-1000-NUMERIC-EDIT-X
               IF  L0280-OK
015508*            COMPUTE RCLI-GIV-NM-PHNT-NUM = L0280-OUTPUT /
016103*            COMPUTE RCLNM-GIV-NM-PHNT-NUM = L0280-OUTPUT /
016103             COMPUTE WS-PHNT-NUM           = L0280-OUTPUT /
                       (10 ** L0280-PRECISION)
016103             MOVE WS-PHNT-TXT          TO RCLNM-GIV-NM-PHNT-TXT
                   MOVE WAPUP-C-YES          TO LAPUP-REC-CHANGED-SW
                   GO TO 4000-PROCESS-NUMERIC-FIELD-X
               ELSE
                   MOVE WAPUP-C-NUM-CONV-ERR TO LAPUP-RETURN-CD
                   MOVE L0280-STATUS         TO LAPUP-SUB-RETURN-CD
                   GO TO 4000-PROCESS-NUMERIC-FIELD-X
               END-IF
           END-IF.

557659*    IF  RUFLD-UPLD-FLD-NM = 'PREV_EMPL_YR_QTY'
557659     IF  RUFLD-UPLD-FLD-NM = 'PREV_EMPL_YR_DUR'
               MOVE 3                        TO L0280-LENGTH
               MOVE 0                        TO L0280-PRECISION
               MOVE LAPUP-INPUT-DATA         TO L0280-INPUT-DATA
               PERFORM  0280-1000-NUMERIC-EDIT
                   THRU 0280-1000-NUMERIC-EDIT-X
               IF  L0280-OK
557659*            COMPUTE RCLI-PREV-EMPL-YR-QTY = L0280-OUTPUT /
557659             COMPUTE RCLI-PREV-EMPL-YR-DUR = L0280-OUTPUT /
                       (10 ** L0280-PRECISION)
                   MOVE WAPUP-C-YES          TO LAPUP-REC-CHANGED-SW
                   GO TO 4000-PROCESS-NUMERIC-FIELD-X
               ELSE
                   MOVE WAPUP-C-NUM-CONV-ERR TO LAPUP-RETURN-CD
                   MOVE L0280-STATUS         TO LAPUP-SUB-RETURN-CD
                   GO TO 4000-PROCESS-NUMERIC-FIELD-X
               END-IF
           END-IF.

           IF  RUFLD-UPLD-FLD-NM = 'SUR_CO_NM_PHNT_NUM'
               MOVE 9                        TO L0280-LENGTH
               MOVE 0                        TO L0280-PRECISION
               MOVE LAPUP-INPUT-DATA         TO L0280-INPUT-DATA
               PERFORM  0280-1000-NUMERIC-EDIT
                   THRU 0280-1000-NUMERIC-EDIT-X
               IF  L0280-OK
015508*            COMPUTE RCLI-SUR-CO-NM-PHNT-NUM = L0280-OUTPUT /
016103*            COMPUTE RCLNC-CLI-CO-NM-PHNT-NUM = L0280-OUTPUT /
016103             COMPUTE WS-PHNT-NUM              = L0280-OUTPUT /
                       (10 ** L0280-PRECISION)
016103             MOVE WS-PHNT-TXT          TO RCLNC-CLI-CO-NM-PHNT-TXT
                   MOVE WAPUP-C-YES          TO LAPUP-REC-CHANGED-SW
                   GO TO 4000-PROCESS-NUMERIC-FIELD-X
               ELSE
                   MOVE WAPUP-C-NUM-CONV-ERR TO LAPUP-RETURN-CD
                   MOVE L0280-STATUS         TO LAPUP-SUB-RETURN-CD
                   GO TO 4000-PROCESS-NUMERIC-FIELD-X
               END-IF
           END-IF.

           IF  RUFLD-UPLD-FLD-NM = 'UWG_WRKSHT_NUM'
               MOVE 3                        TO L0280-LENGTH
               MOVE 0                        TO L0280-PRECISION
               MOVE LAPUP-INPUT-DATA         TO L0280-INPUT-DATA
               PERFORM  0280-1000-NUMERIC-EDIT
                   THRU 0280-1000-NUMERIC-EDIT-X
               IF  L0280-OK
                   COMPUTE RCLI-UWG-WRKSHT-NUM-N = L0280-OUTPUT /
                       (10 ** L0280-PRECISION)
                   MOVE WAPUP-C-YES          TO LAPUP-REC-CHANGED-SW
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

           IF  RUFLD-UPLD-FLD-NM = 'CLI_BTH_LOC_CD'
               MOVE RUFLD-UPLD-TTBL-TYP-ID    TO WUTTB-UPLD-TTBL-TYP-ID
               MOVE LAPUP-INPUT-DATA          TO WUTTB-UPLD-TTBL-VALU-ID
               PERFORM  UTTB-1000-LOOKUP-UTTB
                   THRU UTTB-1000-LOOKUP-UTTB-X
               IF  WUTTB-IO-OK
                   MOVE RUTTB-UPLD-TTBL-VALU-TXT
                                              TO RCLI-CLI-BTH-LOC-CD
                   MOVE WAPUP-C-YES           TO LAPUP-REC-CHANGED-SW
                   GO TO 5000-PROCESS-TRANS-FIELD-X
               ELSE
                   MOVE WAPUP-C-TRAN-CONV-ERR TO LAPUP-RETURN-CD
                   GO TO 5000-PROCESS-TRANS-FIELD-X
               END-IF
           END-IF.

           IF  RUFLD-UPLD-FLD-NM = 'CLI_EMPLR_LOC_CD'
               MOVE RUFLD-UPLD-TTBL-TYP-ID    TO WUTTB-UPLD-TTBL-TYP-ID
               MOVE LAPUP-INPUT-DATA          TO WUTTB-UPLD-TTBL-VALU-ID
               PERFORM  UTTB-1000-LOOKUP-UTTB
                   THRU UTTB-1000-LOOKUP-UTTB-X
               IF  WUTTB-IO-OK
                   MOVE RUTTB-UPLD-TTBL-VALU-TXT
                                              TO RCLI-CLI-EMPLR-LOC-CD
                   MOVE WAPUP-C-YES           TO LAPUP-REC-CHANGED-SW
                   GO TO 5000-PROCESS-TRANS-FIELD-X
               ELSE
                   MOVE WAPUP-C-TRAN-CONV-ERR TO LAPUP-RETURN-CD
                   GO TO 5000-PROCESS-TRANS-FIELD-X
               END-IF
           END-IF.

           IF  RUFLD-UPLD-FLD-NM = 'CLI_MAIL_TITL_TXT'
               MOVE RUFLD-UPLD-TTBL-TYP-ID    TO WUTTB-UPLD-TTBL-TYP-ID
               MOVE LAPUP-INPUT-DATA          TO WUTTB-UPLD-TTBL-VALU-ID
               PERFORM  UTTB-1000-LOOKUP-UTTB
                   THRU UTTB-1000-LOOKUP-UTTB-X
               IF  WUTTB-IO-OK
                   MOVE RUTTB-UPLD-TTBL-VALU-TXT
015508*                                       TO RCLI-CLI-MAIL-TITL-TXT
015508                                        TO RCLNM-CLI-INDV-TITL-TXT
                   MOVE WAPUP-C-YES           TO LAPUP-REC-CHANGED-SW
APEX52             MOVE WGLOB-SYSTEM-DATE-INT TO RCLI-CLI-NM-CHNG-DT
                   GO TO 5000-PROCESS-TRANS-FIELD-X
               ELSE
                   MOVE WAPUP-C-TRAN-CONV-ERR TO LAPUP-RETURN-CD
                   GO TO 5000-PROCESS-TRANS-FIELD-X
               END-IF
           END-IF.

           IF  RUFLD-UPLD-FLD-NM = 'CLI_MARIT_STAT_CD'
               MOVE RUFLD-UPLD-TTBL-TYP-ID    TO WUTTB-UPLD-TTBL-TYP-ID
               MOVE LAPUP-INPUT-DATA          TO WUTTB-UPLD-TTBL-VALU-ID
               PERFORM  UTTB-1000-LOOKUP-UTTB
                   THRU UTTB-1000-LOOKUP-UTTB-X
               IF  WUTTB-IO-OK
                   MOVE RUTTB-UPLD-TTBL-VALU-TXT
                                              TO RCLI-CLI-MARIT-STAT-CD
                   MOVE WAPUP-C-YES           TO LAPUP-REC-CHANGED-SW
                   GO TO 5000-PROCESS-TRANS-FIELD-X
               ELSE
                   MOVE WAPUP-C-TRAN-CONV-ERR TO LAPUP-RETURN-CD
                   GO TO 5000-PROCESS-TRANS-FIELD-X
               END-IF
           END-IF.

           IF  RUFLD-UPLD-FLD-NM = 'CLI_OCCP_CLAS_CD'
               MOVE RUFLD-UPLD-TTBL-TYP-ID    TO WUTTB-UPLD-TTBL-TYP-ID
               MOVE LAPUP-INPUT-DATA          TO WUTTB-UPLD-TTBL-VALU-ID
               PERFORM  UTTB-1000-LOOKUP-UTTB
                   THRU UTTB-1000-LOOKUP-UTTB-X
               IF  WUTTB-IO-OK
                   MOVE RUTTB-UPLD-TTBL-VALU-TXT
                                              TO RCLI-CLI-OCCP-CLAS-CD
                   MOVE WAPUP-C-YES           TO LAPUP-REC-CHANGED-SW
                   GO TO 5000-PROCESS-TRANS-FIELD-X
               ELSE
                   MOVE WAPUP-C-TRAN-CONV-ERR TO LAPUP-RETURN-CD
                   GO TO 5000-PROCESS-TRANS-FIELD-X
               END-IF
           END-IF.

           IF  RUFLD-UPLD-FLD-NM = 'OCCP_ID'
               MOVE RUFLD-UPLD-TTBL-TYP-ID    TO WUTTB-UPLD-TTBL-TYP-ID
               MOVE LAPUP-INPUT-DATA          TO WUTTB-UPLD-TTBL-VALU-ID
               PERFORM  UTTB-1000-LOOKUP-UTTB
                   THRU UTTB-1000-LOOKUP-UTTB-X
               IF  WUTTB-IO-OK
                   MOVE RUTTB-UPLD-TTBL-VALU-TXT TO RCLI-OCCP-ID
                   MOVE WAPUP-C-YES           TO LAPUP-REC-CHANGED-SW
                   GO TO 5000-PROCESS-TRANS-FIELD-X
               ELSE
                   MOVE WAPUP-C-TRAN-CONV-ERR TO LAPUP-RETURN-CD
                   GO TO 5000-PROCESS-TRANS-FIELD-X
               END-IF
           END-IF.

           IF  RUFLD-UPLD-FLD-NM = 'CLI_SELF_EMPL_IND'
               MOVE RUFLD-UPLD-TTBL-TYP-ID    TO WUTTB-UPLD-TTBL-TYP-ID
               MOVE LAPUP-INPUT-DATA          TO WUTTB-UPLD-TTBL-VALU-ID
               PERFORM  UTTB-1000-LOOKUP-UTTB
                   THRU UTTB-1000-LOOKUP-UTTB-X
               IF  WUTTB-IO-OK
                   MOVE RUTTB-UPLD-TTBL-VALU-TXT
                                              TO RCLI-CLI-SELF-EMPL-IND
                   MOVE WAPUP-C-YES           TO LAPUP-REC-CHANGED-SW
                   GO TO 5000-PROCESS-TRANS-FIELD-X
               ELSE
                   MOVE WAPUP-C-TRAN-CONV-ERR TO LAPUP-RETURN-CD
                   GO TO 5000-PROCESS-TRANS-FIELD-X
               END-IF
           END-IF.

           IF  RUFLD-UPLD-FLD-NM = 'CLI_SEX_CD'
               MOVE RUFLD-UPLD-TTBL-TYP-ID    TO WUTTB-UPLD-TTBL-TYP-ID
               MOVE LAPUP-INPUT-DATA          TO WUTTB-UPLD-TTBL-VALU-ID
               PERFORM  UTTB-1000-LOOKUP-UTTB
                   THRU UTTB-1000-LOOKUP-UTTB-X
               IF  WUTTB-IO-OK
                   MOVE RUTTB-UPLD-TTBL-VALU-TXT
                                              TO RCLI-CLI-SEX-CD
                   MOVE WAPUP-C-YES           TO LAPUP-REC-CHANGED-SW
                   GO TO 5000-PROCESS-TRANS-FIELD-X
               ELSE
                   MOVE WAPUP-C-TRAN-CONV-ERR TO LAPUP-RETURN-CD
                   GO TO 5000-PROCESS-TRANS-FIELD-X
               END-IF
           END-IF.

           IF  RUFLD-UPLD-FLD-NM = 'CLI_UIC_QUALF_IND'
               MOVE RUFLD-UPLD-TTBL-TYP-ID    TO WUTTB-UPLD-TTBL-TYP-ID
               MOVE LAPUP-INPUT-DATA          TO WUTTB-UPLD-TTBL-VALU-ID
               PERFORM  UTTB-1000-LOOKUP-UTTB
                   THRU UTTB-1000-LOOKUP-UTTB-X
               IF  WUTTB-IO-OK
                   MOVE RUTTB-UPLD-TTBL-VALU-TXT
                                              TO RCLI-CLI-UIC-QUALF-IND
                   MOVE WAPUP-C-YES           TO LAPUP-REC-CHANGED-SW
                   GO TO 5000-PROCESS-TRANS-FIELD-X
               ELSE
                   MOVE WAPUP-C-TRAN-CONV-ERR TO LAPUP-RETURN-CD
                   GO TO 5000-PROCESS-TRANS-FIELD-X
               END-IF
           END-IF.

           IF  RUFLD-UPLD-FLD-NM = 'CLI_WRK_PT_IND'
               MOVE RUFLD-UPLD-TTBL-TYP-ID    TO WUTTB-UPLD-TTBL-TYP-ID
               MOVE LAPUP-INPUT-DATA          TO WUTTB-UPLD-TTBL-VALU-ID
               PERFORM  UTTB-1000-LOOKUP-UTTB
                   THRU UTTB-1000-LOOKUP-UTTB-X
               IF  WUTTB-IO-OK
                   MOVE RUTTB-UPLD-TTBL-VALU-TXT
                                              TO RCLI-CLI-WRK-PT-IND
                   MOVE WAPUP-C-YES           TO LAPUP-REC-CHANGED-SW
                   GO TO 5000-PROCESS-TRANS-FIELD-X
               ELSE
                   MOVE WAPUP-C-TRAN-CONV-ERR TO LAPUP-RETURN-CD
                   GO TO 5000-PROCESS-TRANS-FIELD-X
               END-IF
           END-IF.

           MOVE WAPUP-C-UNKNOWN-FIELD-ERR     TO LAPUP-RETURN-CD.

       5000-PROCESS-TRANS-FIELD-X.
           EXIT.
      /
      *---------------------------
       6000-PROCESS-COMPLEX-FIELD.
      *---------------------------

557700*    IF  RUFLD-UPLD-FLD-NM = 'CLI_BUS_PHON_NUM'
557700*        MOVE LAPUP-INPUT-DATA         TO WS-PHONE-NUMBER-IN
557700*        MOVE WS-PNUM-IN-AREA          TO WS-PNUM-OUT-AREA
557700*        MOVE WS-PNUM-IN-EXCHANGE      TO WS-PNUM-OUT-EXCHANGE
557700*        MOVE WS-PNUM-IN-LAST4         TO WS-PNUM-OUT-LAST4
557700*        MOVE WS-PHONE-NUMBER-OUT      TO RCLI-CLI-BUS-PHON-NUM
557700*        GO TO 6000-PROCESS-COMPLEX-FIELD-X
557700*    END-IF.

557659*    IF  RUFLD-UPLD-FLD-NM = 'CLI_EMPL_YR_QTY'
557659     IF  RUFLD-UPLD-FLD-NM = 'CLI_EMPL_YR_DUR'
           AND RUFLD-UPLD-FLD-APEX-NM = 'OCC_MONTHS'
               MOVE 3                        TO L0280-LENGTH
               MOVE 0                        TO L0280-PRECISION
               MOVE LAPUP-INPUT-DATA         TO L0280-INPUT-DATA
               PERFORM  0280-1000-NUMERIC-EDIT
                   THRU 0280-1000-NUMERIC-EDIT-X
               IF  L0280-OK
557659*            COMPUTE RCLI-CLI-EMPL-YR-QTY = L0280-OUTPUT / 12
557659             COMPUTE RCLI-CLI-EMPL-YR-DUR = L0280-OUTPUT / 12
                   MOVE WAPUP-C-YES          TO LAPUP-REC-CHANGED-SW
                   GO TO 6000-PROCESS-COMPLEX-FIELD-X
               ELSE
                   MOVE WAPUP-C-NUM-CONV-ERR TO LAPUP-RETURN-CD
                   MOVE L0280-STATUS         TO LAPUP-SUB-RETURN-CD
                   GO TO 6000-PROCESS-COMPLEX-FIELD-X
               END-IF
           END-IF.

           IF  RUFLD-UPLD-FLD-NM = 'CLI_HT'
               MOVE 3                        TO L0280-LENGTH
               MOVE 6                        TO L0280-PRECISION
               MOVE LAPUP-INPUT-DATA         TO L0280-INPUT-DATA
               PERFORM  0280-1000-NUMERIC-EDIT
                   THRU 0280-1000-NUMERIC-EDIT-X
               IF  L0280-OK
                   COMPUTE RCLI-CLI-HT = L0280-OUTPUT /
                       (10 ** L0280-PRECISION)
                   MOVE WAPUP-C-YES          TO LAPUP-REC-CHANGED-SW
                   GO TO 6000-PROCESS-COMPLEX-FIELD-X
               ELSE
                   MOVE WAPUP-C-NUM-CONV-ERR TO LAPUP-RETURN-CD
                   MOVE L0280-STATUS         TO LAPUP-SUB-RETURN-CD
                   GO TO 6000-PROCESS-COMPLEX-FIELD-X
               END-IF
           END-IF.

           IF  RUFLD-UPLD-FLD-NM = 'CLI_ID'
           OR  RUFLD-UPLD-FLD-NM = 'CLI_LANG_CD'
               GO TO 6000-PROCESS-COMPLEX-FIELD-X
           END-IF.

557700*    IF  RUFLD-UPLD-FLD-NM = 'CLI_OTHR_INCM_AMT'
557700*        MOVE 9                        TO L0280-LENGTH
557700*        MOVE 2                        TO L0280-PRECISION
557700*        MOVE LAPUP-INPUT-DATA         TO L0280-INPUT-DATA
557700*        PERFORM  0280-1000-NUMERIC-EDIT
557700*            THRU 0280-1000-NUMERIC-EDIT-X
557700*        PERFORM  6010-CLI-OTHR-INCM-AMT
557700*            THRU 6010-CLI-OTHR-INCM-AMT-X
557700*        GO TO 6000-PROCESS-COMPLEX-FIELD-X
557700*    END-IF.

557700*    IF  RUFLD-UPLD-FLD-NM = 'CLI_RES_PHON_NUM'
557700*        MOVE LAPUP-INPUT-DATA         TO WS-PHONE-NUMBER-IN
557700*        MOVE WS-PNUM-IN-AREA          TO WS-PNUM-OUT-AREA
557700*        MOVE WS-PNUM-IN-EXCHANGE      TO WS-PNUM-OUT-EXCHANGE
557700*        MOVE WS-PNUM-IN-LAST4         TO WS-PNUM-OUT-LAST4
557700*        MOVE WS-PHONE-NUMBER-OUT      TO RCLI-CLI-RES-PHON-NUM
557700*        GO TO 6000-PROCESS-COMPLEX-FIELD-X
557700*    END-IF.

           IF  RUFLD-UPLD-FLD-NM = 'CLI_SUR_NM'
               IF  RCLI-CLI-SEX-CD = 'C'
015508*            MOVE LAPUP-INPUT-DATA     TO RCLI-CLI-ENTR-CO-NM
015508             MOVE LAPUP-INPUT-DATA     TO RCLNC-CLI-CO-ENTR-NM
557698             PERFORM  8000-TRANSLATE-UPPER-CASE
557698                 THRU 8000-TRANSLATE-UPPER-CASE-X
                   MOVE LAPUP-INPUT-DATA     TO RCLNC-CLI-CO-NM
                   MOVE WAPUP-C-YES          TO LAPUP-REC-CHANGED-SW
APEX52             MOVE WGLOB-SYSTEM-DATE-INT
015508*                                      TO RCLI-CLI-CO-NM-CHNG-DT
015508                                       TO RCLI-CLI-NM-CHNG-DT
                   GO TO 6000-PROCESS-COMPLEX-FIELD-X
               ELSE
015508*            MOVE LAPUP-INPUT-DATA     TO RCLI-CLI-ENTR-SUR-NM
015508             MOVE LAPUP-INPUT-DATA     TO RCLNM-ENTR-SUR-NM
557698             PERFORM  8000-TRANSLATE-UPPER-CASE
557698                 THRU 8000-TRANSLATE-UPPER-CASE-X
015508*            MOVE LAPUP-INPUT-DATA     TO RCLI-CLI-SUR-NM
015508             MOVE LAPUP-INPUT-DATA     TO RCLNM-CLI-INDV-SUR-NM
                   MOVE WAPUP-C-YES          TO LAPUP-REC-CHANGED-SW
APEX52             MOVE WGLOB-SYSTEM-DATE-INT
                                             TO RCLI-CLI-NM-CHNG-DT
                   GO TO 6000-PROCESS-COMPLEX-FIELD-X
               END-IF
           END-IF.

           IF  RUFLD-UPLD-FLD-NM = 'CLI_WGT'
               MOVE 3                        TO L0280-LENGTH
               MOVE 6                        TO L0280-PRECISION
               MOVE LAPUP-INPUT-DATA         TO L0280-INPUT-DATA
               PERFORM  0280-1000-NUMERIC-EDIT
                   THRU 0280-1000-NUMERIC-EDIT-X
               IF  L0280-OK
                   COMPUTE RCLI-CLI-WGT = L0280-OUTPUT /
                       (10 ** L0280-PRECISION)
                   MOVE WAPUP-C-YES          TO LAPUP-REC-CHANGED-SW
                   GO TO 6000-PROCESS-COMPLEX-FIELD-X
               ELSE
                   MOVE WAPUP-C-NUM-CONV-ERR TO LAPUP-RETURN-CD
                   MOVE L0280-STATUS         TO LAPUP-SUB-RETURN-CD
                   GO TO 6000-PROCESS-COMPLEX-FIELD-X
               END-IF
           END-IF.

           IF  RUFLD-UPLD-FLD-NM = 'CLI_WRK_QTY'
               MOVE 3                        TO L0280-LENGTH
               MOVE 2                        TO L0280-PRECISION
               MOVE LAPUP-INPUT-DATA         TO L0280-INPUT-DATA
               PERFORM  0280-1000-NUMERIC-EDIT
                   THRU 0280-1000-NUMERIC-EDIT-X
APEX54         PERFORM  6020-CLI-WRK-QTY
APEX54             THRU 6020-CLI-WRK-QTY-X
APEX54         GO TO 6000-PROCESS-COMPLEX-FIELD-X
           END-IF.

557659*    IF  RUFLD-UPLD-FLD-NM = 'PREV_EMPL_YR_QTY'
557659     IF  RUFLD-UPLD-FLD-NM = 'PREV_EMPL_YR_DUR'
           AND RUFLD-UPLD-FLD-STRUCT-NM = 'PERSPO'
           AND RUFLD-UPLD-FLD-APEX-NM = 'YRS_MONTHS'
               MOVE 3                        TO L0280-LENGTH
               MOVE 0                        TO L0280-PRECISION
               MOVE LAPUP-INPUT-DATA         TO L0280-INPUT-DATA
               PERFORM  0280-1000-NUMERIC-EDIT
                   THRU 0280-1000-NUMERIC-EDIT-X
               IF  L0280-OK
557659*            COMPUTE RCLI-PREV-EMPL-YR-QTY = L0280-OUTPUT / 12
557659             COMPUTE RCLI-PREV-EMPL-YR-DUR = L0280-OUTPUT / 12
                   MOVE WAPUP-C-YES          TO LAPUP-REC-CHANGED-SW
                   GO TO 6000-PROCESS-COMPLEX-FIELD-X
               ELSE
                   MOVE WAPUP-C-NUM-CONV-ERR TO LAPUP-RETURN-CD
                   MOVE L0280-STATUS         TO LAPUP-SUB-RETURN-CD
                   GO TO 6000-PROCESS-COMPLEX-FIELD-X
               END-IF
           END-IF.

           MOVE WAPUP-C-UNKNOWN-FIELD-ERR    TO LAPUP-RETURN-CD.

       6000-PROCESS-COMPLEX-FIELD-X.
           EXIT.
557700*
557700*-----------------------
557700*6010-CLI-OTHR-INCM-AMT.
557700*-----------------------
557700*
557700*    IF  L0280-OK
557700*        MOVE WAPUP-C-YES              TO LAPUP-REC-CHANGED-SW
557700*        IF  L0280-OUTPUT-DOLLAR > RCLI-CLI-INCM-EARN-AMT
557700*            COMPUTE RCLI-CLI-OTHR-INCM-AMT
557700*                  = L0280-OUTPUT-DOLLAR
557700*                  - RCLI-CLI-INCM-EARN-AMT
557700*        ELSE
557700*            MOVE ZERO                 TO RCLI-CLI-OTHR-INCM-AMT
557700*        END-IF
557700*    ELSE
557700*        MOVE WAPUP-C-NUM-CONV-ERR     TO LAPUP-RETURN-CD
557700*        MOVE L0280-STATUS             TO LAPUP-SUB-RETURN-CD
557700*    END-IF.
557700*
557700*6010-CLI-OTHR-INCM-AMT-X.
557700*    EXIT.
      /
      *-----------------------
APEX54 6020-CLI-WRK-QTY.
      *-----------------------

           IF  L0280-OK
APEX54         COMPUTE RCLI-CLI-WRK-QTY
APEX54               = L0280-OUTPUT
APEX54               / (10 ** L0280-PRECISION)
               MOVE WAPUP-C-YES              TO LAPUP-REC-CHANGED-SW
               IF  RUFLD-UPLD-FLD-APEX-NM = 'HOURSWEEK'
                   MOVE 'HW'                 TO RCLI-CLI-WRK-INFO
               END-IF
           ELSE
               MOVE WAPUP-C-NUM-CONV-ERR     TO LAPUP-RETURN-CD
               MOVE L0280-STATUS             TO LAPUP-SUB-RETURN-CD
           END-IF.

APEX54 6020-CLI-WRK-QTY-X.
           EXIT.
      /
      *-------------------------
       7000-PROCESS-FIELD-FIELD.
      *-------------------------

           IF  LAPUP-INPUT-DATA NOT = '1'
               GO TO 7000-PROCESS-FIELD-FIELD-X
           END-IF.

           MOVE WAPUP-C-UNKNOWN-FIELD-ERR     TO LAPUP-RETURN-CD.

       7000-PROCESS-FIELD-FIELD-X.
           EXIT.
      /
557698*--------------------------
557698 8000-TRANSLATE-UPPER-CASE.
557698*--------------------------
557698
557698     PERFORM  0005-1000-BUILD-PARM-INFO
557698         THRU 0005-1000-BUILD-PARM-INFO-X.
557698
557698     MOVE LAPUP-INPUT-DATA           TO L0005-INPUT-STRING.
557698
557698     PERFORM  0005-2000-CONVERT-NO-ACCENTS
557698         THRU 0005-2000-CONVERT-NO-ACCENTS-X.
557698
557698     IF  L0005-RETRN-OK
557698         MOVE L0005-OUTPUT-STRING    TO LAPUP-INPUT-DATA
557698     END-IF.
557698
557698 8000-TRANSLATE-UPPER-CASE-X.
557698     EXIT.
      /
      *****************************************************************
      *  PROCESSING COPYBOOKS                                         *
      *****************************************************************
       COPY ACPPUTTB.
      /
      *****************************************************************
      *  LINKAGE PROCESSING COPYBOOKS                                 *
      *****************************************************************
557698 COPY XCPL0005.
557698 COPY XCPS0005.
       COPY XCPL0280.
      /
      *****************************************************************
      *  FILE 1/O PROCESS MODULES                                     *
      *****************************************************************
       COPY ACPNUTTB.
      /
      *****************************************************************
      *  ERROR HANDLING ROUTINES                                      *
      *****************************************************************
       COPY XCPL0030.

      *****************************************************************
      **                 END OF PROGRAM ASRUCLI                      **
      *****************************************************************
