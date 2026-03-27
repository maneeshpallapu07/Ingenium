      *************************
       IDENTIFICATION DIVISION.
      *************************
 
       PROGRAM-ID.  ASRUAPPV.
 
       COPY XCWWCRHT.
 
      *****************************************************************
      **  MEMBER :  ASRUAPPV                                         **
      **  REMARKS:  APEX UPLOAD APPV TABLE PROCESSING                **
      **                                                             **
      **  DOMAIN :  CL                                               **
      **  CLASS  :  FD                                               **
      *****************************************************************
      **   DATE   AUTHOR  DESCRIPTION                                **
      **                                                             **
      **  31JAN94  APEX   NBS/APEX REDESIGN                          **
APEX52**  30NOV94  JJS    UPGRADE TO RELEASE 5.2                     **
APEX53**  30NOV95  JJS    UPGRADE TO INGENIUM 5.3 & WINAPEX 1.0,     **
APEX53**                  NAME CHANGES NOT TAGGED,                   **
APEX53**                  ADD WORKING STORAGE COPYBOOK XCWWPGWS,     **
APEX53**                  CHANGES TO SUPPORT I/O PROGRAMS,           **
APEX53**                  CHANGES TO PROCESSING OF DRIVING OFFENSES  **
APEX54**  31DEC96  TJS    MODIFICATIONS FOR MAINTAINABILITY          **
557660**  20SEP97  JTS    STANDARDIZATION OF CODE STRUCTURE          **
557700**  30SEP97  TJS    APEX UPLOAD 5.5                            **
014590**  15DEC99  60     ARCHITECTURAL CHANGES                      **
      *****************************************************************
 
       ENVIRONMENT DIVISION.
 
       DATA DIVISION.
 
       WORKING-STORAGE SECTION.
 
APEX53 COPY XCWWPGWS REPLACING '$VAR1' BY 'ASRUAPPV'.
 
       COPY SQLCA.
 
014590*COPY XCWL0030.
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
 
       COPY NCFRAPPV.
      /
       PROCEDURE DIVISION  USING  WGLOB-GLOBAL-AREA
                                  LAPUP-PARM-AREA
                                  RUFLD-REC-INFO
                                  RAPPV-REC-INFO.
 
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
 
           GOBACK.
 
       0000-MAINLINE-X.
           EXIT.
      /
      *----------------
       1000-INITIALIZE.
      *----------------
 
           MOVE WAPUP-C-GOOD-RETURN-CD     TO LAPUP-RETURN-CD.
           MOVE WAPUP-C-GOOD-RETURN-CD     TO LAPUP-SUB-RETURN-CD.
           MOVE WAPUP-C-NO                 TO LAPUP-REC-CHANGED-SW.
 
       1000-INITIALIZE-X.
           EXIT.
      /
      *------------------------
       2000-PROCESS-CHAR-FIELD.
      *------------------------
 
           MOVE WAPUP-C-YES                TO LAPUP-REC-CHANGED-SW.
 
           EVALUATE TRUE
 
               WHEN RUFLD-UPLD-FLD-NM = 'AGT_AFCT_ACPT_IND'
                    MOVE LAPUP-INPUT-DATA  TO RAPPV-AGT-AFCT-ACPT-IND
 
               WHEN RUFLD-UPLD-FLD-NM = 'AGT_SEEN_JV_IND'
                    MOVE LAPUP-INPUT-DATA  TO RAPPV-AGT-SEEN-JV-IND
 
               WHEN RUFLD-UPLD-FLD-NM = 'APP_FORM_TYP_ID'
                    MOVE LAPUP-INPUT-DATA  TO RAPPV-APP-FORM-TYP-ID
 
               WHEN RUFLD-UPLD-FLD-NM = 'CLI_ABSNT_WRK_IND'
                    MOVE LAPUP-INPUT-DATA  TO RAPPV-CLI-ABSNT-WRK-IND
 
               WHEN RUFLD-UPLD-FLD-NM = 'CLI_APP_VAR_1_INFO'
                    MOVE LAPUP-INPUT-DATA  TO RAPPV-CLI-APP-VAR-1-INFO
 
               WHEN RUFLD-UPLD-FLD-NM = 'CLI_APP_VAR_2_INFO'
                    MOVE LAPUP-INPUT-DATA  TO RAPPV-CLI-APP-VAR-2-INFO
 
               WHEN RUFLD-UPLD-FLD-NM = 'CLI_DISAB_BNFT_IND'
                    MOVE LAPUP-INPUT-DATA  TO RAPPV-CLI-DISAB-BNFT-IND
 
               WHEN RUFLD-UPLD-FLD-NM = 'CLI_DISAB_IND'
                    MOVE LAPUP-INPUT-DATA  TO RAPPV-CLI-DISAB-IND
 
               WHEN RUFLD-UPLD-FLD-NM = 'CLI_GOOD_HLTH_IND'
                    MOVE LAPUP-INPUT-DATA  TO RAPPV-CLI-GOOD-HLTH-IND
 
               WHEN RUFLD-UPLD-FLD-NM = 'CLI_HO_AMEND_IND'
                    MOVE LAPUP-INPUT-DATA  TO RAPPV-CLI-HO-AMEND-IND
 
               WHEN RUFLD-UPLD-FLD-NM = 'CLI_HO_AMEND_TXT'
                    MOVE LAPUP-INPUT-DATA  TO RAPPV-CLI-HO-AMEND-TXT
 
               WHEN RUFLD-UPLD-FLD-NM = 'CLI_HZRD_AVOC_IND'
                    MOVE LAPUP-INPUT-DATA  TO RAPPV-CLI-HZRD-AVOC-IND
 
               WHEN RUFLD-UPLD-FLD-NM = 'CLI_SMK_CEAS_IND'
                    MOVE LAPUP-INPUT-DATA  TO RAPPV-CLI-SMK-CEAS-IND
 
               WHEN RUFLD-UPLD-FLD-NM = 'CLI_SMK_OTHR_TXT'
                    MOVE LAPUP-INPUT-DATA  TO RAPPV-CLI-SMK-OTHR-TXT
 
               WHEN RUFLD-UPLD-FLD-NM = 'CLI_SPCL_INSTR_IND'
                    MOVE LAPUP-INPUT-DATA  TO RAPPV-CLI-SPCL-INSTR-IND
 
               WHEN RUFLD-UPLD-FLD-NM = 'CLI_SPCL_INSTR_TXT'
                    MOVE LAPUP-INPUT-DATA  TO RAPPV-CLI-SPCL-INSTR-TXT
 
               WHEN RUFLD-UPLD-FLD-NM = 'CLI_TRAV_DEST_TXT'
                    MOVE LAPUP-INPUT-DATA  TO RAPPV-CLI-TRAV-DEST-TXT
 
               WHEN RUFLD-UPLD-FLD-NM = 'DRUG_FREQ_USE_TXT'
                    MOVE LAPUP-INPUT-DATA  TO RAPPV-DRUG-FREQ-USE-TXT
 
               WHEN RUFLD-UPLD-FLD-NM = 'GRWTH_IN_BODY_IND'
                    MOVE LAPUP-INPUT-DATA  TO RAPPV-GRWTH-IN-BODY-IND
 
               WHEN RUFLD-UPLD-FLD-NM = 'HZRD_DRV_LIC_ID'
                    MOVE LAPUP-INPUT-DATA  TO RAPPV-HZRD-DRV-LIC-ID
 
APEX52         WHEN RUFLD-UPLD-FLD-NM = 'HZRD_DRV_OFFNS_TXT'
APEX52              MOVE LAPUP-INPUT-DATA  TO RAPPV-HZRD-DRV-OFFNS-TXT
 
               WHEN RUFLD-UPLD-FLD-NM = 'INSRD_APCH_AGT_IND'
                    MOVE LAPUP-INPUT-DATA  TO RAPPV-INSRD-APCH-AGT-IND
 
               WHEN RUFLD-UPLD-FLD-NM = 'JV_GOOD_HLTH_IND'
                    MOVE LAPUP-INPUT-DATA  TO RAPPV-JV-GOOD-HLTH-IND
 
               WHEN RUFLD-UPLD-FLD-NM = 'JV_LIVES_WTH_TXT'
                    MOVE LAPUP-INPUT-DATA  TO RAPPV-JV-LIVES-WTH-TXT
 
               WHEN RUFLD-UPLD-FLD-NM = 'JV_NM_DIFF_OWN_TXT'
                    MOVE LAPUP-INPUT-DATA  TO RAPPV-JV-NM-DIFF-OWN-TXT
 
               WHEN RUFLD-UPLD-FLD-NM = 'JV_NM_EQ_OWN_IND'
                    MOVE LAPUP-INPUT-DATA  TO RAPPV-JV-NM-EQ-OWN-IND
 
               WHEN RUFLD-UPLD-FLD-NM = 'JV_OWN_PARNT_IND'
                    MOVE LAPUP-INPUT-DATA  TO RAPPV-JV-OWN-PARNT-IND
 
               WHEN RUFLD-UPLD-FLD-NM = 'OTHR_ILL_SURGY_IND'
                    MOVE LAPUP-INPUT-DATA  TO RAPPV-OTHR-ILL-SURGY-IND
 
               WHEN RUFLD-UPLD-FLD-NM = 'OTHR_JV_INSRD_IND'
                    MOVE LAPUP-INPUT-DATA  TO RAPPV-OTHR-JV-INSRD-IND
 
               WHEN RUFLD-UPLD-FLD-NM = 'OWN_INS_XCD_JV_IND'
                    MOVE LAPUP-INPUT-DATA  TO RAPPV-OWN-INS-XCD-JV-IND
 
               WHEN RUFLD-UPLD-FLD-NM = 'OWN_NOT_PARNT_TXT'
                    MOVE LAPUP-INPUT-DATA  TO RAPPV-OWN-NOT-PARNT-TXT
 
               WHEN RUFLD-UPLD-FLD-NM = 'OWN_SUPRT_JV_IND'
                    MOVE LAPUP-INPUT-DATA  TO RAPPV-OWN-SUPRT-JV-IND
 
               WHEN RUFLD-UPLD-FLD-NM = 'PREV_UPDT_CO_ID'
                    MOVE LAPUP-INPUT-DATA  TO RAPPV-PREV-UPDT-CO-ID
 
               WHEN RUFLD-UPLD-FLD-NM = 'SMK_CIG_EVER_IND'
                    MOVE LAPUP-INPUT-DATA  TO RAPPV-SMK-CIG-EVER-IND
 
               WHEN RUFLD-UPLD-FLD-NM = 'WHY_CHOS_JV_TXT'
                    MOVE LAPUP-INPUT-DATA  TO RAPPV-WHY-CHOS-JV-TXT
 
               WHEN RUFLD-UPLD-FLD-NM = 'CLI_TRAV_DUR_TXT'
                    MOVE LAPUP-INPUT-DATA  TO RAPPV-CLI-TRAV-DUR-TXT
 
               WHEN RUFLD-UPLD-FLD-NM = 'WHY_INSRD_JV_TXT'
                    MOVE LAPUP-INPUT-DATA  TO RAPPV-WHY-INSRD-JV-TXT
 
APEX52         WHEN RUFLD-UPLD-FLD-NM = 'OCCP_CHNG_TRAV_TXT'
APEX52              MOVE LAPUP-INPUT-DATA  TO RAPPV-HZRD-DRV-OFFNS-TXT
 
               WHEN OTHER
APEX52              MOVE WAPUP-C-NO        TO LAPUP-REC-CHANGED-SW
                    MOVE WAPUP-C-UNKNOWN-FIELD-ERR
                                           TO LAPUP-RETURN-CD
 
           END-EVALUATE.
 
       2000-PROCESS-CHAR-FIELD-X.
           EXIT.
      /
      *------------------------
       3000-PROCESS-DATE-FIELD.
      *------------------------
 
           IF  RUFLD-UPLD-FLD-NM = 'CLI_PREV_SMK_DT'
               MOVE LAPUP-INPUT-DT-YR      TO WAPUP-CONV-DT-YR
               MOVE LAPUP-INPUT-DT-MON     TO WAPUP-CONV-DT-MON
               MOVE LAPUP-INPUT-DT-DAY     TO WAPUP-CONV-DT-DAY
               MOVE WAPUP-CONV-DT          TO RAPPV-CLI-PREV-SMK-DT
               MOVE WAPUP-C-YES            TO LAPUP-REC-CHANGED-SW
               GO TO 3000-PROCESS-DATE-FIELD-X
           END-IF.
 
           IF  RUFLD-UPLD-FLD-NM = 'DRUG_PREV_USE_DT'
               MOVE LAPUP-INPUT-DT-YR      TO WAPUP-CONV-DT-YR
               MOVE LAPUP-INPUT-DT-MON     TO WAPUP-CONV-DT-MON
               MOVE LAPUP-INPUT-DT-DAY     TO WAPUP-CONV-DT-DAY
               MOVE WAPUP-CONV-DT          TO RAPPV-DRUG-PREV-USE-DT
               MOVE WAPUP-C-YES            TO LAPUP-REC-CHANGED-SW
               GO TO 3000-PROCESS-DATE-FIELD-X
           END-IF.
 
           IF  RUFLD-UPLD-FLD-NM = 'HZRD_DRV_OFFNS_DT'
               MOVE LAPUP-INPUT-DT-YR      TO WAPUP-CONV-DT-YR
               MOVE LAPUP-INPUT-DT-MON     TO WAPUP-CONV-DT-MON
               MOVE LAPUP-INPUT-DT-DAY     TO WAPUP-CONV-DT-DAY
APEX53         IF  RAPPV-HZRD-DRV-OFFNS-DT = '0000-00-00'
APEX53             MOVE WAPUP-CONV-DT      TO RAPPV-HZRD-DRV-OFFNS-DT
APEX53             MOVE WAPUP-C-YES        TO LAPUP-REC-CHANGED-SW
APEX53         ELSE
APEX53             MOVE WAPUP-C-MULT-VALUES-ERR
APEX53                                     TO LAPUP-RETURN-CD
APEX53         END-IF
APEX53         GO TO 3000-PROCESS-DATE-FIELD-X
APEX53     END-IF.
 
           IF  RUFLD-UPLD-FLD-NM = 'OCCP_CHNG_TRAV_DT'
               MOVE LAPUP-INPUT-DT-YR      TO WAPUP-CONV-DT-YR
               MOVE LAPUP-INPUT-DT-MON     TO WAPUP-CONV-DT-MON
               MOVE LAPUP-INPUT-DT-DAY     TO WAPUP-CONV-DT-DAY
               MOVE WAPUP-CONV-DT          TO RAPPV-OCCP-CHNG-TRAV-DT
               MOVE WAPUP-C-YES            TO LAPUP-REC-CHANGED-SW
               GO TO 3000-PROCESS-DATE-FIELD-X
           END-IF.
 
           MOVE WAPUP-C-UNKNOWN-FIELD-ERR  TO LAPUP-RETURN-CD.
 
       3000-PROCESS-DATE-FIELD-X.
           EXIT.
      /
      *---------------------------
       4000-PROCESS-NUMERIC-FIELD.
      *---------------------------
 
           IF  RUFLD-UPLD-FLD-NM = 'CLI_SMK_OTHR_QTY'
APEX52         MOVE 1                      TO L0280-LENGTH
APEX52         MOVE 6                      TO L0280-PRECISION
               MOVE LAPUP-INPUT-DATA       TO L0280-INPUT-DATA
               PERFORM  0280-1000-NUMERIC-EDIT
                   THRU 0280-1000-NUMERIC-EDIT-X
               IF  L0280-OK
APEX52             COMPUTE RAPPV-CLI-SMK-OTHR-QTY = L0280-OUTPUT *
APEX52                 25 / 1000000
                   MOVE WAPUP-C-YES        TO LAPUP-REC-CHANGED-SW
               ELSE
                   MOVE WAPUP-C-NUM-CONV-ERR TO LAPUP-RETURN-CD
                   MOVE L0280-STATUS         TO LAPUP-SUB-RETURN-CD
               END-IF
               GO TO 4000-PROCESS-NUMERIC-FIELD-X
           END-IF.
 
APEX52     IF  RUFLD-UPLD-FLD-NM = 'CLI_SMK_CIG_QTY'
APEX52         MOVE 1                      TO L0280-LENGTH
APEX52         MOVE 6                      TO L0280-PRECISION
APEX52         MOVE LAPUP-INPUT-DATA       TO L0280-INPUT-DATA
APEX52         PERFORM  0280-1000-NUMERIC-EDIT
APEX52             THRU 0280-1000-NUMERIC-EDIT-X
APEX52         IF  L0280-OK
APEX52             COMPUTE RAPPV-CLI-SMK-CIG-QTY = L0280-OUTPUT *
APEX52                 25 / 1000000
APEX52             MOVE WAPUP-C-YES        TO LAPUP-REC-CHANGED-SW
APEX52         ELSE
APEX52             MOVE WAPUP-C-NUM-CONV-ERR TO LAPUP-RETURN-CD
APEX52             MOVE L0280-STATUS         TO LAPUP-SUB-RETURN-CD
               END-IF
               GO TO 4000-PROCESS-NUMERIC-FIELD-X
           END-IF.
 
           IF  RUFLD-UPLD-FLD-NM = 'JV_FTHR_INS_AMT'
               MOVE 9                      TO L0280-LENGTH
               MOVE 0                      TO L0280-PRECISION
               MOVE LAPUP-INPUT-DATA       TO L0280-INPUT-DATA
               PERFORM  0280-1000-NUMERIC-EDIT
                   THRU 0280-1000-NUMERIC-EDIT-X
               IF  L0280-OK
                   COMPUTE RAPPV-JV-FTHR-INS-AMT = L0280-OUTPUT /
                       (10 ** L0280-PRECISION)
                   MOVE WAPUP-C-YES        TO LAPUP-REC-CHANGED-SW
               ELSE
                   MOVE WAPUP-C-NUM-CONV-ERR TO LAPUP-RETURN-CD
                   MOVE L0280-STATUS         TO LAPUP-SUB-RETURN-CD
               END-IF
               GO TO 4000-PROCESS-NUMERIC-FIELD-X
           END-IF.
 
           IF  RUFLD-UPLD-FLD-NM = 'JV_MTHR_INS_AMT'
               MOVE 9                      TO L0280-LENGTH
               MOVE 0                      TO L0280-PRECISION
               MOVE LAPUP-INPUT-DATA       TO L0280-INPUT-DATA
               PERFORM  0280-1000-NUMERIC-EDIT
                   THRU 0280-1000-NUMERIC-EDIT-X
               IF  L0280-OK
                   COMPUTE RAPPV-JV-MTHR-INS-AMT = L0280-OUTPUT /
                       (10 ** L0280-PRECISION)
                   MOVE WAPUP-C-YES        TO LAPUP-REC-CHANGED-SW
               ELSE
                   MOVE WAPUP-C-NUM-CONV-ERR TO LAPUP-RETURN-CD
                   MOVE L0280-STATUS         TO LAPUP-SUB-RETURN-CD
               END-IF
               GO TO 4000-PROCESS-NUMERIC-FIELD-X
           END-IF.
 
           IF  RUFLD-UPLD-FLD-NM = 'JV_SIBL_1_INS_AMT'
               MOVE 9                      TO L0280-LENGTH
               MOVE 0                      TO L0280-PRECISION
               MOVE LAPUP-INPUT-DATA       TO L0280-INPUT-DATA
               PERFORM  0280-1000-NUMERIC-EDIT
                   THRU 0280-1000-NUMERIC-EDIT-X
               IF  L0280-OK
                   COMPUTE RAPPV-JV-SIBL-1-INS-AMT = L0280-OUTPUT /
                       (10 ** L0280-PRECISION)
                   MOVE WAPUP-C-YES        TO LAPUP-REC-CHANGED-SW
               ELSE
                   MOVE WAPUP-C-NUM-CONV-ERR TO LAPUP-RETURN-CD
                   MOVE L0280-STATUS         TO LAPUP-SUB-RETURN-CD
               END-IF
               GO TO 4000-PROCESS-NUMERIC-FIELD-X
           END-IF.
 
           IF  RUFLD-UPLD-FLD-NM = 'JV_SIBL_2_INS_AMT'
               MOVE 9                      TO L0280-LENGTH
               MOVE 0                      TO L0280-PRECISION
               MOVE LAPUP-INPUT-DATA       TO L0280-INPUT-DATA
               PERFORM  0280-1000-NUMERIC-EDIT
                   THRU 0280-1000-NUMERIC-EDIT-X
               IF  L0280-OK
                   COMPUTE RAPPV-JV-SIBL-2-INS-AMT = L0280-OUTPUT /
                       (10 ** L0280-PRECISION)
                   MOVE WAPUP-C-YES        TO LAPUP-REC-CHANGED-SW
               ELSE
                   MOVE WAPUP-C-NUM-CONV-ERR TO LAPUP-RETURN-CD
                   MOVE L0280-STATUS         TO LAPUP-SUB-RETURN-CD
               END-IF
               GO TO 4000-PROCESS-NUMERIC-FIELD-X
           END-IF.
 
           IF  RUFLD-UPLD-FLD-NM = 'JV_SIBL_3_INS_AMT'
               MOVE 9                      TO L0280-LENGTH
               MOVE 0                      TO L0280-PRECISION
               MOVE LAPUP-INPUT-DATA       TO L0280-INPUT-DATA
               PERFORM  0280-1000-NUMERIC-EDIT
                   THRU 0280-1000-NUMERIC-EDIT-X
               IF  L0280-OK
                   COMPUTE RAPPV-JV-SIBL-3-INS-AMT = L0280-OUTPUT /
                       (10 ** L0280-PRECISION)
                   MOVE WAPUP-C-YES        TO LAPUP-REC-CHANGED-SW
               ELSE
                   MOVE WAPUP-C-NUM-CONV-ERR TO LAPUP-RETURN-CD
                   MOVE L0280-STATUS         TO LAPUP-SUB-RETURN-CD
               END-IF
               GO TO 4000-PROCESS-NUMERIC-FIELD-X
           END-IF.
 
           MOVE WAPUP-C-UNKNOWN-FIELD-ERR  TO LAPUP-RETURN-CD.
 
       4000-PROCESS-NUMERIC-FIELD-X.
           EXIT.
      /
      *-------------------------
       5000-PROCESS-TRANS-FIELD.
      *-------------------------
 
           IF  RUFLD-UPLD-FLD-NM = 'AIDS_ANTIBD_IND'
 
               MOVE RUFLD-UPLD-TTBL-TYP-ID    TO WUTTB-UPLD-TTBL-TYP-ID
               MOVE LAPUP-INPUT-DATA          TO WUTTB-UPLD-TTBL-VALU-ID
 
               PERFORM  UTTB-1000-LOOKUP-UTTB
                   THRU UTTB-1000-LOOKUP-UTTB-X
 
APEX54         IF  NOT WUTTB-IO-OK
                   MOVE WAPUP-C-TRAN-CONV-ERR TO LAPUP-RETURN-CD
                   GO TO 5000-PROCESS-TRANS-FIELD-X
APEX54         END-IF
 
APEX54         IF  RAPPV-AIDS-ANTIBD-IND NOT = 'Y'
                   MOVE RUTTB-UPLD-TTBL-VALU-TXT
                                              TO RAPPV-AIDS-ANTIBD-IND
                   MOVE WAPUP-C-YES           TO LAPUP-REC-CHANGED-SW
               END-IF
 
               GO TO 5000-PROCESS-TRANS-FIELD-X
 
           END-IF.
 
           IF  RUFLD-UPLD-FLD-NM = 'ALCHL_TRTMNT_IND'
 
               MOVE RUFLD-UPLD-TTBL-TYP-ID    TO WUTTB-UPLD-TTBL-TYP-ID
               MOVE LAPUP-INPUT-DATA          TO WUTTB-UPLD-TTBL-VALU-ID
 
               PERFORM  UTTB-1000-LOOKUP-UTTB
                   THRU UTTB-1000-LOOKUP-UTTB-X
 
APEX54         IF  NOT WUTTB-IO-OK
                   MOVE WAPUP-C-TRAN-CONV-ERR TO LAPUP-RETURN-CD
                   GO TO 5000-PROCESS-TRANS-FIELD-X
               END-IF
 
APEX54         IF  RAPPV-ALCHL-TRTMNT-IND NOT = 'Y'
                   MOVE RUTTB-UPLD-TTBL-VALU-TXT
                                              TO RAPPV-ALCHL-TRTMNT-IND
                   MOVE WAPUP-C-YES           TO LAPUP-REC-CHANGED-SW
               END-IF
 
               GO TO 5000-PROCESS-TRANS-FIELD-X
 
           END-IF.
 
           IF  RUFLD-UPLD-FLD-NM = 'CLI_AIDS_IND'
 
               MOVE RUFLD-UPLD-TTBL-TYP-ID    TO WUTTB-UPLD-TTBL-TYP-ID
               MOVE LAPUP-INPUT-DATA          TO WUTTB-UPLD-TTBL-VALU-ID
 
               PERFORM  UTTB-1000-LOOKUP-UTTB
                   THRU UTTB-1000-LOOKUP-UTTB-X
 
APEX54         IF  NOT WUTTB-IO-OK
                   MOVE WAPUP-C-TRAN-CONV-ERR TO LAPUP-RETURN-CD
                   GO TO 5000-PROCESS-TRANS-FIELD-X
               END-IF
 
APEX54         IF  RAPPV-CLI-AIDS-IND NOT = 'Y'
                   MOVE RUTTB-UPLD-TTBL-VALU-TXT
                                              TO RAPPV-CLI-AIDS-IND
                   MOVE WAPUP-C-YES           TO LAPUP-REC-CHANGED-SW
               END-IF
 
               GO TO 5000-PROCESS-TRANS-FIELD-X
 
           END-IF.
 
           IF RUFLD-UPLD-FLD-NM = 'CLI_CARDIO_SYS_IND'
 
               MOVE RUFLD-UPLD-TTBL-TYP-ID    TO WUTTB-UPLD-TTBL-TYP-ID
               MOVE LAPUP-INPUT-DATA          TO WUTTB-UPLD-TTBL-VALU-ID
 
               PERFORM  UTTB-1000-LOOKUP-UTTB
                   THRU UTTB-1000-LOOKUP-UTTB-X
 
APEX54         IF  NOT WUTTB-IO-OK
                   MOVE WAPUP-C-TRAN-CONV-ERR TO LAPUP-RETURN-CD
                   GO TO 5000-PROCESS-TRANS-FIELD-X
               END-IF
 
APEX54         IF  RAPPV-CLI-CARDIO-SYS-IND NOT = 'Y'
                   MOVE RUTTB-UPLD-TTBL-VALU-TXT
                                        TO RAPPV-CLI-CARDIO-SYS-IND
                   MOVE WAPUP-C-YES     TO LAPUP-REC-CHANGED-SW
               END-IF
 
               GO TO 5000-PROCESS-TRANS-FIELD-X
 
           END-IF.
 
           IF  RUFLD-UPLD-FLD-NM = 'CLI_DIAGNS_TST_IND'
 
               MOVE RUFLD-UPLD-TTBL-TYP-ID    TO WUTTB-UPLD-TTBL-TYP-ID
               MOVE LAPUP-INPUT-DATA          TO WUTTB-UPLD-TTBL-VALU-ID
 
               PERFORM  UTTB-1000-LOOKUP-UTTB
                   THRU UTTB-1000-LOOKUP-UTTB-X
 
APEX54         IF  NOT WUTTB-IO-OK
                   MOVE WAPUP-C-TRAN-CONV-ERR TO LAPUP-RETURN-CD
                   GO TO 5000-PROCESS-TRANS-FIELD-X
               END-IF
 
APEX54         IF  RAPPV-CLI-DIAGNS-TST-IND NOT = 'Y'
                   MOVE RUTTB-UPLD-TTBL-VALU-TXT
                                            TO RAPPV-CLI-DIAGNS-TST-IND
                   MOVE WAPUP-C-YES         TO LAPUP-REC-CHANGED-SW
               END-IF
 
               GO TO 5000-PROCESS-TRANS-FIELD-X
 
           END-IF.
 
           IF  RUFLD-UPLD-FLD-NM = 'CLI_DIGEST_SYS_IND'
 
               MOVE RUFLD-UPLD-TTBL-TYP-ID    TO WUTTB-UPLD-TTBL-TYP-ID
               MOVE LAPUP-INPUT-DATA          TO WUTTB-UPLD-TTBL-VALU-ID
 
               PERFORM  UTTB-1000-LOOKUP-UTTB
                   THRU UTTB-1000-LOOKUP-UTTB-X
 
APEX54         IF  NOT WUTTB-IO-OK
                   MOVE WAPUP-C-TRAN-CONV-ERR TO LAPUP-RETURN-CD
                   GO TO 5000-PROCESS-TRANS-FIELD-X
               END-IF
 
APEX54         IF  RAPPV-CLI-DIGEST-SYS-IND NOT = 'Y'
                   MOVE RUTTB-UPLD-TTBL-VALU-TXT
                                            TO RAPPV-CLI-DIGEST-SYS-IND
                   MOVE WAPUP-C-YES         TO LAPUP-REC-CHANGED-SW
               END-IF
 
               GO TO 5000-PROCESS-TRANS-FIELD-X
 
           END-IF.
 
           IF  RUFLD-UPLD-FLD-NM = 'CLI_EPLPSY_IND'
 
               MOVE RUFLD-UPLD-TTBL-TYP-ID    TO WUTTB-UPLD-TTBL-TYP-ID
               MOVE LAPUP-INPUT-DATA          TO WUTTB-UPLD-TTBL-VALU-ID
 
               PERFORM  UTTB-1000-LOOKUP-UTTB
                   THRU UTTB-1000-LOOKUP-UTTB-X
 
APEX54         IF  NOT WUTTB-IO-OK
                   MOVE WAPUP-C-TRAN-CONV-ERR TO LAPUP-RETURN-CD
                   GO TO 5000-PROCESS-TRANS-FIELD-X
               END-IF
 
APEX54         IF  RAPPV-CLI-EPLPSY-IND NOT = 'Y'
                   MOVE RUTTB-UPLD-TTBL-VALU-TXT
                                              TO RAPPV-CLI-EPLPSY-IND
                   MOVE WAPUP-C-YES           TO LAPUP-REC-CHANGED-SW
               END-IF
 
               GO TO 5000-PROCESS-TRANS-FIELD-X
 
           END-IF.
 
           IF  RUFLD-UPLD-FLD-NM = 'CLI_HZRD_FLY_IND'
 
               MOVE RUFLD-UPLD-TTBL-TYP-ID    TO WUTTB-UPLD-TTBL-TYP-ID
               MOVE LAPUP-INPUT-DATA          TO WUTTB-UPLD-TTBL-VALU-ID
 
               PERFORM  UTTB-1000-LOOKUP-UTTB
                   THRU UTTB-1000-LOOKUP-UTTB-X
 
APEX54         IF  NOT WUTTB-IO-OK
                   MOVE WAPUP-C-TRAN-CONV-ERR TO LAPUP-RETURN-CD
                   GO TO 5000-PROCESS-TRANS-FIELD-X
               END-IF
 
APEX54         IF  RAPPV-CLI-HZRD-FLY-IND NOT = 'Y'
                   MOVE RUTTB-UPLD-TTBL-VALU-TXT
                                          TO RAPPV-CLI-HZRD-FLY-IND
                   MOVE WAPUP-C-YES       TO LAPUP-REC-CHANGED-SW
               END-IF
 
               GO TO 5000-PROCESS-TRANS-FIELD-X
 
           END-IF.
 
           IF  RUFLD-UPLD-FLD-NM = 'CLI_HZRD_SPORT_IND'
 
               MOVE RUFLD-UPLD-TTBL-TYP-ID    TO WUTTB-UPLD-TTBL-TYP-ID
               MOVE LAPUP-INPUT-DATA          TO WUTTB-UPLD-TTBL-VALU-ID
 
               PERFORM  UTTB-1000-LOOKUP-UTTB
                   THRU UTTB-1000-LOOKUP-UTTB-X
 
APEX54         IF  NOT WUTTB-IO-OK
                   MOVE WAPUP-C-TRAN-CONV-ERR TO LAPUP-RETURN-CD
                   GO TO 5000-PROCESS-TRANS-FIELD-X
               END-IF
 
APEX54         IF  RAPPV-CLI-HZRD-SPORT-IND NOT = 'Y'
                   MOVE RUTTB-UPLD-TTBL-VALU-TXT
                                        TO RAPPV-CLI-HZRD-SPORT-IND
                   MOVE WAPUP-C-YES     TO LAPUP-REC-CHANGED-SW
               END-IF
 
               GO TO 5000-PROCESS-TRANS-FIELD-X
 
           END-IF.
 
           IF  RUFLD-UPLD-FLD-NM = 'CLI_LIQR_DRINK_IND'
 
               MOVE RUFLD-UPLD-TTBL-TYP-ID    TO WUTTB-UPLD-TTBL-TYP-ID
               MOVE LAPUP-INPUT-DATA          TO WUTTB-UPLD-TTBL-VALU-ID
 
               PERFORM  UTTB-1000-LOOKUP-UTTB
                   THRU UTTB-1000-LOOKUP-UTTB-X
 
APEX54         IF  NOT WUTTB-IO-OK
                   MOVE WAPUP-C-TRAN-CONV-ERR TO LAPUP-RETURN-CD
                   GO TO 5000-PROCESS-TRANS-FIELD-X
               END-IF
 
APEX54         IF  RAPPV-CLI-LIQR-DRINK-IND NOT = 'Y'
                   MOVE RUTTB-UPLD-TTBL-VALU-TXT
                                            TO RAPPV-CLI-LIQR-DRINK-IND
                   MOVE WAPUP-C-YES         TO LAPUP-REC-CHANGED-SW
               END-IF
 
               GO TO 5000-PROCESS-TRANS-FIELD-X
 
           END-IF.
 
           IF  RUFLD-UPLD-FLD-NM = 'CLI_NERV_SYS_IND'
 
               MOVE RUFLD-UPLD-TTBL-TYP-ID    TO WUTTB-UPLD-TTBL-TYP-ID
               MOVE LAPUP-INPUT-DATA          TO WUTTB-UPLD-TTBL-VALU-ID
 
               PERFORM  UTTB-1000-LOOKUP-UTTB
                   THRU UTTB-1000-LOOKUP-UTTB-X
 
APEX54         IF  NOT WUTTB-IO-OK
                   MOVE WAPUP-C-TRAN-CONV-ERR TO LAPUP-RETURN-CD
                   GO TO 5000-PROCESS-TRANS-FIELD-X
               END-IF
 
APEX54         IF  RAPPV-CLI-NERV-SYS-IND NOT = 'Y'
                   MOVE RUTTB-UPLD-TTBL-VALU-TXT
                                          TO RAPPV-CLI-NERV-SYS-IND
                   MOVE WAPUP-C-YES       TO LAPUP-REC-CHANGED-SW
               END-IF
 
               GO TO 5000-PROCESS-TRANS-FIELD-X
 
           END-IF.
 
           IF  RUFLD-UPLD-FLD-NM = 'CLI_RESPTY_IND'
 
               MOVE RUFLD-UPLD-TTBL-TYP-ID    TO WUTTB-UPLD-TTBL-TYP-ID
               MOVE LAPUP-INPUT-DATA          TO WUTTB-UPLD-TTBL-VALU-ID
 
               PERFORM  UTTB-1000-LOOKUP-UTTB
                   THRU UTTB-1000-LOOKUP-UTTB-X
 
APEX54         IF  NOT WUTTB-IO-OK
                   MOVE WAPUP-C-TRAN-CONV-ERR TO LAPUP-RETURN-CD
                   GO TO 5000-PROCESS-TRANS-FIELD-X
               END-IF
 
APEX54         IF  RAPPV-CLI-RESPTY-IND NOT = 'Y'
                   MOVE RUTTB-UPLD-TTBL-VALU-TXT
                                          TO RAPPV-CLI-RESPTY-IND
                   MOVE WAPUP-C-YES       TO LAPUP-REC-CHANGED-SW
               END-IF
 
               GO TO 5000-PROCESS-TRANS-FIELD-X
 
           END-IF.
 
           IF  RUFLD-UPLD-FLD-NM = 'CLI_SMK_CIG_IND'
 
               MOVE RUFLD-UPLD-TTBL-TYP-ID    TO WUTTB-UPLD-TTBL-TYP-ID
               MOVE LAPUP-INPUT-DATA          TO WUTTB-UPLD-TTBL-VALU-ID
 
               PERFORM  UTTB-1000-LOOKUP-UTTB
                   THRU UTTB-1000-LOOKUP-UTTB-X
 
APEX54         IF  NOT WUTTB-IO-OK
                   MOVE WAPUP-C-TRAN-CONV-ERR TO LAPUP-RETURN-CD
                   GO TO 5000-PROCESS-TRANS-FIELD-X
               END-IF
 
APEX54         IF  RAPPV-CLI-SMK-CIG-IND NOT = 'Y'
                   MOVE RUTTB-UPLD-TTBL-VALU-TXT
                                          TO RAPPV-CLI-SMK-CIG-IND
                   MOVE WAPUP-C-YES       TO LAPUP-REC-CHANGED-SW
               END-IF
 
               GO TO 5000-PROCESS-TRANS-FIELD-X
 
           END-IF.
 
           IF  RUFLD-UPLD-FLD-NM = 'CNSLT_DOCTOR_IND'
 
               MOVE RUFLD-UPLD-TTBL-TYP-ID    TO WUTTB-UPLD-TTBL-TYP-ID
               MOVE LAPUP-INPUT-DATA          TO WUTTB-UPLD-TTBL-VALU-ID
 
               PERFORM  UTTB-1000-LOOKUP-UTTB
                   THRU UTTB-1000-LOOKUP-UTTB-X
 
APEX54         IF  NOT WUTTB-IO-OK
                   MOVE WAPUP-C-TRAN-CONV-ERR TO LAPUP-RETURN-CD
                   GO TO 5000-PROCESS-TRANS-FIELD-X
               END-IF
 
APEX54         IF  RAPPV-CNSLT-DOCTOR-IND NOT = 'Y'
                   MOVE RUTTB-UPLD-TTBL-VALU-TXT
                                          TO RAPPV-CNSLT-DOCTOR-IND
                   MOVE WAPUP-C-YES       TO LAPUP-REC-CHANGED-SW
               ELSE
 
                   GO TO 5000-PROCESS-TRANS-FIELD-X
557660         END-IF
 
           END-IF.
 
           IF  RUFLD-UPLD-FLD-NM = 'GLAND_BLOOD_IND'
 
               MOVE RUFLD-UPLD-TTBL-TYP-ID    TO WUTTB-UPLD-TTBL-TYP-ID
               MOVE LAPUP-INPUT-DATA          TO WUTTB-UPLD-TTBL-VALU-ID
 
               PERFORM  UTTB-1000-LOOKUP-UTTB
                   THRU UTTB-1000-LOOKUP-UTTB-X
 
APEX54         IF  NOT WUTTB-IO-OK
                   MOVE WAPUP-C-TRAN-CONV-ERR TO LAPUP-RETURN-CD
                   GO TO 5000-PROCESS-TRANS-FIELD-X
               END-IF
 
APEX54         IF  RAPPV-GLAND-BLOOD-IND NOT = 'Y'
                   MOVE RUTTB-UPLD-TTBL-VALU-TXT
                                              TO RAPPV-GLAND-BLOOD-IND
                   MOVE WAPUP-C-YES           TO LAPUP-REC-CHANGED-SW
 
               END-IF
 
               GO TO 5000-PROCESS-TRANS-FIELD-X
 
           END-IF.
 
           IF  RUFLD-UPLD-FLD-NM = 'HZRD_DRV_OFFNS_IND'
 
               MOVE RUFLD-UPLD-TTBL-TYP-ID    TO WUTTB-UPLD-TTBL-TYP-ID
               MOVE LAPUP-INPUT-DATA          TO WUTTB-UPLD-TTBL-VALU-ID
 
               PERFORM  UTTB-1000-LOOKUP-UTTB
                   THRU UTTB-1000-LOOKUP-UTTB-X
 
APEX54         IF  NOT WUTTB-IO-OK
                   MOVE WAPUP-C-TRAN-CONV-ERR TO LAPUP-RETURN-CD
                   GO TO 5000-PROCESS-TRANS-FIELD-X
               END-IF
 
APEX54         IF  RAPPV-HZRD-DRV-OFFNS-IND NOT = 'Y'
                   MOVE RUTTB-UPLD-TTBL-VALU-TXT
                                        TO RAPPV-HZRD-DRV-OFFNS-IND
                   MOVE WAPUP-C-YES     TO LAPUP-REC-CHANGED-SW
               END-IF
 
               GO TO 5000-PROCESS-TRANS-FIELD-X
 
           END-IF.
 
           IF  RUFLD-UPLD-FLD-NM = 'MUSCL_SKEL_SYS_IND'
 
               MOVE RUFLD-UPLD-TTBL-TYP-ID    TO WUTTB-UPLD-TTBL-TYP-ID
               MOVE LAPUP-INPUT-DATA          TO WUTTB-UPLD-TTBL-VALU-ID
 
               PERFORM  UTTB-1000-LOOKUP-UTTB
                   THRU UTTB-1000-LOOKUP-UTTB-X
 
APEX54         IF  NOT WUTTB-IO-OK
                   MOVE WAPUP-C-TRAN-CONV-ERR TO LAPUP-RETURN-CD
                   GO TO 5000-PROCESS-TRANS-FIELD-X
               END-IF
 
APEX54         IF  RAPPV-MUSCL-SKEL-SYS-IND NOT = 'Y'
                   MOVE RUTTB-UPLD-TTBL-VALU-TXT
                                            TO RAPPV-MUSCL-SKEL-SYS-IND
                   MOVE WAPUP-C-YES         TO LAPUP-REC-CHANGED-SW
               END-IF
 
               GO TO 5000-PROCESS-TRANS-FIELD-X
 
           END-IF.
 
           IF  RUFLD-UPLD-FLD-NM = 'NPRSCB_DRUG_IND'
 
               MOVE RUFLD-UPLD-TTBL-TYP-ID    TO WUTTB-UPLD-TTBL-TYP-ID
               MOVE LAPUP-INPUT-DATA          TO WUTTB-UPLD-TTBL-VALU-ID
 
               PERFORM  UTTB-1000-LOOKUP-UTTB
                   THRU UTTB-1000-LOOKUP-UTTB-X
 
APEX54         IF  NOT WUTTB-IO-OK
                   MOVE WAPUP-C-TRAN-CONV-ERR TO LAPUP-RETURN-CD
                   GO TO 5000-PROCESS-TRANS-FIELD-X
               END-IF
 
APEX54         IF  RAPPV-NPRSCB-DRUG-IND NOT = 'Y'
                   MOVE RUTTB-UPLD-TTBL-VALU-TXT
                                              TO RAPPV-NPRSCB-DRUG-IND
                   MOVE WAPUP-C-YES           TO LAPUP-REC-CHANGED-SW
               END-IF
 
               GO TO 5000-PROCESS-TRANS-FIELD-X
 
           END-IF.
 
           IF  RUFLD-UPLD-FLD-NM = 'OCCP_CHNG_TRAV_IND'
 
               MOVE RUFLD-UPLD-TTBL-TYP-ID    TO WUTTB-UPLD-TTBL-TYP-ID
               MOVE LAPUP-INPUT-DATA          TO WUTTB-UPLD-TTBL-VALU-ID
 
               PERFORM  UTTB-1000-LOOKUP-UTTB
                   THRU UTTB-1000-LOOKUP-UTTB-X
 
APEX54         IF  NOT WUTTB-IO-OK
                   MOVE WAPUP-C-TRAN-CONV-ERR TO LAPUP-RETURN-CD
                   GO TO 5000-PROCESS-TRANS-FIELD-X
               END-IF
 
APEX54         IF  RAPPV-OCCP-CHNG-TRAV-IND NOT = 'Y'
                   MOVE RUTTB-UPLD-TTBL-VALU-TXT
                                            TO RAPPV-OCCP-CHNG-TRAV-IND
                   MOVE WAPUP-C-YES         TO LAPUP-REC-CHANGED-SW
               END-IF
 
               GO TO 5000-PROCESS-TRANS-FIELD-X
 
           END-IF.
 
           IF  RUFLD-UPLD-FLD-NM = 'TUMR_CANCER_IND'
 
               MOVE RUFLD-UPLD-TTBL-TYP-ID    TO WUTTB-UPLD-TTBL-TYP-ID
               MOVE LAPUP-INPUT-DATA          TO WUTTB-UPLD-TTBL-VALU-ID
 
               PERFORM  UTTB-1000-LOOKUP-UTTB
                   THRU UTTB-1000-LOOKUP-UTTB-X
 
APEX54         IF  NOT WUTTB-IO-OK
                   MOVE WAPUP-C-TRAN-CONV-ERR TO LAPUP-RETURN-CD
                   GO TO 5000-PROCESS-TRANS-FIELD-X
               END-IF
 
APEX54         IF  RAPPV-TUMR-CANCER-IND NOT = 'Y'
                   MOVE RUTTB-UPLD-TTBL-VALU-TXT
                                              TO RAPPV-TUMR-CANCER-IND
                   MOVE WAPUP-C-YES           TO LAPUP-REC-CHANGED-SW
               END-IF
 
               GO TO 5000-PROCESS-TRANS-FIELD-X
 
           END-IF.
 
           IF  RUFLD-UPLD-FLD-NM = 'URIN_REPRO_SYS_IND'
 
               MOVE RUFLD-UPLD-TTBL-TYP-ID    TO WUTTB-UPLD-TTBL-TYP-ID
               MOVE LAPUP-INPUT-DATA          TO WUTTB-UPLD-TTBL-VALU-ID
 
               PERFORM  UTTB-1000-LOOKUP-UTTB
                   THRU UTTB-1000-LOOKUP-UTTB-X
 
APEX54         IF  NOT WUTTB-IO-OK
                   MOVE WAPUP-C-TRAN-CONV-ERR TO LAPUP-RETURN-CD
                   GO TO 5000-PROCESS-TRANS-FIELD-X
               END-IF
 
APEX54         IF  RAPPV-URIN-REPRO-SYS-IND NOT = 'Y'
                   MOVE RUTTB-UPLD-TTBL-VALU-TXT
                                            TO RAPPV-URIN-REPRO-SYS-IND
                   MOVE WAPUP-C-YES         TO LAPUP-REC-CHANGED-SW
               END-IF
 
               GO TO 5000-PROCESS-TRANS-FIELD-X
 
           END-IF.
 
APEX52     IF  RUFLD-UPLD-FLD-NM = 'DRUG_TRTMNT_IND'
 
APEX52         MOVE RUFLD-UPLD-TTBL-TYP-ID    TO WUTTB-UPLD-TTBL-TYP-ID
APEX52         MOVE LAPUP-INPUT-DATA          TO WUTTB-UPLD-TTBL-VALU-ID
 
APEX52         PERFORM  UTTB-1000-LOOKUP-UTTB
APEX52             THRU UTTB-1000-LOOKUP-UTTB-X
 
APEX54         IF  NOT WUTTB-IO-OK
APEX52             MOVE WAPUP-C-TRAN-CONV-ERR TO LAPUP-RETURN-CD
APEX52             GO TO 5000-PROCESS-TRANS-FIELD-X
               END-IF
 
APEX54         IF  RAPPV-DRUG-TRTMNT-IND NOT = 'Y'
APEX52             MOVE RUTTB-UPLD-TTBL-VALU-TXT
APEX52                                        TO RAPPV-DRUG-TRTMNT-IND
APEX52             MOVE WAPUP-C-YES           TO LAPUP-REC-CHANGED-SW
               END-IF
 
APEX52         GO TO 5000-PROCESS-TRANS-FIELD-X
 
           END-IF.
 
           MOVE WAPUP-C-UNKNOWN-FIELD-ERR     TO LAPUP-RETURN-CD.
 
       5000-PROCESS-TRANS-FIELD-X.
           EXIT.
      /
      *---------------------------
       6000-PROCESS-COMPLEX-FIELD.
      *---------------------------
 
           IF  RUFLD-UPLD-FLD-NM = 'AVG_HRD_LIQR_QTY'
               MOVE 3                      TO L0280-LENGTH
               MOVE 0                      TO L0280-PRECISION
               MOVE LAPUP-INPUT-DATA       TO L0280-INPUT-DATA
               PERFORM  0280-1000-NUMERIC-EDIT
                   THRU 0280-1000-NUMERIC-EDIT-X
               IF  L0280-OK
                   PERFORM  6100-PROCESS-LIQ
                       THRU 6100-PROCESS-LIQ-X
               ELSE
                   MOVE WAPUP-C-NUM-CONV-ERR TO LAPUP-RETURN-CD
                   MOVE L0280-STATUS         TO LAPUP-SUB-RETURN-CD
                   GO TO 6000-PROCESS-COMPLEX-FIELD-X
               END-IF
           END-IF.
 
           IF  RUFLD-UPLD-FLD-NM = 'CLI_AVG_BEER_QTY'
               MOVE 3                      TO L0280-LENGTH
               MOVE 0                      TO L0280-PRECISION
               MOVE LAPUP-INPUT-DATA       TO L0280-INPUT-DATA
               PERFORM  0280-1000-NUMERIC-EDIT
                   THRU 0280-1000-NUMERIC-EDIT-X
               IF  L0280-OK
                   PERFORM  6200-PROCESS-BEER
                       THRU 6200-PROCESS-BEER-X
               ELSE
                   MOVE WAPUP-C-NUM-CONV-ERR TO LAPUP-RETURN-CD
                   MOVE L0280-STATUS         TO LAPUP-SUB-RETURN-CD
                   GO TO 6000-PROCESS-COMPLEX-FIELD-X
               END-IF
           END-IF.
 
           IF  RUFLD-UPLD-FLD-NM = 'CLI_AVG_WINE_QTY'
               MOVE 3                      TO L0280-LENGTH
               MOVE 0                      TO L0280-PRECISION
               MOVE LAPUP-INPUT-DATA       TO L0280-INPUT-DATA
               PERFORM  0280-1000-NUMERIC-EDIT
                   THRU 0280-1000-NUMERIC-EDIT-X
               IF  L0280-OK
                   PERFORM  6300-PROCESS-WINE
                       THRU 6300-PROCESS-WINE-X
               ELSE
                   MOVE WAPUP-C-NUM-CONV-ERR TO LAPUP-RETURN-CD
                   MOVE L0280-STATUS         TO LAPUP-SUB-RETURN-CD
                   GO TO 6000-PROCESS-COMPLEX-FIELD-X
               END-IF
           END-IF.
 
           IF  RUFLD-UPLD-FLD-NM = 'CLI_DRUG_USE_TXT'
           AND RUFLD-UPLD-FLD-STRUCT-NM = 'DRUG'
APEX52     AND LAPUP-INPUT-DATA = '1'
               MOVE RUFLD-UPLD-TTBL-TYP-ID TO WUTTB-UPLD-TTBL-TYP-ID
               MOVE RUFLD-UPLD-FLD-APEX-NM TO WUTTB-UPLD-TTBL-VALU-ID
               PERFORM  UTTB-1000-LOOKUP-UTTB
                   THRU UTTB-1000-LOOKUP-UTTB-X
               IF  WUTTB-IO-OK
                   PERFORM  6400-PROCESS-DRUG
                       THRU 6400-PROCESS-DRUG-X
               ELSE
                   MOVE WAPUP-C-FIELD-CONV-ERR
                                           TO LAPUP-RETURN-CD
APEX52             GO TO 6000-PROCESS-COMPLEX-FIELD-X
               END-IF
           END-IF.
 
 
APEX52     IF  RUFLD-UPLD-FLD-NM = 'CLI_SMK_OTHR_QTY'
APEX52         MOVE 3                      TO L0280-LENGTH
APEX52         MOVE 0                      TO L0280-PRECISION
APEX52         MOVE LAPUP-INPUT-DATA       TO L0280-INPUT-DATA
APEX52         PERFORM  0280-1000-NUMERIC-EDIT
APEX52             THRU 0280-1000-NUMERIC-EDIT-X
APEX52         IF  L0280-OK
APEX52             MOVE L0280-OUTPUT       TO RAPPV-CLI-SMK-OTHR-QTY
APEX52             MOVE WAPUP-C-YES        TO LAPUP-REC-CHANGED-SW
APEX52             GO TO 6000-PROCESS-COMPLEX-FIELD-X
APEX52         ELSE
APEX52             MOVE WAPUP-C-NUM-CONV-ERR TO LAPUP-RETURN-CD
APEX52             MOVE L0280-STATUS         TO LAPUP-SUB-RETURN-CD
APEX52             GO TO 6000-PROCESS-COMPLEX-FIELD-X
               END-IF
           END-IF.
 
           IF  RUFLD-UPLD-FLD-NM = 'HZRD_DRV_OFFNS_DT'
               MOVE LAPUP-INPUT-DT-YR      TO WAPUP-CONV-DT-YR
               MOVE LAPUP-INPUT-DT-MON     TO WAPUP-CONV-DT-MON
               MOVE LAPUP-INPUT-DT-DAY     TO WAPUP-CONV-DT-DAY
               IF  WAPUP-CONV-DT > RAPPV-HZRD-DRV-OFFNS-DT
                   MOVE WAPUP-CONV-DT      TO RAPPV-HZRD-DRV-OFFNS-DT
                   MOVE WAPUP-C-YES        TO LAPUP-REC-CHANGED-SW
                   GO TO 6000-PROCESS-COMPLEX-FIELD-X
               ELSE
                   GO TO 6000-PROCESS-COMPLEX-FIELD-X
               END-IF
           END-IF.
 
           IF  RUFLD-UPLD-FLD-NM = 'HZRD_DRV_OFFNS_TXT'
           AND RUFLD-UPLD-FLD-STRUCT-NM = 'DRIV'
APEX52     AND LAPUP-INPUT-DATA = '1'
               MOVE RUFLD-UPLD-TTBL-TYP-ID TO WUTTB-UPLD-TTBL-TYP-ID
               MOVE RUFLD-UPLD-FLD-APEX-NM TO WUTTB-UPLD-TTBL-VALU-ID
               PERFORM  UTTB-1000-LOOKUP-UTTB
                   THRU UTTB-1000-LOOKUP-UTTB-X
               IF  WUTTB-IO-OK
                   PERFORM  6700-PROCESS-HZRD-DRIV
                       THRU 6700-PROCESS-HZRD-DRIV-X
               ELSE
                   MOVE WAPUP-C-FIELD-CONV-ERR
                                           TO LAPUP-RETURN-CD
APEX52             GO TO 6000-PROCESS-COMPLEX-FIELD-X
               END-IF
           END-IF.
 
      *
APEX52* THIS TEST WILL SET THE SMOKE INDICATORS FOR CIG AND OTHR TO
APEX52* 'N'.  IF THE SMOKE_CURR FIELD IN THE LIFESTYLE SED IS SET TO
APEX52* '1' THEN THE SED TYPESMOKE WILL BE GENERATED AND FROM THAT SED
APEX52* WE WILL DECIDE WHICH INDICATOR WILL BE 'Y'
      *
APEX52     IF  RUFLD-UPLD-FLD-STRUCT-NM = 'LIFESTYLE'
APEX52     AND RUFLD-UPLD-FLD-APEX-NM = 'SMOKE_CURR'
APEX52         MOVE 'N'                    TO RAPPV-CLI-SMK-CIG-IND
APEX52         MOVE 'N'                    TO RAPPV-CLI-SMK-OTHR-IND
APEX52         GO TO 6000-PROCESS-COMPLEX-FIELD-X
           END-IF.
 
APEX52     IF  RUFLD-UPLD-FLD-STRUCT-NM = 'TYPESMOKE'
APEX52     AND RUFLD-UPLD-FLD-NM = 'CLI_SMK_OTHR_IND'
APEX52         MOVE RUFLD-UPLD-FLD-APEX-NM TO RAPPV-CLI-SMK-OTHR-TXT
APEX52         MOVE RUFLD-UPLD-TTBL-TYP-ID TO WUTTB-UPLD-TTBL-TYP-ID
APEX52         MOVE LAPUP-INPUT-DATA       TO WUTTB-UPLD-TTBL-VALU-ID
APEX52         PERFORM  UTTB-1000-LOOKUP-UTTB
APEX52             THRU UTTB-1000-LOOKUP-UTTB-X
APEX52         IF  WUTTB-IO-OK
                   PERFORM  6800-PROCESS-TYPESMOKE
                       THRU 6800-PROCESS-TYPESMOKE-X
               ELSE
                   MOVE WAPUP-C-FIELD-CONV-ERR
                                           TO LAPUP-RETURN-CD
APEX52             GO TO 6000-PROCESS-COMPLEX-FIELD-X
               END-IF
           END-IF.
 
           MOVE WAPUP-C-UNKNOWN-FIELD-ERR  TO LAPUP-RETURN-CD.
 
       6000-PROCESS-COMPLEX-FIELD-X.
           EXIT.
      /
      *-----------------
       6100-PROCESS-LIQ.
      *-----------------
 
           EVALUATE TRUE
 
               WHEN RUFLD-UPLD-FLD-APEX-NM = 'LIQ_D'
APEX52              COMPUTE RAPPV-AVG-HRD-LIQR-QTY
APEX52                    = RAPPV-AVG-HRD-LIQR-QTY
APEX52                    + (L0280-OUTPUT
APEX52                    * 7)
                    MOVE WAPUP-C-YES       TO LAPUP-REC-CHANGED-SW
 
               WHEN RUFLD-UPLD-FLD-APEX-NM = 'LIQ_W'
APEX52              ADD  L0280-OUTPUT      TO RAPPV-AVG-HRD-LIQR-QTY
                    MOVE WAPUP-C-YES       TO LAPUP-REC-CHANGED-SW
 
               WHEN RUFLD-UPLD-FLD-APEX-NM = 'LIQ_M'
APEX52              COMPUTE RAPPV-AVG-HRD-LIQR-QTY
APEX52                    = RAPPV-AVG-HRD-LIQR-QTY
APEX52                    + (L0280-OUTPUT
APEX52                    / 4)
                    MOVE WAPUP-C-YES       TO LAPUP-REC-CHANGED-SW
 
               WHEN OTHER
                    MOVE WAPUP-C-UNKNOWN-FIELD-ERR
                                           TO LAPUP-RETURN-CD
 
           END-EVALUATE.
 
       6100-PROCESS-LIQ-X.
           EXIT.
      /
      *------------------
       6200-PROCESS-BEER.
      *------------------
 
           EVALUATE TRUE
 
               WHEN RUFLD-UPLD-FLD-APEX-NM = 'BEER_D'
APEX52              COMPUTE RAPPV-CLI-AVG-BEER-QTY
APEX52                    = RAPPV-CLI-AVG-BEER-QTY
APEX52                    + (L0280-OUTPUT
APEX52                    * 7)
                    MOVE WAPUP-C-YES       TO LAPUP-REC-CHANGED-SW
 
               WHEN RUFLD-UPLD-FLD-APEX-NM = 'BEER_W'
APEX52              ADD  L0280-OUTPUT      TO RAPPV-CLI-AVG-BEER-QTY
                    MOVE WAPUP-C-YES       TO LAPUP-REC-CHANGED-SW
 
               WHEN RUFLD-UPLD-FLD-APEX-NM = 'BEER_M'
APEX52              COMPUTE RAPPV-CLI-AVG-BEER-QTY
APEX52                    = RAPPV-CLI-AVG-BEER-QTY
APEX52                    + (L0280-OUTPUT
APEX52                    / 4)
                    MOVE WAPUP-C-YES       TO LAPUP-REC-CHANGED-SW
 
               WHEN OTHER
                    MOVE WAPUP-C-UNKNOWN-FIELD-ERR
                                           TO LAPUP-RETURN-CD
 
           END-EVALUATE.
 
       6200-PROCESS-BEER-X.
           EXIT.
      /
      *------------------
       6300-PROCESS-WINE.
      *------------------
 
           EVALUATE TRUE
 
               WHEN RUFLD-UPLD-FLD-APEX-NM = 'WINE_D'
APEX52              COMPUTE RAPPV-CLI-AVG-WINE-QTY
APEX52                    = RAPPV-CLI-AVG-WINE-QTY
APEX52                    + (L0280-OUTPUT
APEX52                    * 7)
                    MOVE WAPUP-C-YES       TO LAPUP-REC-CHANGED-SW
 
               WHEN RUFLD-UPLD-FLD-APEX-NM = 'WINE_W'
APEX52              ADD  L0280-OUTPUT      TO RAPPV-CLI-AVG-WINE-QTY
                    MOVE WAPUP-C-YES       TO LAPUP-REC-CHANGED-SW
 
               WHEN RUFLD-UPLD-FLD-APEX-NM = 'WINE_M'
APEX52              COMPUTE RAPPV-CLI-AVG-WINE-QTY
APEX52                    = RAPPV-CLI-AVG-WINE-QTY
APEX52                    + (L0280-OUTPUT
APEX52                    / 4)
                    MOVE WAPUP-C-YES       TO LAPUP-REC-CHANGED-SW
 
               WHEN OTHER
                    MOVE WAPUP-C-UNKNOWN-FIELD-ERR
                                           TO LAPUP-RETURN-CD
 
           END-EVALUATE.
 
       6300-PROCESS-WINE-X.
           EXIT.
      /
      *------------------
       6400-PROCESS-DRUG.
      *------------------
 
           IF  RAPPV-CLI-DRUG-USE-TXT      =  SPACES
               MOVE RUTTB-UPLD-TTBL-VALU-TXT
                                           TO RAPPV-CLI-DRUG-USE-TXT
               MOVE WAPUP-C-YES            TO LAPUP-REC-CHANGED-SW
           ELSE
               MOVE WAPUP-C-MULT-VALUES-ERR
                                           TO LAPUP-RETURN-CD
           END-IF.
 
       6400-PROCESS-DRUG-X.
           EXIT.
      /
      *-----------------------
       6700-PROCESS-HZRD-DRIV.
      *-----------------------
 
           IF  RAPPV-HZRD-DRV-OFFNS-TXT = SPACES
               MOVE RUTTB-UPLD-TTBL-VALU-TXT
                                           TO RAPPV-HZRD-DRV-OFFNS-TXT
               MOVE WAPUP-C-YES            TO LAPUP-REC-CHANGED-SW
           ELSE
               MOVE WAPUP-C-MULT-VALUES-ERR
                                           TO LAPUP-RETURN-CD
           END-IF.
 
       6700-PROCESS-HZRD-DRIV-X.
           EXIT.
      /
      *-----------------------
       6800-PROCESS-TYPESMOKE.
      *-----------------------
 
APEX52     IF  RAPPV-CLI-SMK-OTHR-IND  NOT = 'Y'
APEX52         MOVE RUTTB-UPLD-TTBL-VALU-TXT
APEX52                                     TO RAPPV-CLI-SMK-OTHR-IND
APEX52         MOVE WAPUP-C-YES            TO LAPUP-REC-CHANGED-SW
APEX52     END-IF.
 
       6800-PROCESS-TYPESMOKE-X.
           EXIT.
      /
      *-------------------------
       7000-PROCESS-FIELD-FIELD.
      *-------------------------
 
           IF  LAPUP-INPUT-DATA NOT = '1'
               GO TO 7000-PROCESS-FIELD-FIELD-X
           END-IF.
 
           IF  RUFLD-UPLD-FLD-NM = 'CLI_TRAV_DUR_TXT'
               MOVE RUFLD-UPLD-TTBL-TYP-ID TO WUTTB-UPLD-TTBL-TYP-ID
               MOVE RUFLD-UPLD-FLD-APEX-NM TO WUTTB-UPLD-TTBL-VALU-ID
               PERFORM  UTTB-1000-LOOKUP-UTTB
                   THRU UTTB-1000-LOOKUP-UTTB-X
               IF  WUTTB-IO-OK
                   MOVE RUTTB-UPLD-TTBL-VALU-TXT
                                           TO RAPPV-CLI-TRAV-DUR-TXT
                   MOVE WAPUP-C-YES        TO LAPUP-REC-CHANGED-SW
                   GO TO 7000-PROCESS-FIELD-FIELD-X
               ELSE
                   MOVE WAPUP-C-FIELD-CONV-ERR
                                           TO LAPUP-RETURN-CD
                   GO TO 7000-PROCESS-FIELD-FIELD-X
               END-IF
           END-IF.
 
           IF  RUFLD-UPLD-FLD-NM = 'HZRD_SPORT_DTL_TXT'
               MOVE RUFLD-UPLD-TTBL-TYP-ID TO WUTTB-UPLD-TTBL-TYP-ID
               MOVE RUFLD-UPLD-FLD-APEX-NM TO WUTTB-UPLD-TTBL-VALU-ID
               PERFORM  UTTB-1000-LOOKUP-UTTB
                   THRU UTTB-1000-LOOKUP-UTTB-X
               IF  WUTTB-IO-OK
                   MOVE RUTTB-UPLD-TTBL-VALU-TXT
                                           TO RAPPV-HZRD-SPORT-DTL-TXT
                   MOVE WAPUP-C-YES        TO LAPUP-REC-CHANGED-SW
                   GO TO 7000-PROCESS-FIELD-FIELD-X
               ELSE
                   MOVE WAPUP-C-FIELD-CONV-ERR
                                           TO LAPUP-RETURN-CD
                   GO TO 7000-PROCESS-FIELD-FIELD-X
               END-IF
           END-IF.
 
           IF  RUFLD-UPLD-FLD-NM = 'OCCP_CHNG_TRAV_TXT'
               MOVE RUFLD-UPLD-TTBL-TYP-ID TO WUTTB-UPLD-TTBL-TYP-ID
               MOVE RUFLD-UPLD-FLD-APEX-NM TO WUTTB-UPLD-TTBL-VALU-ID
               PERFORM  UTTB-1000-LOOKUP-UTTB
                   THRU UTTB-1000-LOOKUP-UTTB-X
               IF  WUTTB-IO-OK
                   MOVE RUTTB-UPLD-TTBL-VALU-TXT
                                           TO RAPPV-OCCP-CHNG-TRAV-TXT
                   MOVE WAPUP-C-YES        TO LAPUP-REC-CHANGED-SW
                   GO TO 7000-PROCESS-FIELD-FIELD-X
               ELSE
                   MOVE WAPUP-C-FIELD-CONV-ERR
                                           TO LAPUP-RETURN-CD
                   GO TO 7000-PROCESS-FIELD-FIELD-X
               END-IF
           END-IF.
 
           MOVE WAPUP-C-UNKNOWN-FIELD-ERR  TO LAPUP-RETURN-CD.
 
       7000-PROCESS-FIELD-FIELD-X.
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
      **                 END OF PROGRAM ASRUAPPV                     **
      *****************************************************************
