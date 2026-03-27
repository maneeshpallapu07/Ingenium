      *************************
       IDENTIFICATION DIVISION.
      *************************
 
       PROGRAM-ID.  ASRUAPPF.
 
       COPY XCWWCRHT.
 
      *****************************************************************
      **  MEMBER :  ASRUAPPF                                         **
      **  REMARKS:  APEX UPLOAD APPF TABLE PROCESSING                **
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
557700**  30SEP97  AMA    APEX UPLOAD                                **
014590**  15DEC99  60     ARCHITECTURAL CHANGES                      **
      *****************************************************************
      /
      **********************
       ENVIRONMENT DIVISION.
      **********************
 
      ***************
       DATA DIVISION.
      ***************
 
       WORKING-STORAGE SECTION.
 
APEX53 COPY XCWWPGWS REPLACING '$VAR1' BY 'ASRUAPPF'.
 
       COPY SQLCA.
 
014590*COPY XCWL0030.
      /
       01  WS-COUNTERS.
           05  WS-SUB                       PIC S9(04) COMP.
 
       01  WS-SWITCHES.
           05  WS-BYPASS-FIELD-SW           PIC X(01).
               88  WS-BYPASS-FIELD          VALUE 'Y'.
      /
      *****************************************************************
      *  COMMON COPYBOOKS                                             *
      *****************************************************************
       COPY ACWWAPUP.
      /
       COPY XCWTFCMD.
      /
       COPY XCWWWKDT.
      /
      *****************************************************************
      *  I/O COPYBOOKS                                                *
      *****************************************************************
       COPY ACFWUTTB.
      /
       COPY ACFRUTTB.
      /
      *****************************************************************
      *  CALLED MODULE PARAMETER INFORMATION                          *
      *****************************************************************
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
 
       COPY NCFRAPPF.
      /
       PROCEDURE DIVISION  USING  WGLOB-GLOBAL-AREA
                                  LAPUP-PARM-AREA
                                  RUFLD-REC-INFO
                                  RAPPF-REC-INFO.
 
      *--------------
       0000-MAINLINE.
      *--------------
 
           PERFORM  1000-INITIALIZE
               THRU 1000-INITIALIZE-X.
 
           MOVE WAPUP-C-NO                 TO WS-BYPASS-FIELD-SW.
 
           IF  WS-BYPASS-FIELD
APEX54         GO TO 0000-MAINLINE-X
APEX54     END-IF.
 
APEX54     EVALUATE TRUE
 
557700*        WHEN RUFLD-UPLD-FLD-TYP-CHAR
557700         WHEN RUFLD-UPLD-FLD-TYP-UPPER-CASE
557700         WHEN RUFLD-UPLD-FLD-TYP-MIX-CASE
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
                    CONTINUE
 
APEX54         WHEN OTHER
                    MOVE WAPUP-C-UNKNOWN-FIELD-TYPE TO LAPUP-RETURN-CD
 
APEX54     END-EVALUATE.
 
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
 
APEX54     EVALUATE  RUFLD-UPLD-FLD-NM
 
               WHEN 'CLI_DOCTOR_NM'
                    MOVE LAPUP-INPUT-DATA   TO RAPPF-CLI-DOCTOR-NM
                    MOVE WAPUP-C-YES        TO LAPUP-REC-CHANGED-SW
 
               WHEN 'CLI_ID'
                    MOVE LAPUP-INPUT-DATA   TO RAPPF-CLI-ID
                    MOVE WAPUP-C-YES        TO LAPUP-REC-CHANGED-SW
 
               WHEN 'CLI_MEDCTN_GIV_TXT'
                    MOVE LAPUP-INPUT-DATA   TO RAPPF-CLI-MEDCTN-GIV-TXT
                    MOVE WAPUP-C-YES        TO LAPUP-REC-CHANGED-SW
 
               WHEN 'DISORD_DIAGNS_TXT'
                    MOVE LAPUP-INPUT-DATA   TO RAPPF-DISORD-DIAGNS-TXT
                    MOVE WAPUP-C-YES        TO LAPUP-REC-CHANGED-SW
 
               WHEN 'DI_REFUS_CO_1_IND'
                    MOVE LAPUP-INPUT-DATA   TO RAPPF-DI-REFUS-CO-1-IND
                    MOVE WAPUP-C-YES        TO LAPUP-REC-CHANGED-SW
 
               WHEN 'DI_REFUS_CO_2_IND'
                    MOVE LAPUP-INPUT-DATA   TO RAPPF-DI-REFUS-CO-2-IND
                    MOVE WAPUP-C-YES        TO LAPUP-REC-CHANGED-SW
 
               WHEN 'DOCTOR_CITY_NM_TXT'
                    MOVE LAPUP-INPUT-DATA   TO RAPPF-DOCTOR-CITY-NM-TXT
                    MOVE WAPUP-C-YES        TO LAPUP-REC-CHANGED-SW
 
               WHEN 'HIST_UNKNWN_IND'
                    MOVE LAPUP-INPUT-DATA   TO RAPPF-HIST-UNKNWN-IND
                    MOVE WAPUP-C-YES        TO LAPUP-REC-CHANGED-SW
 
               WHEN 'PREV_VST_REASN_CD'
                    MOVE LAPUP-INPUT-DATA   TO RAPPF-PREV-VST-REASN-CD
                    MOVE WAPUP-C-YES        TO LAPUP-REC-CHANGED-SW
 
               WHEN 'PREV_VST_RSLT_TXT'
                    MOVE LAPUP-INPUT-DATA   TO RAPPF-PREV-VST-RSLT-TXT
                    MOVE WAPUP-C-YES        TO LAPUP-REC-CHANGED-SW
 
               WHEN 'REGU_DOCTOR_IND'
                    MOVE LAPUP-INPUT-DATA   TO RAPPF-REGU-DOCTOR-IND
                    MOVE WAPUP-C-YES        TO LAPUP-REC-CHANGED-SW
 
               WHEN 'TRTMNT_RECV_TXT'
                    MOVE LAPUP-INPUT-DATA   TO RAPPF-TRTMNT-RECV-TXT
                    MOVE WAPUP-C-YES        TO LAPUP-REC-CHANGED-SW
 
557659*        WHEN 'WGT_GAIN_LOS_IND'
557659         WHEN 'WGT_GAIN_LOS_CD'
557659*             MOVE LAPUP-INPUT-DATA   TO RAPPF-WGT-GAIN-LOS-IND
557659              MOVE LAPUP-INPUT-DATA   TO RAPPF-WGT-GAIN-LOS-CD
                    MOVE WAPUP-C-YES        TO LAPUP-REC-CHANGED-SW
 
               WHEN OTHER
                    MOVE WAPUP-C-UNKNOWN-FIELD-ERR
                                       TO LAPUP-RETURN-CD
 
APEX54     END-EVALUATE.
 
       2000-PROCESS-CHAR-FIELD-X.
           EXIT.
      /
      *------------------------
       3000-PROCESS-DATE-FIELD.
      *------------------------
 
APEX54     EVALUATE  RUFLD-UPLD-FLD-NM
 
APEX54         WHEN 'CLI_PREG_CNFMT_DT'
                    MOVE LAPUP-INPUT-DT-YR  TO WAPUP-CONV-DT-YR
                    MOVE LAPUP-INPUT-DT-MON TO WAPUP-CONV-DT-MON
                    MOVE LAPUP-INPUT-DT-DAY TO WAPUP-CONV-DT-DAY
                    MOVE WAPUP-CONV-DT      TO RAPPF-CLI-PREG-CNFMT-DT
                    MOVE WAPUP-C-YES        TO LAPUP-REC-CHANGED-SW
 
APEX54         WHEN 'DOCTOR_PREV_VST_DT'
                    MOVE LAPUP-INPUT-DT-YR  TO WAPUP-CONV-DT-YR
                    MOVE LAPUP-INPUT-DT-MON TO WAPUP-CONV-DT-MON
                    MOVE LAPUP-INPUT-DT-DAY TO WAPUP-CONV-DT-DAY
                    MOVE WAPUP-CONV-DT      TO RAPPF-DOCTOR-PREV-VST-DT
                    MOVE WAPUP-C-YES        TO LAPUP-REC-CHANGED-SW
 
APEX54         WHEN OTHER
                    MOVE WAPUP-C-UNKNOWN-FIELD-ERR
                                            TO LAPUP-RETURN-CD
 
APEX54     END-EVALUATE.
 
       3000-PROCESS-DATE-FIELD-X.
           EXIT.
      /
      *---------------------------
       4000-PROCESS-NUMERIC-FIELD.
      *---------------------------
 
           IF  RUFLD-UPLD-FLD-NM = 'CLI_SPOUS_INS_AMT'
               MOVE 9                           TO L0280-LENGTH
               MOVE 0                           TO L0280-PRECISION
               MOVE LAPUP-INPUT-DATA            TO L0280-INPUT-DATA
               PERFORM  0280-1000-NUMERIC-EDIT
                   THRU 0280-1000-NUMERIC-EDIT-X
               IF  L0280-OK
                   COMPUTE RAPPF-CLI-SPOUS-INS-AMT = L0280-OUTPUT /
                       (10 ** L0280-PRECISION)
                   MOVE WAPUP-C-YES             TO LAPUP-REC-CHANGED-SW
                   GO TO 4000-PROCESS-NUMERIC-FIELD-X
               ELSE
                   MOVE WAPUP-C-NUM-CONV-ERR    TO LAPUP-RETURN-CD
                   MOVE L0280-STATUS            TO LAPUP-SUB-RETURN-CD
                   GO TO 4000-PROCESS-NUMERIC-FIELD-X
               END-IF
           END-IF.
 
           MOVE WAPUP-C-UNKNOWN-FIELD-ERR       TO LAPUP-RETURN-CD.
 
       4000-PROCESS-NUMERIC-FIELD-X.
           EXIT.
      /
      *-------------------------
       5000-PROCESS-TRANS-FIELD.
      *-------------------------
 
           IF  RUFLD-UPLD-FLD-NM = 'CLI_DI_OINS_IND'
               MOVE RUFLD-UPLD-TTBL-TYP-ID  TO WUTTB-UPLD-TTBL-TYP-ID
               MOVE LAPUP-INPUT-DATA        TO WUTTB-UPLD-TTBL-VALU-ID
               PERFORM  UTTB-1000-LOOKUP-UTTB
                   THRU UTTB-1000-LOOKUP-UTTB-X
               IF  WUTTB-IO-OK
                   MOVE RUTTB-UPLD-TTBL-VALU-TXT
                                            TO RAPPF-CLI-DI-OINS-IND
                   MOVE WAPUP-C-YES         TO LAPUP-REC-CHANGED-SW
                   GO TO 5000-PROCESS-TRANS-FIELD-X
               ELSE
                   MOVE WAPUP-C-TRAN-CONV-ERR TO LAPUP-RETURN-CD
                   GO TO 5000-PROCESS-TRANS-FIELD-X
               END-IF
           END-IF.
 
           IF  RUFLD-UPLD-FLD-NM = 'REGU_DOCTOR_IND'
               MOVE RUFLD-UPLD-TTBL-TYP-ID  TO WUTTB-UPLD-TTBL-TYP-ID
               MOVE LAPUP-INPUT-DATA        TO WUTTB-UPLD-TTBL-VALU-ID
               PERFORM  UTTB-1000-LOOKUP-UTTB
                   THRU UTTB-1000-LOOKUP-UTTB-X
               IF  WUTTB-IO-OK
                   MOVE RUTTB-UPLD-TTBL-VALU-TXT
                                            TO RAPPF-REGU-DOCTOR-IND
                   MOVE WAPUP-C-YES         TO LAPUP-REC-CHANGED-SW
                   GO TO 5000-PROCESS-TRANS-FIELD-X
               ELSE
                   MOVE WAPUP-C-TRAN-CONV-ERR TO LAPUP-RETURN-CD
                   GO TO 5000-PROCESS-TRANS-FIELD-X
               END-IF
           END-IF.
 
           IF  RUFLD-UPLD-FLD-NM = 'CLI_DOCTOR_PROV_CD'
               MOVE RUFLD-UPLD-TTBL-TYP-ID  TO WUTTB-UPLD-TTBL-TYP-ID
               MOVE LAPUP-INPUT-DATA        TO WUTTB-UPLD-TTBL-VALU-ID
               PERFORM  UTTB-1000-LOOKUP-UTTB
                   THRU UTTB-1000-LOOKUP-UTTB-X
               IF  WUTTB-IO-OK
                   MOVE RUTTB-UPLD-TTBL-VALU-TXT
                                            TO RAPPF-CLI-DOCTOR-PROV-CD
                   MOVE WAPUP-C-YES         TO LAPUP-REC-CHANGED-SW
                   GO TO 5000-PROCESS-TRANS-FIELD-X
               ELSE
                   MOVE WAPUP-C-TRAN-CONV-ERR TO LAPUP-RETURN-CD
                   GO TO 5000-PROCESS-TRANS-FIELD-X
               END-IF
           END-IF.
 
           IF  RUFLD-UPLD-FLD-NM = 'CLI_INS_REFUS_IND'
               MOVE RUFLD-UPLD-TTBL-TYP-ID  TO WUTTB-UPLD-TTBL-TYP-ID
               MOVE LAPUP-INPUT-DATA        TO WUTTB-UPLD-TTBL-VALU-ID
               PERFORM  UTTB-1000-LOOKUP-UTTB
                   THRU UTTB-1000-LOOKUP-UTTB-X
               IF  WUTTB-IO-OK
                   MOVE RUTTB-UPLD-TTBL-VALU-TXT
                                            TO RAPPF-CLI-INS-REFUS-IND
                   MOVE WAPUP-C-YES         TO LAPUP-REC-CHANGED-SW
                   GO TO 5000-PROCESS-TRANS-FIELD-X
               ELSE
                   MOVE WAPUP-C-TRAN-CONV-ERR TO LAPUP-RETURN-CD
                   GO TO 5000-PROCESS-TRANS-FIELD-X
               END-IF
           END-IF.
 
557659*    IF  RUFLD-UPLD-FLD-NM = 'CLI_PREG_CMPLIC_CD'
557659     IF  RUFLD-UPLD-FLD-NM = 'PREG_CMPLIC_IND'
               MOVE RUFLD-UPLD-TTBL-TYP-ID  TO WUTTB-UPLD-TTBL-TYP-ID
               MOVE LAPUP-INPUT-DATA        TO WUTTB-UPLD-TTBL-VALU-ID
               PERFORM  UTTB-1000-LOOKUP-UTTB
                   THRU UTTB-1000-LOOKUP-UTTB-X
               IF  WUTTB-IO-OK
                   MOVE RUTTB-UPLD-TTBL-VALU-TXT
557659*                                     TO RAPPF-CLI-PREG-CMPLIC-CD
557659                                      TO RAPPF-PREG-CMPLIC-IND
                   MOVE WAPUP-C-YES         TO LAPUP-REC-CHANGED-SW
                   GO TO 5000-PROCESS-TRANS-FIELD-X
               ELSE
                   MOVE WAPUP-C-TRAN-CONV-ERR TO LAPUP-RETURN-CD
                   GO TO 5000-PROCESS-TRANS-FIELD-X
               END-IF
           END-IF.
 
           IF  RUFLD-UPLD-FLD-NM = 'CLI_PREG_IND'
               MOVE RUFLD-UPLD-TTBL-TYP-ID  TO WUTTB-UPLD-TTBL-TYP-ID
               MOVE LAPUP-INPUT-DATA        TO WUTTB-UPLD-TTBL-VALU-ID
               PERFORM  UTTB-1000-LOOKUP-UTTB
                   THRU UTTB-1000-LOOKUP-UTTB-X
               IF  WUTTB-IO-OK
                   MOVE RUTTB-UPLD-TTBL-VALU-TXT
                                            TO RAPPF-CLI-PREG-IND
                   MOVE WAPUP-C-YES         TO LAPUP-REC-CHANGED-SW
                   GO TO 5000-PROCESS-TRANS-FIELD-X
               ELSE
                   MOVE WAPUP-C-TRAN-CONV-ERR TO LAPUP-RETURN-CD
                   GO TO 5000-PROCESS-TRANS-FIELD-X
               END-IF
           END-IF.
 
557659*    IF  RUFLD-UPLD-FLD-NM = 'DI_ELIG_CD'
557659     IF  RUFLD-UPLD-FLD-NM = 'CLI_DI_ELIG_IND'
               MOVE RUFLD-UPLD-TTBL-TYP-ID  TO WUTTB-UPLD-TTBL-TYP-ID
               MOVE LAPUP-INPUT-DATA        TO WUTTB-UPLD-TTBL-VALU-ID
               PERFORM  UTTB-1000-LOOKUP-UTTB
                   THRU UTTB-1000-LOOKUP-UTTB-X
               IF  WUTTB-IO-OK
                   MOVE RUTTB-UPLD-TTBL-VALU-TXT
557659*                                     TO RAPPF-DI-ELIG-CD
557659                                      TO RAPPF-CLI-DI-ELIG-IND
                   MOVE WAPUP-C-YES         TO LAPUP-REC-CHANGED-SW
                   GO TO 5000-PROCESS-TRANS-FIELD-X
               ELSE
                   MOVE WAPUP-C-TRAN-CONV-ERR TO LAPUP-RETURN-CD
                   GO TO 5000-PROCESS-TRANS-FIELD-X
               END-IF
           END-IF.
 
           IF  RUFLD-UPLD-FLD-NM = 'DI_INS_REFUS_IND'
               MOVE RUFLD-UPLD-TTBL-TYP-ID  TO WUTTB-UPLD-TTBL-TYP-ID
               MOVE LAPUP-INPUT-DATA        TO WUTTB-UPLD-TTBL-VALU-ID
               PERFORM  UTTB-1000-LOOKUP-UTTB
                   THRU UTTB-1000-LOOKUP-UTTB-X
               IF  WUTTB-IO-OK
                   MOVE RUTTB-UPLD-TTBL-VALU-TXT
                                            TO RAPPF-DI-INS-REFUS-IND
                   MOVE WAPUP-C-YES         TO LAPUP-REC-CHANGED-SW
                   GO TO 5000-PROCESS-TRANS-FIELD-X
               ELSE
                   MOVE WAPUP-C-TRAN-CONV-ERR TO LAPUP-RETURN-CD
                   GO TO 5000-PROCESS-TRANS-FIELD-X
               END-IF
           END-IF.
 
           IF  RUFLD-UPLD-FLD-NM = 'HERED_DISORD_IND'
               MOVE RUFLD-UPLD-TTBL-TYP-ID  TO WUTTB-UPLD-TTBL-TYP-ID
               MOVE LAPUP-INPUT-DATA        TO WUTTB-UPLD-TTBL-VALU-ID
               PERFORM  UTTB-1000-LOOKUP-UTTB
                   THRU UTTB-1000-LOOKUP-UTTB-X
               IF  WUTTB-IO-OK
                   MOVE RUTTB-UPLD-TTBL-VALU-TXT
                                            TO RAPPF-HERED-DISORD-IND
                   MOVE WAPUP-C-YES         TO LAPUP-REC-CHANGED-SW
                   GO TO 5000-PROCESS-TRANS-FIELD-X
               ELSE
                   MOVE WAPUP-C-TRAN-CONV-ERR
                                            TO LAPUP-RETURN-CD
                   GO TO 5000-PROCESS-TRANS-FIELD-X
               END-IF
           END-IF.
 
           IF  RUFLD-UPLD-FLD-NM = 'OINS_INFC_PEND_IND'
               MOVE RUFLD-UPLD-TTBL-TYP-ID  TO WUTTB-UPLD-TTBL-TYP-ID
               MOVE LAPUP-INPUT-DATA        TO WUTTB-UPLD-TTBL-VALU-ID
               PERFORM  UTTB-1000-LOOKUP-UTTB
                   THRU UTTB-1000-LOOKUP-UTTB-X
APEX54         PERFORM  5050-PROCESS-UTTB
APEX54             THRU 5050-PROCESS-UTTB-X
APEX54         GO TO 5000-PROCESS-TRANS-FIELD-X
           END-IF.
 
           MOVE WAPUP-C-UNKNOWN-FIELD-ERR   TO LAPUP-RETURN-CD.
 
       5000-PROCESS-TRANS-FIELD-X.
           EXIT.
      /
      *------------------
APEX54 5050-PROCESS-UTTB.
      *------------------
 
           IF  WUTTB-IO-OK
APEX52         IF  RAPPF-OINS-INFC-PEND-IND = 'Y'
APEX54             CONTINUE
APEX52         ELSE
                   MOVE RUTTB-UPLD-TTBL-VALU-TXT
                                            TO RAPPF-OINS-INFC-PEND-IND
                   MOVE WAPUP-C-YES         TO LAPUP-REC-CHANGED-SW
               END-IF
           ELSE
               MOVE WAPUP-C-TRAN-CONV-ERR   TO LAPUP-RETURN-CD
           END-IF.
 
APEX54 5050-PROCESS-UTTB-X.
           EXIT.
      /
      *---------------------------
       6000-PROCESS-COMPLEX-FIELD.
      *---------------------------
 
           IF  RUFLD-UPLD-FLD-NM = 'CLI_ELIG_BP_DUR'
           OR  RUFLD-UPLD-FLD-NM = 'CLI_ELIG_EP_DUR'
               GO TO 6000-PROCESS-COMPLEX-FIELD-X
           END-IF.
 
           IF  RUFLD-UPLD-FLD-NM = 'CLI_REFUS_CO_1_IND'
               IF  LAPUP-INPUT-DATA = '1'
                   MOVE RUFLD-UPLD-TTBL-TYP-ID
                                             TO WUTTB-UPLD-TTBL-TYP-ID
                   MOVE RUFLD-UPLD-FLD-APEX-NM
                                             TO WUTTB-UPLD-TTBL-VALU-ID
                   PERFORM  UTTB-1000-LOOKUP-UTTB
                       THRU UTTB-1000-LOOKUP-UTTB-X
APEX54             PERFORM  6010-CLI-REFUS-CO-1-IND
APEX54                 THRU 6010-CLI-REFUS-CO-1-IND-X
APEX54             GO TO 6000-PROCESS-COMPLEX-FIELD-X
               ELSE
                   GO TO 6000-PROCESS-COMPLEX-FIELD-X
               END-IF
           END-IF.
 
           IF  RUFLD-UPLD-FLD-NM = 'CLI_REFUS_CO_2_IND'
               IF  LAPUP-INPUT-DATA = '1'
                   MOVE RUFLD-UPLD-TTBL-TYP-ID
                                             TO WUTTB-UPLD-TTBL-TYP-ID
                   MOVE RUFLD-UPLD-FLD-APEX-NM
                                             TO WUTTB-UPLD-TTBL-VALU-ID
                   PERFORM  UTTB-1000-LOOKUP-UTTB
                       THRU UTTB-1000-LOOKUP-UTTB-X
APEX54             PERFORM  6020-CLI-REFUS-CO-2-IND
APEX54                 THRU 6020-CLI-REFUS-CO-2-IND-X
APEX54             GO TO 6000-PROCESS-COMPLEX-FIELD-X
               ELSE
                   GO TO 6000-PROCESS-COMPLEX-FIELD-X
               END-IF
           END-IF.
 
           IF  RUFLD-UPLD-FLD-NM = 'CLI_REL_DTH_AGE'
           OR  RUFLD-UPLD-FLD-NM = 'CLI_REL_DTH_QTY'
           OR  RUFLD-UPLD-FLD-NM = 'CLI_REL_TO_INSRD_CD'
           OR  RUFLD-UPLD-FLD-NM = 'CLI_REL_LVNG_AGE'
           OR  RUFLD-UPLD-FLD-NM = 'CLI_REL_LVNG_QTY'
               GO TO 6000-PROCESS-COMPLEX-FIELD-X
           END-IF.
 
           IF  RUFLD-UPLD-FLD-NM = 'CLI_WGT_CHNG_QTY'
               MOVE 4                        TO L0280-LENGTH
               MOVE 6                        TO L0280-PRECISION
               MOVE LAPUP-INPUT-DATA         TO L0280-INPUT-DATA
               PERFORM  0280-1000-NUMERIC-EDIT
                   THRU 0280-1000-NUMERIC-EDIT-X
               IF  L0280-OK
APEX54             PERFORM  6030-CLI-WGT-CHNG-QTY
APEX54                 THRU 6030-CLI-WGT-CHNG-QTY-X
APEX54             GO TO 6000-PROCESS-COMPLEX-FIELD-X
               ELSE
                   MOVE WAPUP-C-NUM-CONV-ERR TO LAPUP-RETURN-CD
                   MOVE L0280-STATUS         TO LAPUP-SUB-RETURN-CD
                   GO TO 6000-PROCESS-COMPLEX-FIELD-X
               END-IF
           END-IF.
 
           IF  RUFLD-UPLD-FLD-NM = 'DI_ELIG_AMT'
557659*    OR  RUFLD-UPLD-FLD-NM = 'DI_ELIG_CD'
557659     OR  RUFLD-UPLD-FLD-NM = 'CLI_DI_ELIG_IND'
           OR  RUFLD-UPLD-FLD-NM = 'DI_ELIG_DT_MO'
           OR  RUFLD-UPLD-FLD-NM = 'DI_ELIG_DT_YR'
           OR  RUFLD-UPLD-FLD-NM = 'DISORD_DTL_TXT'
           OR  RUFLD-UPLD-FLD-NM = 'HERED_DISORD_TXT'
               GO TO 6000-PROCESS-COMPLEX-FIELD-X
           END-IF.
 
           IF  RUFLD-UPLD-FLD-NM = 'INS_REFUS_CO_1_NM'
               IF  LAPUP-POL-TYP-DISABILITY
                   MOVE LAPUP-INPUT-DATA     TO RAPPF-DI-REFUS-CO-1-NM
                   MOVE WAPUP-C-YES          TO LAPUP-REC-CHANGED-SW
                   GO TO 6000-PROCESS-COMPLEX-FIELD-X
               ELSE
                   MOVE LAPUP-INPUT-DATA     TO RAPPF-INS-REFUS-CO-1-NM
                   MOVE WAPUP-C-YES          TO LAPUP-REC-CHANGED-SW
                   GO TO 6000-PROCESS-COMPLEX-FIELD-X
               END-IF
           END-IF.
 
           IF  RUFLD-UPLD-FLD-NM = 'INS_REFUS_CO_2_NM'
               IF  LAPUP-POL-TYP-DISABILITY
                   MOVE LAPUP-INPUT-DATA     TO RAPPF-DI-REFUS-CO-2-NM
                   MOVE WAPUP-C-YES          TO LAPUP-REC-CHANGED-SW
                   GO TO 6000-PROCESS-COMPLEX-FIELD-X
               ELSE
                   MOVE LAPUP-INPUT-DATA     TO RAPPF-INS-REFUS-CO-2-NM
                   MOVE WAPUP-C-YES          TO LAPUP-REC-CHANGED-SW
                   GO TO 6000-PROCESS-COMPLEX-FIELD-X
               END-IF
           END-IF.
 
557659*    IF  RUFLD-UPLD-FLD-NM = 'INS_REFUS_YR_QTY'
557659     IF  RUFLD-UPLD-FLD-NM = 'CLI_INS_REFUS_YR'
               MOVE 4                        TO L0280-LENGTH
               MOVE 0                        TO L0280-PRECISION
               MOVE LAPUP-INPUT-DT-YR        TO L0280-INPUT-DATA
               PERFORM  0280-1000-NUMERIC-EDIT
                   THRU 0280-1000-NUMERIC-EDIT-X
               IF  L0280-OK
APEX54             PERFORM   6040-INS-REFUS-YR-QTY
APEX54                 THRU  6040-INS-REFUS-YR-QTY-X
APEX54             GO TO 6000-PROCESS-COMPLEX-FIELD-X
               ELSE
                   MOVE WAPUP-C-NUM-CONV-ERR TO LAPUP-RETURN-CD
                   MOVE L0280-STATUS         TO LAPUP-SUB-RETURN-CD
                   GO TO 6000-PROCESS-COMPLEX-FIELD-X
               END-IF
           END-IF.
 
           IF  RUFLD-UPLD-FLD-NM = 'REASN_WGT_CHNG_TXT'
APEX54         PERFORM  6050-REASN-WGT-CHNG-TXT
APEX54             THRU 6050-REASN-WGT-CHNG-TXT-X
APEX54         GO TO 6000-PROCESS-COMPLEX-FIELD-X
           END-IF.
 
           IF  RUFLD-UPLD-FLD-NM = 'REFUS_REASN_1_TXT'
               MOVE RUFLD-UPLD-TTBL-TYP-ID  TO WUTTB-UPLD-TTBL-TYP-ID
               MOVE LAPUP-INPUT-DATA        TO WUTTB-UPLD-TTBL-VALU-ID
               PERFORM  UTTB-1000-LOOKUP-UTTB
                   THRU UTTB-1000-LOOKUP-UTTB-X
APEX54         PERFORM  6060-REFUS-REASN-1-TXT
APEX54             THRU 6060-REFUS-REASN-1-TXT-X
APEX54         GO TO 6000-PROCESS-COMPLEX-FIELD-X
           END-IF.
 
           IF  RUFLD-UPLD-FLD-NM = 'REFUS_REASN_2_TXT'
               MOVE RUFLD-UPLD-TTBL-TYP-ID  TO WUTTB-UPLD-TTBL-TYP-ID
               MOVE LAPUP-INPUT-DATA        TO WUTTB-UPLD-TTBL-VALU-ID
               PERFORM  UTTB-1000-LOOKUP-UTTB
                   THRU UTTB-1000-LOOKUP-UTTB-X
APEX54         PERFORM  6070-REFUS-REASN-2-TXT
APEX54             THRU 6070-REFUS-REASN-2-TXT-X
APEX54         GO TO 6000-PROCESS-COMPLEX-FIELD-X
           END-IF.
 
           IF  RUFLD-UPLD-FLD-NM = 'REL_HLTH_STATE_TXT'
           OR  RUFLD-UPLD-FLD-NM = 'REL_TO_INSRD_CD'
               GO TO 6000-PROCESS-COMPLEX-FIELD-X
           END-IF.
 
           MOVE WAPUP-C-UNKNOWN-FIELD-ERR     TO LAPUP-RETURN-CD.
 
       6000-PROCESS-COMPLEX-FIELD-X.
           EXIT.
      /
      *------------------------
APEX54 6010-CLI-REFUS-CO-1-IND.
      *------------------------
 
           IF  WUTTB-IO-OK
               IF  LAPUP-POL-TYP-DISABILITY
                   MOVE RUTTB-UPLD-TTBL-VALU-TXT
                                            TO RAPPF-DI-REFUS-CO-1-IND
                   MOVE WAPUP-C-YES         TO LAPUP-REC-CHANGED-SW
               ELSE
                   MOVE RUTTB-UPLD-TTBL-VALU-TXT
                                            TO RAPPF-CLI-REFUS-CO-1-IND
                   MOVE WAPUP-C-YES         TO LAPUP-REC-CHANGED-SW
               END-IF
           ELSE
               MOVE WAPUP-C-TRAN-CONV-ERR   TO LAPUP-RETURN-CD
           END-IF.
 
APEX54 6010-CLI-REFUS-CO-1-IND-X.
           EXIT.
      /
      *------------------------
APEX54 6020-CLI-REFUS-CO-2-IND.
      *------------------------
 
           IF  WUTTB-IO-OK
               IF  LAPUP-POL-TYP-DISABILITY
                   MOVE RUTTB-UPLD-TTBL-VALU-TXT
                                            TO RAPPF-DI-REFUS-CO-2-IND
                   MOVE WAPUP-C-YES         TO LAPUP-REC-CHANGED-SW
               ELSE
                   MOVE RUTTB-UPLD-TTBL-VALU-TXT
                                            TO RAPPF-CLI-REFUS-CO-2-IND
                   MOVE WAPUP-C-YES         TO LAPUP-REC-CHANGED-SW
               END-IF
           ELSE
               MOVE WAPUP-C-TRAN-CONV-ERR   TO LAPUP-RETURN-CD
           END-IF.
 
APEX54 6020-CLI-REFUS-CO-2-IND-X.
           EXIT.
      /
      *------------------------
APEX54 6030-CLI-WGT-CHNG-QTY.
      *------------------------
 
           IF  L0280-OUTPUT = ZERO
APEX54         CONTINUE
           ELSE
               IF  RAPPF-CLI-WGT-CHNG-QTY > ZERO
                   MOVE WAPUP-C-MULT-VALUES-ERR
                                            TO LAPUP-RETURN-CD
               ELSE
                   MOVE WAPUP-C-YES         TO LAPUP-REC-CHANGED-SW
 
                   COMPUTE RAPPF-CLI-WGT-CHNG-QTY =
                       L0280-OUTPUT / (10 ** L0280-PRECISION)
APEX54             PERFORM  6035-GAIN-KGS
APEX54                 THRU 6035-GAIN-KGS-X
               END-IF
           END-IF.
 
APEX54 6030-CLI-WGT-CHNG-QTY-X.
           EXIT.
      /
      *------------------------
APEX54 6035-GAIN-KGS.
      *------------------------
 
           IF  RUFLD-UPLD-FLD-APEX-NM = 'GAIN_KGS'
557659*        MOVE '+'                     TO RAPPF-WGT-GAIN-LOS-IND
557659         MOVE '+'                     TO RAPPF-WGT-GAIN-LOS-CD
           ELSE
557659*        MOVE '-'                     TO RAPPF-WGT-GAIN-LOS-IND
557659         MOVE '-'                     TO RAPPF-WGT-GAIN-LOS-CD
           END-IF.
 
APEX54 6035-GAIN-KGS-X.
           EXIT.
      /
      *------------------------
APEX54 6040-INS-REFUS-YR-QTY.
      *------------------------
 
           IF  LAPUP-POL-TYP-DISABILITY
APEX54         PERFORM  6045-UPLD-FLD-APEX-NM
APEX54             THRU 6045-UPLD-FLD-APEX-NM-X
           ELSE
557659*        IF  L0280-OUTPUT > RAPPF-INS-REFUS-YR-QTY
557659         IF  L0280-OUTPUT > RAPPF-CLI-INS-REFUS-YR
557659*            MOVE L0280-OUTPUT        TO RAPPF-INS-REFUS-YR-QTY
557659             MOVE L0280-OUTPUT        TO RAPPF-CLI-INS-REFUS-YR
                   MOVE WAPUP-C-YES         TO LAPUP-REC-CHANGED-SW
               END-IF
           END-IF.
 
APEX54 6040-INS-REFUS-YR-QTY-X.
           EXIT.
      /
      *------------------------
APEX54 6045-UPLD-FLD-APEX-NM.
      *------------------------
 
APEX54     EVALUATE  RUFLD-UPLD-FLD-APEX-NM
 
APEX54         WHEN 'YEAR1'
557659*             MOVE L0280-OUTPUT       TO RAPPF-DI-REFUS-YR-1-QTY
557659              MOVE L0280-OUTPUT       TO RAPPF-CLI-DI-REFUS-1-YR
                    MOVE WAPUP-C-YES        TO LAPUP-REC-CHANGED-SW
 
APEX54         WHEN 'YEAR2'
557659*                MOVE L0280-OUTPUT    TO RAPPF-DI-REFUS-YR-2-QTY
557659                 MOVE L0280-OUTPUT    TO RAPPF-CLI-DI-REFUS-2-YR
                       MOVE WAPUP-C-YES     TO LAPUP-REC-CHANGED-SW
 
APEX54     END-EVALUATE.
 
APEX54 6045-UPLD-FLD-APEX-NM-X.
           EXIT.
      /
      *------------------------
APEX54 6050-REASN-WGT-CHNG-TXT.
      *------------------------
 
           IF  RAPPF-REASN-WGT-CHNG-TXT NOT = SPACES
               MOVE WAPUP-C-MULT-VALUES-ERR TO LAPUP-RETURN-CD
           ELSE
               MOVE RUFLD-UPLD-TTBL-TYP-ID  TO WUTTB-UPLD-TTBL-TYP-ID
               MOVE RUFLD-UPLD-FLD-APEX-NM  TO WUTTB-UPLD-TTBL-VALU-ID
               PERFORM  UTTB-1000-LOOKUP-UTTB
                   THRU UTTB-1000-LOOKUP-UTTB-X
               IF  WUTTB-IO-OK
                   MOVE RUTTB-UPLD-TTBL-VALU-TXT
                                            TO RAPPF-REASN-WGT-CHNG-TXT
                   MOVE WAPUP-C-YES         TO LAPUP-REC-CHANGED-SW
               ELSE
                   MOVE WAPUP-C-FIELD-CONV-ERR
                                            TO LAPUP-RETURN-CD
               END-IF
           END-IF.
 
APEX54 6050-REASN-WGT-CHNG-TXT-X.
           EXIT.
      /
      *------------------------
APEX54 6060-REFUS-REASN-1-TXT.
      *------------------------
 
           IF  WUTTB-IO-OK
               IF  LAPUP-POL-TYP-DISABILITY
                   MOVE RUTTB-UPLD-TTBL-VALU-TXT
                                            TO RAPPF-DI-REFUS-1-TXT
                   MOVE WAPUP-C-YES         TO LAPUP-REC-CHANGED-SW
               ELSE
                   MOVE RUTTB-UPLD-TTBL-VALU-TXT
                                            TO RAPPF-REFUS-REASN-1-TXT
                   MOVE WAPUP-C-YES         TO LAPUP-REC-CHANGED-SW
               END-IF
           ELSE
               MOVE WAPUP-C-TRAN-CONV-ERR   TO LAPUP-RETURN-CD
           END-IF.
 
APEX54 6060-REFUS-REASN-1-TXT-X.
           EXIT.
      /
      *-----------------------
APEX54 6070-REFUS-REASN-2-TXT.
      *-----------------------
 
           IF  WUTTB-IO-OK
               IF  LAPUP-POL-TYP-DISABILITY
                   MOVE RUTTB-UPLD-TTBL-VALU-TXT
                                            TO RAPPF-DI-REFUS-2-TXT
                   MOVE WAPUP-C-YES         TO LAPUP-REC-CHANGED-SW
               ELSE
                   MOVE RUTTB-UPLD-TTBL-VALU-TXT
                                            TO RAPPF-REFUS-REASN-2-TXT
                   MOVE WAPUP-C-YES         TO LAPUP-REC-CHANGED-SW
               END-IF
           ELSE
               MOVE WAPUP-C-TRAN-CONV-ERR   TO LAPUP-RETURN-CD
           END-IF.
 
APEX54 6070-REFUS-REASN-2-TXT-X.
           EXIT.
      /
      *-------------------------
       7000-PROCESS-FIELD-FIELD.
      *-------------------------
 
           IF  LAPUP-INPUT-DATA NOT = '1'
               GO TO 7000-PROCESS-FIELD-FIELD-X
           END-IF.
 
           MOVE WAPUP-C-UNKNOWN-FIELD-ERR   TO LAPUP-RETURN-CD.
 
       7000-PROCESS-FIELD-FIELD-X.
           EXIT.
      /
      *****************************************************************
      *  LINKAGE PROCESSING COPYBOOKS                                 *
      *****************************************************************
       COPY XCPL0280.
      /
      *****************************************************************
      *  FILE I/O PROCESS MODULES                                     *
      *****************************************************************
       COPY ACPNUTTB.
      /
       COPY ACPPUTTB.
      /
      *****************************************************************
      *  ERROR HANDLING ROUTINES                                      *
      *****************************************************************
       COPY XCPL0030.
 
      *****************************************************************
      **                 END OF PROGRAM ASRUAPPF                     **
      *****************************************************************
