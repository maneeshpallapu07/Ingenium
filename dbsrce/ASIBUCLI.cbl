      *************************
       IDENTIFICATION DIVISION.
      *************************

       PROGRAM-ID. ASIBUCLI.

       COPY XCWWCRHT.

      *****************************************************************
      **  MEMBER :  ASIBUCLI                                         **
      **  REMARKS:  SQL I/O PROGRAM USED TO BROWSE ROWS IN THE       **
      **            APPLICATION UPLOAD CLIENT TABLE                  **
      *****************************************************************
      **  RELEASE   DESCRIPTION                                      **
      **                                                             **
      **  11FEB13   CREATED FOR UCLI PROCESSING                      **
C18250**  29MAR13   PS - SD91302 ADDED NEW FIELD FOR KANJI           **        
C18250**            SAMAKATA BU ADDRESS                              **        
MP270A**  23MAR16  FIELD ADDED AS PART OF E-POS APPLICATION XML      **
R12024**  23JUN16  UW_REVW_IND FIELD ADDED AS PART OF R12024         ** 
018396**  28MAY19   CREATED FOR UCLI PROCESSING                      **
26878B**  11APR23  CTS    ENHANCE CONTROL OVER ELDERLY & SPECIFIC    **
26878B**                  POLICY (PHASE 2)                           **
      *****************************************************************
      /
      **********************
       ENVIRONMENT DIVISION.
      **********************

      ***************
       DATA DIVISION.
      ***************
      /
      *************************
       WORKING-STORAGE SECTION.
      *************************

       COPY XCWWPGWS REPLACING '$VAR1' BY 'ASIBUCLI'.

       01  WS-WORKING-STORAGE.
           05  WS-WA-ADDRESS                    POINTER   VALUE NULL.
           05  WS-TL-ADDRESS                    POINTER   VALUE NULL.
           05  WS-OPTM-SQL-REQIR                PIC 9(02).
           05  WS-OPTM-SQL-EXEC                 PIC X(02) VALUE SPACES.
               88  WS-OPTM-SQL-CUR-CLOSED                 VALUE SPACES.
           05  WS-OPTM-SQL-EXEC-N               REDEFINES
               WS-OPTM-SQL-EXEC                 PIC 9(02).
      /
       COPY XCWWWKDT.
      /
           EXEC SQL INCLUDE ACWZUCLI  END-EXEC.
      /
      *****************
       LINKAGE SECTION.
      *****************

           EXEC SQL INCLUDE SQLCA     END-EXEC.

           EXEC SQL INCLUDE ACFWUCLI  END-EXEC.

           EXEC SQL INCLUDE ACFRUCLI  END-EXEC.

      /
       PROCEDURE DIVISION USING SQLCA
                                WUCLI-IO-WORK-AREA
                                RUCLI-REC-INFO.

      ***************
       0000-MAINLINE.
      ***************

           IF  WS-WA-ADDRESS NOT = ADDRESS OF WUCLI-IO-WORK-AREA
           OR  WS-TL-ADDRESS NOT = ADDRESS OF RUCLI-REC-INFO
               SET  WS-WA-ADDRESS  TO  ADDRESS OF WUCLI-IO-WORK-AREA
               SET  WS-TL-ADDRESS  TO  ADDRESS OF RUCLI-REC-INFO
               MOVE ZERO           TO  SQL-INIT-FLAG
           END-IF.


           EVALUATE TRUE

               WHEN WUCLI-RQST-BROWSE
                    PERFORM  1000-DETERMINE-SQL-REQIR
                        THRU 1000-DETERMINE-SQL-REQIR-X
                    PERFORM  2000-EXEC-BROWSE
                        THRU 2000-EXEC-BROWSE-X

               WHEN WUCLI-RQST-FETCH-NEXT
                    PERFORM  3000-EXEC-FETCH-NEXT
                        THRU 3000-EXEC-FETCH-NEXT-X

               WHEN WUCLI-RQST-CLOSE-BROWSE-CUR
                    PERFORM  4000-EXEC-CLOSE-CUR
                        THRU 4000-EXEC-CLOSE-CUR-X

               WHEN WUCLI-RQST-BROWSE-INDEX
                    PERFORM  1000-DETERMINE-SQL-REQIR
                        THRU 1000-DETERMINE-SQL-REQIR-X
                    PERFORM  5000-EXEC-BROWSE-INDEX
                        THRU 5000-EXEC-BROWSE-INDEX-X

               WHEN WUCLI-RQST-FETCH-NEXT-INDEX
                    PERFORM  6000-EXEC-FETCH-NEXT-INDEX
                        THRU 6000-EXEC-FETCH-NEXT-INDEX-X

               WHEN WUCLI-RQST-CLOSE-BROWSE-INDEX
                    PERFORM  7000-EXEC-CLOSE-INDEX
                        THRU 7000-EXEC-CLOSE-INDEX-X

               WHEN OTHER
                    SET  WUCLI-IO-ERROR      TO  TRUE

           END-EVALUATE.


           GOBACK.


       0000-MAINLINE-X.
           EXIT.


      **************************
       1000-DETERMINE-SQL-REQIR.
      **************************

           MOVE 2                            TO  WS-OPTM-SQL-REQIR.

           IF  WUCLI-APP-ID = WUCLI-ENDBR-APP-ID
               SUBTRACT +1                   FROM WS-OPTM-SQL-REQIR
           ELSE
               GO TO 1000-DETERMINE-SQL-REQIR-X
           END-IF.


           IF  WUCLI-SEQ-NUM = WUCLI-ENDBR-SEQ-NUM
               SUBTRACT +1                   FROM WS-OPTM-SQL-REQIR
           ELSE
               GO TO 1000-DETERMINE-SQL-REQIR-X
           END-IF.


       1000-DETERMINE-SQL-REQIR-X.
           EXIT.


      ******************
       2000-EXEC-BROWSE.
      ******************

           IF  WS-OPTM-SQL-EXEC NUMERIC
               MOVE WS-OPTM-SQL-EXEC-N       TO  WS-OPTM-SQL-REQIR
           END-IF.


           EVALUATE TRUE

               WHEN WS-OPTM-SQL-REQIR <= 1
                    MOVE '01'                TO  WS-OPTM-SQL-EXEC
                    PERFORM  2101-BROWSE
                        THRU 2101-BROWSE-X

               WHEN WS-OPTM-SQL-REQIR <= 2
                    MOVE '02'                TO  WS-OPTM-SQL-EXEC
                    PERFORM  210G-BROWSE
                        THRU 210G-BROWSE-X

               WHEN OTHER
                    MOVE SPACES              TO  WS-OPTM-SQL-EXEC
                    SET  WUCLI-IO-ERROR      TO  TRUE

           END-EVALUATE.


           MOVE WS-OPTM-SQL-REQIR            TO  WUCLI-OPTM-SQL-REQIR.
           MOVE WS-OPTM-SQL-EXEC             TO  WUCLI-OPTM-SQL-EXEC.

           EVALUATE TRUE

               WHEN WUCLI-OPTM-SQL-EXEC = WUCLI-OPTM-SQL-REQIR
                   SET WUCLI-OPTM-SQL-OK     TO  TRUE

               WHEN WUCLI-OPTM-SQL-EXEC = SPACES
                   SET WUCLI-OPTM-SQL-ERROR  TO  TRUE

               WHEN OTHER
                   SET WUCLI-OPTM-SQL-IMPRV  TO  TRUE

           END-EVALUATE.


       2000-EXEC-BROWSE-X.
           EXIT.


      *************
       2101-BROWSE.
      *************

           EXEC SQL
             DECLARE B1CUR_UCLI CURSOR FOR
             SELECT
                 APP_ID,
                 SEQ_NUM,
                 STCKR_ID,
                 POL_CLI_REL_TYP_CD,
                 CVG_CLI_REL_TYP_CD,
                 CLI_PHON_NUM_TXT,
                 CLI_GIV_NM,
                 CLI_SUR_NM,
                 CLI_BTH_DT,
                 CLI_SEX_CD,
                 CLI_SMKR_CD,
                 CLI_OCCP_CD,
                 CLI_ADDR_CD,
                 CLI_PSTL_CD,
                 CLI_ADDR_TXT,
                 CLI_KA_SUR_NM,
                 CLI_KA_GIV_NM,
                 POL_CLI_INSRD_CD,
                 DTH_BEN_SUR_NM,
                 DTH_BEN_GIV_NM,
                 DTH_BEN_REL_CD,
                 DTH_BEN_PCT,
                 PR_DTH_BEN_GIV_NM,
                 PR_DTH_BEN_SUR_NM,
                 PR_DTH_GIV_NM,
                 PR_DTH_BEN_REL_CD,
                 SEL_CD,
                 PRE_UW_IND,
                 SELF_DISC_IND,
                 BASE_CVG_IND,
                 LNB_IND,
                 DTH_BNFY_CO_NM,
                 CLI_CO_NM,
                 CLI_KA_CO_NM,
                 CLI_EARN_INCM_AMT,
                 OTHR_STCKR_ID,
                 TCB_IND,
                 CVG_PKG_CD,
                 OWN_INV_XPER_IND,
                 CLI_FIN_ASSET_CD,
                 IP_DTH_BEN_SUR_NM,
                 IP_DTH_BEN_GIV_NM,
                 IP_DTH_BNFY_CO_NM,
                 IP_DTH_BEN_REL_CD,
                 IP_DTH_BEN_PCT,
                 BNFY_ANTY_PERI_CD,
                 CLI_ADDR_KJ_TXT,
                 OTHR_INS_CO_BNFT_AMT,
                 INSRD_GUAR_IND,
                 HEALTH_STAT_IND,
                 BNFT_ENTLMT_HIST_IND,
                 ESIGN_INSRD_GUAR_IND,
                 BLOOD_TEST_IND,
                 ECG_IND,
                 ANN_INCM_AMT_IND,
                 DLY_INCM_AMT_IND,
                 AIS_UWDECSN_CD,
                 ADD_DSCLSRE_IND,
                 RADI_JOB_IND,
                 SCV_SELCT_INFO_IND,
                 OWN_GUAR_IND,
                 MNGR_INTRVW_IND,
                 MORAL_RISK_IND,
                 ESIGN_OWN_GUAR_IND,
                 SELF_OATH_INFO_IND,
                 EDSCLSRE_IND,
                 UW_REVW_IND,
                 GRDN_MORAL_RSK_CD,
                 INTRVW_OTHR_IND,
                 MINR_UNEMPL_IND,
                 CLI_FIN_ASSET_AMT
             FROM TUCLI
             WHERE
                 APP_ID   = :WUCLI-APP-ID
               AND
                 SEQ_NUM  BETWEEN
                            :WUCLI-SEQ-NUM       AND
                            :WUCLI-ENDBR-SEQ-NUM
             ORDER BY
                 APP_ID,
                 SEQ_NUM
             FOR FETCH ONLY
             OPTIMIZE FOR 1 ROW
           END-EXEC.

           EXEC SQL
                OPEN B1CUR_UCLI
           END-EXEC.


           EVALUATE SQLCODE

               WHEN ZERO
                    SET  WUCLI-IO-OK         TO  TRUE

               WHEN OTHER
                    SET  WUCLI-IO-ERROR      TO  TRUE

           END-EVALUATE.


       2101-BROWSE-X.
           EXIT.


      *************
       210G-BROWSE.
      *************

           EXEC SQL
             DECLARE BCUR_UCLI CURSOR FOR
             SELECT
                 APP_ID,
                 SEQ_NUM,
                 STCKR_ID,
                 POL_CLI_REL_TYP_CD,
                 CVG_CLI_REL_TYP_CD,
                 CLI_PHON_NUM_TXT,
                 CLI_GIV_NM,
                 CLI_SUR_NM,
                 CLI_BTH_DT,
                 CLI_SEX_CD,
                 CLI_SMKR_CD,
                 CLI_OCCP_CD,
                 CLI_ADDR_CD,
                 CLI_PSTL_CD,
                 CLI_ADDR_TXT,
                 CLI_KA_SUR_NM,
                 CLI_KA_GIV_NM,
                 POL_CLI_INSRD_CD,
                 DTH_BEN_SUR_NM,
                 DTH_BEN_GIV_NM,
                 DTH_BEN_REL_CD,
                 DTH_BEN_PCT,
                 PR_DTH_BEN_GIV_NM,
                 PR_DTH_BEN_SUR_NM,
                 PR_DTH_GIV_NM,
                 PR_DTH_BEN_REL_CD,
                 SEL_CD,
                 PRE_UW_IND,
                 SELF_DISC_IND,
                 BASE_CVG_IND,
                 LNB_IND,
                 DTH_BNFY_CO_NM,
                 CLI_CO_NM,
                 CLI_KA_CO_NM,
                 CLI_EARN_INCM_AMT,
                 OTHR_STCKR_ID,
                 TCB_IND,
                 CVG_PKG_CD,
                 OWN_INV_XPER_IND,
                 CLI_FIN_ASSET_CD,
                 IP_DTH_BEN_SUR_NM,
                 IP_DTH_BEN_GIV_NM,
                 IP_DTH_BNFY_CO_NM,
                 IP_DTH_BEN_REL_CD,
                 IP_DTH_BEN_PCT,
                 BNFY_ANTY_PERI_CD,
                 CLI_ADDR_KJ_TXT,
                 OTHR_INS_CO_BNFT_AMT,
                 INSRD_GUAR_IND,
                 HEALTH_STAT_IND,
                 BNFT_ENTLMT_HIST_IND,
                 ESIGN_INSRD_GUAR_IND,
                 BLOOD_TEST_IND,
                 ECG_IND,
                 ANN_INCM_AMT_IND,
                 DLY_INCM_AMT_IND,
                 AIS_UWDECSN_CD,
                 ADD_DSCLSRE_IND,
                 RADI_JOB_IND,
                 SCV_SELCT_INFO_IND,
                 OWN_GUAR_IND,
                 MNGR_INTRVW_IND,
                 MORAL_RISK_IND,
                 ESIGN_OWN_GUAR_IND,
                 SELF_OATH_INFO_IND,
                 EDSCLSRE_IND,
                 UW_REVW_IND,
                 GRDN_MORAL_RSK_CD,
                 INTRVW_OTHR_IND,
                 MINR_UNEMPL_IND,
                 CLI_FIN_ASSET_AMT
             FROM TUCLI
             WHERE
                 APP_ID   BETWEEN
                            :WUCLI-APP-ID          AND
                            :WUCLI-ENDBR-APP-ID
               AND
                (SEQ_NUM >= :WUCLI-SEQ-NUM         OR
                 APP_ID   > :WUCLI-APP-ID)
               AND
                (SEQ_NUM <= :WUCLI-ENDBR-SEQ-NUM   OR
                 APP_ID   < :WUCLI-ENDBR-APP-ID)
             ORDER BY
                 APP_ID,
                 SEQ_NUM
             FOR FETCH ONLY
             OPTIMIZE FOR 1 ROW
           END-EXEC.

           EXEC SQL
                OPEN BCUR_UCLI
           END-EXEC.


           EVALUATE SQLCODE

               WHEN ZERO
                    SET  WUCLI-IO-OK         TO  TRUE

               WHEN OTHER
                    SET  WUCLI-IO-ERROR      TO  TRUE

           END-EVALUATE.


       210G-BROWSE-X.
           EXIT.


      **********************
       3000-EXEC-FETCH-NEXT.
      **********************

           EVALUATE WS-OPTM-SQL-EXEC

               WHEN '01'
                    PERFORM  3101-FETCH-NEXT
                        THRU 3101-FETCH-NEXT-X

               WHEN '02'
                    PERFORM  310G-FETCH-NEXT
                        THRU 310G-FETCH-NEXT-X

               WHEN OTHER
                    PERFORM  310G-FETCH-NEXT
                        THRU 310G-FETCH-NEXT-X

           END-EVALUATE.


           SET  WUCLI-OPTM-SQL-OK            TO  TRUE.
           MOVE SPACES                       TO  WUCLI-OPTM-SQL-REQIR.
           MOVE SPACES                       TO  WUCLI-OPTM-SQL-EXEC.


       3000-EXEC-FETCH-NEXT-X.
           EXIT.


      *****************
       3101-FETCH-NEXT.
      *****************

           MOVE LOW-VALUES                   TO  ZUCLI-NULL-INDICATORS.

           EXEC SQL
             FETCH B1CUR_UCLI
             INTO
            :RUCLI-APP-ID,
                :RUCLI-SEQ-NUM,
                :RUCLI-STCKR-ID           :ZUCLI-STCKR-ID-NI,
                :RUCLI-POL-CLI-REL-TYP-CD :ZUCLI-POL-CLI-REL-TYP-CD-NI,
                :RUCLI-CVG-CLI-REL-TYP-CD :ZUCLI-CVG-CLI-REL-TYP-CD-NI,
                :RUCLI-CLI-PHON-NUM-TXT   :ZUCLI-CLI-PHON-NUM-TXT-NI,
                :RUCLI-CLI-GIV-NM         :ZUCLI-CLI-GIV-NM-NI,
                :RUCLI-CLI-SUR-NM         :ZUCLI-CLI-SUR-NM-NI,
                :RUCLI-CLI-BTH-DT         :ZUCLI-CLI-BTH-DT-NI,
                :RUCLI-CLI-SEX-CD         :ZUCLI-CLI-SEX-CD-NI,
                :RUCLI-CLI-SMKR-CD        :ZUCLI-CLI-SMKR-CD-NI,
                :RUCLI-CLI-OCCP-CD        :ZUCLI-CLI-OCCP-CD-NI,
                :RUCLI-CLI-ADDR-CD        :ZUCLI-CLI-ADDR-CD-NI,
                :RUCLI-CLI-PSTL-CD        :ZUCLI-CLI-PSTL-CD-NI,
                :RUCLI-CLI-ADDR-TXT       :ZUCLI-CLI-ADDR-TXT-NI,
                :RUCLI-CLI-KA-SUR-NM      :ZUCLI-CLI-KA-SUR-NM-NI,
                :RUCLI-CLI-KA-GIV-NM      :ZUCLI-CLI-KA-GIV-NM-NI,
                :RUCLI-POL-CLI-INSRD-CD   :ZUCLI-POL-CLI-INSRD-CD-NI,
                :RUCLI-DTH-BEN-SUR-NM     :ZUCLI-DTH-BEN-SUR-NM-NI,
                :RUCLI-DTH-BEN-GIV-NM     :ZUCLI-DTH-BEN-GIV-NM-NI,
                :RUCLI-DTH-BEN-REL-CD     :ZUCLI-DTH-BEN-REL-CD-NI,
                :RUCLI-DTH-BEN-PCT        :ZUCLI-DTH-BEN-PCT-NI,
                :RUCLI-PR-DTH-BEN-GIV-NM  :ZUCLI-PR-DTH-BEN-GIV-NM-NI,
                :RUCLI-PR-DTH-BEN-SUR-NM  :ZUCLI-PR-DTH-BEN-SUR-NM-NI,
                :RUCLI-PR-DTH-GIV-NM      :ZUCLI-PR-DTH-GIV-NM-NI,
                :RUCLI-PR-DTH-BEN-REL-CD  :ZUCLI-PR-DTH-BEN-REL-CD-NI,
                :RUCLI-SEL-CD             :ZUCLI-SEL-CD-NI,
                :RUCLI-PRE-UW-IND         :ZUCLI-PRE-UW-IND-NI,
                :RUCLI-SELF-DISC-IND      :ZUCLI-SELF-DISC-IND-NI,
                :RUCLI-BASE-CVG-IND       :ZUCLI-BASE-CVG-IND-NI,
                :RUCLI-LNB-IND            :ZUCLI-LNB-IND-NI,
                :RUCLI-DTH-BNFY-CO-NM     :ZUCLI-DTH-BNFY-CO-NM-NI,
                :RUCLI-CLI-CO-NM          :ZUCLI-CLI-CO-NM-NI,
                :RUCLI-CLI-KA-CO-NM       :ZUCLI-CLI-KA-CO-NM-NI,
                :RUCLI-CLI-EARN-INCM-AMT  :ZUCLI-CLI-EARN-INCM-AMT-NI,
                :RUCLI-OTHR-STCKR-ID      :ZUCLI-OTHR-STCKR-ID-NI,
                :RUCLI-TCB-IND            :ZUCLI-TCB-IND-NI,
                :RUCLI-CVG-PKG-CD         :ZUCLI-CVG-PKG-CD-NI,
                :RUCLI-OWN-INV-XPER-IND   :ZUCLI-OWN-INV-XPER-IND-NI,
                :RUCLI-CLI-FIN-ASSET-CD   :ZUCLI-CLI-FIN-ASSET-CD-NI,
                :RUCLI-IP-DTH-BEN-SUR-NM  :ZUCLI-IP-DTH-BEN-SUR-NM-NI,
                :RUCLI-IP-DTH-BEN-GIV-NM  :ZUCLI-IP-DTH-BEN-GIV-NM-NI,
                :RUCLI-IP-DTH-BNFY-CO-NM  :ZUCLI-IP-DTH-BNFY-CO-NM-NI,
                :RUCLI-IP-DTH-BEN-REL-CD  :ZUCLI-IP-DTH-BEN-REL-CD-NI,
                :RUCLI-IP-DTH-BEN-PCT     :ZUCLI-IP-DTH-BEN-PCT-NI,
                :RUCLI-BNFY-ANTY-PERI-CD  :ZUCLI-BNFY-ANTY-PERI-CD-NI,
            :RUCLI-CLI-ADDR-KJ-TXT      :ZUCLI-CLI-ADDR-KJ-TXT-NI,
            :RUCLI-OTHR-INS-CO-BNFT-AMT :ZUCLI-OTHR-INS-CO-BNFT-AMT-NI,
            :RUCLI-INSRD-GUAR-IND       :ZUCLI-INSRD-GUAR-IND-NI,
            :RUCLI-HEALTH-STAT-IND      :ZUCLI-HEALTH-STAT-IND-NI,
            :RUCLI-BNFT-ENTLMT-HIST-IND :ZUCLI-BNFT-ENTLMT-HIST-IND-NI,
            :RUCLI-ESIGN-INSRD-GUAR-IND :ZUCLI-ESIGN-INSRD-GUAR-IND-NI,
            :RUCLI-BLOOD-TEST-IND       :ZUCLI-BLOOD-TEST-IND-NI,
            :RUCLI-ECG-IND              :ZUCLI-ECG-IND-NI,
            :RUCLI-ANN-INCM-AMT-IND     :ZUCLI-ANN-INCM-AMT-IND-NI,
            :RUCLI-DLY-INCM-AMT-IND     :ZUCLI-DLY-INCM-AMT-IND-NI,
            :RUCLI-AIS-UWDECSN-CD       :ZUCLI-AIS-UWDECSN-CD-NI,
            :RUCLI-ADD-DSCLSRE-IND      :ZUCLI-ADD-DSCLSRE-IND-NI,
            :RUCLI-RADI-JOB-IND         :ZUCLI-RADI-JOB-IND-NI,
            :RUCLI-SCV-SELCT-INFO-IND   :ZUCLI-SCV-SELCT-INFO-IND-NI,
            :RUCLI-OWN-GUAR-IND         :ZUCLI-OWN-GUAR-IND-NI,
            :RUCLI-MNGR-INTRVW-IND      :ZUCLI-MNGR-INTRVW-IND-NI,
            :RUCLI-MORAL-RISK-IND       :ZUCLI-MORAL-RISK-IND-NI,
            :RUCLI-ESIGN-OWN-GUAR-IND   :ZUCLI-ESIGN-OWN-GUAR-IND-NI,
            :RUCLI-SELF-OATH-INFO-IND   :ZUCLI-SELF-OATH-INFO-IND-NI,
            :RUCLI-EDSCLSRE-IND         :ZUCLI-EDSCLSRE-IND-NI,
            :RUCLI-UW-REVW-IND          :ZUCLI-UW-REVW-IND-NI,
            :RUCLI-GRDN-MORAL-RSK-CD    :ZUCLI-GRDN-MORAL-RSK-CD-NI,
            :RUCLI-INTRVW-OTHR-IND      :ZUCLI-INTRVW-OTHR-IND-NI,
            :RUCLI-MINR-UNEMPL-IND      :ZUCLI-MINR-UNEMPL-IND-NI,
26878B      :RUCLI-CLI-FIN-ASSET-AMT    :ZUCLI-CLI-FIN-ASSET-AMT-NI
           END-EXEC.


           EVALUATE SQLCODE

               WHEN ZERO
                    SET  WUCLI-IO-OK         TO  TRUE
                    PERFORM  UCLI-2000-SET-NULL-DFLT
                        THRU UCLI-2000-SET-NULL-DFLT-X
                    MOVE RUCLI-KEY           TO  WUCLI-KEY

               WHEN +100
                    SET  WUCLI-IO-EOF        TO  TRUE

               WHEN OTHER
                    SET  WUCLI-IO-ERROR      TO  TRUE

           END-EVALUATE.


       3101-FETCH-NEXT-X.
           EXIT.


      *****************
       310G-FETCH-NEXT.
      *****************

           MOVE LOW-VALUES                   TO  ZUCLI-NULL-INDICATORS.

           EXEC SQL
             FETCH BCUR_UCLI
             INTO
                :RUCLI-APP-ID,
                :RUCLI-SEQ-NUM,
                :RUCLI-STCKR-ID           :ZUCLI-STCKR-ID-NI,
                :RUCLI-POL-CLI-REL-TYP-CD :ZUCLI-POL-CLI-REL-TYP-CD-NI,
                :RUCLI-CVG-CLI-REL-TYP-CD :ZUCLI-CVG-CLI-REL-TYP-CD-NI,
                :RUCLI-CLI-PHON-NUM-TXT   :ZUCLI-CLI-PHON-NUM-TXT-NI,
                :RUCLI-CLI-GIV-NM         :ZUCLI-CLI-GIV-NM-NI,
                :RUCLI-CLI-SUR-NM         :ZUCLI-CLI-SUR-NM-NI,
                :RUCLI-CLI-BTH-DT         :ZUCLI-CLI-BTH-DT-NI,
                :RUCLI-CLI-SEX-CD         :ZUCLI-CLI-SEX-CD-NI,
                :RUCLI-CLI-SMKR-CD        :ZUCLI-CLI-SMKR-CD-NI,
                :RUCLI-CLI-OCCP-CD        :ZUCLI-CLI-OCCP-CD-NI,
                :RUCLI-CLI-ADDR-CD        :ZUCLI-CLI-ADDR-CD-NI,
                :RUCLI-CLI-PSTL-CD        :ZUCLI-CLI-PSTL-CD-NI,
                :RUCLI-CLI-ADDR-TXT       :ZUCLI-CLI-ADDR-TXT-NI,
                :RUCLI-CLI-KA-SUR-NM      :ZUCLI-CLI-KA-SUR-NM-NI,
                :RUCLI-CLI-KA-GIV-NM      :ZUCLI-CLI-KA-GIV-NM-NI,
                :RUCLI-POL-CLI-INSRD-CD   :ZUCLI-POL-CLI-INSRD-CD-NI,
                :RUCLI-DTH-BEN-SUR-NM     :ZUCLI-DTH-BEN-SUR-NM-NI,
                :RUCLI-DTH-BEN-GIV-NM     :ZUCLI-DTH-BEN-GIV-NM-NI,
                :RUCLI-DTH-BEN-REL-CD     :ZUCLI-DTH-BEN-REL-CD-NI,
                :RUCLI-DTH-BEN-PCT        :ZUCLI-DTH-BEN-PCT-NI,
                :RUCLI-PR-DTH-BEN-GIV-NM  :ZUCLI-PR-DTH-BEN-GIV-NM-NI,
                :RUCLI-PR-DTH-BEN-SUR-NM  :ZUCLI-PR-DTH-BEN-SUR-NM-NI,
                :RUCLI-PR-DTH-GIV-NM      :ZUCLI-PR-DTH-GIV-NM-NI,
                :RUCLI-PR-DTH-BEN-REL-CD  :ZUCLI-PR-DTH-BEN-REL-CD-NI,
                :RUCLI-SEL-CD             :ZUCLI-SEL-CD-NI,
                :RUCLI-PRE-UW-IND         :ZUCLI-PRE-UW-IND-NI,
                :RUCLI-SELF-DISC-IND      :ZUCLI-SELF-DISC-IND-NI,
                :RUCLI-BASE-CVG-IND       :ZUCLI-BASE-CVG-IND-NI,
                :RUCLI-LNB-IND            :ZUCLI-LNB-IND-NI,
                :RUCLI-DTH-BNFY-CO-NM     :ZUCLI-DTH-BNFY-CO-NM-NI,
                :RUCLI-CLI-CO-NM          :ZUCLI-CLI-CO-NM-NI,
                :RUCLI-CLI-KA-CO-NM       :ZUCLI-CLI-KA-CO-NM-NI,
                :RUCLI-CLI-EARN-INCM-AMT  :ZUCLI-CLI-EARN-INCM-AMT-NI,
                :RUCLI-OTHR-STCKR-ID      :ZUCLI-OTHR-STCKR-ID-NI,
                :RUCLI-TCB-IND            :ZUCLI-TCB-IND-NI,
                :RUCLI-CVG-PKG-CD         :ZUCLI-CVG-PKG-CD-NI,
                :RUCLI-OWN-INV-XPER-IND   :ZUCLI-OWN-INV-XPER-IND-NI,
                :RUCLI-CLI-FIN-ASSET-CD   :ZUCLI-CLI-FIN-ASSET-CD-NI,
                :RUCLI-IP-DTH-BEN-SUR-NM  :ZUCLI-IP-DTH-BEN-SUR-NM-NI,
                :RUCLI-IP-DTH-BEN-GIV-NM  :ZUCLI-IP-DTH-BEN-GIV-NM-NI,
                :RUCLI-IP-DTH-BNFY-CO-NM  :ZUCLI-IP-DTH-BNFY-CO-NM-NI,
                :RUCLI-IP-DTH-BEN-REL-CD  :ZUCLI-IP-DTH-BEN-REL-CD-NI,
                :RUCLI-IP-DTH-BEN-PCT     :ZUCLI-IP-DTH-BEN-PCT-NI,
                :RUCLI-BNFY-ANTY-PERI-CD  :ZUCLI-BNFY-ANTY-PERI-CD-NI,
            :RUCLI-CLI-ADDR-KJ-TXT      :ZUCLI-CLI-ADDR-KJ-TXT-NI,
            :RUCLI-OTHR-INS-CO-BNFT-AMT :ZUCLI-OTHR-INS-CO-BNFT-AMT-NI,
            :RUCLI-INSRD-GUAR-IND       :ZUCLI-INSRD-GUAR-IND-NI,
            :RUCLI-HEALTH-STAT-IND      :ZUCLI-HEALTH-STAT-IND-NI,
            :RUCLI-BNFT-ENTLMT-HIST-IND :ZUCLI-BNFT-ENTLMT-HIST-IND-NI,
            :RUCLI-ESIGN-INSRD-GUAR-IND :ZUCLI-ESIGN-INSRD-GUAR-IND-NI,
            :RUCLI-BLOOD-TEST-IND       :ZUCLI-BLOOD-TEST-IND-NI,
            :RUCLI-ECG-IND              :ZUCLI-ECG-IND-NI,
            :RUCLI-ANN-INCM-AMT-IND     :ZUCLI-ANN-INCM-AMT-IND-NI,
            :RUCLI-DLY-INCM-AMT-IND     :ZUCLI-DLY-INCM-AMT-IND-NI,
            :RUCLI-AIS-UWDECSN-CD       :ZUCLI-AIS-UWDECSN-CD-NI,
            :RUCLI-ADD-DSCLSRE-IND      :ZUCLI-ADD-DSCLSRE-IND-NI,
            :RUCLI-RADI-JOB-IND         :ZUCLI-RADI-JOB-IND-NI,
            :RUCLI-SCV-SELCT-INFO-IND   :ZUCLI-SCV-SELCT-INFO-IND-NI,
            :RUCLI-OWN-GUAR-IND         :ZUCLI-OWN-GUAR-IND-NI,
            :RUCLI-MNGR-INTRVW-IND      :ZUCLI-MNGR-INTRVW-IND-NI,
            :RUCLI-MORAL-RISK-IND       :ZUCLI-MORAL-RISK-IND-NI,
            :RUCLI-ESIGN-OWN-GUAR-IND   :ZUCLI-ESIGN-OWN-GUAR-IND-NI,
            :RUCLI-SELF-OATH-INFO-IND   :ZUCLI-SELF-OATH-INFO-IND-NI,
            :RUCLI-EDSCLSRE-IND         :ZUCLI-EDSCLSRE-IND-NI,
            :RUCLI-UW-REVW-IND          :ZUCLI-UW-REVW-IND-NI,
            :RUCLI-GRDN-MORAL-RSK-CD    :ZUCLI-GRDN-MORAL-RSK-CD-NI,
            :RUCLI-INTRVW-OTHR-IND      :ZUCLI-INTRVW-OTHR-IND-NI,
            :RUCLI-MINR-UNEMPL-IND      :ZUCLI-MINR-UNEMPL-IND-NI
           END-EXEC.


           EVALUATE SQLCODE

               WHEN ZERO
                    SET  WUCLI-IO-OK         TO  TRUE
                    PERFORM  UCLI-2000-SET-NULL-DFLT
                        THRU UCLI-2000-SET-NULL-DFLT-X
                    MOVE RUCLI-KEY           TO  WUCLI-KEY

               WHEN +100
                    SET  WUCLI-IO-EOF        TO  TRUE

               WHEN OTHER
                    SET  WUCLI-IO-ERROR      TO  TRUE

           END-EVALUATE.


       310G-FETCH-NEXT-X.
           EXIT.


      *********************
       4000-EXEC-CLOSE-CUR.
      *********************

           EVALUATE WS-OPTM-SQL-EXEC

               WHEN '01'
                    PERFORM  4101-CLOSE-BROWSE-CUR
                        THRU 4101-CLOSE-BROWSE-CUR-X

               WHEN '02'
                    PERFORM  410G-CLOSE-BROWSE-CUR
                        THRU 410G-CLOSE-BROWSE-CUR-X

               WHEN OTHER
                    PERFORM  410G-CLOSE-BROWSE-CUR
                        THRU 410G-CLOSE-BROWSE-CUR-X

           END-EVALUATE.


           SET  WS-OPTM-SQL-CUR-CLOSED       TO  TRUE.


       4000-EXEC-CLOSE-CUR-X.
           EXIT.


      ***********************
       4101-CLOSE-BROWSE-CUR.
      ***********************

           EXEC SQL
                CLOSE B1CUR_UCLI
           END-EXEC.


           EVALUATE SQLCODE

               WHEN ZERO
                    SET  WUCLI-IO-OK         TO  TRUE

               WHEN OTHER
                    SET  WUCLI-IO-ERROR      TO  TRUE

           END-EVALUATE.


       4101-CLOSE-BROWSE-CUR-X.
           EXIT.


      ***********************
       410G-CLOSE-BROWSE-CUR.
      ***********************

           EXEC SQL
                CLOSE BCUR_UCLI
           END-EXEC.


           EVALUATE SQLCODE

               WHEN ZERO
                    SET  WUCLI-IO-OK         TO  TRUE

               WHEN OTHER
                    SET  WUCLI-IO-ERROR      TO  TRUE

           END-EVALUATE.


       410G-CLOSE-BROWSE-CUR-X.
           EXIT.


      ************************
       5000-EXEC-BROWSE-INDEX.
      ************************

           IF  WS-OPTM-SQL-EXEC NUMERIC
               MOVE WS-OPTM-SQL-EXEC-N       TO  WS-OPTM-SQL-REQIR
           END-IF.


      * PROFILE INDICATED THAT -BROWSE-INDEX
      * PARAGRAPHS ARE NOT APPLICABLE

           SET  WUCLI-IO-ERROR               TO  TRUE.
           MOVE WS-OPTM-SQL-REQIR            TO  WUCLI-OPTM-SQL-REQIR.
           MOVE WS-OPTM-SQL-EXEC             TO  WUCLI-OPTM-SQL-EXEC.

           EVALUATE TRUE

               WHEN WUCLI-OPTM-SQL-EXEC = WUCLI-OPTM-SQL-REQIR
                   SET WUCLI-OPTM-SQL-OK     TO  TRUE

               WHEN WUCLI-OPTM-SQL-EXEC = SPACES
                   SET WUCLI-OPTM-SQL-ERROR  TO  TRUE

               WHEN OTHER
                   SET WUCLI-OPTM-SQL-IMPRV  TO  TRUE

           END-EVALUATE.


       5000-EXEC-BROWSE-INDEX-X.
           EXIT.


      ****************************
       6000-EXEC-FETCH-NEXT-INDEX.
      ****************************

      * PROFILE INDICATED THAT -FETCH-NEXT-INDEX
      * PARAGRAPHS ARE NOT APPLICABLE

           SET  WUCLI-IO-ERROR               TO  TRUE.
           SET  WUCLI-OPTM-SQL-OK            TO  TRUE.
           MOVE SPACES                       TO  WUCLI-OPTM-SQL-REQIR.
           MOVE SPACES                       TO  WUCLI-OPTM-SQL-EXEC.


       6000-EXEC-FETCH-NEXT-INDEX-X.
           EXIT.


      ***********************
       7000-EXEC-CLOSE-INDEX.
      ***********************

      * PROFILE INDICATED THAT -CLOSE-BROWSE-INDEX
      * PARAGRAPHS ARE NOT APPLICABLE

           SET  WUCLI-IO-ERROR               TO  TRUE.
           SET  WS-OPTM-SQL-CUR-CLOSED       TO  TRUE.


       7000-EXEC-CLOSE-INDEX-X.
           EXIT.


       COPY ACPZUCLI.

      *****************************************************************
      **                 END OF PROGRAM ASIBUCLI                     **
      *****************************************************************
