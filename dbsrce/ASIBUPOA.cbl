      *************************
       IDENTIFICATION DIVISION.
      *************************

       PROGRAM-ID. ASIBUPOA.

       COPY XCWWCRHT.

      *****************************************************************
      **  MEMBER :  ASIBUPOA                                         **
      **  REMARKS:  SQL I/O PROGRAM USED TO BROWSE ROWS IN THE       **
      **            APPLICATION UPLOAD POLICY TABLE (ALT. ACCESS)    **
      *****************************************************************
      **  RELEASE   DESCRIPTION                                      **
      **                                                             **
      **  25JUL09   CREATED FOR UPOL PROCESSING                      **
MP270A**  23MAR16  FIELD ADDED AS PART OF E-POS APPLICATION XML      ** 
M319N1**  03FEB17  FIELD ADDED AS PART OF FXWL XML CHANGES           **
018396**  29MAY19  CTS   CHANGES FOR EPOS DAY2.0                     **
FF2003**  03JUN20  CTS   ADDED NEW FIELD FOR FFF-SMBC CHANGES        **
29746F**  09OCT24  CTS   NEW FIELD ADDED AS PART OF CERBERUS PROJECT **
27624C**  31JAN24  CTS   MANAGEMENT FOR POL ECERTIFICATE             **
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

       COPY XCWWPGWS REPLACING '$VAR1' BY 'ASIBUPOA'.

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
           EXEC SQL INCLUDE ACWZUPOL  END-EXEC.
      /
      *****************
       LINKAGE SECTION.
      *****************

           EXEC SQL INCLUDE SQLCA     END-EXEC.

           EXEC SQL INCLUDE ACFWUPOA  END-EXEC.

           EXEC SQL INCLUDE ACFRUPOL  END-EXEC.

      /
       PROCEDURE DIVISION USING SQLCA
                                WUPOA-IO-WORK-AREA
                                RUPOL-REC-INFO.

      ***************
       0000-MAINLINE.
      ***************

           IF  WS-WA-ADDRESS NOT = ADDRESS OF WUPOA-IO-WORK-AREA
           OR  WS-TL-ADDRESS NOT = ADDRESS OF RUPOL-REC-INFO
               SET  WS-WA-ADDRESS  TO  ADDRESS OF WUPOA-IO-WORK-AREA
               SET  WS-TL-ADDRESS  TO  ADDRESS OF RUPOL-REC-INFO
               MOVE ZERO           TO  SQL-INIT-FLAG
           END-IF.


           EVALUATE TRUE

               WHEN WUPOA-RQST-BROWSE
                    PERFORM  1000-DETERMINE-SQL-REQIR
                        THRU 1000-DETERMINE-SQL-REQIR-X
                    PERFORM  2000-EXEC-BROWSE
                        THRU 2000-EXEC-BROWSE-X

               WHEN WUPOA-RQST-FETCH-NEXT
                    PERFORM  3000-EXEC-FETCH-NEXT
                        THRU 3000-EXEC-FETCH-NEXT-X

               WHEN WUPOA-RQST-CLOSE-BROWSE-CUR
                    PERFORM  4000-EXEC-CLOSE-CUR
                        THRU 4000-EXEC-CLOSE-CUR-X

               WHEN WUPOA-RQST-BROWSE-INDEX
                    PERFORM  1000-DETERMINE-SQL-REQIR
                        THRU 1000-DETERMINE-SQL-REQIR-X
                    PERFORM  5000-EXEC-BROWSE-INDEX
                        THRU 5000-EXEC-BROWSE-INDEX-X

               WHEN WUPOA-RQST-FETCH-NEXT-INDEX
                    PERFORM  6000-EXEC-FETCH-NEXT-INDEX
                        THRU 6000-EXEC-FETCH-NEXT-INDEX-X

               WHEN WUPOA-RQST-CLOSE-BROWSE-INDEX
                    PERFORM  7000-EXEC-CLOSE-INDEX
                        THRU 7000-EXEC-CLOSE-INDEX-X

               WHEN OTHER
                    SET  WUPOA-IO-ERROR      TO  TRUE

           END-EVALUATE.


           GOBACK.


       0000-MAINLINE-X.
           EXIT.


      **************************
       1000-DETERMINE-SQL-REQIR.
      **************************

           MOVE 1                            TO  WS-OPTM-SQL-REQIR.

           IF  WUPOA-APP-UPLD-DT = WUPOA-ENDBR-APP-UPLD-DT
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
                    PERFORM  210G-BROWSE
                        THRU 210G-BROWSE-X

               WHEN OTHER
                    MOVE SPACES              TO  WS-OPTM-SQL-EXEC
                    SET  WUPOA-IO-ERROR      TO  TRUE

           END-EVALUATE.


           MOVE WS-OPTM-SQL-REQIR            TO  WUPOA-OPTM-SQL-REQIR.
           MOVE WS-OPTM-SQL-EXEC             TO  WUPOA-OPTM-SQL-EXEC.

           EVALUATE TRUE

               WHEN WUPOA-OPTM-SQL-EXEC = WUPOA-OPTM-SQL-REQIR
                   SET WUPOA-OPTM-SQL-OK     TO  TRUE

               WHEN WUPOA-OPTM-SQL-EXEC = SPACES
                   SET WUPOA-OPTM-SQL-ERROR  TO  TRUE

               WHEN OTHER
                   SET WUPOA-OPTM-SQL-IMPRV  TO  TRUE

           END-EVALUATE.


       2000-EXEC-BROWSE-X.
           EXIT.


      *************
       210G-BROWSE.
      *************

           EXEC SQL
             DECLARE BCUR_UPOA CURSOR FOR
             SELECT
                 APP_ID,
                 POL_ID,
                 PLAN_ID,
                 BASE_CVG_CD,
                 PYMT_AMT,
                 PYMT_MODE_CD,
                 PYMT_TYP_CD,
                 BNK_ACCT_ID,
                 BNK_ACCT_HLDR_NM,
                 NAYOSE_POL_CD,
                 ACCT_HLD_TYP_CD,
                 BNK_ACCT_TYP_CD,
                 EMAIL_ADDR_CD,
                 APP_UPLD_DT,
                 ILLUS_CALC_DT,
                 APP_RPT_DT,
                 LUMP_SUM_AMT,
                 CWA_RECPT_DT,
                 CWA_RECPT_NUM,
                 POL_ASGN_IND,
                 APP_CORCT_IND,
                 HO_CNSLT_IND,
                 PRE_ISS_CNFRM_IND,
                 POL_HLD_AGT_IND,
                 MOD_REG_PREM_AMT,
                 BNK_ID,
                 BNK_BR_ID,
                 BR_NUM,
                 SO_NUM,
                 SALES_REP_NUM,
                 CO_SALES_REP_NUM,
                 REFRL_CD,
                 LBILL_GR_CD,
                 LBILL_CO_ID,
                 LBILL_GR_CLI_ID,
                 LBILL_GR_EMP_ID,
                 BT_BILL_DLAY_IND,
                 POL_CNVR_KEY_NUM,
                 POL_CNVR_TYP_CD,
                 PROD_APP_TYP_CD,
                 MY_KEMPO_TYP_CD,
                 SUB_CAT_CD,
                 ORIG_POL_ID,
                 ORIG_POL_ISS_DT,
                 ORIG_POL_DUR,
                 GA_INIT_PREM_AMT,
                 GA_SUBSEQ_PREM_AMT,
                 GA_INIT_LMPSM_AMT,
                 GA_CNVR_FND_AMT,
                 AGT_XPER_CD,
                 VERIF_MTHD_CD,
                 DESGNT_ISS_DT_IND,
                 APL_REJ_IND,
                 BULK_REMIT_NUM,
                 AUTO_PREM_CHNG_IND,
                 PLAN_INIT_PMT_TYP_CD,
                 POL_BULK_APP_ID,
                 SPCL_AGT_SIGN_DT,
                 NON_FACE_TO_FACE_IND,
                 WIRE_XFER_VIRTUAL_NUM,
                 SCHD_ADV_PMT_DUR,
                 SNGL_PREM_AMT,
                 PMT_CRCY_CD,
                 ADDR_CNFRM_IND,
                 OVRSEAS_TRAV_IND,
                 FRGN_OWN_IND,
                 WORK_INS_CO_IND,
                 THRD_PARTY_BNFY_IND,
                 FRGN_CLI_ATCH_IND,
                 DOCS_TO_BE_SENT_IND,
                 SPCL_NOTES_IND,
                 PRELIM_UW_IND,
                 VOLNTR_APPL_IND,
                 MNGR_SPCL_NOTES_IND,
                 PHYS_SPCL_NOTES_IND,
                 DONATE_FORM_IND,
                 MULT_APPL_IND,
                 PAPR_LESS_APP_IND,
                 APPROV_NUM_IND,
                 YBA_RPT_REQIR_IND,
                 SPEC_ASSOC_CORP_IND,
                 SAL_TST_RSLT_CD,
                 SPCL_NOTE_INTNT_IND,
                 ANTY_DONAT_FORM_IND,
                 POL_PRVD_TYP_CD,
				 POL_ECERT_IND,
                 TRXN_CNFRM_OS_IND,
                 MTHLY_STD_PREM_AMT,
                 CPN_AUTO_PAYO_IND,
                 JPY_PAYO_IND,
                 PAYO_BNK_ID,
                 PAYO_BNK_BR_ID,
                 PAYO_BNK_ACCT_ID,
                 PAYO_BNK_ACCT_TYP_CD,
                 PAYO_BNK_ACC_HLDR_NM			 
             FROM TUPOL
             WHERE
                 APP_UPLD_DT  BETWEEN
                                :WUPOA-APP-UPLD-DT       AND
                                :WUPOA-ENDBR-APP-UPLD-DT
             ORDER BY
                 APP_UPLD_DT
             FOR FETCH ONLY
             OPTIMIZE FOR 1 ROW
           END-EXEC.

           EXEC SQL
                OPEN BCUR_UPOA
           END-EXEC.


           EVALUATE SQLCODE

               WHEN ZERO
                    SET  WUPOA-IO-OK         TO  TRUE

               WHEN OTHER
                    SET  WUPOA-IO-ERROR      TO  TRUE

           END-EVALUATE.


       210G-BROWSE-X.
           EXIT.


      **********************
       3000-EXEC-FETCH-NEXT.
      **********************

           EVALUATE WS-OPTM-SQL-EXEC

               WHEN '01'
                    PERFORM  310G-FETCH-NEXT
                        THRU 310G-FETCH-NEXT-X

               WHEN OTHER
                    PERFORM  310G-FETCH-NEXT
                        THRU 310G-FETCH-NEXT-X

           END-EVALUATE.


           SET  WUPOA-OPTM-SQL-OK            TO  TRUE.
           MOVE SPACES                       TO  WUPOA-OPTM-SQL-REQIR.
           MOVE SPACES                       TO  WUPOA-OPTM-SQL-EXEC.


       3000-EXEC-FETCH-NEXT-X.
           EXIT.


      *****************
       310G-FETCH-NEXT.
      *****************

           MOVE LOW-VALUES                   TO  ZUPOL-NULL-INDICATORS.

           EXEC SQL
             FETCH BCUR_UPOA
             INTO
                :RUPOL-APP-ID,
                :RUPOL-POL-ID             :ZUPOL-POL-ID-NI,
                :RUPOL-PLAN-ID            :ZUPOL-PLAN-ID-NI,
                :RUPOL-BASE-CVG-CD        :ZUPOL-BASE-CVG-CD-NI,
                :RUPOL-PYMT-AMT           :ZUPOL-PYMT-AMT-NI,
                :RUPOL-PYMT-MODE-CD       :ZUPOL-PYMT-MODE-CD-NI,
                :RUPOL-PYMT-TYP-CD        :ZUPOL-PYMT-TYP-CD-NI,
                :RUPOL-BNK-ACCT-ID        :ZUPOL-BNK-ACCT-ID-NI,
                :RUPOL-BNK-ACCT-HLDR-NM   :ZUPOL-BNK-ACCT-HLDR-NM-NI,
                :RUPOL-NAYOSE-POL-CD      :ZUPOL-NAYOSE-POL-CD-NI,
                :RUPOL-ACCT-HLD-TYP-CD    :ZUPOL-ACCT-HLD-TYP-CD-NI,
                :RUPOL-BNK-ACCT-TYP-CD    :ZUPOL-BNK-ACCT-TYP-CD-NI,
                :RUPOL-EMAIL-ADDR-CD      :ZUPOL-EMAIL-ADDR-CD-NI,
                :RUPOL-APP-UPLD-DT        :ZUPOL-APP-UPLD-DT-NI,
                :RUPOL-ILLUS-CALC-DT      :ZUPOL-ILLUS-CALC-DT-NI,
                :RUPOL-APP-RPT-DT         :ZUPOL-APP-RPT-DT-NI,
                :RUPOL-LUMP-SUM-AMT       :ZUPOL-LUMP-SUM-AMT-NI,
                :RUPOL-CWA-RECPT-DT       :ZUPOL-CWA-RECPT-DT-NI,
                :RUPOL-CWA-RECPT-NUM      :ZUPOL-CWA-RECPT-NUM-NI,
                :RUPOL-POL-ASGN-IND       :ZUPOL-POL-ASGN-IND-NI,
                :RUPOL-APP-CORCT-IND      :ZUPOL-APP-CORCT-IND-NI,
                :RUPOL-HO-CNSLT-IND       :ZUPOL-HO-CNSLT-IND-NI,
                :RUPOL-PRE-ISS-CNFRM-IND  :ZUPOL-PRE-ISS-CNFRM-IND-NI,
                :RUPOL-POL-HLD-AGT-IND    :ZUPOL-POL-HLD-AGT-IND-NI,
                :RUPOL-MOD-REG-PREM-AMT   :ZUPOL-MOD-REG-PREM-AMT-NI,
                :RUPOL-BNK-ID             :ZUPOL-BNK-ID-NI,
                :RUPOL-BNK-BR-ID          :ZUPOL-BNK-BR-ID-NI,
                :RUPOL-BR-NUM             :ZUPOL-BR-NUM-NI,
                :RUPOL-SO-NUM             :ZUPOL-SO-NUM-NI,
                :RUPOL-SALES-REP-NUM      :ZUPOL-SALES-REP-NUM-NI,
                :RUPOL-CO-SALES-REP-NUM   :ZUPOL-CO-SALES-REP-NUM-NI,
                :RUPOL-REFRL-CD           :ZUPOL-REFRL-CD-NI,
                :RUPOL-LBILL-GR-CD        :ZUPOL-LBILL-GR-CD-NI,
                :RUPOL-LBILL-CO-ID        :ZUPOL-LBILL-CO-ID-NI,
                :RUPOL-LBILL-GR-CLI-ID    :ZUPOL-LBILL-GR-CLI-ID-NI,
                :RUPOL-LBILL-GR-EMP-ID    :ZUPOL-LBILL-GR-EMP-ID-NI,
                :RUPOL-BT-BILL-DLAY-IND   :ZUPOL-BT-BILL-DLAY-IND-NI,
                :RUPOL-POL-CNVR-KEY-NUM   :ZUPOL-POL-CNVR-KEY-NUM-NI,
                :RUPOL-POL-CNVR-TYP-CD    :ZUPOL-POL-CNVR-TYP-CD-NI,
                :RUPOL-PROD-APP-TYP-CD    :ZUPOL-PROD-APP-TYP-CD-NI,
                :RUPOL-MY-KEMPO-TYP-CD    :ZUPOL-MY-KEMPO-TYP-CD-NI,
                :RUPOL-SUB-CAT-CD         :ZUPOL-SUB-CAT-CD-NI,
                :RUPOL-ORIG-POL-ID        :ZUPOL-ORIG-POL-ID-NI,
                :RUPOL-ORIG-POL-ISS-DT    :ZUPOL-ORIG-POL-ISS-DT-NI,
                :RUPOL-ORIG-POL-DUR       :ZUPOL-ORIG-POL-DUR-NI,
                :RUPOL-GA-INIT-PREM-AMT   :ZUPOL-GA-INIT-PREM-AMT-NI,
                :RUPOL-GA-SUBSEQ-PREM-AMT :ZUPOL-GA-SUBSEQ-PREM-AMT-NI,
                :RUPOL-GA-INIT-LMPSM-AMT  :ZUPOL-GA-INIT-LMPSM-AMT-NI,
                :RUPOL-GA-CNVR-FND-AMT    :ZUPOL-GA-CNVR-FND-AMT-NI,
                :RUPOL-AGT-XPER-CD        :ZUPOL-AGT-XPER-CD-NI,
                :RUPOL-VERIF-MTHD-CD      :ZUPOL-VERIF-MTHD-CD-NI,
                :RUPOL-DESGNT-ISS-DT-IND  :ZUPOL-DESGNT-ISS-DT-IND-NI,
                :RUPOL-APL-REJ-IND        :ZUPOL-APL-REJ-IND-NI,
                :RUPOL-BULK-REMIT-NUM     :ZUPOL-BULK-REMIT-NUM-NI,
          :RUPOL-AUTO-PREM-CHNG-IND    :ZUPOL-AUTO-PREM-CHNG-IND-NI,
          :RUPOL-PLAN-INIT-PMT-TYP-CD  :ZUPOL-PLAN-INIT-PMT-TYP-CD-NI,
          :RUPOL-POL-BULK-APP-ID       :ZUPOL-POL-BULK-APP-ID-NI,
          :RUPOL-SPCL-AGT-SIGN-DT      :ZUPOL-SPCL-AGT-SIGN-DT-NI,
          :RUPOL-NON-FACE-TO-FACE-IND  :ZUPOL-NON-FACE-TO-FACE-IND-NI,
         :RUPOL-WIRE-XFER-VIRTUAL-NUM :ZUPOL-WIRE-XFER-VIRTUAL-NUM-NI,
          :RUPOL-SCHD-ADV-PMT-DUR      :ZUPOL-SCHD-ADV-PMT-DUR-NI,
          :RUPOL-SNGL-PREM-AMT         :ZUPOL-SNGL-PREM-AMT-NI,
          :RUPOL-PMT-CRCY-CD           :ZUPOL-PMT-CRCY-CD-NI,
          :RUPOL-ADDR-CNFRM-IND        :ZUPOL-ADDR-CNFRM-IND-NI,
          :RUPOL-OVRSEAS-TRAV-IND      :ZUPOL-OVRSEAS-TRAV-IND-NI,
          :RUPOL-FRGN-OWN-IND          :ZUPOL-FRGN-OWN-IND-NI,
          :RUPOL-WORK-INS-CO-IND       :ZUPOL-WORK-INS-CO-IND-NI,
          :RUPOL-THRD-PARTY-BNFY-IND   :ZUPOL-THRD-PARTY-BNFY-IND-NI,
          :RUPOL-FRGN-CLI-ATCH-IND     :ZUPOL-FRGN-CLI-ATCH-IND-NI,
          :RUPOL-DOCS-TO-BE-SENT-IND   :ZUPOL-DOCS-TO-BE-SENT-IND-NI,
          :RUPOL-SPCL-NOTES-IND        :ZUPOL-SPCL-NOTES-IND-NI,
          :RUPOL-PRELIM-UW-IND         :ZUPOL-PRELIM-UW-IND-NI,
          :RUPOL-VOLNTR-APPL-IND       :ZUPOL-VOLNTR-APPL-IND-NI,
          :RUPOL-MNGR-SPCL-NOTES-IND   :ZUPOL-MNGR-SPCL-NOTES-IND-NI,
          :RUPOL-PHYS-SPCL-NOTES-IND   :ZUPOL-PHYS-SPCL-NOTES-IND-NI,
          :RUPOL-DONATE-FORM-IND       :ZUPOL-DONATE-FORM-IND-NI,
          :RUPOL-MULT-APPL-IND         :ZUPOL-MULT-APPL-IND-NI,
          :RUPOL-PAPR-LESS-APP-IND     :ZUPOL-PAPR-LESS-APP-IND-NI,
          :RUPOL-APPROV-NUM-IND        :ZUPOL-APPROV-NUM-IND-NI,
          :RUPOL-YBA-RPT-REQIR-IND     :ZUPOL-YBA-RPT-REQIR-IND-NI,
          :RUPOL-SPEC-ASSOC-CORP-IND   :ZUPOL-SPEC-ASSOC-CORP-IND-NI,
          :RUPOL-SAL-TST-RSLT-CD       :ZUPOL-SAL-TST-RSLT-CD-NI,
          :RUPOL-SPCL-NOTE-INTNT-IND   :ZUPOL-SPCL-NOTE-INTNT-IND-NI,
          :RUPOL-ANTY-DONAT-FORM-IND   :ZUPOL-ANTY-DONAT-FORM-IND-NI,
          :RUPOL-POL-PRVD-TYP-CD,
		  :RUPOL-POL-ECERT-IND         :ZUPOL-POL-ECERT-IND-NI,
          :RUPOL-TRXN-CNFRM-OS-IND     :ZUPOL-TRXN-CNFRM-OS-IND-NI,
          :RUPOL-MTHLY-STD-PREM-AMT    :ZUPOL-MTHLY-STD-PREM-AMT-NI,
          :RUPOL-CPN-AUTO-PAYO-IND     :ZUPOL-CPN-AUTO-PAYO-IND-NI,
          :RUPOL-JPY-PAYO-IND          :ZUPOL-JPY-PAYO-IND-NI,
          :RUPOL-PAYO-BNK-ID           :ZUPOL-PAYO-BNK-ID-NI,
          :RUPOL-PAYO-BNK-BR-ID        :ZUPOL-PAYO-BNK-BR-ID-NI,
          :RUPOL-PAYO-BNK-ACCT-ID      :ZUPOL-PAYO-BNK-ACCT-ID-NI,
          :RUPOL-PAYO-BNK-ACCT-TYP-CD  :ZUPOL-PAYO-ACCT-TYP-CD-NI,
          :RUPOL-PAYO-BNK-ACC-HLDR-NM  :ZUPOL-PAYO-ACC-HLDR-NM-NI
           END-EXEC.


           EVALUATE SQLCODE

               WHEN ZERO
                    SET  WUPOA-IO-OK         TO  TRUE
                    PERFORM  UPOL-2000-SET-NULL-DFLT
                        THRU UPOL-2000-SET-NULL-DFLT-X
                    MOVE RUPOL-APP-UPLD-DT   TO  WUPOA-APP-UPLD-DT

               WHEN +100
                    SET  WUPOA-IO-EOF        TO  TRUE

               WHEN OTHER
                    SET  WUPOA-IO-ERROR      TO  TRUE

           END-EVALUATE.


       310G-FETCH-NEXT-X.
           EXIT.


      *********************
       4000-EXEC-CLOSE-CUR.
      *********************

           EVALUATE WS-OPTM-SQL-EXEC

               WHEN '01'
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
       410G-CLOSE-BROWSE-CUR.
      ***********************

           EXEC SQL
                CLOSE BCUR_UPOA
           END-EXEC.


           EVALUATE SQLCODE

               WHEN ZERO
                    SET  WUPOA-IO-OK         TO  TRUE

               WHEN OTHER
                    SET  WUPOA-IO-ERROR      TO  TRUE

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

           SET  WUPOA-IO-ERROR               TO  TRUE.
           MOVE WS-OPTM-SQL-REQIR            TO  WUPOA-OPTM-SQL-REQIR.
           MOVE WS-OPTM-SQL-EXEC             TO  WUPOA-OPTM-SQL-EXEC.

           EVALUATE TRUE

               WHEN WUPOA-OPTM-SQL-EXEC = WUPOA-OPTM-SQL-REQIR
                   SET WUPOA-OPTM-SQL-OK     TO  TRUE

               WHEN WUPOA-OPTM-SQL-EXEC = SPACES
                   SET WUPOA-OPTM-SQL-ERROR  TO  TRUE

               WHEN OTHER
                   SET WUPOA-OPTM-SQL-IMPRV  TO  TRUE

           END-EVALUATE.


       5000-EXEC-BROWSE-INDEX-X.
           EXIT.


      ****************************
       6000-EXEC-FETCH-NEXT-INDEX.
      ****************************

      * PROFILE INDICATED THAT -FETCH-NEXT-INDEX
      * PARAGRAPHS ARE NOT APPLICABLE

           SET  WUPOA-IO-ERROR               TO  TRUE.
           SET  WUPOA-OPTM-SQL-OK            TO  TRUE.
           MOVE SPACES                       TO  WUPOA-OPTM-SQL-REQIR.
           MOVE SPACES                       TO  WUPOA-OPTM-SQL-EXEC.


       6000-EXEC-FETCH-NEXT-INDEX-X.
           EXIT.


      ***********************
       7000-EXEC-CLOSE-INDEX.
      ***********************

      * PROFILE INDICATED THAT -CLOSE-BROWSE-INDEX
      * PARAGRAPHS ARE NOT APPLICABLE

           SET  WUPOA-IO-ERROR               TO  TRUE.
           SET  WS-OPTM-SQL-CUR-CLOSED       TO  TRUE.


       7000-EXEC-CLOSE-INDEX-X.
           EXIT.


       COPY ACPZUPOL.

      *****************************************************************
      **                 END OF PROGRAM ASIBUPOA                     **
      *****************************************************************
