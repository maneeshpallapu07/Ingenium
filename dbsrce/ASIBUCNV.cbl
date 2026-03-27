      *************************
       IDENTIFICATION DIVISION.
      *************************

       PROGRAM-ID. ASIBUCNV.

       COPY XCWWCRHT.

      *****************************************************************
      **  MEMBER :  ASIBUCNV                                         **
      **  REMARKS:  SQL I/O PROGRAM USED TO BROWSE ROWS IN THE       **
      **            APPLICATION UPLOAD POLICY CONVERSION TABLE       **
      *****************************************************************
      **  RELEASE   DESCRIPTION                                      **
      **                                                             **
      **  15AUG02   CREATED FOR UCNV TABLE PROCESSING                **
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

       COPY XCWWPGWS REPLACING '$VAR1' BY 'ASIBUCNV'.

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
           EXEC SQL INCLUDE ACWZUCNV  END-EXEC.
      /
      *****************
       LINKAGE SECTION.
      *****************

           EXEC SQL INCLUDE SQLCA     END-EXEC.

           EXEC SQL INCLUDE ACFWUCNV  END-EXEC.

           EXEC SQL INCLUDE ACFRUCNV  END-EXEC.

      /
       PROCEDURE DIVISION USING SQLCA
                                WUCNV-IO-WORK-AREA
                                RUCNV-REC-INFO.

      ***************
       0000-MAINLINE.
      ***************

           IF  WS-WA-ADDRESS NOT = ADDRESS OF WUCNV-IO-WORK-AREA
           OR  WS-TL-ADDRESS NOT = ADDRESS OF RUCNV-REC-INFO
               SET  WS-WA-ADDRESS  TO  ADDRESS OF WUCNV-IO-WORK-AREA
               SET  WS-TL-ADDRESS  TO  ADDRESS OF RUCNV-REC-INFO
               MOVE ZERO           TO  SQL-INIT-FLAG
           END-IF.


           EVALUATE TRUE

               WHEN WUCNV-RQST-BROWSE
                    PERFORM  1000-DETERMINE-SQL-REQIR
                        THRU 1000-DETERMINE-SQL-REQIR-X
                    PERFORM  2000-EXEC-BROWSE
                        THRU 2000-EXEC-BROWSE-X

               WHEN WUCNV-RQST-FETCH-NEXT
                    PERFORM  3000-EXEC-FETCH-NEXT
                        THRU 3000-EXEC-FETCH-NEXT-X

               WHEN WUCNV-RQST-CLOSE-BROWSE-CUR
                    PERFORM  4000-EXEC-CLOSE-CUR
                        THRU 4000-EXEC-CLOSE-CUR-X

               WHEN WUCNV-RQST-BROWSE-INDEX
                    PERFORM  1000-DETERMINE-SQL-REQIR
                        THRU 1000-DETERMINE-SQL-REQIR-X
                    PERFORM  5000-EXEC-BROWSE-INDEX
                        THRU 5000-EXEC-BROWSE-INDEX-X

               WHEN WUCNV-RQST-FETCH-NEXT-INDEX
                    PERFORM  6000-EXEC-FETCH-NEXT-INDEX
                        THRU 6000-EXEC-FETCH-NEXT-INDEX-X

               WHEN WUCNV-RQST-CLOSE-BROWSE-INDEX
                    PERFORM  7000-EXEC-CLOSE-INDEX
                        THRU 7000-EXEC-CLOSE-INDEX-X

               WHEN OTHER
                    SET  WUCNV-IO-ERROR      TO  TRUE

           END-EVALUATE.


           GOBACK.


       0000-MAINLINE-X.
           EXIT.


      **************************
       1000-DETERMINE-SQL-REQIR.
      **************************

           MOVE 2                            TO  WS-OPTM-SQL-REQIR.

           IF  WUCNV-APP-ID = WUCNV-ENDBR-APP-ID
               SUBTRACT +1                   FROM WS-OPTM-SQL-REQIR
           ELSE
               GO TO 1000-DETERMINE-SQL-REQIR-X
           END-IF.


           IF  WUCNV-SEQ-NUM = WUCNV-ENDBR-SEQ-NUM
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
                    SET  WUCNV-IO-ERROR      TO  TRUE

           END-EVALUATE.


           MOVE WS-OPTM-SQL-REQIR            TO  WUCNV-OPTM-SQL-REQIR.
           MOVE WS-OPTM-SQL-EXEC             TO  WUCNV-OPTM-SQL-EXEC.

           EVALUATE TRUE

               WHEN WUCNV-OPTM-SQL-EXEC = WUCNV-OPTM-SQL-REQIR
                   SET WUCNV-OPTM-SQL-OK     TO  TRUE

               WHEN WUCNV-OPTM-SQL-EXEC = SPACES
                   SET WUCNV-OPTM-SQL-ERROR  TO  TRUE

               WHEN OTHER
                   SET WUCNV-OPTM-SQL-IMPRV  TO  TRUE

           END-EVALUATE.


       2000-EXEC-BROWSE-X.
           EXIT.


      *************
       2101-BROWSE.
      *************

           EXEC SQL
             DECLARE B1CUR_UCNV CURSOR FOR
             SELECT
                 APP_ID,
                 SEQ_NUM,
                 CNVR_ISS_EFF_DT,
                 CNVR_XPRY_DT,
                 ORIG_SML_PROD_CD,
                 ORIG_POL_ID,
                 ORIG_POL_ISS_DT,
                 ORIG_PMT_MTHD_CD,
                 ORIG_POL_MAT_DT,
                 ESC_XEMP_RSRV_AMT,
                 ESC_XEMP_SPREM_AMT,
                 ESC_RSRV_AMT,
                 ESC_SPREM_AMT,
                 ESC_ADJ_CHRG_AMT,
                 ORIG_MO_PREM_AMT,
                 CNVR_ORIG_DB_AMT,
                 CNVR_ORIG_HOSP_AMT,
                 ORIG_LTD_PREM_PD,
                 ORIG_TOT_LOAN_AMT,
                 ORIG_AFYC_AMT,
                 ORIG_CPREM_AMT,
                 ORIG_COMM_FACE_AMT,
                 ORIG_PAY_NUM
             FROM TUCNV
             WHERE
                 APP_ID   = :WUCNV-APP-ID
               AND
                 SEQ_NUM  BETWEEN
                            :WUCNV-SEQ-NUM       AND
                            :WUCNV-ENDBR-SEQ-NUM
             ORDER BY
                 APP_ID,
                 SEQ_NUM
             FOR FETCH ONLY
             OPTIMIZE FOR 1 ROW
           END-EXEC.

           EXEC SQL
                OPEN B1CUR_UCNV
           END-EXEC.


           EVALUATE SQLCODE

               WHEN ZERO
                    SET  WUCNV-IO-OK         TO  TRUE

               WHEN OTHER
                    SET  WUCNV-IO-ERROR      TO  TRUE

           END-EVALUATE.


       2101-BROWSE-X.
           EXIT.


      *************
       210G-BROWSE.
      *************

           EXEC SQL
             DECLARE BCUR_UCNV CURSOR FOR
             SELECT
                 APP_ID,
                 SEQ_NUM,
                 CNVR_ISS_EFF_DT,
                 CNVR_XPRY_DT,
                 ORIG_SML_PROD_CD,
                 ORIG_POL_ID,
                 ORIG_POL_ISS_DT,
                 ORIG_PMT_MTHD_CD,
                 ORIG_POL_MAT_DT,
                 ESC_XEMP_RSRV_AMT,
                 ESC_XEMP_SPREM_AMT,
                 ESC_RSRV_AMT,
                 ESC_SPREM_AMT,
                 ESC_ADJ_CHRG_AMT,
                 ORIG_MO_PREM_AMT,
                 CNVR_ORIG_DB_AMT,
                 CNVR_ORIG_HOSP_AMT,
                 ORIG_LTD_PREM_PD,
                 ORIG_TOT_LOAN_AMT,
                 ORIG_AFYC_AMT,
                 ORIG_CPREM_AMT,
                 ORIG_COMM_FACE_AMT,
                 ORIG_PAY_NUM
             FROM TUCNV
             WHERE
                 APP_ID   BETWEEN
                            :WUCNV-APP-ID          AND
                            :WUCNV-ENDBR-APP-ID
               AND
                (SEQ_NUM >= :WUCNV-SEQ-NUM         OR
                 APP_ID   > :WUCNV-APP-ID)
               AND
                (SEQ_NUM <= :WUCNV-ENDBR-SEQ-NUM   OR
                 APP_ID   < :WUCNV-ENDBR-APP-ID)
             ORDER BY
                 APP_ID,
                 SEQ_NUM
             FOR FETCH ONLY
             OPTIMIZE FOR 1 ROW
           END-EXEC.

           EXEC SQL
                OPEN BCUR_UCNV
           END-EXEC.


           EVALUATE SQLCODE

               WHEN ZERO
                    SET  WUCNV-IO-OK         TO  TRUE

               WHEN OTHER
                    SET  WUCNV-IO-ERROR      TO  TRUE

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


           SET  WUCNV-OPTM-SQL-OK            TO  TRUE.
           MOVE SPACES                       TO  WUCNV-OPTM-SQL-REQIR.
           MOVE SPACES                       TO  WUCNV-OPTM-SQL-EXEC.


       3000-EXEC-FETCH-NEXT-X.
           EXIT.


      *****************
       3101-FETCH-NEXT.
      *****************

           MOVE LOW-VALUES                   TO  ZUCNV-NULL-INDICATORS.

           EXEC SQL
             FETCH B1CUR_UCNV
             INTO
                :RUCNV-APP-ID,
                :RUCNV-SEQ-NUM,
                :RUCNV-CNVR-ISS-EFF-DT    :ZUCNV-CNVR-ISS-EFF-DT-NI,
                :RUCNV-CNVR-XPRY-DT       :ZUCNV-CNVR-XPRY-DT-NI,
                :RUCNV-ORIG-SML-PROD-CD   :ZUCNV-ORIG-SML-PROD-CD-NI,
                :RUCNV-ORIG-POL-ID        :ZUCNV-ORIG-POL-ID-NI,
                :RUCNV-ORIG-POL-ISS-DT    :ZUCNV-ORIG-POL-ISS-DT-NI,
                :RUCNV-ORIG-PMT-MTHD-CD   :ZUCNV-ORIG-PMT-MTHD-CD-NI,
                :RUCNV-ORIG-POL-MAT-DT    :ZUCNV-ORIG-POL-MAT-DT-NI,
                :RUCNV-ESC-XEMP-RSRV-AMT  :ZUCNV-ESC-XEMP-RSRV-AMT-NI,
                :RUCNV-ESC-XEMP-SPREM-AMT :ZUCNV-ESC-XEMP-SPREM-AMT-NI,
                :RUCNV-ESC-RSRV-AMT       :ZUCNV-ESC-RSRV-AMT-NI,
                :RUCNV-ESC-SPREM-AMT      :ZUCNV-ESC-SPREM-AMT-NI,
                :RUCNV-ESC-ADJ-CHRG-AMT   :ZUCNV-ESC-ADJ-CHRG-AMT-NI,
                :RUCNV-ORIG-MO-PREM-AMT   :ZUCNV-ORIG-MO-PREM-AMT-NI,
                :RUCNV-CNVR-ORIG-DB-AMT   :ZUCNV-CNVR-ORIG-DB-AMT-NI,
                :RUCNV-CNVR-ORIG-HOSP-AMT :ZUCNV-CNVR-ORIG-HOSP-AMT-NI,
                :RUCNV-ORIG-LTD-PREM-PD   :ZUCNV-ORIG-LTD-PREM-PD-NI,
                :RUCNV-ORIG-TOT-LOAN-AMT  :ZUCNV-ORIG-TOT-LOAN-AMT-NI,
                :RUCNV-ORIG-AFYC-AMT      :ZUCNV-ORIG-AFYC-AMT-NI,
                :RUCNV-ORIG-CPREM-AMT     :ZUCNV-ORIG-CPREM-AMT-NI,
                :RUCNV-ORIG-COMM-FACE-AMT :ZUCNV-ORIG-COMM-FACE-AMT-NI,
                :RUCNV-ORIG-PAY-NUM       :ZUCNV-ORIG-PAY-NUM-NI
           END-EXEC.


           EVALUATE SQLCODE

               WHEN ZERO
                    SET  WUCNV-IO-OK         TO  TRUE
                    PERFORM  UCNV-2000-SET-NULL-DFLT
                        THRU UCNV-2000-SET-NULL-DFLT-X
                    MOVE RUCNV-KEY           TO  WUCNV-KEY

               WHEN +100
                    SET  WUCNV-IO-EOF        TO  TRUE

               WHEN OTHER
                    SET  WUCNV-IO-ERROR      TO  TRUE

           END-EVALUATE.


       3101-FETCH-NEXT-X.
           EXIT.


      *****************
       310G-FETCH-NEXT.
      *****************

           MOVE LOW-VALUES                   TO  ZUCNV-NULL-INDICATORS.

           EXEC SQL
             FETCH BCUR_UCNV
             INTO
                :RUCNV-APP-ID,
                :RUCNV-SEQ-NUM,
                :RUCNV-CNVR-ISS-EFF-DT    :ZUCNV-CNVR-ISS-EFF-DT-NI,
                :RUCNV-CNVR-XPRY-DT       :ZUCNV-CNVR-XPRY-DT-NI,
                :RUCNV-ORIG-SML-PROD-CD   :ZUCNV-ORIG-SML-PROD-CD-NI,
                :RUCNV-ORIG-POL-ID        :ZUCNV-ORIG-POL-ID-NI,
                :RUCNV-ORIG-POL-ISS-DT    :ZUCNV-ORIG-POL-ISS-DT-NI,
                :RUCNV-ORIG-PMT-MTHD-CD   :ZUCNV-ORIG-PMT-MTHD-CD-NI,
                :RUCNV-ORIG-POL-MAT-DT    :ZUCNV-ORIG-POL-MAT-DT-NI,
                :RUCNV-ESC-XEMP-RSRV-AMT  :ZUCNV-ESC-XEMP-RSRV-AMT-NI,
                :RUCNV-ESC-XEMP-SPREM-AMT :ZUCNV-ESC-XEMP-SPREM-AMT-NI,
                :RUCNV-ESC-RSRV-AMT       :ZUCNV-ESC-RSRV-AMT-NI,
                :RUCNV-ESC-SPREM-AMT      :ZUCNV-ESC-SPREM-AMT-NI,
                :RUCNV-ESC-ADJ-CHRG-AMT   :ZUCNV-ESC-ADJ-CHRG-AMT-NI,
                :RUCNV-ORIG-MO-PREM-AMT   :ZUCNV-ORIG-MO-PREM-AMT-NI,
                :RUCNV-CNVR-ORIG-DB-AMT   :ZUCNV-CNVR-ORIG-DB-AMT-NI,
                :RUCNV-CNVR-ORIG-HOSP-AMT :ZUCNV-CNVR-ORIG-HOSP-AMT-NI,
                :RUCNV-ORIG-LTD-PREM-PD   :ZUCNV-ORIG-LTD-PREM-PD-NI,
                :RUCNV-ORIG-TOT-LOAN-AMT  :ZUCNV-ORIG-TOT-LOAN-AMT-NI,
                :RUCNV-ORIG-AFYC-AMT      :ZUCNV-ORIG-AFYC-AMT-NI,
                :RUCNV-ORIG-CPREM-AMT     :ZUCNV-ORIG-CPREM-AMT-NI,
                :RUCNV-ORIG-COMM-FACE-AMT :ZUCNV-ORIG-COMM-FACE-AMT-NI,
                :RUCNV-ORIG-PAY-NUM       :ZUCNV-ORIG-PAY-NUM-NI
           END-EXEC.


           EVALUATE SQLCODE

               WHEN ZERO
                    SET  WUCNV-IO-OK         TO  TRUE
                    PERFORM  UCNV-2000-SET-NULL-DFLT
                        THRU UCNV-2000-SET-NULL-DFLT-X
                    MOVE RUCNV-KEY           TO  WUCNV-KEY

               WHEN +100
                    SET  WUCNV-IO-EOF        TO  TRUE

               WHEN OTHER
                    SET  WUCNV-IO-ERROR      TO  TRUE

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
                CLOSE B1CUR_UCNV
           END-EXEC.


           EVALUATE SQLCODE

               WHEN ZERO
                    SET  WUCNV-IO-OK         TO  TRUE

               WHEN OTHER
                    SET  WUCNV-IO-ERROR      TO  TRUE

           END-EVALUATE.


       4101-CLOSE-BROWSE-CUR-X.
           EXIT.


      ***********************
       410G-CLOSE-BROWSE-CUR.
      ***********************

           EXEC SQL
                CLOSE BCUR_UCNV
           END-EXEC.


           EVALUATE SQLCODE

               WHEN ZERO
                    SET  WUCNV-IO-OK         TO  TRUE

               WHEN OTHER
                    SET  WUCNV-IO-ERROR      TO  TRUE

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

           SET  WUCNV-IO-ERROR               TO  TRUE.
           MOVE WS-OPTM-SQL-REQIR            TO  WUCNV-OPTM-SQL-REQIR.
           MOVE WS-OPTM-SQL-EXEC             TO  WUCNV-OPTM-SQL-EXEC.

           EVALUATE TRUE

               WHEN WUCNV-OPTM-SQL-EXEC = WUCNV-OPTM-SQL-REQIR
                   SET WUCNV-OPTM-SQL-OK     TO  TRUE

               WHEN WUCNV-OPTM-SQL-EXEC = SPACES
                   SET WUCNV-OPTM-SQL-ERROR  TO  TRUE

               WHEN OTHER
                   SET WUCNV-OPTM-SQL-IMPRV  TO  TRUE

           END-EVALUATE.


       5000-EXEC-BROWSE-INDEX-X.
           EXIT.


      ****************************
       6000-EXEC-FETCH-NEXT-INDEX.
      ****************************

      * PROFILE INDICATED THAT -FETCH-NEXT-INDEX
      * PARAGRAPHS ARE NOT APPLICABLE

           SET  WUCNV-IO-ERROR               TO  TRUE.
           SET  WUCNV-OPTM-SQL-OK            TO  TRUE.
           MOVE SPACES                       TO  WUCNV-OPTM-SQL-REQIR.
           MOVE SPACES                       TO  WUCNV-OPTM-SQL-EXEC.


       6000-EXEC-FETCH-NEXT-INDEX-X.
           EXIT.


      ***********************
       7000-EXEC-CLOSE-INDEX.
      ***********************

      * PROFILE INDICATED THAT -CLOSE-BROWSE-INDEX
      * PARAGRAPHS ARE NOT APPLICABLE

           SET  WUCNV-IO-ERROR               TO  TRUE.
           SET  WS-OPTM-SQL-CUR-CLOSED       TO  TRUE.


       7000-EXEC-CLOSE-INDEX-X.
           EXIT.


       COPY ACPZUCNV.

      *****************************************************************
      **                 END OF PROGRAM ASIBUCNV                     **
      *****************************************************************
