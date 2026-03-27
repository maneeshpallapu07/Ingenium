      *************************
       IDENTIFICATION DIVISION.
      *************************

       PROGRAM-ID. ASIBUCVG.

       COPY XCWWCRHT.

      *****************************************************************
      **  MEMBER :  ASIBUCVG                                         **
      **  REMARKS:  SQL I/O PROGRAM USED TO BROWSE ROWS IN THE       **
      **            APPLICATION UPLOAD COVERAGE TABLE                **
      *****************************************************************
      **  RELEASE   DESCRIPTION                                      **
      **                                                             **
      **  6.5       CREATED FOR UCVG PROCESSING                      **
TLB002**            CHANGES FOR TLB	PRODUCTS                         **
UYS002**  27FEB21  FIELD ADDED FOR COLI PRODUCTS                     ** 
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

       COPY XCWWPGWS REPLACING '$VAR1' BY 'ASIBUCVG'.

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
           EXEC SQL INCLUDE ACWZUCVG  END-EXEC.
      /
      *****************
       LINKAGE SECTION.
      *****************

           EXEC SQL INCLUDE SQLCA     END-EXEC.

           EXEC SQL INCLUDE ACFWUCVG  END-EXEC.

           EXEC SQL INCLUDE ACFRUCVG  END-EXEC.

      /
       PROCEDURE DIVISION USING SQLCA
                                WUCVG-IO-WORK-AREA
                                RUCVG-REC-INFO.

      ***************
       0000-MAINLINE.
      ***************

           IF  WS-WA-ADDRESS NOT = ADDRESS OF WUCVG-IO-WORK-AREA
           OR  WS-TL-ADDRESS NOT = ADDRESS OF RUCVG-REC-INFO
               SET  WS-WA-ADDRESS  TO  ADDRESS OF WUCVG-IO-WORK-AREA
               SET  WS-TL-ADDRESS  TO  ADDRESS OF RUCVG-REC-INFO
               MOVE ZERO           TO  SQL-INIT-FLAG
           END-IF.


           EVALUATE TRUE

               WHEN WUCVG-RQST-BROWSE
                    PERFORM  1000-DETERMINE-SQL-REQIR
                        THRU 1000-DETERMINE-SQL-REQIR-X
                    PERFORM  2000-EXEC-BROWSE
                        THRU 2000-EXEC-BROWSE-X

               WHEN WUCVG-RQST-FETCH-NEXT
                    PERFORM  3000-EXEC-FETCH-NEXT
                        THRU 3000-EXEC-FETCH-NEXT-X

               WHEN WUCVG-RQST-CLOSE-BROWSE-CUR
                    PERFORM  4000-EXEC-CLOSE-CUR
                        THRU 4000-EXEC-CLOSE-CUR-X

               WHEN WUCVG-RQST-BROWSE-INDEX
                    PERFORM  1000-DETERMINE-SQL-REQIR
                        THRU 1000-DETERMINE-SQL-REQIR-X
                    PERFORM  5000-EXEC-BROWSE-INDEX
                        THRU 5000-EXEC-BROWSE-INDEX-X

               WHEN WUCVG-RQST-FETCH-NEXT-INDEX
                    PERFORM  6000-EXEC-FETCH-NEXT-INDEX
                        THRU 6000-EXEC-FETCH-NEXT-INDEX-X

               WHEN WUCVG-RQST-CLOSE-BROWSE-INDEX
                    PERFORM  7000-EXEC-CLOSE-INDEX
                        THRU 7000-EXEC-CLOSE-INDEX-X

               WHEN OTHER
                    SET  WUCVG-IO-ERROR      TO  TRUE

           END-EVALUATE.


           GOBACK.


       0000-MAINLINE-X.
           EXIT.


      **************************
       1000-DETERMINE-SQL-REQIR.
      **************************

           MOVE 3                            TO  WS-OPTM-SQL-REQIR.

           IF  WUCVG-APP-ID = WUCVG-ENDBR-APP-ID
               SUBTRACT +1                   FROM WS-OPTM-SQL-REQIR
           ELSE
               GO TO 1000-DETERMINE-SQL-REQIR-X
           END-IF.


           IF  WUCVG-STCKR-ID = WUCVG-ENDBR-STCKR-ID
               SUBTRACT +1                   FROM WS-OPTM-SQL-REQIR
           ELSE
               GO TO 1000-DETERMINE-SQL-REQIR-X
           END-IF.


           IF  WUCVG-PLAN-ID = WUCVG-ENDBR-PLAN-ID
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

               WHEN WS-OPTM-SQL-REQIR <= 2
                    MOVE '02'                TO  WS-OPTM-SQL-EXEC
                    PERFORM  2102-BROWSE
                        THRU 2102-BROWSE-X

               WHEN WS-OPTM-SQL-REQIR <= 3
                    MOVE '03'                TO  WS-OPTM-SQL-EXEC
                    PERFORM  210G-BROWSE
                        THRU 210G-BROWSE-X

               WHEN OTHER
                    MOVE SPACES              TO  WS-OPTM-SQL-EXEC
                    SET  WUCVG-IO-ERROR      TO  TRUE

           END-EVALUATE.


           MOVE WS-OPTM-SQL-REQIR            TO  WUCVG-OPTM-SQL-REQIR.
           MOVE WS-OPTM-SQL-EXEC             TO  WUCVG-OPTM-SQL-EXEC.

           EVALUATE TRUE

               WHEN WUCVG-OPTM-SQL-EXEC = WUCVG-OPTM-SQL-REQIR
                   SET WUCVG-OPTM-SQL-OK     TO  TRUE

               WHEN WUCVG-OPTM-SQL-EXEC = SPACES
                   SET WUCVG-OPTM-SQL-ERROR  TO  TRUE

               WHEN OTHER
                   SET WUCVG-OPTM-SQL-IMPRV  TO  TRUE

           END-EVALUATE.


       2000-EXEC-BROWSE-X.
           EXIT.


      *************
       2102-BROWSE.
      *************

           EXEC SQL
             DECLARE B2CUR_UCVG CURSOR FOR
             SELECT
                 APP_ID,
                 STCKR_ID,
                 PLAN_ID,
                 INIT_COV_AMT,
                 SMKR_CD,
                 DUR_YR_CD,
                 CVG_STBL_2_CD,
                 CVG_ILLUS_CD,
                 SA_INIT_PREM_AMT,
                 SA_SUBSEQ_PREM_AMT,
                 SA_INIT_LMPSM_AMT,
                 SA_CNVR_FND_AMT,
                 CVG_XPCT_ANTY_AGE,
                 CVG_STBL_4_CD,
                 CVG_STBL_3_CD,
                 CVG_WP_IND,
                 CVG_PRIM_GR_ID,
                 TAX_QUALF_IND,
                 TRG_HIT_CNVR_RT,
                 FRST_POL_PERI_DUR,
                 CANCER_TYP_CD
             FROM TUCVG
             WHERE
                 APP_ID    = :WUCVG-APP-ID
               AND
                 STCKR_ID  BETWEEN
                             :WUCVG-STCKR-ID         AND
                             :WUCVG-ENDBR-STCKR-ID
               AND
                (PLAN_ID  >= :WUCVG-PLAN-ID          OR
                 STCKR_ID  > :WUCVG-STCKR-ID)
               AND
                (PLAN_ID  <= :WUCVG-ENDBR-PLAN-ID    OR
                 STCKR_ID  < :WUCVG-ENDBR-STCKR-ID)
             ORDER BY
                 APP_ID,
                 STCKR_ID,
                 PLAN_ID
             FOR FETCH ONLY
             OPTIMIZE FOR 1 ROW
           END-EXEC.

           EXEC SQL
                OPEN B2CUR_UCVG
           END-EXEC.


           EVALUATE SQLCODE

               WHEN ZERO
                    SET  WUCVG-IO-OK         TO  TRUE

               WHEN OTHER
                    SET  WUCVG-IO-ERROR      TO  TRUE

           END-EVALUATE.


       2102-BROWSE-X.
           EXIT.


      *************
       210G-BROWSE.
      *************

           EXEC SQL
             DECLARE BCUR_UCVG CURSOR FOR
             SELECT
                 APP_ID,
                 STCKR_ID,
                 PLAN_ID,
                 INIT_COV_AMT,
                 SMKR_CD,
                 DUR_YR_CD,
                 CVG_STBL_2_CD,
                 CVG_ILLUS_CD,
                 SA_INIT_PREM_AMT,
                 SA_SUBSEQ_PREM_AMT,
                 SA_INIT_LMPSM_AMT,
                 SA_CNVR_FND_AMT,
                 CVG_XPCT_ANTY_AGE,
                 CVG_STBL_4_CD,
                 CVG_STBL_3_CD,
                 CVG_WP_IND,
                 CVG_PRIM_GR_ID,
                 TAX_QUALF_IND,
                 TRG_HIT_CNVR_RT,
                 FRST_POL_PERI_DUR,
                 CANCER_TYP_CD
             FROM TUCVG
             WHERE
                 APP_ID    BETWEEN
                             :WUCVG-APP-ID           AND
                             :WUCVG-ENDBR-APP-ID
               AND
               ((STCKR_ID  = :WUCVG-STCKR-ID         AND
                 PLAN_ID  >= :WUCVG-PLAN-ID)         OR
                 STCKR_ID  > :WUCVG-STCKR-ID         OR
                 APP_ID    > :WUCVG-APP-ID)
               AND
               ((STCKR_ID  = :WUCVG-ENDBR-STCKR-ID   AND
                 PLAN_ID  <= :WUCVG-ENDBR-PLAN-ID)   OR
                 STCKR_ID  < :WUCVG-ENDBR-STCKR-ID   OR
                 APP_ID    < :WUCVG-ENDBR-APP-ID)
             ORDER BY
                 APP_ID,
                 STCKR_ID,
                 PLAN_ID
             FOR FETCH ONLY
             OPTIMIZE FOR 1 ROW
           END-EXEC.

           EXEC SQL
                OPEN BCUR_UCVG
           END-EXEC.


           EVALUATE SQLCODE

               WHEN ZERO
                    SET  WUCVG-IO-OK         TO  TRUE

               WHEN OTHER
                    SET  WUCVG-IO-ERROR      TO  TRUE

           END-EVALUATE.


       210G-BROWSE-X.
           EXIT.


      **********************
       3000-EXEC-FETCH-NEXT.
      **********************

           EVALUATE WS-OPTM-SQL-EXEC

               WHEN '02'
                    PERFORM  3102-FETCH-NEXT
                        THRU 3102-FETCH-NEXT-X

               WHEN '03'
                    PERFORM  310G-FETCH-NEXT
                        THRU 310G-FETCH-NEXT-X

               WHEN OTHER
                    PERFORM  310G-FETCH-NEXT
                        THRU 310G-FETCH-NEXT-X

           END-EVALUATE.


           SET  WUCVG-OPTM-SQL-OK            TO  TRUE.
           MOVE SPACES                       TO  WUCVG-OPTM-SQL-REQIR.
           MOVE SPACES                       TO  WUCVG-OPTM-SQL-EXEC.


       3000-EXEC-FETCH-NEXT-X.
           EXIT.


      *****************
       3102-FETCH-NEXT.
      *****************

           MOVE LOW-VALUES                   TO  ZUCVG-NULL-INDICATORS.

           EXEC SQL
             FETCH B2CUR_UCVG
             INTO
                :RUCVG-APP-ID,
                :RUCVG-STCKR-ID,
                :RUCVG-PLAN-ID,
                :RUCVG-INIT-COV-AMT       :ZUCVG-INIT-COV-AMT-NI,
                :RUCVG-SMKR-CD            :ZUCVG-SMKR-CD-NI,
                :RUCVG-DUR-YR-CD          :ZUCVG-DUR-YR-CD-NI,
                :RUCVG-CVG-STBL-2-CD      :ZUCVG-CVG-STBL-2-CD-NI,
                :RUCVG-CVG-ILLUS-CD       :ZUCVG-CVG-ILLUS-CD-NI,
                :RUCVG-SA-INIT-PREM-AMT   :ZUCVG-SA-INIT-PREM-AMT-NI,
                :RUCVG-SA-SUBSEQ-PREM-AMT :ZUCVG-SA-SUBSEQ-PREM-AMT-NI,
                :RUCVG-SA-INIT-LMPSM-AMT  :ZUCVG-SA-INIT-LMPSM-AMT-NI,
                :RUCVG-SA-CNVR-FND-AMT    :ZUCVG-SA-CNVR-FND-AMT-NI,
                :RUCVG-CVG-XPCT-ANTY-AGE  :ZUCVG-CVG-XPCT-ANTY-AGE-NI,
                :RUCVG-CVG-STBL-4-CD      :ZUCVG-CVG-STBL-4-CD-NI,
                :RUCVG-CVG-STBL-3-CD      :ZUCVG-CVG-STBL-3-CD-NI,
                :RUCVG-CVG-WP-IND         :ZUCVG-CVG-WP-IND-NI,
                :RUCVG-CVG-PRIM-GR-ID     :ZUCVG-CVG-PRIM-GR-ID-NI,
                :RUCVG-TAX-QUALF-IND      :ZUCVG-TAX-QUALF-IND-NI,
                :RUCVG-TRG-HIT-CNVR-RT    :ZUCVG-TRG-HIT-CNVR-RT-NI,
                :RUCVG-FRST-POL-PERI-DUR  :ZUCVG-FRST-POL-PERI-DUR-NI,
                :RUCVG-CANCER-TYP-CD      :ZUCVG-CANCER-TYP-CD-NI
           END-EXEC.


           EVALUATE SQLCODE

               WHEN ZERO
                    SET  WUCVG-IO-OK         TO  TRUE
                    PERFORM  UCVG-2000-SET-NULL-DFLT
                        THRU UCVG-2000-SET-NULL-DFLT-X
                    MOVE RUCVG-KEY           TO  WUCVG-KEY

               WHEN +100
                    SET  WUCVG-IO-EOF        TO  TRUE

               WHEN OTHER
                    SET  WUCVG-IO-ERROR      TO  TRUE

           END-EVALUATE.


       3102-FETCH-NEXT-X.
           EXIT.


      *****************
       310G-FETCH-NEXT.
      *****************

           MOVE LOW-VALUES                   TO  ZUCVG-NULL-INDICATORS.

           EXEC SQL
             FETCH BCUR_UCVG
             INTO
                :RUCVG-APP-ID,
                :RUCVG-STCKR-ID,
                :RUCVG-PLAN-ID,
                :RUCVG-INIT-COV-AMT       :ZUCVG-INIT-COV-AMT-NI,
                :RUCVG-SMKR-CD            :ZUCVG-SMKR-CD-NI,
                :RUCVG-DUR-YR-CD          :ZUCVG-DUR-YR-CD-NI,
                :RUCVG-CVG-STBL-2-CD      :ZUCVG-CVG-STBL-2-CD-NI,
                :RUCVG-CVG-ILLUS-CD       :ZUCVG-CVG-ILLUS-CD-NI,
                :RUCVG-SA-INIT-PREM-AMT   :ZUCVG-SA-INIT-PREM-AMT-NI,
                :RUCVG-SA-SUBSEQ-PREM-AMT :ZUCVG-SA-SUBSEQ-PREM-AMT-NI,
                :RUCVG-SA-INIT-LMPSM-AMT  :ZUCVG-SA-INIT-LMPSM-AMT-NI,
                :RUCVG-SA-CNVR-FND-AMT    :ZUCVG-SA-CNVR-FND-AMT-NI,
                :RUCVG-CVG-XPCT-ANTY-AGE  :ZUCVG-CVG-XPCT-ANTY-AGE-NI,
                :RUCVG-CVG-STBL-4-CD      :ZUCVG-CVG-STBL-4-CD-NI,
                :RUCVG-CVG-STBL-3-CD      :ZUCVG-CVG-STBL-3-CD-NI,
                :RUCVG-CVG-WP-IND         :ZUCVG-CVG-WP-IND-NI,
                :RUCVG-CVG-PRIM-GR-ID     :ZUCVG-CVG-PRIM-GR-ID-NI,
                :RUCVG-TAX-QUALF-IND      :ZUCVG-TAX-QUALF-IND-NI,
                :RUCVG-TRG-HIT-CNVR-RT    :ZUCVG-TRG-HIT-CNVR-RT-NI,
                :RUCVG-FRST-POL-PERI-DUR  :ZUCVG-FRST-POL-PERI-DUR-NI,
                :RUCVG-CANCER-TYP-CD      :ZUCVG-CANCER-TYP-CD-NI
           END-EXEC.


           EVALUATE SQLCODE

               WHEN ZERO
                    SET  WUCVG-IO-OK         TO  TRUE
                    PERFORM  UCVG-2000-SET-NULL-DFLT
                        THRU UCVG-2000-SET-NULL-DFLT-X
                    MOVE RUCVG-KEY           TO  WUCVG-KEY

               WHEN +100
                    SET  WUCVG-IO-EOF        TO  TRUE

               WHEN OTHER
                    SET  WUCVG-IO-ERROR      TO  TRUE

           END-EVALUATE.


       310G-FETCH-NEXT-X.
           EXIT.


      *********************
       4000-EXEC-CLOSE-CUR.
      *********************

           EVALUATE WS-OPTM-SQL-EXEC

               WHEN '02'
                    PERFORM  4102-CLOSE-BROWSE-CUR
                        THRU 4102-CLOSE-BROWSE-CUR-X

               WHEN '03'
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
       4102-CLOSE-BROWSE-CUR.
      ***********************

           EXEC SQL
                CLOSE B2CUR_UCVG
           END-EXEC.


           EVALUATE SQLCODE

               WHEN ZERO
                    SET  WUCVG-IO-OK         TO  TRUE

               WHEN OTHER
                    SET  WUCVG-IO-ERROR      TO  TRUE

           END-EVALUATE.


       4102-CLOSE-BROWSE-CUR-X.
           EXIT.


      ***********************
       410G-CLOSE-BROWSE-CUR.
      ***********************

           EXEC SQL
                CLOSE BCUR_UCVG
           END-EXEC.


           EVALUATE SQLCODE

               WHEN ZERO
                    SET  WUCVG-IO-OK         TO  TRUE

               WHEN OTHER
                    SET  WUCVG-IO-ERROR      TO  TRUE

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

           SET  WUCVG-IO-ERROR               TO  TRUE.
           MOVE WS-OPTM-SQL-REQIR            TO  WUCVG-OPTM-SQL-REQIR.
           MOVE WS-OPTM-SQL-EXEC             TO  WUCVG-OPTM-SQL-EXEC.

           EVALUATE TRUE

               WHEN WUCVG-OPTM-SQL-EXEC = WUCVG-OPTM-SQL-REQIR
                   SET WUCVG-OPTM-SQL-OK     TO  TRUE

               WHEN WUCVG-OPTM-SQL-EXEC = SPACES
                   SET WUCVG-OPTM-SQL-ERROR  TO  TRUE

               WHEN OTHER
                   SET WUCVG-OPTM-SQL-IMPRV  TO  TRUE

           END-EVALUATE.


       5000-EXEC-BROWSE-INDEX-X.
           EXIT.


      ****************************
       6000-EXEC-FETCH-NEXT-INDEX.
      ****************************

      * PROFILE INDICATED THAT -FETCH-NEXT-INDEX
      * PARAGRAPHS ARE NOT APPLICABLE

           SET  WUCVG-IO-ERROR               TO  TRUE.
           SET  WUCVG-OPTM-SQL-OK            TO  TRUE.
           MOVE SPACES                       TO  WUCVG-OPTM-SQL-REQIR.
           MOVE SPACES                       TO  WUCVG-OPTM-SQL-EXEC.


       6000-EXEC-FETCH-NEXT-INDEX-X.
           EXIT.


      ***********************
       7000-EXEC-CLOSE-INDEX.
      ***********************

      * PROFILE INDICATED THAT -CLOSE-BROWSE-INDEX
      * PARAGRAPHS ARE NOT APPLICABLE

           SET  WUCVG-IO-ERROR               TO  TRUE.
           SET  WS-OPTM-SQL-CUR-CLOSED       TO  TRUE.


       7000-EXEC-CLOSE-INDEX-X.
           EXIT.


       COPY ACPZUCVG.

      *****************************************************************
      **                 END OF PROGRAM ASIBUCVG                     **
      *****************************************************************
