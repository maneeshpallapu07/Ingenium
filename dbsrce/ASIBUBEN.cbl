      *************************
       IDENTIFICATION DIVISION.
      *************************

       PROGRAM-ID. ASIBUBEN.

       COPY XCWWCRHT.

      *****************************************************************
      **  MEMBER :  ASIBUBEN                                         **
      **  REMARKS:  SQL I/O PROGRAM USED TO BROWSE ROWS IN THE       **
      **            APPLICATION UPLOAD BENEFICIARY TABLE             **
      *****************************************************************
      **  RELEASE   DESCRIPTION                                      **
      **                                                             **
C12392**  11NOV2011 CREATED FOR UBEN PROCESSING                      **
MP270A**  23MAR16  FIELD ADDED AS PART OF E-POS APPLICATION XML      **
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

       COPY XCWWPGWS REPLACING '$VAR1' BY 'ASIBUBEN'.

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
           EXEC SQL INCLUDE ACWZUBEN  END-EXEC.
      /
      *****************
       LINKAGE SECTION.
      *****************

           EXEC SQL INCLUDE SQLCA     END-EXEC.

           EXEC SQL INCLUDE ACFWUBEN  END-EXEC.

           EXEC SQL INCLUDE ACFRUBEN  END-EXEC.

      /
       PROCEDURE DIVISION USING SQLCA
                                WUBEN-IO-WORK-AREA
                                RUBEN-REC-INFO.

      ***************
       0000-MAINLINE.
      ***************

           IF  WS-WA-ADDRESS NOT = ADDRESS OF WUBEN-IO-WORK-AREA
           OR  WS-TL-ADDRESS NOT = ADDRESS OF RUBEN-REC-INFO
               SET  WS-WA-ADDRESS  TO  ADDRESS OF WUBEN-IO-WORK-AREA
               SET  WS-TL-ADDRESS  TO  ADDRESS OF RUBEN-REC-INFO
               MOVE ZERO           TO  SQL-INIT-FLAG
           END-IF.


           EVALUATE TRUE

               WHEN WUBEN-RQST-BROWSE
                    PERFORM  1000-DETERMINE-SQL-REQIR
                        THRU 1000-DETERMINE-SQL-REQIR-X
                    PERFORM  2000-EXEC-BROWSE
                        THRU 2000-EXEC-BROWSE-X

               WHEN WUBEN-RQST-FETCH-NEXT
                    PERFORM  3000-EXEC-FETCH-NEXT
                        THRU 3000-EXEC-FETCH-NEXT-X

               WHEN WUBEN-RQST-CLOSE-BROWSE-CUR
                    PERFORM  4000-EXEC-CLOSE-CUR
                        THRU 4000-EXEC-CLOSE-CUR-X

               WHEN WUBEN-RQST-BROWSE-INDEX
                    PERFORM  1000-DETERMINE-SQL-REQIR
                        THRU 1000-DETERMINE-SQL-REQIR-X
                    PERFORM  5000-EXEC-BROWSE-INDEX
                        THRU 5000-EXEC-BROWSE-INDEX-X

               WHEN WUBEN-RQST-FETCH-NEXT-INDEX
                    PERFORM  6000-EXEC-FETCH-NEXT-INDEX
                        THRU 6000-EXEC-FETCH-NEXT-INDEX-X

               WHEN WUBEN-RQST-CLOSE-BROWSE-INDEX
                    PERFORM  7000-EXEC-CLOSE-INDEX
                        THRU 7000-EXEC-CLOSE-INDEX-X

               WHEN OTHER
                    SET  WUBEN-IO-ERROR      TO  TRUE

           END-EVALUATE.


           GOBACK.


       0000-MAINLINE-X.
           EXIT.


      **************************
       1000-DETERMINE-SQL-REQIR.
      **************************

           MOVE 4                            TO  WS-OPTM-SQL-REQIR.

           IF  WUBEN-APP-ID = WUBEN-ENDBR-APP-ID
               SUBTRACT +1                   FROM WS-OPTM-SQL-REQIR
           ELSE
               GO TO 1000-DETERMINE-SQL-REQIR-X
           END-IF.


           IF  WUBEN-STCKR-ID = WUBEN-ENDBR-STCKR-ID
               SUBTRACT +1                   FROM WS-OPTM-SQL-REQIR
           ELSE
               GO TO 1000-DETERMINE-SQL-REQIR-X
           END-IF.


           IF  WUBEN-BEN-TYP-CD = WUBEN-ENDBR-BEN-TYP-CD
               SUBTRACT +1                   FROM WS-OPTM-SQL-REQIR
           ELSE
               GO TO 1000-DETERMINE-SQL-REQIR-X
           END-IF.


           IF  WUBEN-SEQ-NUM = WUBEN-ENDBR-SEQ-NUM
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

               WHEN WS-OPTM-SQL-REQIR <= 4
                    MOVE '04'                TO  WS-OPTM-SQL-EXEC
                    PERFORM  210G-BROWSE
                        THRU 210G-BROWSE-X

               WHEN OTHER
                    MOVE SPACES              TO  WS-OPTM-SQL-EXEC
                    SET  WUBEN-IO-ERROR      TO  TRUE

           END-EVALUATE.


           MOVE WS-OPTM-SQL-REQIR            TO  WUBEN-OPTM-SQL-REQIR.
           MOVE WS-OPTM-SQL-EXEC             TO  WUBEN-OPTM-SQL-EXEC.

           EVALUATE TRUE

               WHEN WUBEN-OPTM-SQL-EXEC = WUBEN-OPTM-SQL-REQIR
                   SET WUBEN-OPTM-SQL-OK     TO  TRUE

               WHEN WUBEN-OPTM-SQL-EXEC = SPACES
                   SET WUBEN-OPTM-SQL-ERROR  TO  TRUE

               WHEN OTHER
                   SET WUBEN-OPTM-SQL-IMPRV  TO  TRUE

           END-EVALUATE.


       2000-EXEC-BROWSE-X.
           EXIT.


      *************
       2102-BROWSE.
      *************

           EXEC SQL
             DECLARE B2CUR_UBEN CURSOR FOR
             SELECT
                 APP_ID,
                 STCKR_ID,
                 BEN_TYP_CD,
                 SEQ_NUM,
                 BEN_SUR_NM,
                 BEN_GIV_NM,
                 BNFY_CO_NM,
                 BEN_REL_CD,
                 BEN_PCT,
                 BNFY_ANTY_PERI_CD,
                 BNFY_MORAL_RISK_IND,
                 BNFY_KA_GIV_NM,
                 BNFY_KA_SUR_NM,
                 BNFY_CO_KA_NM,
                 BNFY_SEX_CD,
                 BNFY_BTH_DT,
                 BNFY_REL_TYP_CD
             FROM TUBEN
             WHERE
                 APP_ID      = :WUBEN-APP-ID             AND
                 STCKR_ID    = :WUBEN-STCKR-ID
               AND
                 BEN_TYP_CD  BETWEEN
                               :WUBEN-BEN-TYP-CD         AND
                               :WUBEN-ENDBR-BEN-TYP-CD
               AND
                (SEQ_NUM    >= :WUBEN-SEQ-NUM            OR
                 BEN_TYP_CD  > :WUBEN-BEN-TYP-CD)
               AND
                (SEQ_NUM    <= :WUBEN-ENDBR-SEQ-NUM      OR
                 BEN_TYP_CD  < :WUBEN-ENDBR-BEN-TYP-CD)
             ORDER BY
                 APP_ID,
                 STCKR_ID,
                 BEN_TYP_CD,
                 SEQ_NUM
             FOR FETCH ONLY
             OPTIMIZE FOR 1 ROW
           END-EXEC.

           EXEC SQL
                OPEN B2CUR_UBEN
           END-EXEC.


           EVALUATE SQLCODE

               WHEN ZERO
                    SET  WUBEN-IO-OK         TO  TRUE

               WHEN OTHER
                    SET  WUBEN-IO-ERROR      TO  TRUE

           END-EVALUATE.


       2102-BROWSE-X.
           EXIT.


      *************
       210G-BROWSE.
      *************

           EXEC SQL
             DECLARE BCUR_UBEN CURSOR FOR
             SELECT
                 APP_ID,
                 STCKR_ID,
                 BEN_TYP_CD,
                 SEQ_NUM,
                 BEN_SUR_NM,
                 BEN_GIV_NM,
                 BNFY_CO_NM,
                 BEN_REL_CD,
                 BEN_PCT,
                 BNFY_ANTY_PERI_CD,
                 BNFY_MORAL_RISK_IND,
                 BNFY_KA_GIV_NM,
                 BNFY_KA_SUR_NM,
                 BNFY_CO_KA_NM,
                 BNFY_SEX_CD,
                 BNFY_BTH_DT,
                 BNFY_REL_TYP_CD
             FROM TUBEN
             WHERE
                 APP_ID      BETWEEN
                               :WUBEN-APP-ID             AND
                               :WUBEN-ENDBR-APP-ID
               AND
               ((STCKR_ID    = :WUBEN-STCKR-ID           AND
               ((BEN_TYP_CD  = :WUBEN-BEN-TYP-CD         AND
                 SEQ_NUM    >= :WUBEN-SEQ-NUM)           OR
                 BEN_TYP_CD  > :WUBEN-BEN-TYP-CD))       OR
                 STCKR_ID    > :WUBEN-STCKR-ID           OR
                 APP_ID      > :WUBEN-APP-ID)
               AND
               ((STCKR_ID    = :WUBEN-ENDBR-STCKR-ID     AND
               ((BEN_TYP_CD  = :WUBEN-ENDBR-BEN-TYP-CD   AND
                 SEQ_NUM    <= :WUBEN-ENDBR-SEQ-NUM)     OR
                 BEN_TYP_CD  < :WUBEN-ENDBR-BEN-TYP-CD)) OR
                 STCKR_ID    < :WUBEN-ENDBR-STCKR-ID     OR
                 APP_ID      < :WUBEN-ENDBR-APP-ID)
             ORDER BY
                 APP_ID,
                 STCKR_ID,
                 BEN_TYP_CD,
                 SEQ_NUM
             FOR FETCH ONLY
             OPTIMIZE FOR 1 ROW
           END-EXEC.

           EXEC SQL
                OPEN BCUR_UBEN
           END-EXEC.


           EVALUATE SQLCODE

               WHEN ZERO
                    SET  WUBEN-IO-OK         TO  TRUE

               WHEN OTHER
                    SET  WUBEN-IO-ERROR      TO  TRUE

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

               WHEN '04'
                    PERFORM  310G-FETCH-NEXT
                        THRU 310G-FETCH-NEXT-X

               WHEN OTHER
                    PERFORM  310G-FETCH-NEXT
                        THRU 310G-FETCH-NEXT-X

           END-EVALUATE.


           SET  WUBEN-OPTM-SQL-OK            TO  TRUE.
           MOVE SPACES                       TO  WUBEN-OPTM-SQL-REQIR.
           MOVE SPACES                       TO  WUBEN-OPTM-SQL-EXEC.


       3000-EXEC-FETCH-NEXT-X.
           EXIT.


      *****************
       3102-FETCH-NEXT.
      *****************

           MOVE LOW-VALUES                   TO  ZUBEN-NULL-INDICATORS.

           EXEC SQL
             FETCH B2CUR_UBEN
             INTO
                 :RUBEN-APP-ID,
                 :RUBEN-STCKR-ID,
                 :RUBEN-BEN-TYP-CD,
                 :RUBEN-SEQ-NUM,
                 :RUBEN-BEN-SUR-NM        :ZUBEN-BEN-SUR-NM-NI,
                 :RUBEN-BEN-GIV-NM        :ZUBEN-BEN-GIV-NM-NI,
                 :RUBEN-BNFY-CO-NM        :ZUBEN-BNFY-CO-NM-NI,
                 :RUBEN-BEN-REL-CD        :ZUBEN-BEN-REL-CD-NI,
                 :RUBEN-BEN-PCT           :ZUBEN-BEN-PCT-NI,
              :RUBEN-BNFY-ANTY-PERI-CD   :ZUBEN-BNFY-ANTY-PERI-CD-NI,
              :RUBEN-BNFY-MORAL-RISK-IND :ZUBEN-BNFY-MORAL-RISK-IND-NI,
              :RUBEN-BNFY-KA-GIV-NM      :ZUBEN-BNFY-KA-GIV-NM-NI,
              :RUBEN-BNFY-KA-SUR-NM      :ZUBEN-BNFY-KA-SUR-NM-NI,
              :RUBEN-BNFY-CO-KA-NM       :ZUBEN-BNFY-CO-KA-NM-NI,
              :RUBEN-BNFY-SEX-CD         :ZUBEN-BNFY-SEX-CD-NI,
              :RUBEN-BNFY-BTH-DT         :ZUBEN-BNFY-BTH-DT-NI,
              :RUBEN-BNFY-REL-TYP-CD     :ZUBEN-BNFY-REL-TYP-CD-NI
           END-EXEC.


           EVALUATE SQLCODE

               WHEN ZERO
                    SET  WUBEN-IO-OK         TO  TRUE
                    PERFORM  UBEN-2000-SET-NULL-DFLT
                        THRU UBEN-2000-SET-NULL-DFLT-X
                    MOVE RUBEN-KEY           TO  WUBEN-KEY

               WHEN +100
                    SET  WUBEN-IO-EOF        TO  TRUE

               WHEN OTHER
                    SET  WUBEN-IO-ERROR      TO  TRUE

           END-EVALUATE.


       3102-FETCH-NEXT-X.
           EXIT.


      *****************
       310G-FETCH-NEXT.
      *****************

           MOVE LOW-VALUES                   TO  ZUBEN-NULL-INDICATORS.

           EXEC SQL
             FETCH BCUR_UBEN
             INTO
                 :RUBEN-APP-ID,
                 :RUBEN-STCKR-ID,
                 :RUBEN-BEN-TYP-CD,
                 :RUBEN-SEQ-NUM,
                 :RUBEN-BEN-SUR-NM        :ZUBEN-BEN-SUR-NM-NI,
                 :RUBEN-BEN-GIV-NM        :ZUBEN-BEN-GIV-NM-NI,
                 :RUBEN-BNFY-CO-NM        :ZUBEN-BNFY-CO-NM-NI,
                 :RUBEN-BEN-REL-CD        :ZUBEN-BEN-REL-CD-NI,
                 :RUBEN-BEN-PCT           :ZUBEN-BEN-PCT-NI,
              :RUBEN-BNFY-ANTY-PERI-CD   :ZUBEN-BNFY-ANTY-PERI-CD-NI,
              :RUBEN-BNFY-MORAL-RISK-IND :ZUBEN-BNFY-MORAL-RISK-IND-NI,
              :RUBEN-BNFY-KA-GIV-NM      :ZUBEN-BNFY-KA-GIV-NM-NI,
              :RUBEN-BNFY-KA-SUR-NM      :ZUBEN-BNFY-KA-SUR-NM-NI,
              :RUBEN-BNFY-CO-KA-NM       :ZUBEN-BNFY-CO-KA-NM-NI,
              :RUBEN-BNFY-SEX-CD         :ZUBEN-BNFY-SEX-CD-NI,
              :RUBEN-BNFY-BTH-DT         :ZUBEN-BNFY-BTH-DT-NI,
              :RUBEN-BNFY-REL-TYP-CD     :ZUBEN-BNFY-REL-TYP-CD-NI
           END-EXEC.


           EVALUATE SQLCODE

               WHEN ZERO
                    SET  WUBEN-IO-OK         TO  TRUE
                    PERFORM  UBEN-2000-SET-NULL-DFLT
                        THRU UBEN-2000-SET-NULL-DFLT-X
                    MOVE RUBEN-KEY           TO  WUBEN-KEY

               WHEN +100
                    SET  WUBEN-IO-EOF        TO  TRUE

               WHEN OTHER
                    SET  WUBEN-IO-ERROR      TO  TRUE

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

               WHEN '04'
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
                CLOSE B2CUR_UBEN
           END-EXEC.


           EVALUATE SQLCODE

               WHEN ZERO
                    SET  WUBEN-IO-OK         TO  TRUE

               WHEN OTHER
                    SET  WUBEN-IO-ERROR      TO  TRUE

           END-EVALUATE.


       4102-CLOSE-BROWSE-CUR-X.
           EXIT.


      ***********************
       410G-CLOSE-BROWSE-CUR.
      ***********************

           EXEC SQL
                CLOSE BCUR_UBEN
           END-EXEC.


           EVALUATE SQLCODE

               WHEN ZERO
                    SET  WUBEN-IO-OK         TO  TRUE

               WHEN OTHER
                    SET  WUBEN-IO-ERROR      TO  TRUE

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

           SET  WUBEN-IO-ERROR               TO  TRUE.
           MOVE WS-OPTM-SQL-REQIR            TO  WUBEN-OPTM-SQL-REQIR.
           MOVE WS-OPTM-SQL-EXEC             TO  WUBEN-OPTM-SQL-EXEC.

           EVALUATE TRUE

               WHEN WUBEN-OPTM-SQL-EXEC = WUBEN-OPTM-SQL-REQIR
                   SET WUBEN-OPTM-SQL-OK     TO  TRUE

               WHEN WUBEN-OPTM-SQL-EXEC = SPACES
                   SET WUBEN-OPTM-SQL-ERROR  TO  TRUE

               WHEN OTHER
                   SET WUBEN-OPTM-SQL-IMPRV  TO  TRUE

           END-EVALUATE.


       5000-EXEC-BROWSE-INDEX-X.
           EXIT.


      ****************************
       6000-EXEC-FETCH-NEXT-INDEX.
      ****************************

      * PROFILE INDICATED THAT -FETCH-NEXT-INDEX
      * PARAGRAPHS ARE NOT APPLICABLE

           SET  WUBEN-IO-ERROR               TO  TRUE.
           SET  WUBEN-OPTM-SQL-OK            TO  TRUE.
           MOVE SPACES                       TO  WUBEN-OPTM-SQL-REQIR.
           MOVE SPACES                       TO  WUBEN-OPTM-SQL-EXEC.


       6000-EXEC-FETCH-NEXT-INDEX-X.
           EXIT.


      ***********************
       7000-EXEC-CLOSE-INDEX.
      ***********************

      * PROFILE INDICATED THAT -CLOSE-BROWSE-INDEX
      * PARAGRAPHS ARE NOT APPLICABLE

           SET  WUBEN-IO-ERROR               TO  TRUE.
           SET  WS-OPTM-SQL-CUR-CLOSED       TO  TRUE.


       7000-EXEC-CLOSE-INDEX-X.
           EXIT.


       COPY ACPZUBEN.

      *****************************************************************
      **                 END OF PROGRAM ASIBUBEN                     **
      *****************************************************************
