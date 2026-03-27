      *************************
       IDENTIFICATION DIVISION.
      *************************

       PROGRAM-ID. ASIBCWAE.

       COPY XCWWCRHT.

      *****************************************************************
      **  MEMBER :  ASIBCWAE                                         **
      **  REMARKS:  SQL I/O PROGRAM USED TO BROWSE ROWS IN THE       **
      **            CWA ERROR TABLE                                  **
      *****************************************************************
      **  RELEASE   DESCRIPTION                                      **
      **                                                             **
      **  25JUL09   CREATED FOR CWAE PROCESSING                      **
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

       COPY XCWWPGWS REPLACING '$VAR1' BY 'ASIBCWAE'.

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
           EXEC SQL INCLUDE ACWZCWAE  END-EXEC.
      /
      *****************
       LINKAGE SECTION.
      *****************

           EXEC SQL INCLUDE SQLCA     END-EXEC.

           EXEC SQL INCLUDE ACFWCWAE  END-EXEC.

           EXEC SQL INCLUDE ACFRCWAE  END-EXEC.

      /
       PROCEDURE DIVISION USING SQLCA
                                WCWAE-IO-WORK-AREA
                                RCWAE-REC-INFO.

      ***************
       0000-MAINLINE.
      ***************

           IF  WS-WA-ADDRESS NOT = ADDRESS OF WCWAE-IO-WORK-AREA
           OR  WS-TL-ADDRESS NOT = ADDRESS OF RCWAE-REC-INFO
               SET  WS-WA-ADDRESS  TO  ADDRESS OF WCWAE-IO-WORK-AREA
               SET  WS-TL-ADDRESS  TO  ADDRESS OF RCWAE-REC-INFO
               MOVE ZERO           TO  SQL-INIT-FLAG
           END-IF.


           EVALUATE TRUE

               WHEN WCWAE-RQST-BROWSE
                    PERFORM  1000-DETERMINE-SQL-REQIR
                        THRU 1000-DETERMINE-SQL-REQIR-X
                    PERFORM  2000-EXEC-BROWSE
                        THRU 2000-EXEC-BROWSE-X

               WHEN WCWAE-RQST-FETCH-NEXT
                    PERFORM  3000-EXEC-FETCH-NEXT
                        THRU 3000-EXEC-FETCH-NEXT-X

               WHEN WCWAE-RQST-CLOSE-BROWSE-CUR
                    PERFORM  4000-EXEC-CLOSE-CUR
                        THRU 4000-EXEC-CLOSE-CUR-X

               WHEN WCWAE-RQST-BROWSE-INDEX
                    PERFORM  1000-DETERMINE-SQL-REQIR
                        THRU 1000-DETERMINE-SQL-REQIR-X
                    PERFORM  5000-EXEC-BROWSE-INDEX
                        THRU 5000-EXEC-BROWSE-INDEX-X

               WHEN WCWAE-RQST-FETCH-NEXT-INDEX
                    PERFORM  6000-EXEC-FETCH-NEXT-INDEX
                        THRU 6000-EXEC-FETCH-NEXT-INDEX-X

               WHEN WCWAE-RQST-CLOSE-BROWSE-INDEX
                    PERFORM  7000-EXEC-CLOSE-INDEX
                        THRU 7000-EXEC-CLOSE-INDEX-X

               WHEN OTHER
                    SET  WCWAE-IO-ERROR      TO  TRUE

           END-EVALUATE.


           GOBACK.


       0000-MAINLINE-X.
           EXIT.


      **************************
       1000-DETERMINE-SQL-REQIR.
      **************************

           MOVE 3                            TO  WS-OPTM-SQL-REQIR.

           IF  WCWAE-CO-ID = WCWAE-ENDBR-CO-ID
               SUBTRACT +1                   FROM WS-OPTM-SQL-REQIR
           ELSE
               GO TO 1000-DETERMINE-SQL-REQIR-X
           END-IF.


           IF  WCWAE-APP-ID = WCWAE-ENDBR-APP-ID
               SUBTRACT +1                   FROM WS-OPTM-SQL-REQIR
           ELSE
               GO TO 1000-DETERMINE-SQL-REQIR-X
           END-IF.


           IF  WCWAE-SEQ-NUM = WCWAE-ENDBR-SEQ-NUM
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

               WHEN WS-OPTM-SQL-REQIR = ZERO
                    MOVE ZERO                TO  WS-OPTM-SQL-EXEC
                    PERFORM  2100-BROWSE
                        THRU 2100-BROWSE-X

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
                    SET  WCWAE-IO-ERROR      TO  TRUE

           END-EVALUATE.


           MOVE WS-OPTM-SQL-REQIR            TO  WCWAE-OPTM-SQL-REQIR.
           MOVE WS-OPTM-SQL-EXEC             TO  WCWAE-OPTM-SQL-EXEC.

           EVALUATE TRUE

               WHEN WCWAE-OPTM-SQL-EXEC = WCWAE-OPTM-SQL-REQIR
                   SET WCWAE-OPTM-SQL-OK     TO  TRUE

               WHEN WCWAE-OPTM-SQL-EXEC = SPACES
                   SET WCWAE-OPTM-SQL-ERROR  TO  TRUE

               WHEN OTHER
                   SET WCWAE-OPTM-SQL-IMPRV  TO  TRUE

           END-EVALUATE.


       2000-EXEC-BROWSE-X.
           EXIT.


      *************
       2100-BROWSE.
      *************

           EXEC SQL
             DECLARE B0CUR_CWAE CURSOR FOR
             SELECT
                 CO_ID,
                 APP_ID,
                 SEQ_NUM,
                 CWA_CHNL_CD,
                 CWA_UPLD_DT,
                 RECPT_NUM,
                 RECPT_AMT,
                 RECPT_DT,
                 PMT_TYP_CD,
                 REJ_REASN_CD
             FROM TCWAE
             WHERE
                 CO_ID    = :WCWAE-CO-ID         AND
                 APP_ID   = :WCWAE-APP-ID        AND
                 SEQ_NUM  = :WCWAE-SEQ-NUM
             ORDER BY
                 CO_ID,
                 APP_ID,
                 SEQ_NUM
             FOR FETCH ONLY
             OPTIMIZE FOR 1 ROW
           END-EXEC.

           EXEC SQL
                OPEN B0CUR_CWAE
           END-EXEC.


           EVALUATE SQLCODE

               WHEN ZERO
                    SET  WCWAE-IO-OK         TO  TRUE

               WHEN OTHER
                    SET  WCWAE-IO-ERROR      TO  TRUE

           END-EVALUATE.


       2100-BROWSE-X.
           EXIT.


      *************
       2101-BROWSE.
      *************

           EXEC SQL
             DECLARE B1CUR_CWAE CURSOR FOR
             SELECT
                 CO_ID,
                 APP_ID,
                 SEQ_NUM,
                 CWA_CHNL_CD,
                 CWA_UPLD_DT,
                 RECPT_NUM,
                 RECPT_AMT,
                 RECPT_DT,
                 PMT_TYP_CD,
                 REJ_REASN_CD
             FROM TCWAE
             WHERE
                 CO_ID    = :WCWAE-CO-ID         AND
                 APP_ID   = :WCWAE-APP-ID
               AND
                 SEQ_NUM  BETWEEN
                            :WCWAE-SEQ-NUM       AND
                            :WCWAE-ENDBR-SEQ-NUM
             ORDER BY
                 CO_ID,
                 APP_ID,
                 SEQ_NUM
             FOR FETCH ONLY
             OPTIMIZE FOR 1 ROW
           END-EXEC.

           EXEC SQL
                OPEN B1CUR_CWAE
           END-EXEC.


           EVALUATE SQLCODE

               WHEN ZERO
                    SET  WCWAE-IO-OK         TO  TRUE

               WHEN OTHER
                    SET  WCWAE-IO-ERROR      TO  TRUE

           END-EVALUATE.


       2101-BROWSE-X.
           EXIT.


      *************
       210G-BROWSE.
      *************

           EXEC SQL
             DECLARE BCUR_CWAE CURSOR FOR
             SELECT
                 CO_ID,
                 APP_ID,
                 SEQ_NUM,
                 CWA_CHNL_CD,
                 CWA_UPLD_DT,
                 RECPT_NUM,
                 RECPT_AMT,
                 RECPT_DT,
                 PMT_TYP_CD,
                 REJ_REASN_CD
             FROM TCWAE
             WHERE
                 CO_ID    = :WCWAE-CO-ID
               AND
                 APP_ID   BETWEEN
                            :WCWAE-APP-ID          AND
                            :WCWAE-ENDBR-APP-ID
               AND
                (SEQ_NUM >= :WCWAE-SEQ-NUM         OR
                 APP_ID   > :WCWAE-APP-ID)
               AND
                (SEQ_NUM <= :WCWAE-ENDBR-SEQ-NUM   OR
                 APP_ID   < :WCWAE-ENDBR-APP-ID)
             ORDER BY
                 CO_ID,
                 APP_ID,
                 SEQ_NUM
             FOR FETCH ONLY
             OPTIMIZE FOR 1 ROW
           END-EXEC.

           EXEC SQL
                OPEN BCUR_CWAE
           END-EXEC.


           EVALUATE SQLCODE

               WHEN ZERO
                    SET  WCWAE-IO-OK         TO  TRUE

               WHEN OTHER
                    SET  WCWAE-IO-ERROR      TO  TRUE

           END-EVALUATE.


       210G-BROWSE-X.
           EXIT.


      **********************
       3000-EXEC-FETCH-NEXT.
      **********************

           EVALUATE WS-OPTM-SQL-EXEC

               WHEN ZERO
                    PERFORM  3100-FETCH-NEXT
                        THRU 3100-FETCH-NEXT-X

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


           SET  WCWAE-OPTM-SQL-OK            TO  TRUE.
           MOVE SPACES                       TO  WCWAE-OPTM-SQL-REQIR.
           MOVE SPACES                       TO  WCWAE-OPTM-SQL-EXEC.


       3000-EXEC-FETCH-NEXT-X.
           EXIT.


      *****************
       3100-FETCH-NEXT.
      *****************

           MOVE LOW-VALUES                   TO  ZCWAE-NULL-INDICATORS.

           EXEC SQL
             FETCH B0CUR_CWAE
             INTO
                 :RCWAE-CO-ID,
                 :RCWAE-APP-ID,
                 :RCWAE-SEQ-NUM,
                 :RCWAE-CWA-CHNL-CD,
                 :RCWAE-CWA-UPLD-DT,
                 :RCWAE-RECPT-NUM,
                 :RCWAE-RECPT-AMT    :ZCWAE-RECPT-AMT-NI,
                 :RCWAE-RECPT-DT     :ZCWAE-RECPT-DT-NI,
                 :RCWAE-PMT-TYP-CD,
                 :RCWAE-REJ-REASN-CD
           END-EXEC.


           EVALUATE SQLCODE

               WHEN ZERO
                    SET  WCWAE-IO-OK         TO  TRUE
                    PERFORM  CWAE-2000-SET-NULL-DFLT
                        THRU CWAE-2000-SET-NULL-DFLT-X
                    MOVE RCWAE-KEY           TO  WCWAE-KEY

               WHEN +100
                    SET  WCWAE-IO-EOF        TO  TRUE

               WHEN OTHER
                    SET  WCWAE-IO-ERROR      TO  TRUE

           END-EVALUATE.


       3100-FETCH-NEXT-X.
           EXIT.


      *****************
       3101-FETCH-NEXT.
      *****************

           MOVE LOW-VALUES                   TO  ZCWAE-NULL-INDICATORS.

           EXEC SQL
             FETCH B1CUR_CWAE
             INTO
                 :RCWAE-CO-ID,
                 :RCWAE-APP-ID,
                 :RCWAE-SEQ-NUM,
                 :RCWAE-CWA-CHNL-CD,
                 :RCWAE-CWA-UPLD-DT,
                 :RCWAE-RECPT-NUM,
                 :RCWAE-RECPT-AMT    :ZCWAE-RECPT-AMT-NI,
                 :RCWAE-RECPT-DT     :ZCWAE-RECPT-DT-NI,
                 :RCWAE-PMT-TYP-CD,
                 :RCWAE-REJ-REASN-CD
           END-EXEC.


           EVALUATE SQLCODE

               WHEN ZERO
                    SET  WCWAE-IO-OK         TO  TRUE
                    PERFORM  CWAE-2000-SET-NULL-DFLT
                        THRU CWAE-2000-SET-NULL-DFLT-X
                    MOVE RCWAE-KEY           TO  WCWAE-KEY

               WHEN +100
                    SET  WCWAE-IO-EOF        TO  TRUE

               WHEN OTHER
                    SET  WCWAE-IO-ERROR      TO  TRUE

           END-EVALUATE.


       3101-FETCH-NEXT-X.
           EXIT.


      *****************
       310G-FETCH-NEXT.
      *****************

           MOVE LOW-VALUES                   TO  ZCWAE-NULL-INDICATORS.

           EXEC SQL
             FETCH BCUR_CWAE
             INTO
                 :RCWAE-CO-ID,
                 :RCWAE-APP-ID,
                 :RCWAE-SEQ-NUM,
                 :RCWAE-CWA-CHNL-CD,
                 :RCWAE-CWA-UPLD-DT,
                 :RCWAE-RECPT-NUM,
                 :RCWAE-RECPT-AMT    :ZCWAE-RECPT-AMT-NI,
                 :RCWAE-RECPT-DT     :ZCWAE-RECPT-DT-NI,
                 :RCWAE-PMT-TYP-CD,
                 :RCWAE-REJ-REASN-CD
           END-EXEC.


           EVALUATE SQLCODE

               WHEN ZERO
                    SET  WCWAE-IO-OK         TO  TRUE
                    PERFORM  CWAE-2000-SET-NULL-DFLT
                        THRU CWAE-2000-SET-NULL-DFLT-X
                    MOVE RCWAE-KEY           TO  WCWAE-KEY

               WHEN +100
                    SET  WCWAE-IO-EOF        TO  TRUE

               WHEN OTHER
                    SET  WCWAE-IO-ERROR      TO  TRUE

           END-EVALUATE.


       310G-FETCH-NEXT-X.
           EXIT.


      *********************
       4000-EXEC-CLOSE-CUR.
      *********************

           EVALUATE WS-OPTM-SQL-EXEC

               WHEN ZERO
                    PERFORM  4100-CLOSE-BROWSE-CUR
                        THRU 4100-CLOSE-BROWSE-CUR-X

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
       4100-CLOSE-BROWSE-CUR.
      ***********************

           EXEC SQL
                CLOSE B0CUR_CWAE
           END-EXEC.


           EVALUATE SQLCODE

               WHEN ZERO
                    SET  WCWAE-IO-OK         TO  TRUE

               WHEN OTHER
                    SET  WCWAE-IO-ERROR      TO  TRUE

           END-EVALUATE.


       4100-CLOSE-BROWSE-CUR-X.
           EXIT.


      ***********************
       4101-CLOSE-BROWSE-CUR.
      ***********************

           EXEC SQL
                CLOSE B1CUR_CWAE
           END-EXEC.


           EVALUATE SQLCODE

               WHEN ZERO
                    SET  WCWAE-IO-OK         TO  TRUE

               WHEN OTHER
                    SET  WCWAE-IO-ERROR      TO  TRUE

           END-EVALUATE.


       4101-CLOSE-BROWSE-CUR-X.
           EXIT.


      ***********************
       410G-CLOSE-BROWSE-CUR.
      ***********************

           EXEC SQL
                CLOSE BCUR_CWAE
           END-EXEC.


           EVALUATE SQLCODE

               WHEN ZERO
                    SET  WCWAE-IO-OK         TO  TRUE

               WHEN OTHER
                    SET  WCWAE-IO-ERROR      TO  TRUE

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

           SET  WCWAE-IO-ERROR               TO  TRUE.
           MOVE WS-OPTM-SQL-REQIR            TO  WCWAE-OPTM-SQL-REQIR.
           MOVE WS-OPTM-SQL-EXEC             TO  WCWAE-OPTM-SQL-EXEC.

           EVALUATE TRUE

               WHEN WCWAE-OPTM-SQL-EXEC = WCWAE-OPTM-SQL-REQIR
                   SET WCWAE-OPTM-SQL-OK     TO  TRUE

               WHEN WCWAE-OPTM-SQL-EXEC = SPACES
                   SET WCWAE-OPTM-SQL-ERROR  TO  TRUE

               WHEN OTHER
                   SET WCWAE-OPTM-SQL-IMPRV  TO  TRUE

           END-EVALUATE.


       5000-EXEC-BROWSE-INDEX-X.
           EXIT.


      ****************************
       6000-EXEC-FETCH-NEXT-INDEX.
      ****************************

      * PROFILE INDICATED THAT -FETCH-NEXT-INDEX
      * PARAGRAPHS ARE NOT APPLICABLE

           SET  WCWAE-IO-ERROR               TO  TRUE.
           SET  WCWAE-OPTM-SQL-OK            TO  TRUE.
           MOVE SPACES                       TO  WCWAE-OPTM-SQL-REQIR.
           MOVE SPACES                       TO  WCWAE-OPTM-SQL-EXEC.


       6000-EXEC-FETCH-NEXT-INDEX-X.
           EXIT.


      ***********************
       7000-EXEC-CLOSE-INDEX.
      ***********************

      * PROFILE INDICATED THAT -CLOSE-BROWSE-INDEX
      * PARAGRAPHS ARE NOT APPLICABLE

           SET  WCWAE-IO-ERROR               TO  TRUE.
           SET  WS-OPTM-SQL-CUR-CLOSED       TO  TRUE.


       7000-EXEC-CLOSE-INDEX-X.
           EXIT.


       COPY ACPZCWAE.

      *****************************************************************
      **                 END OF PROGRAM ASIBCWAE                     **
      *****************************************************************
