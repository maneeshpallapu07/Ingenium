      *************************
       IDENTIFICATION DIVISION.
      *************************

       PROGRAM-ID. ASIBXMLE.

       COPY XCWWCRHT.

      *****************************************************************
      **  MEMBER :  ASIBXMLE                                         **
      **  REMARKS:  SQL I/O PROGRAM USED TO BROWSE ROWS IN THE       **
      **            XML MESSAGE EXTRACT TABLE                        **
      *****************************************************************
      **  RELEASE   DESCRIPTION                                      **
      **                                                             **
      **  23JUL09   CREATED FOR XMLE PROCESSING                      **
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

       COPY XCWWPGWS REPLACING '$VAR1' BY 'ASIBXMLE'.

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
      *****************
       LINKAGE SECTION.
      *****************

           EXEC SQL INCLUDE SQLCA     END-EXEC.

           EXEC SQL INCLUDE ACFWXMLE  END-EXEC.

           EXEC SQL INCLUDE ACFRXMLE  END-EXEC.

      /
       PROCEDURE DIVISION USING SQLCA
                                WXMLE-IO-WORK-AREA
                                RXMLE-REC-INFO.

      ***************
       0000-MAINLINE.
      ***************

           IF  WS-WA-ADDRESS NOT = ADDRESS OF WXMLE-IO-WORK-AREA
           OR  WS-TL-ADDRESS NOT = ADDRESS OF RXMLE-REC-INFO
               SET  WS-WA-ADDRESS  TO  ADDRESS OF WXMLE-IO-WORK-AREA
               SET  WS-TL-ADDRESS  TO  ADDRESS OF RXMLE-REC-INFO
               MOVE ZERO           TO  SQL-INIT-FLAG
           END-IF.


           EVALUATE TRUE

               WHEN WXMLE-RQST-BROWSE
                    PERFORM  1000-DETERMINE-SQL-REQIR
                        THRU 1000-DETERMINE-SQL-REQIR-X
                    PERFORM  2000-EXEC-BROWSE
                        THRU 2000-EXEC-BROWSE-X

               WHEN WXMLE-RQST-FETCH-NEXT
                    PERFORM  3000-EXEC-FETCH-NEXT
                        THRU 3000-EXEC-FETCH-NEXT-X

               WHEN WXMLE-RQST-CLOSE-BROWSE-CUR
                    PERFORM  4000-EXEC-CLOSE-CUR
                        THRU 4000-EXEC-CLOSE-CUR-X

               WHEN WXMLE-RQST-BROWSE-INDEX
                    PERFORM  1000-DETERMINE-SQL-REQIR
                        THRU 1000-DETERMINE-SQL-REQIR-X
                    PERFORM  5000-EXEC-BROWSE-INDEX
                        THRU 5000-EXEC-BROWSE-INDEX-X

               WHEN WXMLE-RQST-FETCH-NEXT-INDEX
                    PERFORM  6000-EXEC-FETCH-NEXT-INDEX
                        THRU 6000-EXEC-FETCH-NEXT-INDEX-X

               WHEN WXMLE-RQST-CLOSE-BROWSE-INDEX
                    PERFORM  7000-EXEC-CLOSE-INDEX
                        THRU 7000-EXEC-CLOSE-INDEX-X

               WHEN OTHER
                    SET  WXMLE-IO-ERROR      TO  TRUE

           END-EVALUATE.


           GOBACK.


       0000-MAINLINE-X.
           EXIT.


      **************************
       1000-DETERMINE-SQL-REQIR.
      **************************

           MOVE 4                            TO  WS-OPTM-SQL-REQIR.

           IF  WXMLE-CO-ID = WXMLE-ENDBR-CO-ID
               SUBTRACT +1                   FROM WS-OPTM-SQL-REQIR
           ELSE
               GO TO 1000-DETERMINE-SQL-REQIR-X
           END-IF.


           IF  WXMLE-APP-ID = WXMLE-ENDBR-APP-ID
               SUBTRACT +1                   FROM WS-OPTM-SQL-REQIR
           ELSE
               GO TO 1000-DETERMINE-SQL-REQIR-X
           END-IF.


           IF  WXMLE-APP-CHNL-CD = WXMLE-ENDBR-APP-CHNL-CD
               SUBTRACT +1                   FROM WS-OPTM-SQL-REQIR
           ELSE
               GO TO 1000-DETERMINE-SQL-REQIR-X
           END-IF.


           IF  WXMLE-APP-UPLD-DT = WXMLE-ENDBR-APP-UPLD-DT
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

               WHEN WS-OPTM-SQL-REQIR <= 3
                    MOVE '03'                TO  WS-OPTM-SQL-EXEC
                    PERFORM  210G-BROWSE
                        THRU 210G-BROWSE-X

               WHEN OTHER
                    MOVE SPACES              TO  WS-OPTM-SQL-EXEC
                    SET  WXMLE-IO-ERROR      TO  TRUE

           END-EVALUATE.


           MOVE WS-OPTM-SQL-REQIR            TO  WXMLE-OPTM-SQL-REQIR.
           MOVE WS-OPTM-SQL-EXEC             TO  WXMLE-OPTM-SQL-EXEC.

           EVALUATE TRUE

               WHEN WXMLE-OPTM-SQL-EXEC = WXMLE-OPTM-SQL-REQIR
                   SET WXMLE-OPTM-SQL-OK     TO  TRUE

               WHEN WXMLE-OPTM-SQL-EXEC = SPACES
                   SET WXMLE-OPTM-SQL-ERROR  TO  TRUE

               WHEN OTHER
                   SET WXMLE-OPTM-SQL-IMPRV  TO  TRUE

           END-EVALUATE.


       2000-EXEC-BROWSE-X.
           EXIT.


      *************
       2100-BROWSE.
      *************

           EXEC SQL
             DECLARE B0CUR_XMLE CURSOR FOR
             SELECT
                 CO_ID,
                 APP_ID,
                 APP_CHNL_CD,
                 APP_UPLD_DT,
                 APP_REJ_REASN_CD
             FROM TXMLE
             WHERE
                 CO_ID        = :WXMLE-CO-ID             AND
                 APP_ID       = :WXMLE-APP-ID            AND
                 APP_CHNL_CD  = :WXMLE-APP-CHNL-CD       AND
                 APP_UPLD_DT  = :WXMLE-APP-UPLD-DT
             ORDER BY
                 CO_ID,
                 APP_ID,
                 APP_CHNL_CD,
                 APP_UPLD_DT
             FOR FETCH ONLY
             OPTIMIZE FOR 1 ROW
           END-EXEC.

           EXEC SQL
                OPEN B0CUR_XMLE
           END-EXEC.


           EVALUATE SQLCODE

               WHEN ZERO
                    SET  WXMLE-IO-OK         TO  TRUE

               WHEN OTHER
                    SET  WXMLE-IO-ERROR      TO  TRUE

           END-EVALUATE.


       2100-BROWSE-X.
           EXIT.


      *************
       2101-BROWSE.
      *************

           EXEC SQL
             DECLARE B1CUR_XMLE CURSOR FOR
             SELECT
                 CO_ID,
                 APP_ID,
                 APP_CHNL_CD,
                 APP_UPLD_DT,
                 APP_REJ_REASN_CD
             FROM TXMLE
             WHERE
                 CO_ID        = :WXMLE-CO-ID             AND
                 APP_ID       = :WXMLE-APP-ID            AND
                 APP_CHNL_CD  = :WXMLE-APP-CHNL-CD
               AND
                 APP_UPLD_DT  BETWEEN
                                :WXMLE-APP-UPLD-DT       AND
                                :WXMLE-ENDBR-APP-UPLD-DT
             ORDER BY
                 CO_ID,
                 APP_ID,
                 APP_CHNL_CD,
                 APP_UPLD_DT
             FOR FETCH ONLY
             OPTIMIZE FOR 1 ROW
           END-EXEC.

           EXEC SQL
                OPEN B1CUR_XMLE
           END-EXEC.


           EVALUATE SQLCODE

               WHEN ZERO
                    SET  WXMLE-IO-OK         TO  TRUE

               WHEN OTHER
                    SET  WXMLE-IO-ERROR      TO  TRUE

           END-EVALUATE.


       2101-BROWSE-X.
           EXIT.


      *************
       210G-BROWSE.
      *************

           EXEC SQL
             DECLARE BCUR_XMLE CURSOR FOR
             SELECT
                 CO_ID,
                 APP_ID,
                 APP_CHNL_CD,
                 APP_UPLD_DT,
                 APP_REJ_REASN_CD
             FROM TXMLE
             WHERE
                 CO_ID        = :WXMLE-CO-ID
               AND
                 APP_ID       BETWEEN
                                :WXMLE-APP-ID              AND
                                :WXMLE-ENDBR-APP-ID
               AND
               ((APP_CHNL_CD  = :WXMLE-APP-CHNL-CD         AND
                 APP_UPLD_DT >= :WXMLE-APP-UPLD-DT)        OR
                 APP_CHNL_CD  > :WXMLE-APP-CHNL-CD         OR
                 APP_ID       > :WXMLE-APP-ID)
               AND
               ((APP_CHNL_CD  = :WXMLE-ENDBR-APP-CHNL-CD   AND
                 APP_UPLD_DT <= :WXMLE-ENDBR-APP-UPLD-DT)  OR
                 APP_CHNL_CD  < :WXMLE-ENDBR-APP-CHNL-CD   OR
                 APP_ID       < :WXMLE-ENDBR-APP-ID)
             ORDER BY
                 CO_ID,
                 APP_ID,
                 APP_CHNL_CD,
                 APP_UPLD_DT
             FOR FETCH ONLY
             OPTIMIZE FOR 1 ROW
           END-EXEC.

           EXEC SQL
                OPEN BCUR_XMLE
           END-EXEC.


           EVALUATE SQLCODE

               WHEN ZERO
                    SET  WXMLE-IO-OK         TO  TRUE

               WHEN OTHER
                    SET  WXMLE-IO-ERROR      TO  TRUE

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

               WHEN '03'
                    PERFORM  310G-FETCH-NEXT
                        THRU 310G-FETCH-NEXT-X

               WHEN OTHER
                    PERFORM  310G-FETCH-NEXT
                        THRU 310G-FETCH-NEXT-X

           END-EVALUATE.


           SET  WXMLE-OPTM-SQL-OK            TO  TRUE.
           MOVE SPACES                       TO  WXMLE-OPTM-SQL-REQIR.
           MOVE SPACES                       TO  WXMLE-OPTM-SQL-EXEC.


       3000-EXEC-FETCH-NEXT-X.
           EXIT.


      *****************
       3100-FETCH-NEXT.
      *****************

           EXEC SQL
             FETCH B0CUR_XMLE
             INTO
                 :RXMLE-CO-ID,
                 :RXMLE-APP-ID,
                 :RXMLE-APP-CHNL-CD,
                 :RXMLE-APP-UPLD-DT,
                 :RXMLE-APP-REJ-REASN-CD
           END-EXEC.


           EVALUATE SQLCODE

               WHEN ZERO
                    SET  WXMLE-IO-OK         TO  TRUE
                    MOVE RXMLE-KEY           TO  WXMLE-KEY

               WHEN +100
                    SET  WXMLE-IO-EOF        TO  TRUE

               WHEN OTHER
                    SET  WXMLE-IO-ERROR      TO  TRUE

           END-EVALUATE.


       3100-FETCH-NEXT-X.
           EXIT.


      *****************
       3101-FETCH-NEXT.
      *****************

           EXEC SQL
             FETCH B1CUR_XMLE
             INTO
                 :RXMLE-CO-ID,
                 :RXMLE-APP-ID,
                 :RXMLE-APP-CHNL-CD,
                 :RXMLE-APP-UPLD-DT,
                 :RXMLE-APP-REJ-REASN-CD
           END-EXEC.


           EVALUATE SQLCODE

               WHEN ZERO
                    SET  WXMLE-IO-OK         TO  TRUE
                    MOVE RXMLE-KEY           TO  WXMLE-KEY

               WHEN +100
                    SET  WXMLE-IO-EOF        TO  TRUE

               WHEN OTHER
                    SET  WXMLE-IO-ERROR      TO  TRUE

           END-EVALUATE.


       3101-FETCH-NEXT-X.
           EXIT.


      *****************
       310G-FETCH-NEXT.
      *****************

           EXEC SQL
             FETCH BCUR_XMLE
             INTO
                 :RXMLE-CO-ID,
                 :RXMLE-APP-ID,
                 :RXMLE-APP-CHNL-CD,
                 :RXMLE-APP-UPLD-DT,
                 :RXMLE-APP-REJ-REASN-CD
           END-EXEC.


           EVALUATE SQLCODE

               WHEN ZERO
                    SET  WXMLE-IO-OK         TO  TRUE
                    MOVE RXMLE-KEY           TO  WXMLE-KEY

               WHEN +100
                    SET  WXMLE-IO-EOF        TO  TRUE

               WHEN OTHER
                    SET  WXMLE-IO-ERROR      TO  TRUE

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
       4100-CLOSE-BROWSE-CUR.
      ***********************

           EXEC SQL
                CLOSE B0CUR_XMLE
           END-EXEC.


           EVALUATE SQLCODE

               WHEN ZERO
                    SET  WXMLE-IO-OK         TO  TRUE

               WHEN OTHER
                    SET  WXMLE-IO-ERROR      TO  TRUE

           END-EVALUATE.


       4100-CLOSE-BROWSE-CUR-X.
           EXIT.


      ***********************
       4101-CLOSE-BROWSE-CUR.
      ***********************

           EXEC SQL
                CLOSE B1CUR_XMLE
           END-EXEC.


           EVALUATE SQLCODE

               WHEN ZERO
                    SET  WXMLE-IO-OK         TO  TRUE

               WHEN OTHER
                    SET  WXMLE-IO-ERROR      TO  TRUE

           END-EVALUATE.


       4101-CLOSE-BROWSE-CUR-X.
           EXIT.


      ***********************
       410G-CLOSE-BROWSE-CUR.
      ***********************

           EXEC SQL
                CLOSE BCUR_XMLE
           END-EXEC.


           EVALUATE SQLCODE

               WHEN ZERO
                    SET  WXMLE-IO-OK         TO  TRUE

               WHEN OTHER
                    SET  WXMLE-IO-ERROR      TO  TRUE

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

           SET  WXMLE-IO-ERROR               TO  TRUE.
           MOVE WS-OPTM-SQL-REQIR            TO  WXMLE-OPTM-SQL-REQIR.
           MOVE WS-OPTM-SQL-EXEC             TO  WXMLE-OPTM-SQL-EXEC.

           EVALUATE TRUE

               WHEN WXMLE-OPTM-SQL-EXEC = WXMLE-OPTM-SQL-REQIR
                   SET WXMLE-OPTM-SQL-OK     TO  TRUE

               WHEN WXMLE-OPTM-SQL-EXEC = SPACES
                   SET WXMLE-OPTM-SQL-ERROR  TO  TRUE

               WHEN OTHER
                   SET WXMLE-OPTM-SQL-IMPRV  TO  TRUE

           END-EVALUATE.


       5000-EXEC-BROWSE-INDEX-X.
           EXIT.


      ****************************
       6000-EXEC-FETCH-NEXT-INDEX.
      ****************************

      * PROFILE INDICATED THAT -FETCH-NEXT-INDEX
      * PARAGRAPHS ARE NOT APPLICABLE

           SET  WXMLE-IO-ERROR               TO  TRUE.
           SET  WXMLE-OPTM-SQL-OK            TO  TRUE.
           MOVE SPACES                       TO  WXMLE-OPTM-SQL-REQIR.
           MOVE SPACES                       TO  WXMLE-OPTM-SQL-EXEC.


       6000-EXEC-FETCH-NEXT-INDEX-X.
           EXIT.


      ***********************
       7000-EXEC-CLOSE-INDEX.
      ***********************

      * PROFILE INDICATED THAT -CLOSE-BROWSE-INDEX
      * PARAGRAPHS ARE NOT APPLICABLE

           SET  WXMLE-IO-ERROR               TO  TRUE.
           SET  WS-OPTM-SQL-CUR-CLOSED       TO  TRUE.


       7000-EXEC-CLOSE-INDEX-X.
           EXIT.


      *****************************************************************
      **                 END OF PROGRAM ASIBXMLE                     **
      *****************************************************************
