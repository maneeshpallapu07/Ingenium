      *************************
       IDENTIFICATION DIVISION.
      *************************

       PROGRAM-ID. ASIBUAPE.

       COPY XCWWCRHT.

      *****************************************************************
      **  MEMBER :  ASIBUAPE                                         **
      **  REMARKS:  SQL I/O PROGRAM USED TO BROWSE ROWS IN THE       **
      **            INCOMPLETENESS INFORMATION UPLOAD TABLE          **
      *****************************************************************
      **  RELEASE   DESCRIPTION                                      **
      **                                                             **
      **  6.5       CREATED FOR UAPE PROCESSING                      **
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

       COPY XCWWPGWS REPLACING '$VAR1' BY 'ASIBUAPE'.

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
           EXEC SQL INCLUDE ACWZUAPE  END-EXEC.
      /
      *****************
       LINKAGE SECTION.
      *****************

           EXEC SQL INCLUDE SQLCA     END-EXEC.

           EXEC SQL INCLUDE ACFWUAPE  END-EXEC.

           EXEC SQL INCLUDE ACFRUAPE  END-EXEC.

      /
       PROCEDURE DIVISION USING SQLCA
                                WUAPE-IO-WORK-AREA
                                RUAPE-REC-INFO.

      ***************
       0000-MAINLINE.
      ***************

           IF  WS-WA-ADDRESS NOT = ADDRESS OF WUAPE-IO-WORK-AREA
           OR  WS-TL-ADDRESS NOT = ADDRESS OF RUAPE-REC-INFO
               SET  WS-WA-ADDRESS  TO  ADDRESS OF WUAPE-IO-WORK-AREA
               SET  WS-TL-ADDRESS  TO  ADDRESS OF RUAPE-REC-INFO
               MOVE ZERO           TO  SQL-INIT-FLAG
           END-IF.


           EVALUATE TRUE

               WHEN WUAPE-RQST-BROWSE
                    PERFORM  1000-DETERMINE-SQL-REQIR
                        THRU 1000-DETERMINE-SQL-REQIR-X
                    PERFORM  2000-EXEC-BROWSE
                        THRU 2000-EXEC-BROWSE-X

               WHEN WUAPE-RQST-FETCH-NEXT
                    PERFORM  3000-EXEC-FETCH-NEXT
                        THRU 3000-EXEC-FETCH-NEXT-X

               WHEN WUAPE-RQST-CLOSE-BROWSE-CUR
                    PERFORM  4000-EXEC-CLOSE-CUR
                        THRU 4000-EXEC-CLOSE-CUR-X

               WHEN WUAPE-RQST-BROWSE-INDEX
                    PERFORM  1000-DETERMINE-SQL-REQIR
                        THRU 1000-DETERMINE-SQL-REQIR-X
                    PERFORM  5000-EXEC-BROWSE-INDEX
                        THRU 5000-EXEC-BROWSE-INDEX-X

               WHEN WUAPE-RQST-FETCH-NEXT-INDEX
                    PERFORM  6000-EXEC-FETCH-NEXT-INDEX
                        THRU 6000-EXEC-FETCH-NEXT-INDEX-X

               WHEN WUAPE-RQST-CLOSE-BROWSE-INDEX
                    PERFORM  7000-EXEC-CLOSE-INDEX
                        THRU 7000-EXEC-CLOSE-INDEX-X

               WHEN OTHER
                    SET  WUAPE-IO-ERROR      TO  TRUE

           END-EVALUATE.


           GOBACK.


       0000-MAINLINE-X.
           EXIT.


      **************************
       1000-DETERMINE-SQL-REQIR.
      **************************

           MOVE 2                            TO  WS-OPTM-SQL-REQIR.

           IF  WUAPE-APP-ID = WUAPE-ENDBR-APP-ID
               SUBTRACT +1                   FROM WS-OPTM-SQL-REQIR
           ELSE
               GO TO 1000-DETERMINE-SQL-REQIR-X
           END-IF.


           IF  WUAPE-INCMPLT-SEQ-NUM = WUAPE-ENDBR-INCMPLT-SEQ-NUM
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
                    PERFORM  210G-BROWSE
                        THRU 210G-BROWSE-X

               WHEN OTHER
                    MOVE SPACES              TO  WS-OPTM-SQL-EXEC
                    SET  WUAPE-IO-ERROR      TO  TRUE

           END-EVALUATE.


           MOVE WS-OPTM-SQL-REQIR            TO  WUAPE-OPTM-SQL-REQIR.
           MOVE WS-OPTM-SQL-EXEC             TO  WUAPE-OPTM-SQL-EXEC.

           EVALUATE TRUE

               WHEN WUAPE-OPTM-SQL-EXEC = WUAPE-OPTM-SQL-REQIR
                   SET WUAPE-OPTM-SQL-OK     TO  TRUE

               WHEN WUAPE-OPTM-SQL-EXEC = SPACES
                   SET WUAPE-OPTM-SQL-ERROR  TO  TRUE

               WHEN OTHER
                   SET WUAPE-OPTM-SQL-IMPRV  TO  TRUE

           END-EVALUATE.


       2000-EXEC-BROWSE-X.
           EXIT.


      *************
       210G-BROWSE.
      *************

           EXEC SQL
             DECLARE BCUR_UAPE CURSOR FOR
             SELECT
                 APP_ID,
                 INCMPLT_SEQ_NUM,
                 INCMPLT_ID,
                 INCMPLT_DTL_TXT
             FROM TUAPE
             WHERE
                 APP_ID           BETWEEN
                                    :WUAPE-APP-ID                  AND
                                    :WUAPE-ENDBR-APP-ID
               AND
                (INCMPLT_SEQ_NUM >= :WUAPE-INCMPLT-SEQ-NUM         OR
                 APP_ID           > :WUAPE-APP-ID)
               AND
                (INCMPLT_SEQ_NUM <= :WUAPE-ENDBR-INCMPLT-SEQ-NUM   OR
                 APP_ID           < :WUAPE-ENDBR-APP-ID)
             ORDER BY
                 APP_ID,
                 INCMPLT_SEQ_NUM
             FOR FETCH ONLY
             OPTIMIZE FOR 1 ROW
           END-EXEC.

           EXEC SQL
                OPEN BCUR_UAPE
           END-EXEC.


           EVALUATE SQLCODE

               WHEN ZERO
                    SET  WUAPE-IO-OK         TO  TRUE

               WHEN OTHER
                    SET  WUAPE-IO-ERROR      TO  TRUE

           END-EVALUATE.


       210G-BROWSE-X.
           EXIT.


      **********************
       3000-EXEC-FETCH-NEXT.
      **********************

           EVALUATE WS-OPTM-SQL-EXEC

               WHEN '02'
                    PERFORM  310G-FETCH-NEXT
                        THRU 310G-FETCH-NEXT-X

               WHEN OTHER
                    PERFORM  310G-FETCH-NEXT
                        THRU 310G-FETCH-NEXT-X

           END-EVALUATE.


           SET  WUAPE-OPTM-SQL-OK            TO  TRUE.
           MOVE SPACES                       TO  WUAPE-OPTM-SQL-REQIR.
           MOVE SPACES                       TO  WUAPE-OPTM-SQL-EXEC.


       3000-EXEC-FETCH-NEXT-X.
           EXIT.


      *****************
       310G-FETCH-NEXT.
      *****************

           MOVE LOW-VALUES                   TO  ZUAPE-NULL-INDICATORS.

           MOVE SPACES                 TO  RUAPE-INCMPLT-DTL-TXT-TXT.

           EXEC SQL
             FETCH BCUR_UAPE
             INTO
                 :RUAPE-APP-ID,
                 :RUAPE-INCMPLT-SEQ-NUM,
                 :RUAPE-INCMPLT-ID      :ZUAPE-INCMPLT-ID-NI,
                 :RUAPE-INCMPLT-DTL-TXT :ZUAPE-INCMPLT-DTL-TXT-NI
           END-EXEC.


           EVALUATE SQLCODE

               WHEN ZERO
                    SET  WUAPE-IO-OK         TO  TRUE
                    PERFORM  UAPE-2000-SET-NULL-DFLT
                        THRU UAPE-2000-SET-NULL-DFLT-X
                    MOVE RUAPE-KEY           TO  WUAPE-KEY

               WHEN +100
                    SET  WUAPE-IO-EOF        TO  TRUE

               WHEN OTHER
                    SET  WUAPE-IO-ERROR      TO  TRUE

           END-EVALUATE.


       310G-FETCH-NEXT-X.
           EXIT.


      *********************
       4000-EXEC-CLOSE-CUR.
      *********************

           EVALUATE WS-OPTM-SQL-EXEC

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
       410G-CLOSE-BROWSE-CUR.
      ***********************

           EXEC SQL
                CLOSE BCUR_UAPE
           END-EXEC.


           EVALUATE SQLCODE

               WHEN ZERO
                    SET  WUAPE-IO-OK         TO  TRUE

               WHEN OTHER
                    SET  WUAPE-IO-ERROR      TO  TRUE

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

           SET  WUAPE-IO-ERROR               TO  TRUE.
           MOVE WS-OPTM-SQL-REQIR            TO  WUAPE-OPTM-SQL-REQIR.
           MOVE WS-OPTM-SQL-EXEC             TO  WUAPE-OPTM-SQL-EXEC.

           EVALUATE TRUE

               WHEN WUAPE-OPTM-SQL-EXEC = WUAPE-OPTM-SQL-REQIR
                   SET WUAPE-OPTM-SQL-OK     TO  TRUE

               WHEN WUAPE-OPTM-SQL-EXEC = SPACES
                   SET WUAPE-OPTM-SQL-ERROR  TO  TRUE

               WHEN OTHER
                   SET WUAPE-OPTM-SQL-IMPRV  TO  TRUE

           END-EVALUATE.


       5000-EXEC-BROWSE-INDEX-X.
           EXIT.


      ****************************
       6000-EXEC-FETCH-NEXT-INDEX.
      ****************************

      * PROFILE INDICATED THAT -FETCH-NEXT-INDEX
      * PARAGRAPHS ARE NOT APPLICABLE

           SET  WUAPE-IO-ERROR               TO  TRUE.
           SET  WUAPE-OPTM-SQL-OK            TO  TRUE.
           MOVE SPACES                       TO  WUAPE-OPTM-SQL-REQIR.
           MOVE SPACES                       TO  WUAPE-OPTM-SQL-EXEC.


       6000-EXEC-FETCH-NEXT-INDEX-X.
           EXIT.


      ***********************
       7000-EXEC-CLOSE-INDEX.
      ***********************

      * PROFILE INDICATED THAT -CLOSE-BROWSE-INDEX
      * PARAGRAPHS ARE NOT APPLICABLE

           SET  WUAPE-IO-ERROR               TO  TRUE.
           SET  WS-OPTM-SQL-CUR-CLOSED       TO  TRUE.


       7000-EXEC-CLOSE-INDEX-X.
           EXIT.


       COPY ACPZUAPE.

      *****************************************************************
      **                 END OF PROGRAM ASIBUAPE                     **
      *****************************************************************
