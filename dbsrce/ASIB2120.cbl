      *************************
       IDENTIFICATION DIVISION.
      *************************

       PROGRAM-ID. ASIB2120.

       COPY XCWWCRHT.

      *****************************************************************
      **  MEMBER :  ASIB2120                                         **
      **  REMARKS:  SQL I/O PROGRAM USED TO BROWSE ROWS IN THE       **
      **            APEX TO NBS IMPORT MESSAGE EXTRACT TABLE         **
      *****************************************************************
      **  RELEASE   DESCRIPTION                                      **
      **                                                             **
      **  612J      CREATED FOR 2120 TABLE PROCESSING                **
      *****************************************************************
      *****************************************************************
      **  MEMBER :  ASIB2120                                         **
      **  REMARKS:  SQL I/O PROGRAM USED TO BROWSE ROWS IN THE       **
      **            APEX TO NBS IMPORT MESSAGE EXTRACT TABLE         **
      *****************************************************************
      **  RELEASE   DESCRIPTION                                      **
      **                                                             **
      **  612J      CREATED FOR 2120 TABLE PROCESSING                **
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

       COPY XCWWPGWS REPLACING '$VAR1' BY 'ASIB2120'.

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

           EXEC SQL INCLUDE ACFW2120  END-EXEC.

           EXEC SQL INCLUDE ACFR2120  END-EXEC.

      /
       PROCEDURE DIVISION USING SQLCA
                                W2120-IO-WORK-AREA
                                R2120-REC-INFO.

      ***************
       0000-MAINLINE.
      ***************

           IF  WS-WA-ADDRESS NOT = ADDRESS OF W2120-IO-WORK-AREA
           OR  WS-TL-ADDRESS NOT = ADDRESS OF R2120-REC-INFO
               SET  WS-WA-ADDRESS  TO  ADDRESS OF W2120-IO-WORK-AREA
               SET  WS-TL-ADDRESS  TO  ADDRESS OF R2120-REC-INFO
               MOVE ZERO           TO  SQL-INIT-FLAG
           END-IF.


           EVALUATE TRUE

               WHEN W2120-RQST-BROWSE
                    PERFORM  1000-DETERMINE-SQL-REQIR
                        THRU 1000-DETERMINE-SQL-REQIR-X
                    PERFORM  2000-EXEC-BROWSE
                        THRU 2000-EXEC-BROWSE-X

               WHEN W2120-RQST-FETCH-NEXT
                    PERFORM  3000-EXEC-FETCH-NEXT
                        THRU 3000-EXEC-FETCH-NEXT-X

               WHEN W2120-RQST-CLOSE-BROWSE-CUR
                    PERFORM  4000-EXEC-CLOSE-CUR
                        THRU 4000-EXEC-CLOSE-CUR-X

               WHEN W2120-RQST-BROWSE-INDEX
                    PERFORM  1000-DETERMINE-SQL-REQIR
                        THRU 1000-DETERMINE-SQL-REQIR-X
                    PERFORM  5000-EXEC-BROWSE-INDEX
                        THRU 5000-EXEC-BROWSE-INDEX-X

               WHEN W2120-RQST-FETCH-NEXT-INDEX
                    PERFORM  6000-EXEC-FETCH-NEXT-INDEX
                        THRU 6000-EXEC-FETCH-NEXT-INDEX-X

               WHEN W2120-RQST-CLOSE-BROWSE-INDEX
                    PERFORM  7000-EXEC-CLOSE-INDEX
                        THRU 7000-EXEC-CLOSE-INDEX-X

               WHEN OTHER
                    SET  W2120-IO-ERROR      TO  TRUE

           END-EVALUATE.


           GOBACK.


       0000-MAINLINE-X.
           EXIT.


      **************************
       1000-DETERMINE-SQL-REQIR.
      **************************

           MOVE 5                            TO  WS-OPTM-SQL-REQIR.

           IF  W2120-CO-ID = W2120-ENDBR-CO-ID
               SUBTRACT +1                   FROM WS-OPTM-SQL-REQIR
           ELSE
               GO TO 1000-DETERMINE-SQL-REQIR-X
           END-IF.


           IF  W2120-SEQ-FILE-PGM-ID = W2120-ENDBR-SEQ-FILE-PGM-ID
               SUBTRACT +1                   FROM WS-OPTM-SQL-REQIR
           ELSE
               GO TO 1000-DETERMINE-SQL-REQIR-X
           END-IF.


           IF  W2120-SEQ-FILE-OUTPT-NM = W2120-ENDBR-SEQ-FILE-OUTPT-NM
               SUBTRACT +1                   FROM WS-OPTM-SQL-REQIR
           ELSE
               GO TO 1000-DETERMINE-SQL-REQIR-X
           END-IF.


           IF  W2120-SEQ-FILE-INSTC-ID = W2120-ENDBR-SEQ-FILE-INSTC-ID
               SUBTRACT +1                   FROM WS-OPTM-SQL-REQIR
           ELSE
               GO TO 1000-DETERMINE-SQL-REQIR-X
           END-IF.


           IF  W2120-SEQ-FILE-TS = W2120-ENDBR-SEQ-FILE-TS
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

               WHEN OTHER
                    MOVE SPACES              TO  WS-OPTM-SQL-EXEC
                    SET  W2120-IO-ERROR      TO  TRUE

           END-EVALUATE.


           MOVE WS-OPTM-SQL-REQIR            TO  W2120-OPTM-SQL-REQIR.
           MOVE WS-OPTM-SQL-EXEC             TO  W2120-OPTM-SQL-EXEC.

           EVALUATE TRUE

               WHEN W2120-OPTM-SQL-EXEC = W2120-OPTM-SQL-REQIR
                   SET W2120-OPTM-SQL-OK     TO  TRUE

               WHEN W2120-OPTM-SQL-EXEC = SPACES
                   SET W2120-OPTM-SQL-ERROR  TO  TRUE

               WHEN OTHER
                   SET W2120-OPTM-SQL-IMPRV  TO  TRUE

           END-EVALUATE.


       2000-EXEC-BROWSE-X.
           EXIT.


      *************
       2102-BROWSE.
      *************

           EXEC SQL
             DECLARE B2CUR_2120 CURSOR FOR
             SELECT
                 CO_ID,
                 SEQ_FILE_PGM_ID,
                 SEQ_FILE_OUTPT_NM,
                 SEQ_FILE_INSTC_ID,
                 SEQ_FILE_TS,
                 SEQ_FILE_REC_INFO
             FROM S2120
             WHERE
               CO_ID              = :W2120-CO-ID                     AND
               SEQ_FILE_PGM_ID    = :W2120-SEQ-FILE-PGM-ID           AND
               SEQ_FILE_OUTPT_NM  = :W2120-SEQ-FILE-OUTPT-NM
             AND
               SEQ_FILE_INSTC_ID  BETWEEN
                                    :W2120-SEQ-FILE-INSTC-ID         AND
                                    :W2120-ENDBR-SEQ-FILE-INSTC-ID
             AND
              (SEQ_FILE_TS       >= :W2120-SEQ-FILE-TS               OR
               SEQ_FILE_INSTC_ID  > :W2120-SEQ-FILE-INSTC-ID)
             AND
              (SEQ_FILE_TS       <= :W2120-ENDBR-SEQ-FILE-TS         OR
               SEQ_FILE_INSTC_ID  < :W2120-ENDBR-SEQ-FILE-INSTC-ID)
             ORDER BY
                 CO_ID,
                 SEQ_FILE_PGM_ID,
                 SEQ_FILE_OUTPT_NM,
                 SEQ_FILE_INSTC_ID,
                 SEQ_FILE_TS
             FOR FETCH ONLY
             OPTIMIZE FOR 1 ROW
           END-EXEC.

           EXEC SQL
                OPEN B2CUR_2120
           END-EXEC.


           EVALUATE SQLCODE

               WHEN ZERO
                    SET  W2120-IO-OK         TO  TRUE

               WHEN OTHER
                    SET  W2120-IO-ERROR      TO  TRUE

           END-EVALUATE.


       2102-BROWSE-X.
           EXIT.


      **********************
       3000-EXEC-FETCH-NEXT.
      **********************

           EVALUATE WS-OPTM-SQL-EXEC

               WHEN '02'
                    PERFORM  3102-FETCH-NEXT
                        THRU 3102-FETCH-NEXT-X

               WHEN OTHER
                    PERFORM  3102-FETCH-NEXT
                        THRU 3102-FETCH-NEXT-X

           END-EVALUATE.


           SET  W2120-OPTM-SQL-OK            TO  TRUE.
           MOVE SPACES                       TO  W2120-OPTM-SQL-REQIR.
           MOVE SPACES                       TO  W2120-OPTM-SQL-EXEC.


       3000-EXEC-FETCH-NEXT-X.
           EXIT.


      *****************
       3102-FETCH-NEXT.
      *****************

           EXEC SQL
             FETCH B2CUR_2120
             INTO
                 :R2120-CO-ID,
                 :R2120-SEQ-FILE-PGM-ID,
                 :R2120-SEQ-FILE-OUTPT-NM,
                 :R2120-SEQ-FILE-INSTC-ID,
                 :R2120-SEQ-FILE-TS,
                 :R2120-SEQ-FILE-REC-INFO
           END-EXEC.


           EVALUATE SQLCODE

               WHEN ZERO
                    SET  W2120-IO-OK         TO  TRUE
                    MOVE R2120-KEY           TO  W2120-KEY

               WHEN +100
                    SET  W2120-IO-EOF        TO  TRUE

               WHEN OTHER
                    SET  W2120-IO-ERROR      TO  TRUE

           END-EVALUATE.


       3102-FETCH-NEXT-X.
           EXIT.


      *********************
       4000-EXEC-CLOSE-CUR.
      *********************

           EVALUATE WS-OPTM-SQL-EXEC

               WHEN '02'
                    PERFORM  4102-CLOSE-BROWSE-CUR
                        THRU 4102-CLOSE-BROWSE-CUR-X

               WHEN OTHER
                    PERFORM  4102-CLOSE-BROWSE-CUR
                        THRU 4102-CLOSE-BROWSE-CUR-X

           END-EVALUATE.


           SET  WS-OPTM-SQL-CUR-CLOSED       TO  TRUE.


       4000-EXEC-CLOSE-CUR-X.
           EXIT.


      ***********************
       4102-CLOSE-BROWSE-CUR.
      ***********************

           EXEC SQL
                CLOSE B2CUR_2120
           END-EXEC.


           EVALUATE SQLCODE

               WHEN ZERO
                    SET  W2120-IO-OK         TO  TRUE

               WHEN OTHER
                    SET  W2120-IO-ERROR      TO  TRUE

           END-EVALUATE.


       4102-CLOSE-BROWSE-CUR-X.
           EXIT.


      ************************
       5000-EXEC-BROWSE-INDEX.
      ************************

           IF  WS-OPTM-SQL-EXEC NUMERIC
               MOVE WS-OPTM-SQL-EXEC-N       TO  WS-OPTM-SQL-REQIR
           END-IF.


      * PROFILE INDICATED THAT -BROWSE-INDEX
      * PARAGRAPHS ARE NOT APPLICABLE

           SET  W2120-IO-ERROR               TO  TRUE.
           MOVE WS-OPTM-SQL-REQIR            TO  W2120-OPTM-SQL-REQIR.
           MOVE WS-OPTM-SQL-EXEC             TO  W2120-OPTM-SQL-EXEC.

           EVALUATE TRUE

               WHEN W2120-OPTM-SQL-EXEC = W2120-OPTM-SQL-REQIR
                   SET W2120-OPTM-SQL-OK     TO  TRUE

               WHEN W2120-OPTM-SQL-EXEC = SPACES
                   SET W2120-OPTM-SQL-ERROR  TO  TRUE

               WHEN OTHER
                   SET W2120-OPTM-SQL-IMPRV  TO  TRUE

           END-EVALUATE.


       5000-EXEC-BROWSE-INDEX-X.
           EXIT.


      ****************************
       6000-EXEC-FETCH-NEXT-INDEX.
      ****************************

      * PROFILE INDICATED THAT -FETCH-NEXT-INDEX
      * PARAGRAPHS ARE NOT APPLICABLE

           SET  W2120-IO-ERROR               TO  TRUE.
           SET  W2120-OPTM-SQL-OK            TO  TRUE.
           MOVE SPACES                       TO  W2120-OPTM-SQL-REQIR.
           MOVE SPACES                       TO  W2120-OPTM-SQL-EXEC.


       6000-EXEC-FETCH-NEXT-INDEX-X.
           EXIT.


      ***********************
       7000-EXEC-CLOSE-INDEX.
      ***********************

      * PROFILE INDICATED THAT -CLOSE-BROWSE-INDEX
      * PARAGRAPHS ARE NOT APPLICABLE

           SET  W2120-IO-ERROR               TO  TRUE.
           SET  WS-OPTM-SQL-CUR-CLOSED       TO  TRUE.


       7000-EXEC-CLOSE-INDEX-X.
           EXIT.


      *****************************************************************
      **                 END OF PROGRAM ASIB2120                     **
      *****************************************************************
