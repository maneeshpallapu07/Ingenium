      *************************
       IDENTIFICATION DIVISION.
      *************************

       PROGRAM-ID. ASIB2110.

       COPY XCWWCRHT.

      *****************************************************************
      **  MEMBER :  ASIB2110                                         **
      **  REMARKS:  SQL I/O PROGRAM USED TO BROWSE ROWS IN THE       **
      **            APEX TO NBS IMPORT AUDIT EXTRACT TABLE           **
      *****************************************************************
      *****************************************************************
      **  MEMBER :  ASIB2110                                         **
      **  REMARKS:  SQL I/O PROGRAM USED TO BROWSE ROWS IN THE       **
      **            APEX TO NBS IMPORT AUDIT EXTRACT TABLE           **
      *****************************************************************
      **  RELEASE   DESCRIPTION                                      **
      **                                                             **
      **  612J      CREATED FOR 2110 TABLE PROCESSING                **
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

       COPY XCWWPGWS REPLACING '$VAR1' BY 'ASIB2110'.

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

           EXEC SQL INCLUDE ACFW2110  END-EXEC.

           EXEC SQL INCLUDE ACFR2110  END-EXEC.

      /
       PROCEDURE DIVISION USING SQLCA
                                W2110-IO-WORK-AREA
                                R2110-REC-INFO.

      ***************
       0000-MAINLINE.
      ***************

           IF  WS-WA-ADDRESS NOT = ADDRESS OF W2110-IO-WORK-AREA
           OR  WS-TL-ADDRESS NOT = ADDRESS OF R2110-REC-INFO
               SET  WS-WA-ADDRESS  TO  ADDRESS OF W2110-IO-WORK-AREA
               SET  WS-TL-ADDRESS  TO  ADDRESS OF R2110-REC-INFO
               MOVE ZERO           TO  SQL-INIT-FLAG
           END-IF.


           EVALUATE TRUE

               WHEN W2110-RQST-BROWSE
                    PERFORM  1000-DETERMINE-SQL-REQIR
                        THRU 1000-DETERMINE-SQL-REQIR-X
                    PERFORM  2000-EXEC-BROWSE
                        THRU 2000-EXEC-BROWSE-X

               WHEN W2110-RQST-FETCH-NEXT
                    PERFORM  3000-EXEC-FETCH-NEXT
                        THRU 3000-EXEC-FETCH-NEXT-X

               WHEN W2110-RQST-CLOSE-BROWSE-CUR
                    PERFORM  4000-EXEC-CLOSE-CUR
                        THRU 4000-EXEC-CLOSE-CUR-X

               WHEN W2110-RQST-BROWSE-INDEX
                    PERFORM  1000-DETERMINE-SQL-REQIR
                        THRU 1000-DETERMINE-SQL-REQIR-X
                    PERFORM  5000-EXEC-BROWSE-INDEX
                        THRU 5000-EXEC-BROWSE-INDEX-X

               WHEN W2110-RQST-FETCH-NEXT-INDEX
                    PERFORM  6000-EXEC-FETCH-NEXT-INDEX
                        THRU 6000-EXEC-FETCH-NEXT-INDEX-X

               WHEN W2110-RQST-CLOSE-BROWSE-INDEX
                    PERFORM  7000-EXEC-CLOSE-INDEX
                        THRU 7000-EXEC-CLOSE-INDEX-X

               WHEN OTHER
                    SET  W2110-IO-ERROR      TO  TRUE

           END-EVALUATE.


           GOBACK.


       0000-MAINLINE-X.
           EXIT.


      **************************
       1000-DETERMINE-SQL-REQIR.
      **************************

           MOVE 5                            TO  WS-OPTM-SQL-REQIR.

           IF  W2110-CO-ID = W2110-ENDBR-CO-ID
               SUBTRACT +1                   FROM WS-OPTM-SQL-REQIR
           ELSE
               GO TO 1000-DETERMINE-SQL-REQIR-X
           END-IF.


           IF  W2110-SEQ-FILE-PGM-ID = W2110-ENDBR-SEQ-FILE-PGM-ID
               SUBTRACT +1                   FROM WS-OPTM-SQL-REQIR
           ELSE
               GO TO 1000-DETERMINE-SQL-REQIR-X
           END-IF.


           IF  W2110-SEQ-FILE-OUTPT-NM = W2110-ENDBR-SEQ-FILE-OUTPT-NM
               SUBTRACT +1                   FROM WS-OPTM-SQL-REQIR
           ELSE
               GO TO 1000-DETERMINE-SQL-REQIR-X
           END-IF.


           IF  W2110-SEQ-FILE-INSTC-ID = W2110-ENDBR-SEQ-FILE-INSTC-ID
               SUBTRACT +1                   FROM WS-OPTM-SQL-REQIR
           ELSE
               GO TO 1000-DETERMINE-SQL-REQIR-X
           END-IF.


           IF  W2110-SEQ-FILE-TS = W2110-ENDBR-SEQ-FILE-TS
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
                    SET  W2110-IO-ERROR      TO  TRUE

           END-EVALUATE.


           MOVE WS-OPTM-SQL-REQIR            TO  W2110-OPTM-SQL-REQIR.
           MOVE WS-OPTM-SQL-EXEC             TO  W2110-OPTM-SQL-EXEC.

           EVALUATE TRUE

               WHEN W2110-OPTM-SQL-EXEC = W2110-OPTM-SQL-REQIR
                   SET W2110-OPTM-SQL-OK     TO  TRUE

               WHEN W2110-OPTM-SQL-EXEC = SPACES
                   SET W2110-OPTM-SQL-ERROR  TO  TRUE

               WHEN OTHER
                   SET W2110-OPTM-SQL-IMPRV  TO  TRUE

           END-EVALUATE.


       2000-EXEC-BROWSE-X.
           EXIT.


      *************
       2102-BROWSE.
      *************

           EXEC SQL
             DECLARE B2CUR_2110 CURSOR FOR
             SELECT
                 CO_ID,
                 SEQ_FILE_PGM_ID,
                 SEQ_FILE_OUTPT_NM,
                 SEQ_FILE_INSTC_ID,
                 SEQ_FILE_TS,
                 SEQ_FILE_REC_INFO
             FROM S2110
             WHERE
               CO_ID              = :W2110-CO-ID                     AND
               SEQ_FILE_PGM_ID    = :W2110-SEQ-FILE-PGM-ID           AND
               SEQ_FILE_OUTPT_NM  = :W2110-SEQ-FILE-OUTPT-NM
             AND
               SEQ_FILE_INSTC_ID  BETWEEN
                                    :W2110-SEQ-FILE-INSTC-ID         AND
                                    :W2110-ENDBR-SEQ-FILE-INSTC-ID
             AND
              (SEQ_FILE_TS       >= :W2110-SEQ-FILE-TS               OR
               SEQ_FILE_INSTC_ID  > :W2110-SEQ-FILE-INSTC-ID)
             AND
              (SEQ_FILE_TS       <= :W2110-ENDBR-SEQ-FILE-TS         OR
               SEQ_FILE_INSTC_ID  < :W2110-ENDBR-SEQ-FILE-INSTC-ID)
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
                OPEN B2CUR_2110
           END-EXEC.


           EVALUATE SQLCODE

               WHEN ZERO
                    SET  W2110-IO-OK         TO  TRUE

               WHEN OTHER
                    SET  W2110-IO-ERROR      TO  TRUE

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


           SET  W2110-OPTM-SQL-OK            TO  TRUE.
           MOVE SPACES                       TO  W2110-OPTM-SQL-REQIR.
           MOVE SPACES                       TO  W2110-OPTM-SQL-EXEC.


       3000-EXEC-FETCH-NEXT-X.
           EXIT.


      *****************
       3102-FETCH-NEXT.
      *****************

           EXEC SQL
             FETCH B2CUR_2110
             INTO
                 :R2110-CO-ID,
                 :R2110-SEQ-FILE-PGM-ID,
                 :R2110-SEQ-FILE-OUTPT-NM,
                 :R2110-SEQ-FILE-INSTC-ID,
                 :R2110-SEQ-FILE-TS,
                 :R2110-SEQ-FILE-REC-INFO
           END-EXEC.


           EVALUATE SQLCODE

               WHEN ZERO
                    SET  W2110-IO-OK         TO  TRUE
                    MOVE R2110-KEY           TO  W2110-KEY

               WHEN +100
                    SET  W2110-IO-EOF        TO  TRUE

               WHEN OTHER
                    SET  W2110-IO-ERROR      TO  TRUE

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
                CLOSE B2CUR_2110
           END-EXEC.


           EVALUATE SQLCODE

               WHEN ZERO
                    SET  W2110-IO-OK         TO  TRUE

               WHEN OTHER
                    SET  W2110-IO-ERROR      TO  TRUE

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

           SET  W2110-IO-ERROR               TO  TRUE.
           MOVE WS-OPTM-SQL-REQIR            TO  W2110-OPTM-SQL-REQIR.
           MOVE WS-OPTM-SQL-EXEC             TO  W2110-OPTM-SQL-EXEC.

           EVALUATE TRUE

               WHEN W2110-OPTM-SQL-EXEC = W2110-OPTM-SQL-REQIR
                   SET W2110-OPTM-SQL-OK     TO  TRUE

               WHEN W2110-OPTM-SQL-EXEC = SPACES
                   SET W2110-OPTM-SQL-ERROR  TO  TRUE

               WHEN OTHER
                   SET W2110-OPTM-SQL-IMPRV  TO  TRUE

           END-EVALUATE.


       5000-EXEC-BROWSE-INDEX-X.
           EXIT.


      ****************************
       6000-EXEC-FETCH-NEXT-INDEX.
      ****************************

      * PROFILE INDICATED THAT -FETCH-NEXT-INDEX
      * PARAGRAPHS ARE NOT APPLICABLE

           SET  W2110-IO-ERROR               TO  TRUE.
           SET  W2110-OPTM-SQL-OK            TO  TRUE.
           MOVE SPACES                       TO  W2110-OPTM-SQL-REQIR.
           MOVE SPACES                       TO  W2110-OPTM-SQL-EXEC.


       6000-EXEC-FETCH-NEXT-INDEX-X.
           EXIT.


      ***********************
       7000-EXEC-CLOSE-INDEX.
      ***********************

      * PROFILE INDICATED THAT -CLOSE-BROWSE-INDEX
      * PARAGRAPHS ARE NOT APPLICABLE

           SET  W2110-IO-ERROR               TO  TRUE.
           SET  WS-OPTM-SQL-CUR-CLOSED       TO  TRUE.


       7000-EXEC-CLOSE-INDEX-X.
           EXIT.


      *****************************************************************
      **                 END OF PROGRAM ASIB2110                     **
      *****************************************************************
