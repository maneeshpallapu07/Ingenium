      *************************
       IDENTIFICATION DIVISION.
      *************************

       PROGRAM-ID. ASIG2120.

       COPY XCWWCRHT.

      *****************************************************************
      **  MEMBER :  ASIG2120                                         **
      **  REMARKS:  SQL I/O PROGRAM USED TO DELETE ROWS USING AN     **
      **            EXACT KEY OR KEY RANGE FROM THE APEX TO NBS      **
      **            IMPORT MESSAGE EXTRACT TABLE                     **
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

       COPY XCWWPGWS REPLACING '$VAR1' BY 'ASIG2120'.

       01  WS-WORKING-STORAGE.
           05  WS-WA-ADDRESS                    POINTER   VALUE NULL.
           05  WS-TL-ADDRESS                    POINTER   VALUE NULL.
           05  WS-OPTM-SQL-REQIR                PIC 9(02).
           05  WS-OPTM-SQL-EXEC                 PIC X(02).
      /
       COPY XCWWWKDT.
      /
      *****************
       LINKAGE SECTION.
      *****************

           EXEC SQL INCLUDE SQLCA     END-EXEC.

           EXEC SQL INCLUDE ACFW2120  END-EXEC.

      /
       PROCEDURE DIVISION USING SQLCA
                                W2120-IO-WORK-AREA.

      ***************
       0000-MAINLINE.
      ***************

           IF  WS-WA-ADDRESS NOT = ADDRESS OF W2120-IO-WORK-AREA
               SET  WS-WA-ADDRESS  TO  ADDRESS OF W2120-IO-WORK-AREA
               MOVE ZERO           TO  SQL-INIT-FLAG
           END-IF.


           EVALUATE TRUE

               WHEN W2120-RQST-DELETE-KEY-RANGE
                    PERFORM  1000-DETERMINE-SQL-REQIR
                        THRU 1000-DETERMINE-SQL-REQIR-X
                    PERFORM  2000-EXEC-DELETE-KEY-RANGE
                        THRU 2000-EXEC-DELETE-KEY-RANGE-X

               WHEN W2120-RQST-DELETE-WITH-KEY
                    PERFORM  3000-DELETE-WITH-KEY
                        THRU 3000-DELETE-WITH-KEY-X

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


      ****************************
       2000-EXEC-DELETE-KEY-RANGE.
      ****************************

           EVALUATE TRUE

               WHEN WS-OPTM-SQL-REQIR <= 2
                    MOVE '02'                TO  WS-OPTM-SQL-EXEC
                    PERFORM  2102-DELETE-KEY-RANGE
                        THRU 2102-DELETE-KEY-RANGE-X

               WHEN OTHER
                    MOVE SPACES              TO  WS-OPTM-SQL-EXEC
                    SET  W2120-IO-ERROR      TO  TRUE

           END-EVALUATE.


           MOVE WS-OPTM-SQL-REQIR            TO  W2120-OPTM-SQL-REQIR.
           MOVE WS-OPTM-SQL-EXEC             TO  W2120-OPTM-SQL-EXEC.

           IF  W2120-OPTM-SQL-EXEC = W2120-OPTM-SQL-REQIR
               SET W2120-OPTM-SQL-OK         TO  TRUE
           ELSE
               IF  W2120-OPTM-SQL-EXEC = SPACES
                   SET W2120-OPTM-SQL-ERROR  TO  TRUE
               ELSE
                   SET W2120-OPTM-SQL-IMPRV  TO  TRUE
               END-IF
           END-IF.


       2000-EXEC-DELETE-KEY-RANGE-X.
           EXIT.


      ***********************
       2102-DELETE-KEY-RANGE.
      ***********************

           EXEC SQL
             DELETE FROM S2120
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
           END-EXEC.


           EVALUATE SQLCODE

               WHEN ZERO
                    SET  W2120-IO-OK         TO  TRUE

               WHEN +100
                    SET  W2120-IO-NOT-FOUND  TO  TRUE

               WHEN OTHER
                    SET  W2120-IO-ERROR      TO  TRUE

           END-EVALUATE.


       2102-DELETE-KEY-RANGE-X.
           EXIT.


      **********************
       3000-DELETE-WITH-KEY.
      **********************

           EXEC SQL
             DELETE FROM S2120
             WHERE
                 CO_ID             = :W2120-CO-ID             AND
                 SEQ_FILE_PGM_ID   = :W2120-SEQ-FILE-PGM-ID   AND
                 SEQ_FILE_OUTPT_NM = :W2120-SEQ-FILE-OUTPT-NM AND
                 SEQ_FILE_INSTC_ID = :W2120-SEQ-FILE-INSTC-ID AND
                 SEQ_FILE_TS       = :W2120-SEQ-FILE-TS
           END-EXEC.


           EVALUATE SQLCODE

               WHEN ZERO
                    SET  W2120-IO-OK         TO  TRUE

               WHEN +100
                    SET  W2120-IO-NOT-FOUND  TO  TRUE

               WHEN OTHER
                    SET  W2120-IO-ERROR      TO  TRUE

           END-EVALUATE.


           SET  W2120-OPTM-SQL-OK            TO  TRUE.
           MOVE SPACES                       TO  W2120-OPTM-SQL-REQIR.
           MOVE SPACES                       TO  W2120-OPTM-SQL-EXEC.


       3000-DELETE-WITH-KEY-X.
           EXIT.


      *****************************************************************
      **                 END OF PROGRAM ASIG2120                     **
      *****************************************************************
