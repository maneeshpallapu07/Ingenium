      *************************
       IDENTIFICATION DIVISION.
      *************************

       PROGRAM-ID. ASIA2110.

       COPY XCWWCRHT.

      *****************************************************************
      **  MEMBER :  ASIA2110                                         **
      **  REMARKS:  SQL I/O PROGRAM USED TO INSERT A ROW INTO THE    **
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

       COPY XCWWPGWS REPLACING '$VAR1' BY 'ASIA2110'.

       01  WS-WORKING-STORAGE.
           05  WS-WA-ADDRESS                    POINTER   VALUE NULL.
           05  WS-TL-ADDRESS                    POINTER   VALUE NULL.
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

               WHEN W2110-RQST-INSERT
                    PERFORM  1000-INSERT
                        THRU 1000-INSERT-X

               WHEN OTHER
                    SET  W2110-IO-ERROR      TO  TRUE

           END-EVALUATE.


           SET  W2110-OPTM-SQL-OK            TO  TRUE.
           MOVE SPACES                       TO  W2110-OPTM-SQL-REQIR.
           MOVE SPACES                       TO  W2110-OPTM-SQL-EXEC.


           GOBACK.


       0000-MAINLINE-X.
           EXIT.


      *************
       1000-INSERT.
      *************

           MOVE W2110-KEY                    TO  R2110-KEY.

           EXEC SQL
             INSERT INTO S2110
                (CO_ID,
                 SEQ_FILE_PGM_ID,
                 SEQ_FILE_OUTPT_NM,
                 SEQ_FILE_INSTC_ID,
                 SEQ_FILE_TS,
                 SEQ_FILE_REC_INFO)
             VALUES
               (:R2110-CO-ID,
                :R2110-SEQ-FILE-PGM-ID,
                :R2110-SEQ-FILE-OUTPT-NM,
                :R2110-SEQ-FILE-INSTC-ID,
                CURRENT TIMESTAMP,
                :R2110-SEQ-FILE-REC-INFO)
           END-EXEC.


           EVALUATE SQLCODE

               WHEN ZERO
                    SET  W2110-IO-OK         TO  TRUE

               WHEN OTHER
                    SET  W2110-IO-ERROR      TO  TRUE

           END-EVALUATE.


       1000-INSERT-X.
           EXIT.


      *****************************************************************
      **                 END OF PROGRAM ASIA2110                     **
      *****************************************************************
