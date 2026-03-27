      *************************
       IDENTIFICATION DIVISION.
      *************************

       PROGRAM-ID. ASIACLUM.

       COPY XCWWCRHT.

      *****************************************************************
      **  MEMBER :  ASIACLUM                                         **
      **  REMARKS:  SQL I/O PROGRAM USED TO INSERT A ROW INTO THE    **
      **            ARM 2 NON FACE CLIENT UNMATCHED EXTRACT TABLE    **
      *****************************************************************
      **  RELEASE   DESCRIPTION                                      **
      **                                                             **
      **  30NOV14   CREATED FOR CLUM PROCESSING                      **
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

       COPY XCWWPGWS REPLACING '$VAR1' BY 'CSIACLUM'.

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

           EXEC SQL INCLUDE ACFWCLUM  END-EXEC.

           EXEC SQL INCLUDE ACFRCLUM  END-EXEC.

      /
       PROCEDURE DIVISION USING SQLCA
                                WCLUM-IO-WORK-AREA
                                RCLUM-REC-INFO.

      ***************
       0000-MAINLINE.
      ***************

           IF  WS-WA-ADDRESS NOT = ADDRESS OF WCLUM-IO-WORK-AREA
           OR  WS-TL-ADDRESS NOT = ADDRESS OF RCLUM-REC-INFO
               SET  WS-WA-ADDRESS  TO  ADDRESS OF WCLUM-IO-WORK-AREA
               SET  WS-TL-ADDRESS  TO  ADDRESS OF RCLUM-REC-INFO
               MOVE ZERO           TO  SQL-INIT-FLAG
           END-IF.


           EVALUATE TRUE

               WHEN WCLUM-RQST-INSERT
                    PERFORM  1000-INSERT
                        THRU 1000-INSERT-X

               WHEN OTHER
                    SET  WCLUM-IO-ERROR      TO  TRUE

           END-EVALUATE.


           SET  WCLUM-OPTM-SQL-OK            TO  TRUE.
           MOVE SPACES                       TO  WCLUM-OPTM-SQL-REQIR.
           MOVE SPACES                       TO  WCLUM-OPTM-SQL-EXEC.


           GOBACK.


       0000-MAINLINE-X.
           EXIT.


      *************
       1000-INSERT.
      *************

           MOVE WCLUM-KEY                    TO  RCLUM-KEY.

           EXEC SQL
             INSERT INTO SCLUM
                (CO_ID,
                 SEQ_FILE_PGM_ID,
                 SEQ_FILE_OUTPT_NM,
                 SEQ_FILE_INSTC_ID,
                 SEQ_FILE_TS,
                 SEQ_FILE_REC_INFO)
             VALUES
               (:RCLUM-CO-ID,
                :RCLUM-SEQ-FILE-PGM-ID,
                :RCLUM-SEQ-FILE-OUTPT-NM,
                :RCLUM-SEQ-FILE-INSTC-ID,
                CURRENT TIMESTAMP,
                :RCLUM-SEQ-FILE-REC-INFO)
           END-EXEC.


           EVALUATE SQLCODE

               WHEN ZERO
                    SET  WCLUM-IO-OK         TO  TRUE

               WHEN OTHER
                    SET  WCLUM-IO-ERROR      TO  TRUE

           END-EVALUATE.


       1000-INSERT-X.
           EXIT.


      *****************************************************************
      **                 END OF PROGRAM ASIACLUM                     **
      *****************************************************************
