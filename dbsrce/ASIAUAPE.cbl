      *************************
       IDENTIFICATION DIVISION.
      *************************

       PROGRAM-ID. ASIAUAPE.

       COPY XCWWCRHT.

      *****************************************************************
      **  MEMBER :  ASIAUAPE                                         **
      **  REMARKS:  SQL I/O PROGRAM USED TO INSERT A ROW INTO THE    **
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

       COPY XCWWPGWS REPLACING '$VAR1' BY 'ASIAUAPE'.

       01  WS-WORKING-STORAGE.
           05  WS-WA-ADDRESS                    POINTER   VALUE NULL.
           05  WS-TL-ADDRESS                    POINTER   VALUE NULL.
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

               WHEN WUAPE-RQST-INSERT
                    PERFORM  1000-INSERT
                        THRU 1000-INSERT-X

               WHEN OTHER
                    SET  WUAPE-IO-ERROR      TO  TRUE

           END-EVALUATE.


           SET  WUAPE-OPTM-SQL-OK            TO  TRUE.
           MOVE SPACES                       TO  WUAPE-OPTM-SQL-REQIR.
           MOVE SPACES                       TO  WUAPE-OPTM-SQL-EXEC.


           GOBACK.


       0000-MAINLINE-X.
           EXIT.


      *************
       1000-INSERT.
      *************

           PERFORM  UAPE-1000-SET-NULL-IND
               THRU UAPE-1000-SET-NULL-IND-X.

           MOVE WUAPE-KEY                    TO  RUAPE-KEY.

           EXEC SQL
             INSERT INTO TUAPE
                (APP_ID,
                 INCMPLT_SEQ_NUM,
                 INCMPLT_ID,
                 INCMPLT_DTL_TXT)
             VALUES
               (:RUAPE-APP-ID,
                :RUAPE-INCMPLT-SEQ-NUM,
                :RUAPE-INCMPLT-ID      :ZUAPE-INCMPLT-ID-NI,
                :RUAPE-INCMPLT-DTL-TXT :ZUAPE-INCMPLT-DTL-TXT-NI)
           END-EXEC.


           EVALUATE SQLCODE

               WHEN ZERO
                    SET  WUAPE-IO-OK         TO  TRUE

               WHEN OTHER
                    SET  WUAPE-IO-ERROR      TO  TRUE

           END-EVALUATE.


       1000-INSERT-X.
           EXIT.


       COPY ACPZUAPE.

      *****************************************************************
      **                 END OF PROGRAM ASIAUAPE                     **
      *****************************************************************
