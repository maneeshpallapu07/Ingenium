      *************************
       IDENTIFICATION DIVISION.
      *************************

       PROGRAM-ID. ASIAXMLE.

       COPY XCWWCRHT.

      *****************************************************************
      **  MEMBER :  ASIAXMLE                                         **
      **  REMARKS:  SQL I/O PROGRAM USED TO INSERT A ROW INTO THE    **
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
      ***************
       DATA DIVISION.
      ***************
      /
      *************************
       WORKING-STORAGE SECTION.
      *************************

       COPY XCWWPGWS REPLACING '$VAR1' BY 'ASIAXMLE'.

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

               WHEN WXMLE-RQST-INSERT
                    PERFORM  1000-INSERT
                        THRU 1000-INSERT-X

               WHEN OTHER
                    SET  WXMLE-IO-ERROR      TO  TRUE

           END-EVALUATE.


           SET  WXMLE-OPTM-SQL-OK            TO  TRUE.
           MOVE SPACES                       TO  WXMLE-OPTM-SQL-REQIR.
           MOVE SPACES                       TO  WXMLE-OPTM-SQL-EXEC.


           GOBACK.


       0000-MAINLINE-X.
           EXIT.


      *************
       1000-INSERT.
      *************

           MOVE WXMLE-KEY                    TO  RXMLE-KEY.

           EXEC SQL
             INSERT INTO TXMLE
                (CO_ID,
                 APP_ID,
                 APP_CHNL_CD,
                 APP_UPLD_DT,
                 APP_REJ_REASN_CD)
             VALUES
               (:RXMLE-CO-ID,
                :RXMLE-APP-ID,
                :RXMLE-APP-CHNL-CD,
                :RXMLE-APP-UPLD-DT,
                :RXMLE-APP-REJ-REASN-CD)
           END-EXEC.


           EVALUATE SQLCODE

               WHEN ZERO
                    SET  WXMLE-IO-OK         TO  TRUE

               WHEN OTHER
                    SET  WXMLE-IO-ERROR      TO  TRUE

           END-EVALUATE.


       1000-INSERT-X.
           EXIT.


      *****************************************************************
      **                 END OF PROGRAM ASIAXMLE                     **
      *****************************************************************
