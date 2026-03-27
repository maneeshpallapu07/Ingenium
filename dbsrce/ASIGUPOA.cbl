      *************************
       IDENTIFICATION DIVISION.
      *************************

       PROGRAM-ID. ASIGUPOA.

       COPY XCWWCRHT.

      *****************************************************************
      **  MEMBER :  ASIGUPOA                                         **
      **  REMARKS:  SQL I/O PROGRAM USED TO DELETE ROWS USING AN     **
      **            EXACT KEY OR KEY RANGE FROM THE APPLICATION      **
      **            UPLOAD POLICY TABLE (ALT. ACCESS)                **
      *****************************************************************
      **  RELEASE   DESCRIPTION                                      **
      **                                                             **
      **  612J      CREATED FOR UPOL TABLE PROCESSING                **
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

       COPY XCWWPGWS REPLACING '$VAR1' BY 'ASIGUPOA'.

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

           EXEC SQL INCLUDE ACFWUPOA  END-EXEC.

      /
       PROCEDURE DIVISION USING SQLCA
                                WUPOA-IO-WORK-AREA.

      ***************
       0000-MAINLINE.
      ***************

           IF  WS-WA-ADDRESS NOT = ADDRESS OF WUPOA-IO-WORK-AREA
               SET  WS-WA-ADDRESS  TO  ADDRESS OF WUPOA-IO-WORK-AREA
               MOVE ZERO           TO  SQL-INIT-FLAG
           END-IF.


           EVALUATE TRUE

               WHEN WUPOA-RQST-DELETE-KEY-RANGE
                    PERFORM  1000-DETERMINE-SQL-REQIR
                        THRU 1000-DETERMINE-SQL-REQIR-X
                    PERFORM  2000-EXEC-DELETE-KEY-RANGE
                        THRU 2000-EXEC-DELETE-KEY-RANGE-X

               WHEN WUPOA-RQST-DELETE-WITH-KEY
                    PERFORM  3000-DELETE-WITH-KEY
                        THRU 3000-DELETE-WITH-KEY-X

               WHEN OTHER
                    SET  WUPOA-IO-ERROR      TO  TRUE

           END-EVALUATE.


           GOBACK.


       0000-MAINLINE-X.
           EXIT.


      **************************
       1000-DETERMINE-SQL-REQIR.
      **************************

           MOVE 1                            TO  WS-OPTM-SQL-REQIR.

           IF  WUPOA-APP-UPLD-DT = WUPOA-ENDBR-APP-UPLD-DT
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

               WHEN WS-OPTM-SQL-REQIR <= 1
                    MOVE '01'                TO  WS-OPTM-SQL-EXEC
                    PERFORM  210G-DELETE-KEY-RANGE
                        THRU 210G-DELETE-KEY-RANGE-X

               WHEN OTHER
                    MOVE SPACES              TO  WS-OPTM-SQL-EXEC
                    SET  WUPOA-IO-ERROR      TO  TRUE

           END-EVALUATE.


           MOVE WS-OPTM-SQL-REQIR            TO  WUPOA-OPTM-SQL-REQIR.
           MOVE WS-OPTM-SQL-EXEC             TO  WUPOA-OPTM-SQL-EXEC.

           IF  WUPOA-OPTM-SQL-EXEC = WUPOA-OPTM-SQL-REQIR
               SET WUPOA-OPTM-SQL-OK         TO  TRUE
           ELSE
               IF  WUPOA-OPTM-SQL-EXEC = SPACES
                   SET WUPOA-OPTM-SQL-ERROR  TO  TRUE
               ELSE
                   SET WUPOA-OPTM-SQL-IMPRV  TO  TRUE
               END-IF
           END-IF.


       2000-EXEC-DELETE-KEY-RANGE-X.
           EXIT.


      ***********************
       210G-DELETE-KEY-RANGE.
      ***********************

           EXEC SQL
             DELETE FROM TUPOL
             WHERE
                 APP_UPLD_DT  BETWEEN
                                :WUPOA-APP-UPLD-DT       AND
                                :WUPOA-ENDBR-APP-UPLD-DT
           END-EXEC.


           EVALUATE SQLCODE

               WHEN ZERO
                    SET  WUPOA-IO-OK         TO  TRUE

               WHEN +100
                    SET  WUPOA-IO-NOT-FOUND  TO  TRUE

               WHEN OTHER
                    SET  WUPOA-IO-ERROR      TO  TRUE

           END-EVALUATE.


       210G-DELETE-KEY-RANGE-X.
           EXIT.


      **********************
       3000-DELETE-WITH-KEY.
      **********************

           EXEC SQL
             DELETE FROM TUPOL
             WHERE
                 APP_UPLD_DT = :WUPOA-APP-UPLD-DT
           END-EXEC.


           EVALUATE SQLCODE

               WHEN ZERO
                    SET  WUPOA-IO-OK         TO  TRUE

               WHEN +100
                    SET  WUPOA-IO-NOT-FOUND  TO  TRUE

               WHEN OTHER
                    SET  WUPOA-IO-ERROR      TO  TRUE

           END-EVALUATE.


           SET  WUPOA-OPTM-SQL-OK            TO  TRUE.
           MOVE SPACES                       TO  WUPOA-OPTM-SQL-REQIR.
           MOVE SPACES                       TO  WUPOA-OPTM-SQL-EXEC.


       3000-DELETE-WITH-KEY-X.
           EXIT.


      *****************************************************************
      **                 END OF PROGRAM ASIGUPOA                     **
      *****************************************************************
