      *************************
       IDENTIFICATION DIVISION.
      *************************

       PROGRAM-ID. ASIACWAE.

       COPY XCWWCRHT.

      *****************************************************************
      **  MEMBER :  ASIACWAE                                         **
      **  REMARKS:  SQL I/O PROGRAM USED TO INSERT A ROW INTO THE    **
      **            CWA ERROR TABLE                                  **
      *****************************************************************
      **  RELEASE   DESCRIPTION                                      **
      **                                                             **
      **  25JUL09   CREATED FOR CWAE PROCESSING                      **
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

       COPY XCWWPGWS REPLACING '$VAR1' BY 'ASIACWAE'.

       01  WS-WORKING-STORAGE.
           05  WS-WA-ADDRESS                    POINTER   VALUE NULL.
           05  WS-TL-ADDRESS                    POINTER   VALUE NULL.
      /
       COPY XCWWWKDT.
      /
           EXEC SQL INCLUDE ACWZCWAE  END-EXEC.
      /
      *****************
       LINKAGE SECTION.
      *****************

           EXEC SQL INCLUDE SQLCA     END-EXEC.

           EXEC SQL INCLUDE ACFWCWAE  END-EXEC.

           EXEC SQL INCLUDE ACFRCWAE  END-EXEC.

      /
       PROCEDURE DIVISION USING SQLCA
                                WCWAE-IO-WORK-AREA
                                RCWAE-REC-INFO.

      ***************
       0000-MAINLINE.
      ***************

           IF  WS-WA-ADDRESS NOT = ADDRESS OF WCWAE-IO-WORK-AREA
           OR  WS-TL-ADDRESS NOT = ADDRESS OF RCWAE-REC-INFO
               SET  WS-WA-ADDRESS  TO  ADDRESS OF WCWAE-IO-WORK-AREA
               SET  WS-TL-ADDRESS  TO  ADDRESS OF RCWAE-REC-INFO
               MOVE ZERO           TO  SQL-INIT-FLAG
           END-IF.


           EVALUATE TRUE

               WHEN WCWAE-RQST-INSERT
                    PERFORM  1000-INSERT
                        THRU 1000-INSERT-X

               WHEN OTHER
                    SET  WCWAE-IO-ERROR      TO  TRUE

           END-EVALUATE.


           SET  WCWAE-OPTM-SQL-OK            TO  TRUE.
           MOVE SPACES                       TO  WCWAE-OPTM-SQL-REQIR.
           MOVE SPACES                       TO  WCWAE-OPTM-SQL-EXEC.


           GOBACK.


       0000-MAINLINE-X.
           EXIT.


      *************
       1000-INSERT.
      *************

           PERFORM  CWAE-1000-SET-NULL-IND
               THRU CWAE-1000-SET-NULL-IND-X.

           MOVE WCWAE-KEY                    TO  RCWAE-KEY.

           EXEC SQL
             INSERT INTO TCWAE
                (CO_ID,
                 APP_ID,
                 SEQ_NUM,
                 CWA_CHNL_CD,
                 CWA_UPLD_DT,
                 RECPT_NUM,
                 RECPT_AMT,
                 RECPT_DT,
                 PMT_TYP_CD,
                 REJ_REASN_CD)
             VALUES
               (:RCWAE-CO-ID,
                :RCWAE-APP-ID,
                :RCWAE-SEQ-NUM,
                :RCWAE-CWA-CHNL-CD,
                :RCWAE-CWA-UPLD-DT,
                :RCWAE-RECPT-NUM,
                :RCWAE-RECPT-AMT    :ZCWAE-RECPT-AMT-NI,
                :RCWAE-RECPT-DT     :ZCWAE-RECPT-DT-NI,
                :RCWAE-PMT-TYP-CD,
                :RCWAE-REJ-REASN-CD)
           END-EXEC.


           EVALUATE SQLCODE

               WHEN ZERO
                    SET  WCWAE-IO-OK         TO  TRUE

               WHEN OTHER
                    SET  WCWAE-IO-ERROR      TO  TRUE

           END-EVALUATE.


       1000-INSERT-X.
           EXIT.


       COPY ACPZCWAE.

      *****************************************************************
      **                 END OF PROGRAM ASIACWAE                     **
      *****************************************************************
