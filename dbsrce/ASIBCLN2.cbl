      *************************
       IDENTIFICATION DIVISION.
      *************************

       PROGRAM-ID. ASIBCLN2.

       COPY XCWWCRHT.

      *****************************************************************
      **  MEMBER :  ASIBCLN2                                         **
      **  REMARKS:  SQL I/O PROGRAM USED TO BROWSE ROWS IN THE       **
      **            POLICY TABLE (CUSTOM SQL).                       **
      *****************************************************************
      **  RELEASE   DESCRIPTION                                      **
      **  TLB003    XML& CWA UPLOADS FOR BUNDLE PROCESSING           **
      *****************************************************************
      /
      **********************
       ENVIRONMENT DIVISION.
      **********************
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

       COPY XCWWPGWS REPLACING '$VAR1' BY 'ASIBCLN2'.

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
      
      /
      *****************
       LINKAGE SECTION.
      *****************

           EXEC SQL INCLUDE SQLCA     END-EXEC.

           EXEC SQL INCLUDE ACFWCLN2  END-EXEC.

           EXEC SQL INCLUDE ACFRCLN2   END-EXEC.

      /
       PROCEDURE DIVISION USING SQLCA
                                WCLN2-IO-WORK-AREA
                                RCLN2-REC-INFO.

      ***************
       0000-MAINLINE.
      ***************

           IF  WS-WA-ADDRESS NOT = ADDRESS OF WCLN2-IO-WORK-AREA
           OR  WS-TL-ADDRESS NOT = ADDRESS OF RCLN2-REC-INFO
               SET  WS-WA-ADDRESS  TO  ADDRESS OF WCLN2-IO-WORK-AREA
               SET  WS-TL-ADDRESS  TO  ADDRESS OF RCLN2-REC-INFO
               MOVE ZERO           TO  SQL-INIT-FLAG
           END-IF.


           EVALUATE TRUE

               WHEN WCLN2-RQST-BROWSE
                    PERFORM  1000-DETERMINE-SQL-REQIR
                        THRU 1000-DETERMINE-SQL-REQIR-X
                    PERFORM  2000-EXEC-BROWSE
                        THRU 2000-EXEC-BROWSE-X

               WHEN WCLN2-RQST-FETCH-NEXT
                    PERFORM  3000-EXEC-FETCH-NEXT
                        THRU 3000-EXEC-FETCH-NEXT-X

               WHEN WCLN2-RQST-CLOSE-BROWSE-CUR
                    PERFORM  4000-EXEC-CLOSE-CUR
                        THRU 4000-EXEC-CLOSE-CUR-X

               WHEN OTHER
                    SET  WCLN2-IO-ERROR     TO  TRUE

           END-EVALUATE.


           GOBACK.


       0000-MAINLINE-X.
           EXIT.


      **************************
       1000-DETERMINE-SQL-REQIR.
      **************************

           MOVE 5                            TO  WS-OPTM-SQL-REQIR.

       1000-DETERMINE-SQL-REQIR-X.
           EXIT.


      ******************
       2000-EXEC-BROWSE.
      ******************

           IF  WS-OPTM-SQL-EXEC NUMERIC
               MOVE WS-OPTM-SQL-EXEC-N       TO  WS-OPTM-SQL-REQIR
           END-IF.


           EVALUATE TRUE

               WHEN WS-OPTM-SQL-REQIR <= 5
                    MOVE '05'                TO  WS-OPTM-SQL-EXEC
                    PERFORM  210G-BROWSE
                        THRU 210G-BROWSE-X

               WHEN OTHER
                    MOVE SPACES              TO  WS-OPTM-SQL-EXEC
                    SET  WCLN2-IO-ERROR      TO  TRUE

           END-EVALUATE.


           MOVE WS-OPTM-SQL-REQIR            TO  WCLN2-OPTM-SQL-REQIR.
           MOVE WS-OPTM-SQL-EXEC             TO  WCLN2-OPTM-SQL-EXEC.

           IF  WCLN2-OPTM-SQL-EXEC = WCLN2-OPTM-SQL-REQIR
               SET WCLN2-OPTM-SQL-OK         TO  TRUE
           ELSE
               IF  WCLN2-OPTM-SQL-EXEC = SPACES
                   SET WCLN2-OPTM-SQL-ERROR  TO  TRUE
               ELSE
                   SET WCLN2-OPTM-SQL-IMPRV  TO  TRUE
               END-IF
           END-IF.


       2000-EXEC-BROWSE-X.
           EXIT.


      *************
       210G-BROWSE.
      *************

           EXEC SQL
             DECLARE BCUR_CLN2 CURSOR FOR
            SELECT
                TPOL.CO_ID,
                TPOL.POL_ID          
                FROM   TPOL, TPOLC
                WHERE
                TPOL.CO_ID = :WCLN2-CO-ID
                AND TPOL.POL_APP_UPLD_DT = :WCLN2-CTL-PRCES-DT
                AND TPOL.POL_BULK_APP_ID = ' '
                AND TPOL.POL_BUNDLE_APP_ID = ' '
                AND TPOL.COLI_PROD_IND ='Y'
                AND TPOLC.CO_ID = TPOL.CO_ID
                AND TPOLC.POL_ID = TPOL.POL_ID
                AND TPOLC.POL_CLI_REL_TYP_CD = 'O'
                AND TPOLC.CLI_ID IN
                (SELECT  TCLNC.CLI_ID 
                FROM TCLNC
                WHERE ((TCLNC.CO_ID = 'CP' 
                AND TCLNC.CLI_CO_ENTR_NM = :WCLN2-CLI-KJ-NM               
                AND TCLNC.CLI_CO_GR_CD = 'KJ' 
                AND TCLNC.CLI_CO_NM_TYP_CD = 'CL' )            
                OR (TCLNC.CO_ID = 'CP' 
                AND TCLNC.CLI_CO_ENTR_NM = :WCLN2-CLI-NM             
                AND TCLNC.CLI_CO_GR_CD = 'KA' 
                AND TCLNC.CLI_CO_NM_TYP_CD = 'CL')))
                     
                       ORDER BY
                 POL_ID
                   
             FOR FETCH ONLY
             OPTIMIZE FOR 1 ROW
           END-EXEC.

           EXEC SQL
                OPEN BCUR_CLN2
           END-EXEC.


           EVALUATE SQLCODE

               WHEN ZERO
                    SET  WCLN2-IO-OK        TO  TRUE

               WHEN OTHER
                    SET  WCLN2-IO-ERROR     TO  TRUE

           END-EVALUATE.


       210G-BROWSE-X.
           EXIT.
      **********************
       3000-EXEC-FETCH-NEXT.
      **********************

           EXEC SQL
             FETCH BCUR_CLN2
             INTO
              :RCLN2-CO-ID,
              :RCLN2-POL-ID
             END-EXEC.


           EVALUATE SQLCODE

               WHEN ZERO
                    SET  WCLN2-IO-OK        TO  TRUE
                   

               WHEN +100
                    SET  WCLN2-IO-EOF       TO  TRUE

               WHEN OTHER
                    SET  WCLN2-IO-ERROR     TO  TRUE

           END-EVALUATE.


       3000-EXEC-FETCH-NEXT-X.
           EXIT.


      *********************
       4000-EXEC-CLOSE-CUR.
      *********************

           EVALUATE WS-OPTM-SQL-EXEC

               WHEN '05'
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
                CLOSE BCUR_CLN2
           END-EXEC.


           EVALUATE SQLCODE

               WHEN ZERO
                    SET  WCLN2-IO-OK        TO  TRUE

               WHEN OTHER
                    SET  WCLN2-IO-ERROR     TO  TRUE

           END-EVALUATE.


       410G-CLOSE-BROWSE-CUR-X.
           EXIT.


      *****************************************************************
      **                 END OF PROGRAM ASIBCLN2                     **
      *****************************************************************
