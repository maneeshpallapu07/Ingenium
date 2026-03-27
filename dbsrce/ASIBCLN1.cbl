      *************************
       IDENTIFICATION DIVISION.
      *************************

       PROGRAM-ID. ASIBCLN1.

       COPY XCWWCRHT.

      *****************************************************************
      **  MEMBER :  ASIBCLN1                                         **
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

      ***************
       DATA DIVISION.
      ***************
      /
      *************************
       WORKING-STORAGE SECTION.
      *************************

       COPY XCWWPGWS REPLACING '$VAR1' BY 'ASIBCLN1'.

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

           EXEC SQL INCLUDE ACFWCLN1  END-EXEC.

           EXEC SQL INCLUDE ACFRCLN1   END-EXEC.

      /
       PROCEDURE DIVISION USING SQLCA
                                WCLN1-IO-WORK-AREA
                                RCLN1-REC-INFO.

      ***************
       0000-MAINLINE.
      ***************

           IF  WS-WA-ADDRESS NOT = ADDRESS OF WCLN1-IO-WORK-AREA
           OR  WS-TL-ADDRESS NOT = ADDRESS OF RCLN1-REC-INFO
               SET  WS-WA-ADDRESS  TO  ADDRESS OF WCLN1-IO-WORK-AREA
               SET  WS-TL-ADDRESS  TO  ADDRESS OF RCLN1-REC-INFO
               MOVE ZERO           TO  SQL-INIT-FLAG
           END-IF.


           EVALUATE TRUE

               WHEN WCLN1-RQST-BROWSE
                    PERFORM  1000-DETERMINE-SQL-REQIR
                        THRU 1000-DETERMINE-SQL-REQIR-X
                    PERFORM  2000-EXEC-BROWSE
                        THRU 2000-EXEC-BROWSE-X

               WHEN WCLN1-RQST-FETCH-NEXT
                    PERFORM  3000-EXEC-FETCH-NEXT
                        THRU 3000-EXEC-FETCH-NEXT-X

               WHEN WCLN1-RQST-CLOSE-BROWSE-CUR
                    PERFORM  4000-EXEC-CLOSE-CUR
                        THRU 4000-EXEC-CLOSE-CUR-X

               WHEN OTHER
                    SET  WCLN1-IO-ERROR     TO  TRUE

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
                    SET  WCLN1-IO-ERROR      TO  TRUE

           END-EVALUATE.


           MOVE WS-OPTM-SQL-REQIR            TO  WCLN1-OPTM-SQL-REQIR.
           MOVE WS-OPTM-SQL-EXEC             TO  WCLN1-OPTM-SQL-EXEC.

           IF  WCLN1-OPTM-SQL-EXEC = WCLN1-OPTM-SQL-REQIR
               SET WCLN1-OPTM-SQL-OK         TO  TRUE
           ELSE
               IF  WCLN1-OPTM-SQL-EXEC = SPACES
                   SET WCLN1-OPTM-SQL-ERROR  TO  TRUE
               ELSE
                   SET WCLN1-OPTM-SQL-IMPRV  TO  TRUE
               END-IF
           END-IF.


       2000-EXEC-BROWSE-X.
           EXIT.


      *************
       210G-BROWSE.
      *************

           EXEC SQL
             DECLARE BCUR_POL CURSOR FOR
           SELECT
                 TPOL.CO_ID,
                 TPOL.POL_ID          
                 FROM   TPOL, TPOLC, TMAST
                 WHERE
                 TPOL.CO_ID = :WCLN1-CO-ID
                 AND TPOL.POL_APP_UPLD_DT = TMAST.APPL_CTL_PRCES_DT
                 AND TPOL.POL_BULK_APP_ID = ' '
                 AND TPOL.POL_BUNDLE_APP_ID = ' '
                 AND TPOL.COLI_PROD_IND ='Y'
                 AND TPOLC.CO_ID = TPOL.CO_ID
                 AND TPOLC.POL_ID = TPOL.POL_ID
                 AND TPOLC.POL_CLI_REL_TYP_CD = 'O'
                 AND TPOLC.CLI_ID IN
                (SELECT  TCLNM.CLI_ID 
                 FROM TCLNM
                 WHERE ((TCLNM.CO_ID = 'CP' 
                 AND TCLNM.ENTR_SUR_NM = :WCLN1-CLI-KJ-SUR-NM
                 AND TCLNM.ENTR_GIV_NM = :WCLN1-CLI-KJ-GIV-NM
                 AND TCLNM.CLI_INDV_GR_CD = 'KJ' 
                 AND TCLNM.CLI_INDV_NM_TYP_CD = 'CR' 
                 AND TCLNM.CLI_INDV_SEQ_NUM = '001')
                 OR (TCLNM.CO_ID = 'CP' 
                 AND TCLNM.ENTR_SUR_NM = :WCLN1-CLI-SUR-NM 
                 AND TCLNM.ENTR_GIV_NM = :WCLN1-CLI-GIV-NM 
                 AND TCLNM.CLI_INDV_GR_CD = 'KA' 
                 AND TCLNM.CLI_INDV_NM_TYP_CD = 'CR' 
                 AND TCLNM.CLI_INDV_SEQ_NUM = '001')))
           
                ORDER BY
                 POL_ID
                   
             FOR FETCH ONLY
             OPTIMIZE FOR 1 ROW
           END-EXEC.

           EXEC SQL
                OPEN BCUR_POL
           END-EXEC.


           EVALUATE SQLCODE

               WHEN ZERO
                    SET  WCLN1-IO-OK        TO  TRUE

               WHEN OTHER
                    SET  WCLN1-IO-ERROR     TO  TRUE

           END-EVALUATE.


       210G-BROWSE-X.
           EXIT.
      /
      **********************
       3000-EXEC-FETCH-NEXT.
      **********************

           EXEC SQL
             FETCH BCUR_POL
             INTO
              :RCLN1-CO-ID,
              :RCLN1-POL-ID
             END-EXEC.


           EVALUATE SQLCODE

               WHEN ZERO
                    SET  WCLN1-IO-OK        TO  TRUE
                   

               WHEN +100
                    SET  WCLN1-IO-EOF       TO  TRUE

               WHEN OTHER
                    SET  WCLN1-IO-ERROR     TO  TRUE

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
                CLOSE BCUR_POL
           END-EXEC.


           EVALUATE SQLCODE

               WHEN ZERO
                    SET  WCLN1-IO-OK        TO  TRUE

               WHEN OTHER
                    SET  WCLN1-IO-ERROR     TO  TRUE

           END-EVALUATE.


       410G-CLOSE-BROWSE-CUR-X.
           EXIT.


      *****************************************************************
      **                 END OF PROGRAM ASIBCLN1                     **
      *****************************************************************
