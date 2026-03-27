      *************************
       IDENTIFICATION DIVISION.
      *************************

       PROGRAM-ID. ASIBUFND.

       COPY XCWWCRHT.

      *****************************************************************
      **  MEMBER :  ASIBUFND                                         **
      **  REMARKS:  SQL I/O PROGRAM USED TO BROWSE ROWS IN THE       **
      **            APPLICATION UPLOAD FUND TABLE                    **
      *****************************************************************
      **  RELEASE   DESCRIPTION                                      **
      **                                                             **
      **  01OCT05   CREATED FOR UFND PROCESSING                      **
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

       COPY XCWWPGWS REPLACING '$VAR1' BY 'ASIBUFND'.

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
           EXEC SQL INCLUDE ACWZUFND  END-EXEC.
      /
      *****************
       LINKAGE SECTION.
      *****************

           EXEC SQL INCLUDE SQLCA     END-EXEC.

           EXEC SQL INCLUDE ACFWUFND  END-EXEC.

           EXEC SQL INCLUDE ACFRUFND  END-EXEC.

      /
       PROCEDURE DIVISION USING SQLCA
                                WUFND-IO-WORK-AREA
                                RUFND-REC-INFO.

      ***************
       0000-MAINLINE.
      ***************

           IF  WS-WA-ADDRESS NOT = ADDRESS OF WUFND-IO-WORK-AREA
           OR  WS-TL-ADDRESS NOT = ADDRESS OF RUFND-REC-INFO
               SET  WS-WA-ADDRESS  TO  ADDRESS OF WUFND-IO-WORK-AREA
               SET  WS-TL-ADDRESS  TO  ADDRESS OF RUFND-REC-INFO
               MOVE ZERO           TO  SQL-INIT-FLAG
           END-IF.


           EVALUATE TRUE

               WHEN WUFND-RQST-BROWSE
                    PERFORM  1000-DETERMINE-SQL-REQIR
                        THRU 1000-DETERMINE-SQL-REQIR-X
                    PERFORM  2000-EXEC-BROWSE
                        THRU 2000-EXEC-BROWSE-X

               WHEN WUFND-RQST-FETCH-NEXT
                    PERFORM  3000-EXEC-FETCH-NEXT
                        THRU 3000-EXEC-FETCH-NEXT-X

               WHEN WUFND-RQST-CLOSE-BROWSE-CUR
                    PERFORM  4000-EXEC-CLOSE-CUR
                        THRU 4000-EXEC-CLOSE-CUR-X

               WHEN WUFND-RQST-BROWSE-INDEX
                    PERFORM  1000-DETERMINE-SQL-REQIR
                        THRU 1000-DETERMINE-SQL-REQIR-X
                    PERFORM  5000-EXEC-BROWSE-INDEX
                        THRU 5000-EXEC-BROWSE-INDEX-X

               WHEN WUFND-RQST-FETCH-NEXT-INDEX
                    PERFORM  6000-EXEC-FETCH-NEXT-INDEX
                        THRU 6000-EXEC-FETCH-NEXT-INDEX-X

               WHEN WUFND-RQST-CLOSE-BROWSE-INDEX
                    PERFORM  7000-EXEC-CLOSE-INDEX
                        THRU 7000-EXEC-CLOSE-INDEX-X

               WHEN OTHER
                    SET  WUFND-IO-ERROR      TO  TRUE

           END-EVALUATE.


           GOBACK.


       0000-MAINLINE-X.
           EXIT.


      **************************
       1000-DETERMINE-SQL-REQIR.
      **************************

           MOVE 4                            TO  WS-OPTM-SQL-REQIR.

           IF  WUFND-APP-ID = WUFND-ENDBR-APP-ID
               SUBTRACT +1                   FROM WS-OPTM-SQL-REQIR
           ELSE
               GO TO 1000-DETERMINE-SQL-REQIR-X
           END-IF.


           IF  WUFND-STCKR-ID = WUFND-ENDBR-STCKR-ID
               SUBTRACT +1                   FROM WS-OPTM-SQL-REQIR
           ELSE
               GO TO 1000-DETERMINE-SQL-REQIR-X
           END-IF.


           IF  WUFND-PLAN-ID = WUFND-ENDBR-PLAN-ID
               SUBTRACT +1                   FROM WS-OPTM-SQL-REQIR
           ELSE
               GO TO 1000-DETERMINE-SQL-REQIR-X
           END-IF.


           IF  WUFND-FND-ID = WUFND-ENDBR-FND-ID
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

               WHEN WS-OPTM-SQL-REQIR <= 4
                    MOVE '04'                TO  WS-OPTM-SQL-EXEC
                    PERFORM  210G-BROWSE
                        THRU 210G-BROWSE-X

               WHEN OTHER
                    MOVE SPACES              TO  WS-OPTM-SQL-EXEC
                    SET  WUFND-IO-ERROR      TO  TRUE

           END-EVALUATE.


           MOVE WS-OPTM-SQL-REQIR            TO  WUFND-OPTM-SQL-REQIR.
           MOVE WS-OPTM-SQL-EXEC             TO  WUFND-OPTM-SQL-EXEC.

           EVALUATE TRUE

               WHEN WUFND-OPTM-SQL-EXEC = WUFND-OPTM-SQL-REQIR
                   SET WUFND-OPTM-SQL-OK     TO  TRUE

               WHEN WUFND-OPTM-SQL-EXEC = SPACES
                   SET WUFND-OPTM-SQL-ERROR  TO  TRUE

               WHEN OTHER
                   SET WUFND-OPTM-SQL-IMPRV  TO  TRUE

           END-EVALUATE.


       2000-EXEC-BROWSE-X.
           EXIT.


      *************
       2102-BROWSE.
      *************

           EXEC SQL
             DECLARE B2CUR_UFND CURSOR FOR
             SELECT
                 APP_ID,
                 STCKR_ID,
                 PLAN_ID,
                 FND_ID,
                 SA_INIT_PREM_PCT,
                 SA_SUBSEQ_PREM_PCT,
                 SA_INIT_LMPSM_PCT,
                 SA_CNVR_FND_PCT
             FROM TUFND
             WHERE
                 APP_ID    = :WUFND-APP-ID           AND
                 STCKR_ID  = :WUFND-STCKR-ID
               AND
                 PLAN_ID   BETWEEN
                             :WUFND-PLAN-ID          AND
                             :WUFND-ENDBR-PLAN-ID
               AND
                (FND_ID   >= :WUFND-FND-ID           OR
                 PLAN_ID   > :WUFND-PLAN-ID)
               AND
                (FND_ID   <= :WUFND-ENDBR-FND-ID     OR
                 PLAN_ID   < :WUFND-ENDBR-PLAN-ID)
             ORDER BY
                 APP_ID,
                 STCKR_ID,
                 PLAN_ID,
                 FND_ID
             FOR FETCH ONLY
             OPTIMIZE FOR 1 ROW
           END-EXEC.

           EXEC SQL
                OPEN B2CUR_UFND
           END-EXEC.


           EVALUATE SQLCODE

               WHEN ZERO
                    SET  WUFND-IO-OK         TO  TRUE

               WHEN OTHER
                    SET  WUFND-IO-ERROR      TO  TRUE

           END-EVALUATE.


       2102-BROWSE-X.
           EXIT.


      *************
       210G-BROWSE.
      *************

           EXEC SQL
             DECLARE BCUR_UFND CURSOR FOR
             SELECT
                 APP_ID,
                 STCKR_ID,
                 PLAN_ID,
                 FND_ID,
                 SA_INIT_PREM_PCT,
                 SA_SUBSEQ_PREM_PCT,
                 SA_INIT_LMPSM_PCT,
                 SA_CNVR_FND_PCT
             FROM TUFND
             WHERE
                 APP_ID    BETWEEN
                             :WUFND-APP-ID           AND
                             :WUFND-ENDBR-APP-ID
               AND
               ((STCKR_ID  = :WUFND-STCKR-ID         AND
               ((PLAN_ID   = :WUFND-PLAN-ID          AND
                 FND_ID   >= :WUFND-FND-ID)          OR
                 PLAN_ID   > :WUFND-PLAN-ID))        OR
                 STCKR_ID  > :WUFND-STCKR-ID         OR
                 APP_ID    > :WUFND-APP-ID)
               AND
               ((STCKR_ID  = :WUFND-ENDBR-STCKR-ID   AND
               ((PLAN_ID   = :WUFND-ENDBR-PLAN-ID    AND
                 FND_ID   <= :WUFND-ENDBR-FND-ID)    OR
                 PLAN_ID   < :WUFND-ENDBR-PLAN-ID))  OR
                 STCKR_ID  < :WUFND-ENDBR-STCKR-ID   OR
                 APP_ID    < :WUFND-ENDBR-APP-ID)
             ORDER BY
                 APP_ID,
                 STCKR_ID,
                 PLAN_ID,
                 FND_ID
             FOR FETCH ONLY
             OPTIMIZE FOR 1 ROW
           END-EXEC.

           EXEC SQL
                OPEN BCUR_UFND
           END-EXEC.


           EVALUATE SQLCODE

               WHEN ZERO
                    SET  WUFND-IO-OK         TO  TRUE

               WHEN OTHER
                    SET  WUFND-IO-ERROR      TO  TRUE

           END-EVALUATE.


       210G-BROWSE-X.
           EXIT.


      **********************
       3000-EXEC-FETCH-NEXT.
      **********************

           EVALUATE WS-OPTM-SQL-EXEC

               WHEN '02'
                    PERFORM  3102-FETCH-NEXT
                        THRU 3102-FETCH-NEXT-X

               WHEN '04'
                    PERFORM  310G-FETCH-NEXT
                        THRU 310G-FETCH-NEXT-X

               WHEN OTHER
                    PERFORM  310G-FETCH-NEXT
                        THRU 310G-FETCH-NEXT-X

           END-EVALUATE.


           SET  WUFND-OPTM-SQL-OK            TO  TRUE.
           MOVE SPACES                       TO  WUFND-OPTM-SQL-REQIR.
           MOVE SPACES                       TO  WUFND-OPTM-SQL-EXEC.


       3000-EXEC-FETCH-NEXT-X.
           EXIT.


      *****************
       3102-FETCH-NEXT.
      *****************

           MOVE LOW-VALUES                   TO  ZUFND-NULL-INDICATORS.

           EXEC SQL
             FETCH B2CUR_UFND
             INTO
                :RUFND-APP-ID,
                :RUFND-STCKR-ID,
                :RUFND-PLAN-ID,
                :RUFND-FND-ID,
                :RUFND-SA-INIT-PREM-PCT   :ZUFND-SA-INIT-PREM-PCT-NI,
                :RUFND-SA-SUBSEQ-PREM-PCT :ZUFND-SA-SUBSEQ-PREM-PCT-NI,
                :RUFND-SA-INIT-LMPSM-PCT  :ZUFND-SA-INIT-LMPSM-PCT-NI,
                :RUFND-SA-CNVR-FND-PCT    :ZUFND-SA-CNVR-FND-PCT-NI
           END-EXEC.


           EVALUATE SQLCODE

               WHEN ZERO
                    SET  WUFND-IO-OK         TO  TRUE
                    PERFORM  UFND-2000-SET-NULL-DFLT
                        THRU UFND-2000-SET-NULL-DFLT-X
                    MOVE RUFND-KEY           TO  WUFND-KEY

               WHEN +100
                    SET  WUFND-IO-EOF        TO  TRUE

               WHEN OTHER
                    SET  WUFND-IO-ERROR      TO  TRUE

           END-EVALUATE.


       3102-FETCH-NEXT-X.
           EXIT.


      *****************
       310G-FETCH-NEXT.
      *****************

           MOVE LOW-VALUES                   TO  ZUFND-NULL-INDICATORS.

           EXEC SQL
             FETCH BCUR_UFND
             INTO
                :RUFND-APP-ID,
                :RUFND-STCKR-ID,
                :RUFND-PLAN-ID,
                :RUFND-FND-ID,
                :RUFND-SA-INIT-PREM-PCT   :ZUFND-SA-INIT-PREM-PCT-NI,
                :RUFND-SA-SUBSEQ-PREM-PCT :ZUFND-SA-SUBSEQ-PREM-PCT-NI,
                :RUFND-SA-INIT-LMPSM-PCT  :ZUFND-SA-INIT-LMPSM-PCT-NI,
                :RUFND-SA-CNVR-FND-PCT    :ZUFND-SA-CNVR-FND-PCT-NI
           END-EXEC.


           EVALUATE SQLCODE

               WHEN ZERO
                    SET  WUFND-IO-OK         TO  TRUE
                    PERFORM  UFND-2000-SET-NULL-DFLT
                        THRU UFND-2000-SET-NULL-DFLT-X
                    MOVE RUFND-KEY           TO  WUFND-KEY

               WHEN +100
                    SET  WUFND-IO-EOF        TO  TRUE

               WHEN OTHER
                    SET  WUFND-IO-ERROR      TO  TRUE

           END-EVALUATE.


       310G-FETCH-NEXT-X.
           EXIT.


      *********************
       4000-EXEC-CLOSE-CUR.
      *********************

           EVALUATE WS-OPTM-SQL-EXEC

               WHEN '02'
                    PERFORM  4102-CLOSE-BROWSE-CUR
                        THRU 4102-CLOSE-BROWSE-CUR-X

               WHEN '04'
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
       4102-CLOSE-BROWSE-CUR.
      ***********************

           EXEC SQL
                CLOSE B2CUR_UFND
           END-EXEC.


           EVALUATE SQLCODE

               WHEN ZERO
                    SET  WUFND-IO-OK         TO  TRUE

               WHEN OTHER
                    SET  WUFND-IO-ERROR      TO  TRUE

           END-EVALUATE.


       4102-CLOSE-BROWSE-CUR-X.
           EXIT.


      ***********************
       410G-CLOSE-BROWSE-CUR.
      ***********************

           EXEC SQL
                CLOSE BCUR_UFND
           END-EXEC.


           EVALUATE SQLCODE

               WHEN ZERO
                    SET  WUFND-IO-OK         TO  TRUE

               WHEN OTHER
                    SET  WUFND-IO-ERROR      TO  TRUE

           END-EVALUATE.


       410G-CLOSE-BROWSE-CUR-X.
           EXIT.


      ************************
       5000-EXEC-BROWSE-INDEX.
      ************************

           IF  WS-OPTM-SQL-EXEC NUMERIC
               MOVE WS-OPTM-SQL-EXEC-N       TO  WS-OPTM-SQL-REQIR
           END-IF.


      * PROFILE INDICATED THAT -BROWSE-INDEX
      * PARAGRAPHS ARE NOT APPLICABLE

           SET  WUFND-IO-ERROR               TO  TRUE.
           MOVE WS-OPTM-SQL-REQIR            TO  WUFND-OPTM-SQL-REQIR.
           MOVE WS-OPTM-SQL-EXEC             TO  WUFND-OPTM-SQL-EXEC.

           EVALUATE TRUE

               WHEN WUFND-OPTM-SQL-EXEC = WUFND-OPTM-SQL-REQIR
                   SET WUFND-OPTM-SQL-OK     TO  TRUE

               WHEN WUFND-OPTM-SQL-EXEC = SPACES
                   SET WUFND-OPTM-SQL-ERROR  TO  TRUE

               WHEN OTHER
                   SET WUFND-OPTM-SQL-IMPRV  TO  TRUE

           END-EVALUATE.


       5000-EXEC-BROWSE-INDEX-X.
           EXIT.


      ****************************
       6000-EXEC-FETCH-NEXT-INDEX.
      ****************************

      * PROFILE INDICATED THAT -FETCH-NEXT-INDEX
      * PARAGRAPHS ARE NOT APPLICABLE

           SET  WUFND-IO-ERROR               TO  TRUE.
           SET  WUFND-OPTM-SQL-OK            TO  TRUE.
           MOVE SPACES                       TO  WUFND-OPTM-SQL-REQIR.
           MOVE SPACES                       TO  WUFND-OPTM-SQL-EXEC.


       6000-EXEC-FETCH-NEXT-INDEX-X.
           EXIT.


      ***********************
       7000-EXEC-CLOSE-INDEX.
      ***********************

      * PROFILE INDICATED THAT -CLOSE-BROWSE-INDEX
      * PARAGRAPHS ARE NOT APPLICABLE

           SET  WUFND-IO-ERROR               TO  TRUE.
           SET  WS-OPTM-SQL-CUR-CLOSED       TO  TRUE.


       7000-EXEC-CLOSE-INDEX-X.
           EXIT.


       COPY ACPZUFND.

      *****************************************************************
      **                 END OF PROGRAM ASIBUFND                     **
      *****************************************************************
