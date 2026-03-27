      *************************
       IDENTIFICATION DIVISION.
      *************************
 
       PROGRAM-ID. ASIBUTTB.
 
       COPY XCWWCRHT.
 
      *****************************************************************
      **  MEMBER : ASIBUTTB                                          **
      **  REMARKS: SQL I/O PROGRAM USED TO BROWSE ROWS IN THE        **
      **           UPLOAD TRANSLATION TABLE                          **
      *****************************************************************
      **  DATE     AUTH.  DESCRIPTION                                **
      **                                                             **
      **  30OCT98  56     CREATED FOR UTTB TABLE PROCESSING          **
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
 
       COPY XCWWPGWS REPLACING '$VAR1' BY 'ASIBUTTB'.
 
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
           EXEC SQL INCLUDE ACWZUTTB  END-EXEC.
      /
      *****************
       LINKAGE SECTION.
      *****************
 
           EXEC SQL INCLUDE SQLCA     END-EXEC.
 
           EXEC SQL INCLUDE ACFWUTTB  END-EXEC.
 
           EXEC SQL INCLUDE ACFRUTTB  END-EXEC.
 
      /
       PROCEDURE DIVISION USING SQLCA
                                WUTTB-IO-WORK-AREA
                                RUTTB-REC-INFO.
 
      ***************
       0000-MAINLINE.
      ***************
 
           IF  WS-WA-ADDRESS NOT = ADDRESS OF WUTTB-IO-WORK-AREA
           OR  WS-TL-ADDRESS NOT = ADDRESS OF RUTTB-REC-INFO
               SET  WS-WA-ADDRESS  TO  ADDRESS OF WUTTB-IO-WORK-AREA
               SET  WS-TL-ADDRESS  TO  ADDRESS OF RUTTB-REC-INFO
DB2MVS         MOVE ZERO           TO  SQL-INIT-FLAG
           END-IF.
 
 
           EVALUATE TRUE
 
               WHEN WUTTB-RQST-BROWSE
                    PERFORM  1000-DETERMINE-SQL-REQIR
                        THRU 1000-DETERMINE-SQL-REQIR-X
                    PERFORM  2000-EXEC-BROWSE
                        THRU 2000-EXEC-BROWSE-X
 
               WHEN WUTTB-RQST-FETCH-NEXT
                    PERFORM  3000-EXEC-FETCH-NEXT
                        THRU 3000-EXEC-FETCH-NEXT-X
 
               WHEN WUTTB-RQST-CLOSE-BROWSE-CUR
                    PERFORM  4000-EXEC-CLOSE-CUR
                        THRU 4000-EXEC-CLOSE-CUR-X
 
               WHEN WUTTB-RQST-BROWSE-INDEX
                    PERFORM  1000-DETERMINE-SQL-REQIR
                        THRU 1000-DETERMINE-SQL-REQIR-X
                    PERFORM  5000-EXEC-BROWSE-INDEX
                        THRU 5000-EXEC-BROWSE-INDEX-X
 
               WHEN WUTTB-RQST-FETCH-NEXT-INDEX
                    PERFORM  6000-EXEC-FETCH-NEXT-INDEX
                        THRU 6000-EXEC-FETCH-NEXT-INDEX-X
 
               WHEN WUTTB-RQST-CLOSE-BROWSE-INDEX
                    PERFORM  7000-EXEC-CLOSE-INDEX
                        THRU 7000-EXEC-CLOSE-INDEX-X
 
               WHEN OTHER
                    SET  WUTTB-IO-ERROR      TO  TRUE
 
           END-EVALUATE.
 
 
           GOBACK.
 
 
       0000-MAINLINE-X.
           EXIT.
 
 
      **************************
       1000-DETERMINE-SQL-REQIR.
      **************************
 
           MOVE 3                            TO  WS-OPTM-SQL-REQIR.
 
           IF  WUTTB-CO-ID = WUTTB-ENDBR-CO-ID
               SUBTRACT +1                   FROM WS-OPTM-SQL-REQIR
           ELSE
               GO TO 1000-DETERMINE-SQL-REQIR-X
           END-IF.
 
 
           IF  WUTTB-UPLD-TTBL-TYP-ID = WUTTB-ENDBR-UPLD-TTBL-TYP-ID
               SUBTRACT +1                   FROM WS-OPTM-SQL-REQIR
           ELSE
               GO TO 1000-DETERMINE-SQL-REQIR-X
           END-IF.
 
 
           IF  WUTTB-UPLD-TTBL-VALU-ID = WUTTB-ENDBR-UPLD-TTBL-VALU-ID
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
 
               WHEN WS-OPTM-SQL-REQIR <= 1
                    MOVE '01'                TO  WS-OPTM-SQL-EXEC
                    PERFORM  2101-BROWSE
                        THRU 2101-BROWSE-X
 
               WHEN OTHER
                    MOVE SPACES              TO  WS-OPTM-SQL-EXEC
                    SET  WUTTB-IO-ERROR      TO  TRUE
 
           END-EVALUATE.
 
 
           MOVE WS-OPTM-SQL-REQIR            TO  WUTTB-OPTM-SQL-REQIR.
           MOVE WS-OPTM-SQL-EXEC             TO  WUTTB-OPTM-SQL-EXEC.
 
           EVALUATE TRUE
 
               WHEN WUTTB-OPTM-SQL-EXEC = WUTTB-OPTM-SQL-REQIR
                   SET WUTTB-OPTM-SQL-OK     TO  TRUE
 
               WHEN WUTTB-OPTM-SQL-EXEC = SPACES
                   SET WUTTB-OPTM-SQL-ERROR  TO  TRUE
 
               WHEN OTHER
                   SET WUTTB-OPTM-SQL-IMPRV  TO  TRUE
 
           END-EVALUATE.
 
 
       2000-EXEC-BROWSE-X.
           EXIT.
 
 
      *************
       2101-BROWSE.
      *************
 
           EXEC SQL
             DECLARE B1CUR_UTTB CURSOR FOR
             SELECT
                 CO_ID,
                 UPLD_TTBL_TYP_ID,
                 UPLD_TTBL_VALU_ID,
                 PREV_UPDT_USER_ID,
                 PREV_UPDT_DT,
                 UPLD_TTBL_VALU_TXT
             FROM TUTTB
             WHERE
                 CO_ID              = :WUTTB-CO-ID                   AND
                 UPLD_TTBL_TYP_ID   = :WUTTB-UPLD-TTBL-TYP-ID
               AND
                 UPLD_TTBL_VALU_ID  BETWEEN
                                      :WUTTB-UPLD-TTBL-VALU-ID       AND
                                      :WUTTB-ENDBR-UPLD-TTBL-VALU-ID
             ORDER BY
                 CO_ID,
                 UPLD_TTBL_TYP_ID,
                 UPLD_TTBL_VALU_ID
             FOR FETCH ONLY
             OPTIMIZE FOR 1 ROW
           END-EXEC.
 
           EXEC SQL
                OPEN B1CUR_UTTB
           END-EXEC.
 
 
           EVALUATE SQLCODE
 
               WHEN ZERO
                    SET  WUTTB-IO-OK         TO  TRUE
 
               WHEN OTHER
                    SET  WUTTB-IO-ERROR      TO  TRUE
 
           END-EVALUATE.
 
 
       2101-BROWSE-X.
           EXIT.
 
 
      **********************
       3000-EXEC-FETCH-NEXT.
      **********************
 
           EVALUATE WS-OPTM-SQL-EXEC
 
               WHEN '01'
                    PERFORM  3101-FETCH-NEXT
                        THRU 3101-FETCH-NEXT-X
 
               WHEN OTHER
                    PERFORM  3101-FETCH-NEXT
                        THRU 3101-FETCH-NEXT-X
 
           END-EVALUATE.
 
 
           SET  WUTTB-OPTM-SQL-OK            TO  TRUE.
           MOVE SPACES                       TO  WUTTB-OPTM-SQL-REQIR.
           MOVE SPACES                       TO  WUTTB-OPTM-SQL-EXEC.
 
 
       3000-EXEC-FETCH-NEXT-X.
           EXIT.
 
 
      *****************
       3101-FETCH-NEXT.
      *****************
 
           MOVE LOW-VALUES                   TO  ZUTTB-NULL-INDICATORS.
 
           EXEC SQL
             FETCH B1CUR_UTTB
             INTO
                :RUTTB-CO-ID,
                :RUTTB-UPLD-TTBL-TYP-ID,
                :RUTTB-UPLD-TTBL-VALU-ID,
                :RUTTB-PREV-UPDT-USER-ID,
                :RUTTB-PREV-UPDT-DT       :ZUTTB-PREV-UPDT-DT-NI,
                :RUTTB-UPLD-TTBL-VALU-TXT
           END-EXEC.
 
 
           EVALUATE SQLCODE
 
               WHEN ZERO
                    SET  WUTTB-IO-OK         TO  TRUE
                    PERFORM  UTTB-2000-SET-NULL-DFLT
                        THRU UTTB-2000-SET-NULL-DFLT-X
                    MOVE RUTTB-KEY           TO  WUTTB-KEY
 
               WHEN +100
                    SET  WUTTB-IO-EOF        TO  TRUE
 
               WHEN OTHER
                    SET  WUTTB-IO-ERROR      TO  TRUE
 
           END-EVALUATE.
 
 
       3101-FETCH-NEXT-X.
           EXIT.
 
 
      *********************
       4000-EXEC-CLOSE-CUR.
      *********************
 
           EVALUATE WS-OPTM-SQL-EXEC
 
               WHEN '01'
                    PERFORM  4101-CLOSE-BROWSE-CUR
                        THRU 4101-CLOSE-BROWSE-CUR-X
 
               WHEN OTHER
                    PERFORM  4101-CLOSE-BROWSE-CUR
                        THRU 4101-CLOSE-BROWSE-CUR-X
 
           END-EVALUATE.
 
 
           SET  WS-OPTM-SQL-CUR-CLOSED       TO  TRUE.
 
 
       4000-EXEC-CLOSE-CUR-X.
           EXIT.
 
 
      ***********************
       4101-CLOSE-BROWSE-CUR.
      ***********************
 
           EXEC SQL
                CLOSE B1CUR_UTTB
           END-EXEC.
 
 
           EVALUATE SQLCODE
 
               WHEN ZERO
                    SET  WUTTB-IO-OK         TO  TRUE
 
               WHEN OTHER
                    SET  WUTTB-IO-ERROR      TO  TRUE
 
           END-EVALUATE.
 
 
       4101-CLOSE-BROWSE-CUR-X.
           EXIT.
 
 
      ************************
       5000-EXEC-BROWSE-INDEX.
      ************************
 
           IF  WS-OPTM-SQL-EXEC NUMERIC
               MOVE WS-OPTM-SQL-EXEC-N       TO  WS-OPTM-SQL-REQIR
           END-IF.
 
 
      * PROFILE INDICATED THAT -BROWSE-INDEX
      * PARAGRAPHS ARE NOT APPLICABLE
 
           SET  WUTTB-IO-ERROR               TO  TRUE.
           MOVE WS-OPTM-SQL-REQIR            TO  WUTTB-OPTM-SQL-REQIR.
           MOVE WS-OPTM-SQL-EXEC             TO  WUTTB-OPTM-SQL-EXEC.
 
           EVALUATE TRUE
 
               WHEN WUTTB-OPTM-SQL-EXEC = WUTTB-OPTM-SQL-REQIR
                   SET WUTTB-OPTM-SQL-OK     TO  TRUE
 
               WHEN WUTTB-OPTM-SQL-EXEC = SPACES
                   SET WUTTB-OPTM-SQL-ERROR  TO  TRUE
 
               WHEN OTHER
                   SET WUTTB-OPTM-SQL-IMPRV  TO  TRUE
 
           END-EVALUATE.
 
 
       5000-EXEC-BROWSE-INDEX-X.
           EXIT.
 
 
      ****************************
       6000-EXEC-FETCH-NEXT-INDEX.
      ****************************
 
      * PROFILE INDICATED THAT -FETCH-NEXT-INDEX
      * PARAGRAPHS ARE NOT APPLICABLE
 
           SET  WUTTB-IO-ERROR               TO  TRUE.
           SET  WUTTB-OPTM-SQL-OK            TO  TRUE.
           MOVE SPACES                       TO  WUTTB-OPTM-SQL-REQIR.
           MOVE SPACES                       TO  WUTTB-OPTM-SQL-EXEC.
 
 
       6000-EXEC-FETCH-NEXT-INDEX-X.
           EXIT.
 
 
      ***********************
       7000-EXEC-CLOSE-INDEX.
      ***********************
 
      * PROFILE INDICATED THAT -CLOSE-BROWSE-INDEX
      * PARAGRAPHS ARE NOT APPLICABLE
 
           SET  WUTTB-IO-ERROR               TO  TRUE.
           SET  WS-OPTM-SQL-CUR-CLOSED       TO  TRUE.
 
 
       7000-EXEC-CLOSE-INDEX-X.
           EXIT.
 
 
       COPY ACPZUTTB.
 
      *****************************************************************
      **                 END OF PROGRAM ASIBUTTB                     **
      *****************************************************************
