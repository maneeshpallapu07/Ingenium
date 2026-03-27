      *************************
       IDENTIFICATION DIVISION.
      *************************
 
       PROGRAM-ID. ASIBUFLD.
 
       COPY XCWWCRHT.
 
      *****************************************************************
      **  MEMBER : ASIBUFLD                                          **
      **  REMARKS: SQL I/O PROGRAM USED TO BROWSE ROWS IN THE        **
      **           UPLOAD DEFINED FIELD TABLE                        **
      *****************************************************************
      **  DATE     AUTH.  DESCRIPTION                                **
      **                                                             **
      **  30OCT98  56     CREATED FOR UFLD TABLE PROCESSING          **
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
 
       COPY XCWWPGWS REPLACING '$VAR1' BY 'ASIBUFLD'.
 
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
           EXEC SQL INCLUDE ACWZUFLD  END-EXEC.
      /
      *****************
       LINKAGE SECTION.
      *****************
 
           EXEC SQL INCLUDE SQLCA     END-EXEC.
 
           EXEC SQL INCLUDE ACFWUFLD  END-EXEC.
 
           EXEC SQL INCLUDE ACFRUFLD  END-EXEC.
 
      /
       PROCEDURE DIVISION USING SQLCA
                                WUFLD-IO-WORK-AREA
                                RUFLD-REC-INFO.
 
      ***************
       0000-MAINLINE.
      ***************
 
           IF  WS-WA-ADDRESS NOT = ADDRESS OF WUFLD-IO-WORK-AREA
           OR  WS-TL-ADDRESS NOT = ADDRESS OF RUFLD-REC-INFO
               SET  WS-WA-ADDRESS  TO  ADDRESS OF WUFLD-IO-WORK-AREA
               SET  WS-TL-ADDRESS  TO  ADDRESS OF RUFLD-REC-INFO
DB2MVS         MOVE ZERO           TO  SQL-INIT-FLAG
           END-IF.
 
 
           EVALUATE TRUE
 
               WHEN WUFLD-RQST-BROWSE
                    PERFORM  1000-DETERMINE-SQL-REQIR
                        THRU 1000-DETERMINE-SQL-REQIR-X
                    PERFORM  2000-EXEC-BROWSE
                        THRU 2000-EXEC-BROWSE-X
 
               WHEN WUFLD-RQST-FETCH-NEXT
                    PERFORM  3000-EXEC-FETCH-NEXT
                        THRU 3000-EXEC-FETCH-NEXT-X
 
               WHEN WUFLD-RQST-CLOSE-BROWSE-CUR
                    PERFORM  4000-EXEC-CLOSE-CUR
                        THRU 4000-EXEC-CLOSE-CUR-X
 
               WHEN WUFLD-RQST-BROWSE-INDEX
                    PERFORM  1000-DETERMINE-SQL-REQIR
                        THRU 1000-DETERMINE-SQL-REQIR-X
                    PERFORM  5000-EXEC-BROWSE-INDEX
                        THRU 5000-EXEC-BROWSE-INDEX-X
 
               WHEN WUFLD-RQST-FETCH-NEXT-INDEX
                    PERFORM  6000-EXEC-FETCH-NEXT-INDEX
                        THRU 6000-EXEC-FETCH-NEXT-INDEX-X
 
               WHEN WUFLD-RQST-CLOSE-BROWSE-INDEX
                    PERFORM  7000-EXEC-CLOSE-INDEX
                        THRU 7000-EXEC-CLOSE-INDEX-X
 
               WHEN OTHER
                    SET  WUFLD-IO-ERROR      TO  TRUE
 
           END-EVALUATE.
 
 
           GOBACK.
 
 
       0000-MAINLINE-X.
           EXIT.
 
 
      **************************
       1000-DETERMINE-SQL-REQIR.
      **************************
 
           MOVE 3                            TO  WS-OPTM-SQL-REQIR.
 
           IF  WUFLD-CO-ID = WUFLD-ENDBR-CO-ID
               SUBTRACT +1                   FROM WS-OPTM-SQL-REQIR
           ELSE
               GO TO 1000-DETERMINE-SQL-REQIR-X
           END-IF.
 
 
           IF  WUFLD-UPLD-FLD-STRUCT-NM = WUFLD-ENDBR-UPLD-FLD-STRUCT-NM
               SUBTRACT +1                   FROM WS-OPTM-SQL-REQIR
           ELSE
               GO TO 1000-DETERMINE-SQL-REQIR-X
           END-IF.
 
 
           IF  WUFLD-UPLD-FLD-APEX-NM = WUFLD-ENDBR-UPLD-FLD-APEX-NM
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
 
               WHEN WS-OPTM-SQL-REQIR <= 2
                    MOVE '02'                TO  WS-OPTM-SQL-EXEC
                    PERFORM  210G-BROWSE
                        THRU 210G-BROWSE-X
 
               WHEN OTHER
                    MOVE SPACES              TO  WS-OPTM-SQL-EXEC
                    SET  WUFLD-IO-ERROR      TO  TRUE
 
           END-EVALUATE.
 
 
           MOVE WS-OPTM-SQL-REQIR            TO  WUFLD-OPTM-SQL-REQIR.
           MOVE WS-OPTM-SQL-EXEC             TO  WUFLD-OPTM-SQL-EXEC.
 
           EVALUATE TRUE
 
               WHEN WUFLD-OPTM-SQL-EXEC = WUFLD-OPTM-SQL-REQIR
                   SET WUFLD-OPTM-SQL-OK     TO  TRUE
 
               WHEN WUFLD-OPTM-SQL-EXEC = SPACES
                   SET WUFLD-OPTM-SQL-ERROR  TO  TRUE
 
               WHEN OTHER
                   SET WUFLD-OPTM-SQL-IMPRV  TO  TRUE
 
           END-EVALUATE.
 
 
       2000-EXEC-BROWSE-X.
           EXIT.
 
 
      *************
       2101-BROWSE.
      *************
 
           EXEC SQL
             DECLARE B1CUR_UFLD CURSOR FOR
             SELECT
                 CO_ID,
                 UPLD_FLD_STRUCT_NM,
                 UPLD_FLD_APEX_NM,
                 PREV_UPDT_USER_ID,
                 PREV_UPDT_DT,
                 UPLD_FLD_TYP_CD,
                 UPLD_FLD_FILE_CD,
                 UPLD_FLD_NM,
                 UPLD_TTBL_TYP_ID
             FROM TUFLD
             WHERE
               CO_ID               = :WUFLD-CO-ID                    AND
               UPLD_FLD_STRUCT_NM  = :WUFLD-UPLD-FLD-STRUCT-NM
             AND
               UPLD_FLD_APEX_NM    BETWEEN
                                     :WUFLD-UPLD-FLD-APEX-NM         AND
                                     :WUFLD-ENDBR-UPLD-FLD-APEX-NM
             ORDER BY
                 CO_ID,
                 UPLD_FLD_STRUCT_NM,
                 UPLD_FLD_APEX_NM
             FOR FETCH ONLY
             OPTIMIZE FOR 1 ROW
           END-EXEC.
 
           EXEC SQL
                OPEN B1CUR_UFLD
           END-EXEC.
 
 
           EVALUATE SQLCODE
 
               WHEN ZERO
                    SET  WUFLD-IO-OK         TO  TRUE
 
               WHEN OTHER
                    SET  WUFLD-IO-ERROR      TO  TRUE
 
           END-EVALUATE.
 
 
       2101-BROWSE-X.
           EXIT.
 
 
      *************
       210G-BROWSE.
      *************
 
           EXEC SQL
             DECLARE BCUR_UFLD CURSOR FOR
             SELECT
                 CO_ID,
                 UPLD_FLD_STRUCT_NM,
                 UPLD_FLD_APEX_NM,
                 PREV_UPDT_USER_ID,
                 PREV_UPDT_DT,
                 UPLD_FLD_TYP_CD,
                 UPLD_FLD_FILE_CD,
                 UPLD_FLD_NM,
                 UPLD_TTBL_TYP_ID
             FROM TUFLD
             WHERE
             CO_ID               = :WUFLD-CO-ID
           AND
             UPLD_FLD_STRUCT_NM  BETWEEN
                                   :WUFLD-UPLD-FLD-STRUCT-NM         AND
                                   :WUFLD-ENDBR-UPLD-FLD-STRUCT-NM
           AND
            (UPLD_FLD_APEX_NM   >= :WUFLD-UPLD-FLD-APEX-NM           OR
             UPLD_FLD_STRUCT_NM  > :WUFLD-UPLD-FLD-STRUCT-NM)
           AND
            (UPLD_FLD_APEX_NM   <= :WUFLD-ENDBR-UPLD-FLD-APEX-NM     OR
             UPLD_FLD_STRUCT_NM  < :WUFLD-ENDBR-UPLD-FLD-STRUCT-NM)
             ORDER BY
                 CO_ID,
                 UPLD_FLD_STRUCT_NM,
                 UPLD_FLD_APEX_NM
             FOR FETCH ONLY
             OPTIMIZE FOR 1 ROW
           END-EXEC.
 
           EXEC SQL
                OPEN BCUR_UFLD
           END-EXEC.
 
 
           EVALUATE SQLCODE
 
               WHEN ZERO
                    SET  WUFLD-IO-OK         TO  TRUE
 
               WHEN OTHER
                    SET  WUFLD-IO-ERROR      TO  TRUE
 
           END-EVALUATE.
 
 
       210G-BROWSE-X.
           EXIT.
 
 
      **********************
       3000-EXEC-FETCH-NEXT.
      **********************
 
           EVALUATE WS-OPTM-SQL-EXEC
 
               WHEN '01'
                    PERFORM  3101-FETCH-NEXT
                        THRU 3101-FETCH-NEXT-X
 
               WHEN '02'
                    PERFORM  310G-FETCH-NEXT
                        THRU 310G-FETCH-NEXT-X
 
               WHEN OTHER
                    PERFORM  310G-FETCH-NEXT
                        THRU 310G-FETCH-NEXT-X
 
           END-EVALUATE.
 
 
           SET  WUFLD-OPTM-SQL-OK            TO  TRUE.
           MOVE SPACES                       TO  WUFLD-OPTM-SQL-REQIR.
           MOVE SPACES                       TO  WUFLD-OPTM-SQL-EXEC.
 
 
       3000-EXEC-FETCH-NEXT-X.
           EXIT.
 
 
      *****************
       3101-FETCH-NEXT.
      *****************
 
           MOVE LOW-VALUES                   TO  ZUFLD-NULL-INDICATORS.
 
           EXEC SQL
             FETCH B1CUR_UFLD
             INTO
                :RUFLD-CO-ID,
                :RUFLD-UPLD-FLD-STRUCT-NM,
                :RUFLD-UPLD-FLD-APEX-NM,
                :RUFLD-PREV-UPDT-USER-ID,
                :RUFLD-PREV-UPDT-DT       :ZUFLD-PREV-UPDT-DT-NI,
                :RUFLD-UPLD-FLD-TYP-CD,
                :RUFLD-UPLD-FLD-FILE-CD,
                :RUFLD-UPLD-FLD-NM,
                :RUFLD-UPLD-TTBL-TYP-ID
           END-EXEC.
 
 
           EVALUATE SQLCODE
 
               WHEN ZERO
                    SET  WUFLD-IO-OK         TO  TRUE
                    PERFORM  UFLD-2000-SET-NULL-DFLT
                        THRU UFLD-2000-SET-NULL-DFLT-X
                    MOVE RUFLD-KEY           TO  WUFLD-KEY
 
               WHEN +100
                    SET  WUFLD-IO-EOF        TO  TRUE
 
               WHEN OTHER
                    SET  WUFLD-IO-ERROR      TO  TRUE
 
           END-EVALUATE.
 
 
       3101-FETCH-NEXT-X.
           EXIT.
 
 
      *****************
       310G-FETCH-NEXT.
      *****************
 
           MOVE LOW-VALUES                   TO  ZUFLD-NULL-INDICATORS.
 
           EXEC SQL
             FETCH BCUR_UFLD
             INTO
                :RUFLD-CO-ID,
                :RUFLD-UPLD-FLD-STRUCT-NM,
                :RUFLD-UPLD-FLD-APEX-NM,
                :RUFLD-PREV-UPDT-USER-ID,
                :RUFLD-PREV-UPDT-DT       :ZUFLD-PREV-UPDT-DT-NI,
                :RUFLD-UPLD-FLD-TYP-CD,
                :RUFLD-UPLD-FLD-FILE-CD,
                :RUFLD-UPLD-FLD-NM,
                :RUFLD-UPLD-TTBL-TYP-ID
           END-EXEC.
 
 
           EVALUATE SQLCODE
 
               WHEN ZERO
                    SET  WUFLD-IO-OK         TO  TRUE
                    PERFORM  UFLD-2000-SET-NULL-DFLT
                        THRU UFLD-2000-SET-NULL-DFLT-X
                    MOVE RUFLD-KEY           TO  WUFLD-KEY
 
               WHEN +100
                    SET  WUFLD-IO-EOF        TO  TRUE
 
               WHEN OTHER
                    SET  WUFLD-IO-ERROR      TO  TRUE
 
           END-EVALUATE.
 
 
       310G-FETCH-NEXT-X.
           EXIT.
 
 
      *********************
       4000-EXEC-CLOSE-CUR.
      *********************
 
           EVALUATE WS-OPTM-SQL-EXEC
 
               WHEN '01'
                    PERFORM  4101-CLOSE-BROWSE-CUR
                        THRU 4101-CLOSE-BROWSE-CUR-X
 
               WHEN '02'
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
       4101-CLOSE-BROWSE-CUR.
      ***********************
 
           EXEC SQL
                CLOSE B1CUR_UFLD
           END-EXEC.
 
 
           EVALUATE SQLCODE
 
               WHEN ZERO
                    SET  WUFLD-IO-OK         TO  TRUE
 
               WHEN OTHER
                    SET  WUFLD-IO-ERROR      TO  TRUE
 
           END-EVALUATE.
 
 
       4101-CLOSE-BROWSE-CUR-X.
           EXIT.
 
 
      ***********************
       410G-CLOSE-BROWSE-CUR.
      ***********************
 
           EXEC SQL
                CLOSE BCUR_UFLD
           END-EXEC.
 
 
           EVALUATE SQLCODE
 
               WHEN ZERO
                    SET  WUFLD-IO-OK         TO  TRUE
 
               WHEN OTHER
                    SET  WUFLD-IO-ERROR      TO  TRUE
 
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
 
           SET  WUFLD-IO-ERROR               TO  TRUE.
           MOVE WS-OPTM-SQL-REQIR            TO  WUFLD-OPTM-SQL-REQIR.
           MOVE WS-OPTM-SQL-EXEC             TO  WUFLD-OPTM-SQL-EXEC.
 
           EVALUATE TRUE
 
               WHEN WUFLD-OPTM-SQL-EXEC = WUFLD-OPTM-SQL-REQIR
                   SET WUFLD-OPTM-SQL-OK     TO  TRUE
 
               WHEN WUFLD-OPTM-SQL-EXEC = SPACES
                   SET WUFLD-OPTM-SQL-ERROR  TO  TRUE
 
               WHEN OTHER
                   SET WUFLD-OPTM-SQL-IMPRV  TO  TRUE
 
           END-EVALUATE.
 
 
       5000-EXEC-BROWSE-INDEX-X.
           EXIT.
 
 
      ****************************
       6000-EXEC-FETCH-NEXT-INDEX.
      ****************************
 
      * PROFILE INDICATED THAT -FETCH-NEXT-INDEX
      * PARAGRAPHS ARE NOT APPLICABLE
 
           SET  WUFLD-IO-ERROR               TO  TRUE.
           SET  WUFLD-OPTM-SQL-OK            TO  TRUE.
           MOVE SPACES                       TO  WUFLD-OPTM-SQL-REQIR.
           MOVE SPACES                       TO  WUFLD-OPTM-SQL-EXEC.
 
 
       6000-EXEC-FETCH-NEXT-INDEX-X.
           EXIT.
 
 
      ***********************
       7000-EXEC-CLOSE-INDEX.
      ***********************
 
      * PROFILE INDICATED THAT -CLOSE-BROWSE-INDEX
      * PARAGRAPHS ARE NOT APPLICABLE
 
           SET  WUFLD-IO-ERROR               TO  TRUE.
           SET  WS-OPTM-SQL-CUR-CLOSED       TO  TRUE.
 
 
       7000-EXEC-CLOSE-INDEX-X.
           EXIT.
 
 
       COPY ACPZUFLD.
 
      *****************************************************************
      **                 END OF PROGRAM ASIBUFLD                     **
      *****************************************************************
