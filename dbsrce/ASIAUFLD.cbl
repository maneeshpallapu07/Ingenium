      *************************
       IDENTIFICATION DIVISION.
      *************************
 
       PROGRAM-ID. ASIAUFLD.
 
       COPY XCWWCRHT.
 
      *****************************************************************
      **  MEMBER : ASIAUFLD                                          **
      **  REMARKS: SQL I/O PROGRAM USED TO INSERT A ROW INTO THE     **
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
 
       COPY XCWWPGWS REPLACING '$VAR1' BY 'ASIAUFLD'.
 
       01  WS-WORKING-STORAGE.
           05  WS-WA-ADDRESS                    POINTER   VALUE NULL.
           05  WS-TL-ADDRESS                    POINTER   VALUE NULL.
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
 
               WHEN WUFLD-RQST-INSERT
                    PERFORM  1000-INSERT
                        THRU 1000-INSERT-X
 
               WHEN OTHER
                    SET  WUFLD-IO-ERROR      TO  TRUE
 
           END-EVALUATE.
 
 
           SET  WUFLD-OPTM-SQL-OK            TO  TRUE.
           MOVE SPACES                       TO  WUFLD-OPTM-SQL-REQIR.
           MOVE SPACES                       TO  WUFLD-OPTM-SQL-EXEC.
 
 
           GOBACK.
 
 
       0000-MAINLINE-X.
           EXIT.
 
 
      *************
       1000-INSERT.
      *************
 
           PERFORM  UFLD-1000-SET-NULL-IND
               THRU UFLD-1000-SET-NULL-IND-X.
 
           MOVE WUFLD-KEY                    TO  RUFLD-KEY.
 
           EXEC SQL
             INSERT INTO TUFLD
                (CO_ID,
                 UPLD_FLD_STRUCT_NM,
                 UPLD_FLD_APEX_NM,
                 PREV_UPDT_USER_ID,
                 PREV_UPDT_DT,
                 UPLD_FLD_TYP_CD,
                 UPLD_FLD_FILE_CD,
                 UPLD_FLD_NM,
                 UPLD_TTBL_TYP_ID)
             VALUES
               (:RUFLD-CO-ID,
                :RUFLD-UPLD-FLD-STRUCT-NM,
                :RUFLD-UPLD-FLD-APEX-NM,
                :RUFLD-PREV-UPDT-USER-ID,
                :RUFLD-PREV-UPDT-DT       :ZUFLD-PREV-UPDT-DT-NI,
                :RUFLD-UPLD-FLD-TYP-CD,
                :RUFLD-UPLD-FLD-FILE-CD,
                :RUFLD-UPLD-FLD-NM,
                :RUFLD-UPLD-TTBL-TYP-ID)
           END-EXEC.
 
 
           EVALUATE SQLCODE
 
               WHEN ZERO
                    SET  WUFLD-IO-OK         TO  TRUE
 
               WHEN OTHER
                    SET  WUFLD-IO-ERROR      TO  TRUE
 
           END-EVALUATE.
 
 
       1000-INSERT-X.
           EXIT.
 
 
       COPY ACPZUFLD.
 
      *****************************************************************
      **                 END OF PROGRAM ASIAUFLD                     **
      *****************************************************************

