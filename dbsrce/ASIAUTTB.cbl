      *************************
       IDENTIFICATION DIVISION.
      *************************
 
       PROGRAM-ID. ASIAUTTB.
 
       COPY XCWWCRHT.
 
      *****************************************************************
      **  MEMBER : ASIAUTTB                                          **
      **  REMARKS: SQL I/O PROGRAM USED TO INSERT A ROW INTO THE     **
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
 
       COPY XCWWPGWS REPLACING '$VAR1' BY 'ASIAUTTB'.
 
       01  WS-WORKING-STORAGE.
           05  WS-WA-ADDRESS                    POINTER   VALUE NULL.
           05  WS-TL-ADDRESS                    POINTER   VALUE NULL.
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
 
               WHEN WUTTB-RQST-INSERT
                    PERFORM  1000-INSERT
                        THRU 1000-INSERT-X
 
               WHEN OTHER
                    SET  WUTTB-IO-ERROR      TO  TRUE
 
           END-EVALUATE.
 
 
           SET  WUTTB-OPTM-SQL-OK            TO  TRUE.
           MOVE SPACES                       TO  WUTTB-OPTM-SQL-REQIR.
           MOVE SPACES                       TO  WUTTB-OPTM-SQL-EXEC.
 
 
           GOBACK.
 
 
       0000-MAINLINE-X.
           EXIT.
 
 
      *************
       1000-INSERT.
      *************
 
           PERFORM  UTTB-1000-SET-NULL-IND
               THRU UTTB-1000-SET-NULL-IND-X.
 
           MOVE WUTTB-KEY                    TO  RUTTB-KEY.
 
           EXEC SQL
             INSERT INTO TUTTB
                (CO_ID,
                 UPLD_TTBL_TYP_ID,
                 UPLD_TTBL_VALU_ID,
                 PREV_UPDT_USER_ID,
                 PREV_UPDT_DT,
                 UPLD_TTBL_VALU_TXT)
             VALUES
               (:RUTTB-CO-ID,
                :RUTTB-UPLD-TTBL-TYP-ID,
                :RUTTB-UPLD-TTBL-VALU-ID,
                :RUTTB-PREV-UPDT-USER-ID,
                :RUTTB-PREV-UPDT-DT       :ZUTTB-PREV-UPDT-DT-NI,
                :RUTTB-UPLD-TTBL-VALU-TXT)
           END-EXEC.
 
 
           EVALUATE SQLCODE
 
               WHEN ZERO
                    SET  WUTTB-IO-OK         TO  TRUE
 
               WHEN OTHER
                    SET  WUTTB-IO-ERROR      TO  TRUE
 
           END-EVALUATE.
 
 
       1000-INSERT-X.
           EXIT.
 
 
       COPY ACPZUTTB.
 
      *****************************************************************
      **                 END OF PROGRAM ASIAUTTB                     **
      *****************************************************************
