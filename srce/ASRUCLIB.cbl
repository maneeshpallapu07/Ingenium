      *************************
       IDENTIFICATION DIVISION.
      *************************
 
       PROGRAM-ID.  ASRUCLIB.
 
       COPY XCWWCRHT.
 
      *****************************************************************
      **  MEMBER :  ASRUCLIB                                         **
      **  REMARKS:  APEX UPLOAD CLIB TABLE PROCESSING                **
      **                                                             **
      **  DOMAIN :  CL                                               **
      **  CLASS  :  FD                                               **
      *****************************************************************
      **  DATE     AUTH.  DESCRIPTION                                **
      **                                                             **
APEX54**  31DEC96  TJS    MODIFICATIONS FOR MAINTAINABILITY          **
557698**  30SEP97  TJS    MIXED CASE DATA                            **
557700**  30SEP97  TJS    APEX UPLOAD 5.5                            **
014590**  15DEC99  60     ARCHITECTURAL CHANGES                      **
      *****************************************************************
 
      **********************
       ENVIRONMENT DIVISION.
      **********************
 
      ***************
       DATA DIVISION.
      ***************
 
       WORKING-STORAGE SECTION.
 
       COPY XCWWPGWS REPLACING '$VAR1' BY 'ASRUCLIB'.
 
       COPY SQLCA.
 
014590*COPY XCWL0030.
      /
557698 COPY XCWL0005.
      /
      *****************************************************************
      *  COMMON COPYBOOKS
      *****************************************************************
       COPY ACWWAPUP.
      /
      *****************************************************************
      *  I/O COPYBOOKS
      *****************************************************************
       COPY ACFRUTTB.
       COPY ACFWUTTB.
      /
      *****************
       LINKAGE SECTION.
      *****************
       01  WGLOB-GLOBAL-AREA.
       COPY XCWWGLOB.
      /
       COPY ACWLAPUP.
       COPY ACWLCLIB.
       COPY ACFRUFLD.
       COPY CCFRCLIB.
      /
       PROCEDURE DIVISION  USING  WGLOB-GLOBAL-AREA
                                  LAPUP-PARM-AREA
                                  LCLIB-PARM-AREA
                                  RUFLD-REC-INFO
                                  RCLIB-REC-INFO.
 
      *--------------
       0000-MAINLINE.
      *--------------
 
           PERFORM  1000-INITIALIZE
               THRU 1000-INITIALIZE-X.
 
           EVALUATE TRUE
 
               WHEN RUFLD-UPLD-FLD-TYP-COMPLEX
                    PERFORM  2000-PROCESS-COMPLEX-FIELD
                        THRU 2000-PROCESS-COMPLEX-FIELD-X
 
557700*        WHEN RUFLD-UPLD-FLD-TYP-FIELD
557700         WHEN RUFLD-UPLD-FLD-TYP-TRANS-NAME
                    PERFORM  3000-PROCESS-FIELD-FIELD
                        THRU 3000-PROCESS-FIELD-FIELD-X
 
557700*        WHEN RUFLD-UPLD-FLD-TYP-CHAR
557700         WHEN RUFLD-UPLD-FLD-TYP-MIX-CASE
557700         WHEN RUFLD-UPLD-FLD-TYP-UPPER-CASE
               WHEN RUFLD-UPLD-FLD-TYP-DATE
               WHEN RUFLD-UPLD-FLD-TYP-NUMERIC
               WHEN RUFLD-UPLD-FLD-TYP-DOLLAR-AMT
557700*        WHEN RUFLD-UPLD-FLD-TYP-TRANS
557700         WHEN RUFLD-UPLD-FLD-TYP-TRANS-VALU
                    SET  LAPUP-UNKNOWN-FIELD-ERR
                                            TO TRUE
 
               WHEN RUFLD-UPLD-FLD-TYP-UNUSED
                    CONTINUE
 
               WHEN OTHER
                    SET  LAPUP-UNKNOWN-FIELD-TYPE
                                            TO TRUE
 
           END-EVALUATE.
 
       0000-MAINLINE-X.
           GOBACK.
      /
      *----------------
       1000-INITIALIZE.
      *----------------
 
           MOVE WAPUP-C-GOOD-RETURN-CD      TO LAPUP-SUB-RETURN-CD.
           SET  LAPUP-GOOD-RETURN           TO TRUE.
           SET  LAPUP-REC-NOT-CHANGED       TO TRUE.
 
       1000-INITIALIZE-X.
           EXIT.
      /
      *---------------------------
       2000-PROCESS-COMPLEX-FIELD.
      *---------------------------
 
           EVALUATE RUFLD-UPLD-FLD-NM
 
               WHEN 'CLI_CO_NM'
                    PERFORM  2100-CLI-CO-NM
                        THRU 2100-CLI-CO-NM-X
 
               WHEN 'CLI_PAC_ACCT_ID'
                    MOVE LAPUP-INPUT-DATA   TO LCLIB-CLI-PAC-ACCT-ID
 
               WHEN 'CLI_PAC_BNK_ID'
                    MOVE LAPUP-INPUT-DATA   TO LCLIB-CLI-PAC-BNK-ID
 
               WHEN 'CLI_PAC_MICR_IND'
                    PERFORM  2300-CLI-PAC-MICR-IND
                        THRU 2300-CLI-PAC-MICR-IND-X
 
               WHEN 'PAC_ACCT_TYP_CD'
                    PERFORM  2500-PAC-ACCT-TYP-CD
                        THRU 2500-PAC-ACCT-TYP-CD-X
 
               WHEN 'PAC_BNK_BR_CD'
                    MOVE LAPUP-INPUT-DATA   TO LCLIB-PAC-BNK-BR-ID
 
               WHEN OTHER
                    SET  LAPUP-UNKNOWN-FIELD-ERR
                                            TO TRUE
 
           END-EVALUATE.
 
       2000-PROCESS-COMPLEX-FIELD-X.
           EXIT.
      /
      *---------------
       2100-CLI-CO-NM.
      *---------------
 
           IF  RUFLD-PAC-STRUCT
               PERFORM  2110-EVALUATE-NAME
                   THRU 2110-EVALUATE-NAME-X
 
           ELSE
               SET  LAPUP-UNKNOWN-FIELD-ERR TO TRUE
           END-IF.
 
       2100-CLI-CO-NM-X.
           EXIT.
      /
      *-------------------
       2110-EVALUATE-NAME.
      *-------------------
 
           EVALUATE RUFLD-UPLD-FLD-APEX-NM
 
               WHEN 'FIRST_NAME'
557698              PERFORM  8000-TRANSLATE-UPPER-CASE
557698                  THRU 8000-TRANSLATE-UPPER-CASE-X
                    MOVE LAPUP-INPUT-DATA   TO LCLIB-PAC-FIRST-NM
                    SET  LAPUP-REC-CHANGED  TO TRUE
 
               WHEN 'INITIAL'
                    MOVE LAPUP-INPUT-DATA   TO LCLIB-PAC-INIT-NM
                    SET  LAPUP-REC-CHANGED  TO TRUE
 
               WHEN 'LAST_NAME'
557698              PERFORM  8000-TRANSLATE-UPPER-CASE
557698                  THRU 8000-TRANSLATE-UPPER-CASE-X
                    MOVE LAPUP-INPUT-DATA   TO LCLIB-PAC-LAST-NM
                    SET  LAPUP-REC-CHANGED  TO TRUE
 
               WHEN OTHER
                    SET  LAPUP-UNKNOWN-FIELD-ERR
                                            TO TRUE
 
           END-EVALUATE.
 
       2110-EVALUATE-NAME-X.
           EXIT.
      /
      *----------------------
       2300-CLI-PAC-MICR-IND.
      *----------------------
 
           MOVE RUFLD-UPLD-TTBL-TYP-ID      TO WUTTB-UPLD-TTBL-TYP-ID.
           MOVE LAPUP-INPUT-DATA            TO WUTTB-UPLD-TTBL-VALU-ID.
 
           PERFORM  UTTB-1000-LOOKUP-UTTB
               THRU UTTB-1000-LOOKUP-UTTB-X.
 
           IF  WUTTB-IO-OK
               MOVE RUTTB-UPLD-TTBL-VALU-TXT
                                            TO LCLIB-CLI-PAC-MICR-IND
               SET  LAPUP-REC-CHANGED       TO TRUE
           ELSE
               SET  LAPUP-TRAN-CONV-ERR     TO TRUE
           END-IF.
 
       2300-CLI-PAC-MICR-IND-X.
           EXIT.
      /
      *---------------------
       2500-PAC-ACCT-TYP-CD.
      *---------------------
 
           MOVE RUFLD-UPLD-TTBL-TYP-ID      TO WUTTB-UPLD-TTBL-TYP-ID.
           MOVE LAPUP-INPUT-DATA            TO WUTTB-UPLD-TTBL-VALU-ID.
 
           PERFORM  UTTB-1000-LOOKUP-UTTB
               THRU UTTB-1000-LOOKUP-UTTB-X.
 
           IF  WUTTB-IO-OK
               MOVE RUTTB-UPLD-TTBL-VALU-TXT
                                            TO LCLIB-PAC-ACCT-TYP-CD
               SET  LAPUP-REC-CHANGED       TO TRUE
           ELSE
               SET  LAPUP-TRAN-CONV-ERR     TO TRUE
           END-IF.
 
       2500-PAC-ACCT-TYP-CD-X.
           EXIT.
      /
      *-------------------------
       3000-PROCESS-FIELD-FIELD.
      *-------------------------
 
           IF  LAPUP-INPUT-DATA  = '1'
               SET  LAPUP-UNKNOWN-FIELD-ERR TO TRUE
           END-IF.
 
       3000-PROCESS-FIELD-FIELD-X.
           EXIT.
      /
557698*--------------------------
557698 8000-TRANSLATE-UPPER-CASE.
557698*--------------------------
557698
557698     PERFORM  0005-1000-BUILD-PARM-INFO
557698         THRU 0005-1000-BUILD-PARM-INFO-X.
557698
557698     MOVE LAPUP-INPUT-DATA           TO L0005-INPUT-STRING.
557698
557698     PERFORM  0005-2000-CONVERT-NO-ACCENTS
557698         THRU 0005-2000-CONVERT-NO-ACCENTS-X.
557698
557698     IF  L0005-RETRN-OK
557698         MOVE L0005-OUTPUT-STRING    TO LAPUP-INPUT-DATA
557698     END-IF.
557698
557698 8000-TRANSLATE-UPPER-CASE-X.
557698     EXIT.
      /
      *****************************************************************
      *  PROCESSING COPYBOOKS
      *****************************************************************
       COPY ACPPUTTB.
557698 COPY XCPL0005.
557698 COPY XCPS0005.
      /
      *****************************************************************
      *  FILE I/O PROCESS MODULES
      *****************************************************************
       COPY ACPNUTTB.
      /
      *****************************************************************
      *  ERROR HANDLING ROUTINES
      *****************************************************************
       COPY XCPL0030.
 
      *****************************************************************
      **                 END OF PROGRAM ASRUCLIB                     **
      *****************************************************************
