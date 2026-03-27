      *************************
      *IDENTIFICATION DIVISION.
      *************************
      *
      *PROGRAM-ID.  ASRUBENE.
      *
      *COPY XCWWCRHT.
      *
      *****************************************************************
      **  MEMBER :  ASRUBENE                                         **
      **  REMARKS:  APEX UPLOAD BENE TABLE PROCESSING                **
      **                                                             **
      **  DOMAIN :  CL                                               **
      **  CLASS  :  FD                                               **
      *****************************************************************
      **  DATE     AUTH.  DESCRIPTION                                **
      **                                                             **
APEX53**  30NOV95  JJS    CREATED FOR INGENIUM 5.3 & WINAPEX 1.0     **
APEX54**  31DEC96  TJS    MODIFICATIONS FOR MAINTAINABILITY          **
557700**  30SEP97  TJS    APEX UPLOAD 5.5                            **
010313**  30OCT98  56     APEX UPLOAD 5.6                            **
014590**  15DEC99  60     ARCHITECTURAL CHANGES                      **
MCL   **  01AUG01  EKM    THIS PROGRAM NOT USED BY MCL               **
      *****************************************************************
      *
      **********************
      *ENVIRONMENT DIVISION.
      **********************
      *
      ***************
      *DATA DIVISION.
      ***************
      *
      *WORKING-STORAGE SECTION.
      *
      *COPY XCWWPGWS REPLACING '$VAR1' BY 'ASRUBENE'.
      *
      *COPY SQLCA.
      *
014590*COPY XCWL0030.
      *
      *01  WS-WORK-AREA.
      *    05  WS-SUB                       PIC S9(04) COMP.
      *    05  WS-APEX-FLD-NM.
      *        10  FILLER                   PIC X(01).
      *        10  WS-APEX-FLD-NM-BENE-TYP  PIC X(01).
      *            88 WS-APEX-FLD-NM-BENE-PRIMARY  VALUE 'P'.
      *            88 WS-APEX-FLD-NM-BENE-CONT     VALUE 'C'.
      *        10  WS-APEX-FLD-NM-BENE-NO   PIC 9(01).
010313*    05  WS-APEX-FLD-NM-R             REDEFINES
010313*        WS-APEX-FLD-NM.
010313*        10  WS-APEX-FLD-NM-2         PIC X(02).
010313*            88  WS-APEX-FLD-EQUAL-SHARES VALUE 'EP' 'EC'.
010313*        10  FILLER                   PIC X(01).
      *
      *
      *COPY ACWWAPUP.
      *
      *COPY XCWTFCMD.
      *
      *COPY XCWWWKDT.
      *
      *COPY ACFRUTTB.
      *COPY ACFWUTTB.
      *
      *COPY XCWL0280.
      *
      *****************
      *LINKAGE SECTION.
      *****************
      *
      *01  WGLOB-GLOBAL-AREA.
      *COPY XCWWGLOB.
      *
      *COPY ACWLAPUP.
      *
      *COPY ACFRUFLD.
      *
      *COPY CCFRBENE.
      *
      *PROCEDURE DIVISION  USING  WGLOB-GLOBAL-AREA
      *                           LAPUP-PARM-AREA
      *                           RUFLD-REC-INFO
      *                           RBENE-REC-INFO.
      *
      *--------------
      *0000-MAINLINE.
      *--------------
      *
      *    PERFORM  1000-INITIALIZE
      *        THRU 1000-INITIALIZE-X.
      *
      *    EVALUATE TRUE
      *
557700*        WHEN RUFLD-UPLD-FLD-TYP-CHAR
557700*        WHEN RUFLD-UPLD-FLD-TYP-MIX-CASE
557700*        WHEN RUFLD-UPLD-FLD-TYP-UPPER-CASE
      *             PERFORM  2000-PROCESS-CHAR-FIELD
      *                 THRU 2000-PROCESS-CHAR-FIELD-X
      *
      *        WHEN RUFLD-UPLD-FLD-TYP-DATE
      *             PERFORM  3000-PROCESS-DATE-FIELD
      *                 THRU 3000-PROCESS-DATE-FIELD-X
      *
      *        WHEN RUFLD-UPLD-FLD-TYP-NUMERIC
      *        WHEN RUFLD-UPLD-FLD-TYP-DOLLAR-AMT
      *             PERFORM  4000-PROCESS-NUMERIC-FIELD
      *                 THRU 4000-PROCESS-NUMERIC-FIELD-X
      *
557700*        WHEN RUFLD-UPLD-FLD-TYP-TRANS
557700*        WHEN RUFLD-UPLD-FLD-TYP-TRANS-VALU
      *             PERFORM  5000-PROCESS-TRANS-FIELD
      *                 THRU 5000-PROCESS-TRANS-FIELD-X
      *
      *        WHEN RUFLD-UPLD-FLD-TYP-COMPLEX
      *             PERFORM  6000-PROCESS-COMPLEX-FIELD
      *                 THRU 6000-PROCESS-COMPLEX-FIELD-X
      *
557700*        WHEN RUFLD-UPLD-FLD-TYP-FIELD
557700*        WHEN RUFLD-UPLD-FLD-TYP-TRANS-NAME
      *             PERFORM  7000-PROCESS-FIELD-FIELD
      *                 THRU 7000-PROCESS-FIELD-FIELD-X
      *
      *        WHEN RUFLD-UPLD-FLD-TYP-UNUSED
      *             CONTINUE
      *
      *        WHEN OTHER
      *             MOVE WAPUP-C-UNKNOWN-FIELD-TYPE TO LAPUP-RETURN-CD
      *
      *    END-EVALUATE.
      *
      *0000-MAINLINE-X.
      *    GOBACK.
      *
      *----------------
      *1000-INITIALIZE.
      *----------------
      *
      *    MOVE WAPUP-C-GOOD-RETURN-CD     TO LAPUP-RETURN-CD.
      *    MOVE WAPUP-C-GOOD-RETURN-CD     TO LAPUP-SUB-RETURN-CD.
      *    MOVE WAPUP-C-NO                 TO LAPUP-REC-CHANGED-SW.
      *
      *1000-INITIALIZE-X.
      *    EXIT.
      *
      *------------------------
      *2000-PROCESS-CHAR-FIELD.
      *------------------------
      *
      *    MOVE WAPUP-C-UNKNOWN-FIELD-ERR  TO LAPUP-RETURN-CD.
      *
      *2000-PROCESS-CHAR-FIELD-X.
      *    EXIT.
      *
      *------------------------
      *3000-PROCESS-DATE-FIELD.
      *------------------------
      *
      *    MOVE WAPUP-C-UNKNOWN-FIELD-ERR  TO LAPUP-RETURN-CD.
      *
      *3000-PROCESS-DATE-FIELD-X.
      *    EXIT.
      *
      *---------------------------
      *4000-PROCESS-NUMERIC-FIELD.
      *---------------------------
      *
      *    MOVE WAPUP-C-UNKNOWN-FIELD-ERR  TO LAPUP-RETURN-CD.
      *
      *4000-PROCESS-NUMERIC-FIELD-X.
      *    EXIT.
      *
      *-------------------------
      *5000-PROCESS-TRANS-FIELD.
      *-------------------------
      *
      *    MOVE WAPUP-C-UNKNOWN-FIELD-ERR  TO LAPUP-RETURN-CD.
      *
      *5000-PROCESS-TRANS-FIELD-X.
      *    EXIT.
      *
      *---------------------------
      *6000-PROCESS-COMPLEX-FIELD.
      *---------------------------
      *
      *
      * CHECK IF BENEFICIARY IS THE ESTATE
      *
      *
      *    IF  (RUFLD-UPLD-FLD-STRUCT-NM = 'INSURANCE'
      *    OR  RUFLD-UPLD-FLD-STRUCT-NM  = 'IMMEDIATE'
      *    OR  RUFLD-UPLD-FLD-STRUCT-NM  = 'DEFERRED')
      *    AND RUFLD-UPLD-FLD-APEX-NM    = 'BENEFICIARY'
      *        IF  LAPUP-INPUT-DATA = '1'
      *            MOVE 'ESTATE'          TO LAPUP-BENE-CLI-ID (1)
      *            MOVE 'O'               TO LAPUP-BENE-TYP-CD (1)
      *            MOVE 'ESTA'            TO LAPUP-BENE-REL-INSRD-CD (1)
      *            MOVE 100               TO LAPUP-BENE-PRCDS-PCT-N (1)
      *            MOVE 'P'               TO LAPUP-BENE-DESGNT-CD (1)
      *            MOVE WAPUP-C-YES       TO LAPUP-REC-CHANGED-SW
      *            GO TO 6000-PROCESS-COMPLEX-FIELD-X
      *        ELSE
      *            GO TO 6000-PROCESS-COMPLEX-FIELD-X
      *        END-IF
      *    END-IF.
      *
      *
      * ONLY PROCESS BENEFICIARY STRUCTURE
      *
      *
      *    IF  NOT RUFLD-BENEF-STRUCT
      *        MOVE WAPUP-C-UNKNOWN-FIELD-ERR TO LAPUP-RETURN-CD
      *        GO TO 6000-PROCESS-COMPLEX-FIELD-X
      *    END-IF.
      *
      *
      * NOW BEGIN TO PROCESS BENEFICIARY
      *
      *
      *    MOVE RUFLD-UPLD-FLD-APEX-NM     TO WS-APEX-FLD-NM.
      *
      *    EVALUATE TRUE
      *
      *        WHEN WS-APEX-FLD-NM-BENE-PRIMARY
      *             MOVE WS-APEX-FLD-NM-BENE-NO  TO WS-SUB
      *             MOVE 'P'            TO LAPUP-BENE-DESGNT-CD (WS-SUB)
      *
      *        WHEN WS-APEX-FLD-NM-BENE-CONT
      *             COMPUTE WS-SUB = WS-APEX-FLD-NM-BENE-NO + 6
      *             MOVE 'C'            TO LAPUP-BENE-DESGNT-CD (WS-SUB)
      *
      *        WHEN OTHER
      *            MOVE WAPUP-C-UNKNOWN-FIELD-ERR TO LAPUP-RETURN-CD
      *            GO TO 6000-PROCESS-COMPLEX-FIELD-X
      *
      *    END-EVALUATE.
      *
010313*    IF  RUFLD-UPLD-FLD-NM = 'BNFY_NM'
010313*        MOVE LAPUP-INPUT-DATA    TO LAPUP-BENE-BNFY-NM (WS-SUB)
010313*        MOVE WAPUP-C-YES         TO LAPUP-REC-CHANGED-SW
010313*        GO TO 6000-PROCESS-COMPLEX-FIELD-X
010313*    END-IF.
      *
      *    IF  RUFLD-UPLD-FLD-NM = 'CLI_ID'
      *        MOVE 2                      TO L0280-LENGTH
      *        MOVE 0                      TO L0280-PRECISION
      *        MOVE LAPUP-INPUT-DATA       TO L0280-INPUT-DATA
      *        PERFORM  0280-1000-NUMERIC-EDIT
      *            THRU 0280-1000-NUMERIC-EDIT-X
      *        IF  L0280-OK
APEX54*            PERFORM  6001-OUTPUT-ZERO
APEX54*                THRU 6001-OUTPUT-ZERO-X
APEX54*            GO TO 6000-PROCESS-COMPLEX-FIELD-X
      *
      *        ELSE
      *            MOVE WAPUP-C-NUM-CONV-ERR TO LAPUP-RETURN-CD
      *            MOVE L0280-STATUS         TO LAPUP-SUB-RETURN-CD
      *            GO TO 6000-PROCESS-COMPLEX-FIELD-X
      *        END-IF
      *    END-IF.
      *
      *    IF  RUFLD-UPLD-FLD-NM = 'BNFY_PRCDS_PCT'
010313*** MULTIPLE APEX FIELDS, WITH DIFFERENT MEANINGS, MAP TO
010313*** BNFY_PRCDS_PCT.  EC? AND EP? MEAN EQUAL SHARES AND THERE
010313*** WILL BE NO ACTUAL NUMERIC SHARE UPLOADED.
010313*        IF  RUFLD-UPLD-FLD-STRUCT-NM = 'BENEF'
010313*        AND WS-APEX-FLD-EQUAL-SHARES
010313*            MOVE LAPUP-INPUT-DATA       TO
010313*                 LAPUP-BENE-EQUAL-SHARES-SW (WS-SUB)
010313*            MOVE WAPUP-C-YES            TO LAPUP-REC-CHANGED-SW
010313*            GO TO 6000-PROCESS-COMPLEX-FIELD-X
010313*        END-IF
010313*
      *        MOVE 3                      TO L0280-LENGTH
      *        MOVE 0                      TO L0280-PRECISION
      *        MOVE LAPUP-INPUT-DATA       TO L0280-INPUT-DATA
      *        PERFORM  0280-1000-NUMERIC-EDIT
      *            THRU 0280-1000-NUMERIC-EDIT-X
      *        IF  L0280-OK
      *            COMPUTE LAPUP-BENE-PRCDS-PCT-N (WS-SUB)
      *                = L0280-OUTPUT / (10 ** L0280-PRECISION)
      *            MOVE WAPUP-C-YES        TO LAPUP-REC-CHANGED-SW
      *            GO TO 6000-PROCESS-COMPLEX-FIELD-X
      *        ELSE
      *            MOVE WAPUP-C-NUM-CONV-ERR TO LAPUP-RETURN-CD
      *            MOVE L0280-STATUS         TO LAPUP-SUB-RETURN-CD
      *            GO TO 6000-PROCESS-COMPLEX-FIELD-X
      *        END-IF
      *    END-IF.
      *
      *    IF  RUFLD-UPLD-FLD-NM = 'BNFY_REL_INSRD_CD'
      *        MOVE RUFLD-UPLD-TTBL-TYP-ID TO WUTTB-UPLD-TTBL-TYP-ID
      *        MOVE LAPUP-INPUT-DATA       TO WUTTB-UPLD-TTBL-VALU-ID
      *        PERFORM  UTTB-1000-LOOKUP-UTTB
      *            THRU UTTB-1000-LOOKUP-UTTB-X
      *        IF  WUTTB-IO-OK
      *            MOVE RUTTB-UPLD-TTBL-VALU-TXT TO
      *                    LAPUP-BENE-REL-INSRD-CD (WS-SUB)
      *            MOVE WAPUP-C-YES        TO LAPUP-REC-CHANGED-SW
      *            GO TO 6000-PROCESS-COMPLEX-FIELD-X
      *        ELSE
      *            MOVE WAPUP-C-TRAN-CONV-ERR TO LAPUP-RETURN-CD
      *            GO TO 6000-PROCESS-COMPLEX-FIELD-X
      *        END-IF
      *    END-IF.
      *
      *    IF  RUFLD-UPLD-FLD-NM = 'BNFY_TYP_CD'
      *        MOVE RUFLD-UPLD-TTBL-TYP-ID TO WUTTB-UPLD-TTBL-TYP-ID
      *        MOVE LAPUP-INPUT-DATA       TO WUTTB-UPLD-TTBL-VALU-ID
      *        PERFORM  UTTB-1000-LOOKUP-UTTB
      *            THRU UTTB-1000-LOOKUP-UTTB-X
      *        IF  WUTTB-IO-OK
      *            MOVE RUTTB-UPLD-TTBL-VALU-TXT TO
      *                    LAPUP-BENE-TYP-CD (WS-SUB)
      *            MOVE WAPUP-C-YES        TO LAPUP-REC-CHANGED-SW
      *            GO TO 6000-PROCESS-COMPLEX-FIELD-X
      *        ELSE
      *            MOVE WAPUP-C-TRAN-CONV-ERR TO LAPUP-RETURN-CD
      *            GO TO 6000-PROCESS-COMPLEX-FIELD-X
      *        END-IF
      *    END-IF.
      *
010313*    IF  RUFLD-UPLD-FLD-NM = 'BNFY_MINR_IND'
010313*        MOVE RUFLD-UPLD-TTBL-TYP-ID TO WUTTB-UPLD-TTBL-TYP-ID
010313*        MOVE LAPUP-INPUT-DATA       TO WUTTB-UPLD-TTBL-VALU-ID
010313*        PERFORM  UTTB-1000-LOOKUP-UTTB
010313*            THRU UTTB-1000-LOOKUP-UTTB-X
010313*        IF  WUTTB-IO-OK
010313*            MOVE RUTTB-UPLD-TTBL-VALU-TXT TO
010313*                    LAPUP-BENE-MINR-IND (WS-SUB)
010313*            MOVE WAPUP-C-YES        TO LAPUP-REC-CHANGED-SW
010313*            GO TO 6000-PROCESS-COMPLEX-FIELD-X
010313*        ELSE
010313*            MOVE WAPUP-C-TRAN-CONV-ERR TO LAPUP-RETURN-CD
010313*            GO TO 6000-PROCESS-COMPLEX-FIELD-X
010313*        END-IF
010313*    END-IF.
      *
      *    MOVE WAPUP-C-UNKNOWN-FIELD-ERR  TO LAPUP-RETURN-CD.
      *
      *6000-PROCESS-COMPLEX-FIELD-X.
      *    EXIT.
      *
      *-----------------
APEX54*6001-OUTPUT-ZERO.
      *-----------------
      *
      *    IF  L0280-OUTPUT = ZERO
APEX54*        GO TO 6001-OUTPUT-ZERO-X
      *    ELSE
      *        MOVE LAPUP-CLI-ID (L0280-OUTPUT)
      *                            TO LAPUP-BENE-CLI-ID (WS-SUB)
      *        MOVE WAPUP-C-YES    TO LAPUP-REC-CHANGED-SW
APEX54*        GO TO 6001-OUTPUT-ZERO-X
      *    END-IF.
      *
APEX54*6001-OUTPUT-ZERO-X.
      *    EXIT.
      *
      *-------------------------
      *7000-PROCESS-FIELD-FIELD.
      *-------------------------
      *
      *    IF  LAPUP-INPUT-DATA NOT = '1'
      *        GO TO 7000-PROCESS-FIELD-FIELD-X
      *    END-IF.
      *
      *    MOVE WAPUP-C-UNKNOWN-FIELD-ERR  TO LAPUP-RETURN-CD.
      *
      *7000-PROCESS-FIELD-FIELD-X.
      *    EXIT.
      *
      *****************************************************************
      * PROCESSING COPYBOOKS                                          *
      *****************************************************************
      *
      *COPY ACPPUTTB.
      *
      *COPY XCPL0280.
      *
      *COPY ACPNUTTB.
      *
      *COPY XCPL0030.
      *
      *****************************************************************
      **                 END OF PROGRAM ASRUBENE                     **
      *****************************************************************
