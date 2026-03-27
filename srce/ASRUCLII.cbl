      *************************
       IDENTIFICATION DIVISION.
      *************************
 
       PROGRAM-ID.  ASRUCLII.
 
       COPY XCWWCRHT.
 
      *****************************************************************
      **  MEMBER :  ASRUCLII                                         **
      **  REMARKS:  APEX UPLOAD CLII TABLE PROCESSING                **
      **                                                             **
      **  DOMAIN :  CL                                               **
      **  CLASS  :  FD                                               **
      *****************************************************************
      **  DATE     AUTH.  DESCRIPTION                                **
      **                                                             **
557700**  30SEP97  KLE    APEX UPLOAD                                **
014590**  15DEC99  60     ARCHITECTURAL CHANGES                      **
      *****************************************************************
 
      **********************
       ENVIRONMENT DIVISION.
      **********************
 
      ***************
       DATA DIVISION.
      ***************
 
       WORKING-STORAGE SECTION.
 
       COPY XCWWPGWS REPLACING '$VAR1' BY 'ASRUCLII'.
 
       COPY SQLCA.
 
014590*COPY XCWL0030.
      /
      *****************************************************************
      *  COMMON COPYBOOKS
      *****************************************************************
       COPY ACWWAPUP.
      /
      *****************************************************************
      *  CALLED MODULE PARAMETER INFORMATION
      *****************************************************************
       COPY XCWL0280.
      /
      *****************
       LINKAGE SECTION.
      *****************
       01  WGLOB-GLOBAL-AREA.
       COPY XCWWGLOB.
      /
       COPY ACWLAPUP.
       COPY ACFRUFLD.
       COPY CCFRCLII.
      /
       PROCEDURE DIVISION  USING  WGLOB-GLOBAL-AREA
                                  LAPUP-PARM-AREA
                                  RUFLD-REC-INFO
                                  RCLII-REC-INFO.
 
      *--------------
       0000-MAINLINE.
      *--------------
 
           PERFORM  1000-INITIALIZE
               THRU 1000-INITIALIZE-X.
 
           EVALUATE TRUE
 
               WHEN RUFLD-UPLD-FLD-TYP-NUMERIC
               WHEN RUFLD-UPLD-FLD-TYP-DOLLAR-AMT
                    PERFORM  1000-PROCESS-NUMERIC-FIELD
                        THRU 1000-PROCESS-NUMERIC-FIELD-X
 
               WHEN RUFLD-UPLD-FLD-TYP-COMPLEX
                    PERFORM  2000-PROCESS-COMPLEX-FIELD
                        THRU 2000-PROCESS-COMPLEX-FIELD-X
 
               WHEN RUFLD-UPLD-FLD-TYP-TRANS-NAME
               WHEN RUFLD-UPLD-FLD-TYP-UPPER-CASE
               WHEN RUFLD-UPLD-FLD-TYP-MIX-CASE
               WHEN RUFLD-UPLD-FLD-TYP-DATE
               WHEN RUFLD-UPLD-FLD-TYP-TRANS-VALU
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
       1000-PROCESS-NUMERIC-FIELD.
      *---------------------------
 
           IF  RUFLD-UPLD-FLD-NM = 'CLI_EARN_INCM_AMT'
               MOVE 7                        TO L0280-LENGTH
               MOVE 2                        TO L0280-PRECISION
               MOVE LAPUP-INPUT-DATA         TO L0280-INPUT-DATA
               PERFORM  0280-1000-NUMERIC-EDIT
                   THRU 0280-1000-NUMERIC-EDIT-X
               IF  L0280-OK
                   COMPUTE RCLII-CLI-EARN-INCM-AMT = L0280-OUTPUT /
                       (10 ** L0280-PRECISION)
                   MOVE 'A'                  TO RCLII-EARN-INCM-MODE-CD
                   MOVE WGLOB-PROCESS-DATE   TO RCLII-CLI-INCM-EFF-DT
                   MOVE WAPUP-C-YES          TO LAPUP-REC-CHANGED-SW
                   GO TO 1000-PROCESS-NUMERIC-FIELD-X
               ELSE
                   MOVE WAPUP-C-NUM-CONV-ERR TO LAPUP-RETURN-CD
                   MOVE L0280-STATUS         TO LAPUP-SUB-RETURN-CD
                   GO TO 1000-PROCESS-NUMERIC-FIELD-X
               END-IF
           END-IF.
 
           IF  RUFLD-UPLD-FLD-NM = 'CLI_NET_WRTH_AMT'
               MOVE 9                        TO L0280-LENGTH
               MOVE 2                        TO L0280-PRECISION
               MOVE LAPUP-INPUT-DATA         TO L0280-INPUT-DATA
               PERFORM  0280-1000-NUMERIC-EDIT
                   THRU 0280-1000-NUMERIC-EDIT-X
               IF  L0280-OK
                   COMPUTE RCLII-CLI-NET-WRTH-AMT = L0280-OUTPUT /
                       (10 ** L0280-PRECISION)
                   MOVE WGLOB-PROCESS-DATE   TO RCLII-CLI-INCM-EFF-DT
                   MOVE WAPUP-C-YES          TO LAPUP-REC-CHANGED-SW
                   GO TO 1000-PROCESS-NUMERIC-FIELD-X
               ELSE
                   MOVE WAPUP-C-NUM-CONV-ERR TO LAPUP-RETURN-CD
                   MOVE L0280-STATUS         TO LAPUP-SUB-RETURN-CD
                   GO TO 1000-PROCESS-NUMERIC-FIELD-X
               END-IF
           END-IF.
 
           IF  RUFLD-UPLD-FLD-NM = 'CLI_OTHR_INCM_AMT'
               MOVE 7                        TO L0280-LENGTH
               MOVE 2                        TO L0280-PRECISION
               MOVE LAPUP-INPUT-DATA         TO L0280-INPUT-DATA
               PERFORM  0280-1000-NUMERIC-EDIT
                   THRU 0280-1000-NUMERIC-EDIT-X
               IF  L0280-OK
                   COMPUTE RCLII-CLI-OTHR-INCM-AMT = L0280-OUTPUT /
                       (10 ** L0280-PRECISION)
                   MOVE 'A'                  TO RCLII-OTHR-INCM-MODE-CD
                   MOVE WGLOB-PROCESS-DATE   TO RCLII-CLI-INCM-EFF-DT
                   MOVE WAPUP-C-YES          TO LAPUP-REC-CHANGED-SW
                   GO TO 1000-PROCESS-NUMERIC-FIELD-X
               ELSE
                   MOVE WAPUP-C-NUM-CONV-ERR TO LAPUP-RETURN-CD
                   MOVE L0280-STATUS         TO LAPUP-SUB-RETURN-CD
                   GO TO 1000-PROCESS-NUMERIC-FIELD-X
               END-IF
           END-IF.
 
       1000-PROCESS-NUMERIC-FIELD-X.
           EXIT.
      /
      *---------------------------
       2000-PROCESS-COMPLEX-FIELD.
      *---------------------------
 
           IF  RUFLD-UPLD-FLD-NM = 'CLI_OTHR_INCM_AMT'
               MOVE 9                        TO L0280-LENGTH
               MOVE 2                        TO L0280-PRECISION
               MOVE LAPUP-INPUT-DATA         TO L0280-INPUT-DATA
               PERFORM  0280-1000-NUMERIC-EDIT
                   THRU 0280-1000-NUMERIC-EDIT-X
               PERFORM  2010-CLI-OTHR-INCM-AMT
                   THRU 2010-CLI-OTHR-INCM-AMT-X
               GO TO 2000-PROCESS-COMPLEX-FIELD-X
           END-IF.
 
       2000-PROCESS-COMPLEX-FIELD-X.
           EXIT.
      /
      *-----------------------
       2010-CLI-OTHR-INCM-AMT.
      *-----------------------
 
           IF  L0280-OK
               MOVE WAPUP-C-YES              TO LAPUP-REC-CHANGED-SW
               MOVE 'A'                      TO RCLII-OTHR-INCM-MODE-CD
               MOVE WGLOB-PROCESS-DATE       TO RCLII-CLI-INCM-EFF-DT
               IF  L0280-OUTPUT-DOLLAR > RCLII-CLI-EARN-INCM-AMT
                   COMPUTE RCLII-CLI-OTHR-INCM-AMT
                         = L0280-OUTPUT-DOLLAR
                         - RCLII-CLI-EARN-INCM-AMT
               ELSE
                   MOVE ZERO                 TO RCLII-CLI-OTHR-INCM-AMT
               END-IF
           ELSE
               MOVE WAPUP-C-NUM-CONV-ERR     TO LAPUP-RETURN-CD
               MOVE L0280-STATUS             TO LAPUP-SUB-RETURN-CD
           END-IF.
 
       2010-CLI-OTHR-INCM-AMT-X.
           EXIT.
 
      /
      *****************************************************************
      *  PROCESSING COPYBOOKS
      *****************************************************************
      /
      *****************************************************************
      *  LINKAGE PROCESSING COPYBOOKS
      *****************************************************************
       COPY XCPL0280.
      /
      *****************************************************************
      *  ERROR HANDLING ROUTINES
      *****************************************************************
       COPY XCPL0030.
 
      *****************************************************************
      **                 END OF PROGRAM ASRUCLII                     **
      *****************************************************************
