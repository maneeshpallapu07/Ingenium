      *************************
       IDENTIFICATION DIVISION.
      *************************
 
       PROGRAM-ID. ASBM2120.
 
       COPY XCWWCRHT.
 
      *****************************************************************
      **  MEMBER :  ASBM2120                                         **
      **  REMARKS:  PRINT IMPORT MESSAGES REPORT                     **
      **                                                             **
      **  DOMAIN :  PO                                               **
      **  CLASS  :  PD                                               **
      *****************************************************************
      **  DATE     AUTH.  DESCRIPTION                                **
      **                                                             **
      **  31JUL90   NBS    INITIAL NBS/APPS UPLOAD SYSTEM DESIGN     **
APEX52**  30NOV94   JJS    UPGRADE TO RELEASE 5.2,                   **
APEX52**                   COMBINED MESSAGES INTO ONE LINE           **
APEX53**  30NOV95   JJS    UPGRADE TO INGENIUM 5.3 & WINAPEX 1.0,    **
APEX53**                   ADD WORKING STORAGE COPYBOOK XCWWPGWS,    **
APEX53**                   CHANGES TO SUPPORT I/O PROGRAMS,          **
APEX53**                   SEPARATE GLOBAL AREA AND MIR              **
APEX54**  31DEC96  TJS     MODIFICATIONS FOR MAINTAINABILITY         **
007684**  30OCT98  56     CHECKPOINT/RESTART                         **
014590**  15DEC99  60     ARCHITECTURAL CHANGES                      **
01NB01**  31AUG01  DPK    ADDED BRANCH CODE ON REPORT                **
P00045**  26OCT01  BMB    CORRECT CKPT RESTART LOGIC IN ASBM9400     **
P00045**                  MAKE SEQUENTIAL FILES, SEQUENTIAL TABLES   ** 
P02229**  20OCT04  CY     ADD COMMIT LOGIC TO MAINLINES              **
      *****************************************************************
 
      **********************
       ENVIRONMENT DIVISION.
      **********************
 
       CONFIGURATION SECTION.
 
       INPUT-OUTPUT SECTION.
 
       FILE-CONTROL.
 
       COPY NCSSPRT1.
P00045 COPY ACSS2120.
 
      ***************
       DATA DIVISION.
      ***************
 
       FILE SECTION.
 
       COPY NCSDPRT1.
       COPY NCSRPRT1.
       
P00045 COPY ACSD2120.
P00045 COPY ACSR2120.
      /
       WORKING-STORAGE SECTION.
APEX53 COPY XCWWPGWS REPLACING '$VAR1' BY 'ASBM2120'.
 
       COPY SQLCA.
 
014590*COPY XCWL0030.
 
       01  CONSTANTS.
           05  WS-HOLD-DATA-REC            PIC X(133).
 
       01  WS-SWITCHES.
           05  WS-PRINT-COMPANY-SW         PIC X.
               88  WS-PRT-COMPANY-BREAK            VALUE 'Y'.
               88  WS-PRT-NO-COMPANY-BREAK         VALUE 'N'.
           05  WS-PRINT-FINAL-BREAK-SW     PIC X   VALUE 'N'.
               88  WS-PRT-FINAL-BREAK              VALUE 'Y'.
               88  WS-PRT-NOT-FINAL-BREAK          VALUE 'N'.
           05  WS-FIRST-PAGE-SW            PIC X   VALUE 'Y'.
               88  WS-FIRST-PAGE                   VALUE 'Y'.
               88  WS-NOT-FIRST-PAGE               VALUE 'N'.
 
       01  WS-COUNTERS.
           05  WS-PAGE-CNTR                PIC S9(04) COMP VALUE ZEROS.
           05  WS-LINE-CNTR                PIC S9(04) COMP VALUE ZEROS.
APEX52     05  WS-MAX-LINES                PIC S9(04) COMP VALUE +50.
           05  WS-RECORDS-IN-COMPANY       PIC S9(04) COMP VALUE ZEROS.
           05  WS-RECORDS-IN-TOTAL         PIC S9(04) COMP VALUE ZEROS.
           05  WS-COMPANY-CNTR             PIC S9(04) COMP VALUE ZEROS.
 
       01  WS-PROGRAM-WORK-AREA.
           05  WS-COMPANY-CODE             PIC X(02).
           05  WS-POLICY-ID                PIC X(10).
 
       01  WS-PRINT-LINE.
           05  FILLER                      PIC X(01).
           05  WS-P1-COMPANY-CODE          PIC X(02).
           05  FILLER                      PIC X(01).
           05  WS-P1-POLICY-ID             PIC X(10).
01NB01     05  FILLER                      PIC X(01).
01NB01     05  WS-P1-BRANCH-CODE           PIC X(05).
           05  FILLER                      PIC X(01).
           05  WS-P1-MESSAGE-NUMBER        PIC X(10).
           05  FILLER                      PIC X(01).
           05  WS-P1-MESSAGE-DATA          PIC X(100).
           05  FILLER                      PIC X(07).
 
       01  WS-REPORT-LINE.
           05  FILLER                      PIC X(01)  VALUE SPACES.
           05  WS-HEADING                  PIC X(132).
      /
P02229 COPY XCWL0035.

       COPY XCWTFCMD.
      /
       COPY XCWWHDG.
      /
       COPY XCWWWKDT.
       COPY XCWWTIME.
      /
       COPY ACSW2120.
P00045*COPY ACSR2120.
      /
       COPY XCFWMSAU.
 
       COPY XCFRMSAU.
      /
       COPY XCFWMSGS.
       COPY XCFRMSGS.
      /
       COPY XCSWOCF.
       COPY XCSROCF.
      /
       COPY CCFWEDIT.
       COPY CCFREDIT.
      /
       COPY CCFWCLI.
       COPY CCFRCLI.
       COPY CCFHCLI.
      /
       COPY CCFWCLIA.
       COPY CCFRCLIA.
      /
       COPY NCSWPRT1.
      /
       COPY NCWL0960.
       COPY XCWL0040.
       COPY XCWL1640.
       COPY XCWLDTLK.
      /
       01  WGLOB-GLOBAL-AREA.
       COPY XCWWGLOB.
      /
      ********************
       PROCEDURE DIVISION.
      ********************
 
      *--------------
       0000-MAINLINE.
      *--------------
 
           PERFORM  1000-OPEN-FILES
               THRU 1000-OPEN-FILES-X.
 
           PERFORM  2000-INITIALIZE-FIELDS
               THRU 2000-INITIALIZE-FIELDS-X.
 
           PERFORM  3000-PROCESS-TRANSACTIONS
               THRU 3000-PROCESS-TRANSACTIONS-X.
 
      *
      * PRINT THE GRAND TOTALS
      *
           SET  WS-PRT-NO-COMPANY-BREAK    TO TRUE.
           SET  WS-PRT-FINAL-BREAK         TO TRUE.
 
           PERFORM  8000-PRINT-TOTALS
               THRU 8000-PRINT-TOTALS-X.
 
           PERFORM  9000-CLOSE-FILES
               THRU 9000-CLOSE-FILES-X.
 
P02229     PERFORM  0035-1000-COMMIT
P02229         THRU 0035-1000-COMMIT-X.

           STOP RUN.
 
       0000-MAINLINE-X.
           EXIT.
      /
      *----------------
       1000-OPEN-FILES.
      *----------------
 
      *
      * OPEN ALL FILES
      *
 
           PERFORM  OCF-3000-OPEN-OUTPUT
               THRU OCF-3000-OPEN-OUTPUT-X.

P00045*    PERFORM  2120-1000-OPEN-INPUT
P00045*        THRU 2120-1000-OPEN-INPUT-X.
 
P00045     PERFORM  2120-3000-OPEN-INPUT
P00045         THRU 2120-3000-OPEN-INPUT-X.
 
           PERFORM  PRT1-3000-OPEN-OUTPUT
               THRU PRT1-3000-OPEN-OUTPUT-X.
 
       1000-OPEN-FILES-X.
           EXIT.
      /
      *-----------------------
       2000-INITIALIZE-FIELDS.
      *-----------------------
 
      *
      * INITIALIZE GLOBAL AREA AND CONTROL REPORT TITLES TO
      * DEFAULTS
      *
 
APEX53     MOVE WPGWS-CRNT-PGM-ID          TO L0960-PROGRAM-ID.
 
           PERFORM  0960-2000-INIT-DEFAULT
               THRU 0960-2000-INIT-DEFAULT-X.
 
           SET  WS-PRT-NO-COMPANY-BREAK    TO TRUE.
           MOVE SPACES                     TO WS-PRINT-LINE.
 
           PERFORM  7000-INIT-RUN-TITLES
               THRU 7000-INIT-RUN-TITLES-X.
 
       2000-INITIALIZE-FIELDS-X.
           EXIT.
      /
      *--------------------------
       3000-PROCESS-TRANSACTIONS.
      *--------------------------
 
      *
      * PROCESS TRANSACTIONS - SEQUENTIALLY READ THE IMPORT AUDIT
      * EXTRACT FILE AND PRINT EACH RECORD, BREAKING ON COMPANY
      *
 
           SET  WS-PRT-COMPANY-BREAK       TO TRUE.
 
           PERFORM  2120-1000-READ
               THRU 2120-1000-READ-X.
 
           PERFORM  4000-PROCESS-COMPANY
               THRU 4000-PROCESS-COMPANY-X
007684         UNTIL W2120-SEQ-IO-EOF.
 
       3000-PROCESS-TRANSACTIONS-X.
           EXIT.
      /
      *---------------------
       4000-PROCESS-COMPANY.
      *---------------------
 
      *
      * PROCESS EACH COMPANY'S AUDIT POLICY RECORDS
      *
 
      *
      * INITIALIZE GLOBAL AREA AND CONTROL REPORT FOR COMPANY
      *
           MOVE R2120-COMPANY-CODE         TO L0960-COMPANY-CODE.
 
           PERFORM  0960-3000-INIT-COMPANY
               THRU 0960-3000-INIT-COMPANY-X.
 
           PERFORM  7000-INIT-RUN-TITLES
               THRU 7000-INIT-RUN-TITLES-X.
 
           MOVE R2120-COMPANY-CODE         TO WS-COMPANY-CODE.
 
           PERFORM  4100-PROCESS-EXTRACTS
               THRU 4100-PROCESS-EXTRACTS-X
007684         UNTIL W2120-SEQ-IO-EOF
               OR    R2120-COMPANY-CODE NOT = WS-COMPANY-CODE.
 
           PERFORM  8000-PRINT-TOTALS
               THRU 8000-PRINT-TOTALS-X.
 
       4000-PROCESS-COMPANY-X.
           EXIT.
      /
      *----------------------
       4100-PROCESS-EXTRACTS.
      *----------------------
 
      *
      * PRINT EXTRACT RECORDS
      *
 
           ADD +1                          TO WS-RECORDS-IN-COMPANY.
 
           IF  R2120-POLICY-ID         NOT = WS-POLICY-ID
               MOVE +1                     TO WPRT1-NUMBER-LINES
               MOVE SPACES                 TO WS-PRINT-LINE
               PERFORM  8300-PRINT-LINE
                   THRU 8300-PRINT-LINE-X
           END-IF.
 
           MOVE R2120-POLICY-ID            TO WS-POLICY-ID.
 
           MOVE R2120-COMPANY-CODE         TO WS-P1-COMPANY-CODE.
           MOVE R2120-POLICY-ID            TO WS-P1-POLICY-ID.
01NB01     IF R2120-BRANCH-CODE NOT = SPACES
01NB01        MOVE R2120-BRANCH-CODE       TO WS-P1-BRANCH-CODE
01NB01     END-IF.
           MOVE R2120-MESSAGE-NUMBER       TO WS-P1-MESSAGE-NUMBER.
           MOVE R2120-MESSAGE-DATA         TO WS-P1-MESSAGE-DATA.
 
           MOVE +1                         TO WPRT1-NUMBER-LINES.
 
           PERFORM  8300-PRINT-LINE
               THRU 8300-PRINT-LINE-X.
 
           PERFORM  2120-1000-READ
               THRU 2120-1000-READ-X.
 
       4100-PROCESS-EXTRACTS-X.
           EXIT.
      /
      *---------------------
       7000-INIT-RUN-TITLES.
      *---------------------
 
      *
      * INITIALIZE RUN MESSAGES CONTROL REPORT TITLES
      *
           PERFORM  7900-INIT-COMMON-TITLES
               THRU 7900-INIT-COMMON-TITLES-X.
 
           IF  WS-PRT-NO-COMPANY-BREAK
      *MSG: RUN MESSAGES
               MOVE 'XS00000153'           TO WGLOB-MSG-REF-INFO
           ELSE
      *MSG: COMPANY RUN MESSAGES
               MOVE 'XS00000161'           TO WGLOB-MSG-REF-INFO
           END-IF.
 
           PERFORM  0260-1000-GENERATE-MESSAGE
               THRU 0260-1000-GENERATE-MESSAGE-X.
 
           MOVE WGLOB-MSG-TXT              TO L0040-HDG-LINE-3.
 
           PERFORM  0040-1000-INIT-TITLE
               THRU 0040-1000-INIT-TITLE-X.
 
      *
      * INITIALIZE IMPORT MESSAGES TITLES
      *
 
           IF  WS-PRT-NO-COMPANY-BREAK
               GO TO 7000-INIT-RUN-TITLES-X
           END-IF.
 
           MOVE SPACES                     TO WHDG-LINE-1.
           MOVE SPACES                     TO WHDG-LINE-2.
           MOVE SPACES                     TO WS-REPORT-LINE.
 
           PERFORM  HDG-1000-INIT-CONSTANTS
               THRU HDG-1000-INIT-CONSTANTS-X.
 
           MOVE L0040-SYSTEM-ID            TO WHDG-SYSTEM-ID.
           MOVE L0960-COMPANY-NAME         TO WHDG-COMPANY-NAME.
 
APEX53     MOVE WGLOB-MAIN-PGM-ID          TO WHDG-PROGRAM-ID.
 
           MOVE WGLOB-SYSTEM-DATE-INT      TO L1640-INTERNAL-DATE.
 
           PERFORM  1640-2000-INTERNAL-TO-EXT
               THRU 1640-2000-INTERNAL-TO-EXT-X.
 
           MOVE L1640-EXTERNAL-DATE        TO WHDG-DATE.
 
           MOVE WGLOB-SYSTEM-TIME          TO WTIME-EDIT-TIME.
 
           PERFORM  TIME-1000-REFORMAT
               THRU TIME-1000-REFORMAT-X.
 
           MOVE WTIME-DISPLAY-TIME         TO WHDG-TIME.
 
      *
      * GET THE PROGRAM DESCRIPTION
      *
           MOVE 'AS21200001'               TO WGLOB-MSG-REF-INFO.
      *MSG: IMPORT MESSAGES PRINT
 
           PERFORM  0260-1000-GENERATE-MESSAGE
               THRU 0260-1000-GENERATE-MESSAGE-X.
 
           MOVE WGLOB-MSG-TXT              TO WHDG-REPORT-TITLE.
 
           MOVE 'AS21200004'               TO WGLOB-MSG-REF-INFO.
      *MSG:  POLICY-NO  MESSAGE #  MESSAGE
 
           PERFORM  0260-1000-GENERATE-MESSAGE
               THRU 0260-1000-GENERATE-MESSAGE-X.
 
           MOVE WGLOB-MSG-TXT              TO WS-HEADING.
 
           MOVE +99                        TO WS-LINE-CNTR.
           MOVE ZERO                       TO WS-PAGE-CNTR.
 
       7000-INIT-RUN-TITLES-X.
           EXIT.
      /
      *------------------------
       7900-INIT-COMMON-TITLES.
      *------------------------
 
      *
      * INITIALIZE FIELDS COMMON TO BOTH RUN AND TOTAL CONTROL
      * REPORTS
      *
 
      *
      * SET UP THE COMPANY NAME
      *
           MOVE L0960-COMPANY-NAME         TO L0040-COMPANY-NAME.
 
      *
      * GET THE PROGRAM DESCRIPTION
      *
           MOVE 'AS21200001'               TO WGLOB-MSG-REF-INFO.
      *MSG: IMPORT MESSAGES PRINT
 
           PERFORM  0260-1000-GENERATE-MESSAGE
               THRU 0260-1000-GENERATE-MESSAGE-X.
 
           MOVE WGLOB-MSG-TXT              TO L0040-PROGRAM-DESC.
 
      *
      * GET THE SYSTEM DESCRIPTION
      *
           MOVE 'XS00000145'               TO WGLOB-MSG-REF-INFO.
      *MSG: INGENIUM 5.4
 
           PERFORM  0260-1000-GENERATE-MESSAGE
               THRU 0260-1000-GENERATE-MESSAGE-X.
 
           MOVE WGLOB-MSG-TXT              TO L0040-SYSTEM-ID.
 
       7900-INIT-COMMON-TITLES-X.
           EXIT.
      /
      *------------------
       8000-PRINT-TOTALS.
      *------------------
 
           IF  WS-PRT-NO-COMPANY-BREAK
               MOVE WPGWS-CRNT-PGM-ID      TO L0960-PROGRAM-ID
               PERFORM  0960-4000-INIT-DEFAULT-COMP
                   THRU 0960-4000-INIT-DEFAULT-COMP-X
           END-IF.
 
      *MSG: EXTRACT RECORDS READ
           IF  WS-PRT-COMPANY-BREAK
               MOVE 'AS21200002'           TO WGLOB-MSG-REF-INFO
               MOVE WS-RECORDS-IN-COMPANY  TO WGLOB-MSG-PARM (1)
           ELSE
               MOVE 'AS21200003'           TO WGLOB-MSG-REF-INFO
               MOVE WS-RECORDS-IN-TOTAL    TO WGLOB-MSG-PARM (1)
           END-IF.
 
           PERFORM  0260-1000-GENERATE-MESSAGE
               THRU 0260-1000-GENERATE-MESSAGE-X.
 
           MOVE WGLOB-MSG-TXT              TO L0040-INPUT-LINE.
 
           PERFORM  0040-3000-WRITE-OTHER
               THRU 0040-3000-WRITE-OTHER-X.
 
           IF  WS-PRT-COMPANY-BREAK
               ADD WS-RECORDS-IN-COMPANY   TO WS-RECORDS-IN-TOTAL
               MOVE ZEROS                  TO WS-RECORDS-IN-COMPANY
           END-IF.
 
       8000-PRINT-TOTALS-X.
           EXIT.
      /
      *--------------------
       8200-PRINT-HEADINGS.
      *--------------------
 
      *
      * INITIALIZE REMAINING FIELDS IN TOP 2 HEADING LINES AND PRINT
      *
 
           ADD +1                          TO WS-PAGE-CNTR.
           MOVE WS-PAGE-CNTR               TO WHDG-PAGE.
007684     MOVE WHDG-LINE-1                TO RPRT1-SEQ-REC-INFO.
 
           PERFORM  PRT1-2000-WRITE
               THRU PRT1-2000-WRITE-X.
 
           MOVE +1                         TO WPRT1-NUMBER-LINES.
007684     MOVE WHDG-LINE-2                TO RPRT1-SEQ-REC-INFO.
 
           PERFORM  PRT1-1000-WRITE
               THRU PRT1-1000-WRITE-X.
 
           MOVE +1                         TO WPRT1-NUMBER-LINES.
007684     MOVE SPACES                     TO RPRT1-SEQ-REC-INFO.
 
           PERFORM  PRT1-1000-WRITE
               THRU PRT1-1000-WRITE-X.
 
           MOVE +2                         TO WPRT1-NUMBER-LINES.
007684     MOVE WS-REPORT-LINE             TO RPRT1-SEQ-REC-INFO.
 
           PERFORM  PRT1-1000-WRITE
               THRU PRT1-1000-WRITE-X.
 
           MOVE +1                         TO WPRT1-NUMBER-LINES.
007684     MOVE SPACES                     TO RPRT1-SEQ-REC-INFO.
 
           PERFORM  PRT1-1000-WRITE
               THRU PRT1-1000-WRITE-X.
 
           MOVE ZERO                       TO WS-LINE-CNTR.
 
       8200-PRINT-HEADINGS-X.
           EXIT.
      /
      *----------------
       8300-PRINT-LINE.
      *----------------
 
      *
      * PRINT A LINE ON THE REPORT
      *
           IF  WS-LINE-CNTR                > WS-MAX-LINES
               PERFORM  8200-PRINT-HEADINGS
                   THRU 8200-PRINT-HEADINGS-X
           END-IF.
 
007684     MOVE WS-PRINT-LINE              TO RPRT1-SEQ-REC-INFO.
 
           PERFORM  PRT1-1000-WRITE
               THRU PRT1-1000-WRITE-X.
 
           ADD WPRT1-NUMBER-LINES          TO WS-LINE-CNTR.
 
       8300-PRINT-LINE-X.
           EXIT.
      /
      *-----------------
       9000-CLOSE-FILES.
      *-----------------

P00045*    PERFORM  2120-4000-CLOSE
P00045*        THRU 2120-4000-CLOSE-X.
 
P00045     PERFORM  2120-5000-CLOSE
P00045         THRU 2120-5000-CLOSE-X.
 
           PERFORM  PRT1-4000-CLOSE
               THRU PRT1-4000-CLOSE-X.
 
           PERFORM  OCF-4000-CLOSE
               THRU OCF-4000-CLOSE-X.
 
       9000-CLOSE-FILES-X.
           EXIT.
      /
      ******************************************************************
      *   PROCESSING COPYBOOKS
      ******************************************************************
 
P02229 COPY XCPL0035.

       COPY NCPL0960.
      /
       COPY XCPPHDG.
      /
       COPY XCPPTIME.
      /
       COPY XCPL0040.
      /
       COPY XCPL0260.
      /
       COPY XCPL1640.
      /
      ******************************************************************
      *   FILE I/O COPYBOOKS
      ******************************************************************
 
P00045*COPY ACPL2120.
P00045*COPY ACPN2120.
P00045*COPY ACPO2120.
P00045 COPY ACPI2120.
      /
       COPY CCPNEDIT.
      /
       COPY XCPAMSAU.
       COPY XCPCMSAU.
      /
       COPY XCPNMSGS.
      /
       COPY XCPLOCF.
       COPY XCPOOCF.
      /
       COPY NCPIPRT1.
      /
       COPY CCPACLI.
       COPY CCPNCLI.
       COPY CCPUCLI.
       COPY CCPXCLI.
       COPY CCPCCLI.
      /
       COPY CCPACLIA.
       COPY CCPCCLIA.
      /
       COPY XCPL0030.
 
      *****************************************************************
      **                 END OF PROGRAM ASBM2120                     **
      *****************************************************************
