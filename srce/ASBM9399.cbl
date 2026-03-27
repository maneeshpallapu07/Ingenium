      *************************
       IDENTIFICATION DIVISION.
      *************************
       PROGRAM-ID. ASBM9399.

       COPY XCWWCRHT.

      *****************************************************************
      **  MEMBER :  ASBM9399                                         **
      **  REMARKS:  CHECK FOR DUPLICATE APP ID'S                     **
      **  DOMAIN :  UW                                               **
      **  CLASS  :  PD                                               **
      *****************************************************************
      **  DATE     AUTH.  DESCRIPTION                                **
      **                                                             **
P00697**  25OCT02  DPK    NEW PROGRAM                                **
P00697**  24FEB03  AC     FIX THE XSOCF PAGE NUMBER                  **
P02229**  20OCT04  CY     ADD COMMIT LOGIC TO MAINLINES              **
      *****************************************************************

      **********************
       ENVIRONMENT DIVISION.
      **********************

       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
        
      ***************
       DATA DIVISION.
      ***************

       FILE SECTION.
        
       WORKING-STORAGE SECTION.

       COPY XCWWPGWS REPLACING '$VAR1' BY 'ASBM9399'.

       COPY SQLCA.

       01  WS-SWITCHES.
           05  WS-FATAL-ERROR-SW             PIC X.
               88  WS-FATAL-ERROR            VALUE 'Y'.
           05  WS-PROGRAM-ID-FOUND-SW        PIC X.
               88  WS-PROGRAM-ID-FOUND       VALUE 'Y'.
           05  WS-COMPANY-FOUND-SW           PIC X.
               88  WS-COMPANY-FOUND          VALUE 'Y'.
           05  WS-RUN-DATE-FOUND-SW          PIC X.
               88  WS-RUN-DATE-FOUND         VALUE 'Y'.
           05  WS-PROCESS-DATE-FOUND-SW      PIC X.
               88  WS-PROCESS-DATE-FOUND     VALUE 'Y'.
           05  WS-ALL-COMPANY-CARDS-READ-SW  PIC X.
               88  WS-ALL-COMPANY-CARDS-READ VALUE 'Y'.
           05  WS-NO-MORE-DUPLICATES-SW      PIC X.
               88  WS-NO-MORE-DUPLICATES     VALUE 'Y'.
                
       01  WS-COUNTERS.
           05  WS-RECORDS-IN-APP           PIC 9(07)  VALUE ZEROS.
           05  WS-PARM-CARD-COUNTER        PIC 9(07)  VALUE ZEROS.
           05  WS-APP-REC-COUNTER          PIC 9(07)  VALUE ZEROS.
           05  WS-DUP-APP-REC-COUNTER      PIC 9(07)  VALUE ZEROS.
           05  WS-PIC-COUNTER              PIC Z(06)9.
                
       01  WS-PGM-WORK-AREA.
           05  WS-PCOM-CO-AUD-CTR-LOB-CD   PIC X(01).
           05  WS-USER-ID                  PIC X(08)  VALUE 'MANUFLEX'.
           05  WS-APP-ID                   PIC X(15).
           05  WS-COMPANY-CODE             PIC X(02).
           05  WS-DUPLICATE-APP-ID         PIC X(15).
           05  WS-DUPLICATE-APP-ID-R REDEFINES WS-DUPLICATE-APP-ID.
               10  WS-DUP-APP-SALES-OFFICE PIC X(03).
               10  WS-DUP-APP-REST         PIC X(12).
 
           05  WS-DUP-APP-ID-SO            PIC X(03).
           05  WS-DUP-APP-ID-SO-R REDEFINES WS-DUP-APP-ID-SO.
               10  WS-DUP-APP-1                PIC 9.
               10  WS-DUP-APP-2-3              PIC 99.

      /
P02229 COPY XCWL0035.

       COPY ACWWAPIN.
       COPY XCWL0040.
       COPY NCWL0960.
       COPY XCWL0290.
       COPY NCWWPARM.
       COPY XCWTFCMD.
      /
       COPY CCFWPCOM.
       COPY CCFRPCOM.
       COPY CCFWMAST.
       COPY CCFRMAST.
       COPY ACWLUPOL.
       COPY ACFWUPOL.
       COPY ACFRUPOL.
      /
       COPY XCSWBCF.
       COPY XCSRBCF.
       COPY XCSWOCF.
       COPY XCSROCF.
      /
       COPY ACSW9399.
       COPY ACSR9399.
       COPY ACSRADUP.
       COPY ACSWADUP.
      *****************************************************************
      * INPUT PARAMETER INFORMATION                                   *
      *****************************************************************

       01  WGLOB-GLOBAL-AREA.
       COPY XCWWGLOB.
      /
       PROCEDURE DIVISION.

      *--------------
       0000-MAINLINE.
      *--------------

           PERFORM  0100-OPEN-FILES
               THRU 0100-OPEN-FILES-X.

           PERFORM  0200-INITIALIZE
               THRU 0200-INITIALIZE-X.

           PERFORM  1000-PROCESS-TRANSACTIONS
               THRU 1000-PROCESS-TRANSACTIONS-X.

      * READ THE FIRST APPLICATION FROM THE INPUT FILE

           PERFORM 4000-READ-APP-ID
              THRU 4000-READ-APP-ID-X.

           PERFORM 3000-PROCESS-EACH-APP
              THRU 3000-PROCESS-EACH-APP-X
              UNTIL WAPIN-NO-MORE-APPS.

           PERFORM  5000-PRINT-GRAND-TOTALS
               THRU 5000-PRINT-GRAND-TOTALS-X.

           PERFORM  9999-CLOSE-FILES
               THRU 9999-CLOSE-FILES-X.

P02229     PERFORM  0035-1000-COMMIT
P02229         THRU 0035-1000-COMMIT-X.

           STOP RUN.

       0000-MAINLINE-X.
           EXIT.
      /
      *----------------
       0100-OPEN-FILES.
      *----------------
      *
      * THESE FILES ARE REQUIRED IN ALL BATCH PROGRAMS THAT REQUIRE
      * CONTROL CARDS (I.E. EXCLUDING THE INITIALIZATION PROGRAMS)
      *

           PERFORM  OCF-5000-OPEN-EXTEND
               THRU OCF-5000-OPEN-EXTEND-X.

           PERFORM  BCF-1000-OPEN-INPUT
               THRU BCF-1000-OPEN-INPUT-X.

           PERFORM  9399-1000-OPEN-INPUT
              THRU  9399-1000-OPEN-INPUT-X.
               
           PERFORM  ADUP-3000-OPEN-OUTPUT
              THRU  ADUP-3000-OPEN-OUTPUT-X.

       0100-OPEN-FILES-X.
           EXIT.
      /
      *----------------
       0200-INITIALIZE.
      *----------------

           MOVE WPGWS-CRNT-PGM-ID      TO L0960-PROGRAM-ID.
           MOVE SPACES                 TO L0960-COMPANY-CODE.

           MOVE ZERO                   TO WS-APP-REC-COUNTER.
           MOVE ZERO                   TO WS-DUP-APP-REC-COUNTER.
           MOVE SPACES                 TO WS-DUPLICATE-APP-ID.
           MOVE SPACES                 TO WS-DUP-APP-ID-SO.

           PERFORM  0960-2000-INIT-DEFAULT
               THRU 0960-2000-INIT-DEFAULT-X.

           MOVE WS-USER-ID             TO WGLOB-USER-ID.

           PERFORM  6000-INIT-TITLES
               THRU 6000-INIT-TITLES-X.

       0200-INITIALIZE-X.
           EXIT.
      /
      *--------------------------
       1000-PROCESS-TRANSACTIONS.
      *--------------------------
      *
      * PROCESS TRANSACTIONS - EDIT CONTROL CARDS AND, IF NO ERRORS
      * ARE FOUND, PROCESS THE TRANS FILE FOR EACH COMPANY INPUT
      *
           PERFORM  1100-EDIT-CONTROL-CARDS
               THRU 1100-EDIT-CONTROL-CARDS-X.

           IF  WS-FATAL-ERROR
               GO TO 1000-PROCESS-TRANSACTIONS-X
           END-IF.
      *
      * REPOSITION CONTROL CARD FILE AT THE BEGINNING
      *
           PERFORM  BCF-4000-CLOSE
               THRU BCF-4000-CLOSE-X.

           PERFORM  BCF-1000-OPEN-INPUT
               THRU BCF-1000-OPEN-INPUT-X.

           MOVE ZERO TO WS-PARM-CARD-COUNTER.
      *
      * BYPASS THE PROGRAM ID CARD
      *
           PERFORM  BCF-1000-READ
               THRU BCF-1000-READ-X.

           ADD 1 TO WS-PARM-CARD-COUNTER.

      *
      * PROCESS THE PARM FILE FOR EACH COMPANY
      *
           PERFORM  7000-READ-CONTROL-CARD
               THRU 7000-READ-CONTROL-CARD-X.

      * ONLY ALLOW FOR PROCESSING OF ONE COMPANY.

           PERFORM  2000-PROCESS-COMPANY
              THRU  2000-PROCESS-COMPANY-X.

       1000-PROCESS-TRANSACTIONS-X.
           EXIT.
      /
      *------------------------
       1100-EDIT-CONTROL-CARDS.
      *------------------------
      *
      * PERFORM INITIAL EDITS ON CONTROL CARD FILE, THEN LOOP THRU
      * FILE AND EDIT EACH CARD
      *
           MOVE 'N'                    TO WS-FATAL-ERROR-SW.
           MOVE 'N'                    TO WS-COMPANY-FOUND-SW.
           MOVE 'N'                    TO WS-PROGRAM-ID-FOUND-SW.
           MOVE 'N'                    TO WS-RUN-DATE-FOUND-SW.
           MOVE 'N'                    TO WS-PROCESS-DATE-FOUND-SW.

           PERFORM  7000-READ-CONTROL-CARD
               THRU 7000-READ-CONTROL-CARD-X.

           IF  WBCF-SEQ-IO-EOF
               SET WS-FATAL-ERROR      TO TRUE
      *MSG: CONTROL CARD FILE EMPTY, NO PROCESSING DONE
               MOVE 'XS00000151'       TO WGLOB-MSG-REF-INFO
               PERFORM  0260-1000-GENERATE-MESSAGE
                   THRU 0260-1000-GENERATE-MESSAGE-X
               GO TO 1100-EDIT-CONTROL-CARDS-X
           END-IF.

           MOVE RBCF-SEQ-REC-INFO      TO WPARM-CARD-AREA.

           IF  NOT WPARM-PROGRAM-ID
               SET WS-FATAL-ERROR      TO TRUE
      *MSG: FIRST CONTROL CARD MUST BE PROGRAM ID CARD
               MOVE 'XS00000147'       TO WGLOB-MSG-REF-INFO
               PERFORM  0260-1000-GENERATE-MESSAGE
                   THRU 0260-1000-GENERATE-MESSAGE-X
           END-IF.

           PERFORM  1110-EDIT-CONTROL-CARD
               THRU 1110-EDIT-CONTROL-CARD-X
               UNTIL WBCF-SEQ-IO-EOF.

           IF  NOT WS-PROGRAM-ID-FOUND
               SET WS-FATAL-ERROR      TO TRUE
      *MSG: MISSING PROGRAM ID CARD
               MOVE 'XS00000122'       TO WGLOB-MSG-REF-INFO
               PERFORM  0260-1000-GENERATE-MESSAGE
                   THRU 0260-1000-GENERATE-MESSAGE-X
           END-IF.

           IF  NOT WS-COMPANY-FOUND
               SET WS-FATAL-ERROR      TO TRUE
      *MSG: COMPANY CONTROL CARD MISSING
               MOVE 'XS00000134'       TO WGLOB-MSG-REF-INFO
               PERFORM  0260-1000-GENERATE-MESSAGE
                   THRU 0260-1000-GENERATE-MESSAGE-X
           END-IF.

           IF  WGLOB-SYSTEM-DATE-INT < WGLOB-PROCESS-DATE
               SET WS-FATAL-ERROR      TO TRUE
      *MSG: SYSTEM DATE MUST BE >= PROCESS DATE
               MOVE 'AS93990001'       TO WGLOB-MSG-REF-INFO
               PERFORM  0260-1000-GENERATE-MESSAGE
                   THRU 0260-1000-GENERATE-MESSAGE-X
           END-IF.

      *
      * PRINT CONTROL CARD TOTALS
      *
      *MSG: TOTAL NUMBER OF PARM CARDS READ @1
           MOVE 'XS00000142'           TO WGLOB-MSG-REF-INFO.
           MOVE WS-PARM-CARD-COUNTER   TO WS-PIC-COUNTER.
           MOVE WS-PIC-COUNTER         TO WGLOB-MSG-PARM (1).

           PERFORM  0260-2000-GET-MESSAGE
               THRU 0260-2000-GET-MESSAGE-X.

           MOVE WGLOB-MSG-TXT          TO L0040-INPUT-LINE.

           PERFORM  0040-3000-WRITE-OTHER
               THRU 0040-3000-WRITE-OTHER-X.

           PERFORM  0040-4000-WRITE-ERROR-TOTAL
               THRU 0040-4000-WRITE-ERROR-TOTAL-X.

       1100-EDIT-CONTROL-CARDS-X.
           EXIT.
      /
      *-----------------------
       1110-EDIT-CONTROL-CARD.
      *-----------------------
      *
      * EDIT AN INDIVIDUAL CONTROL CARD
      *

      *
      * MOVE THE CONTROL CARD TO A PARAMETER LAYOUT
      *
           MOVE RBCF-SEQ-REC-INFO      TO WPARM-CARD-AREA.

           EVALUATE TRUE

               WHEN WPARM-PROGRAM-ID
                    PERFORM  1111-EDIT-PROGRAM-ID
                        THRU 1111-EDIT-PROGRAM-ID-X

               WHEN WPARM-COMPANY-CODE
                    PERFORM  1112-EDIT-COMPANY-CODE
                        THRU 1112-EDIT-COMPANY-CODE-X

               WHEN OTHER
                    SET WS-FATAL-ERROR TO TRUE
      *MSG: INVALID PARM CARD TYPE
                    MOVE 'XS00000152'  TO WGLOB-MSG-REF-INFO
                    PERFORM  0260-1000-GENERATE-MESSAGE
                        THRU 0260-1000-GENERATE-MESSAGE-X

           END-EVALUATE.

           MOVE SPACES                 TO RMAST-KEY.
           MOVE WGLOB-COMPANY-CODE     TO WMAST-CO-ID.
           PERFORM MAST-1000-READ
              THRU MAST-1000-READ-X.
           IF NOT WMAST-IO-OK
              MOVE WMAST-KEY          TO WGLOB-MSG-PARM (1)
              MOVE 'AS93990003'       TO WGLOB-MSG-REF-INFO
              PERFORM 0260-1000-GENERATE-MESSAGE
                 THRU 0260-1000-GENERATE-MESSAGE-X
              PERFORM 0030-5000-LOGIC-ERROR
                 THRU 0030-5000-LOGIC-ERROR-X
           END-IF.
           MOVE RMAST-APPL-CTL-PRCES-DT TO WGLOB-PROCESS-DATE.

      *
      * PRINT CONTROL CARD
      *
           MOVE RBCF-SEQ-REC-INFO      TO L0040-INPUT-LINE.

           PERFORM  0040-3000-WRITE-OTHER
              THRU  0040-3000-WRITE-OTHER-X.

           PERFORM  7000-READ-CONTROL-CARD
               THRU 7000-READ-CONTROL-CARD-X.

       1110-EDIT-CONTROL-CARD-X.
           EXIT.
      /
      *---------------------
       1111-EDIT-PROGRAM-ID.
      *---------------------
      *
      * EDIT PROGRAM ID AGAINST HARD CODED VALUE AND ENSURE THAT
      * THERE IS ONLY ONE
      *

           IF  WS-PROGRAM-ID-FOUND
               SET WS-FATAL-ERROR      TO TRUE
      *MSG: MUST ONLY HAVE ONE PROGRAM ID CARD
               MOVE 'XS00000162'       TO WGLOB-MSG-REF-INFO
               PERFORM  0260-1000-GENERATE-MESSAGE
                   THRU 0260-1000-GENERATE-MESSAGE-X
           END-IF.

           SET WS-PROGRAM-ID-FOUND     TO TRUE.

           IF  WPARM-VALUE = WPGWS-CRNT-PGM-ID
               NEXT SENTENCE
           ELSE
               SET WS-FATAL-ERROR      TO TRUE
      *MSG: INVALID PROGRAM ID CARD
               MOVE 'XS00000121'       TO WGLOB-MSG-REF-INFO
               PERFORM  0260-1000-GENERATE-MESSAGE
                   THRU 0260-1000-GENERATE-MESSAGE-X
           END-IF.

       1111-EDIT-PROGRAM-ID-X.
           EXIT.
      /
      *-----------------------
       1112-EDIT-COMPANY-CODE.
      *-----------------------
      *
      * EDIT COMPANY CODE CARD DEPENDING ON WHETHER PCOM
      *
      *
           MOVE 'N'                    TO WS-RUN-DATE-FOUND-SW.

           IF  WPCOM-COMPANY-NOT-REQUIRED
               IF  WS-COMPANY-FOUND
                   SET WS-FATAL-ERROR  TO TRUE
      *MSG: NO MULTI-COMPANY-CAN ONLY HAVE 1 COMPANY CARD
                   MOVE 'XS00000163'   TO WGLOB-MSG-REF-INFO
                   PERFORM  0260-1000-GENERATE-MESSAGE
                       THRU 0260-1000-GENERATE-MESSAGE-X
               END-IF
           END-IF.

           SET WS-COMPANY-FOUND        TO TRUE.

           IF  WPCOM-COMPANY-NOT-REQUIRED
               IF  WPARM-VALUE = SPACES
                   GO TO 1112-EDIT-COMPANY-CODE-X
               ELSE
                   SET WS-FATAL-ERROR  TO TRUE
      *MSG: COMPANY CODE MUST EQUAL SPACES-PCOM MULTI-COMP OFF
                   MOVE 'XS00000148'   TO WGLOB-MSG-REF-INFO
                   PERFORM  0260-1000-GENERATE-MESSAGE
                       THRU 0260-1000-GENERATE-MESSAGE-X
                   GO TO 1112-EDIT-COMPANY-CODE-X
               END-IF
           END-IF.

           MOVE WGLOB-COMPANY-CODE     TO WS-COMPANY-CODE.

           MOVE WPARM-VALUE            TO WGLOB-COMPANY-CODE.

           PERFORM  PCOM-1000-READ
               THRU PCOM-1000-READ-X.

           MOVE WS-COMPANY-CODE        TO WGLOB-COMPANY-CODE.

           IF  NOT WPCOM-IO-OK
               SET WS-FATAL-ERROR      TO TRUE
      *MSG: COMPANY CODE INVALID, NO PCOM RECORD EXISTS
               MOVE 'XS00000149'       TO WGLOB-MSG-REF-INFO
               PERFORM  0260-1000-GENERATE-MESSAGE
                   THRU 0260-1000-GENERATE-MESSAGE-X
           END-IF.

       1112-EDIT-COMPANY-CODE-X.
           EXIT.
      /
      *---------------------
       2000-PROCESS-COMPANY.
      *---------------------
      *
      * FIRST CARD MUST BE COMPANY CARD
      *
           MOVE RBCF-SEQ-REC-INFO      TO WPARM-CARD-AREA.
           MOVE WPARM-VALUE            TO WS-COMPANY-CODE.
      *
      * INITIALIZE GLOBAL AREA AND REPORTS FOR COMPANY
      *
           MOVE WS-COMPANY-CODE        TO L0960-COMPANY-CODE.

           PERFORM  0960-3000-INIT-COMPANY
               THRU 0960-3000-INIT-COMPANY-X.

           PERFORM 0290-1000-BUILD-PARM-INFO
              THRU 0290-1000-BUILD-PARM-INFO-X.

P00697*    PERFORM  6000-INIT-TITLES
P00697*        THRU 6000-INIT-TITLES-X.
      *
      * GET THE REPORT HEADING
      *

      *MSG: COMPANY RUN MESSAGES
           MOVE 'XS00000161'           TO WGLOB-MSG-REF-INFO.

           PERFORM  0260-2000-GET-MESSAGE
               THRU 0260-2000-GET-MESSAGE-X.

P00697*    MOVE WGLOB-MSG-TXT          TO L0040-HDG-LINE-3.

P00697     MOVE WGLOB-MSG-TXT          TO L0040-INPUT-LINE.
P00697
P00697     PERFORM  0040-3000-WRITE-OTHER
P00697         THRU 0040-3000-WRITE-OTHER-X.

P00697*    PERFORM  0040-1000-INIT-TITLE
P00697*        THRU 0040-1000-INIT-TITLE-X.

           PERFORM  PCOM-1000-READ
               THRU PCOM-1000-READ-X.

           IF  WPCOM-IO-OK
               MOVE RPCOM-CO-AUD-CTR-LOB-CD TO WS-PCOM-CO-AUD-CTR-LOB-CD
           ELSE
      *MSG: COMPANY PROFILE RECORD NOT FOUND (@1)
               MOVE 'AS93990002'       TO WGLOB-MSG-REF-INFO
               MOVE WGLOB-COMPANY-CODE TO WGLOB-MSG-PARM (1)
               PERFORM  0260-1000-GENERATE-MESSAGE
                   THRU 0260-1000-GENERATE-MESSAGE-X
               PERFORM  0030-5000-LOGIC-ERROR
                   THRU 0030-5000-LOGIC-ERROR-X
           END-IF.

       2000-PROCESS-COMPANY-X.
           EXIT.
      /
      *----------------------
       3000-PROCESS-EACH-APP.
      *----------------------

      * HERE WE WANT TO CHECK IF THE APPLICATION EXISTS ON THE
      * UPLOAD POLICY TABLE (UPOL).
       
           MOVE WS-APP-ID TO WUPOL-APP-ID.
           PERFORM UPOL-1000-READ
              THRU UPOL-1000-READ-X.
      
      * IF IT IS NOT FOUND, GREAT WE KNOW WE DO NOT HAVE A
      * DUPLICATE APP.  IF IT IS FOUND, WE DO HAVE A DUPLICATE
      * APP AND WE NEED TO CHANGE THE SALES OFFICE NUMBER TO '090'.
      * THE SALES OFFICE NUMBER IS FOUND IN THE FIRST 3 BYTES OF THE
      * APP ID.  EG.  IF THE APP ID IS 03970001412 WE NEED TO CHANGE
      * 039 TO 090 - SO THE APP ID WOULD END UP AS 09070001412.
       
           IF  WUPOL-IO-NOT-FOUND
               CONTINUE
           ELSE
               MOVE 'N' TO WS-NO-MORE-DUPLICATES-SW
               MOVE WS-APP-ID TO WS-DUPLICATE-APP-ID
               MOVE ZERO      TO WS-DUP-APP-1
               MOVE 90        TO WS-DUP-APP-2-3
               PERFORM 3500-PROCESS-DUP-APP-ID
                  THRU 3500-PROCESS-DUP-APP-ID-X
                    UNTIL WS-NO-MORE-DUPLICATES
           END-IF.

           PERFORM 4000-READ-APP-ID
              THRU 4000-READ-APP-ID-X.

       3000-PROCESS-EACH-APP-X.
           EXIT.
      /
       3500-PROCESS-DUP-APP-ID.
        
           MOVE WS-DUP-APP-ID-SO TO WS-DUP-APP-SALES-OFFICE.
            
      * AFTER WE CHANGED THE SALES OFFICE TO '090' CHECK TO SEE IF
      * THIS APP IS ON UPOL.
       
           MOVE WS-DUPLICATE-APP-ID TO WUPOL-APP-ID.
           PERFORM UPOL-1000-READ
              THRU UPOL-1000-READ-X.
      
      * IF THE CHANGED APP IS NOT FOUND, THEN WE KNOW IT IS SAFE TO
      * UPLOAD THIS APP.  IF IT IS FOUND WE KNOW IT HAS BEEN CHANGED
      * AND UPLOADED IN THE PAST.  KEEP INCREMENTING THE APP FROM
      * '090' UNTIL '099' AND CHECK UPOL AGAIN.  IF IT EXCEEDS '099',
      * DISPLAY ERROR MESSAGE AND ABEND.
       
           IF  WUPOL-IO-NOT-FOUND
               MOVE SPACES TO RADUP-SEQ-REC-INFO
               MOVE WS-APP-ID           TO RADUP-OLD-APP-ID
               MOVE WS-DUPLICATE-APP-ID TO RADUP-NEW-APP-ID
               PERFORM ADUP-1000-WRITE
                  THRU ADUP-1000-WRITE-X
               SET WS-NO-MORE-DUPLICATES TO TRUE
      *MSG: DUPLICATION APPLICATION NUMBER: @1 CHANGED TO: @2 
               MOVE 'AS93990004'        TO WGLOB-MSG-REF-INFO
               MOVE WS-APP-ID           TO WGLOB-MSG-PARM (1)
               MOVE WS-DUPLICATE-APP-ID TO WGLOB-MSG-PARM (2)
               PERFORM  0260-1000-GENERATE-MESSAGE
                  THRU 0260-1000-GENERATE-MESSAGE-X
               ADD 1                    TO WS-DUP-APP-REC-COUNTER
           ELSE
              IF WS-DUP-APP-2-3 = 99
      *MSG: WARNING: APP ID @1 HAS BEEN FOUND AS @2 AND WILL
      *     EXCEED SALES OFFICE ID 099
                 MOVE 'AS93990007'        TO WGLOB-MSG-REF-INFO
                 MOVE WS-APP-ID           TO WGLOB-MSG-PARM (1)
                 MOVE WS-DUPLICATE-APP-ID TO WGLOB-MSG-PARM (2)
                 PERFORM  0260-1000-GENERATE-MESSAGE
                     THRU 0260-1000-GENERATE-MESSAGE-X
                 PERFORM  0030-5000-LOGIC-ERROR
                     THRU 0030-5000-LOGIC-ERROR-X
              ELSE
                 ADD 1 TO WS-DUP-APP-2-3
              END-IF
           END-IF.
            
            
        3500-PROCESS-DUP-APP-ID-X.
            EXIT.
      /
       4000-READ-APP-ID.

      * READ THE APPLICATION ID TO PROCESS FROM THE AS9399 INPUT FILE

           MOVE SPACES TO WS-APP-ID.

           PERFORM 9399-1000-READ
              THRU 9399-1000-READ-X.

           IF  W9399-SEQ-IO-EOF
               SET WAPIN-NO-MORE-APPS TO TRUE
           ELSE
               MOVE R9399-APP-ID                 TO WS-APP-ID
               ADD 1                             TO WS-APP-REC-COUNTER
           END-IF.

       4000-READ-APP-ID-X.
           EXIT.
      *------------------------
       5000-PRINT-GRAND-TOTALS.
      *------------------------
      *
      * RE-INITIALIZE TITLES/HEADINGS USING DEFAULT COMPANY CODE
      * SO THE REPORT IS NOT TIED TO ONE SPECIFIC COMPANY
      *
P00697*    MOVE WPGWS-CRNT-PGM-ID      TO L0960-PROGRAM-ID.
P00697*
P00697*    PERFORM  0960-4000-INIT-DEFAULT-COMP
P00697*        THRU 0960-4000-INIT-DEFAULT-COMP-X.
P00697*
P00697*    PERFORM 0290-1000-BUILD-PARM-INFO
P00697*       THRU 0290-1000-BUILD-PARM-INFO-X.
P00697*
P00697*    PERFORM  6000-INIT-TITLES
P00697*        THRU 6000-INIT-TITLES-X.

      *MSG: TOTAL NUMBER OF UPLOADED RECORDS READ @1
           MOVE 'AS93990005'           TO WGLOB-MSG-REF-INFO.
           MOVE WS-APP-REC-COUNTER     TO WS-PIC-COUNTER.
           MOVE WS-PIC-COUNTER         TO WGLOB-MSG-PARM (1).
           PERFORM  0260-1000-GENERATE-MESSAGE
               THRU 0260-1000-GENERATE-MESSAGE-X.

      *MSG: TOTAL NUMBER OF DUPLICATE APPLICATIONS CHANGED
           MOVE 'AS93990008'           TO WGLOB-MSG-REF-INFO.
           MOVE WS-DUP-APP-REC-COUNTER TO WS-PIC-COUNTER.
           MOVE WS-PIC-COUNTER         TO WGLOB-MSG-PARM (1).
           PERFORM  0260-1000-GENERATE-MESSAGE
               THRU 0260-1000-GENERATE-MESSAGE-X.

           MOVE SPACES                 TO L0040-INPUT-LINE.

           PERFORM  0040-3000-WRITE-OTHER
               THRU 0040-3000-WRITE-OTHER-X.

       5000-PRINT-GRAND-TOTALS-X.
           EXIT.
      /
      *-----------------
       6000-INIT-TITLES.
      *-----------------

           MOVE L0960-COMPANY-NAME     TO L0040-COMPANY-NAME.
           MOVE ZERO                   TO L0040-ERROR-CNT.

      *
      * SET UP THE TITLE/HEADING LINES
      *
      * GET THE SYSTEM ID
      *
           MOVE 'XS00000145'           TO WGLOB-MSG-REF-INFO.

           PERFORM  0260-2000-GET-MESSAGE
               THRU 0260-2000-GET-MESSAGE-X.

           MOVE WGLOB-MSG-TXT          TO L0040-SYSTEM-ID.
      *
      * GET THE PROGRAM DESCRIPTION
      *
           MOVE 'AS93990006'           TO WGLOB-MSG-REF-INFO.

           PERFORM  0260-2000-GET-MESSAGE
               THRU 0260-2000-GET-MESSAGE-X.

           MOVE WGLOB-MSG-TXT          TO L0040-PROGRAM-DESC.
      *
      * GET THE DETAIL HEADINGS FOR PRINTING CONTROL CARDS
      *
           MOVE 'XS00000150'           TO WGLOB-MSG-REF-INFO.

           PERFORM  0260-2000-GET-MESSAGE
               THRU 0260-2000-GET-MESSAGE-X.

           MOVE WGLOB-MSG-TXT          TO L0040-HDG-LINE-3.

           PERFORM  0040-1000-INIT-TITLE
               THRU 0040-1000-INIT-TITLE-X.

       6000-INIT-TITLES-X.
           EXIT.
      /
      *-----------------------
       7000-READ-CONTROL-CARD.
      *-----------------------

           PERFORM  BCF-1000-READ
               THRU BCF-1000-READ-X.

           IF  WBCF-SEQ-IO-OK
               ADD 1                   TO WS-PARM-CARD-COUNTER
           END-IF.

       7000-READ-CONTROL-CARD-X.
           EXIT.
      /
      *-----------------
       9999-CLOSE-FILES.
      *-----------------
      *
      * THIS ROUTINE WILL CLOSE ALL FILES PRIOR TO THE PROGRAM FINISH
      *

           PERFORM  BCF-4000-CLOSE
               THRU BCF-4000-CLOSE-X.

           PERFORM  9399-4000-CLOSE
              THRU  9399-4000-CLOSE-X.

           PERFORM  ADUP-4000-CLOSE
              THRU  ADUP-4000-CLOSE-X.

           PERFORM  OCF-4000-CLOSE
               THRU OCF-4000-CLOSE-X.

       9999-CLOSE-FILES-X.
           EXIT.
      /
      *****************************************************************
      * FILE I/O PROCESS MODULES                                      *
      *****************************************************************

P02229 COPY XCPL0035.

       COPY ACPNUPOL.
       COPY CCPNPCOM.
       COPY CCPNMAST.
      /
       COPY XCPL0040.
       COPY XCPL0260.
      /
       COPY NCPL0960.
       COPY XCPS0290.
       COPY XCPL0290.
      /
      **********************************
      * APEX UPLOAD FILE I/O COPYBOOKS *
      **********************************
      *
      /
       COPY ACPO9399.
       COPY ACPN9399.
       COPY ACPL9399.
       COPY ACPAADUP.
       COPY ACPNADUP.
       COPY ACPOADUP.
       COPY ACPLADUP.
      /
      **************************
      * TPI FILE I/O COPYBOOKS *
      **************************
      *
       COPY XCPLBCF.
       COPY XCPNBCF.
       COPY XCPOBCF.
      /
       COPY XCPLOCF.
       COPY XCPOOCF.
      /
       COPY XCPL0030.

      *****************************************************************
      **                 END OF PROGRAM ASBM9399                     **
      *****************************************************************
