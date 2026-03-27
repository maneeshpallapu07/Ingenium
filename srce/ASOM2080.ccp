      *************************
       IDENTIFICATION DIVISION.
      *************************
    *************************
       IDENTIFICATION DIVISION.
      *************************

       PROGRAM-ID. ASOM2080.

       COPY XCWWCRHT.

      *****************************************************************
      **  MEMBER :  ASOM2080                                         **
      **  REMARKS:  PROCESS DRIVER FOR UPLOAD FIELD TRANSACTION      **
      **            TRANSACTION UFLD.                                **
      **                                                             **
      **  DOMAIN :  SY                                               **
      **  CLASS  :  PD                                               **
      *****************************************************************
      **  DATE     AUTH.  DESCRIPTION                                **
      **                                                             **
APEX  **  24FEB94  CMM    ALLOW POLC, CVGC AND CVGA, AND REQT        **
APEX52**  30NOV94  JJS    UPGRADE TO RELEASE 5.2,                    **
APEX52**                  REMOVE OFFSET, LEN & DEC FIELDS,           **
APEX52**                  COMBINED MESSAGES INTO ONE LINE            **
APEX53**  30NOV95  JJS    UPGRADE TO INGENIUM 5.3 & WINAPEX 1.0,     **
APEX53**                  NAME CHANGES NOT TAGGED,                   **
APEX53**                  ARCHITECTURE CHANGES TO SUPPORT GUI,       **
APEX53**                  CHANGES TO SUPPORT I/O PROGRAMS,           **
APEX53**                  SET MORE DATA INDICATOR FOR GUI INTERFACE  **
APEX54**  31DEC96  TJS    MODIFICATIONS FOR MAINTAINABILITY          **
54-100**  31DEC96  SLB    CHANGE INQUIRE TO BROWSE & BROWSE TO       **
54-100**                  BROWSE-SUMMARY. STANDARDIZE MESSAGES.      **
557700**  30SEP97  CG     ADD NEW TYPE FOR MIXED CASE DATA.          **
557708**  30SEP97  JTS    ABEND HANDLING                             **
008445**  31MAR98  552    GENERATE MIR FROM TECH ARCH DATA BASE      **
007766**  30OCT98  56     ARCHITECTURE CHANGES TO SUPPORT PASSING    **
007766**                  PARAMETERS VIA AN ADDRESS                  **
013578** 15DEC99   60     REMOVAL OF 3270 LOGIC, MIR RENAMES         **
014660** 31DEC1999 60     REMOVE XCPPMEXT                            **
014590**  15DEC99  60     ARCHITECTURAL CHANGES                      **
015543**  15DEC99  60     CODE CLEANUP                               **
      *****************************************************************
 
      **********************
       ENVIRONMENT DIVISION.
      **********************
 
      ***************
       DATA DIVISION.
      ***************
 
       WORKING-STORAGE SECTION.
 
APEX53 COPY XCWWPGWS REPLACING '$VAR1' BY 'ASOM2080'.
 
       COPY SQLCA.
007766 COPY XCWLPTR.
 
 
014590*COPY XCWL0030.
 
       01  WS-PGM-WORK-AREA.
           05  WS-TRANS-NAME               PIC X(04)  VALUE 'UFLD'.
 
 
       01  WS-EDIT-WORK-AREA.
           05  WS-BUS-FCN-ID               PIC X(04).
               88  WS-BUS-FCN-VALID        VALUE '2080' '2081' '2082'
                                                 '2083' '2084' '2085'.
               88  WS-BUS-FCN-RETRIEVE     VALUE '2080'.
               88  WS-BUS-FCN-CREATE       VALUE '2081'.
               88  WS-BUS-FCN-UPDATE       VALUE '2082'.
               88  WS-BUS-FCN-DELETE       VALUE '2083'.
               88  WS-BUS-FCN-LIST         VALUE '2084'.
               88  WS-BUS-FCN-SUMMARY      VALUE '2085'.
 
           05  WS-EDIT-NAME                PIC X(20).
           05  WS-EDIT-FIELD-NAME-R        REDEFINES WS-EDIT-NAME.
               10  WS-EDIT-CHAR            OCCURS 20 TIMES
                                           INDEXED BY WS-CHAR
                                           PIC X(01).
 
           05  WS-EDIT-FIELD-TYPE          PIC X(01).
               88  VALID-FIELD-TYPE        VALUES 'C' 'D' 'F' 'M' 'N'
557700*                                           'T' 'U' 'X' 'A'.
557700                                            'T' 'U' 'X' 'A' 'B'.
               88  DOLLAR-AMOUNT-TYPE      VALUE 'A'.
557700*        88  CHAR-TYPE               VALUE 'C'.
557700         88  MIXED-CHAR-TYPE         VALUE 'B'.
557700         88  UPPER-CHAR-TYPE         VALUE 'C'.
               88  DATE-TYPE               VALUE 'D'.
               88  MESSAGE-TYPE            VALUE 'M'.
               88  NUMERIC-TYPE            VALUE 'N'.
               88  TRANSLATE-TYPE          VALUE 'F' 'T'.
               88  UNUSED-TYPE             VALUE 'U'.
               88  COMPLEX-TYPE            VALUE 'X'.
 
      *
       01  WS-SWITCHES.
           05  WS-VALIDATE-FAIL-SW         PIC X(01).
               88  WS-VALIDATE-FAILED      VALUE 'Y'.
               88  WS-VALIDATE-OK          VALUE 'N'.
           05  WS-EDITS-OK-SW              PIC X(01)  VALUE 'Y'.
               88  WS-EDIT-OK              VALUE 'Y'.
               88  WS-EDIT-FAILED          VALUE 'N'.
           05  WS-FIRST-SPACE-SW           PIC X(01).
               88  WS-FIRST-SPACE          VALUE 'Y'.
               88  WS-FIRST-SPACE-NOT      VALUE 'N'.
 
       01  WS-WORK-FIELDS.
           05  WS-LINE                     PIC S9(04) COMP.
           05  WS-MAX-LINES                PIC S9(04) COMP VALUE +12.
      *
       COPY XCWEBLCH.
 
       COPY ACWTUFCT.
 
 
       COPY XCWWWKDT.
      *
       COPY ACFWUFLD.
       COPY ACFRUFLD.
      *
       COPY ACFWUTTB.
       COPY ACFRUTTB.
      *
 
007766*01  WGLOB-GLOBAL-AREA.
007766*COPY XCWWGLOB.
      *
      *****************
       LINKAGE SECTION.
      *****************
 
007766 01 WGLOB-GLOBAL-AREA.
007766 COPY XCWWGLOB.
       COPY ACWM2080.
      *
       PROCEDURE DIVISION USING WGLOB-GLOBAL-AREA
                                MIR-PARM-AREA.

      *--------------
       0000-MAINLINE.
      *--------------

557708     PERFORM  ABND-1000-HANDLE-ABEND
557708         THRU ABND-1000-HANDLE-ABEND-X.

           PERFORM  INIT-1000-INITIALIZE
               THRU INIT-1000-INITIALIZE-X.
 
           PERFORM  2000-PROCESS-REQUEST
               THRU 2000-PROCESS-REQUEST-X.
 
           PERFORM  EXIT-1000-FINALIZE
               THRU EXIT-1000-FINALIZE-X.
 
           GOBACK.
 
       0000-MAINLINE-X.
           EXIT.
      *
      *--------------------------
       2000-PROCESS-REQUEST.
      *--------------------------
 
           PERFORM  9300-SETUP-MSIN-REFERENCE
               THRU 9300-SETUP-MSIN-REFERENCE-X.
 
           MOVE MIR-BUS-FCN-ID       TO WS-BUS-FCN-ID.
 
      *
      * PROCESS SCREEN FUNCTIONS
      *
           EVALUATE TRUE
 
54-100         WHEN WS-BUS-FCN-LIST
54-100              PERFORM  3000-PROCESS-LIST
54-100                  THRU 3000-PROCESS-LIST-X
 
               WHEN WS-BUS-FCN-CREATE
                    PERFORM  4000-CREATE
                        THRU 4000-CREATE-X

               WHEN WS-BUS-FCN-UPDATE
                    PERFORM  5200-PROCESS-UPDATE
                        THRU 5200-PROCESS-UPDATE-X
 
               WHEN WS-BUS-FCN-DELETE
                    PERFORM  6000-PROCESS-DELETE
                        THRU 6000-PROCESS-DELETE-X

               WHEN WS-BUS-FCN-RETRIEVE
                    PERFORM  5000-RETRIEVE
                        THRU 5000-RETRIEVE-X
 
               WHEN OTHER
54-100              PERFORM  3500-BROWSE-SUMMARY
54-100                  THRU 3500-BROWSE-SUMMARY-X
 
           END-EVALUATE.
 
       2000-PROCESS-REQUEST-X.
           EXIT.
      *
      *-------------
54-100 3000-PROCESS-LIST.
      *-------------
 
      *
      * BROWSE PROCESSING:  SET UP BROWSE KEYS, BEGIN BROWSE, AND
      * LOAD DATA ARRAY UNTIL END-OF-FILE OR SCREEN IS FULL.
      *
 
           MOVE LOW-VALUES            TO WUFLD-KEY.
 
           PERFORM  8000-BUILD-UFLD-KEY
               THRU 8000-BUILD-UFLD-KEY-X.
 
           MOVE HIGH-VALUES           TO WUFLD-ENDBR-KEY.
           MOVE MIR-UPLD-FLD-STRUCT-NM           
                                      TO WUFLD-ENDBR-UPLD-FLD-STRUCT-NM.
 
 
           PERFORM  9100-BLANK-DATA-FIELDS
               THRU 9100-BLANK-DATA-FIELDS-X
               VARYING WS-LINE FROM +1 BY +1
               UNTIL WS-LINE > WS-MAX-LINES.
 
           PERFORM  UFLD-1000-BROWSE
               THRU UFLD-1000-BROWSE-X.
 
           IF  WUFLD-IO-EOF
               MOVE 'XS00000034'      TO WGLOB-MSG-REF-INFO
               PERFORM  0260-1000-GENERATE-MESSAGE
                   THRU 0260-1000-GENERATE-MESSAGE-X
54-100         GO TO 3000-PROCESS-LIST-X
           END-IF.
 
           PERFORM  UFLD-2000-READ-NEXT
               THRU UFLD-2000-READ-NEXT-X.
 
           IF  WUFLD-IO-EOF
               MOVE 'XS00000034'      TO WGLOB-MSG-REF-INFO
               PERFORM  0260-1000-GENERATE-MESSAGE
                   THRU 0260-1000-GENERATE-MESSAGE-X
54-100         GO TO 3000-PROCESS-LIST-X
           END-IF.
 
           PERFORM  3100-DISPLAY-RECORD
               THRU 3100-DISPLAY-RECORD-X
               VARYING WS-LINE FROM +1 BY +1
               UNTIL WUFLD-IO-EOF
               OR WS-LINE > WS-MAX-LINES.
 
           IF  WUFLD-IO-EOF
54-100* MSG: END OF LIST
54-100         MOVE 'XS00000015'      TO WGLOB-MSG-REF-INFO
               PERFORM  0260-1000-GENERATE-MESSAGE
                   THRU 0260-1000-GENERATE-MESSAGE-X
           ELSE
               MOVE RUFLD-UPLD-FLD-APEX-NM
                                      TO MIR-UPLD-FLD-APEX-NM
               MOVE 'XS00000014'      TO WGLOB-MSG-REF-INFO
APEX53         SET WGLOB-MORE-DATA-EXISTS  TO TRUE
               PERFORM  0260-1000-GENERATE-MESSAGE
                   THRU 0260-1000-GENERATE-MESSAGE-X
           END-IF.
 
           PERFORM  UFLD-3000-END-BROWSE
               THRU UFLD-3000-END-BROWSE-X.
 
54-100 3000-PROCESS-LIST-X.
           EXIT.
      *
      *--------------------
       3100-DISPLAY-RECORD.
      *--------------------
 
           PERFORM  9200-MOVE-RECORD-TO-SCREEN
               THRU 9200-MOVE-RECORD-TO-SCREEN-X.
 
           PERFORM  UFLD-2000-READ-NEXT
               THRU UFLD-2000-READ-NEXT-X.
 
       3100-DISPLAY-RECORD-X.
           EXIT.
      *
      *-------------------
54-100 3500-BROWSE-SUMMARY.
      *-------------------
 
      *
      * BROWSE SUMMARY PROCESSING:  SET UP BROWSE KEYS, BEGIN BROWSE, &
      * LOAD DATA ARRAY UNTIL END-OF-FILE OR SCREEN IS FULL.
      *
 
           PERFORM  9100-BLANK-DATA-FIELDS
               THRU 9100-BLANK-DATA-FIELDS-X
               VARYING WS-LINE FROM +1 BY +1
               UNTIL WS-LINE > WS-MAX-LINES.
 
 
           MOVE LOW-VALUES            TO WUFLD-KEY.
           MOVE HIGH-VALUES           TO WUFLD-ENDBR-KEY.
 
           PERFORM  8000-BUILD-UFLD-KEY
               THRU 8000-BUILD-UFLD-KEY-X.
 
           PERFORM  UFLD-1000-BROWSE
               THRU UFLD-1000-BROWSE-X.
 
           IF  WUFLD-IO-EOF
               MOVE 'XS00000034'      TO WGLOB-MSG-REF-INFO
               PERFORM  0260-1000-GENERATE-MESSAGE
                   THRU 0260-1000-GENERATE-MESSAGE-X
54-100         GO TO 3500-BROWSE-SUMMARY-X
           END-IF.
 
           PERFORM  UFLD-2000-READ-NEXT
               THRU UFLD-2000-READ-NEXT-X.
 
           PERFORM  3100-DISPLAY-RECORD
               THRU 3100-DISPLAY-RECORD-X
               VARYING WS-LINE FROM +1 BY +1
               UNTIL WUFLD-IO-EOF
               OR WS-LINE > WS-MAX-LINES.
 
           IF  WUFLD-IO-EOF
54-100* MSG: END OF FILE REACHED
54-100         MOVE 'XS00000025'      TO WGLOB-MSG-REF-INFO
               PERFORM  0260-1000-GENERATE-MESSAGE
                   THRU 0260-1000-GENERATE-MESSAGE-X
           ELSE
008445*        MOVE RUFLD-UPLD-FLD-STRUCT-NM  TO MIR-UPLD-FLD-STRUCT-NM2
008445         MOVE RUFLD-UPLD-FLD-STRUCT-NM 
                                      TO MIR-UPLD-FLD-STRUCT-NM
008445*        MOVE RUFLD-UPLD-FLD-APEX-NM TO MIR-UPLD-FLD-APEX-NM2
008445         MOVE RUFLD-UPLD-FLD-APEX-NM
                                      TO MIR-UPLD-FLD-APEX-NM
               MOVE 'XS00000014'      TO WGLOB-MSG-REF-INFO
APEX53         SET WGLOB-MORE-DATA-EXISTS  TO TRUE
               PERFORM  0260-1000-GENERATE-MESSAGE
                   THRU 0260-1000-GENERATE-MESSAGE-X
           END-IF.
 
           PERFORM  UFLD-3000-END-BROWSE
               THRU UFLD-3000-END-BROWSE-X.
 
54-100 3500-BROWSE-SUMMARY-X.
           EXIT.
      *
      *------------
       4000-CREATE.
      *------------
 
      *
      * CREATE PROCESSING:  CHECK IF RECORD DOES NOT EXIST, INIT
      * NEW RECORD AND ALLOW USER TO MODIFY.
      *
 
           PERFORM  4100-VALIDATE-KEY-FIELDS
               THRU 4100-VALIDATE-KEY-FIELDS-X.
 
           IF  WS-VALIDATE-FAILED
               GO TO 4000-CREATE-X
           END-IF.
 
           PERFORM  8000-BUILD-UFLD-KEY
               THRU 8000-BUILD-UFLD-KEY-X.
 
           PERFORM  UFLD-1000-READ
               THRU UFLD-1000-READ-X.
 
           IF  WUFLD-IO-OK
               MOVE WUFLD-KEY         TO WGLOB-MSG-PARM (1)
               MOVE 'XS00000003'      TO WGLOB-MSG-REF-INFO
               PERFORM  0260-1000-GENERATE-MESSAGE
                   THRU 0260-1000-GENERATE-MESSAGE-X
               PERFORM  9100-BLANK-DATA-FIELDS
                   THRU 9100-BLANK-DATA-FIELDS-X
                   VARYING WS-LINE FROM +1 BY +1
                   UNTIL WS-LINE > WS-MAX-LINES
           ELSE
               PERFORM  UFLD-1000-CREATE
                   THRU UFLD-1000-CREATE-X
               PERFORM  UFLD-1000-WRITE
                   THRU UFLD-1000-WRITE-X
               MOVE 'XS00000004'      TO WGLOB-MSG-REF-INFO
               PERFORM  0260-1000-GENERATE-MESSAGE
                   THRU 0260-1000-GENERATE-MESSAGE-X
               MOVE +1                TO WS-LINE
               PERFORM  9200-MOVE-RECORD-TO-SCREEN
                   THRU 9200-MOVE-RECORD-TO-SCREEN-X
               MOVE 'XS00000016'      TO WGLOB-MSG-REF-INFO
               PERFORM  0260-1000-GENERATE-MESSAGE
                   THRU 0260-1000-GENERATE-MESSAGE-X
           END-IF.
 
       4000-CREATE-X.
           EXIT.
      *
      *-------------------------
       4100-VALIDATE-KEY-FIELDS.
      *-------------------------
 
           SET WS-VALIDATE-OK              TO TRUE.
 
           PERFORM  4110-VALIDATE-STRUCTURE
               THRU 4110-VALIDATE-STRUCTURE-X.
 
           PERFORM  4120-VALIDATE-APEX-FIELD
               THRU 4120-VALIDATE-APEX-FIELD-X.
 
       4100-VALIDATE-KEY-FIELDS-X.
           EXIT.
      *
      *------------------------
       4110-VALIDATE-STRUCTURE.
      *------------------------
 
           IF  MIR-UPLD-FLD-STRUCT-NM = SPACES
               SET WS-VALIDATE-FAILED      TO TRUE
           ELSE
               SET  WS-FIRST-SPACE-NOT     TO TRUE
               MOVE MIR-UPLD-FLD-STRUCT-NM             
                                      TO WS-EDIT-NAME
               PERFORM  4150-CHECK-IMBEDDED-SPACE
                   THRU 4150-CHECK-IMBEDDED-SPACE-X
                   VARYING WS-CHAR FROM +1 BY +1
                   UNTIL WS-CHAR > +20
                   OR WS-VALIDATE-FAILED
           END-IF.
 
           IF  WS-VALIDATE-FAILED
               MOVE MIR-UPLD-FLD-STRUCT-NM             
                                      TO WGLOB-MSG-PARM (1)
               MOVE 'AS20800001'      TO WGLOB-MSG-REF-INFO
               PERFORM  0260-1000-GENERATE-MESSAGE
                   THRU 0260-1000-GENERATE-MESSAGE-X
           END-IF.
 
       4110-VALIDATE-STRUCTURE-X.
           EXIT.
      *
      *-------------------------
       4120-VALIDATE-APEX-FIELD.
      *-------------------------
 
           IF  MIR-UPLD-FLD-APEX-NM = SPACES
               SET WS-VALIDATE-FAILED      TO TRUE
           ELSE
               SET  WS-FIRST-SPACE-NOT     TO TRUE
               MOVE MIR-UPLD-FLD-APEX-NM             
                                      TO WS-EDIT-NAME
               PERFORM  4150-CHECK-IMBEDDED-SPACE
                   THRU 4150-CHECK-IMBEDDED-SPACE-X
                   VARYING WS-CHAR FROM +1 BY +1
                   UNTIL WS-CHAR > +20
                   OR WS-VALIDATE-FAILED
           END-IF.
 
           IF  WS-VALIDATE-FAILED
               MOVE MIR-UPLD-FLD-APEX-NM             
                                      TO WGLOB-MSG-PARM (1)
               MOVE 'AS20800002'      TO WGLOB-MSG-REF-INFO
               PERFORM  0260-1000-GENERATE-MESSAGE
                   THRU 0260-1000-GENERATE-MESSAGE-X
           END-IF.
 
       4120-VALIDATE-APEX-FIELD-X.
           EXIT.
      *
      *--------------------------
       4150-CHECK-IMBEDDED-SPACE.
      *--------------------------
 
           IF  WS-EDIT-CHAR (WS-CHAR) = SPACES
               IF  WS-FIRST-SPACE
                   NEXT SENTENCE
               ELSE
                   SET  WS-FIRST-SPACE     TO TRUE
               END-IF
           ELSE
               IF  WS-FIRST-SPACE
                   SET WS-VALIDATE-FAILED  TO TRUE
               END-IF
           END-IF.
 
       4150-CHECK-IMBEDDED-SPACE-X.
           EXIT.
      *
      *--------------
       5000-RETRIEVE.
      *--------------
 
 
           PERFORM  8000-BUILD-UFLD-KEY
               THRU 8000-BUILD-UFLD-KEY-X.
 
           PERFORM  UFLD-1000-READ
               THRU UFLD-1000-READ-X.
 
           IF  WUFLD-IO-NOT-FOUND
               MOVE WUFLD-KEY         TO WGLOB-MSG-PARM (1)
               MOVE 'XS00000001'      TO WGLOB-MSG-REF-INFO
               PERFORM  0260-1000-GENERATE-MESSAGE
                   THRU 0260-1000-GENERATE-MESSAGE-X
               PERFORM  9100-BLANK-DATA-FIELDS
                   THRU 9100-BLANK-DATA-FIELDS-X
                   VARYING WS-LINE FROM +1 BY +1
                   UNTIL WS-LINE > WS-MAX-LINES
           ELSE
               MOVE +1                TO WS-LINE
               PERFORM  9200-MOVE-RECORD-TO-SCREEN
                   THRU 9200-MOVE-RECORD-TO-SCREEN-X
               MOVE 'XS00000016'      TO WGLOB-MSG-REF-INFO
               PERFORM  0260-1000-GENERATE-MESSAGE
                   THRU 0260-1000-GENERATE-MESSAGE-X
           END-IF.
 
       5000-RETRIEVE-X.
           EXIT.
      *
      *------------------------
       5200-PROCESS-UPDATE.
      *------------------------
 
           PERFORM  8000-BUILD-UFLD-KEY
               THRU 8000-BUILD-UFLD-KEY-X.
 
           PERFORM  UFLD-1000-READ-FOR-UPDATE
               THRU UFLD-1000-READ-FOR-UPDATE-X.
 
           IF  WUFLD-IO-NOT-FOUND
               MOVE WUFLD-KEY         TO WGLOB-MSG-PARM (1)
               MOVE 'XS00000006'      TO WGLOB-MSG-REF-INFO
               PERFORM  0260-1000-GENERATE-MESSAGE
                   THRU 0260-1000-GENERATE-MESSAGE-X
               GO TO 5200-PROCESS-UPDATE-X
           END-IF.
 
           PERFORM  5300-EDIT-DATA-FIELDS
               THRU 5300-EDIT-DATA-FIELDS-X.
 
      *
      * RECORD IS NOT REWRITTEN IF ANY FIELD IS INVALID
      *
           IF  WS-EDIT-FAILED
               PERFORM  UFLD-3000-UNLOCK
                   THRU UFLD-3000-UNLOCK-X
               MOVE 'XS00000032'      TO WGLOB-MSG-REF-INFO
               PERFORM  0260-1000-GENERATE-MESSAGE
                   THRU 0260-1000-GENERATE-MESSAGE-X
               GO TO 5200-PROCESS-UPDATE-X
           END-IF.
 
           PERFORM  UFLD-2000-REWRITE
               THRU UFLD-2000-REWRITE-X.
 
           MOVE 'XS00000007'          TO WGLOB-MSG-REF-INFO.
 
           PERFORM  0260-1000-GENERATE-MESSAGE
               THRU 0260-1000-GENERATE-MESSAGE-X.
 
 
       5200-PROCESS-UPDATE-X.
           EXIT.
      *
      *---------------------
       5300-EDIT-DATA-FIELDS.
      *---------------------
 
      *
      * FIELD TYPE EDIT:  CHECK VALIDITY OF FIELD TYPE, SINCE
      * NO UPDATES WILL BE PERFORMED IF THE FIELD TYPE IS INVALID.
      *
 
           SET WS-EDIT-OK                  TO TRUE.
 
           PERFORM  5310-EDIT-FIELD-TYPE
               THRU 5310-EDIT-FIELD-TYPE-X.
 
           PERFORM  5360-EDIT-FILE
               THRU 5360-EDIT-FILE-X.
 
           IF  WS-EDIT-FAILED
               GO TO 5300-EDIT-DATA-FIELDS-X
           END-IF.

           PERFORM  5380-EDIT-UTTB
               THRU 5380-EDIT-UTTB-X.
 
           PERFORM  5390-EDIT-FIELD-NAME
               THRU 5390-EDIT-FIELD-NAME-X.
 
       5300-EDIT-DATA-FIELDS-X.
           EXIT.
      *
      *---------------------
       5310-EDIT-FIELD-TYPE.
      *---------------------
 
      *
      * FIELD TYPE EDIT:  CHECK VALIDITY OF FIELD TYPE.
      * NO UPDATES WILL BE PERFORMED IF THE FIELD TYPE IS INVALID.
      *
 
           IF  MIR-UPLD-FLD-TYP-CD-T (1) = SPACES
               MOVE RUFLD-UPLD-FLD-TYP-CD 
                                      TO MIR-UPLD-FLD-TYP-CD-T (1)
           ELSE
               IF  MIR-UPLD-FLD-TYP-CD-T (1) = EBLCH-BLANK-FIELD-CHAR
                   MOVE SPACES        TO MIR-UPLD-FLD-TYP-CD-T (1)
               END-IF
           END-IF.
 
           MOVE MIR-UPLD-FLD-TYP-CD-T (1)           
                                      TO WS-EDIT-FIELD-TYPE.
 
           IF  VALID-FIELD-TYPE
               MOVE MIR-UPLD-FLD-TYP-CD-T (1)       
                                      TO RUFLD-UPLD-FLD-TYP-CD
           ELSE
               MOVE MIR-UPLD-FLD-TYP-CD-T (1)       
                                      TO WGLOB-MSG-PARM (1)
               MOVE 'AS20800003'      TO WGLOB-MSG-REF-INFO
               PERFORM  0260-1000-GENERATE-MESSAGE
                   THRU 0260-1000-GENERATE-MESSAGE-X
               SET WS-EDIT-FAILED          TO TRUE
           END-IF.
 
       5310-EDIT-FIELD-TYPE-X.
           EXIT.
      *
      *---------------
       5360-EDIT-FILE.
      *---------------
 
      *
      * EDIT FILE:  EDIT FILE CODE
      *
 
           IF  MIR-UPLD-FLD-FILE-CD-T (1) = SPACES
               MOVE RUFLD-UPLD-FLD-FILE-CD
                                      TO MIR-UPLD-FLD-FILE-CD-T (1)
           ELSE
               IF  MIR-UPLD-FLD-FILE-CD-T (1) = EBLCH-BLANK-FIELD-CHAR
                   MOVE SPACES        TO MIR-UPLD-FLD-FILE-CD-T (1)
               END-IF
           END-IF.
 
           MOVE MIR-UPLD-FLD-FILE-CD-T (1)           
                                      TO TUFCT-SEARCH-KEY.
 
      *
      * NOT NECESSARY TO SPECIFY FILE FOR UNUSED OR MESSAGE TYPES
      *
APEX52     IF  (UNUSED-TYPE
APEX52     OR  MESSAGE-TYPE)
APEX52     AND MIR-UPLD-FLD-FILE-CD-T (1) = SPACES
APEX52         MOVE SPACES            TO RUFLD-UPLD-FLD-FILE-CD
APEX52         GO TO 5360-EDIT-FILE-X
           END-IF.
 
           PERFORM
               VARYING TUFCT-SUB FROM +1 BY +1
               UNTIL   TUFCT-SUB > TUFCT-SIZE
               OR      TUFCT-FILE (TUFCT-SUB) = TUFCT-SEARCH-KEY
015543         CONTINUE               
           END-PERFORM.
 
APEX52     IF  TUFCT-SUB                   > TUFCT-SIZE
APEX52         MOVE TUFCT-SEARCH-KEY  TO WGLOB-MSG-PARM (1)
APEX52         MOVE 'AS20800004'      TO WGLOB-MSG-REF-INFO
APEX52         PERFORM  0260-1000-GENERATE-MESSAGE
APEX52             THRU 0260-1000-GENERATE-MESSAGE-X
APEX52         SET  WS-EDIT-FAILED         TO TRUE
APEX52     ELSE
APEX52         MOVE MIR-UPLD-FLD-FILE-CD-T (1)       
                                      TO RUFLD-UPLD-FLD-FILE-CD
           END-IF.
 
       5360-EDIT-FILE-X.
           EXIT.
      *
      *---------------
       5380-EDIT-UTTB.
      *---------------
 
           IF  MIR-UPLD-TTBL-TYP-ID-T (1)   = SPACES
               MOVE RUFLD-UPLD-TTBL-TYP-ID
                                      TO MIR-UPLD-TTBL-TYP-ID-T (1)
           END-IF.
 
           IF  MIR-UPLD-TTBL-TYP-ID-T (1)   = EBLCH-BLANK-FIELD-CHAR
               MOVE SPACES            TO MIR-UPLD-TTBL-TYP-ID-T (1)
           END-IF.
 
      *
      * UTTB COLUMN MAY HAVE VALID UTTB TYPE OR SPACES
      *
 
APEX54     EVALUATE TRUE
 
APEX54         WHEN COMPLEX-TYPE
APEX54         WHEN UNUSED-TYPE
                    IF  MIR-UPLD-TTBL-TYP-ID-T (1)    = SPACES
                        MOVE SPACES   TO RUFLD-UPLD-TTBL-TYP-ID
                        GO TO 5380-EDIT-UTTB-X
                    END-IF
 
APEX54         WHEN TRANSLATE-TYPE
      *MSG: UTTB COLUMN MUST HAVE VALID VALUE, NOT SPACES
                    IF  MIR-UPLD-TTBL-TYP-ID-T (1)      = SPACES
                        MOVE MIR-UPLD-TTBL-TYP-ID-T (1)
                                      TO WGLOB-MSG-PARM (1)
                        MOVE 'AS20800005'   
                                      TO WGLOB-MSG-REF-INFO
                        PERFORM  0260-1000-GENERATE-MESSAGE
                            THRU 0260-1000-GENERATE-MESSAGE-X
                        SET  WS-EDIT-FAILED    TO TRUE
                        GO TO 5380-EDIT-UTTB-X
                    END-IF
 
APEX54         WHEN OTHER
      *MSG: IF NOT TRANSLATE OR COMPLEX TYPE, UTTB COLUMN MUST BE SPACES
                    IF  MIR-UPLD-TTBL-TYP-ID-T (1) NOT = SPACES
                        MOVE 'AS20800006'   
                                      TO WGLOB-MSG-REF-INFO
                        PERFORM  0260-1000-GENERATE-MESSAGE
                            THRU 0260-1000-GENERATE-MESSAGE-X
                        MOVE SPACES   TO MIR-UPLD-TTBL-TYP-ID-T (1)
                        MOVE SPACES   TO RUFLD-UPLD-TTBL-TYP-ID
                        GO TO 5380-EDIT-UTTB-X
                    ELSE
                        GO TO 5380-EDIT-UTTB-X
                    END-IF
 
APEX54     END-EVALUATE.
 
           MOVE 'UTTB'                TO WUTTB-UPLD-TTBL-TYP-ID.
           MOVE MIR-UPLD-TTBL-TYP-ID-T (1)           
                                      TO WUTTB-UPLD-TTBL-VALU-ID.
 
           PERFORM  UTTB-1000-READ
               THRU UTTB-1000-READ-X.
 
           IF  WUTTB-IO-OK
               MOVE MIR-UPLD-TTBL-TYP-ID-T (1)       
                                      TO RUFLD-UPLD-TTBL-TYP-ID
           ELSE
               MOVE MIR-UPLD-TTBL-TYP-ID-T (1)       
                                      TO WGLOB-MSG-PARM (1)
               MOVE 'AS20800005'      TO WGLOB-MSG-REF-INFO
               PERFORM  0260-1000-GENERATE-MESSAGE
                   THRU 0260-1000-GENERATE-MESSAGE-X
               SET  WS-EDIT-FAILED         TO TRUE
           END-IF.
 
       5380-EDIT-UTTB-X.
           EXIT.
      *
      *---------------------
       5390-EDIT-FIELD-NAME.
      *---------------------
 
           IF  MIR-UPLD-FLD-NM-T (1) = SPACES
               MOVE RUFLD-UPLD-FLD-NM TO MIR-UPLD-FLD-NM-T (1)
               GO TO 5390-EDIT-FIELD-NAME-X
           END-IF.
 
           IF  MIR-UPLD-FLD-NM-T (1) = EBLCH-BLANK-FIELD-CHAR
               MOVE SPACES            TO MIR-UPLD-FLD-NM-T (1)
           END-IF.
 
           MOVE MIR-UPLD-FLD-NM-T (1) TO RUFLD-UPLD-FLD-NM.
 
       5390-EDIT-FIELD-NAME-X.
           EXIT.
      *
      *--------------------
       6000-PROCESS-DELETE.
      *--------------------
 
      *
      * DELETE PROCESSING:
      *
           PERFORM  8000-BUILD-UFLD-KEY
               THRU 8000-BUILD-UFLD-KEY-X.
 
           PERFORM  UFLD-1000-READ-FOR-UPDATE
               THRU UFLD-1000-READ-FOR-UPDATE-X.
 
           IF  WUFLD-IO-OK
               PERFORM  UFLD-1000-DELETE
                   THRU UFLD-1000-DELETE-X
               MOVE 'XS00000011'      TO WGLOB-MSG-REF-INFO
               PERFORM  9100-BLANK-DATA-FIELDS
                   THRU 9100-BLANK-DATA-FIELDS-X
                   VARYING WS-LINE FROM +1 BY +1
                     UNTIL WS-LINE > WS-MAX-LINES
           ELSE
               MOVE WUFLD-KEY         TO WGLOB-MSG-PARM (1)
               MOVE 'XS00000010'      TO WGLOB-MSG-REF-INFO
               PERFORM  0260-1000-GENERATE-MESSAGE
                   THRU 0260-1000-GENERATE-MESSAGE-X
           END-IF.
 
 
       6000-PROCESS-DELETE-X.
           EXIT.
      *
      *--------------------
       8000-BUILD-UFLD-KEY.
      *--------------------
 
           MOVE MIR-UPLD-FLD-STRUCT-NM      TO WUFLD-UPLD-FLD-STRUCT-NM.
           MOVE MIR-UPLD-FLD-APEX-NM  TO WUFLD-UPLD-FLD-APEX-NM.
 
       8000-BUILD-UFLD-KEY-X.
           EXIT.
      *
      *-----------------------
       9100-BLANK-DATA-FIELDS.
      *-----------------------
 
           MOVE SPACES           TO MIR-UPLD-FLD-STRUCT-NM-T (WS-LINE).
           MOVE SPACES           TO MIR-UPLD-FLD-APEX-NM-T (WS-LINE).
           MOVE SPACES           TO MIR-UPLD-FLD-TYP-CD-T (WS-LINE).
           MOVE SPACES           TO MIR-UPLD-FLD-FILE-CD-T (WS-LINE).
           MOVE SPACES           TO MIR-UPLD-FLD-NM-T (WS-LINE).
           MOVE SPACES           TO MIR-UPLD-TTBL-TYP-ID-T (WS-LINE).
 
       9100-BLANK-DATA-FIELDS-X.
           EXIT.
      *
      *---------------------------
       9200-MOVE-RECORD-TO-SCREEN.
      *---------------------------
 
           MOVE RUFLD-UPLD-FLD-STRUCT-NM  
                                  TO MIR-UPLD-FLD-STRUCT-NM-T (WS-LINE).
           MOVE RUFLD-UPLD-FLD-APEX-NM    
                                    TO MIR-UPLD-FLD-APEX-NM-T (WS-LINE).
           MOVE RUFLD-UPLD-FLD-TYP-CD     
                                     TO MIR-UPLD-FLD-TYP-CD-T (WS-LINE).
           MOVE RUFLD-UPLD-FLD-FILE-CD    
                                    TO MIR-UPLD-FLD-FILE-CD-T (WS-LINE).
           MOVE RUFLD-UPLD-FLD-NM     TO MIR-UPLD-FLD-NM-T (WS-LINE).
           MOVE RUFLD-UPLD-TTBL-TYP-ID    
                                    TO MIR-UPLD-TTBL-TYP-ID-T (WS-LINE).
 
       9200-MOVE-RECORD-TO-SCREEN-X.
           EXIT.
      *
      *--------------------------
       9300-SETUP-MSIN-REFERENCE.
      *--------------------------
 
           MOVE SPACES                TO WGLOB-MSIN-REFERENCE.
           MOVE WGLOB-COMPANY-CODE    TO WGLOB-REF-COMPANY-CODE.
 
       9300-SETUP-MSIN-REFERENCE-X.
           EXIT.
      *
      ****************************************************************
      * PROCESSING COPYBOOKS                                         *
      ****************************************************************
       COPY XCCL0260.
      *
      ****************************************************************
      * GENERAL & MESSAGE PROCESSING COPYBOOKS                       *
      ****************************************************************
      *
       COPY XCPPINIT.
      *
       COPY XCPPEXIT.
      *
014660*COPY XCPPMEXT.
      *
      *****************************************************************
      *  FILE I/O PROCESS MODULES
      *****************************************************************
       COPY ACPAUFLD.
       COPY ACPBUFLD.
       COPY ACPNUFLD.
       COPY ACPUUFLD.
       COPY ACPXUFLD.
       COPY ACPCUFLD.
      *
       COPY ACPNUTTB.
      *
557708*COPY XCCPHNDL.
557708 COPY XCCPABND.
       COPY XCCP0030.

      *****************************************************************
      **                 END OF PROGRAM ASOM2080                     **
      *****************************************************************
