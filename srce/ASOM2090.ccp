      *************************
       IDENTIFICATION DIVISION.
      *************************
 
       PROGRAM-ID. ASOM2090.
 
       COPY XCWWCRHT.
 
      *****************************************************************
      **  MEMBER :  ASOM2090                                         **
      **  REMARKS:  PROCESS DRIVER FOR UPLOAD TRANSLATION TABLE      **
      **            TRANSACTION UTTB.                                **
      **                                                             **
      **  DOMAIN :  SY                                               **
      **  CLASS  :  PD                                               **
     *****************************************************************
      **  MEMBER :  ASOM2090                                         **
      **  REMARKS:  PROCESS DRIVER FOR UPLOAD TRANSLATION TABLE      **
      **            TRANSACTION UTTB.                                **
      **                                                             **
      **  DOMAIN :  SY                                               **
      **  CLASS  :  PD                                               **
      *****************************************************************
      **  DATE     AUTH.  DESCRIPTION                                **
      **                                                             **
APEX52**  30NOV94  JJS    UPGRADE TO RELEASE 5.2,                    **
APEX52**                  COMBINED MESSAGES INTO ONE LINE            **
APEX53**  30NOV95  JJS    UPGRADE TO INGENIUM 5.3 & WINAPEX 1.0,     **
APEX53**                  ARCHITECTURE CHANGES TO SUPPORT GUI,       **
APEX53**                  CHANGES TO SUPPORT I/O PROGRAMS,           **
APEX53**                  SET MORE DATA INDICATOR FOR GUI INTERFACE  **
APEX54**  31DEC96  TJS    MODIFICATIONS FOR MAINTAINABILITY          **
54-100**  31DEC96  SLB    CHANGE INQUIRE TO BROWSE                   **
557708**  30SEP97  LLE    HANDLING OF CICS ABENDSE                   **
007766**  30OCT98  56     ARCHITECTURE CHANGES TO SUPPORT PASSING    **
007766**                  PARAMETERS VIA AN ADDRESS                  **
013578** 15DEC99   60     REMOVAL OF 3270 LOGIC, MIR RENAMES         **
014660** 31DEC1999 60     REMOVE XCPPMEXT                            **
014590**  15DEC99  60     ARCHITECTURAL CHANGES                      **
      *****************************************************************
 
      *
      **********************
       ENVIRONMENT DIVISION.
      **********************
 
      ***************
       DATA DIVISION.
      ***************
 
       WORKING-STORAGE SECTION.
 
APEX53 COPY XCWWPGWS REPLACING '$VAR1' BY 'ASOM2090'.
 
       COPY SQLCA.
007766 COPY XCWLPTR.
 
 
014590*COPY XCWL0030.
 
       01  WS-PGM-WORK-AREA.
           05  WS-EDIT-CHECKS.
               10  WS-BUS-FCN-ID              PIC X(04).
                   88  WS-BUS-FCN-VALID       VALUE '2090' '2091' '2092'
                                                    '2093' '2094'.
                   88  WS-BUS-FCN-RETRIEVE    VALUE '2090'.
                   88  WS-BUS-FCN-CREATE      VALUE '2091'.
                   88  WS-BUS-FCN-UPDATE      VALUE '2092'.
                   88  WS-BUS-FCN-DELETE      VALUE '2093'.
                   88  WS-BUS-FCN-LIST        VALUE '2094'.
 
           05  WS-VALIDATE-FAIL-SW            PIC X(01).
               88  WS-VALIDATE-FAILED         VALUE 'Y'.
APEX54         88  WS-VALIDATE-FAILED-NOT     VALUE 'N'.
 
           05  WS-SUB                         PIC S9(04) COMP.
           05  WS-LINE                        PIC S9(04) COMP.
           05  WS-MAX-ARRAY-LINES             PIC S9(04) COMP VALUE +12.
 
      *
      *****************************************************************
      *  COMMON COPYBOOKS                                             *
      *****************************************************************
       COPY XCWEBLCH.
      *
      *****************************************************************
      *  I/O COPYBOOKS                                                *
      *****************************************************************
       COPY ACFWUTTB.
      *
       COPY ACFRUTTB.
      *
      *****************************************************************
      *  CALLED MODULE PARAMETER INFORMATION                          *
      *****************************************************************
      *
      *****************************************************************
      *  INPUT PARAMETER INFORMATION                                  *
      *****************************************************************
      *
007766*01  WGLOB-GLOBAL-AREA.
007766*COPY XCWWGLOB.
      *
      *****************
       LINKAGE SECTION.
      *****************
 
007766 01 WGLOB-GLOBAL-AREA.
007766 COPY XCWWGLOB.
       COPY ACWM2090.
      *
       PROCEDURE DIVISION USING WGLOB-GLOBAL-AREA
                                MIR-PARM-AREA.
 
      *--------------
       0000-MAINLINE.
      *--------------
 
557708     PERFORM  ABND-1000-HANDLE-ABEND
557708         THRU ABND-1000-HANDLE-ABEND-X.
557708
           PERFORM  INIT-1000-INITIALIZE
               THRU INIT-1000-INITIALIZE-X.
 
           PERFORM  2000-PROCESS-REQUEST
               THRU 2000-PROCESS-REQUEST-X.
 
           PERFORM  EXIT-1000-FINALIZE
               THRU EXIT-1000-FINALIZE-X.
 
       0000-MAINLINE-X.
           GOBACK.
      *
      *--------------------------
       2000-PROCESS-REQUEST.
      *--------------------------
 
           MOVE MIR-BUS-FCN-ID        TO WS-BUS-FCN-ID.
 
           PERFORM  9300-SETUP-MSIN-REFERENCE
               THRU 9300-SETUP-MSIN-REFERENCE-X.
 
      *
      * PROCESS SCREEN FUNCTIONS
      *
 
APEX54     EVALUATE TRUE
 
54-100         WHEN WS-BUS-FCN-LIST      
54-100              PERFORM  3000-BROWSE
54-100                  THRU 3000-BROWSE-X
 
013578         WHEN WS-BUS-FCN-RETRIEVE  
013578              PERFORM  3500-PROCESS-RETRIEVE
013578                  THRU 3500-PROCESS-RETRIEVE-X
 
APEX54         WHEN WS-BUS-FCN-CREATE
                    PERFORM  4000-CREATE
                        THRU 4000-CREATE-X
 
APEX54         WHEN WS-BUS-FCN-UPDATE 
                    PERFORM  5000-PROCESS-UPDATE
                        THRU 5000-PROCESS-UPDATE-X
 
APEX54         WHEN WS-BUS-FCN-DELETE
                    PERFORM  6000-PROCESS-DELETE
                        THRU 6000-PROCESS-DELETE-X
APEX54     END-EVALUATE.
 
       2000-PROCESS-REQUEST-X.
           EXIT.
      *
      *-------------
54-100 3000-BROWSE.
      *-------------
 
      *
      * BROWSE PROCESSING:  SETUP BROWSE KEYS, BEGIN BROWSE, AND
      * LOAD DATA ARRAY UNTIL END-OF-FILE OR SCREEN IS FULL.
      *
 
           PERFORM  9100-BLANK-DATA-FIELDS
               THRU 9100-BLANK-DATA-FIELDS-X.
 
           IF  MIR-UPLD-TTBL-TYP-ID = SPACES
               MOVE 'AS20900001'      TO WGLOB-MSG-REF-INFO
               PERFORM  0260-1000-GENERATE-MESSAGE
                   THRU 0260-1000-GENERATE-MESSAGE-X
54-100         GO TO 3000-BROWSE-X
           END-IF.
 
           MOVE LOW-VALUES            TO WUTTB-KEY.
           MOVE HIGH-VALUES           TO WUTTB-ENDBR-KEY.
           MOVE MIR-UPLD-TTBL-TYP-ID  TO WUTTB-UPLD-TTBL-TYP-ID
           MOVE MIR-UPLD-TTBL-TYP-ID  TO WUTTB-ENDBR-UPLD-TTBL-TYP-ID.
 
           IF  MIR-UPLD-TTBL-VALU-ID NOT = SPACES
               MOVE MIR-UPLD-TTBL-VALU-ID         
                                      TO WUTTB-UPLD-TTBL-VALU-ID
           END-IF.
 
           PERFORM  UTTB-1000-BROWSE
               THRU UTTB-1000-BROWSE-X.
 
           IF  WUTTB-IO-EOF
               MOVE 'XS00000034'      TO WGLOB-MSG-REF-INFO
               PERFORM  0260-1000-GENERATE-MESSAGE
                   THRU 0260-1000-GENERATE-MESSAGE-X
54-100         GO TO 3000-BROWSE-X
           END-IF.
 
           PERFORM  UTTB-2000-READ-NEXT
               THRU UTTB-2000-READ-NEXT-X.
 
           IF  WUTTB-IO-EOF
               MOVE 'XS00000034'      TO WGLOB-MSG-REF-INFO
               PERFORM  0260-1000-GENERATE-MESSAGE
                   THRU 0260-1000-GENERATE-MESSAGE-X
               PERFORM  UTTB-3000-END-BROWSE
                   THRU UTTB-3000-END-BROWSE-X
54-100         GO TO 3000-BROWSE-X
           END-IF.
 
           PERFORM  3100-DISPLAY-RECORD
               THRU 3100-DISPLAY-RECORD-X
               VARYING WS-LINE FROM +1 BY +1
               UNTIL WUTTB-IO-EOF
               OR WS-LINE > WS-MAX-ARRAY-LINES.
 
           IF  WUTTB-IO-EOF
54-100* MSG: END OF LIST
54-100         MOVE 'XS00000015'      TO WGLOB-MSG-REF-INFO
               PERFORM  0260-1000-GENERATE-MESSAGE
                   THRU 0260-1000-GENERATE-MESSAGE-X
013578     ELSE
               MOVE WUTTB-UPLD-TTBL-VALU-ID
                                      TO MIR-UPLD-TTBL-VALU-ID         
           END-IF.
 
           PERFORM  UTTB-3000-END-BROWSE
               THRU UTTB-3000-END-BROWSE-X.
 
54-100 3000-BROWSE-X.
           EXIT.
      *
      *--------------------
       3100-DISPLAY-RECORD.
      *--------------------
 
           PERFORM  9200-MOVE-RECORD-TO-SCREEN
               THRU 9200-MOVE-RECORD-TO-SCREEN-X.
 
           PERFORM  UTTB-2000-READ-NEXT
               THRU UTTB-2000-READ-NEXT-X.
 
       3100-DISPLAY-RECORD-X.
           EXIT.
      *
013578*----------------------
       3500-PROCESS-RETRIEVE.
      *----------------------

           PERFORM  7100-BUILD-UTTB-KEY
               THRU 7100-BUILD-UTTB-KEY-X.
 
           PERFORM  UTTB-1000-READ
               THRU UTTB-1000-READ-X.
 
           PERFORM  9100-BLANK-DATA-FIELDS
               THRU 9100-BLANK-DATA-FIELDS-X.
 
           IF  WUTTB-IO-NOT-FOUND
               MOVE WUTTB-KEY         TO WGLOB-MSG-PARM (1)
               MOVE 'XS00000001'      TO WGLOB-MSG-REF-INFO
               PERFORM  0260-1000-GENERATE-MESSAGE
                   THRU 0260-1000-GENERATE-MESSAGE-X
           ELSE
               MOVE +1                TO WS-LINE
               PERFORM  9200-MOVE-RECORD-TO-SCREEN
                   THRU 9200-MOVE-RECORD-TO-SCREEN-X
           END-IF.

       3500-PROCESS-RETRIEVE-X.
013578     EXIT.
      *------------
       4000-CREATE.
      *------------
 
      *
      * CREATE PROCESSING:  CHECK IF RECORD DOES NOT EXIST, INIT
      * NEW RECORD AND ALLOW USER TO MODIFY.
      *
 
           PERFORM  7000-VALIDATE-CONTROL-FIELDS
               THRU 7000-VALIDATE-CONTROL-FIELDS-X.
 
           IF  WS-VALIDATE-FAILED
               GO TO 4000-CREATE-X
           END-IF.
 
           PERFORM  9100-BLANK-DATA-FIELDS
               THRU 9100-BLANK-DATA-FIELDS-X.
 
           PERFORM  7100-BUILD-UTTB-KEY
               THRU 7100-BUILD-UTTB-KEY-X.
 
           PERFORM  UTTB-1000-READ
               THRU UTTB-1000-READ-X.
 
           IF  WUTTB-IO-OK
               MOVE WUTTB-KEY         TO WGLOB-MSG-PARM (1)
               MOVE 'XS00000003'      TO WGLOB-MSG-REF-INFO
               PERFORM  0260-1000-GENERATE-MESSAGE
                   THRU 0260-1000-GENERATE-MESSAGE-X
           ELSE
               PERFORM  UTTB-1000-CREATE
                   THRU UTTB-1000-CREATE-X
               PERFORM  UTTB-1000-WRITE
                   THRU UTTB-1000-WRITE-X
               MOVE 'XS00000004'      TO WGLOB-MSG-REF-INFO
               PERFORM  0260-1000-GENERATE-MESSAGE
                   THRU 0260-1000-GENERATE-MESSAGE-X
               MOVE +1                TO WS-LINE
               PERFORM  9200-MOVE-RECORD-TO-SCREEN
                   THRU 9200-MOVE-RECORD-TO-SCREEN-X
           END-IF.
 
       4000-CREATE-X.
           EXIT.
      *
      *--------------------
       5000-PROCESS-UPDATE.
      *--------------------
 
           PERFORM  7100-BUILD-UTTB-KEY
               THRU 7100-BUILD-UTTB-KEY-X.
 
           PERFORM  UTTB-1000-READ-FOR-UPDATE
               THRU UTTB-1000-READ-FOR-UPDATE-X.
 
           IF  WUTTB-IO-NOT-FOUND
               MOVE WUTTB-KEY         TO WGLOB-MSG-PARM (1)
               MOVE 'XS00000001'      TO WGLOB-MSG-REF-INFO
               PERFORM  0260-1000-GENERATE-MESSAGE
                   THRU 0260-1000-GENERATE-MESSAGE-X
               GO TO 5000-PROCESS-UPDATE-X
           END-IF.
 
           IF  MIR-UPLD-TTBL-VALU-TXT-T (1) = SPACES
               MOVE RUTTB-UPLD-TTBL-VALU-TXT
                                      TO MIR-UPLD-TTBL-VALU-TXT-T (1)
           ELSE
               IF  MIR-UPLD-TTBL-VALU-TXT-T (1) = EBLCH-BLANK-FIELD-CHAR
                   MOVE SPACES        TO MIR-UPLD-TTBL-VALU-TXT-T (1)
               END-IF
           END-IF.
 
           MOVE MIR-UPLD-TTBL-VALU-TXT-T (1)           
                                      TO RUTTB-UPLD-TTBL-VALU-TXT.
 
           PERFORM  UTTB-2000-REWRITE
               THRU UTTB-2000-REWRITE-X.
 
           MOVE 'XS00000008'          TO WGLOB-MSG-REF-INFO.
           PERFORM  0260-1000-GENERATE-MESSAGE
               THRU 0260-1000-GENERATE-MESSAGE-X.
 
       5000-PROCESS-UPDATE-X.
           EXIT.
      *
      *--------------------
       6000-PROCESS-DELETE.
      *--------------------
      *
      * DELETE PROCESSING:
      *
           PERFORM  7100-BUILD-UTTB-KEY
               THRU 7100-BUILD-UTTB-KEY-X.
 
           PERFORM  UTTB-1000-READ-FOR-UPDATE
               THRU UTTB-1000-READ-FOR-UPDATE-X.
 
           IF  WUTTB-IO-OK
               PERFORM  UTTB-1000-DELETE
                   THRU UTTB-1000-DELETE-X
               PERFORM  9100-BLANK-DATA-FIELDS
                   THRU 9100-BLANK-DATA-FIELDS-X
               MOVE 'XS00000011'      TO WGLOB-MSG-REF-INFO
               PERFORM  0260-1000-GENERATE-MESSAGE
                   THRU 0260-1000-GENERATE-MESSAGE-X
           ELSE
               MOVE WUTTB-KEY         TO WGLOB-MSG-PARM (1)
               MOVE 'XS00000010'      TO WGLOB-MSG-REF-INFO
               PERFORM  0260-1000-GENERATE-MESSAGE
                   THRU 0260-1000-GENERATE-MESSAGE-X
           END-IF.
 
       6000-PROCESS-DELETE-X.
           EXIT.
      *
      *-----------------------------
       7000-VALIDATE-CONTROL-FIELDS.
      *-----------------------------
 
APEX54     SET WS-VALIDATE-FAILED-NOT       TO TRUE.
 
           IF  MIR-UPLD-TTBL-TYP-ID = SPACES
               MOVE 'AS20900002'      TO WGLOB-MSG-REF-INFO
               PERFORM  0260-1000-GENERATE-MESSAGE
                   THRU 0260-1000-GENERATE-MESSAGE-X
APEX54         SET WS-VALIDATE-FAILED       TO TRUE
           ELSE
APEX54         PERFORM  7050-VALIDATE-UTTP-FIELD
APEX54             THRU 7050-VALIDATE-UTTP-FIELD-X
           END-IF.
 
       7000-VALIDATE-CONTROL-FIELDS-X.
           EXIT.
      *
      *-------------------------
APEX54 7050-VALIDATE-UTTP-FIELD.
      *-------------------------
 
           IF  MIR-UPLD-TTBL-TYP-ID = 'UTTB'
 
      *
      * DO NOT EDIT TYPE WHEN CREATING AN INDEX
      *
               CONTINUE
           ELSE
               MOVE 'UTTB'            TO WUTTB-UPLD-TTBL-TYP-ID
               MOVE MIR-UPLD-TTBL-TYP-ID              
                                      TO WUTTB-UPLD-TTBL-VALU-ID
               PERFORM  UTTB-1000-READ
                   THRU UTTB-1000-READ-X
               IF  WUTTB-IO-OK
                   CONTINUE
               ELSE
                   MOVE MIR-UPLD-TTBL-TYP-ID          
                                      TO WGLOB-MSG-PARM (1)
                   MOVE 'AS20900003'  TO WGLOB-MSG-REF-INFO
                   PERFORM  0260-1000-GENERATE-MESSAGE
                       THRU 0260-1000-GENERATE-MESSAGE-X
APEX54             SET WS-VALIDATE-FAILED   TO TRUE
               END-IF
           END-IF.
 
APEX54 7050-VALIDATE-UTTP-FIELD-X.
           EXIT.
      *
      *--------------------
       7100-BUILD-UTTB-KEY.
      *--------------------
 
           MOVE MIR-UPLD-TTBL-TYP-ID  TO WUTTB-UPLD-TTBL-TYP-ID.
           MOVE MIR-UPLD-TTBL-VALU-ID TO WUTTB-UPLD-TTBL-VALU-ID.
 
       7100-BUILD-UTTB-KEY-X.
           EXIT.
      *
      *-----------------------
       9100-BLANK-DATA-FIELDS.
      *-----------------------
 
           PERFORM  9110-BLANK-LINE
               THRU 9110-BLANK-LINE-X
               VARYING WS-LINE FROM +1 BY +1
               UNTIL WS-LINE > WS-MAX-ARRAY-LINES.
 
       9100-BLANK-DATA-FIELDS-X.
           EXIT.
      *
      *----------------
       9110-BLANK-LINE.
      *----------------
 
           MOVE SPACES              TO MIR-UPLD-TTBL-TYP-ID-T (WS-LINE).
           MOVE SPACES             TO MIR-UPLD-TTBL-VALU-ID-T (WS-LINE).
           MOVE SPACES            TO MIR-UPLD-TTBL-VALU-TXT-T (WS-LINE).
 
       9110-BLANK-LINE-X.
           EXIT.
      *
      *---------------------------
       9200-MOVE-RECORD-TO-SCREEN.
      *---------------------------
 
           MOVE RUTTB-UPLD-TTBL-TYP-ID     
                                    TO MIR-UPLD-TTBL-TYP-ID-T (WS-LINE).
           MOVE RUTTB-UPLD-TTBL-VALU-ID    
                                   TO MIR-UPLD-TTBL-VALU-ID-T (WS-LINE).
           MOVE RUTTB-UPLD-TTBL-VALU-TXT   
                                  TO MIR-UPLD-TTBL-VALU-TXT-T (WS-LINE).
 
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
       COPY XCPPINIT.
      *
       COPY XCPPEXIT.
      *
014660*COPY XCPPMEXT.
      *
      ****************************************************************
      * LINKAGE PROCESSING COPYBOOKS                                 *
      ****************************************************************
       COPY XCCL0260.
      *
      *****************************************************************
      *  FILE I/O PROCESS MODULES
      *****************************************************************
       COPY ACPAUTTB.
      *
       COPY ACPBUTTB.
      *
       COPY ACPCUTTB.
      *
       COPY ACPNUTTB.
      *
       COPY ACPUUTTB.
      *
       COPY ACPXUTTB.
      *
      *****************************************************************
      *  ERROR HANDLING ROUTINES
      *****************************************************************
557708*COPY XCCPHNDL.
557708 COPY XCCPABND.
      *
       COPY XCCP0030.
 
      *****************************************************************
      **                 END OF PROGRAM ASOM2090                     **
      *****************************************************************
