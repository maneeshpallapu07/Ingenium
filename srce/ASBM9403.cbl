
      *************************
       IDENTIFICATION DIVISION.
      *************************

       PROGRAM-ID. ASBM9403.

       COPY XCWWCRHT.

      *****************************************************************
      **  MEMBER :  ASBM9403                                         **   
      **  REMARKS:  XML & CWA UPLOADS FOR BUNDLE PROCESSING          **   
      **                                                             **   
      **  DOMAIN :  AC                                               **   
      **  CLASS  :  PD                                               **
      *****************************************************************
      **  DATE       AUTH.   DESCRIPTION                             **
TLB003**  13Apr21     CTS    CHANGES FOR NEW BUSINESS                **
      *****************************************************************

      
      **********************
       ENVIRONMENT DIVISION.
      **********************

       CONFIGURATION SECTION.

      ***************
       DATA DIVISION.
      ***************

       FILE SECTION.

       WORKING-STORAGE SECTION.

       COPY XCWWPGWS REPLACING '$VAR1' BY 'ASBM9403'.

       COPY SQLCA.

       COPY XCWWCVGM.
       
       01  WS-WORK-AREA.
           05 WS-BUNDLE-APP-ID                 PIC X(15).
           05 WS-BUNDLE-APP-ID-N               REDEFINES
              WS-BUNDLE-APP-ID                 PIC 9(15).           
           05 WS-HOLD-POL-ID                   PIC X(07).
           05 WS-BUNDLE-POL-IND                PIC X(01).
              88 WS-BUNDLE-POL-YES             VALUE 'Y'.
              88 WS-BUNDLE-POL-NO              VALUE 'N'.
           05 WS-BUNDLE-APP-IND                PIC X(01).
              88 WS-BUNDLE-APP-YES             VALUE 'Y'.
              88 WS-BUNDLE-APP-NO              VALUE 'N'.
           05 WS-HI-BUNDLE-APP-CHK             PIC X(15)
                                              VALUE '999999999999999'.
                                              
      *****************************************************************
      *  COMMON COPYBOOKS                                             *
      *****************************************************************
       COPY CCWL2430.
      
       COPY XCWL0040.
       COPY CCWL0950.
       COPY CCWL0010.
       COPY XCWL0035.
       COPY XCWLDTLK.
       COPY CCWWCCC.
       COPY CCWWCVGS.
       COPY CCFHCVGS.
       COPY CCWWINDX.
      /
       COPY XCWTFCMD.
      /
       COPY XCWWHDG.
       COPY XCWWTIME.
       COPY XCWWWKDT.
      / 

      *****************************************************************
      *  I/O COPYBOOKS                                                *
      *****************************************************************
       COPY XCSWPRT  REPLACING ==:ID:==  BY OCF
                               ==':ID:'==  BY =='OCF'==.
       COPY XCSROCF.
      /      
       COPY ACSR9403.
       COPY XCSRBCF.
       COPY CCFRMAST.
       COPY CCFWMAST.
      /   
       COPY CCFWPOL.
       COPY CCFRPOL.
      /
       COPY CCFWPCOM.
       COPY CCFRPCOM.
      /
       COPY ACFRCLN1.
       COPY ACFRCLN2.
       COPY ACFWCLN1.
       COPY ACFWCLN2.
      /
       COPY XCSWSEQ  REPLACING ==:ID:==  BY BCF
                               ==':ID:'==  BY =='BCF'==.
      /
       COPY XCSWSEQ  REPLACING ==:ID:==  BY 9403
                               ==':ID:'==  BY =='9403'==.  
      /           
      *****************************************************************
      *  CALLED MODULE PARAMETER INFORMATION                          *
      *****************************************************************
      
       01  WGLOB-GLOBAL-AREA.
       COPY XCWWGLOB.
      / 
      ********************
       PROCEDURE DIVISION.
      *******************

  
      *--------------
       0000-MAINLINE.
      *--------------

           PERFORM  0100-OPEN-FILES
               THRU 0100-OPEN-FILES-X.
 
           PERFORM  0200-INITIALIZE-VALUES
               THRU 0200-INITIALIZE-VALUES-X.

           PERFORM  2000-PROCESS-REPORT
               THRU 2000-PROCESS-REPORT-X
               UNTIL W9403-SEQ-IO-EOF.
           
           IF  WS-BUNDLE-APP-YES
               PERFORM  4000-UPDATE-TPCOM
                   THRU 4000-UPDATE-TPCOM-X
           END-IF.

           PERFORM  9999-CLOSE-FILES
               THRU 9999-CLOSE-FILES-X.
               
           PERFORM  0035-1000-COMMIT
               THRU 0035-1000-COMMIT-X.
     
           STOP RUN.

       0000-MAINLINE-X.
           EXIT.
           
      /    
      *----------------
       0100-OPEN-FILES.
      *----------------

           PERFORM  OCF-3000-OPEN-OUTPUT
               THRU OCF-3000-OPEN-OUTPUT-X.

           PERFORM  BCF-1000-OPEN-INPUT
               THRU BCF-1000-OPEN-INPUT-X.

           PERFORM  9403-1000-OPEN-INPUT
               THRU 9403-1000-OPEN-INPUT-X.

       0100-OPEN-FILES-X.
           EXIT.
      /
      *-----------------------
       0200-INITIALIZE-VALUES.
      *-----------------------

           PERFORM  CCC-1000-PRCES-CO-CTL-CARD
               THRU CCC-1000-PRCES-CO-CTL-CARD-X.

           PERFORM  0950-0000-INIT-PARM-INFO
               THRU 0950-0000-INIT-PARM-INFO-X.

           PERFORM  0950-1000-GET-COMPANY-NAME
               THRU 0950-1000-GET-COMPANY-NAME-X.

           PERFORM  HDG-1000-INIT-CONSTANTS
               THRU HDG-1000-INIT-CONSTANTS-X.

           PERFORM  HDG-2000-INIT-SBSDRY-CO
               THRU HDG-2000-INIT-SBSDRY-CO-X.  
           
           PERFORM  MAST-1000-READ
               THRU MAST-1000-READ-X.
              
           IF  WMAST-IO-OK
               MOVE RMAST-APPL-CTL-PRCES-DT TO WGLOB-PROCESS-DATE
           ELSE
      *    MSG: 'MASTER CONTROL RECORD (@1) NOT FOUND'
               MOVE WMAST-KEY               TO WGLOB-MSG-PARM (1)
               MOVE 'CS94030001'            TO WGLOB-MSG-REF-INFO
               PERFORM  0260-1000-GENERATE-MESSAGE
                   THRU 0260-1000-GENERATE-MESSAGE-X
               PERFORM  0030-5000-LOGIC-ERROR
                   THRU 0030-5000-LOGIC-ERROR-X
           END-IF.            

      *    OBTAIN SYSTEM ID           

           MOVE 'XS00000145'                TO WGLOB-MSG-REF-INFO.
           PERFORM  0260-2000-GET-MESSAGE
               THRU 0260-2000-GET-MESSAGE-X.
           MOVE WGLOB-MSG-TXT               TO L0040-SYSTEM-ID
                                               WHDG-SYSTEM-ID.
           MOVE L0950-COMPANY-NAME          TO L0040-COMPANY-NAME.

      *    GET THE DETAIL HEADINGS 'ERROR MESSAGES'
           MOVE 'XS00000019'                TO WGLOB-MSG-REF-INFO.
     
           PERFORM  0260-2000-GET-MESSAGE
               THRU 0260-2000-GET-MESSAGE-X.
     
           MOVE WGLOB-MSG-TXT               TO L0040-HDG-LINE-3.
     
           PERFORM  0040-1000-INIT-TITLE
               THRU 0040-1000-INIT-TITLE-X.

      *    POPULATE HEADING VALUES

           MOVE L0950-COMPANY-NAME          TO WHDG-COMPANY-NAME.
           MOVE L0950-COMPANY-TIME          TO WHDG-TIME.
           
           MOVE WGLOB-PROCESS-DATE          TO WHDG-DATE.
           
           SET  WS-BUNDLE-APP-NO            TO TRUE.
           PERFORM  9403-1000-READ
               THRU 9403-1000-READ-X.

           IF NOT W9403-SEQ-IO-OK
      *    MSG:'INPUT FILE EMPTY'
               MOVE  'CS94030002'           TO  WGLOB-MSG-REF-INFO
               PERFORM  0260-1000-GENERATE-MESSAGE
                   THRU 0260-1000-GENERATE-MESSAGE-X  
               GO TO 0200-INITIALIZE-VALUES-X
           END-IF.    
           MOVE ZEROES                      TO WS-BUNDLE-APP-ID.
           PERFORM  PCOM-1000-READ
               THRU PCOM-1000-READ-X.
        
           MOVE RPCOM-NXT-BUNDLE-APP-ID     TO WS-BUNDLE-APP-ID.
      *    MSG:BUNDLE ID STARTS FROM @1          
           MOVE  'CS94030003'               TO WGLOB-MSG-REF-INFO.
           MOVE RPCOM-NXT-BUNDLE-APP-ID     TO WGLOB-MSG-PARM (1).
           PERFORM  0260-1000-GENERATE-MESSAGE
               THRU 0260-1000-GENERATE-MESSAGE-X.

       0200-INITIALIZE-VALUES-X.
           EXIT.
      /
      *--------------------
       2000-PROCESS-REPORT.
      *--------------------
      
           PERFORM  3000-ASSIGN-BUNDLE-ID
               THRU 3000-ASSIGN-BUNDLE-ID-X.
      *    INPUT FILE READ
      
           PERFORM  9403-1000-READ
               THRU 9403-1000-READ-X.
           
       2000-PROCESS-REPORT-X.
           EXIT.
      /       
      *----------------------
       3000-ASSIGN-BUNDLE-ID.
      *----------------------
     
           INITIALIZE WS-HOLD-POL-ID.
      *    MSGS: NOW PROCESSING POLICY(@1)
            
           MOVE 'CS94030001'                TO WGLOB-MSG-REF-INFO.
           MOVE R9403-POL-ID                TO WGLOB-MSG-PARM (1).
          
           PERFORM  0260-1000-GENERATE-MESSAGE
               THRU 0260-1000-GENERATE-MESSAGE-X.
               
           MOVE  R9403-POL-ID               TO WPOL-POL-ID.
           
           PERFORM  POL-1000-READ
               THRU POL-1000-READ-X.
           
           IF NOT WPOL-IO-OK
      *    MSG: 'POLICY (@1) IS NOT IN TPOL SO EXCLUDED'    
               MOVE 'CS94030002'            TO WGLOB-MSG-REF-INFO
               MOVE R9403-POL-ID            TO WGLOB-MSG-PARM (1)
               PERFORM  0260-1000-GENERATE-MESSAGE
                   THRU 0260-1000-GENERATE-MESSAGE-X        
               GO TO 3000-ASSIGN-BUNDLE-ID-X
           END-IF.
      
      *    ASSIGNING OF BUNDLE APPLICATION ID STARTS HERE    
      
           IF  RPOL-POL-BUNDLE-APP-ID NOT = SPACES
               GO TO 3000-ASSIGN-BUNDLE-ID-X
           END-IF.
           
           MOVE RPOL-POL-ID                 TO WS-HOLD-POL-ID
           MOVE ZEROS                       TO I.
          
      *   FETCHING CLIENT OWNER NAMES   
           PERFORM  2430-1000-BUILD-PARM-INFO
               THRU 2430-1000-BUILD-PARM-INFO-X.
               
           PERFORM  2430-2100-GET-OWNER
               THRU 2430-2100-GET-OWNER-X.
         
           IF NOT L2430-RETRN-OK
              GO TO 3000-ASSIGN-BUNDLE-ID-X
           END-IF.
		   
      *   CHECKS WHETHER THE POLICY IS A COMPANY OR CLIENT     
               
           IF  L2430-CLI-SEX-COMPANY  
               PERFORM  3100-BROWSE-CO-CLI-DTLS
                   THRU 3100-BROWSE-CO-CLI-DTLS-X
           ELSE     
               PERFORM  3300-BROWSE-CLI-DTLS
                   THRU 3300-BROWSE-CLI-DTLS-X
           END-IF.        

           IF  WS-BUNDLE-POL-NO
               GO TO 3000-ASSIGN-BUNDLE-ID-X
           END-IF.
       
      * CHECKS WHETHER MAXIMUM NEXT BUNDLE APPLICATION IS REACHED 
           IF  WS-BUNDLE-APP-ID-N = WS-HI-BUNDLE-APP-CHK
               MOVE 'CS94030005'            TO WGLOB-MSG-REF-INFO
               MOVE R9403-POL-ID            TO WGLOB-MSG-PARM (1)
               PERFORM  0260-1000-GENERATE-MESSAGE
                   THRU 0260-1000-GENERATE-MESSAGE-X             
               GO TO 3000-ASSIGN-BUNDLE-ID-X
           END-IF.           

           ADD +1                           TO WS-BUNDLE-APP-ID-N.
           
       3000-ASSIGN-BUNDLE-ID-X.
           EXIT.
      /
      *------------------------
       3100-BROWSE-CO-CLI-DTLS.
      *------------------------

           SET  WS-BUNDLE-POL-NO           TO TRUE.
           MOVE L2430-CLI-CO-NM            TO WCLN2-CLI-NM.
           MOVE L2430-KJ-CO-NM             TO WCLN2-CLI-KJ-NM.  
           MOVE WGLOB-PROCESS-DATE         TO WCLN2-CTL-PRCES-DT.
           MOVE WCLN2-KEY                  TO WCLN2-ENDBR-KEY.
                                   
           PERFORM  CLN2-1000-BROWSE
               THRU CLN2-1000-BROWSE-X.

           IF NOT WCLN2-IO-OK
               GO TO 3100-BROWSE-CO-CLI-DTLS-X
           END-IF.
            
           PERFORM  CLN2-2000-READ-NEXT
               THRU CLN2-2000-READ-NEXT-X.
         
           IF  WCLN2-IO-OK
           AND RCLN2-POL-ID =  WS-HOLD-POL-ID
               PERFORM  CLN2-2000-READ-NEXT
                   THRU CLN2-2000-READ-NEXT-X
               IF  WCLN2-IO-EOF
                   PERFORM  CLN2-3000-END-BROWSE
                       THRU CLN2-3000-END-BROWSE-X
                   SET  WS-BUNDLE-POL-NO    TO TRUE
                   GO TO 3100-BROWSE-CO-CLI-DTLS-X
               ELSE
                   MOVE WS-HOLD-POL-ID      TO WPOL-POL-ID
                   PERFORM  POL-1000-READ-FOR-UPDATE
                       THRU POL-1000-READ-FOR-UPDATE-X
                   SET  WS-BUNDLE-APP-YES   TO TRUE
                   MOVE WS-BUNDLE-APP-ID    TO RPOL-POL-BUNDLE-APP-ID
                   MOVE 'Y'                 TO RPOL-POL-SUPRES-ISS-IND
                   PERFORM  POL-2000-REWRITE
                       THRU POL-2000-REWRITE-X
                   SET  WS-BUNDLE-POL-YES   TO TRUE
               END-IF           
           END-IF.
        
          PERFORM  3200-UPDATE-BUNDLE-APP-ID
              THRU 3200-UPDATE-BUNDLE-APP-ID-X
              UNTIL WCLN2-IO-EOF.
            
           PERFORM  CLN2-3000-END-BROWSE
               THRU CLN2-3000-END-BROWSE-X.
           
       3100-BROWSE-CO-CLI-DTLS-X.
           EXIT.
      /
      *--------------------------
       3200-UPDATE-BUNDLE-APP-ID.
      *--------------------------      

           MOVE RCLN2-POL-ID                 TO WPOL-POL-ID
          
           PERFORM  POL-1000-READ-FOR-UPDATE
              THRU  POL-1000-READ-FOR-UPDATE-X.
             
           MOVE WS-BUNDLE-APP-ID            TO RPOL-POL-BUNDLE-APP-ID.
           MOVE 'Y'                         TO RPOL-POL-SUPRES-ISS-IND.
         
           PERFORM  POL-2000-REWRITE
               THRU POL-2000-REWRITE-X.
           SET  WS-BUNDLE-APP-YES           TO TRUE.
           SET  WS-BUNDLE-POL-YES           TO TRUE.
           PERFORM  CLN2-2000-READ-NEXT
               THRU CLN2-2000-READ-NEXT-X.

       3200-UPDATE-BUNDLE-APP-ID-X.
           EXIT.
      /
      *---------------------
       3300-BROWSE-CLI-DTLS.
      *---------------------
      
           SET  WS-BUNDLE-POL-NO            TO TRUE.
           MOVE L2430-CLI-SUR-NM            TO WCLN1-CLI-SUR-NM. 
           MOVE L2430-CLI-GIV-NM            TO WCLN1-CLI-GIV-NM.
           MOVE L2430-KJ-SUR-NM             TO WCLN1-CLI-KJ-SUR-NM.
           MOVE L2430-KJ-GIV-NM             TO WCLN1-CLI-KJ-GIV-NM.
           MOVE WGLOB-PROCESS-DATE          TO WCLN1-CTL-PRCES-DT
           MOVE WCLN1-KEY                   TO WCLN1-ENDBR-KEY.
           
           PERFORM  CLN1-1000-BROWSE
               THRU CLN1-1000-BROWSE-X.

           IF NOT WCLN1-IO-OK
               GO TO 3300-BROWSE-CLI-DTLS-X
           END-IF.
          
           PERFORM  CLN1-2000-READ-NEXT
               THRU CLN1-2000-READ-NEXT-X.

           IF  WCLN1-IO-OK
           AND RCLN1-POL-ID =  WS-HOLD-POL-ID
               PERFORM  CLN1-2000-READ-NEXT
                   THRU CLN1-2000-READ-NEXT-X         
               IF  WCLN1-IO-EOF
                   PERFORM  CLN1-3000-END-BROWSE
                       THRU CLN1-3000-END-BROWSE-X
                   SET  WS-BUNDLE-POL-NO    TO TRUE
                   GO TO 3300-BROWSE-CLI-DTLS-X
               ELSE
                   MOVE WS-HOLD-POL-ID      TO WPOL-POL-ID
                   PERFORM  POL-1000-READ-FOR-UPDATE
                       THRU POL-1000-READ-FOR-UPDATE-X             
                   MOVE WS-BUNDLE-APP-ID    TO RPOL-POL-BUNDLE-APP-ID
                   MOVE 'Y'                 TO RPOL-POL-SUPRES-ISS-IND
                   PERFORM  POL-2000-REWRITE
                       THRU POL-2000-REWRITE-X
                   SET  WS-BUNDLE-APP-YES   TO TRUE
                   SET  WS-BUNDLE-POL-YES   TO TRUE
               END-IF           
           END-IF.         
     
           PERFORM  3400-UPDATE-BUNDLE-APP-ID
               THRU 3400-UPDATE-BUNDLE-APP-ID-X
               UNTIL WCLN1-IO-EOF.
            
           PERFORM  CLN1-3000-END-BROWSE
               THRU CLN1-3000-END-BROWSE-X.
           
       3300-BROWSE-CLI-DTLS-X.
           EXIT.
      /
      *--------------------------
       3400-UPDATE-BUNDLE-APP-ID.
      *--------------------------
      
           MOVE RCLN1-POL-ID                TO WPOL-POL-ID.
          
           PERFORM  POL-1000-READ-FOR-UPDATE
               THRU POL-1000-READ-FOR-UPDATE-X.
             
           MOVE WS-BUNDLE-APP-ID            TO RPOL-POL-BUNDLE-APP-ID.
           MOVE 'Y'                         TO RPOL-POL-SUPRES-ISS-IND.         
         
           PERFORM  POL-2000-REWRITE
               THRU POL-2000-REWRITE-X.
           SET  WS-BUNDLE-APP-YES           TO TRUE.
           SET  WS-BUNDLE-POL-YES           TO TRUE.
           PERFORM  CLN1-2000-READ-NEXT
               THRU CLN1-2000-READ-NEXT-X.

       3400-UPDATE-BUNDLE-APP-ID-X.
           EXIT.
      /
      *------------------
       4000-UPDATE-TPCOM.
      *------------------      
      
      *  ASSIGNING OF NEXT BUNDLE APPLICATION  ID WHERE WE GET TO KNOW *
      *  WHAT IS THE LAST BUNDLE ID IS ASSIGNED                  *
            
           PERFORM  PCOM-1000-READ-FOR-UPDATE
               THRU PCOM-1000-READ-FOR-UPDATE-X.
              
           MOVE WS-BUNDLE-APP-ID            TO RPCOM-NXT-BUNDLE-APP-ID.
                   
           PERFORM  PCOM-2000-REWRITE
               THRU PCOM-2000-REWRITE-X.  

      *    MSG:NEXT BUNDLE ID UPDATED TO TPCOM AS @1          
           MOVE  'CS94030004'               TO  WGLOB-MSG-REF-INFO.
           MOVE RPCOM-NXT-BUNDLE-APP-ID     TO WGLOB-MSG-PARM (1).             
           PERFORM  0260-1000-GENERATE-MESSAGE
               THRU 0260-1000-GENERATE-MESSAGE-X. 
               
       4000-UPDATE-TPCOM-X.
           EXIT.
      /                 
      *-----------------
       9999-CLOSE-FILES.
      *-----------------

           PERFORM  BCF-4000-CLOSE
               THRU BCF-4000-CLOSE-X.

           PERFORM  9403-4000-CLOSE
               THRU 9403-4000-CLOSE-X.

           PERFORM  OCF-4000-CLOSE
               THRU OCF-4000-CLOSE-X.

       9999-CLOSE-FILES-X.
           EXIT.
      /            
      ****************************************************
      ** PROCESSING WITH COMMON CODE
      ****************************************************
      
       COPY CCPPCCC.
       COPY XCPPTIME.
       COPY XCPPHDG.
      /
      ****************************************************
      ** LINKAGE TO COMMON MODULES
      ****************************************************

       COPY CCPL0010.
       COPY CCPS0010.
      / 
       COPY CCPL0950.
       COPY CCPS0950.
      /
       COPY XCPL0035. 
      /
       COPY XCPL0260.
      /
       COPY XCPL0030.
      /
       COPY CCPS2430.
       COPY XCPL0040.
       COPY CCPL2430. 
      /   
      ****************************************************
      ** FILE I/O                                       **
      ****************************************************

       COPY ACPBCLN1.
       COPY ACPBCLN2.
       COPY CCPNPOL.
       COPY CCPUPOL.
       COPY CCPNPCOM.
       COPY CCPUPCOM.
       COPY CCPNMAST.
       COPY XCPLOCF.
       COPY XCPOOCF.
      /
       COPY XCSNSEQ  REPLACING ==:ID:==  BY 9403.
       COPY XCSLFILE REPLACING ==:ID:==  BY 9403
                               ==':PGM:'== BY =='ASRQ9403'==.
       COPY XCSOFILE REPLACING ==:ID:==  BY 9403.
       COPY XCSLFILE REPLACING ==:ID:==  BY BCF
                               ==':PGM:'== BY =='XSRQBCF'==.
       COPY XCSOFILE REPLACING ==:ID:==  BY BCF.
       COPY XCSNSEQ  REPLACING ==:ID:==  BY BCF.
      /
      *****************************************************************
      **                 END OF PROGRAM CSBM9403                     **
      *****************************************************************     