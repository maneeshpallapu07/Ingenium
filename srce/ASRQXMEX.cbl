      *************************
       IDENTIFICATION DIVISION.
      *************************
 
       PROGRAM-ID. ASRQXMEX.
 
       COPY XCWWCRHT.
 
      *****************************************************************
      **  MEMBER : ASRQXMEX                                          **
      **  REMARKS: BATCH I/O PROGRAM FOR THE XML MESSAGE REPORT      **
      **                                                             **
      **           THIS PROGRAM  IS THE I/O ROUTINE FOR THE MESSAGE  **
      **           REPORT FOR XML UPLOAD JOB WHICH WRITES UPLOAD     **
      **           STATUS AND THE ERROR MESSAGES OF EACH APPLICATION ** 
      *****************************************************************
      **  DATE     AUTH.  DESCRIPTION                                **
      **                                                             **
NWLXML**  20JUL09  CTS    CHANGES DONE FOR XML MESSAGE REPORT        **
      *****************************************************************
 
      **********************
       ENVIRONMENT DIVISION.
      **********************
 
       INPUT-OUTPUT SECTION.
 
       FILE-CONTROL.

       COPY XCSSFILE REPLACING ==:ID:==  BY ==XMUL==
                               ==:SYS:== BY ==A==.
       COPY XCSSFILE REPLACING ==:ID:==  BY ==XMPA==
                               ==:SYS:== BY ==A==.
       COPY XCSSFILE REPLACING ==:ID:==  BY ==XMGA==
                               ==:SYS:== BY ==A==.
       COPY XCSSFILE REPLACING ==:ID:==  BY ==XWMD==
                               ==:SYS:== BY ==A==.

      ***************
       DATA DIVISION.
      ***************
 
       FILE SECTION.
 
       COPY XCSDFILE REPLACING ==:ID:== BY ==XMUL==.
       COPY ACSRXMUL.
       COPY XCSDFILE REPLACING ==:ID:== BY ==XMPA==.
       COPY ACSRXMPA.
       COPY XCSDFILE REPLACING ==:ID:== BY ==XMGA==.
       COPY ACSRXMGA.
       COPY XCSDFILE REPLACING ==:ID:== BY ==XWMD==.
       COPY ACSRXWMD.
 
      *************************
       WORKING-STORAGE SECTION.
      *************************
 
       COPY XCWWPGWS REPLACING '$VAR1' BY 'ASRQXMEX'.
 
       COPY SQLCA.
 
       01  WS-WORKING-STORAGE.
           05  WS-FILE-OPEN-SWITCH          PIC X(01)  VALUE 'N'.
               88  WS-FILE-IS-OPEN                     VALUE 'Y'.
               88  WS-FILE-IS-CLOSED                   VALUE 'N'.
 
 
       COPY XCWTFCMD.
 
      *****************
       LINKAGE SECTION.
      *****************
 
       01  WGLOB-GLOBAL-AREA.
       COPY XCWWGLOB.
 
       COPY ACSWXMEX.
 
       01  WXMEX-LINK-RECORD                PIC X(126).
 
       PROCEDURE DIVISION  USING WGLOB-GLOBAL-AREA
                                 WXMEX-SEQ-IO-WORK-AREA
                                 WXMEX-LINK-RECORD.
 
      ***************
       0000-MAINLINE.
      ***************
 
           IF  WS-FILE-IS-OPEN
           AND (WXMEX-SEQ-IO-COMMAND = TFCMD-WRITE-RECORD
           OR   WXMEX-SEQ-IO-COMMAND = TFCMD-REWRITE-RECORD)
               EVALUATE TRUE                                                    
                   WHEN WXMEX-SEQ-FILE-NM-XMUL                                  
                       MOVE WXMEX-LINK-RECORD TO RXMUL-SEQ-REC-INFO             
                   WHEN WXMEX-SEQ-FILE-NM-XMPA                                  
                       MOVE WXMEX-LINK-RECORD TO RXMPA-SEQ-REC-INFO             
                   WHEN WXMEX-SEQ-FILE-NM-XMGA                                  
                       MOVE WXMEX-LINK-RECORD TO RXMGA-SEQ-REC-INFO             
                   WHEN WXMEX-SEQ-FILE-NM-XWMD                                  
                       MOVE WXMEX-LINK-RECORD TO RXWMD-SEQ-REC-INFO             
               END-EVALUATE                                                     
           END-IF.
 
           EVALUATE TRUE 
               
               WHEN WXMEX-SEQ-IO-COMMAND = TFCMD-READ-RECORD
                    PERFORM XMEX-1000-READ
                       THRU XMEX-1000-READ-X
               
               WHEN WXMEX-SEQ-IO-COMMAND = TFCMD-WRITE-RECORD
                    PERFORM XMEX-2000-WRITE
                       THRU XMEX-2000-WRITE-X
               
               WHEN WXMEX-SEQ-IO-COMMAND = TFCMD-OPEN-I-FILE
                    PERFORM XMEX-3000-OPEN-INPUT
                       THRU XMEX-3000-OPEN-INPUT-X
                    SET WS-FILE-IS-OPEN TO TRUE
               
               WHEN WXMEX-SEQ-IO-COMMAND = TFCMD-OPEN-O-FILE
                    PERFORM XMEX-4000-OPEN-OUTPUT
                       THRU XMEX-4000-OPEN-OUTPUT-X
                    SET WS-FILE-IS-OPEN TO TRUE
               
               WHEN WXMEX-SEQ-IO-COMMAND = TFCMD-CLOSE-FILE
                    PERFORM XMEX-5000-CLOSE
                       THRU XMEX-5000-CLOSE-X
                    SET WS-FILE-IS-CLOSED TO TRUE
               
               WHEN WXMEX-SEQ-IO-COMMAND = TFCMD-OPEN-EXTEND-FILE
                    PERFORM XMEX-6000-OPEN-EXTEND
                       THRU XMEX-6000-OPEN-EXTEND-X
                    SET WS-FILE-IS-OPEN TO TRUE
 
           END-EVALUATE.
                         
           IF  WS-FILE-IS-OPEN
           AND (WXMEX-SEQ-IO-COMMAND = TFCMD-READ-RECORD
           OR   WXMEX-SEQ-IO-COMMAND = TFCMD-READ-RECORD-FOR-UPDATE
           OR   WXMEX-SEQ-IO-COMMAND = TFCMD-READNEXT-RECORD)
               EVALUATE TRUE                                                    
                   WHEN WXMEX-SEQ-FILE-NM-XMUL                                  
                       MOVE RXMUL-SEQ-REC-INFO TO WXMEX-LINK-RECORD             
                   WHEN WXMEX-SEQ-FILE-NM-XMPA                                  
                       MOVE RXMPA-SEQ-REC-INFO TO WXMEX-LINK-RECORD             
                   WHEN WXMEX-SEQ-FILE-NM-XMGA                                  
                       MOVE RXMGA-SEQ-REC-INFO TO WXMEX-LINK-RECORD             
                   WHEN WXMEX-SEQ-FILE-NM-XWMD                                  
                       MOVE RXWMD-SEQ-REC-INFO TO WXMEX-LINK-RECORD             
               END-EVALUATE                                                     
           END-IF.
 
 
       0000-MAINLINE-X.
           GOBACK.
 
       COPY ACPIXMEX.
 
       COPY XCPL0030.
 
      *****************************************************************
      **                 END OF PROGRAM ASRQXMEX                     **
      *****************************************************************
