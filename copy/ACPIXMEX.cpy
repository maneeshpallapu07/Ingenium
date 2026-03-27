      *****************************************************************
      **  MEMBER : ACPIXMEX                                          **
      **  REMARKS: BATCH I/O ROUTINE FOR THE                         **
      **           XML UPLOAD MESSAGE EXTRACT                        **         
      *****************************************************************
      **  DATE     AUTH.  DESCRIPTION                                **
      **                                                             **
NWLXML**  20JUL09  CTS    NEW                                        **
      *****************************************************************

      *---------------
       XMEX-1000-READ.
      *---------------

           SET  WXMEX-SEQ-IO-OK             TO TRUE.

           EVALUATE TRUE                                                        
               WHEN WXMEX-SEQ-FILE-NM-XMUL                                      
                   PERFORM  XMEX-1010-READ-XMUL                                 
                       THRU XMEX-1010-READ-XMUL-X                               
               WHEN WXMEX-SEQ-FILE-NM-XMPA                                      
                   PERFORM  XMEX-1020-READ-XMPA                                 
                       THRU XMEX-1020-READ-XMPA-X                               
               WHEN WXMEX-SEQ-FILE-NM-XMGA                                      
                   PERFORM  XMEX-1030-READ-XMGA                                 
                       THRU XMEX-1030-READ-XMGA-X                               
               WHEN WXMEX-SEQ-FILE-NM-XWMD                                      
                   PERFORM  XMEX-1040-READ-XWMD                                 
                       THRU XMEX-1040-READ-XWMD-X                               
          END-EVALUATE.                                                        

           IF  WXMEX-SEQ-FILE-STATUS  NOT = ZERO
               PERFORM  XMEX-9000-HANDLE-ERROR
                   THRU XMEX-9000-HANDLE-ERROR-X
      IF  WXMEX-SEQ-FILE-STATUS  NOT = ZERO
               PERFORM  XMEX-9000-HANDLE-ERROR
                   THRU XMEX-9000-HANDLE-ERROR-X
           END-IF.

       XMEX-1000-READ-X.
           EXIT.

      *--------------------
       XMEX-1010-READ-XMUL.
      *--------------------

           READ XMUL-DATA-FILE
                AT END
                   SET WXMEX-SEQ-IO-EOF     TO TRUE
                   GO TO XMEX-1010-READ-XMUL.

           IF  RXMUL-SEQ-REC-INFO EQUAL HIGH-VALUES
                   SET  WXMEX-SEQ-IO-EOF    TO TRUE
           END-IF.
           
           MOVE  WXMUL-SEQ-FILE-STATUS      TO WXMEX-SEQ-FILE-STATUS.

       XMEX-1010-READ-XMUL-X.
           EXIT.

      *--------------------
       XMEX-1020-READ-XMPA.
      *--------------------

           READ XMPA-DATA-FILE
                AT END
                   SET WXMEX-SEQ-IO-EOF     TO TRUE
                   GO TO XMEX-1020-READ-XMPA.

           IF  RXMPA-SEQ-REC-INFO EQUAL HIGH-VALUES
                   SET  WXMEX-SEQ-IO-EOF    TO TRUE
           END-IF.
           
           MOVE  WXMPA-SEQ-FILE-STATUS      TO WXMEX-SEQ-FILE-STATUS.

       XMEX-1020-READ-XMPA-X.
           EXIT.

      *--------------------
       XMEX-1030-READ-XMGA.
      *--------------------

           READ XMGA-DATA-FILE
                AT END
                   SET WXMEX-SEQ-IO-EOF     TO TRUE
                   GO TO XMEX-1030-READ-XMGA.

           IF  RXMGA-SEQ-REC-INFO EQUAL HIGH-VALUES
                   SET  WXMEX-SEQ-IO-EOF    TO TRUE
           END-IF.

           MOVE  WXMGA-SEQ-FILE-STATUS      TO WXMEX-SEQ-FILE-STATUS.
           
       XMEX-1030-READ-XMGA-X.
           EXIT.

      *--------------------
       XMEX-1040-READ-XWMD.
      *--------------------

           READ XWMD-DATA-FILE
                AT END
                   SET WXMEX-SEQ-IO-EOF     TO TRUE
                   GO TO XMEX-1040-READ-XWMD.

           IF  RXWMD-SEQ-REC-INFO EQUAL HIGH-VALUES
                   SET  WXMEX-SEQ-IO-EOF    TO TRUE
           END-IF.
           
           MOVE  WXWMD-SEQ-FILE-STATUS      TO WXMEX-SEQ-FILE-STATUS.

       XMEX-1040-READ-XWMD-X.
           EXIT.
      /
      *----------------
       XMEX-2000-WRITE.
      *----------------

           SET  WXMEX-SEQ-IO-OK             TO TRUE.
                             
           EVALUATE TRUE                                                        
               WHEN WXMEX-SEQ-FILE-NM-XMUL                                      
                   WRITE RXMUL-SEQ-REC-INFO
                   MOVE  WXMUL-SEQ-FILE-STATUS      
                                            TO WXMEX-SEQ-FILE-STATUS
               WHEN WXMEX-SEQ-FILE-NM-XMPA                                      
                   WRITE RXMPA-SEQ-REC-INFO
                   MOVE  WXMPA-SEQ-FILE-STATUS      
                                            TO WXMEX-SEQ-FILE-STATUS
               WHEN WXMEX-SEQ-FILE-NM-XMGA                                      
                   WRITE RXMGA-SEQ-REC-INFO                                     
                   MOVE  WXMGA-SEQ-FILE-STATUS      
                                            TO WXMEX-SEQ-FILE-STATUS
               WHEN WXMEX-SEQ-FILE-NM-XWMD                                      
                   WRITE RXWMD-SEQ-REC-INFO                                     
                   MOVE  WXWMD-SEQ-FILE-STATUS      
                                            TO WXMEX-SEQ-FILE-STATUS
           END-EVALUATE.                                                        

           IF  WXMEX-SEQ-FILE-STATUS  NOT = ZERO
               PERFORM  XMEX-9000-HANDLE-ERROR
                   THRU XMEX-9000-HANDLE-ERROR-X
           END-IF.

       XMEX-2000-WRITE-X.
           EXIT.
      /
      *---------------------
       XMEX-3000-OPEN-INPUT.
      *---------------------

           SET  WXMEX-SEQ-IO-OK             TO TRUE.
                             
           EVALUATE TRUE                                                        
               WHEN WXMEX-SEQ-FILE-NM-XMUL                                      
                   OPEN INPUT XMUL-DATA-FILE
                   MOVE  WXMUL-SEQ-FILE-STATUS      
                                            TO WXMEX-SEQ-FILE-STATUS
               WHEN WXMEX-SEQ-FILE-NM-XMPA                                      
                   OPEN INPUT XMPA-DATA-FILE                                    
                   MOVE  WXMPA-SEQ-FILE-STATUS      
                                            TO WXMEX-SEQ-FILE-STATUS
               WHEN WXMEX-SEQ-FILE-NM-XMGA                                      
                   OPEN INPUT XMGA-DATA-FILE
                   MOVE  WXMGA-SEQ-FILE-STATUS      
                                            TO WXMEX-SEQ-FILE-STATUS
               WHEN WXMEX-SEQ-FILE-NM-XWMD                                      
                   OPEN INPUT XWMD-DATA-FILE                                    
                   MOVE  WXWMD-SEQ-FILE-STATUS      
                                            TO WXMEX-SEQ-FILE-STATUS
           END-EVALUATE.                                                        

           IF  WXMEX-SEQ-FILE-STATUS  NOT = ZERO
               PERFORM  XMEX-9000-HANDLE-ERROR
                   THRU XMEX-9000-HANDLE-ERROR-X
           END-IF.

       XMEX-3000-OPEN-INPUT-X.
           EXIT.
      /
      *----------------------
       XMEX-4000-OPEN-OUTPUT.
      *----------------------

           SET  WXMEX-SEQ-IO-OK             TO TRUE.

           EVALUATE TRUE                                                        
               WHEN WXMEX-SEQ-FILE-NM-XMUL                                      
                   OPEN OUTPUT XMUL-DATA-FILE
                   MOVE  WXMUL-SEQ-FILE-STATUS      
                                            TO WXMEX-SEQ-FILE-STATUS
               WHEN WXMEX-SEQ-FILE-NM-XMPA                                      
                   OPEN OUTPUT XMPA-DATA-FILE                                   
                   MOVE  WXMPA-SEQ-FILE-STATUS      
                                            TO WXMEX-SEQ-FILE-STATUS
               WHEN WXMEX-SEQ-FILE-NM-XMGA                                      
                   OPEN OUTPUT XMGA-DATA-FILE                                   
                   MOVE  WXMGA-SEQ-FILE-STATUS      
                                            TO WXMEX-SEQ-FILE-STATUS
               WHEN WXMEX-SEQ-FILE-NM-XWMD                                      
                   OPEN OUTPUT XWMD-DATA-FILE                                   
                   MOVE  WXWMD-SEQ-FILE-STATUS      
                                            TO WXMEX-SEQ-FILE-STATUS
           END-EVALUATE.                                                        

           IF  WXMEX-SEQ-FILE-STATUS  NOT = ZERO
               PERFORM  XMEX-9000-HANDLE-ERROR
                   THRU XMEX-9000-HANDLE-ERROR-X
           END-IF.

       XMEX-4000-OPEN-OUTPUT-X.
           EXIT.
      /
      *----------------
       XMEX-5000-CLOSE.
      *----------------

           SET  WXMEX-SEQ-IO-OK             TO TRUE.

           EVALUATE TRUE                                                        
               WHEN WXMEX-SEQ-FILE-NM-XMUL                                      
                   CLOSE XMUL-DATA-FILE                                         
                   MOVE  WXMUL-SEQ-FILE-STATUS      
                                            TO WXMEX-SEQ-FILE-STATUS
               WHEN WXMEX-SEQ-FILE-NM-XMPA                                      
                   CLOSE XMPA-DATA-FILE                                         
                   MOVE  WXMPA-SEQ-FILE-STATUS      
                                            TO WXMEX-SEQ-FILE-STATUS
               WHEN WXMEX-SEQ-FILE-NM-XMGA                                      
                   CLOSE XMGA-DATA-FILE                                         
                   MOVE  WXMGA-SEQ-FILE-STATUS      
                                            TO WXMEX-SEQ-FILE-STATUS
               WHEN WXMEX-SEQ-FILE-NM-XWMD                                      
                   CLOSE XWMD-DATA-FILE                                         
                   MOVE  WXWMD-SEQ-FILE-STATUS      
                                            TO WXMEX-SEQ-FILE-STATUS
           END-EVALUATE.                                                        
                      
           IF  WXMEX-SEQ-FILE-STATUS  NOT = ZERO
               PERFORM  XMEX-9000-HANDLE-ERROR
                   THRU XMEX-9000-HANDLE-ERROR-X
           END-IF.

       XMEX-5000-CLOSE-X.
           EXIT.
      /
      *----------------------
       XMEX-6000-OPEN-EXTEND.
      *----------------------

           SET  WXMEX-SEQ-IO-OK             TO TRUE.

           EVALUATE TRUE                                                        
               WHEN WXMEX-SEQ-FILE-NM-XMUL                                      
                   OPEN EXTEND XMUL-DATA-FILE                                   
                   MOVE  WXMUL-SEQ-FILE-STATUS      
                                            TO WXMEX-SEQ-FILE-STATUS
               WHEN WXMEX-SEQ-FILE-NM-XMPA                                      
                   OPEN EXTEND XMPA-DATA-FILE                                   
                   MOVE  WXMPA-SEQ-FILE-STATUS      
                                            TO WXMEX-SEQ-FILE-STATUS
               WHEN WXMEX-SEQ-FILE-NM-XMGA                                      
                   OPEN EXTEND XMGA-DATA-FILE                                   
                   MOVE  WXMGA-SEQ-FILE-STATUS      
                                            TO WXMEX-SEQ-FILE-STATUS
               WHEN WXMEX-SEQ-FILE-NM-XWMD                                      
                   OPEN EXTEND XWMD-DATA-FILE                                   
                   MOVE  WXWMD-SEQ-FILE-STATUS      
                                            TO WXMEX-SEQ-FILE-STATUS
           END-EVALUATE.                                                        

           IF  WXMEX-SEQ-FILE-STATUS  NOT = ZERO
               PERFORM  XMEX-9000-HANDLE-ERROR
                   THRU XMEX-9000-HANDLE-ERROR-X
           END-IF.

       XMEX-6000-OPEN-EXTEND-X.
           EXIT.
      /
      *-----------------------
       XMEX-9000-HANDLE-ERROR.
      *-----------------------

           MOVE WXMEX-SEQ-FILE-NAME         TO WGLOB-TABLE-NAME.
           MOVE WXMEX-SEQ-FILE-STATUS       TO WGLOB-SEQ-FILE-STATUS.
           MOVE WXMEX-SEQ-IO-COMMAND        TO WGLOB-IO-COMMAND.

           PERFORM 0030-3000-QSAM-ERROR
              THRU 0030-3000-QSAM-ERROR-X.

       XMEX-9000-HANDLE-ERROR-X.
           EXIT.

      *****************************************************************
      **                 END OF COPYBOOK ACPIXMEX                    **
      *****************************************************************
