      *****************************************************************
      **  MEMBER :  ACPIADUP                                         **
      **  REMARKS:  BATCH I/O ROUTINE USED TO READ THE               **
      **            CSADUP FILE WHICH CONTAINS APPLICATION ID'S      **
      **            FROM THE ILLUSTRATION/APPLICATION SYSTEM         **
      **                                                             **
      *****************************************************************
      **  DATE     AUTH.  DESCRIPTION                                **
      **                                                             **
P00697**  31AUG01  DPK    CREATED FOR 'ADUP' FILE PROCESSING         **
      *****************************************************************

      *---------------
       ADUP-1000-READ.
      *---------------

           MOVE ZERO                 TO WADUP-SEQ-IO-STATUS.

           READ ADUP-DATA-FILE
                AT END
                MOVE 8               TO WADUP-SEQ-IO-STATUS
                GO TO ADUP-1000-READ-X.

           IF  RADUP-SEQ-REC-INFO EQUAL HIGH-VALUES
               MOVE 8                TO WADUP-SEQ-IO-STATUS
           END-IF.

           IF  WADUP-SEQ-FILE-STATUS  NOT = ZERO
               PERFORM  ADUP-9000-HANDLE-ERROR
                   THRU ADUP-9000-HANDLE-ERROR-X
           END-IF.

       ADUP-1000-READ-X.
           EXIT.
      /
      *----------------
       ADUP-2000-WRITE.
      *----------------

           MOVE ZERO                 TO WADUP-SEQ-IO-STATUS.

           WRITE RADUP-SEQ-REC-INFO.

           IF  WADUP-SEQ-FILE-STATUS  NOT = ZERO
               PERFORM  ADUP-9000-HANDLE-ERROR
                   THRU ADUP-9000-HANDLE-ERROR-X
           END-IF.

       ADUP-2000-WRITE-X.
           EXIT.
      /
      *---------------------
       ADUP-3000-OPEN-INPUT.
      *---------------------

           MOVE ZERO                   TO WADUP-SEQ-IO-STATUS.

           OPEN INPUT ADUP-DATA-FILE.

           IF  WADUP-SEQ-FILE-STATUS  NOT = ZERO
               PERFORM  ADUP-9000-HANDLE-ERROR
                   THRU ADUP-9000-HANDLE-ERROR-X
           END-IF.

       ADUP-3000-OPEN-INPUT-X.
           EXIT.
      /
      *----------------------
       ADUP-4000-OPEN-OUTPUT.
      *----------------------

           MOVE ZERO                   TO WADUP-SEQ-IO-STATUS.

           OPEN OUTPUT ADUP-DATA-FILE.

           IF  WADUP-SEQ-FILE-STATUS  NOT = ZERO
               PERFORM  ADUP-9000-HANDLE-ERROR
                   THRU ADUP-9000-HANDLE-ERROR-X
           END-IF.

       ADUP-4000-OPEN-OUTPUT-X.
           EXIT.
      /
      *----------------
       ADUP-5000-CLOSE.
      *----------------

           MOVE ZERO                   TO WADUP-SEQ-IO-STATUS.

           CLOSE ADUP-DATA-FILE.

           IF  WADUP-SEQ-FILE-STATUS  NOT = ZERO
               PERFORM  ADUP-9000-HANDLE-ERROR
                   THRU ADUP-9000-HANDLE-ERROR-X
           END-IF.

       ADUP-5000-CLOSE-X.
           EXIT.
      /
      *----------------------
       ADUP-6000-OPEN-EXTEND.
      *----------------------

           MOVE ZERO                   TO WADUP-SEQ-IO-STATUS.

           OPEN EXTEND ADUP-DATA-FILE.

           IF  WADUP-SEQ-FILE-STATUS  NOT = ZERO
               PERFORM  ADUP-9000-HANDLE-ERROR
                   THRU ADUP-9000-HANDLE-ERROR-X
           END-IF.

       ADUP-6000-OPEN-EXTEND-X.
           EXIT.
      /
      *-----------------------
       ADUP-9000-HANDLE-ERROR.
      *-----------------------

           MOVE WADUP-SEQ-FILE-NAME    TO WGLOB-TABLE-NAME.
           MOVE WADUP-SEQ-FILE-STATUS  TO WGLOB-SEQ-FILE-STATUS.
           MOVE WADUP-SEQ-IO-COMMAND   TO WGLOB-IO-COMMAND.

           PERFORM  0030-3000-QSAM-ERROR
               THRU 0030-3000-QSAM-ERROR-X.

       ADUP-9000-HANDLE-ERROR-X.
           EXIT.

      *****************************************************************
      **                 END OF COPYBOOK ACPIADUP                    **
      *****************************************************************
