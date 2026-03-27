      *****************************************************************
      **  MEMBER :  ACPI9400                                         **
      **  REMARKS:  BATCH I/O ROUTINE USED TO READ THE               **
      **            CS9400 FILE WHICH CONTAINS APPLICATION ID'S      **
      **            FROM THE ILLUSTRATION/APPLICATION SYSTEM         **
      **                                                             **
      *****************************************************************
      **  DATE     AUTH.  DESCRIPTION                                **
      **                                                             **
      **  31AUG01  DPK    CREATED FOR '9400' FILE PROCESSING         **
      *****************************************************************

      *---------------
    copy
    copy
    copy
    copy
      *---------------

           MOVE ZERO                 TO W9400-SEQ-IO-STATUS.

           READ 9400-DATA-FILE
                AT END
                MOVE 8               TO W9400-SEQ-IO-STATUS
                GO TO 9400-1000-READ-X.

           IF  R9400-SEQ-REC-INFO EQUAL HIGH-VALUES
               MOVE 8                TO W9400-SEQ-IO-STATUS
           END-IF.

           IF  W9400-SEQ-FILE-STATUS  NOT = ZERO
               PERFORM  9400-9000-HANDLE-ERROR
                   THRU 9400-9000-HANDLE-ERROR-X
           END-IF.

       9400-1000-READ-X.
           EXIT.
      /
      *----------------
       9400-2000-WRITE.
      *----------------

           MOVE ZERO                 TO W9400-SEQ-IO-STATUS.

           WRITE R9400-SEQ-REC-INFO.

           IF  W9400-SEQ-FILE-STATUS  NOT = ZERO
               PERFORM  9400-9000-HANDLE-ERROR
                   THRU 9400-9000-HANDLE-ERROR-X
           END-IF.

       9400-2000-WRITE-X.
           EXIT.
      /
      *---------------------
       9400-3000-OPEN-INPUT.
      *---------------------

           MOVE ZERO                   TO W9400-SEQ-IO-STATUS.

           OPEN INPUT 9400-DATA-FILE.

           IF  W9400-SEQ-FILE-STATUS  NOT = ZERO
               PERFORM  9400-9000-HANDLE-ERROR
                   THRU 9400-9000-HANDLE-ERROR-X
           END-IF.

       9400-3000-OPEN-INPUT-X.
           EXIT.
      /
      *----------------------
       9400-4000-OPEN-OUTPUT.
      *----------------------

           MOVE ZERO                   TO W9400-SEQ-IO-STATUS.

           OPEN OUTPUT 9400-DATA-FILE.

           IF  W9400-SEQ-FILE-STATUS  NOT = ZERO
               PERFORM  9400-9000-HANDLE-ERROR
                   THRU 9400-9000-HANDLE-ERROR-X
           END-IF.

       9400-4000-OPEN-OUTPUT-X.
           EXIT.
      /
      *----------------
       9400-5000-CLOSE.
      *----------------

           MOVE ZERO                   TO W9400-SEQ-IO-STATUS.

           CLOSE 9400-DATA-FILE.

           IF  W9400-SEQ-FILE-STATUS  NOT = ZERO
               PERFORM  9400-9000-HANDLE-ERROR
                   THRU 9400-9000-HANDLE-ERROR-X
           END-IF.

       9400-5000-CLOSE-X.
           EXIT.
      /
      *----------------------
       9400-6000-OPEN-EXTEND.
      *----------------------

           MOVE ZERO                   TO W9400-SEQ-IO-STATUS.

           OPEN EXTEND 9400-DATA-FILE.

           IF  W9400-SEQ-FILE-STATUS  NOT = ZERO
               PERFORM  9400-9000-HANDLE-ERROR
                   THRU 9400-9000-HANDLE-ERROR-X
           END-IF.

       9400-6000-OPEN-EXTEND-X.
           EXIT.
      /
      *-----------------------
       9400-9000-HANDLE-ERROR.
      *-----------------------

           MOVE W9400-SEQ-FILE-NAME    TO WGLOB-TABLE-NAME.
           MOVE W9400-SEQ-FILE-STATUS  TO WGLOB-SEQ-FILE-STATUS.
           MOVE W9400-SEQ-IO-COMMAND   TO WGLOB-IO-COMMAND.

           PERFORM  0030-3000-QSAM-ERROR
               THRU 0030-3000-QSAM-ERROR-X.

       9400-9000-HANDLE-ERROR-X.
           EXIT.

      *****************************************************************
      **                 END OF COPYBOOK ACPI9400                    **
      *****************************************************************
