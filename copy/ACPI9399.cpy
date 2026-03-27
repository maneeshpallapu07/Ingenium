      *****************************************************************
      **  MEMBER :  ACPI9399                                         **
      **  REMARKS:  BATCH I/O ROUTINE USED TO READ THE               **
      **            CS9399 FILE WHICH CONTAINS APPLICATION ID'S      **
      **            FROM THE ILLUSTRATION/APPLICATION SYSTEM         **
      **                                                             **
      *****************************************************************
      **  DATE     AUTH.  DESCRIPTION                                **
      **                                                             **
P00697**  31AUG01  DPK    CREATED FOR '9399' FILE PROCESSING         **
      *****************************************************************

      *---------------
       9399-1000-READ.
      *---------------
            *---------------
       9399-1000-READ.
      *---------------


           MOVE ZERO                 TO W9399-SEQ-IO-STATUS.

           READ 9399-DATA-FILE
                AT END
                MOVE 8               TO W9399-SEQ-IO-STATUS
                GO TO 9399-1000-READ-X.

           IF  R9399-SEQ-REC-INFO EQUAL HIGH-VALUES
               MOVE 8                TO W9399-SEQ-IO-STATUS
           END-IF.

           IF  W9399-SEQ-FILE-STATUS  NOT = ZERO
               PERFORM  9399-9000-HANDLE-ERROR
                   THRU 9399-9000-HANDLE-ERROR-X
           END-IF.

       9399-1000-READ-X.
           EXIT.
      /
      *----------------
       9399-2000-WRITE.
      *----------------

           MOVE ZERO                 TO W9399-SEQ-IO-STATUS.

           WRITE R9399-SEQ-REC-INFO.

           IF  W9399-SEQ-FILE-STATUS  NOT = ZERO
               PERFORM  9399-9000-HANDLE-ERROR
                   THRU 9399-9000-HANDLE-ERROR-X
           END-IF.

       9399-2000-WRITE-X.
           EXIT.
      /
      *---------------------
       9399-3000-OPEN-INPUT.
      *---------------------

           MOVE ZERO                   TO W9399-SEQ-IO-STATUS.

           OPEN INPUT 9399-DATA-FILE.

           IF  W9399-SEQ-FILE-STATUS  NOT = ZERO
               PERFORM  9399-9000-HANDLE-ERROR
                   THRU 9399-9000-HANDLE-ERROR-X
           END-IF.

       9399-3000-OPEN-INPUT-X.
           EXIT.
      /
      *----------------------
       9399-4000-OPEN-OUTPUT.
      *----------------------

           MOVE ZERO                   TO W9399-SEQ-IO-STATUS.

           OPEN OUTPUT 9399-DATA-FILE.

           IF  W9399-SEQ-FILE-STATUS  NOT = ZERO
               PERFORM  9399-9000-HANDLE-ERROR
                   THRU 9399-9000-HANDLE-ERROR-X
           END-IF.

       9399-4000-OPEN-OUTPUT-X.
           EXIT.
      /
      *----------------
       9399-5000-CLOSE.
      *----------------

           MOVE ZERO                   TO W9399-SEQ-IO-STATUS.

           CLOSE 9399-DATA-FILE.

           IF  W9399-SEQ-FILE-STATUS  NOT = ZERO
               PERFORM  9399-9000-HANDLE-ERROR
                   THRU 9399-9000-HANDLE-ERROR-X
           END-IF.

       9399-5000-CLOSE-X.
           EXIT.
      /
      *----------------------
       9399-6000-OPEN-EXTEND.
      *----------------------

           MOVE ZERO                   TO W9399-SEQ-IO-STATUS.

           OPEN EXTEND 9399-DATA-FILE.

           IF  W9399-SEQ-FILE-STATUS  NOT = ZERO
               PERFORM  9399-9000-HANDLE-ERROR
                   THRU 9399-9000-HANDLE-ERROR-X
           END-IF.

       9399-6000-OPEN-EXTEND-X.
           EXIT.
      /
      *-----------------------
       9399-9000-HANDLE-ERROR.
      *-----------------------

           MOVE W9399-SEQ-FILE-NAME    TO WGLOB-TABLE-NAME.
           MOVE W9399-SEQ-FILE-STATUS  TO WGLOB-SEQ-FILE-STATUS.
           MOVE W9399-SEQ-IO-COMMAND   TO WGLOB-IO-COMMAND.

           PERFORM  0030-3000-QSAM-ERROR
               THRU 0030-3000-QSAM-ERROR-X.

       9399-9000-HANDLE-ERROR-X.
           EXIT.

      *****************************************************************
      **                 END OF COPYBOOK ACPI9399                    **
      *****************************************************************
