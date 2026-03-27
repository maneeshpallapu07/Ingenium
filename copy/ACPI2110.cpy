      *****************************************************************
      **  MEMBER :  ACPI2110                                         **
      **  REMARKS:  BATCH I/O ROUTINE FOR THE                        **
      **            APEX TO NBS IMPORT AUDIT EXTRACT                 **
      **                                                             **
      *****************************************************************
      **  DATE     AUTH.  DESCRIPTION                                **
      **                                                             **
      **  28FEB95  DEV    CREATED FOR '2110' FILE PROCESSING         **
APEX54**  31DEC96  TJS    MODIFICATIONS FOR MAINTAINABILITY          **
007684**  30OCT98  56     CHECKPOINT/RESTART                         **
014590**  15DEC99  60     ARCHITECTURAL CHANGES TO ABEND HANDLER     **
      *****************************************************************

      *---------------
       2110-1000-READ.
      *---------------

007684     MOVE ZERO                 TO W2110-SEQ-IO-STATUS.

           READ 2110-DATA-FILE
                AT END
007684          MOVE 8               TO W2110-SEQ-IO-STATUS
                GO TO 2110-1000-READ-X.

007684     IF  R2110-SEQ-REC-INFO EQUAL HIGH-VALUES
007684         MOVE 8                TO W2110-SEQ-IO-STATUS
           END-IF.

007684     IF  W2110-SEQ-FILE-STATUS  NOT = ZERO
               PERFORM  2110-9000-HANDLE-ERROR
                   THRU 2110-9000-HANDLE-ERROR-X
           END-IF.

       2110-1000-READ-X.
           EXIT.
      /
      *----------------
       2110-2000-WRITE.
      *----------------

007684     MOVE ZERO                 TO W2110-SEQ-IO-STATUS.

007684     WRITE R2110-SEQ-REC-INFO.

007684     IF  W2110-SEQ-FILE-STATUS  NOT = ZERO
               PERFORM  2110-9000-HANDLE-ERROR
                   THRU 2110-9000-HANDLE-ERROR-X
           END-IF.

       2110-2000-WRITE-X.
           EXIT.
      /
      *---------------------
       2110-3000-OPEN-INPUT.
      *---------------------

007684     MOVE ZERO                   TO W2110-SEQ-IO-STATUS.

           OPEN INPUT 2110-DATA-FILE.

007684     IF  W2110-SEQ-FILE-STATUS  NOT = ZERO
               PERFORM  2110-9000-HANDLE-ERROR
                   THRU 2110-9000-HANDLE-ERROR-X
           END-IF.

       2110-3000-OPEN-INPUT-X.
           EXIT.
      /
      *----------------------
       2110-4000-OPEN-OUTPUT.
      *----------------------

007684     MOVE ZERO                   TO W2110-SEQ-IO-STATUS.

           OPEN OUTPUT 2110-DATA-FILE.

007684     IF  W2110-SEQ-FILE-STATUS  NOT = ZERO
               PERFORM  2110-9000-HANDLE-ERROR
                   THRU 2110-9000-HANDLE-ERROR-X
           END-IF.

       2110-4000-OPEN-OUTPUT-X.
           EXIT.
      /
      *----------------
       2110-5000-CLOSE.
      *----------------

007684     MOVE ZERO                   TO W2110-SEQ-IO-STATUS.

           CLOSE 2110-DATA-FILE.

007684     IF  W2110-SEQ-FILE-STATUS  NOT = ZERO
               PERFORM  2110-9000-HANDLE-ERROR
                   THRU 2110-9000-HANDLE-ERROR-X
           END-IF.

       2110-5000-CLOSE-X.
           EXIT.
      /
      *----------------------
       2110-6000-OPEN-EXTEND.
      *----------------------

007684     MOVE ZERO                   TO W2110-SEQ-IO-STATUS.

           OPEN EXTEND 2110-DATA-FILE.

007684     IF  W2110-SEQ-FILE-STATUS  NOT = ZERO
               PERFORM  2110-9000-HANDLE-ERROR
                   THRU 2110-9000-HANDLE-ERROR-X
           END-IF.

       2110-6000-OPEN-EXTEND-X.
           EXIT.
      /
      *-----------------------
       2110-9000-HANDLE-ERROR.
      *-----------------------

014590     MOVE W2110-SEQ-FILE-NAME    TO WGLOB-TABLE-NAME.
014590     MOVE W2110-SEQ-FILE-STATUS  TO WGLOB-SEQ-FILE-STATUS.
014590     MOVE W2110-SEQ-IO-COMMAND   TO WGLOB-IO-COMMAND.

           PERFORM  0030-3000-QSAM-ERROR
               THRU 0030-3000-QSAM-ERROR-X.

       2110-9000-HANDLE-ERROR-X.
           EXIT.

      *****************************************************************
      **                 END OF COPYBOOK ACPI2110                    **
      *****************************************************************
