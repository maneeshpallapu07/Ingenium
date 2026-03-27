      *****************************************************************
      **  MEMBER :  ACPI2120                                         **
      **  REMARKS:  BATCH I/O ROUTINE FOR THE                        **
      **            APEX TO NBS IMPORT MESSAGE EXTRACT               **
      **                                                             **
      *****************************************************************
      **  DATE     AUTH.  DESCRIPTION                                **
      **                                                             **
      **  28FEB95  DEV    CREATED FOR '2120' FILE PROCESSING         **
APEX54**  31DEC96  TJS    MODIFICATIONS FOR MAINTAINABILITY          **
007684**  30OCT98  56     CHECKPOINT/RESTART                         **
014590**  15DEC99  60     ARCHITECTURAL CHANGES TO ABEND HANDLER     **
      *****************************************************************

      *---------------
       2120-1000-READ.
      *---------------

007684     MOVE ZERO                   TO W2120-SEQ-IO-STATUS.

           READ 2120-DATA-FILE
                AT END
007684            MOVE 8               TO W2120-SEQ-IO-STATUS
                  GO TO 2120-1000-READ-X.

007684     IF  R2120-SEQ-REC-INFO EQUAL HIGH-VALUES
007684         MOVE 8                   TO W2120-SEQ-IO-STATUS
           END-IF.

007684     IF  W2120-SEQ-FILE-STATUS  NOT = ZERO
               PERFORM  2120-9000-HANDLE-ERROR
                   THRU 2120-9000-HANDLE-ERROR-X
           END-IF.

       2120-1000-READ-X.
           EXIT.
      /
      *----------------
       2120-2000-WRITE.
      *----------------

007684     MOVE ZERO                   TO W2120-SEQ-IO-STATUS.

007684     WRITE R2120-SEQ-REC-INFO.

007684     IF  W2120-SEQ-FILE-STATUS  NOT = ZERO
               PERFORM  2120-9000-HANDLE-ERROR
                   THRU 2120-9000-HANDLE-ERROR-X
           END-IF.

       2120-2000-WRITE-X.
           EXIT.
      /
      *---------------------
       2120-3000-OPEN-INPUT.
      *---------------------

007684     MOVE ZERO                   TO W2120-SEQ-IO-STATUS.

           OPEN INPUT 2120-DATA-FILE.

007684     IF  W2120-SEQ-FILE-STATUS  NOT = ZERO
               PERFORM  2120-9000-HANDLE-ERROR
                   THRU 2120-9000-HANDLE-ERROR-X
           END-IF.

       2120-3000-OPEN-INPUT-X.
           EXIT.
      /
      *----------------------
       2120-4000-OPEN-OUTPUT.
      *----------------------

007684     MOVE ZERO                   TO W2120-SEQ-IO-STATUS.

           OPEN OUTPUT 2120-DATA-FILE.

007684     IF  W2120-SEQ-FILE-STATUS  NOT = ZERO
               PERFORM  2120-9000-HANDLE-ERROR
                   THRU 2120-9000-HANDLE-ERROR-X
           END-IF.

       2120-4000-OPEN-OUTPUT-X.
           EXIT.
      /
      *----------------
       2120-5000-CLOSE.
      *----------------

007684     MOVE ZERO                   TO W2120-SEQ-IO-STATUS.

           CLOSE 2120-DATA-FILE.

007684     IF  W2120-SEQ-FILE-STATUS  NOT = ZERO
               PERFORM  2120-9000-HANDLE-ERROR
                   THRU 2120-9000-HANDLE-ERROR-X
           END-IF.

       2120-5000-CLOSE-X.
           EXIT.
      /
      *----------------------
       2120-6000-OPEN-EXTEND.
      *----------------------

007684     MOVE ZERO                   TO W2120-SEQ-IO-STATUS.

           OPEN EXTEND 2120-DATA-FILE.

007684     IF  W2120-SEQ-FILE-STATUS  NOT = ZERO
               PERFORM  2120-9000-HANDLE-ERROR
                   THRU 2120-9000-HANDLE-ERROR-X
           END-IF.

       2120-6000-OPEN-EXTEND-X.
           EXIT.
      /
      *-----------------------
       2120-9000-HANDLE-ERROR.
      *-----------------------

014590     MOVE W2120-SEQ-FILE-NAME    TO WGLOB-TABLE-NAME.
014590     MOVE W2120-SEQ-FILE-STATUS  TO WGLOB-SEQ-FILE-STATUS.
014590     MOVE W2120-SEQ-IO-COMMAND   TO WGLOB-IO-COMMAND.

           PERFORM  0030-3000-QSAM-ERROR
               THRU 0030-3000-QSAM-ERROR-X.

       2120-9000-HANDLE-ERROR-X.
           EXIT.

      *****************************************************************
      **                 END OF COPYBOOK ACPI2120                    **
      *****************************************************************
