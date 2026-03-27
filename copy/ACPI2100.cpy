      *****************************************************************
      **  MEMBER :  ACPI2100                                         **
      **  REMARKS:  BATCH I/O ROUTINE FOR THE                        **
      **            APEX UPLOAD DATA FILE                            **
      **                                                             **
      *****************************************************************
      **  DATE     AUTH.  DESCRIPTION                                **
      **                                                             **
      **  28FEB95  DEV    CREATED FOR '2100' FILE PROCESSING         **
APEX54**  31DEC96  TJS    MODIFICATIONS FOR MAINTAINABILITY          **
007684**  30OCT98  56     CHECKPOINT/RESTART                         **
014590**  15DEC99  60     ARCHITECTURAL CHANGES TO ABEND HANDLER     **
      *****************************************************************

      *---------------
       2100-1000-READ.
      *---------------

007684     MOVE ZERO                   TO W2100-SEQ-IO-STATUS.

           READ 2100-DATA-FILE
                AT END
007684          MOVE 8                 TO W2100-SEQ-IO-STATUS
                GO TO 2100-1000-READ-X.

007684     IF  R2100-SEQ-REC-INFO EQUAL HIGH-VALUES
007684         MOVE 8                  TO W2100-SEQ-IO-STATUS
           END-IF.

007684     IF  W2100-SEQ-FILE-STATUS  NOT = ZERO
               PERFORM  2100-9000-HANDLE-ERROR
                   THRU 2100-9000-HANDLE-ERROR-X
           END-IF.

       2100-1000-READ-X.
           EXIT.
      /
      *----------------
       2100-2000-WRITE.
      *----------------

007684     MOVE ZERO                   TO W2100-SEQ-IO-STATUS.

007684     WRITE R2100-SEQ-REC-INFO.

007684     IF  W2100-SEQ-FILE-STATUS  NOT = ZERO
               PERFORM  2100-9000-HANDLE-ERROR
                   THRU 2100-9000-HANDLE-ERROR-X
           END-IF.

       2100-2000-WRITE-X.
           EXIT.
      /
      *---------------------
       2100-3000-OPEN-INPUT.
      *---------------------

007684     MOVE ZERO                   TO W2100-SEQ-IO-STATUS.

           OPEN INPUT 2100-DATA-FILE.

007684     IF  W2100-SEQ-FILE-STATUS  NOT = ZERO
               PERFORM  2100-9000-HANDLE-ERROR
                   THRU 2100-9000-HANDLE-ERROR-X
           END-IF.

       2100-3000-OPEN-INPUT-X.
           EXIT.
      /
      *----------------------
       2100-4000-OPEN-OUTPUT.
      *----------------------

007684     MOVE ZERO                   TO W2100-SEQ-IO-STATUS.

           OPEN OUTPUT 2100-DATA-FILE.

007684     IF  W2100-SEQ-FILE-STATUS  NOT = ZERO
               PERFORM  2100-9000-HANDLE-ERROR
                   THRU 2100-9000-HANDLE-ERROR-X
           END-IF.

       2100-4000-OPEN-OUTPUT-X.
           EXIT.
      /
      *----------------
       2100-5000-CLOSE.
      *----------------

007684     MOVE ZERO                   TO W2100-SEQ-IO-STATUS.

           CLOSE 2100-DATA-FILE.

007684     IF  W2100-SEQ-FILE-STATUS  NOT = ZERO
               PERFORM  2100-9000-HANDLE-ERROR
                   THRU 2100-9000-HANDLE-ERROR-X
           END-IF.

       2100-5000-CLOSE-X.
           EXIT.
      /
      *----------------------
       2100-6000-OPEN-EXTEND.
      *----------------------

007684     MOVE ZERO                   TO W2100-SEQ-IO-STATUS.

           OPEN EXTEND 2100-DATA-FILE.

007684     IF  W2100-SEQ-FILE-STATUS  NOT = ZERO
               PERFORM  2100-9000-HANDLE-ERROR
                   THRU 2100-9000-HANDLE-ERROR-X
           END-IF.

       2100-6000-OPEN-EXTEND-X.
           EXIT.
      /
      *-----------------------
       2100-9000-HANDLE-ERROR.
      *-----------------------

014590     MOVE W2100-SEQ-FILE-NAME    TO WGLOB-TABLE-NAME.
014590     MOVE W2100-SEQ-FILE-STATUS  TO WGLOB-SEQ-FILE-STATUS.
014590     MOVE W2100-SEQ-IO-COMMAND   TO WGLOB-IO-COMMAND.

           PERFORM  0030-3000-QSAM-ERROR
               THRU 0030-3000-QSAM-ERROR-X.

       2100-9000-HANDLE-ERROR-X.
           EXIT.

      *****************************************************************
      **                 END OF COPYBOOK ACPI2100                    **
      *****************************************************************
