      *****************************************************************
      **  MEMBER :  ACPA2100                                         **
      **  REMARKS:  BATCH I/O ROUTINE USED TO WRITE A RECORD TO THE  **
      **            APEX UPLOAD DATA FILE                            **
      **                                                             **
      *****************************************************************
      **  DATE     AUTH.  DESCRIPTION                                **
      **                                                             **
      **  28FEB95  DEV    CREATED FOR '2100' FILE PROCESSING         **
APEX54**  31DEC96  TJS    MODIFICATIONS FOR MAINTAINABILITY          **
007684**  30OCT98  56     CHECKPOINT/RESTART                         **
      *****************************************************************
 
      *----------------
       2100-1000-WRITE.
      *----------------
 
007684     MOVE TFCMD-WRITE-RECORD TO W2100-SEQ-IO-COMMAND.
 
           PERFORM  2100-1000-LINK
               THRU 2100-1000-LINK-X.
 
       2100-1000-WRITE-X.
           EXIT.
 
      *****************************************************************
      **                 END OF COPYBOOK ACPA2100                    **
      *****************************************************************
