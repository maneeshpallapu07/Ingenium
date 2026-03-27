      *****************************************************************
      **  MEMBER : ACPAXMEX                                          **
      **  REMARKS: BATCH I/O ROUTINE USED TO WRITE A RECORD TO THE   **
      **           XML UPLOAD MESSAGE EXTRACT                        **
      **                                                             **
      *****************************************************************
      **  DATE     AUTH.  DESCRIPTION                                **
      **                                                             **
NWLXML**  20JUL09  CTS    NEW                                        **
      *****************************************************************

      *----------------
       XMUL-1000-WRITE.
      *----------------

           SET WXMEX-SEQ-FILE-NM-XMUL       TO TRUE.                                 

           MOVE TFCMD-WRITE-RECORD          TO WXMEX-SEQ-IO-COMMAND.
           PERFORM  XMEX-1000-LINK
               THRU XMEX-1000-LINK-X.

       XMUL-1000-WRITE-X.
           EXIT.

      *----------------
       XMPA-1000-WRITE.
      *----------------

           SET WXMEX-SEQ-FILE-NM-XMPA       TO TRUE.                                 

           MOVE TFCMD-WRITE-RECORD          TO WXMEX-SEQ-IO-COMMAND.
           PERFORM  XMEX-1000-LINK
               THRU XMEX-1000-LINK-X.

       XMPA-1000-WRITE-X.
           EXIT.

      *----------------
       XMGA-1000-WRITE.
      *----------------

           SET WXMEX-SEQ-FILE-NM-XMGA       TO TRUE.                                 

           MOVE TFCMD-WRITE-RECORD          TO WXMEX-SEQ-IO-COMMAND.
           PERFORM  XMEX-1000-LINK
               THRU XMEX-1000-LINK-X.

       XMGA-1000-WRITE-X.
           EXIT.

      *----------------
       XWMD-1000-WRITE.
      *----------------

           SET WXMEX-SEQ-FILE-NM-XWMD       TO TRUE.                                 

           MOVE TFCMD-WRITE-RECORD          TO WXMEX-SEQ-IO-COMMAND.
           PERFORM  XMEX-1000-LINK
               THRU XMEX-1000-LINK-X.

       XWMD-1000-WRITE-X.
           EXIT.

      *****************************************************************
      **                 END OF COPYBOOK ACPAXMEX                    **
      *****************************************************************
