      *****************************************************************
      **  MEMBER :  ACPAADUP                                         **
      **  REMARKS: BATCH I/O ROUTINE USED TO WRITE A RECORD TO THE   **
      **           DUPLICATE APPLICATION ID'S EXTRACT FILE           **
      **                                                             **
      *****************************************************************
      **  DATE     AUTH.  DESCRIPTION                                **
      **                                                             **
P00697**  28OCT02  DPK    CREATED FOR 'ADUP' FILE PROCESSING         **
      *****************************************************************
 
      *----------------
       ADUP-1000-WRITE.
      *----------------
 
           MOVE TFCMD-WRITE-RECORD TO WADUP-SEQ-IO-COMMAND.
           PERFORM  ADUP-1000-LINK
               THRU ADUP-1000-LINK-X.
 
       ADUP-1000-WRITE-X.
           EXIT.
 
      *****************************************************************
      **                 END OF COPYBOOK ACPAADUP                    **
      *****************************************************************
