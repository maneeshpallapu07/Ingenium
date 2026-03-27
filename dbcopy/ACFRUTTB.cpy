      *****************************************************************
      **  MEMBER : ACFRUTTB                                          **
      **  REMARKS: TRANSLATION FILE RECORD LAYOUT                    **
      **  LENGTH : 80                                                **
      *****************************************************************
      **  DATE    AUTHOR   DESCRIPTION                               **
      **                                                             **
      *****************************************************************
 
       01  RUTTB-REC-INFO.
           05  RUTTB-KEY.
               10  RUTTB-CO-ID                   PIC X(02).
               10  RUTTB-UPLD-TTBL-TYP-ID        PIC X(05).
               10  RUTTB-UPLD-TTBL-VALU-ID       PIC X(25).
           05  RUTTB-PREV-UPDT-USER-ID           PIC X(08).
           05  RUTTB-PREV-UPDT-DT                PIC X(10).
           05  RUTTB-UPLD-TTBL-VALU-TXT          PIC X(25).
           05  FILLER                            PIC X(05).
 
      *****************************************************************
      **                 END OF COPYBOOK ACFRUTTB                    **
      *****************************************************************
