      *****************************************************************
      **  MEMBER :  ACFR2120                                         **
      **  REMARKS:  APEX TO NBS IMPORT MESSAGE EXTRACT TABLE LAYOUT  **
      **  LENGTH :  1068                                             **
      *****************************************************************
      **  RELEASE   DESCRIPTION                                      **
      **                                                             **
      **  612J      CREATED FOR 2120 TABLE PROCESSING                **
      *****************************************************************

       01  R2120-REC-INFO.
           05  R2120-KEY.
               10  R2120-CO-ID                  PIC X(02).
               10  R2120-SEQ-FILE-PGM-ID        PIC X(08).
               10  R2120-SEQ-FILE-OUTPT-NM      PIC X(08).
               10  R2120-SEQ-FILE-INSTC-ID      PIC S9(04) BINARY.
               10  R2120-SEQ-FILE-TS            PIC X(26).
           05  R2120-SEQ-FILE-REC-INFO.
               49  R2120-SEQ-FILE-REC-INFO-LEN  PIC S9(04) BINARY.
               49  R2120-SEQ-FILE-REC-INFO-TXT  PIC X(1000).
           05  FILLER                           PIC X(20).

      *****************************************************************
      **                 END OF COPYBOOK ACFR2120                    **
      *****************************************************************
