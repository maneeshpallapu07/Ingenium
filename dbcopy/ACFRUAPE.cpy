      *****************************************************************
      **  MEMBER :  ACFRUAPE                                         **
      **  REMARKS:  INCOMPLETENESS INFORMATION UPLOAD TABLE LAYOUT   **
      *****************************************************************
      **  RELEASE   DESCRIPTION                                      **
      **                                                             **
      **  6.5       CREATED FOR UAPE PROCESSING                      **
      *****************************************************************

       01  RUAPE-REC-INFO.
           05  RUAPE-KEY.
               10  RUAPE-APP-ID                 PIC X(15).
               10  RUAPE-INCMPLT-SEQ-NUM        PIC X(02).
               10  RUAPE-INCMPLT-SEQ-NUM-N      REDEFINES
                   RUAPE-INCMPLT-SEQ-NUM        PIC 9(02).
           05  RUAPE-INCMPLT-ID                 PIC X(06).
           05  RUAPE-INCMPLT-DTL-TXT.
               49  RUAPE-INCMPLT-DTL-TXT-LEN    PIC S9(04) BINARY.
               49  RUAPE-INCMPLT-DTL-TXT-TXT    PIC X(400).
           05  FILLER                           PIC X(20).

      *****************************************************************
      **                 END OF COPYBOOK ACFRUAPE                    **
      *****************************************************************

        *****************************************************************
      **                 END OF COPYBOOK ACFRUAPE                    **
      *****************************************************************
