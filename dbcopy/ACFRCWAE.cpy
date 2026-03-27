      *****************************************************************
      **  MEMBER :  ACFRCWAE                                         **
      **  REMARKS:  CWA ERROR TABLE LAYOUT                           **
      *****************************************************************
      **  RELEASE   DESCRIPTION                                      **
      **                                                             **
      **  25JUL09   CREATED FOR CWAE PROCESSING                      **
      *****************************************************************

       01  RCWAE-REC-INFO.
           05  RCWAE-KEY.
               10  RCWAE-CO-ID                  PIC X(02).
               10  RCWAE-APP-ID                 PIC X(15).
               10  RCWAE-SEQ-NUM                PIC X(03).
               10  RCWAE-SEQ-NUM-N              REDEFINES
                   RCWAE-SEQ-NUM                PIC 9(03).
           05  RCWAE-CWA-CHNL-CD                PIC X(01).
           05  RCWAE-CWA-UPLD-DT                PIC X(10).
           05  RCWAE-RECPT-NUM                  PIC X(10).
           05  RCWAE-RECPT-NUM-N                REDEFINES
               RCWAE-RECPT-NUM                  PIC 9(10).
           05  RCWAE-RECPT-AMT                  PIC S9(13)V9(02) COMP-3.
           05  RCWAE-RECPT-DT                   PIC X(10).
           05  RCWAE-PMT-TYP-CD                 PIC X(01).
           05  RCWAE-REJ-REASN-CD               PIC X(01).
           05  FILLER                           PIC X(20).

      *****************************************************************
      **                 END OF COPYBOOK ACFRCWAE                    **
      *****************************************************************
