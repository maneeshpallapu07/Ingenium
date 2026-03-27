      *****************************************************************
      **  MEMBER :  ACFRXMLE                                         **
      **  REMARKS:  XML MESSAGE EXTRACT TABLE LAYOUT                 **
      *****************************************************************
      **  RELEASE   DESCRIPTION                                      **
      **                                                             **
      **  23JUL09   CREATED FOR XMLE PROCESSING                      **
      *****************************************************************

       01  RXMLE-REC-INFO.
           05  RXMLE-KEY.
               10  RXMLE-CO-ID                  PIC X(02).
               10  RXMLE-APP-ID                 PIC X(15).
               10  RXMLE-APP-CHNL-CD            PIC X(01).
               10  RXMLE-APP-UPLD-DT            PIC X(10).
           05  RXMLE-APP-REJ-REASN-CD           PIC X(01).
           05  FILLER                           PIC X(20).

      *****************************************************************
      **                 END OF COPYBOOK ACFRXMLE                    **
      *****************************************************************
