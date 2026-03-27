      *****************************************************************
      **  MEMBER :  ACFRCLUM                                         **
      **  REMARKS:  ARM 2 NON FACE CLIENT UNMATCHED EXTRACT TABLE    **
      **            LAYOUT                                           **
      *****************************************************************
      **  RELEASE   DESCRIPTION                                      **
      **                                                             **
      **  30NOV14   CREATED FOR CLUM PROCESSING                      **
      *****************************************************************

       01  RCLUM-REC-INFO.
           05  RCLUM-KEY.
               10  RCLUM-CO-ID                  PIC X(02).
               10  RCLUM-SEQ-FILE-PGM-ID        PIC X(08).
               10  RCLUM-SEQ-FILE-OUTPT-NM      PIC X(08).
               10  RCLUM-SEQ-FILE-INSTC-ID      PIC S9(04) BINARY.
               10  RCLUM-SEQ-FILE-TS            PIC X(26).
           05  RCLUM-SEQ-FILE-REC-INFO.
               49  RCLUM-SEQ-FILE-REC-INFO-LEN  PIC S9(04) BINARY.
               49  RCLUM-SEQ-FILE-REC-INFO-TXT  PIC X(1000).
           05  FILLER                           PIC X(20).

      *****************************************************************
      **                 END OF COPYBOOK CCFRCLUM                    **
      *****************************************************************
