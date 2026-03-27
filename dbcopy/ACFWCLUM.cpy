      *****************************************************************
      **  MEMBER :  ACFWCLUM                                         **
      **  REMARKS:  ARM 2 NON FACE CLIENT UNMATCHED EXTRACT TABLE    **
      **            WORK AREA                                        **
      *****************************************************************
      **  RELEASE   DESCRIPTION                                      **
      **                                                             **
      **  30NOV14   CREATED FOR CLUM PROCESSING                      **
      *****************************************************************

       01  WCLUM-IO-WORK-AREA.
           05  WCLUM-TABLE-NAME                    PIC X(04)
                                                   VALUE 'CLUM'.
           05  WCLUM-RQST-CD                       PIC X(02).
               88  WCLUM-RQST-SELECT               VALUE '01'.
               88  WCLUM-RQST-SELECT-FOR-UPDATE    VALUE '02'.
               88  WCLUM-RQST-UPDATE               VALUE '03'.
               88  WCLUM-RQST-CLOSE-UPDATE-CUR     VALUE '04'.
               88  WCLUM-RQST-INIT-TBL-LAYOUT      VALUE '05'.
               88  WCLUM-RQST-INSERT               VALUE '06'.
               88  WCLUM-RQST-DELETE               VALUE '07'.
               88  WCLUM-RQST-BROWSE               VALUE '08'.
               88  WCLUM-RQST-FETCH-NEXT           VALUE '09'.
               88  WCLUM-RQST-CLOSE-BROWSE-CUR     VALUE '10'.
               88  WCLUM-RQST-BROWSE-PREV          VALUE '11'.
               88  WCLUM-RQST-FETCH-PREV           VALUE '12'.
               88  WCLUM-RQST-CLOSE-BR-PREV-CUR    VALUE '13'.
               88  WCLUM-RQST-DELETE-KEY-RANGE     VALUE '14'.
               88  WCLUM-RQST-DELETE-WITH-KEY      VALUE '15'.
               88  WCLUM-RQST-SELECT-MIN           VALUE '21'.
               88  WCLUM-RQST-SELECT-MAX           VALUE '22'.
               88  WCLUM-RQST-SELECT-COUNT         VALUE '23'.
               88  WCLUM-RQST-SELECT-INDEX         VALUE '24'.
               88  WCLUM-RQST-BROWSE-INDEX         VALUE '25'.
               88  WCLUM-RQST-FETCH-NEXT-INDEX     VALUE '26'.
               88  WCLUM-RQST-CLOSE-BROWSE-INDEX   VALUE '27'.
               88  WCLUM-RQST-BROWSE-PREV-INDEX    VALUE '28'.
               88  WCLUM-RQST-FETCH-PREV-INDEX     VALUE '29'.
               88  WCLUM-RQST-CLOSE-BR-PREV-INDEX  VALUE '30'.
               88  WCLUM-RQST-SELECT-FOR-UPDATE-2  VALUE '31'.
               88  WCLUM-RQST-BROWSE-UCUR          VALUE '32'.
               88  WCLUM-RQST-FETCH-NEXT-UCUR      VALUE '33'.
               88  WCLUM-RQST-UPDATE-UCUR          VALUE '34'.
               88  WCLUM-RQST-DELETE-UCUR          VALUE '35'.
               88  WCLUM-RQST-CLOSE-UCUR           VALUE '36'.
               88  WCLUM-RQST-BROWSE-IX-UCUR       VALUE '37'.
               88  WCLUM-RQST-FETCH-NEXT-IX-UCUR   VALUE '38'.
               88  WCLUM-RQST-UPDATE-IX-UCUR       VALUE '39'.
               88  WCLUM-RQST-DELETE-IX-UCUR       VALUE '40'.
               88  WCLUM-RQST-CLOSE-IX-UCUR        VALUE '41'.
               88  WCLUM-RQST-BROWSE-TABLE         VALUE '42'.
               88  WCLUM-RQST-FETCH-NEXT-TABLE     VALUE '43'.
               88  WCLUM-RQST-CLOSE-BROWSE-TABLE   VALUE '44'.
               88  WCLUM-RQST-UPDATE-LOB           VALUE '45'.
               88  WCLUM-RQST-SELECT-LOB           VALUE '46'.
               88  WCLUM-RQST-FREE-LOB             VALUE '47'.
           05  WCLUM-IO-STATUS                     PIC 9(01)
                                                   VALUE 9.
               88  WCLUM-IO-OK                     VALUE 0.
               88  WCLUM-IO-TS-MISMATCH            VALUE 6.
               88  WCLUM-IO-NOT-FOUND              VALUE 7.
               88  WCLUM-IO-EOF                    VALUE 8.
               88  WCLUM-IO-ERROR                  VALUE 9.
           05  WCLUM-OPTM-SQL-STAT-CD              PIC X(01).
               88  WCLUM-OPTM-SQL-OK               VALUE '0'.
               88  WCLUM-OPTM-SQL-IMPRV            VALUE '1'.
               88  WCLUM-OPTM-SQL-ERROR            VALUE '2'.
           05  WCLUM-OPTM-SQL-REQIR                PIC X(02).
           05  WCLUM-OPTM-SQL-EXEC                 PIC X(02).
           05  WCLUM-COMPANY-REQUIRED-SW           PIC X(01)
                                                   VALUE 'Y'.
               88  WCLUM-COMPANY-REQUIRED          VALUE 'Y'.
               88  WCLUM-COMPANY-NOT-REQUIRED      VALUE 'N'.
           05  WCLUM-ENVRMNT-CD                    PIC X(02).
               88  WCLUM-ENVRMNT-FOREGROUND        VALUE 'FG'.
               88  WCLUM-ENVRMNT-BACKGROUND        VALUE 'BG'.
               88  WCLUM-ENVRMNT-BATCH             VALUE 'BA'.
               88  WCLUM-ENVRMNT-GUI               VALUE 'GU'.
           05  WCLUM-CHECK-UPDT-TS                 PIC X(26).
           05  WCLUM-KEY.
               10  WCLUM-CO-ID                     PIC X(02).
               10  WCLUM-SEQ-FILE-PGM-ID           PIC X(08).
               10  WCLUM-SEQ-FILE-OUTPT-NM         PIC X(08).
               10  WCLUM-SEQ-FILE-INSTC-ID         PIC S9(04) BINARY.
               10  WCLUM-SEQ-FILE-TS               PIC X(26).
           05  WCLUM-ENDBR-KEY.
               10  WCLUM-ENDBR-CO-ID               PIC X(02).
               10  WCLUM-ENDBR-SEQ-FILE-PGM-ID     PIC X(08).
               10  WCLUM-ENDBR-SEQ-FILE-OUTPT-NM   PIC X(08).
               10  WCLUM-ENDBR-SEQ-FILE-INSTC-ID   PIC S9(04) BINARY.
               10  WCLUM-ENDBR-SEQ-FILE-TS         PIC X(26).
           05  FILLER                              PIC X(20).

      *****************************************************************
      **                  END OF COPYBOOK CCFWCLUM                   **
      *****************************************************************
