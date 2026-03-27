      *****************************************************************
      **  MEMBER :  ACFWCWAE                                         **
      **  REMARKS:  CWA ERROR TABLE WORK AREA                        **
      *****************************************************************
      **  RELEASE   DESCRIPTION                                      **
      **                                                             **
      **  25JUL09   CREATED FOR CWAE PROCESSING                      **
      *****************************************************************

       01  WCWAE-IO-WORK-AREA.
           05  WCWAE-TABLE-NAME                    PIC X(04)
                                                   VALUE 'CWAE'.
           05  WCWAE-RQST-CD                       PIC X(02).
               88  WCWAE-RQST-SELECT               VALUE '01'.
               88  WCWAE-RQST-SELECT-FOR-UPDATE    VALUE '02'.
               88  WCWAE-RQST-UPDATE               VALUE '03'.
               88  WCWAE-RQST-CLOSE-UPDATE-CUR     VALUE '04'.
               88  WCWAE-RQST-INIT-TBL-LAYOUT      VALUE '05'.
               88  WCWAE-RQST-INSERT               VALUE '06'.
               88  WCWAE-RQST-DELETE               VALUE '07'.
               88  WCWAE-RQST-BROWSE               VALUE '08'.
               88  WCWAE-RQST-FETCH-NEXT           VALUE '09'.
               88  WCWAE-RQST-CLOSE-BROWSE-CUR     VALUE '10'.
               88  WCWAE-RQST-BROWSE-PREV          VALUE '11'.
               88  WCWAE-RQST-FETCH-PREV           VALUE '12'.
               88  WCWAE-RQST-CLOSE-BR-PREV-CUR    VALUE '13'.
               88  WCWAE-RQST-DELETE-KEY-RANGE     VALUE '14'.
               88  WCWAE-RQST-DELETE-WITH-KEY      VALUE '15'.
               88  WCWAE-RQST-SELECT-MIN           VALUE '21'.
               88  WCWAE-RQST-SELECT-MAX           VALUE '22'.
               88  WCWAE-RQST-SELECT-COUNT         VALUE '23'.
               88  WCWAE-RQST-SELECT-INDEX         VALUE '24'.
               88  WCWAE-RQST-BROWSE-INDEX         VALUE '25'.
               88  WCWAE-RQST-FETCH-NEXT-INDEX     VALUE '26'.
               88  WCWAE-RQST-CLOSE-BROWSE-INDEX   VALUE '27'.
               88  WCWAE-RQST-BROWSE-PREV-INDEX    VALUE '28'.
               88  WCWAE-RQST-FETCH-PREV-INDEX     VALUE '29'.
               88  WCWAE-RQST-CLOSE-BR-PREV-INDEX  VALUE '30'.
               88  WCWAE-RQST-SELECT-FOR-UPDATE-2  VALUE '31'.
               88  WCWAE-RQST-BROWSE-UCUR          VALUE '32'.
               88  WCWAE-RQST-FETCH-NEXT-UCUR      VALUE '33'.
               88  WCWAE-RQST-UPDATE-UCUR          VALUE '34'.
               88  WCWAE-RQST-DELETE-UCUR          VALUE '35'.
               88  WCWAE-RQST-CLOSE-UCUR           VALUE '36'.
               88  WCWAE-RQST-BROWSE-IX-UCUR       VALUE '37'.
               88  WCWAE-RQST-FETCH-NEXT-IX-UCUR   VALUE '38'.
               88  WCWAE-RQST-UPDATE-IX-UCUR       VALUE '39'.
               88  WCWAE-RQST-DELETE-IX-UCUR       VALUE '40'.
               88  WCWAE-RQST-CLOSE-IX-UCUR        VALUE '41'.
               88  WCWAE-RQST-BROWSE-TABLE         VALUE '42'.
               88  WCWAE-RQST-FETCH-NEXT-TABLE     VALUE '43'.
               88  WCWAE-RQST-CLOSE-BROWSE-TABLE   VALUE '44'.
               88  WCWAE-RQST-UPDATE-LOB           VALUE '45'.
               88  WCWAE-RQST-SELECT-LOB           VALUE '46'.
               88  WCWAE-RQST-FREE-LOB             VALUE '47'.
           05  WCWAE-IO-STATUS                     PIC 9(01)
                                                   VALUE 9.
               88  WCWAE-IO-OK                     VALUE 0.
               88  WCWAE-IO-TS-MISMATCH            VALUE 6.
               88  WCWAE-IO-NOT-FOUND              VALUE 7.
               88  WCWAE-IO-EOF                    VALUE 8.
               88  WCWAE-IO-ERROR                  VALUE 9.
           05  WCWAE-OPTM-SQL-STAT-CD              PIC X(01).
               88  WCWAE-OPTM-SQL-OK               VALUE '0'.
               88  WCWAE-OPTM-SQL-IMPRV            VALUE '1'.
               88  WCWAE-OPTM-SQL-ERROR            VALUE '2'.
           05  WCWAE-OPTM-SQL-REQIR                PIC X(02).
           05  WCWAE-OPTM-SQL-EXEC                 PIC X(02).
           05  WCWAE-COMPANY-REQUIRED-SW           PIC X(01)
                                                   VALUE 'Y'.
               88  WCWAE-COMPANY-REQUIRED          VALUE 'Y'.
               88  WCWAE-COMPANY-NOT-REQUIRED      VALUE 'N'.
           05  WCWAE-ENVRMNT-CD                    PIC X(02).
               88  WCWAE-ENVRMNT-FOREGROUND        VALUE 'FG'.
               88  WCWAE-ENVRMNT-BACKGROUND        VALUE 'BG'.
               88  WCWAE-ENVRMNT-BATCH             VALUE 'BA'.
               88  WCWAE-ENVRMNT-GUI               VALUE 'GU'.
           05  WCWAE-CHECK-UPDT-TS                 PIC X(26).
           05  WCWAE-KEY.
               10  WCWAE-CO-ID                     PIC X(02).
               10  WCWAE-APP-ID                    PIC X(15).
               10  WCWAE-SEQ-NUM                   PIC X(03).
               10  WCWAE-SEQ-NUM-N                 REDEFINES
                   WCWAE-SEQ-NUM                   PIC 9(03).
           05  WCWAE-ENDBR-KEY.
               10  WCWAE-ENDBR-CO-ID               PIC X(02).
               10  WCWAE-ENDBR-APP-ID              PIC X(15).
               10  WCWAE-ENDBR-SEQ-NUM             PIC X(03).
               10  WCWAE-ENDBR-SEQ-NUM-N           REDEFINES
                   WCWAE-ENDBR-SEQ-NUM             PIC 9(03).
           05  FILLER                              PIC X(20).

      *****************************************************************
      **                  END OF COPYBOOK ACFWCWAE                   **
      *****************************************************************
