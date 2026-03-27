      *****************************************************************
      **  MEMBER :  ACFWUCNV                                         **
      **  REMARKS:  APPLICATION UPLOAD POLICY CONVERSION TABLE WORK  **
      **            AREA                                             **
      *****************************************************************
      **  RELEASE   DESCRIPTION                                      **
      **                                                             **
      **  15AUG02   CREATED FOR UCNV TABLE PROCESSING                **
      *****************************************************************

       01  WUCNV-IO-WORK-AREA.
           05  WUCNV-TABLE-NAME                    PIC X(04)
                                                   VALUE 'UCNV'.
           05  WUCNV-RQST-CD                       PIC X(02).
               88  WUCNV-RQST-SELECT               VALUE '01'.
               88  WUCNV-RQST-SELECT-FOR-UPDATE    VALUE '02'.
               88  WUCNV-RQST-UPDATE               VALUE '03'.
               88  WUCNV-RQST-CLOSE-UPDATE-CUR     VALUE '04'.
               88  WUCNV-RQST-INIT-TBL-LAYOUT      VALUE '05'.
               88  WUCNV-RQST-INSERT               VALUE '06'.
               88  WUCNV-RQST-DELETE               VALUE '07'.
               88  WUCNV-RQST-BROWSE               VALUE '08'.
               88  WUCNV-RQST-FETCH-NEXT           VALUE '09'.
               88  WUCNV-RQST-CLOSE-BROWSE-CUR     VALUE '10'.
               88  WUCNV-RQST-BROWSE-PREV          VALUE '11'.
               88  WUCNV-RQST-FETCH-PREV           VALUE '12'.
               88  WUCNV-RQST-CLOSE-BR-PREV-CUR    VALUE '13'.
               88  WUCNV-RQST-DELETE-KEY-RANGE     VALUE '14'.
               88  WUCNV-RQST-DELETE-WITH-KEY      VALUE '15'.
               88  WUCNV-RQST-SELECT-MIN           VALUE '21'.
               88  WUCNV-RQST-SELECT-MAX           VALUE '22'.
               88  WUCNV-RQST-SELECT-INDEX         VALUE '23'.
               88  WUCNV-RQST-BROWSE-INDEX         VALUE '24'.
               88  WUCNV-RQST-FETCH-NEXT-INDEX     VALUE '25'.
               88  WUCNV-RQST-CLOSE-BROWSE-INDEX   VALUE '26'.
               88  WUCNV-RQST-BROWSE-PREV-INDEX    VALUE '27'.
               88  WUCNV-RQST-FETCH-PREV-INDEX     VALUE '28'.
               88  WUCNV-RQST-CLOSE-BR-PREV-INDEX  VALUE '29'.
               88  WUCNV-RQST-SELECT-FOR-UPDATE-2  VALUE '30'.
               88  WUCNV-RQST-BROWSE-UCUR          VALUE '31'.
               88  WUCNV-RQST-FETCH-NEXT-UCUR      VALUE '32'.
               88  WUCNV-RQST-UPDATE-UCUR          VALUE '33'.
               88  WUCNV-RQST-DELETE-UCUR          VALUE '34'.
               88  WUCNV-RQST-CLOSE-UCUR           VALUE '35'.
               88  WUCNV-RQST-BROWSE-IX-UCUR       VALUE '36'.
               88  WUCNV-RQST-FETCH-NEXT-IX-UCUR   VALUE '37'.
               88  WUCNV-RQST-UPDATE-IX-UCUR       VALUE '38'.
               88  WUCNV-RQST-DELETE-IX-UCUR       VALUE '39'.
               88  WUCNV-RQST-CLOSE-IX-UCUR        VALUE '40'.
           05  WUCNV-IO-STATUS                     PIC 9(01)
                                                   VALUE 9.
               88  WUCNV-IO-OK                     VALUE 0.
               88  WUCNV-IO-TS-MISMATCH            VALUE 6.
               88  WUCNV-IO-NOT-FOUND              VALUE 7.
               88  WUCNV-IO-EOF                    VALUE 8.
               88  WUCNV-IO-ERROR                  VALUE 9.
           05  WUCNV-OPTM-SQL-STAT-CD              PIC X(01).
               88  WUCNV-OPTM-SQL-OK               VALUE '0'.
               88  WUCNV-OPTM-SQL-IMPRV            VALUE '1'.
               88  WUCNV-OPTM-SQL-ERROR            VALUE '2'.
           05  WUCNV-OPTM-SQL-REQIR                PIC X(02).
           05  WUCNV-OPTM-SQL-EXEC                 PIC X(02).
           05  WUCNV-ENVRMNT-CD                    PIC X(02).
               88  WUCNV-ENVRMNT-FOREGROUND        VALUE 'FG'.
               88  WUCNV-ENVRMNT-BACKGROUND        VALUE 'BG'.
               88  WUCNV-ENVRMNT-BATCH             VALUE 'BA'.
               88  WUCNV-ENVRMNT-GUI               VALUE 'GU'.
           05  WUCNV-CHECK-UPDT-TS                 PIC X(26).
           05  WUCNV-KEY.
               10  WUCNV-APP-ID                    PIC X(15).
               10  WUCNV-SEQ-NUM                   PIC X(03).
               10  WUCNV-SEQ-NUM-N                 REDEFINES
                   WUCNV-SEQ-NUM                   PIC 9(03).
           05  WUCNV-ENDBR-KEY.
               10  WUCNV-ENDBR-APP-ID              PIC X(15).
               10  WUCNV-ENDBR-SEQ-NUM             PIC X(03).
               10  WUCNV-ENDBR-SEQ-NUM-N           REDEFINES
                   WUCNV-ENDBR-SEQ-NUM             PIC 9(03).
           05  FILLER                              PIC X(20).

      *****************************************************************
      **                  END OF COPYBOOK ACFWUCNV                   **
      *****************************************************************
