      *****************************************************************
      **  MEMBER :  ACFWUCLI                                         **
      **  REMARKS:  APPLICATION UPLOAD CLIENT TABLE WORK AREA        **
      *****************************************************************
      **  RELEASE   DESCRIPTION                                      **
      **                                                             **
      **            CREATED FOR UCLI TABLE PROCESSING                **
      *****************************************************************

       01  WUCLI-IO-WORK-AREA.
           05  WUCLI-TABLE-NAME                    PIC X(04)
                                                   VALUE 'UCLI'.
           05  WUCLI-RQST-CD                       PIC X(02).
               88  WUCLI-RQST-SELECT               VALUE '01'.
               88  WUCLI-RQST-SELECT-FOR-UPDATE    VALUE '02'.
               88  WUCLI-RQST-UPDATE               VALUE '03'.
               88  WUCLI-RQST-CLOSE-UPDATE-CUR     VALUE '04'.
               88  WUCLI-RQST-INIT-TBL-LAYOUT      VALUE '05'.
               88  WUCLI-RQST-INSERT               VALUE '06'.
               88  WUCLI-RQST-DELETE               VALUE '07'.
               88  WUCLI-RQST-BROWSE               VALUE '08'.
               88  WUCLI-RQST-FETCH-NEXT           VALUE '09'.
               88  WUCLI-RQST-CLOSE-BROWSE-CUR     VALUE '10'.
               88  WUCLI-RQST-BROWSE-PREV          VALUE '11'.
               88  WUCLI-RQST-FETCH-PREV           VALUE '12'.
               88  WUCLI-RQST-CLOSE-BR-PREV-CUR    VALUE '13'.
               88  WUCLI-RQST-DELETE-KEY-RANGE     VALUE '14'.
               88  WUCLI-RQST-DELETE-WITH-KEY      VALUE '15'.
               88  WUCLI-RQST-SELECT-MIN           VALUE '21'.
               88  WUCLI-RQST-SELECT-MAX           VALUE '22'.
               88  WUCLI-RQST-SELECT-INDEX         VALUE '23'.
               88  WUCLI-RQST-BROWSE-INDEX         VALUE '24'.
               88  WUCLI-RQST-FETCH-NEXT-INDEX     VALUE '25'.
               88  WUCLI-RQST-CLOSE-BROWSE-INDEX   VALUE '26'.
               88  WUCLI-RQST-BROWSE-PREV-INDEX    VALUE '27'.
               88  WUCLI-RQST-FETCH-PREV-INDEX     VALUE '28'.
               88  WUCLI-RQST-CLOSE-BR-PREV-INDEX  VALUE '29'.
               88  WUCLI-RQST-SELECT-FOR-UPDATE-2  VALUE '30'.
               88  WUCLI-RQST-BROWSE-UCUR          VALUE '31'.
               88  WUCLI-RQST-FETCH-NEXT-UCUR      VALUE '32'.
               88  WUCLI-RQST-UPDATE-UCUR          VALUE '33'.
               88  WUCLI-RQST-DELETE-UCUR          VALUE '34'.
               88  WUCLI-RQST-CLOSE-UCUR           VALUE '35'.
               88  WUCLI-RQST-BROWSE-IX-UCUR       VALUE '36'.
               88  WUCLI-RQST-FETCH-NEXT-IX-UCUR   VALUE '37'.
               88  WUCLI-RQST-UPDATE-IX-UCUR       VALUE '38'.
               88  WUCLI-RQST-DELETE-IX-UCUR       VALUE '39'.
               88  WUCLI-RQST-CLOSE-IX-UCUR        VALUE '40'.
           05  WUCLI-IO-STATUS                     PIC 9(01)
                                                   VALUE 9.
               88  WUCLI-IO-OK                     VALUE 0.
               88  WUCLI-IO-TS-MISMATCH            VALUE 6.
               88  WUCLI-IO-NOT-FOUND              VALUE 7.
               88  WUCLI-IO-EOF                    VALUE 8.
               88  WUCLI-IO-ERROR                  VALUE 9.
           05  WUCLI-OPTM-SQL-STAT-CD              PIC X(01).
               88  WUCLI-OPTM-SQL-OK               VALUE '0'.
               88  WUCLI-OPTM-SQL-IMPRV            VALUE '1'.
               88  WUCLI-OPTM-SQL-ERROR            VALUE '2'.
           05  WUCLI-OPTM-SQL-REQIR                PIC X(02).
           05  WUCLI-OPTM-SQL-EXEC                 PIC X(02).
           05  WUCLI-ENVRMNT-CD                    PIC X(02).
               88  WUCLI-ENVRMNT-FOREGROUND        VALUE 'FG'.
               88  WUCLI-ENVRMNT-BACKGROUND        VALUE 'BG'.
               88  WUCLI-ENVRMNT-BATCH             VALUE 'BA'.
               88  WUCLI-ENVRMNT-GUI               VALUE 'GU'.
           05  WUCLI-CHECK-UPDT-TS                 PIC X(26).
           05  WUCLI-KEY.
               10  WUCLI-APP-ID                    PIC X(15).
               10  WUCLI-SEQ-NUM                   PIC X(02).
               10  WUCLI-SEQ-NUM-N                 REDEFINES
                   WUCLI-SEQ-NUM                   PIC 9(02).
           05  WUCLI-ENDBR-KEY.
               10  WUCLI-ENDBR-APP-ID              PIC X(15).
               10  WUCLI-ENDBR-SEQ-NUM             PIC X(02).
               10  WUCLI-ENDBR-SEQ-NUM-N           REDEFINES
                   WUCLI-ENDBR-SEQ-NUM             PIC 9(02).
           05  FILLER                              PIC X(20).

      *****************************************************************
      **                  END OF COPYBOOK ACFWUCLI                   **
      *****************************************************************
