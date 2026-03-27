      *****************************************************************
      **  MEMBER :  ACFWUFND                                         **
      **  REMARKS:  APPLICATION UPLOAD FUND TABLE WORK AREA          **
      *****************************************************************
      **  RELEASE   DESCRIPTION                                      **
      **                                                             **
MFFUPL**  01OCT05   CREATED FOR UFND PROCESSING                      **
      *****************************************************************

       01  WUFND-IO-WORK-AREA.
           05  WUFND-TABLE-NAME                    PIC X(04)
                                                   VALUE 'UFND'.
           05  WUFND-RQST-CD                       PIC X(02).
               88  WUFND-RQST-SELECT               VALUE '01'.
               88  WUFND-RQST-SELECT-FOR-UPDATE    VALUE '02'.
               88  WUFND-RQST-UPDATE               VALUE '03'.
               88  WUFND-RQST-CLOSE-UPDATE-CUR     VALUE '04'.
               88  WUFND-RQST-INIT-TBL-LAYOUT      VALUE '05'.
               88  WUFND-RQST-INSERT               VALUE '06'.
               88  WUFND-RQST-DELETE               VALUE '07'.
               88  WUFND-RQST-BROWSE               VALUE '08'.
               88  WUFND-RQST-FETCH-NEXT           VALUE '09'.
               88  WUFND-RQST-CLOSE-BROWSE-CUR     VALUE '10'.
               88  WUFND-RQST-BROWSE-PREV          VALUE '11'.
               88  WUFND-RQST-FETCH-PREV           VALUE '12'.
               88  WUFND-RQST-CLOSE-BR-PREV-CUR    VALUE '13'.
               88  WUFND-RQST-DELETE-KEY-RANGE     VALUE '14'.
               88  WUFND-RQST-DELETE-WITH-KEY      VALUE '15'.
               88  WUFND-RQST-SELECT-MIN           VALUE '21'.
               88  WUFND-RQST-SELECT-MAX           VALUE '22'.
               88  WUFND-RQST-SELECT-COUNT         VALUE '23'.
               88  WUFND-RQST-SELECT-INDEX         VALUE '24'.
               88  WUFND-RQST-BROWSE-INDEX         VALUE '25'.
               88  WUFND-RQST-FETCH-NEXT-INDEX     VALUE '26'.
               88  WUFND-RQST-CLOSE-BROWSE-INDEX   VALUE '27'.
               88  WUFND-RQST-BROWSE-PREV-INDEX    VALUE '28'.
               88  WUFND-RQST-FETCH-PREV-INDEX     VALUE '29'.
               88  WUFND-RQST-CLOSE-BR-PREV-INDEX  VALUE '30'.
               88  WUFND-RQST-SELECT-FOR-UPDATE-2  VALUE '31'.
               88  WUFND-RQST-BROWSE-UCUR          VALUE '32'.
               88  WUFND-RQST-FETCH-NEXT-UCUR      VALUE '33'.
               88  WUFND-RQST-UPDATE-UCUR          VALUE '34'.
               88  WUFND-RQST-DELETE-UCUR          VALUE '35'.
               88  WUFND-RQST-CLOSE-UCUR           VALUE '36'.
               88  WUFND-RQST-BROWSE-IX-UCUR       VALUE '37'.
               88  WUFND-RQST-FETCH-NEXT-IX-UCUR   VALUE '38'.
               88  WUFND-RQST-UPDATE-IX-UCUR       VALUE '39'.
               88  WUFND-RQST-DELETE-IX-UCUR       VALUE '40'.
               88  WUFND-RQST-CLOSE-IX-UCUR        VALUE '41'.
               88  WUFND-RQST-BROWSE-TABLE         VALUE '42'.
               88  WUFND-RQST-FETCH-NEXT-TABLE     VALUE '43'.
               88  WUFND-RQST-CLOSE-BROWSE-TABLE   VALUE '44'.
               88  WUFND-RQST-UPDATE-LOB           VALUE '45'.
               88  WUFND-RQST-SELECT-LOB           VALUE '46'.
               88  WUFND-RQST-FREE-LOB             VALUE '47'.
           05  WUFND-IO-STATUS                     PIC 9(01)
                                                   VALUE 9.
               88  WUFND-IO-OK                     VALUE 0.
               88  WUFND-IO-TS-MISMATCH            VALUE 6.
               88  WUFND-IO-NOT-FOUND              VALUE 7.
               88  WUFND-IO-EOF                    VALUE 8.
               88  WUFND-IO-ERROR                  VALUE 9.
           05  WUFND-OPTM-SQL-STAT-CD              PIC X(01).
               88  WUFND-OPTM-SQL-OK               VALUE '0'.
               88  WUFND-OPTM-SQL-IMPRV            VALUE '1'.
               88  WUFND-OPTM-SQL-ERROR            VALUE '2'.
           05  WUFND-OPTM-SQL-REQIR                PIC X(02).
           05  WUFND-OPTM-SQL-EXEC                 PIC X(02).
           05  WUFND-ENVRMNT-CD                    PIC X(02).
               88  WUFND-ENVRMNT-FOREGROUND        VALUE 'FG'.
               88  WUFND-ENVRMNT-BACKGROUND        VALUE 'BG'.
               88  WUFND-ENVRMNT-BATCH             VALUE 'BA'.
               88  WUFND-ENVRMNT-GUI               VALUE 'GU'.
           05  WUFND-CHECK-UPDT-TS                 PIC X(26).
           05  WUFND-KEY.
               10  WUFND-APP-ID                    PIC X(15).
               10  WUFND-STCKR-ID                  PIC X(11).
               10  WUFND-PLAN-ID                   PIC X(15).
               10  WUFND-FND-ID                    PIC X(05).
           05  WUFND-ENDBR-KEY.
               10  WUFND-ENDBR-APP-ID              PIC X(15).
               10  WUFND-ENDBR-STCKR-ID            PIC X(11).
               10  WUFND-ENDBR-PLAN-ID             PIC X(15).
               10  WUFND-ENDBR-FND-ID              PIC X(05).
           05  FILLER                              PIC X(20).

      *****************************************************************
      **                  END OF COPYBOOK ACFWUFND                   **
      *****************************************************************
