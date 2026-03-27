      *****************************************************************
      **  MEMBER :  ACFWUCVG                                         **
      **  REMARKS:  APPLICATION UPLOAD COVERAGE TABLE WORK AREA      **
      *****************************************************************
      **  RELEASE   DESCRIPTION                                      **
      **                                                             **
      **            CREATED FOR UCVG TABLE PROCESSING                **
      *****************************************************************

       01  WUCVG-IO-WORK-AREA.
           05  WUCVG-TABLE-NAME                    PIC X(04)
                                                   VALUE 'UCVG'.
           05  WUCVG-RQST-CD                       PIC X(02).
               88  WUCVG-RQST-SELECT               VALUE '01'.
               88  WUCVG-RQST-SELECT-FOR-UPDATE    VALUE '02'.
               88  WUCVG-RQST-UPDATE               VALUE '03'.
               88  WUCVG-RQST-CLOSE-UPDATE-CUR     VALUE '04'.
               88  WUCVG-RQST-INIT-TBL-LAYOUT      VALUE '05'.
               88  WUCVG-RQST-INSERT               VALUE '06'.
               88  WUCVG-RQST-DELETE               VALUE '07'.
               88  WUCVG-RQST-BROWSE               VALUE '08'.
               88  WUCVG-RQST-FETCH-NEXT           VALUE '09'.
               88  WUCVG-RQST-CLOSE-BROWSE-CUR     VALUE '10'.
               88  WUCVG-RQST-BROWSE-PREV          VALUE '11'.
               88  WUCVG-RQST-FETCH-PREV           VALUE '12'.
               88  WUCVG-RQST-CLOSE-BR-PREV-CUR    VALUE '13'.
               88  WUCVG-RQST-DELETE-KEY-RANGE     VALUE '14'.
               88  WUCVG-RQST-DELETE-WITH-KEY      VALUE '15'.
               88  WUCVG-RQST-SELECT-MIN           VALUE '21'.
               88  WUCVG-RQST-SELECT-MAX           VALUE '22'.
               88  WUCVG-RQST-SELECT-INDEX         VALUE '23'.
               88  WUCVG-RQST-BROWSE-INDEX         VALUE '24'.
               88  WUCVG-RQST-FETCH-NEXT-INDEX     VALUE '25'.
               88  WUCVG-RQST-CLOSE-BROWSE-INDEX   VALUE '26'.
               88  WUCVG-RQST-BROWSE-PREV-INDEX    VALUE '27'.
               88  WUCVG-RQST-FETCH-PREV-INDEX     VALUE '28'.
               88  WUCVG-RQST-CLOSE-BR-PREV-INDEX  VALUE '29'.
               88  WUCVG-RQST-SELECT-FOR-UPDATE-2  VALUE '30'.
               88  WUCVG-RQST-BROWSE-UCUR          VALUE '31'.
               88  WUCVG-RQST-FETCH-NEXT-UCUR      VALUE '32'.
               88  WUCVG-RQST-UPDATE-UCUR          VALUE '33'.
               88  WUCVG-RQST-DELETE-UCUR          VALUE '34'.
               88  WUCVG-RQST-CLOSE-UCUR           VALUE '35'.
               88  WUCVG-RQST-BROWSE-IX-UCUR       VALUE '36'.
               88  WUCVG-RQST-FETCH-NEXT-IX-UCUR   VALUE '37'.
               88  WUCVG-RQST-UPDATE-IX-UCUR       VALUE '38'.
               88  WUCVG-RQST-DELETE-IX-UCUR       VALUE '39'.
               88  WUCVG-RQST-CLOSE-IX-UCUR        VALUE '40'.
           05  WUCVG-IO-STATUS                     PIC 9(01)
                                                   VALUE 9.
               88  WUCVG-IO-OK                     VALUE 0.
               88  WUCVG-IO-TS-MISMATCH            VALUE 6.
               88  WUCVG-IO-NOT-FOUND              VALUE 7.
               88  WUCVG-IO-EOF                    VALUE 8.
               88  WUCVG-IO-ERROR                  VALUE 9.
           05  WUCVG-OPTM-SQL-STAT-CD              PIC X(01).
               88  WUCVG-OPTM-SQL-OK               VALUE '0'.
               88  WUCVG-OPTM-SQL-IMPRV            VALUE '1'.
               88  WUCVG-OPTM-SQL-ERROR            VALUE '2'.
           05  WUCVG-OPTM-SQL-REQIR                PIC X(02).
           05  WUCVG-OPTM-SQL-EXEC                 PIC X(02).
           05  WUCVG-ENVRMNT-CD                    PIC X(02).
               88  WUCVG-ENVRMNT-FOREGROUND        VALUE 'FG'.
               88  WUCVG-ENVRMNT-BACKGROUND        VALUE 'BG'.
               88  WUCVG-ENVRMNT-BATCH             VALUE 'BA'.
               88  WUCVG-ENVRMNT-GUI               VALUE 'GU'.
           05  WUCVG-CHECK-UPDT-TS                 PIC X(26).
           05  WUCVG-KEY.
               10  WUCVG-APP-ID                    PIC X(15).
               10  WUCVG-STCKR-ID                  PIC X(11).
               10  WUCVG-PLAN-ID                   PIC X(15).
           05  WUCVG-ENDBR-KEY.
               10  WUCVG-ENDBR-APP-ID              PIC X(15).
               10  WUCVG-ENDBR-STCKR-ID            PIC X(11).
               10  WUCVG-ENDBR-PLAN-ID             PIC X(15).
           05  FILLER                              PIC X(20).

      *****************************************************************
      **                  END OF COPYBOOK ACFWUCVG                   **
      *****************************************************************
