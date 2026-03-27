      *****************************************************************
      **  MEMBER :  ACFWUAPE                                         **
      **  REMARKS:  INCOMPLETENESS INFORMATION UPLOAD TABLE WORK AREA**
      *****************************************************************
      **  RELEASE   DESCRIPTION                                      **
      **                                                             **
      **  6.5       CREATED FOR UAPE PROCESSING                      **
      *****************************************************************

       01  WUAPE-IO-WORK-AREA.
           05  WUAPE-TABLE-NAME                    PIC X(04)
                                                   VALUE 'UAPE'.
           05  WUAPE-RQST-CD                       PIC X(02).
               88  WUAPE-RQST-SELECT               VALUE '01'.
               88  WUAPE-RQST-SELECT-FOR-UPDATE    VALUE '02'.
               88  WUAPE-RQST-UPDATE               VALUE '03'.
               88  WUAPE-RQST-CLOSE-UPDATE-CUR     VALUE '04'.
               88  WUAPE-RQST-INIT-TBL-LAYOUT      VALUE '05'.
               88  WUAPE-RQST-INSERT               VALUE '06'.
               88  WUAPE-RQST-DELETE               VALUE '07'.
               88  WUAPE-RQST-BROWSE               VALUE '08'.
               88  WUAPE-RQST-FETCH-NEXT           VALUE '09'.
               88  WUAPE-RQST-CLOSE-BROWSE-CUR     VALUE '10'.
               88  WUAPE-RQST-BROWSE-PREV          VALUE '11'.
               88  WUAPE-RQST-FETCH-PREV           VALUE '12'.
               88  WUAPE-RQST-CLOSE-BR-PREV-CUR    VALUE '13'.
               88  WUAPE-RQST-DELETE-KEY-RANGE     VALUE '14'.
               88  WUAPE-RQST-DELETE-WITH-KEY      VALUE '15'.
               88  WUAPE-RQST-SELECT-MIN           VALUE '21'.
               88  WUAPE-RQST-SELECT-MAX           VALUE '22'.
               88  WUAPE-RQST-SELECT-COUNT         VALUE '23'.
               88  WUAPE-RQST-SELECT-INDEX         VALUE '24'.
               88  WUAPE-RQST-BROWSE-INDEX         VALUE '25'.
               88  WUAPE-RQST-FETCH-NEXT-INDEX     VALUE '26'.
               88  WUAPE-RQST-CLOSE-BROWSE-INDEX   VALUE '27'.
               88  WUAPE-RQST-BROWSE-PREV-INDEX    VALUE '28'.
               88  WUAPE-RQST-FETCH-PREV-INDEX     VALUE '29'.
               88  WUAPE-RQST-CLOSE-BR-PREV-INDEX  VALUE '30'.
               88  WUAPE-RQST-SELECT-FOR-UPDATE-2  VALUE '31'.
               88  WUAPE-RQST-BROWSE-UCUR          VALUE '32'.
               88  WUAPE-RQST-FETCH-NEXT-UCUR      VALUE '33'.
               88  WUAPE-RQST-UPDATE-UCUR          VALUE '34'.
               88  WUAPE-RQST-DELETE-UCUR          VALUE '35'.
               88  WUAPE-RQST-CLOSE-UCUR           VALUE '36'.
               88  WUAPE-RQST-BROWSE-IX-UCUR       VALUE '37'.
               88  WUAPE-RQST-FETCH-NEXT-IX-UCUR   VALUE '38'.
               88  WUAPE-RQST-UPDATE-IX-UCUR       VALUE '39'.
               88  WUAPE-RQST-DELETE-IX-UCUR       VALUE '40'.
               88  WUAPE-RQST-CLOSE-IX-UCUR        VALUE '41'.
               88  WUAPE-RQST-BROWSE-TABLE         VALUE '42'.
               88  WUAPE-RQST-FETCH-NEXT-TABLE     VALUE '43'.
               88  WUAPE-RQST-CLOSE-BROWSE-TABLE   VALUE '44'.
               88  WUAPE-RQST-UPDATE-LOB           VALUE '45'.
               88  WUAPE-RQST-SELECT-LOB           VALUE '46'.
               88  WUAPE-RQST-FREE-LOB             VALUE '47'.
           05  WUAPE-IO-STATUS                     PIC 9(01)
                                                   VALUE 9.
               88  WUAPE-IO-OK                     VALUE 0.
               88  WUAPE-IO-TS-MISMATCH            VALUE 6.
               88  WUAPE-IO-NOT-FOUND              VALUE 7.
               88  WUAPE-IO-EOF                    VALUE 8.
               88  WUAPE-IO-ERROR                  VALUE 9.
           05  WUAPE-OPTM-SQL-STAT-CD              PIC X(01).
               88  WUAPE-OPTM-SQL-OK               VALUE '0'.
               88  WUAPE-OPTM-SQL-IMPRV            VALUE '1'.
               88  WUAPE-OPTM-SQL-ERROR            VALUE '2'.
           05  WUAPE-OPTM-SQL-REQIR                PIC X(02).
           05  WUAPE-OPTM-SQL-EXEC                 PIC X(02).
           05  WUAPE-ENVRMNT-CD                    PIC X(02).
               88  WUAPE-ENVRMNT-FOREGROUND        VALUE 'FG'.
               88  WUAPE-ENVRMNT-BACKGROUND        VALUE 'BG'.
               88  WUAPE-ENVRMNT-BATCH             VALUE 'BA'.
               88  WUAPE-ENVRMNT-GUI               VALUE 'GU'.
           05  WUAPE-CHECK-UPDT-TS                 PIC X(26).
           05  WUAPE-KEY.
               10  WUAPE-APP-ID                    PIC X(15).
               10  WUAPE-INCMPLT-SEQ-NUM           PIC X(02).
               10  WUAPE-INCMPLT-SEQ-NUM-N         REDEFINES
                   WUAPE-INCMPLT-SEQ-NUM           PIC 9(02).
           05  WUAPE-ENDBR-KEY.
               10  WUAPE-ENDBR-APP-ID              PIC X(15).
               10  WUAPE-ENDBR-INCMPLT-SEQ-NUM     PIC X(02).
               10  WUAPE-ENDBR-INCMPLT-SEQ-NUM-N   REDEFINES
                   WUAPE-ENDBR-INCMPLT-SEQ-NUM     PIC 9(02).
           05  FILLER                              PIC X(20).

      *****************************************************************
      **                  END OF COPYBOOK ACFWUAPE                   **
      *****************************************************************
