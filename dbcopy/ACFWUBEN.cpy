      *****************************************************************
      **  MEMBER :  ACFWUBEN                                         **
      **  REMARKS:  APPLICATION UPLOAD BENEFICIARY TABLE WORK AREA   **
      *****************************************************************
      **  RELEASE   DESCRIPTION                                      **
      **                                                             **
C12392**  02NOV2011 CREATED FOR UBEN PROCESSING                      **
      *****************************************************************

       01  WUBEN-IO-WORK-AREA.
           05  WUBEN-TABLE-NAME                    PIC X(04)
                                                   VALUE 'UBEN'.
           05  WUBEN-RQST-CD                       PIC X(02).
               88  WUBEN-RQST-SELECT               VALUE '01'.
               88  WUBEN-RQST-SELECT-FOR-UPDATE    VALUE '02'.
               88  WUBEN-RQST-UPDATE               VALUE '03'.
               88  WUBEN-RQST-CLOSE-UPDATE-CUR     VALUE '04'.
               88  WUBEN-RQST-INIT-TBL-LAYOUT      VALUE '05'.
               88  WUBEN-RQST-INSERT               VALUE '06'.
               88  WUBEN-RQST-DELETE               VALUE '07'.
               88  WUBEN-RQST-BROWSE               VALUE '08'.
               88  WUBEN-RQST-FETCH-NEXT           VALUE '09'.
               88  WUBEN-RQST-CLOSE-BROWSE-CUR     VALUE '10'.
               88  WUBEN-RQST-BROWSE-PREV          VALUE '11'.
               88  WUBEN-RQST-FETCH-PREV           VALUE '12'.
               88  WUBEN-RQST-CLOSE-BR-PREV-CUR    VALUE '13'.
               88  WUBEN-RQST-DELETE-KEY-RANGE     VALUE '14'.
               88  WUBEN-RQST-DELETE-WITH-KEY      VALUE '15'.
               88  WUBEN-RQST-SELECT-MIN           VALUE '21'.
               88  WUBEN-RQST-SELECT-MAX           VALUE '22'.
               88  WUBEN-RQST-SELECT-COUNT         VALUE '23'.
               88  WUBEN-RQST-SELECT-INDEX         VALUE '24'.
               88  WUBEN-RQST-BROWSE-INDEX         VALUE '25'.
               88  WUBEN-RQST-FETCH-NEXT-INDEX     VALUE '26'.
               88  WUBEN-RQST-CLOSE-BROWSE-INDEX   VALUE '27'.
               88  WUBEN-RQST-BROWSE-PREV-INDEX    VALUE '28'.
               88  WUBEN-RQST-FETCH-PREV-INDEX     VALUE '29'.
               88  WUBEN-RQST-CLOSE-BR-PREV-INDEX  VALUE '30'.
               88  WUBEN-RQST-SELECT-FOR-UPDATE-2  VALUE '31'.
               88  WUBEN-RQST-BROWSE-UCUR          VALUE '32'.
               88  WUBEN-RQST-FETCH-NEXT-UCUR      VALUE '33'.
               88  WUBEN-RQST-UPDATE-UCUR          VALUE '34'.
               88  WUBEN-RQST-DELETE-UCUR          VALUE '35'.
               88  WUBEN-RQST-CLOSE-UCUR           VALUE '36'.
               88  WUBEN-RQST-BROWSE-IX-UCUR       VALUE '37'.
               88  WUBEN-RQST-FETCH-NEXT-IX-UCUR   VALUE '38'.
               88  WUBEN-RQST-UPDATE-IX-UCUR       VALUE '39'.
               88  WUBEN-RQST-DELETE-IX-UCUR       VALUE '40'.
               88  WUBEN-RQST-CLOSE-IX-UCUR        VALUE '41'.
               88  WUBEN-RQST-BROWSE-TABLE         VALUE '42'.
               88  WUBEN-RQST-FETCH-NEXT-TABLE     VALUE '43'.
               88  WUBEN-RQST-CLOSE-BROWSE-TABLE   VALUE '44'.
               88  WUBEN-RQST-UPDATE-LOB           VALUE '45'.
               88  WUBEN-RQST-SELECT-LOB           VALUE '46'.
               88  WUBEN-RQST-FREE-LOB             VALUE '47'.
           05  WUBEN-IO-STATUS                     PIC 9(01)
                                                   VALUE 9.
               88  WUBEN-IO-OK                     VALUE 0.
               88  WUBEN-IO-TS-MISMATCH            VALUE 6.
               88  WUBEN-IO-NOT-FOUND              VALUE 7.
               88  WUBEN-IO-EOF                    VALUE 8.
               88  WUBEN-IO-ERROR                  VALUE 9.
           05  WUBEN-OPTM-SQL-STAT-CD              PIC X(01).
               88  WUBEN-OPTM-SQL-OK               VALUE '0'.
               88  WUBEN-OPTM-SQL-IMPRV            VALUE '1'.
               88  WUBEN-OPTM-SQL-ERROR            VALUE '2'.
           05  WUBEN-OPTM-SQL-REQIR                PIC X(02).
           05  WUBEN-OPTM-SQL-EXEC                 PIC X(02).
           05  WUBEN-ENVRMNT-CD                    PIC X(02).
               88  WUBEN-ENVRMNT-FOREGROUND        VALUE 'FG'.
               88  WUBEN-ENVRMNT-BACKGROUND        VALUE 'BG'.
               88  WUBEN-ENVRMNT-BATCH             VALUE 'BA'.
               88  WUBEN-ENVRMNT-GUI               VALUE 'GU'.
           05  WUBEN-CHECK-UPDT-TS                 PIC X(26).
           05  WUBEN-KEY.
               10  WUBEN-APP-ID                    PIC X(15).
               10  WUBEN-STCKR-ID                  PIC X(11).
               10  WUBEN-BEN-TYP-CD                PIC X(01).
               10  WUBEN-SEQ-NUM                   PIC X(03).
               10  WUBEN-SEQ-NUM-N                 REDEFINES
                   WUBEN-SEQ-NUM                   PIC 9(03).
           05  WUBEN-ENDBR-KEY.
               10  WUBEN-ENDBR-APP-ID              PIC X(15).
               10  WUBEN-ENDBR-STCKR-ID            PIC X(11).
               10  WUBEN-ENDBR-BEN-TYP-CD          PIC X(01).
               10  WUBEN-ENDBR-SEQ-NUM             PIC X(03).
               10  WUBEN-ENDBR-SEQ-NUM-N           REDEFINES
                   WUBEN-ENDBR-SEQ-NUM             PIC 9(03).
           05  FILLER                              PIC X(20).

      *****************************************************************
      **                  END OF COPYBOOK ACFWUBEN                   **
      *****************************************************************
