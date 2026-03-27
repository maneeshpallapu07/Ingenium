
      *****************************************************************
      **  MEMBER :  ACFWCLN2                                         **
      **  REMARKS:  THIS IS USED TO GIVE DETAILS FROM POLICY         **
      **           (CUSTOM SQL)                                      **
      **            WORK AREA                                        **
      *****************************************************************
      **  RELEASE   DESCRIPTION                                      **
      **                                                             **
TLB003**  20JAN19   CREATED FOR POL TABLE PROCESSING                 **
      *****************************************************************

       01  WCLN2-IO-WORK-AREA.
           05  WCLN2-TABLE-NAME                    PIC X(04)
                                                   VALUE 'POL'.
           05  WCLN2-RQST-CD                       PIC X(02).
               88  WCLN2-RQST-SELECT               VALUE '01'.
               88  WCLN2-RQST-SELECT-FOR-UPDATE    VALUE '02'.
               88  WCLN2-RQST-UPDATE               VALUE '03'.
               88  WCLN2-RQST-CLOSE-UPDATE-CUR     VALUE '04'.
               88  WCLN2-RQST-INIT-TBL-LAYOUT      VALUE '05'.
               88  WCLN2-RQST-INSERT               VALUE '06'.
               88  WCLN2-RQST-DELETE               VALUE '07'.
               88  WCLN2-RQST-BROWSE               VALUE '08'.
               88  WCLN2-RQST-FETCH-NEXT           VALUE '09'.
               88  WCLN2-RQST-CLOSE-BROWSE-CUR     VALUE '10'.
               88  WCLN2-RQST-BROWSE-PREV          VALUE '11'.
               88  WCLN2-RQST-FETCH-PREV           VALUE '12'.
               88  WCLN2-RQST-CLOSE-BR-PREV-CUR    VALUE '13'.
               88  WCLN2-RQST-DELETE-KEY-RANGE     VALUE '14'.
               88  WCLN2-RQST-DELETE-WITH-KEY      VALUE '15'.
               88  WCLN2-RQST-SELECT-MIN           VALUE '21'.
               88  WCLN2-RQST-SELECT-MAX           VALUE '22'.
               88  WCLN2-RQST-SELECT-COUNT         VALUE '23'.
               88  WCLN2-RQST-SELECT-INDEX         VALUE '24'.
               88  WCLN2-RQST-BROWSE-INDEX         VALUE '25'.
               88  WCLN2-RQST-FETCH-NEXT-INDEX     VALUE '26'.
               88  WCLN2-RQST-CLOSE-BROWSE-INDEX   VALUE '27'.
               88  WCLN2-RQST-BROWSE-PREV-INDEX    VALUE '28'.
               88  WCLN2-RQST-FETCH-PREV-INDEX     VALUE '29'.
               88  WCLN2-RQST-CLOSE-BR-PREV-INDEX  VALUE '30'.
               88  WCLN2-RQST-SELECT-FOR-UPDATE-2  VALUE '31'.
               88  WCLN2-RQST-BROWSE-UCUR          VALUE '32'.
               88  WCLN2-RQST-FETCH-NEXT-UCUR      VALUE '33'.
               88  WCLN2-RQST-UPDATE-UCUR          VALUE '34'.
               88  WCLN2-RQST-DELETE-UCUR          VALUE '35'.
               88  WCLN2-RQST-CLOSE-UCUR           VALUE '36'.
               88  WCLN2-RQST-BROWSE-IX-UCUR       VALUE '37'.
               88  WCLN2-RQST-FETCH-NEXT-IX-UCUR   VALUE '38'.
               88  WCLN2-RQST-UPDATE-IX-UCUR       VALUE '39'.
               88  WCLN2-RQST-DELETE-IX-UCUR       VALUE '40'.
               88  WCLN2-RQST-CLOSE-IX-UCUR        VALUE '41'.
               88  WCLN2-RQST-BROWSE-TABLE         VALUE '42'.
               88  WCLN2-RQST-FETCH-NEXT-TABLE     VALUE '43'.
               88  WCLN2-RQST-CLOSE-BROWSE-TABLE   VALUE '44'.
               88  WCLN2-RQST-UPDATE-LOB           VALUE '45'.
               88  WCLN2-RQST-SELECT-LOB           VALUE '46'.
               88  WCLN2-RQST-FREE-LOB             VALUE '47'.
           05  WCLN2-IO-STATUS                     PIC 9(01)
                                                   VALUE 9.
               88  WCLN2-IO-OK                     VALUE 0.
               88  WCLN2-IO-TS-MISMATCH            VALUE 6.
               88  WCLN2-IO-NOT-FOUND              VALUE 7.
               88  WCLN2-IO-EOF                    VALUE 8.
               88  WCLN2-IO-ERROR                  VALUE 9.
           05  WCLN2-OPTM-SQL-STAT-CD              PIC X(01).
               88  WCLN2-OPTM-SQL-OK               VALUE '0'.
               88  WCLN2-OPTM-SQL-IMPRV            VALUE '1'.
               88  WCLN2-OPTM-SQL-ERROR            VALUE '2'.
           05  WCLN2-OPTM-SQL-REQIR                PIC X(02).
           05  WCLN2-OPTM-SQL-EXEC                 PIC X(02).
           05  WCLN2-COMPANY-REQUIRED-SW           PIC X(01)
                                                   VALUE 'Y'.
               88  WCLN2-COMPANY-REQUIRED          VALUE 'Y'.
               88  WCLN2-COMPANY-NOT-REQUIRED      VALUE 'N'.
           05  WCLN2-ENVRMNT-CD                    PIC X(02).
               88  WCLN2-ENVRMNT-FOREGROUND        VALUE 'FG'.
               88  WCLN2-ENVRMNT-BACKGROUND        VALUE 'BG'.
               88  WCLN2-ENVRMNT-BATCH             VALUE 'BA'.
               88  WCLN2-ENVRMNT-GUI               VALUE 'GU'.
           05  WCLN2-CHECK-UPDT-TS                 PIC X(26).
           05  WCLN2-CTL-PRCES-DT                  PIC X(10).
           05  WCLN2-KEY.
               10  WCLN2-CO-ID                     PIC X(02).
               10  WCLN2-CLI-NM                    PIC X(50).    
               10  WCLN2-CLI-KJ-NM                 PIC X(50).    
           05  WCLN2-ENDBR-KEY.
               10  WCLN2-ENDBR-CO-ID               PIC X(02). 
               10  WCLN2-ENDBR-CLI-NM              PIC X(50).  
               10  WCLN2-ENDBR-CLI-KJ-NM           PIC X(50).
                         
           05  FILLER                              PIC X(20).

      *****************************************************************
      **                  END OF COPYBOOK ACFWCLN2                   **
      *****************************************************************
