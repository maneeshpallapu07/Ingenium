      *****************************************************************
      **  MEMBER : ACFWUFLD                                          **
      **  REMARKS: UPLOAD DEFINED FIELD TABLE WORK AREA              **
      *****************************************************************
      **  DATE     AUTH.  DESCRIPTION                                **
      **                                                             **
      **  30OCT98  56     CREATED FOR UFLD TABLE PROCESSING          **
      *****************************************************************
 
       01  WUFLD-IO-WORK-AREA.
           05  WUFLD-TABLE-NAME                   PIC X(04)
                                                  VALUE 'UFLD'.
           05  WUFLD-RQST-CD                      PIC X(02).
               88  WUFLD-RQST-SELECT              VALUE '01'.
               88  WUFLD-RQST-SELECT-FOR-UPDATE   VALUE '02'.
               88  WUFLD-RQST-UPDATE              VALUE '03'.
               88  WUFLD-RQST-CLOSE-UPDATE-CUR    VALUE '04'.
               88  WUFLD-RQST-INIT-TBL-LAYOUT     VALUE '05'.
               88  WUFLD-RQST-INSERT              VALUE '06'.
               88  WUFLD-RQST-DELETE              VALUE '07'.
               88  WUFLD-RQST-BROWSE              VALUE '08'.
               88  WUFLD-RQST-FETCH-NEXT          VALUE '09'.
               88  WUFLD-RQST-CLOSE-BROWSE-CUR    VALUE '10'.
               88  WUFLD-RQST-BROWSE-PREV         VALUE '11'.
               88  WUFLD-RQST-FETCH-PREV          VALUE '12'.
               88  WUFLD-RQST-CLOSE-BR-PREV-CUR   VALUE '13'.
               88  WUFLD-RQST-DELETE-KEY-RANGE    VALUE '14'.
               88  WUFLD-RQST-DELETE-WITH-KEY     VALUE '15'.
               88  WUFLD-RQST-SELECT-MIN          VALUE '21'.
               88  WUFLD-RQST-SELECT-MAX          VALUE '22'.
               88  WUFLD-RQST-SELECT-INDEX        VALUE '23'.
               88  WUFLD-RQST-BROWSE-INDEX        VALUE '24'.
               88  WUFLD-RQST-FETCH-NEXT-INDEX    VALUE '25'.
               88  WUFLD-RQST-CLOSE-BROWSE-INDEX  VALUE '26'.
               88  WUFLD-RQST-BROWSE-PREV-INDEX   VALUE '27'.
               88  WUFLD-RQST-FETCH-PREV-INDEX    VALUE '28'.
               88  WUFLD-RQST-CLOSE-BR-PREV-INDEX VALUE '29'.
           05  WUFLD-IO-STATUS                    PIC 9(01)
                                                  VALUE 9.
               88  WUFLD-IO-OK                    VALUE 0.
               88  WUFLD-IO-NOT-FOUND             VALUE 7.
               88  WUFLD-IO-EOF                   VALUE 8.
               88  WUFLD-IO-ERROR                 VALUE 9.
           05  WUFLD-OPTM-SQL-STAT-CD             PIC X(01).
               88  WUFLD-OPTM-SQL-OK              VALUE '0'.
               88  WUFLD-OPTM-SQL-IMPRV           VALUE '1'.
               88  WUFLD-OPTM-SQL-ERROR           VALUE '2'.
           05  WUFLD-OPTM-SQL-REQIR               PIC X(02).
           05  WUFLD-OPTM-SQL-EXEC                PIC X(02).
           05  WUFLD-COMPANY-REQUIRED-SW          PIC X(01)
                                                  VALUE 'Y'.
               88  WUFLD-COMPANY-REQUIRED         VALUE 'Y'.
               88  WUFLD-COMPANY-NOT-REQUIRED     VALUE 'N'.
           05  WUFLD-ENVRMNT-CD                   PIC X(02).
               88  WUFLD-ENVRMNT-FOREGROUND       VALUE 'FG'.
               88  WUFLD-ENVRMNT-BACKGROUND       VALUE 'BG'.
               88  WUFLD-ENVRMNT-BATCH            VALUE 'BA'.
               88  WUFLD-ENVRMNT-GUI              VALUE 'GU'.
           05  WUFLD-KEY.
               10  WUFLD-CO-ID                    PIC X(02).
               10  WUFLD-UPLD-FLD-STRUCT-NM       PIC X(20).
               10  WUFLD-UPLD-FLD-APEX-NM         PIC X(20).
           05  WUFLD-ENDBR-KEY.
               10  WUFLD-ENDBR-CO-ID              PIC X(02).
               10  WUFLD-ENDBR-UPLD-FLD-STRUCT-NM PIC X(20).
               10  WUFLD-ENDBR-UPLD-FLD-APEX-NM   PIC X(20).
           05  FILLER                             PIC X(20).
 
      *****************************************************************
      **                  END OF COPYBOOK ACFWUFLD                   **
      *****************************************************************
