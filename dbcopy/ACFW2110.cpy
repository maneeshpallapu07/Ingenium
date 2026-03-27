      *****************************************************************
      **  MEMBER :  ACFW2110                                         **
      **  REMARKS:  APEX TO NBS IMPORT AUDIT EXTRACT TABLE WORK AREA **
      *****************************************************************
      **  RELEASE   DESCRIPTION                                      **
      **                                                             **
      **  612J      CREATED FOR 2110 TABLE PROCESSING                **
      *****************************************************************

       01  W2110-IO-WORK-AREA.
           05  W2110-TABLE-NAME                    PIC X(04)
                                                   VALUE '2110'.
           05  W2110-RQST-CD                       PIC X(02).
               88  W2110-RQST-SELECT               VALUE '01'.
               88  W2110-RQST-SELECT-FOR-UPDATE    VALUE '02'.
               88  W2110-RQST-UPDATE               VALUE '03'.
               88  W2110-RQST-CLOSE-UPDATE-CUR     VALUE '04'.
               88  W2110-RQST-INIT-TBL-LAYOUT      VALUE '05'.
               88  W2110-RQST-INSERT               VALUE '06'.
               88  W2110-RQST-DELETE               VALUE '07'.
               88  W2110-RQST-BROWSE               VALUE '08'.
               88  W2110-RQST-FETCH-NEXT           VALUE '09'.
               88  W2110-RQST-CLOSE-BROWSE-CUR     VALUE '10'.
               88  W2110-RQST-BROWSE-PREV          VALUE '11'.
               88  W2110-RQST-FETCH-PREV           VALUE '12'.
               88  W2110-RQST-CLOSE-BR-PREV-CUR    VALUE '13'.
               88  W2110-RQST-DELETE-KEY-RANGE     VALUE '14'.
               88  W2110-RQST-DELETE-WITH-KEY      VALUE '15'.
               88  W2110-RQST-SELECT-MIN           VALUE '21'.
               88  W2110-RQST-SELECT-MAX           VALUE '22'.
               88  W2110-RQST-SELECT-INDEX         VALUE '23'.
               88  W2110-RQST-BROWSE-INDEX         VALUE '24'.
               88  W2110-RQST-FETCH-NEXT-INDEX     VALUE '25'.
               88  W2110-RQST-CLOSE-BROWSE-INDEX   VALUE '26'.
               88  W2110-RQST-BROWSE-PREV-INDEX    VALUE '27'.
               88  W2110-RQST-FETCH-PREV-INDEX     VALUE '28'.
               88  W2110-RQST-CLOSE-BR-PREV-INDEX  VALUE '29'.
               88  W2110-RQST-SELECT-FOR-UPDATE-2  VALUE '30'.
               88  W2110-RQST-BROWSE-UCUR          VALUE '31'.
               88  W2110-RQST-FETCH-NEXT-UCUR      VALUE '32'.
               88  W2110-RQST-UPDATE-UCUR          VALUE '33'.
               88  W2110-RQST-DELETE-UCUR          VALUE '34'.
               88  W2110-RQST-CLOSE-UCUR           VALUE '35'.
               88  W2110-RQST-BROWSE-IX-UCUR       VALUE '36'.
               88  W2110-RQST-FETCH-NEXT-IX-UCUR   VALUE '37'.
               88  W2110-RQST-UPDATE-IX-UCUR       VALUE '38'.
               88  W2110-RQST-DELETE-IX-UCUR       VALUE '39'.
               88  W2110-RQST-CLOSE-IX-UCUR        VALUE '40'.
           05  W2110-IO-STATUS                     PIC 9(01)
                                                   VALUE 9.
               88  W2110-IO-OK                     VALUE 0.
               88  W2110-IO-TS-MISMATCH            VALUE 6.
               88  W2110-IO-NOT-FOUND              VALUE 7.
               88  W2110-IO-EOF                    VALUE 8.
               88  W2110-IO-ERROR                  VALUE 9.
           05  W2110-OPTM-SQL-STAT-CD              PIC X(01).
               88  W2110-OPTM-SQL-OK               VALUE '0'.
               88  W2110-OPTM-SQL-IMPRV            VALUE '1'.
               88  W2110-OPTM-SQL-ERROR            VALUE '2'.
           05  W2110-OPTM-SQL-REQIR                PIC X(02).
           05  W2110-OPTM-SQL-EXEC                 PIC X(02).
           05  W2110-COMPANY-REQUIRED-SW           PIC X(01)
                                                   VALUE 'Y'.
               88  W2110-COMPANY-REQUIRED          VALUE 'Y'.
               88  W2110-COMPANY-NOT-REQUIRED      VALUE 'N'.
           05  W2110-ENVRMNT-CD                    PIC X(02).
               88  W2110-ENVRMNT-FOREGROUND        VALUE 'FG'.
               88  W2110-ENVRMNT-BACKGROUND        VALUE 'BG'.
               88  W2110-ENVRMNT-BATCH             VALUE 'BA'.
               88  W2110-ENVRMNT-GUI               VALUE 'GU'.
           05  W2110-CHECK-UPDT-TS                 PIC X(26).
           05  W2110-KEY.
               10  W2110-CO-ID                     PIC X(02).
               10  W2110-SEQ-FILE-PGM-ID           PIC X(08).
               10  W2110-SEQ-FILE-OUTPT-NM         PIC X(08).
               10  W2110-SEQ-FILE-INSTC-ID         PIC S9(04) BINARY.
               10  W2110-SEQ-FILE-TS               PIC X(26).
           05  W2110-ENDBR-KEY.
               10  W2110-ENDBR-CO-ID               PIC X(02).
               10  W2110-ENDBR-SEQ-FILE-PGM-ID     PIC X(08).
               10  W2110-ENDBR-SEQ-FILE-OUTPT-NM   PIC X(08).
               10  W2110-ENDBR-SEQ-FILE-INSTC-ID   PIC S9(04) BINARY.
               10  W2110-ENDBR-SEQ-FILE-TS         PIC X(26).
           05  FILLER                              PIC X(20).

      *****************************************************************
      **                  END OF COPYBOOK ACFW2110                   **
      *****************************************************************
