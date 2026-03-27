      *****************************************************************
      **  MEMBER :  ACFWUPOA                                         **
      **  REMARKS:  APPLICATION UPLOAD POLICY TABLE (ALT. ACCESS)    **
      **            WORK AREA                                        **
      *****************************************************************
      **  RELEASE   DESCRIPTION                                      **
      **                                                             **
      **  612J      CREATED FOR UPOL TABLE PROCESSING                **
      *****************************************************************

       01  WUPOA-IO-WORK-AREA.
           05  WUPOA-TABLE-NAME                    PIC X(04)
                                                   VALUE 'UPOA'.
           05  WUPOA-RQST-CD                       PIC X(02).
               88  WUPOA-RQST-SELECT               VALUE '01'.
               88  WUPOA-RQST-SELECT-FOR-UPDATE    VALUE '02'.
               88  WUPOA-RQST-UPDATE               VALUE '03'.
               88  WUPOA-RQST-CLOSE-UPDATE-CUR     VALUE '04'.
               88  WUPOA-RQST-INIT-TBL-LAYOUT      VALUE '05'.
               88  WUPOA-RQST-INSERT               VALUE '06'.
               88  WUPOA-RQST-DELETE               VALUE '07'.
               88  WUPOA-RQST-BROWSE               VALUE '08'.
               88  WUPOA-RQST-FETCH-NEXT           VALUE '09'.
               88  WUPOA-RQST-CLOSE-BROWSE-CUR     VALUE '10'.
               88  WUPOA-RQST-BROWSE-PREV          VALUE '11'.
               88  WUPOA-RQST-FETCH-PREV           VALUE '12'.
               88  WUPOA-RQST-CLOSE-BR-PREV-CUR    VALUE '13'.
               88  WUPOA-RQST-DELETE-KEY-RANGE     VALUE '14'.
               88  WUPOA-RQST-DELETE-WITH-KEY      VALUE '15'.
               88  WUPOA-RQST-SELECT-MIN           VALUE '21'.
               88  WUPOA-RQST-SELECT-MAX           VALUE '22'.
               88  WUPOA-RQST-SELECT-INDEX         VALUE '23'.
               88  WUPOA-RQST-BROWSE-INDEX         VALUE '24'.
               88  WUPOA-RQST-FETCH-NEXT-INDEX     VALUE '25'.
               88  WUPOA-RQST-CLOSE-BROWSE-INDEX   VALUE '26'.
               88  WUPOA-RQST-BROWSE-PREV-INDEX    VALUE '27'.
               88  WUPOA-RQST-FETCH-PREV-INDEX     VALUE '28'.
               88  WUPOA-RQST-CLOSE-BR-PREV-INDEX  VALUE '29'.
               88  WUPOA-RQST-SELECT-FOR-UPDATE-2  VALUE '30'.
               88  WUPOA-RQST-BROWSE-UCUR          VALUE '31'.
               88  WUPOA-RQST-FETCH-NEXT-UCUR      VALUE '32'.
               88  WUPOA-RQST-UPDATE-UCUR          VALUE '33'.
               88  WUPOA-RQST-DELETE-UCUR          VALUE '34'.
               88  WUPOA-RQST-CLOSE-UCUR           VALUE '35'.
               88  WUPOA-RQST-BROWSE-IX-UCUR       VALUE '36'.
               88  WUPOA-RQST-FETCH-NEXT-IX-UCUR   VALUE '37'.
               88  WUPOA-RQST-UPDATE-IX-UCUR       VALUE '38'.
               88  WUPOA-RQST-DELETE-IX-UCUR       VALUE '39'.
               88  WUPOA-RQST-CLOSE-IX-UCUR        VALUE '40'.
           05  WUPOA-IO-STATUS                     PIC 9(01)
                                                   VALUE 9.
               88  WUPOA-IO-OK                     VALUE 0.
               88  WUPOA-IO-TS-MISMATCH            VALUE 6.
               88  WUPOA-IO-NOT-FOUND              VALUE 7.
               88  WUPOA-IO-EOF                    VALUE 8.
               88  WUPOA-IO-ERROR                  VALUE 9.
           05  WUPOA-OPTM-SQL-STAT-CD              PIC X(01).
               88  WUPOA-OPTM-SQL-OK               VALUE '0'.
               88  WUPOA-OPTM-SQL-IMPRV            VALUE '1'.
               88  WUPOA-OPTM-SQL-ERROR            VALUE '2'.
           05  WUPOA-OPTM-SQL-REQIR                PIC X(02).
           05  WUPOA-OPTM-SQL-EXEC                 PIC X(02).
           05  WUPOA-ENVRMNT-CD                    PIC X(02).
               88  WUPOA-ENVRMNT-FOREGROUND        VALUE 'FG'.
               88  WUPOA-ENVRMNT-BACKGROUND        VALUE 'BG'.
               88  WUPOA-ENVRMNT-BATCH             VALUE 'BA'.
               88  WUPOA-ENVRMNT-GUI               VALUE 'GU'.
           05  WUPOA-CHECK-UPDT-TS                 PIC X(26).
           05  WUPOA-KEY.
               10  WUPOA-APP-UPLD-DT               PIC X(10).
           05  WUPOA-ENDBR-KEY.
               10  WUPOA-ENDBR-APP-UPLD-DT         PIC X(10).
           05  FILLER                              PIC X(20).

      *****************************************************************
      **                  END OF COPYBOOK ACFWUPOA                   **
      *****************************************************************
