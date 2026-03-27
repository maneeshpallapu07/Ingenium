      *****************************************************************
      **  MEMBER : ACFRUFLD                                          **
      **  REMARKS: UPLOAD DEFINED FIELD TABLE LAYOUT                 **
      **  LENGTH : 96                                                **
      *****************************************************************
      **  DATE     AUTH.  DESCRIPTION                                **
      **                                                             **
53-060**  30NOV95  GPB    DELETED / RENAMED FIELDS                   **
APEX54**  30NOV96  RLE    ADDED 88 LEVEL NAME                        **
557700**  30SEP97  CEC    APEX UPLOAD                                **
      *****************************************************************
 
       01  RUFLD-REC-INFO.
           05  RUFLD-KEY.
               10  RUFLD-CO-ID                  PIC X(02).
               10  RUFLD-UPLD-FLD-STRUCT-NM     PIC X(20).
                   88  RUFLD-BENEF-STRUCT       VALUE 'BENEF'.
                   88  RUFLD-DEFERRED-STRUCT    VALUE 'DEFERRED'.
                   88  RUFLD-IMMEDIATE-STRUCT   VALUE 'IMMEDIATE'.
                   88  RUFLD-INSURANCE-STRUCT   VALUE 'INSURANCE'.
                   88  RUFLD-PAC-STRUCT         VALUE 'PAC'
                                                      'DEFPAC'
                                                      'BANK'.
APEX54             88  RUFLD-TRST-STRUCT        VALUE 'TRUSTEE'.
                   88  RUFLD-UNIVLIFE-STRUCT    VALUE 'UNIVLIFE'.
53-060         10  RUFLD-UPLD-FLD-APEX-NM       PIC X(20).
           05  RUFLD-PREV-UPDT-USER-ID          PIC X(08).
           05  RUFLD-PREV-UPDT-DT               PIC X(10).
           05  RUFLD-UPLD-FLD-TYP-CD            PIC X(01).
               88  RUFLD-UPLD-FLD-TYP-DOLLAR-AMT  VALUE 'A'.
557700         88  RUFLD-UPLD-FLD-TYP-MIX-CASE    VALUE 'B'.
557700         88  RUFLD-UPLD-FLD-TYP-UPPER-CASE  VALUE 'C'.
               88  RUFLD-UPLD-FLD-TYP-DATE        VALUE 'D'.
557700         88  RUFLD-UPLD-FLD-TYP-TRANS-NAME  VALUE 'F'.
               88  RUFLD-UPLD-FLD-TYP-NUMERIC     VALUE 'N'.
               88  RUFLD-UPLD-FLD-TYP-MESSAGE     VALUE 'M'.
557700         88  RUFLD-UPLD-FLD-TYP-TRANS-VALU  VALUE 'T'.
               88  RUFLD-UPLD-FLD-TYP-UNUSED      VALUE 'U'.
               88  RUFLD-UPLD-FLD-TYP-COMPLEX     VALUE 'X'.
           05  RUFLD-UPLD-FLD-FILE-CD           PIC X(04).
               88  RUFLD-UPLD-FLD-FILE-MEDICAL    VALUE 'MEDI'.
557700         88  RUFLD-UPLD-FLD-FILE-CLI-CNTCT  VALUE 'CLIC'.
557700         88  RUFLD-UPLD-FLD-FILE-CLI-INCM   VALUE 'CLII'.
           05  RUFLD-UPLD-FLD-NM                PIC X(18).
           05  RUFLD-UPLD-TTBL-TYP-ID           PIC X(05).
53-060     05  FILLER                           PIC X(08).
 
      *****************************************************************
      **                 END OF COPYBOOK ACFRUFLD                    **
      *****************************************************************
