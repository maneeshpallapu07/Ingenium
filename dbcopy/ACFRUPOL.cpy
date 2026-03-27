      *****************************************************************
      **  MEMBER :  ACFRUPOL                                         **
      **  REMARKS:  APPLICATION UPLOAD POLICY TABLE LAYOUT           **
      **  LENGTH :  406                                              **
      *****************************************************************
      **  RELEASE  AUTH DESCRIPTION                                  **         
      **                                                             **
      **  15AUG02       CREATED FOR UPOL TABLE PROCESSING            **         
PR006D**  26JUN03  BP   ADD PRODUCT APPLICATION TYPE                 **         
PR006Y**  26JUN03  FB   ADD 88 LEVEL TO RUPOL-POL-HLD-AGT-IND        **
PR006Q**  26JUN03  FB   ADD MY KEMPO FIELDS                          **
MFFUPL**  01OCT05  CLB  ADD FIELDS FOR MANUSTEP CHANGES              **
NWLXML**  20JUL09  CTS  ADDED FIELDS DESIGNATED POLICY ISSUE FLAG,   **
NWLXML**                BULK REMITTANCE REGISTRATION NUMBER AND      **
NWLXML**                APL REJECT INDICATOR                         **
MP142L**  30JUN11  CTS  ADDED NEW FIELD AUTOMATIC PREMIUM CHANGE     **
MP142L**                RIDER INDICATOR                              **
M161CA**  17OCT11   IMPLEMENTING THE SCREEN CHANGES FOR THE CREDIT   **
M161CA**            CARD PROCESSING                                  **
MP168A**  14NOV11  CTS    IMPLEMENTING THE CHANGES FOR THE PA CHANNEL**
MP168A**                  CASHLESS  PROCESSING                       **
MP176B**  07JUN12  CTS    ADDED A NEW FIELD - BULK APPLICATION ID    **
C20740**  31MAY13  CT     PS-SD115799 ADD NEW TYPE RESTRICTED AGENT  **
C20740**                  TO CONTRACT RELATIONSHIP TYPE.             **
MP261B** 29OCT14  CTS     ARM2-NON FACE TO FACE XML UPLOAD CHANGES   **
M271N1**  03APR15  CTS    FRA XML UPLOAD CHANGES                     **
R11005**  12SEP15  CTS    ADDED SINGLE PREM AMT & PAYMENT CRCY CODE  **
MP270A**  17MAR16  FIELD ADDED AD PART OF E-POS APPLICATION XML      **
M319N1**  03FEB17  CTS  FIELD ADDED AS PART OF FXWL XML CHANGES      **
M328C8**  12DEC17  CTS   CHANGES FOR FI TABLET NEW BUSINESS          **
018396**  26MAY19  CTS    CHANGES DONE FOR EPOS DAY2.0               **
18396A**  02DEC19  CTS    CHANGES FOR EPOS PHASE 2                   **
FF2003**  29MAY20  CTS    CHANGES FOR FFF FOR SMBC                   **
NV3N01** 26AUG22   CTS    SULV3 NEW BUSINESS BATCH AND ONLINE        **
27624C**  09FEB24  CTS   NEW FIELD FOR POL ECERTIFICATE              **
29746F**  09OCT24  CTS   NEW FIELDS ADDED FOR CERBERUS PRODUCT       **
NVCP1A**  02APR24  CTS   ADDITION OF NEW PROD-APP-TYP-CD FOR ONTARIO **
      *****************************************************************

       01  RUPOL-REC-INFO.
           05  RUPOL-KEY.
               10  RUPOL-APP-ID                 PIC X(15).
           05  RUPOL-POL-ID                     PIC X(10).
           05  RUPOL-POL-ID-R                   REDEFINES
               RUPOL-POL-ID.
               10  RUPOL-POL-ID-BASE            PIC X(09).
               10  RUPOL-POL-ID-SFX             PIC X(01).
           05  RUPOL-PLAN-ID                    PIC X(15).
           05  RUPOL-BASE-CVG-CD                PIC X(10).
           05  RUPOL-PYMT-AMT                   PIC S9(13)V9(02) COMP-3.
           05  RUPOL-PYMT-MODE-CD               PIC X(15).
           05  RUPOL-PYMT-TYP-CD                PIC X(15).
           05  RUPOL-BNK-ACCT-ID                PIC X(17).
           05  RUPOL-BNK-ACCT-HLDR-NM           PIC X(50).
           05  RUPOL-NAYOSE-POL-CD              PIC X(02).
           05  RUPOL-ACCT-HLD-TYP-CD            PIC X(02).
           05  RUPOL-BNK-ACCT-TYP-CD            PIC X(02).
           05  RUPOL-EMAIL-ADDR-CD              PIC X(50).
           05  RUPOL-APP-UPLD-DT                PIC X(10).
           05  RUPOL-ILLUS-CALC-DT              PIC X(10).
           05  RUPOL-APP-RPT-DT                 PIC X(10).
           05  RUPOL-LUMP-SUM-AMT               PIC S9(13)V9(02) COMP-3.
           05  RUPOL-CWA-RECPT-DT               PIC X(10).
           05  RUPOL-CWA-RECPT-NUM              PIC X(08).
           05  RUPOL-CWA-RECPT-NUM-N            REDEFINES
               RUPOL-CWA-RECPT-NUM              PIC 9(08).
           05  RUPOL-POL-ASGN-IND               PIC X(01).
               88  RUPOL-POL-ASGN               VALUE 'Y'.
               88  RUPOL-POL-ASGN-NO            VALUE 'N'.
           05  RUPOL-APP-CORCT-IND              PIC X(01).
               88  RUPOL-APP-CORCT              VALUE 'Y'.
               88  RUPOL-APP-CORCT-NO           VALUE 'N'.
           05  RUPOL-HO-CNSLT-IND               PIC X(01).
               88  RUPOL-HO-CNSLT               VALUE 'Y'.
               88  RUPOL-HO-CNSLT-NO            VALUE 'N'.
           05  RUPOL-PRE-ISS-CNFRM-IND          PIC X(01).
               88  RUPOL-PRE-ISS-CNFRM          VALUE 'Y'.
               88  RUPOL-PRE-ISS-CNFRM-NO       VALUE 'N'.
           05  RUPOL-POL-HLD-AGT-IND            PIC X(01).
PR006Y*        88  RUPOL-POL-HLD-AGT            VALUE 'Y'.
PR006Y*        88  RUPOL-POL-HLD-AGT-NO         VALUE 'N'.
PR006Y         88  RUPOL-POL-HLD-AGT-SELF       VALUE 'Y'.
PR006Y         88  RUPOL-POL-HLD-AGT-INTRNL     VALUE 'B'.
PR006Y         88  RUPOL-POL-HLD-AGT-MLJ        VALUE 'A'.
PR006Y         88  RUPOL-POL-HLD-AGT-NONE       VALUE 'N'.
C20740         88  RUPOL-POL-HLD-AGT-RESTR      VALUE 'R'.
           05  RUPOL-MOD-REG-PREM-AMT           PIC S9(13)V9(02) COMP-3.
           05  RUPOL-BNK-ID                     PIC X(04).
           05  RUPOL-BNK-BR-ID                  PIC X(05).
           05  RUPOL-BR-NUM                     PIC X(03).
           05  RUPOL-BR-NUM-N                   REDEFINES
               RUPOL-BR-NUM                     PIC 9(03).
           05  RUPOL-SO-NUM                     PIC X(03).
           05  RUPOL-SO-NUM-N                   REDEFINES
               RUPOL-SO-NUM                     PIC 9(03).
           05  RUPOL-SALES-REP-NUM              PIC X(06).
           05  RUPOL-SALES-REP-NUM-N            REDEFINES
               RUPOL-SALES-REP-NUM              PIC 9(06).
           05  RUPOL-CO-SALES-REP-NUM           PIC X(06).
           05  RUPOL-CO-SALES-REP-NUM-N         REDEFINES
               RUPOL-CO-SALES-REP-NUM           PIC 9(06).
           05  RUPOL-REFRL-CD                   PIC X(06).
           05  RUPOL-LBILL-GR-CD                PIC X(30).
           05  RUPOL-LBILL-CO-ID                PIC X(10).
           05  RUPOL-LBILL-GR-CLI-ID            PIC X(10).
           05  RUPOL-LBILL-GR-EMP-ID            PIC X(10).
           05  RUPOL-BT-BILL-DLAY-IND           PIC X(01).
               88  RUPOL-BT-BILL-DLAY           VALUE 'Y'.
               88  RUPOL-BT-BILL-DLAY-NO        VALUE 'N'.
           05  RUPOL-POL-CNVR-KEY-NUM           PIC X(10).
           05  RUPOL-POL-CNVR-KEY-NUM-N         REDEFINES
               RUPOL-POL-CNVR-KEY-NUM           PIC 9(10).
           05  RUPOL-POL-CNVR-TYP-CD            PIC X(02).
PR006D     05  RUPOL-PROD-APP-TYP-CD            PIC X(02).                      
PR006Q     05  RUPOL-MY-KEMPO-TYP-CD            PIC X(01).
PR006Q     05  RUPOL-SUB-CAT-CD                 PIC X(03).
PR006Q     05  RUPOL-ORIG-POL-ID                PIC X(07).
PR006Q     05  RUPOL-ORIG-POL-ISS-DT            PIC X(10).
PR006Q     05  RUPOL-ORIG-POL-DUR               PIC X(02).
MFFUPL     05  RUPOL-GA-INIT-PREM-AMT           PIC S9(13)V9(02) COMP-3.
MFFUPL     05  RUPOL-GA-SUBSEQ-PREM-AMT         PIC S9(13)V9(02) COMP-3.
MFFUPL     05  RUPOL-GA-INIT-LMPSM-AMT          PIC S9(13)V9(02) COMP-3.
MFFUPL     05  RUPOL-GA-CNVR-FND-AMT            PIC S9(13)V9(02) COMP-3.
MFFUPL     05  RUPOL-AGT-XPER-CD                PIC X(01).
MFFUPL     05  RUPOL-VERIF-MTHD-CD              PIC X(02).
NWLXML     05  RUPOL-APL-REJ-IND                PIC X(01).
NWLXML         88  RUPOL-APL-REJ-YES            VALUE 'Y'.
NWLXML         88  RUPOL-APL-REJ-NO             VALUE 'N'.
NWLXML     05  RUPOL-DESGNT-ISS-DT-IND          PIC X(01).
NWLXML         88  RUPOL-DESGNT-ISS-DT-YES      VALUE 'Y'.
NWLXML         88  RUPOL-DESGNT-ISS-DT-NO       VALUE 'N'.
NWLXML     05  RUPOL-BULK-REMIT-NUM             PIC X(10).
MP142L     05  RUPOL-AUTO-PREM-CHNG-IND         PIC X(01).
MP142L         88  RUPOL-AUTO-PREM-CHNG-YES     VALUE 'Y'.
MP142L         88  RUPOL-AUTO-PREM-CHNG-NO      VALUE 'N'.
MP168A*M161CA     05  RUPOL-INIT-PMT-TYP-CD            PIC X(01).
MP168A     05  RUPOL-PLAN-INIT-PMT-TYP-CD       PIC X(15).
MP176B     05  RUPOL-POL-BULK-APP-ID            PIC X(15).
MP261B     05  RUPOL-SPCL-AGT-SIGN-DT           PIC X(10).
MP261B     05  RUPOL-NON-FACE-TO-FACE-IND       PIC X(01).
MP261B         88  RUPOL-NON-FACE-TO-FACE-YES   VALUE 'Y'.
MP261B         88  RUPOL-NON-FACE-TO-FACE-NO    VALUE 'N'.
MP261B     05  RUPOL-WIRE-XFER-VIRTUAL-NUM      PIC X(07).
MP261B     05  RUPOL-WIRE-XFER-VIRTUAL-NUM-N    REDEFINES
MP261B         RUPOL-WIRE-XFER-VIRTUAL-NUM      PIC 9(07).
M271N1     05  RUPOL-SCHD-ADV-PMT-DUR           PIC X(03).
M271N1         88  RUPOL-SCHD-ADV-PMT-SEMI-ANN  VALUE '006'.
M271N1         88  RUPOL-SCHD-ADV-PMT-ANN       VALUE '012'.
M271N1         88  RUPOL-SCHD-ADV-PMT-NONE      VALUE SPACES.
R11005     05  RUPOL-SNGL-PREM-AMT             PIC S9(13)V9(02) COMP-3.
R11005     05  RUPOL-PMT-CRCY-CD                PIC X(02).
MP270A     05  RUPOL-ADDR-CNFRM-IND             PIC X(01).
MP270A         88  RUPOL-ADDR-CNFRM-YES         VALUE 'Y'.
MP270A         88  RUPOL-ADDR-CNFRM-NO          VALUE 'N'.
MP270A     05  RUPOL-OVRSEAS-TRAV-IND           PIC X(01).
MP270A         88  RUPOL-OVRSEAS-TRAV-YES       VALUE 'Y'.
MP270A         88  RUPOL-OVRSEAS-TRAV-NO        VALUE 'N'.
MP270A     05  RUPOL-FRGN-OWN-IND               PIC X(01).
MP270A         88  RUPOL-FRGN-OWN-YES           VALUE 'Y'.
MP270A         88  RUPOL-FRGN-OWN-NO            VALUE 'N'.
MP270A     05  RUPOL-WORK-INS-CO-IND            PIC X(01).
MP270A         88  RUPOL-WORK-INS-CO-YES        VALUE 'Y'.
MP270A         88  RUPOL-WORK-INS-CO-NO         VALUE 'N'.
MP270A     05  RUPOL-THRD-PARTY-BNFY-IND        PIC X(01).
MP270A         88  RUPOL-THRD-PARTY-BNFY-YES    VALUE 'Y'.
MP270A         88  RUPOL-THRD-PARTY-BNFY-NO     VALUE 'N'.
MP270A     05  RUPOL-FRGN-CLI-ATCH-IND          PIC X(01).
MP270A         88  RUPOL-FRGN-CLI-ATCH-YES      VALUE 'Y'.
MP270A         88  RUPOL-FRGN-CLI-ATCH-NO       VALUE 'N'.
MP270A     05  RUPOL-DOCS-TO-BE-SENT-IND        PIC X(01).
MP270A         88  RUPOL-DOCS-TO-BE-SENT-YES    VALUE 'Y'.
MP270A         88  RUPOL-DOCS-TO-BE-SENT-NO     VALUE 'N'.
MP270A     05  RUPOL-SPCL-NOTES-IND             PIC X(01).
MP270A         88  RUPOL-SPCL-NOTES-YES         VALUE 'Y'.
MP270A         88  RUPOL-SPCL-NOTES-NO          VALUE 'N'.
MP270A     05  RUPOL-PRELIM-UW-IND              PIC X(01).
MP270A         88  RUPOL-PRELIM-UW-YES          VALUE 'Y'.
MP270A         88  RUPOL-PRELIM-UW-NO           VALUE 'N'.
MP270A     05  RUPOL-VOLNTR-APPL-IND            PIC X(01).
MP270A         88  RUPOL-VOLNTR-APPL-YES        VALUE 'Y'.
MP270A         88  RUPOL-VOLNTR-APPL-NO         VALUE 'N'.
MP270A     05  RUPOL-MNGR-SPCL-NOTES-IND        PIC X(01).
MP270A         88  RUPOL-MNGR-SPCL-NOTES-YES    VALUE 'Y'.
MP270A         88  RUPOL-MNGR-SPCL-NOTES-NO     VALUE 'N'.
MP270A     05  RUPOL-PHYS-SPCL-NOTES-IND        PIC X(01).
MP270A         88  RUPOL-PHYS-SPCL-NOTES-YES    VALUE 'Y'.
MP270A         88  RUPOL-PHYS-SPCL-NOTES-NO     VALUE 'N'.
MP270A     05  RUPOL-DONATE-FORM-IND            PIC X(01).
MP270A         88  RUPOL-DONATE-FORM-YES        VALUE 'Y'.
MP270A         88  RUPOL-DONATE-FORM-NO         VALUE 'N'.
MP270A     05  RUPOL-MULT-APPL-IND              PIC X(01).
MP270A         88  RUPOL-MULT-APPL-YES          VALUE 'Y'.
MP270A         88  RUPOL-MULT-APPL-NO           VALUE 'N'.
MP270A     05  RUPOL-PAPR-LESS-APP-IND          PIC X(01).
MP270A         88  RUPOL-PAPR-LESS-APP-YES      VALUE 'Y'.
MP270A         88  RUPOL-PAPR-LESS-APP-NO       VALUE 'N'.
MP270A     05  RUPOL-APPROV-NUM-IND             PIC X(01).
MP270A         88  RUPOL-APPROV-NUM-YES         VALUE 'Y'.
MP270A         88  RUPOL-APPROV-NUM-NO          VALUE 'N'.
M319N1     05  RUPOL-YBA-RPT-REQIR-IND          PIC X(01).
M319N1         88  RUPOL-YBA-RPT-REQIR-YES      VALUE 'Y'.
M319N1         88  RUPOL-YBA-RPT-REQIR-NO       VALUE 'N'.
M328C8     05  RUPOL-SPEC-ASSOC-CORP-IND        PIC X(01).
M328C8         88  RUPOL-SPEC-ASSOC-CORP-YES    VALUE 'Y'.
M328C8         88  RUPOL-SPEC-ASSOC-CORP-NO     VALUE 'N'.
018396     05  RUPOL-SAL-TST-RSLT-CD            PIC X(01).
018396         88 RUPOL-SAL-TST-RSLT-POS        VALUE '1'.
018396         88 RUPOL-SAL-TST-RSLT-NEG        VALUE '2'.
018396         88 RUPOL-SAL-TST-RSLT-INVALID    VALUE '0'.
018396     05  RUPOL-SPCL-NOTE-INTNT-IND        PIC X(01).
018396         88  RUPOL-SPCL-NOTE-INTNT-YES    VALUE 'Y'.
018396         88  RUPOL-SPCL-NOTE-INTNT-NO     VALUE 'N'.
018396     05  RUPOL-ANTY-DONAT-FORM-IND        PIC X(01).
018396         88  RUPOL-ANTY-DONAT-FORM-YES    VALUE 'Y'.
018396         88  RUPOL-ANTY-DONAT-FORM-NO     VALUE 'N'.    
18396A     05  RUPOL-CNFRM-CALL-IND             PIC X(01).
18396A         88  RUPOL-CNFRM-CALL-YES         VALUE 'Y'.
18396A         88  RUPOL-CNFRM-CALL-NO          VALUE 'N'.
FF2003     05  RUPOL-POL-PRVD-TYP-CD            PIC X(01).
FF2003         88  RUPOL-POL-PRVD-TYP-BLANK     VALUE SPACE.
FF2003         88  RUPOL-POL-PRVD-TYP-YES       VALUE '1'.
FF2003         88  RUPOL-POL-PRVD-TYP-NO        VALUE '2'.
NVCP1A*NV3N01     05  RUPOL-MTHLY-STD-PREM-AMT         PIC S9(13)V9(02) 
NVCP1A*NV3N01                                           COMP-3.
NVCP1A     05  RUPOL-MTHLY-STD-PREM-AMT         PIC S9(11)V9(02) 
NVCP1A                                           COMP-3.
27624C     05  RUPOL-POL-ECERT-IND              PIC X(01).
27624C         88  RUPOL-POL-ECERT-YES          VALUE 'Y'.
27624C         88  RUPOL-POL-ECERT-NO           VALUE 'N'.
27624C     05  RUPOL-TRXN-CNFRM-OS-IND          PIC X(01).
27624C         88  RUPOL-TRXN-CNFRM-OS-YES      VALUE 'Y'.
27624C         88  RUPOL-TRXN-CNFRM-OS-NO       VALUE 'N'.
29746F     05  RUPOL-CPN-AUTO-PAYO-IND          PIC X(01).
29746F         88  RUPOL-CPN-AUTO-PAYO-YES      VALUE 'Y'.
29746F         88  RUPOL-CPN-AUTO-PAYO-NO       VALUE 'N'.
29746F     05  RUPOL-JPY-PAYO-IND               PIC X(01).
29746F         88  RUPOL-JPY-PAYO-YES           VALUE 'Y'.
29746F         88  RUPOL-JPY-PAYO-NO            VALUE 'N'.
29746F     05  RUPOL-PAYO-BNK-ID                PIC X(04).
29746F     05  RUPOL-PAYO-BNK-BR-ID             PIC X(05).
29746F     05  RUPOL-PAYO-BNK-ACCT-ID           PIC X(17).
29746F     05  RUPOL-PAYO-BNK-ACCT-TYP-CD       PIC X(03).
29746F     05  RUPOL-PAYO-BNK-ACC-HLDR-NM       PIC X(50).
           05  FILLER                           PIC X(20).

      *****************************************************************
      **                 END OF COPYBOOK ACFRUPOL                    **
      *****************************************************************
