
      *****************************************************************
      **  MEMBER :  ACFRUBEN                                         **
      **  REMARKS:  APPLICATION UPLOAD BENEFICIARY TABLE LAYOUT      **
      *****************************************************************
      **  RELEASE   DESCRIPTION                                      **
      **                                                             **
C12392**  02NOV2011 CREATED FOR UBEN PROCESSING                      **
TVI002**  12JUL2012 TVI XML UPLOAD CHANGES                           **
M271N1**  28MAR15  CTS    XML UPLOAD CHANGES FOR FRA                 **
MP270A**  17MAR16  FIELD ADDED AS PART OF E-POS APPLICATION XML      **
UYS002**  27FEB21  FIELD ADDED AS PART OF COLI                       **
TL0291**  20OCT21  CTS    TLB041 - BENE NAME LENGTH INCREASE         **
      *****************************************************************

       01  RUBEN-REC-INFO.
           05  RUBEN-KEY.
               10  RUBEN-APP-ID                 PIC X(15).
               10  RUBEN-STCKR-ID               PIC X(11).
               10  RUBEN-BEN-TYP-CD             PIC X(01).
                   88 RUBEN-BEN-TYP-DEATH       VALUE 'D'.
                   88 RUBEN-BEN-TYP-PROXY       VALUE 'P'.
                   88 RUBEN-BEN-TYP-IP          VALUE 'I'.
TVI002             88 RUBEN-BEN-TYP-MATURITY    VALUE 'M'.
M271N1             88 RUBEN-BEN-TYP-ANUTNT      VALUE 'E'.
M271N1             88 RUBEN-BEN-TYP-SUCSD-ANUTNT
M271N1                                          VALUE 'S'.
UYS002             88 RUBEN-BENY-TYP-CANCER      VALUE 'C'.
               10  RUBEN-SEQ-NUM                PIC X(03).
               10  RUBEN-SEQ-NUM-N              REDEFINES
                   RUBEN-SEQ-NUM                PIC 9(03).
TL0291*           05  RUBEN-BEN-SUR-NM                 PIC X(25).
TL0291*           05  RUBEN-BEN-GIV-NM                 PIC X(25).
TL0291*           05  RUBEN-BNFY-CO-NM                 PIC X(50).
TL0291     05  RUBEN-BEN-SUR-NM                 PIC X(50).
TL0291     05  RUBEN-BEN-GIV-NM                 PIC X(50).
TL0291     05  RUBEN-BNFY-CO-NM                 PIC X(100).
           05  RUBEN-BEN-REL-CD                 PIC X(02).
           05  RUBEN-BEN-PCT                    PIC X(03).
           05  RUBEN-BEN-PCT-N                  REDEFINES
               RUBEN-BEN-PCT                    PIC 9(03).
           05  RUBEN-BNFY-ANTY-PERI-CD          PIC X(02).
MP270A     05  RUBEN-BNFY-MORAL-RISK-IND        PIC X(01).
MP270A         88  RUBEN-BNFY-MORAL-RISK-YES    VALUE 'Y'.
MP270A         88  RUBEN-BNFY-MORAL-RISK-NO     VALUE 'N'.
TL0291*MP270A     05  RUBEN-BNFY-KA-GIV-NM             PIC X(25).
TL0291*MP270A     05  RUBEN-BNFY-KA-SUR-NM             PIC X(25).
TL0291*MP270A     05  RUBEN-BNFY-CO-KA-NM              PIC X(50).
TL0291     05  RUBEN-BNFY-KA-GIV-NM             PIC X(50).
TL0291     05  RUBEN-BNFY-KA-SUR-NM             PIC X(50).
TL0291     05  RUBEN-BNFY-CO-KA-NM              PIC X(100).
MP270A     05  RUBEN-BNFY-SEX-CD                PIC X(01).
MP270A     05  RUBEN-BNFY-BTH-DT                PIC X(10).
UYS002     05  RUBEN-BNFY-REL-TYP-CD            PIC X(02).
UYS002         88 RUBEN-BNFY-REL-PLCYHLDR       VALUE 'PH'.
UYS002         88 RUBEN-BNFY-REL-INSURED        VALUE 'S'.
           05  FILLER                           PIC X(20).


      *****************************************************************
      **                 END OF COPYBOOK ACFRUBEN                    **
      *****************************************************************
