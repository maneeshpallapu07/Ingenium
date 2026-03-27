      *****************************************************************
      **  MEMBER :  ACFRUCVG                                         **
      **  REMARKS:  APPLICATION UPLOAD COVERAGE TABLE LAYOUT         **
      **  LENGTH :  91                                               **
      *****************************************************************
      **  RELEASE   DESCRIPTION                                      **
      **                                                             **
      **            CREATED FOR UCVG TABLE PROCESSING                **
PR006C**  26JUN03   ADD  RUCVG-CVG-STBL-2-CD (BENEFIT DAYS)          **
MFPUPL**  20JUL04   SUG  ADD RUCVG-CVG-ILLUS-CD                      **
MFFUPL**  01OCT05   CLB  ADD FIELDS FOR MANUSTEP CHANGES             **
RP2005**  12JUN07   CTS  ADD NEW FIELD RUCVG-CVG-STBL-4-CD           **
RPL005**  24DEC07   CTS  ADD NEW FIELD RUCVG-CVG-STBL-3-CD           **
MP161D**  28SEP11   CTS  ADD NEW FIELD RUCVG-CVG-WP-IND              **
MP213P**  02JAN13   CTS    ADDED A NEW FIELD PRIMARY GROUP ID         ** 
M271N1**  03APR15  CTS    FRA XML UPLOAD CHANGES                     **
TV2003**  20DEC18   CTS    CHANGES FOR SULV2 NEW BUSINESS            **
UYS002**  27FEB21  FIELD ADDED AS PART OF COLI                       **
TLB002**  21JUN21   CTS  CHANGES DONE FOR TLB PRODUCTS               **
NVCN01**  21APR25  CTS   CHANGES DONE AS PART OF ONTARIO XML UPLOAD  **
      *****************************************************************

       01  RUCVG-REC-INFO.
           05  RUCVG-KEY.
               10  RUCVG-APP-ID                 PIC X(15).
               10  RUCVG-STCKR-ID               PIC X(11).
               10  RUCVG-PLAN-ID                PIC X(15).
           05  RUCVG-INIT-COV-AMT               PIC S9(13)V9(02) COMP-3.
           05  RUCVG-SMKR-CD                    PIC X(20).
           05  RUCVG-DUR-YR-CD                  PIC X(02).
PR006C     05  RUCVG-CVG-STBL-2-CD              PIC X(04).
RP2005     05  RUCVG-CVG-STBL-4-CD              PIC X(02).
RPL005     05  RUCVG-CVG-STBL-3-CD              PIC X(05).
MFPUPL     05  RUCVG-CVG-ILLUS-CD               PIC X(01).
MFPUPL         88 RUCVG-CVG-ILLUS-DECR-TRM      VALUE 'D'.
MFFUPL     05  RUCVG-SA-INIT-PREM-AMT           PIC S9(13)V9(02) COMP-3.
MFFUPL     05  RUCVG-SA-SUBSEQ-PREM-AMT         PIC S9(13)V9(02) COMP-3.
MFFUPL     05  RUCVG-SA-INIT-LMPSM-AMT          PIC S9(13)V9(02) COMP-3.
MFFUPL     05  RUCVG-SA-CNVR-FND-AMT            PIC S9(13)V9(02) COMP-3.
MFFUPL     05  RUCVG-CVG-XPCT-ANTY-AGE          PIC X(03).
MFFUPL     05  RUCVG-CVG-XPCT-ANTY-AGE-R        REDEFINES
MFFUPL         RUCVG-CVG-XPCT-ANTY-AGE          PIC 9(03).
MP161D     05  RUCVG-CVG-WP-IND                 PIC X(01).
NVCN01         88  RUCVG-CVG-WP-YES             VALUE 'Y'.
NVCN01         88  RUCVG-CVG-WP-NO              VALUE 'N'.
NVCN01         88  RUCVG-CVG-WP-NONE            VALUE '0'.
NVCN01         88  RUCVG-CVG-WP-TYP-A           VALUE '1'.
NVCN01         88  RUCVG-CVG-WP-TYP-B           VALUE '2'.
MP213P     05  RUCVG-CVG-PRIM-GR-ID             PIC X(02).
M271N1     05  RUCVG-TAX-QUALF-IND              PIC X(01).
M271N1         88  RUCVG-TAX-QUALF-YES          VALUE 'Y'.
M271N1         88  RUCVG-TAX-QUALF-NO           VALUE 'N'.
TV2003     05  RUCVG-TRG-HIT-CNVR-RT            PIC X(03).
TLB002     05  RUCVG-CANCER-TYP-CD              PIC X(02).
UYS002     05  RUCVG-FRST-POL-PERI-DUR          PIC X(02).
PR006C*    05  FILLER                           PIC X(20).
MFPUPL*    05  FILLER                           PIC X(16).
MFFUPL*MFPUPL     05  FILLER                    PIC X(15).
RP2005*MFFUPL     05  FILLER                           PIC X(20).
RPL005*RP2005     05  FILLER                           PIC X(18).
MP161D*RPL005     05  FILLER                           PIC X(13).
TLB002*MP161D     05  FILLER                           PIC X(12).
TLB002     05  FILLER                           PIC X(10).

      *****************************************************************
      **                 END OF COPYBOOK ACFRUCVG                    **
      *****************************************************************
