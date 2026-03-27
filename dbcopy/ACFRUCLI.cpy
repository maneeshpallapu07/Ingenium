      *****************************************************************
      **  MEMBER :  ACFRUCLI                                         **
      **  REMARKS:  APPLICATION UPLOAD CLIENT TABLE LAYOUT           **
      **  LENGTH :  640                                              **
      *****************************************************************
      **  RELEASE  AUTH  DESCRIPTION                                 **
      **                                                             **
      **  11FEB13   CREATED FOR UCLI PROCESSING                      **
HNB002**  HIN/AI         NEW FIELDS FOR AUTO-ISSUE UPLOAD            **
CR116C**  JUN2003        ADD RUCLI-TCB-IND                           **
CR096A**  26JUN03  SG    ADD RUCLI-CVG-PKG-CD                        **
MFFUPL**  01OCT05  CLB   ADD FIELDS FOR MANUSTEP CHANGES             **
IPDDUP**  06OCT06  SUG   ADD FIELDS FOR INCOME PROTECTION RIDERS     **
NWLXML**  20JUL09  CTS   ADDED FIELD ANNUITY PERIOD                  **
C18250**  29MAR13  CTS   PS - SD91302 ADDED NEW FIELD FOR KANJI      **
C18250**                 SAMAKATA BU ADDRESS                         **
MP261B** 29OCT14  CTS     ARM2-NON FACE TO FACE XML UPLOAD CHANGES   **
MP270A**  17MAR16  FIELD ADDED AD PART OF E-POS APPLICATION XML      **
R12024**  25MAY16  CTS    CHANGES FOR UNDERWRITING REVIEW FLAG       **
018396**  26MAY19   CTS   CHANGES DONE FOR EPOSDAY2.0                **
TL0193**  20SEP21  CTS    TLB041 - CO NAME LENGTH INCREASE           **
26878B**  11APR23  CTS    ENHANCE CONTROL OVER ELDERLY & SPECIFIC    **
26878B**                  POLICY (PHASE 2)                           **
      *****************************************************************

       01  RUCLI-REC-INFO.
           05  RUCLI-KEY.
               10  RUCLI-APP-ID                 PIC X(15).
        RUCLI-APP-ID                 PIC X(15).
               10  RUCLI-SEQ-NUM                PIC X(02).
               10  RUCLI-SEQ-NUM-N              REDEFINES
                   RUCLI-SEQ-NUM                PIC 9(02).
           05  RUCLI-STCKR-ID                   PIC X(11).
           05  RUCLI-POL-CLI-REL-TYP-CD         PIC X(10).
           05  RUCLI-CVG-CLI-REL-TYP-CD         PIC X(02).
           05  RUCLI-CLI-PHON-NUM-TXT           PIC X(50).
           05  RUCLI-CLI-GIV-NM                 PIC X(25).
           05  RUCLI-CLI-SUR-NM                 PIC X(25).
           05  RUCLI-CLI-BTH-DT                 PIC X(10).
           05  RUCLI-CLI-SEX-CD                 PIC X(01).
           05  RUCLI-CLI-SMKR-CD                PIC X(20).
           05  RUCLI-CLI-OCCP-CD                PIC X(03).
           05  RUCLI-CLI-ADDR-CD                PIC X(08).
           05  RUCLI-CLI-PSTL-CD                PIC X(08).
           05  RUCLI-CLI-ADDR-TXT               PIC X(72).
           05  RUCLI-CLI-KA-SUR-NM              PIC X(25).
           05  RUCLI-CLI-KA-GIV-NM              PIC X(25).
           05  RUCLI-POL-CLI-INSRD-CD           PIC X(02).
           05  RUCLI-DTH-BEN-SUR-NM             PIC X(25).
           05  RUCLI-DTH-BEN-GIV-NM             PIC X(25).
           05  RUCLI-DTH-BEN-REL-CD             PIC X(02).
           05  RUCLI-DTH-BEN-PCT                PIC X(03).
           05  RUCLI-DTH-BEN-PCT-N              REDEFINES
               RUCLI-DTH-BEN-PCT                PIC 9(03).
           05  RUCLI-PR-DTH-BEN-GIV-NM          PIC X(25).
           05  RUCLI-PR-DTH-BEN-SUR-NM          PIC X(25).
           05  RUCLI-PR-DTH-GIV-NM              PIC X(25).
           05  RUCLI-PR-DTH-BEN-REL-CD          PIC X(02).
           05  RUCLI-SEL-CD                     PIC X(02).
           05  RUCLI-PRE-UW-IND                 PIC X(01).
               88  RUCLI-PRE-UW                 VALUE 'Y'.
               88  RUCLI-PRE-UW-NO              VALUE 'N'.
           05  RUCLI-SELF-DISC-IND              PIC X(01).
               88  RUCLI-SELF-DISC              VALUE 'Y'.
               88  RUCLI-SELF-DISC-NO           VALUE 'N'.
           05  RUCLI-BASE-CVG-IND               PIC X(01).
               88  RUCLI-BASE-CVG               VALUE 'Y'.
               88  RUCLI-BASE-CVG-NO            VALUE 'N'.
           05  RUCLI-LNB-IND                    PIC X(01).
               88  RUCLI-LNB                    VALUE 'Y'.
               88  RUCLI-LNB-NO                 VALUE 'N'.
           05  RUCLI-DTH-BNFY-CO-NM             PIC X(50).
TL0193*           05  RUCLI-CLI-CO-NM                  PIC X(50).
TL0193*           05  RUCLI-CLI-KA-CO-NM               PIC X(50).
TL0193     05  RUCLI-CLI-CO-NM                  PIC X(100).
TL0193     05  RUCLI-CLI-KA-CO-NM               PIC X(100).
HNB002     05  RUCLI-CLI-EARN-INCM-AMT       PIC S9(11)V9(02) COMP-3.
HNB002     05  RUCLI-OTHR-STCKR-ID              PIC X(11).
CR116C     05  RUCLI-TCB-IND                    PIC X(01).
CR116C         88  RUCLI-TCB                    VALUE 'Y'.
CR116C         88  RUCLI-TCB-NO                 VALUE 'N'.
CR096A     05  RUCLI-CVG-PKG-CD                 PIC X(03).
MFFUPL     05  RUCLI-OWN-INV-XPER-IND           PIC X(01).
MFFUPL         88  RUCLI-OWN-INV-XPER           VALUE 'Y'.
MFFUPL         88  RUCLI-OWN-INV-XPER-NO        VALUE 'N'.
MFFUPL     05  RUCLI-CLI-FIN-ASSET-CD           PIC X(01).
CR116C*    05  FILLER                           PIC X(20).
CR116C*    05  FILLER                           PIC X(19).
MFFUPL*CR096A     05  FILLER                    PIC X(16).
IPDDUP*MFFUPL     05  FILLER                           PIC X(14).
IPDDUP     05  RUCLI-IP-DTH-BEN-SUR-NM          PIC X(25).
IPDDUP     05  RUCLI-IP-DTH-BEN-GIV-NM          PIC X(25).
IPDDUP     05  RUCLI-IP-DTH-BNFY-CO-NM          PIC X(50).
IPDDUP     05  RUCLI-IP-DTH-BEN-REL-CD          PIC X(02).
IPDDUP     05  RUCLI-IP-DTH-BEN-PCT             PIC X(03).
IPDDUP     05  RUCLI-IP-DTH-BEN-PCT-N           REDEFINES
IPDDUP         RUCLI-IP-DTH-BEN-PCT             PIC 9(03).
NWLXML     05  RUCLI-BNFY-ANTY-PERI-CD          PIC X(02).
C18250     05  RUCLI-CLI-ADDR-KJ-TXT            PIC X(50).
MP261B     05  RUCLI-OTHR-INS-CO-BNFT-AMT       PIC S9(11)V9(02) COMP-3.
MP270A     05  RUCLI-INSRD-GUAR-IND             PIC X(01).
MP270A         88  RUCLI-INSRD-GUAR-YES         VALUE 'Y'.
MP270A         88  RUCLI-INSRD-GUAR-NO          VALUE 'N'.
MP270A     05  RUCLI-HEALTH-STAT-IND            PIC X(01).
MP270A         88  RUCLI-HEALTH-STAT-YES        VALUE 'Y'.
MP270A         88  RUCLI-HEALTH-STAT-NO         VALUE 'N'.
MP270A     05  RUCLI-BNFT-ENTLMT-HIST-IND       PIC X(01).
MP270A         88  RUCLI-BNFT-ENTLMT-HIST-YES   VALUE 'Y'.
MP270A         88  RUCLI-BNFT-ENTLMT-HIST-NO    VALUE 'N'.
MP270A     05  RUCLI-ESIGN-INSRD-GUAR-IND       PIC X(01).
MP270A         88  RUCLI-ESIGN-INSRD-GUAR-YES   VALUE 'Y'.
MP270A         88  RUCLI-ESIGN-INSRD-GUAR-NO    VALUE 'N'.
MP270A     05  RUCLI-BLOOD-TEST-IND             PIC X(01).
MP270A         88  RUCLI-BLOOD-TEST-YES         VALUE 'Y'.
MP270A         88  RUCLI-BLOOD-TEST-NO          VALUE 'N'.
MP270A     05  RUCLI-ECG-IND                    PIC X(01).
MP270A         88  RUCLI-ECG-YES                VALUE 'Y'.
MP270A         88  RUCLI-ECG-NO                 VALUE 'N'.
MP270A     05  RUCLI-ANN-INCM-AMT-IND           PIC X(01).
MP270A         88  RUCLI-ANN-INCM-AMT-YES       VALUE 'Y'.
MP270A         88  RUCLI-ANN-INCM-AMT-NO        VALUE 'N'.
MP270A     05  RUCLI-DLY-INCM-AMT-IND           PIC X(01).
MP270A         88  RUCLI-DLY-INCM-AMT-YES       VALUE 'Y'.
MP270A         88  RUCLI-DLY-INCM-AMT-NO        VALUE 'N'.
MP270A     05  RUCLI-AIS-UWDECSN-CD             PIC X(02).
MP270A     05  RUCLI-ADD-DSCLSRE-IND            PIC X(01).
MP270A         88  RUCLI-ADD-DSCLSRE-YES        VALUE '1'.
MP270A         88  RUCLI-ADD-DSCLSRE-NO         VALUE '0'.
MP270A     05  RUCLI-RADI-JOB-IND               PIC X(01).
MP270A         88  RUCLI-RADI-JOB-YES           VALUE 'Y'.
MP270A         88  RUCLI-RADI-JOB-NO            VALUE 'N'.
MP270A     05  RUCLI-SCV-SELCT-INFO-IND         PIC X(01).
MP270A         88  RUCLI-SCV-SELCT-INFO-YES     VALUE 'Y'.
MP270A         88  RUCLI-SCV-SELCT-INFO-NO      VALUE 'N'.
MP270A     05  RUCLI-OWN-GUAR-IND               PIC X(01).
MP270A         88  RUCLI-OWN-GUAR-YES           VALUE 'Y'.
MP270A         88  RUCLI-OWN-GUAR-NO            VALUE 'N'.
MP270A     05  RUCLI-MNGR-INTRVW-IND            PIC X(01).
MP270A         88  RUCLI-MNGR-INTRVW-YES        VALUE 'Y'.
MP270A         88  RUCLI-MNGR-INTRVW-NO         VALUE 'N'.
MP270A     05  RUCLI-MORAL-RISK-IND             PIC X(01).
MP270A         88  RUCLI-MORAL-RISK-YES         VALUE 'Y'.
MP270A         88  RUCLI-MORAL-RISK-NO          VALUE 'N'.
MP270A     05  RUCLI-ESIGN-OWN-GUAR-IND         PIC X(01).
MP270A         88  RUCLI-ESIGN-OWN-GUAR-YES     VALUE 'Y'.
MP270A         88  RUCLI-ESIGN-OWN-GUAR-NO      VALUE 'N'.
MP270A     05  RUCLI-SELF-OATH-INFO-IND         PIC X(01).
MP270A         88  RUCLI-SELF-OATH-INFO-YES     VALUE 'Y'.
MP270A         88  RUCLI-SELF-OATH-INFO-NO      VALUE 'N'.
MP270A     05  RUCLI-EDSCLSRE-IND               PIC X(01).
MP270A         88  RUCLI-EDSCLSRE-YES           VALUE 'Y'.
MP270A         88  RUCLI-EDSCLSRE-NO            VALUE 'N'.
R12024     05  RUCLI-UW-REVW-IND                PIC X(01).
R12024         88  RUCLI-UW-REVW-YES            VALUE 'Y'.
R12024         88  RUCLI-UW-REVW-NO             VALUE 'N'.
018396     05  RUCLI-GRDN-MORAL-RSK-CD          PIC X(01).
018396     05  RUCLI-INTRVW-OTHR-IND            PIC X(01).
018396         88  RUCLI-INTRVW-OTHR            VALUE 'Y'.
018396         88  RUCLI-INTRVW-OTHR-NO         VALUE 'N'.
018396     05  RUCLI-MINR-UNEMPL-IND            PIC X(01).
018396         88  RUCLI-MINR-UNEMPL            VALUE 'Y'.
018396         88  RUCLI-MINR-UNEMPL-NO         VALUE 'N'.
26878B     05  RUCLI-CLI-FIN-ASSET-AMT          PIC S9(15)V9(02) COMP-3.
IPDDUP     05  FILLER                           PIC X(20).

      *****************************************************************
      **                 END OF COPYBOOK ACFRUCLI                    **
      *****************************************************************
