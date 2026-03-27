      *************************
      *IDENTIFICATION DIVISION.
      *************************
      *PROGRAM-ID. ASBM2100.
      *
      *COPY XCWWCRHT.
      *
      *****************************************************************
      **  MEMBER :  ASBM2100                                         **
      **  REMARKS:  IMPORT APEX UPLOAD RECORDS AND CONVERT DATA      **
      **            TO NBS FORMAT.                                   **
      **                                                             **
      **  DOMAIN :  UW                                               **
      **  CLASS  :  PD                                               **
      *****************************************************************
      **  DATE     AUTH.  DESCRIPTION                                **
      **                                                             **
      **  04JUL90  NES    INITIAL APEX UPLOAD SYSTEM DESIGN          **
APEX53**  30NOV95  JJS    UPGRADE TO INGENIUM 5.3 & WINAPEX 1.0,     **
APEX53**                  STANDARDIZATION OF L0280 FIELDS,           **
APEX53**                  ADD WORKING STORAGE COPYBOOK XCWWPGWS,     **
APEX53**                  CHANGES TO SUPPORT I/O PROGRAMS            **
54-001**  01SEP96  TJS    MODIFICATIONS FOR MAINTAINABILITY          **
APEX54**  31DEC96  TJS    MODIFICATIONS FOR MAINTAINABILITY          **
555288**  30SEP97  TJS    DEFAULT MIB INDICATOR                      **
557020**  30SEP97  TJS    CHECK PLAN BEFORE WRITING SIR REQT         **
557245**  30SEP97  TJS    LET APPL FORM TYPE DEFAULT FROM PLAN       **
557658**  30SEP97  JWT    YEAR 2000 REPORT MODIFICATIONS             **
557659**  30SEP97  KLE    DATA ARCHITECTURE MODIFICATION             **
557660**  30SEP97  SC     CODE CLEANUP                               **
557698**  30SEP97  KLE    MIXED CASE DATA                            **
557700**  30SEP97  KLE    APEX UPLOAD RELEASE 5.5                    **
557788**  30SEP97  KLE    POLICY TABLE SPLIT                         **
559242**  30SEP97  TJS    TEMP WORKAROUND UNTIL PCR 9242 IS FIXED    **
559577**  30SEP97  TJS    TEMP WORKAROUND UNTIL PCR 9577 IS FIXED    **
008453**  31MAR98  552    IMPLEMENT MULTIPLE EXTERNAL DATE           **
008455**  31MAR98  552    EXTENSIVE NUMERIC FORMATTING               **
005409**  30OCT98  56     MODIFICATIONS REQUIRED FOR LEAP YEAR       **
007678**  30OCT98  56     POLICY NUMBER ASSIGNMENT BY BUSINESS CLASS **
007766**  30OCT98  56     ARCHITECTURE CHANGES TO SUPPORT PASSING    **
007766**                  PARAMETERS VIA AN ADDRESS                  **
010154**  30OCT98  56     FIX PHONETIC ENCODING                      **
010302**  30OCT98  56     ARCHITECTURE CHANGES - REMOVE PRTS ACCESS  **
010303**  30OCT98  56     REPLACEMENT RULES                          **
010310**  30OCT98  56     SQL I/O PERFORMANCE CHANGES                **
010313**  30OCT98  56     APEX ENHANCEMENTS                          **
007684**  30OCT98  56     CHECKPOINT/RESTART                         **
014177**  15MAR99  56     APEX 3.1 UPLOAD TO INGENIUM 5.6            **
012141**  07MAY99  56V    PORTFOLIO EXPANSION                        **
012148**  07MAY99  56V    CHANGES FOR TRAIL COMMISSIONS              **
012696**  07MAY99  56V    CHANGES REQUIRED FOR ALLOCATIONS           **
014035**  07MAY99  56V    CODE CLEANUP                               **
014178**  07MAY99  56V    APEX 3.1 UPLOAD TO INGENIUM 5.6V           **
015508**  15DEC99  60     CLIENT ENHANCEMENT FOR JAPAN               **
015509**  15DEC99  60     ADDRESS ENHANCEMENT FOR JAPAN              **
014590**  15DEC99  60     ARCHITECTURAL CHANGES                      **
015543**  15DEC99  60     CODE CLEANUP                               **
016103**  29SEP00  611J   SEARCH ENHANCEMENTS FOR JAPANESE           **
MCL   **  01AUG01  EKM    PROGRAM NOT USED BY MCL                    **
      *****************************************************************
      *
      **********************
      *ENVIRONMENT DIVISION.
      **********************
      *
      *INPUT-OUTPUT SECTION.
      *
      ***************
      *DATA DIVISION.
      ***************
      *
      *FILE SECTION.
      *
      *WORKING-STORAGE SECTION.
      *
APEX53*COPY XCWWPGWS REPLACING '$VAR1' BY 'ASBM2100'.
      *
      *COPY SQLCA.
      *
014590*COPY XCWL0030.
      *
007766*COPY XCWWCVGM.
012141*COPY XCWWFNDM.
      *
      *01  WS-PGM-WORK-AREA.
      *    05  WS-USER-ID                  PIC X(08)  VALUE 'APEXUSER'.
      *    05  HOLD-RUN-ID                 PIC X(01)  VALUE '1'.
      *    05  WS-ERROR-SW                 PIC X(01).
      *        88  WS-ERROR-FOUND          VALUE 'Y'.
      *    05  WS-COMPANY-CODE             PIC X(02).
      *    05  WS-INSURED-NAME             PIC X(20).
      *    05  WS-SCREEN-NAME              PIC X(25).
557658*    05  WS-RUN-DATE                 PIC X(05).
557658*    05  WS-DATE                     PIC X(07).
      *    05  WS-PIC-COUNTER              PIC Z(06)9.
008455*    05  WS-PIC-9-2                  PIC Z(06)9.99.
      *    05  WS-NAME-LENGTH              PIC S9(04).
      *    05  WS-PH-NO-TYPE               PIC X.
      *        88  WS-PH-NO-TYPE-VALID                VALUE '1' '2' '3'
      *                                                     '4' '5' '6'
      *                                                     '7' '8' '9'.
      *    05  WS-PCOM-CO-AUD-CTR-LOB-CD   PIC X(01).
016103*    05  WS-PHNT-TXT                   PIC X(08).
016103*    05  WS-PHNT-NUM REDEFINES WS-PHNT-TXT PIC 9(8).
      *    05  WS-AUDIT-REC.
      *        10  FILLER                  PIC X(02)  VALUE 'AD'.
      *        10  FILLER                  PIC S9(04) COMP VALUE +10.
      *        10  WS-AUDIT-POL            PIC X(10).
      *    05  WS-CURRENT-AGE              PIC 999.
      *    05  WS-AGE-BAND                 PIC 9999.
      *    05  WS-PATT-PLAN-NUM            PIC 9.
      *    05  WS-ABEND                    PIC 9      VALUE ZERO.
      *    05  I                           PIC S9(04) COMP SYNC.
      *    05  J                           PIC S9(04) COMP SYNC.
      *    05  K                           PIC S9(04) COMP SYNC.
      *    05  X                           PIC S9(04) COMP SYNC.
APEX54*    05  Y                           PIC S9(04) COMP SYNC.
APEX54*    05  WS-CVG                      PIC 9(02)  VALUE ZERO.
      *    05  WS-REMAINDER                PIC 9(01).
      *    05  WS-FIND-ADDR-TYP            PIC X(02).
      *    05  WS-COMPANY-NAME.
      *        10  WS-COMPANY-NAME-FIRST25 PIC X(25).
      *        10  WS-COMPANY-NAME-LAST25  PIC X(25).
      *    05  WS-BNK-ACCT-NUM             PIC 9.
APEX53*    05  WS-BNFY-CONT-EXISTS-IND     PIC X(01).
APEX53*        88  WS-BNFY-CONT-EXISTS-NO             VALUE 'N'.
APEX53*        88  WS-BNFY-CONT-EXISTS                VALUE 'Y'.
APEX53*    05  WS-BNFY-PRIMARY-EXISTS-IND  PIC X(01).
APEX53*        88  WS-BNFY-PRIMARY-EXISTS-NO          VALUE 'N'.
APEX53*        88  WS-BNFY-PRIMARY-EXISTS             VALUE 'Y'.
APEX53*    05  WS-BNFY-PERC-CONTINGENT     PIC 9(03).
APEX53*        88  WS-BNFY-PERC-CONT-ZERO             VALUE ZEROES.
APEX53*        88  WS-BNFY-PERC-CONT-100              VALUE 100.
APEX53*    05  WS-BNFY-PERC-PRIMARY        PIC 9(03).
APEX53*        88  WS-BNFY-PERC-PRIMARY-ZERO          VALUE ZEROES.
APEX53*        88  WS-BNFY-PERC-PRIMARY-100           VALUE 100.
APEX53*    05  WS-BNFY-PERC-ERROR-IND      PIC X(01).
APEX53*        88  WS-BNFY-PERC-ERROR-NO              VALUE 'N'.
APEX53*        88  WS-BNFY-PERC-ERROR                 VALUE 'Y'.
APEX53*    05  WS-LOOP-CVG-NUM             PIC X(02).
APEX54*    05  WS-QUOTE-NUM                PIC 9(06).
010313*    05  WS-SHARE                    PIC S9(04) COMP.
010313*    05  WS-TOTAL-SHARE              PIC S9(04) COMP.
010313*    05  WS-EQUAL-SHARE-CNT          PIC S9(04) COMP.
010313*    05  WS-START                    PIC S9(04) COMP.
010313*    05  WS-END                      PIC S9(04) COMP.
010313*    05  WS-POL-APP-SIGN-DT          PIC X(10).
010313*** FORMAT OF CLIENT_SIGNATURE FIELD IS HH:MM  MM/DD/YYYY@FILENAME
010313*    05  WS-SIGNATURE-FLD.
010313*        10  FILLER                  PIC X(07).
010313*        10  WS-SIGNATURE-MON        PIC X(02).
010313*        10  FILLER                  PIC X(01).
010313*        10  WS-SIGNATURE-DAY        PIC X(02).
010313*        10  FILLER                  PIC X(01).
010313*        10  WS-SIGNATURE-YR         PIC X(04).
      *
      *01  WS-SUBSCRIPTS.
      *    05  WS-SUB                      PIC S9(04) COMP SYNC.
      *    05  WS-IN-SUB                   PIC S9(04) COMP SYNC.
      *    05  WS-BEN-SUB                  PIC S9(04) COMP SYNC.
      *    05  WS-FLD-SUB                  PIC S9(04) COMP SYNC.
      *    05  WS-NUM-SUB                  PIC S9(04) COMP SYNC.
      *    05  REQT-SUB                    PIC S9(04) COMP SYNC.
      *    05  CLI-SUB                     PIC S9(04) COMP SYNC.
      *    05  ADDR-SUB                    PIC S9(04) COMP SYNC.
      *    05  WS-UTTB-SUB                 PIC S9(04) COMP SYNC.
      *    05  WS-SEGF-SUB                 PIC S9(04) COMP SYNC.
      *
      *01  WS-SHIFT-WORK-AREAS.
      *    05  WS-LEN                      PIC S9(04) COMP  SYNC.
      *    05  WS-EXTRA                    PIC S9(04) COMP  SYNC.
      *    05  WS-SRCE                     PIC S9(04) COMP  SYNC.
      *    05  WS-DEST                     PIC S9(04) COMP  SYNC.
      *    05  WS-PAD-BYTE                 PIC X.
008453*    05  WS-INTERNAL-DATE.
008453*        10  WS-INTERNAL-YEAR        PIC 9(03).
008453*        10  WS-INTERNAL-MMDD        PIC X(04).
      *    05  WS-SHIFT-SIZE               PIC S9(04) COMP  SYNC
      *                                               VALUE +80.
      *    05  WS-SHIFT-GROUP.
      *        10  WS-SHIFT-BYTE           PIC X     OCCURS 80 TIMES.
      *    05  WS-SHIFT-GROUP-NUM          REDEFINES WS-SHIFT-GROUP.
      *        10  FILLER                  PIC X(70).
      *        10  WS-SHIFT-XNUM           PIC X(10).
      *
      *01  WS-APP-HEADER-INFO.
      *    05  WS-APP-TYP-CD               PIC 9.
      *        88  WS-APP-IS-SINGLE-LIFE        VALUE 1.
      *        88  WS-APP-IS-JOINT-LIFE         VALUE 2.
      *        88  WS-APP-IS-MULTIPLE-LIFE      VALUE 3.
      *    05  WS-APP-PROD-CD              PIC 9.
      *        88  WS-APP-IS-LIFE-PROD          VALUE 1.
      *        88  WS-APP-IS-DISABILITY-PROD    VALUE 2.
      *        88  WS-APP-IS-IMM-ANNUITY-PROD   VALUE 3.
      *        88  WS-APP-IS-DEF-ANNUITY-PROD   VALUE 4.
014177*        88  WS-APP-IS-HEALTH-PROD        VALUE 5.
      *    05  WS-SERV-AGT-ID              PIC X(06).
557700*    05  WS-APP-SAVE-DATE-X.
557700*        10  WS-APP-SAVE-DATE-X-YR   PIC 9(04).
557700*        10  WS-APP-SAVE-DATE-X-MO   PIC 9(02).
557700*        10  WS-APP-SAVE-DATE-X-DY   PIC 9(02).
557700*    05  WS-APP-SAVED-DATE.
557700*        10  WS-APP-SAVED-DATE-YR    PIC 9(04).
557700*        10  FILLER                  PIC X(01) VALUE '-'.
557700*        10  WS-APP-SAVED-DATE-MO    PIC 9(02).
557700*        10  FILLER                  PIC X(01) VALUE '-'.
557700*        10  WS-APP-SAVED-DATE-DY    PIC 9(02).
      *
      *01  WS-CLI-ID-TABLE.
      *    05  WS-CLI-INFO                 OCCURS 50.
      *        10  WS-CLI-TYPE-CD          PIC 9(01).
      *            88  WS-CLI-IS-PERSON             VALUE 0.
      *            88  WS-CLI-IS-COMPANY            VALUE 1.
      *        10  WS-CLI-BASE-CD          PIC 9(01).
      *            88  WS-CLI-BASE-INSURED          VALUE 1.
      *        10  WS-CLI-RIDER-CD         PIC 9(01).
      *            88  WS-CLI-RIDER-INSURED         VALUE 1.
      *        10  WS-CLI-PRIM-OWN-CD      PIC 9(01).
      *            88  WS-CLI-PRIMARY-OWNER         VALUE 1.
      *        10  WS-CLI-OTHR-OWN-CD      PIC 9(01).
      *            88  WS-CLI-OTHER-OWNER           VALUE 1.
      *        10  WS-CLI-SPOUS-CD         PIC 9(01).
      *            88  WS-CLI-SPOUSE                VALUE 1.
      *        10  WS-CLI-BNFY-CD          PIC 9(01).
      *            88  WS-CLI-BENEFICIARY           VALUE 1.
559577*        10  WS-CLI-PHYS-CD          PIC 9(01).
559577*            88  WS-CLI-PHYSICIAN             VALUE 1.
      *
      *01  WS-CLI-ADDR-TABLE.
      *    05  WS-CLI-ADDR-INFO            OCCURS 5.
      *        10  FILLER                  PIC X(12).
      *        10  WS-CLI-ADDR-TYP-ID      PIC X(02).
      *        10  WS-CLI-ADDR-SEQ-NUM     PIC 9(03).
      *        10  FILLER                  PIC X(207).
      *
557700*01  WS-CLI-CONTACT-TABLE.
557700*    05  WS-CLI-CONTACT              OCCURS 2.
557700*        10  WS-CLI-CNTCT-ID-CD      PIC X(02).
557700*        10  WS-CLI-CNTCT-ID-TXT     PIC X(20).
      *
      *01  WS-PROD-HEADER-INFO.
      *    05  WS-POL-CLASS-CD             PIC 9.
      *        88  WS-POL-IS-BASE-POLICY        VALUE 0.
      *        88  WS-POL-IS-OPTIONAL-POLICY    VALUE 1.
      *        88  WS-POL-IS-ADDITIONAL-POLICY  VALUE 2.
      *    05  WS-CONN-OPTL-POL-ID         PIC X(10).
      *
      *01  WS-HOLD-AREAS.
      *    05  WS-HOLD-POLICY-NUM          PIC X(10).
      *    05  WS-HOLD-LINE-OF-BUSINESS    PIC X(01).
      *        88  WS-LIFE-POLICY          VALUE 'L'.
      *        88  WS-DI-POLICY            VALUE 'H'.
      *        88  WS-IMM-ANN-POLICY       VALUE 'A'.
      *        88  WS-DEF-ANN-POLICY       VALUE 'E'.
      *        88  WS-ANNUITY-POLICY       VALUE 'E', 'A'.
      *    05  WS-CLI-ID-TABLE-MAX-SIZE    PIC S9(04) COMP VALUE +50.
      *    05  WS-CLIENTS-IN-APP           PIC S9(04) COMP.
      *    05  WS-PRIMARY-INSD-CLI-ID      PIC X(10).
      *    05  WS-POL-ISS-LOC-CD           PIC X(02).
010313*    05  WS-SBSDRY-CO-ID             PIC X(02).
      *    05  WS-POL-CTRY-CD              PIC X(02).
      *    05  WS-POL-APP-SIGN-IND         PIC X(01).
557659*    05  WS-POL-MIB-SIGN-IND         PIC X(01).
557659*    05  WS-POL-MIB-SIGN-CD          PIC X(01).
      *    05  WS-POL-OWNER-FOUND-IND      PIC X(01).
APEX54*        88  WS-POL-OWNER-FOUND      VALUE 'Y'.
      *    05  SED-CONTAINS-SPEC-FLD-IND   PIC X(01).
      *        88  SED-CONTAINS-SPECIAL-FIELD VALUE 'Y'.
      *    05  WS-PAC-CLI-MATCH-IND        PIC X(01).
      *        88  WS-PAC-CLIENT-MATCHED   VALUE 'Y'.
      *        88  WS-PAC-CLIENT-NOT-MATCHED   VALUE 'N'.
557660*    05  WS-HOLD-FACE-AMOUNT         PIC S9(9)V99 COMP-3.
      *    05  WS-HOLD-BASE-PLAN           PIC X(05).
      *    05  WS-HOLD-CLI-AREA            PIC X(1050).
      *    05  WS-APPL-FORM-NO             PIC 9(15).
      *    05  WS-APPL-FORM-NO-X           REDEFINES
      *        WS-APPL-FORM-NO             PIC X(15).
      *    05  WS-HOLD-FIRST-INITIAL       PIC X(01).
      *    05  WS-HOLD-FREQ.
      *        10  WS-FREQ-METHOD          PIC X(01).
      *        10  FILLER                  PIC X(01).
      *        10  WS-FREQ-MODE            PIC X(02).
      *    05  WS-HOLD-ASTHMA.
      *        10  WS-HOLD-HOURS           PIC 9(03).
      *        10  FILLER                  PIC X(06)  VALUE ' HOURS'.
      *    05  WS-PICV96-DATA.
      *        10  WS-PICV96               PIC S9(12)V9(6) COMP-3.
      *    05  WS-STRUCTURE-NAME           PIC X(20).
      *        88  WS-MQ-STRUCTURE                   VALUE
      *                                    'AIDSANTIMQ          '
      *                                    'AIDSMQ              '
      *                                    'BACK                '
      *                                    'BENPENS             '
      *                                    'DIABETES            '
      *                                    'INCOMPLETEMQ        '
      *                                    'MEDEVIDALBUMINMQ    '
      *                                    'MEDEVIDALLERGMQ     '
      *                                    'MEDEVIDAMPMQ        '
      *                                    'MEDEVIDANEMIAMQ     '
      *                                    'MEDEVIDAPPENDMQ     '
      *                                    'MEDEVIDARTHMQ       '
      *                                    'MEDEVIDATTACKMQ     '
      *                                    'MEDEVIDBLEEDMQ      '
      *                                    'MEDEVIDBLOODMQ      '
      *                                    'MEDEVIDBREATHMQ     '
      *                                    'MEDEVIDBRONCHMQ     '
      *                                    'MEDEVIDCANCERMQ     '
      *                                    'MEDEVIDCOLITISMQ    '
      *                                    'MEDEVIDCONVULSEMQ   '
      *                                    'MEDEVIDCOUGHMQ      '
      *                                    'MEDEVIDCYSTMQ       '
      *                                    'MEDEVIDDEFORMMQ     '
      *                                    'MEDEVIDDIARRMQ      '
      *                                    'MEDEVIDDISBLADMQ    '
      *                                    'MEDEVIDDISBLOODMQ   '
      *                                    'MEDEVIDDISEASEMQ    '
      *                                    'MEDEVIDDISKIDMQ     '
      *                                    'MEDEVIDDISOTHERMQ   '
      *                                    'MEDEVIDDISPROSMQ    '
      *                                    'MEDEVIDDIVERTMQ     '
      *                                    'MEDEVIDDIZZMQ       '
      *                                    'MEDEVIDEARSMQ       '
      *                                    'MEDEVIDEMOTMQ       '
      *                                    'MEDEVIDEMPHYSMQ     '
      *                                    'MEDEVIDEPILMQ       '
      *                                    'MEDEVIDEYESMQ       '
      *                                    'MEDEVIDFAINTMQ      '
      *                                    'MEDEVIDGALLMQ       '
      *                                    'MEDEVIDGOUTMQ       '
      *                                    'MEDEVIDHEADMQ       '
      *                                    'MEDEVIDHEARTMQ      '
      *                                    'MEDEVIDHERNIAMQ     '
      *                                    'MEDEVIDHOARSEMQ     '
      *                                    'MEDEVIDINFECTMQ     '
      *                                    'MEDEVIDINTESTMQ     '
      *                                    'MEDEVIDJAUNDMQ      '
      *                                    'MEDEVIDLAMEMQ       '
      *                                    'MEDEVIDLIVERMQ      '
      *                                    'MEDEVIDLUNGMQ       '
      *                                    'MEDEVIDLYMPHMQ      '
      *                                    'MEDEVIDMENTALMQ     '
      *                                    'MEDEVIDMUSCLESMQ    '
      *                                    'MEDEVIDNERVMQ       '
      *                                    'MEDEVIDNEURMQ       '
      *                                    'MEDEVIDNOSEMQ       '
      *                                    'MEDEVIDOTHENDOMQ    '
      *                                    'MEDEVIDOTHERMQ      '
      *                                    'MEDEVIDPALPMQ       '
      *                                    'MEDEVIDPARAMQ       '
      *                                    'MEDEVIDPILESMQ      '
      *                                    'MEDEVIDPLEURMQ      '
      *                                    'MEDEVIDPNEUMMQ      '
      *                                    'MEDEVIDPUSMQ        '
      *                                    'MEDEVIDRECURMQ      '
      *                                    'MEDEVIDREUHMMQ      '
      *                                    'MEDEVIDRHEUMMQ      '
      *                                    'MEDEVIDSCIATMQ      '
      *                                    'MEDEVIDSKINLMQ      '
      *                                    'MEDEVIDSKINMQ       '
      *                                    'MEDEVIDSPEECHMQ     '
      *                                    'MEDEVIDSPITMQ       '
      *                                    'MEDEVIDSTOMACHMQ    '
      *                                    'MEDEVIDSTONEMQ      '
      *                                    'MEDEVIDSTROKEMQ     '
      *                                    'MEDEVIDSUGARMQ      '
      *                                    'MEDEVIDTHROATMQ     '
      *                                    'MEDEVIDTHYRMQ       '
      *                                    'MEDEVIDTUBERMQ      '
      *                                    'MEDEVIDULCERMQ      '
      *                                    'MEDEVIDURINEMQ      '
      *                                    'MEDEVIDVDMQ         '.
      *        88  WS-DETAILED-MQ-STRUCTURE          VALUE
      *                                    'CHEST               '
      *                                    'DIABETES            '
      *                                    'ASTHMA              '.
      *        88  WS-ADDL-INFO-MQ-STRUCTURE         VALUE
      *                                    'ASTHMAPHPH          '
      *                                    'CHESTPH             '
      *                                    'CHESTDIAG           '
      *                                    'CHESTECG            '
      *                                    'CHESTHOSPIT         '
      *                                    'CHESTMED            '
      *                                    'CHESTTRESULT        '
      *                                    'DIABPH              '.
      *    05  WS-DECRATMOD-WORK-AREA.
      *        10  WS-DECRATMOD-COUNT      PIC S9(04)  COMP SYNC.
      *        10  WS-DECRATMOD-NAME.
007766*            15  WS-DECRATMOD-BYTE   PIC X OCCURS 20 TIMES.
007766*            15  WS-DECRATMOD-BYTE   PIC X OCCURS 99 TIMES.
      *        10  WS-DECRATMOD-NAME-X     REDEFINES WS-DECRATMOD-NAME.
      *            15  WS-DECRATMOD-FRST6  PIC X(06).
      *            15  FILLER              PIC X(14).
      *        10  WS-DECRATMOD-NAME-X-2   REDEFINES WS-DECRATMOD-NAME.
      *            15  WS-DECRATMOD-FRST12 PIC X(12).
      *            15  FILLER              PIC X(08).
      *        10  WS-NEW-DECRATMOD-CODE.
      *            15  WS-DECRATMOD-NUM    PIC X.
      *        10  WS-DECRATMOD-CODE-TABLE.
      *            15  WS-DECRATMOD-CODE   PIC X     OCCURS 2.
      *        10  WS-DECRATMOD-CODE-FOUND-SW  PIC X.
      *            88  WS-DECRATMOD-CODE-FOUND       VALUE 'Y'.
      *    05  WS-INFC-PEND-WORK-AREA.
      *        10  WS-PEND-NUM             PIC 9(01).
      *        10  WS-INFC-PEND-COUNT      PIC S9(04)  COMP SYNC.
      *        10  WS-INFC-PEND-NAME.
007766*            15  WS-INFC-PEND-BYTE   PIC X OCCURS 20 TIMES.
007766*            15  WS-INFC-PEND-BYTE   PIC X OCCURS 99 TIMES.
      *        10  WS-INFC-PEND-NAME-X     REDEFINES WS-INFC-PEND-NAME.
      *            15  WS-INFC-PEND-FRST10 PIC X(10).
      *            15  FILLER              PIC X(10).
      *        10  WS-INFC-PEND-NAME-X-2   REDEFINES WS-INFC-PEND-NAME.
      *            15  WS-INFC-PEND-FRST11 PIC X(11).
      *            15  FILLER              PIC X(09).
      *        10  WS-INFC-PEND-NAME-X-3   REDEFINES WS-INFC-PEND-NAME.
      *            15  WS-INFC-PEND-FRST12 PIC X(12).
      *            15  FILLER              PIC X(08).
      *        10  WS-INFC-PEND-NAME-X-4   REDEFINES WS-INFC-PEND-NAME.
      *            15  WS-INFC-PEND-FRST4  PIC X(04).
      *            15  FILLER              PIC X(16).
      *        10  WS-NEW-INFC-PEND-CODE.
      *            15  WS-INFC-PEND-TYPE   PIC X(4).
      *            15  WS-INFC-PEND-NUM    PIC X.
      *        10  WS-INFC-PEND-CODE-TABLE.
      *            15  WS-INFC-PEND-CODE   PIC X(5)  OCCURS 6.
      *        10  WS-INFC-PEND-CODE-FOUND-SW  PIC X.
      *            88  WS-INFC-PEND-CODE-FOUND       VALUE 'Y'.
      *
      *    05  WS-RC-WORK-AREA.
      *        10  WS-RC-FIELD-NAME.
007766*            15  WS-RC-FIELD-BYTE    PIC X OCCURS 20 TIMES.
007766*            15  WS-RC-FIELD-BYTE    PIC X OCCURS 99 TIMES.
      *        10  WS-RC-FIELD-NAME-X      REDEFINES WS-RC-FIELD-NAME.
      *            15  WS-RC-FIELD-FRST10  PIC X(10).
      *            15  FILLER              PIC X(10).
      *        10  WS-RC-FIELD-NAME-X-2    REDEFINES WS-RC-FIELD-NAME.
      *            15  WS-RC-FIELD-FRST12  PIC X(12).
      *            15  FILLER              PIC X(08).
      *        10  WS-RC-SUB               PIC 9.
      *        10  WS-SAVE-RC-COMPANY      PIC X OCCURS 5.
      *    05  WS-SAVE-DEFERRED-AREA.
      *        10  WS-SAVE-DEFERRED-RECORDS OCCURS 3 TIMES.
      *            15  WS-SAVE-DEFERRED-PLANRS   PIC X(06).
      *            15  WS-SAVE-DEFERRED-INS-TYPE PIC X(1).
      *    05  WS-SEGFUND-ID               PIC X(07).
      *        88  WS-SEGFUND              VALUE 'SEGFSG'.
      *        88  WS-SINGLE-SEGFUND       VALUE 'SEGFSSG'.
      *        88  WS-REGISTERED-SEGFUND   VALUE 'SEGFRSG'.
014177*        88  WS-SF-FUND              VALUE 'FUND'.
      *    05  WS-SAVE-SEGFUND-AREA.
007766*        10  WS-SAVE-SEGFUND-RECORD OCCURS 20 TIMES.
007766*        10  WS-SAVE-SEGFUND-RECORD OCCURS 99 TIMES.
      *            15  WS-SAVE-APEX-SEGF-CODE    PIC X(05).
      *                88  WS-SAVE-APEX-SEGFUND        VALUE 'SEGF'.
      *                88  WS-SAVE-APEX-SINGLE-SEGFUND VALUE 'SEGFS'.
      *                88  WS-SAVE-APEX-REG-SEGFUND    VALUE 'SEGFR'.
014177*                88  WS-SAVE-APEX-SF-FUND        VALUE 'FUND'.
012141*            15  WS-SAVE-SEGFUND-DETAILS OCCURS 15 TIMES.
012141*            15  WS-SAVE-SEGFUND-DETAILS OCCURS 25 TIMES.
      *                20  WS-SEGFUND-CODE       PIC X(05).
014177*                20  WS-SEGFUND-PERCENT    PIC 9(03)V9(04) COMP-3.
014177*                20  WS-SEGFUND-PERCENT1   PIC 9(03)V9(04) COMP-3.
014177*                20  WS-SEGFUND-PERCENT2   PIC 9(03)V9(04) COMP-3.
012696*                20  WS-SEGFUND-PERCENT    PIC 9(03)V9(04) COMP-3.
012696*                20  WS-SEGFUND-PERCENT1   PIC 9(03)V9(04) COMP-3.
012696*                20  WS-SEGFUND-PERCENT2   PIC 9(03)V9(04) COMP-3.
014177*    05  WS-SEGF-CVG                 PIC 9(02).
014177*    05  WS-FS-ERROR-IND             PIC X(01).
014177*        88  WS-FS-OK                           VALUE 'N'.
014177*        88  WS-FS-ERROR                        VALUE 'Y'.
014177*    05  WS-FUND-CTR                 PIC S9(04) COMP SYNC.
014178*    05  WS-ALLOC-PCT                PIC S9(03)V9(04) COMP-3.
014178*    05  WS-PAYO-IND                 PIC X(01).
014178*        88  WS-PAYO-DCA                        VALUE 'C'.
014178*        88  WS-PAYO-AR                         VALUE 'R'.
014178*    05  WS-DEST-FUND-CTR            PIC S9(04) COMP SYNC.
014178*    05  WS-DEST-FUND-DETAILS    OCCURS 25 TIMES.
014178*        10  WS-DEST-FUND            PIC X(05).
014178*        10  WS-DEST-PERCENT         PIC 9(03)V9(04)  COMP-3.
014178*        10  WS-DEST-AMOUNT          PIC S9(13)V9(02) COMP-3.
014178*    05  WS-DIA-CVG                  PIC 9(02)  VALUE ZERO.
014178*    05  WS-GIA-CVG                  PIC 9(02)  VALUE ZERO.
014178*    05  WS-CVG-ALLOC            OCCURS 20 TIMES.
014178*        10  WS-CVG-ALLOC-PCT1       PIC 9(03)V9(04)  COMP-3.
014178*        10  WS-CVG-ALLOC-PCT2       PIC 9(03)V9(04)  COMP-3.
014178*    05  WS-PALC-CVG-CTR             PIC 9(02).
      *    05  WS-MESSAGE-NUMBER           PIC 9(04).
      *    05  WS-MESSAGE-NUMBER-X         REDEFINES
      *        WS-MESSAGE-NUMBER           PIC X(04).
      *    05  WS-MESSAGE-CODE.
      *        10  FILLER                  PIC X(12).
      *        10  WS-MESSAGE-LAST2        PIC X(02).
      *        10  FILLER                  PIC X(02).
      *
      *    05  WS-MESSAGE-ARRAY-MAX-SIZE   PIC S9(03) COMP-3 VALUE +25.
      *    05  WS-MESSAGE-SET-ARRAY.
      *        10  WS-MESSAGE-SET-SW       PIC X OCCURS 25.
      *            88  WS-MESSAGE-SET                VALUE 'Y'.
      *    05  WS-REQT-TABLE.
      *        10  WS-REQT-DETAILS OCCURS 11 TIMES.
      *            15  WS-REQT-CODE        PIC X(05).
      *            15  WS-REQT-STATUS      PIC X(03).
      *    05  WS-REQT-MAX                 PIC S9(04) COMP VALUE +11.
      *
010313*** THE POL AND CVG BENE HOLD AREAS SHOULD BE THE SAME SIZE AS
010313*** THE LAPUP-BENE-REL-TABLE AREA (CURRENTLY 12 X 76 CHARS)
010313*    05  WS-BENE-HOLD-AREAS.
010313*        10  WS-POL-BENE-HOLD-AREA      PIC X(912).
010313*        10  WS-CVG-BENE-HOLD-AREAS.
010313*            15  WS-CVG-BENE-HOLD-AREA  PIC X(912)
010313***                                     OCCURS 99 TIMES.
010313*                                       OCCURS 20 TIMES.
010313*    05  WS-BENE-CVG                 PIC 9(02).
      *
      *01  WS-PROG-SWITCHES.
      *    05  WS-EOF-MARKER               PIC X.
      *        88  EOF-REACHED                       VALUE 'Y'.
      *    05  WS-END-OF-RECORD-SW         PIC X.
      *        88  WS-END-OF-RECORD                  VALUE 'Y'.
      *    05  WS-DELIMITER-SW             PIC X.
      *        88  WS-DELIMITER-FOUND                VALUE 'Y'.
      *    05  WS-PLAN-ERROR-SW            PIC X.
      *        88  WS-PLAN-ERROR                     VALUE 'Y'.
      *    05  WS-DECRATMOD-FULL-MSG-SW    PIC X.
      *        88  WS-DECRATMOD-FULL-MSG             VALUE 'Y'.
      *    05  WS-INFC-PEND-FULL-MSG-SW    PIC X.
      *        88  WS-INFC-PEND-FULL-MSG             VALUE 'Y'.
      *    05  WS-DUPLICATE-MEDI-SW        PIC X.
      *        88  WS-DUPLICATE-MEDI                 VALUE 'Y'.
      *    05  WS-DU-USED-SW               PIC X.
      *        88  WS-DU-USED                        VALUE 'Y'.
      *    05  WS-INIT-PREM-RCVD-SW        PIC X.
      *        88  WS-INIT-PREM-RCVD                 VALUE 'Y'.
      *    05  WS-MULT-WGT-CHANGE-SW       PIC X.
      *        88  WS-MULT-WGT-CHANGE                VALUE 'Y'.
      *    05  WS-MULT-WGT-CHANGE-RSN-SW   PIC X.
      *        88  WS-MULT-WGT-CHANGE-RSN            VALUE 'Y'.
      *    05  WS-ASSIGN-POLNO-SW          PIC X.
      *        88  ASSIGN-POLNO                      VALUE 'Y'.
      *        88  NO-POLNO-ASSIGN                   VALUE 'N'.
      *    05  WS-NEW-CLIENT-CREATED       PIC X.
      *        88  OLD-CLIENT-MATCHED                VALUE 'C'.
      *        88  NEW-CLIENT-CREATED                VALUE 'Y'.
      *    05  WS-INSURED-TIA-IND-FLAG     PIC X.
      *        88  INSURED-TIA-IND-ON                VALUE 'Y' 'N'.
      *    05  WS-INVALID-QUOTE-SW         PIC X.
      *        88  WS-INVALID-QUOTE                  VALUE 'Y'.
010313*    05  WS-AGT-EQUAL-SHARES-SW      PIC X.
010313*        88  WS-AGT-EQUAL-SHARES               VALUE 'Y'.
010313*    05  WS-MEDI-LOCKED-SW           PIC X.
010313*        88  WS-MEDI-LOCKED                    VALUE 'Y'.
010313*        88  WS-MEDI-LOCKED-NO                 VALUE 'N'.
      *
      *01  WS-COUNTERS.
      *    05  WS-RECORDS-IN-APP           PIC 9(07)  VALUE ZEROS.
      *    05  WS-RECORDS-IN-COMPANY       PIC 9(07)  VALUE ZEROS.
      *    05  WS-APEX-IN-COMPANY          PIC 9(07)  VALUE ZEROS.
      *    05  WS-RECORDS-IN-TOTAL         PIC 9(07)  VALUE ZEROS.
      *    05  WS-APEX-IN-TOTAL            PIC 9(07)  VALUE ZEROS.
      *    05  WS-PARM-CARD-COUNTER        PIC 9(07)  VALUE ZEROS.
      *    05  WS-COMPANY-CNTR             PIC 9(07)  VALUE ZEROS.
      *
      *01  WS-SWITCHES.
      *    05  WS-FATAL-ERROR-SW             PIC X.
      *        88  WS-FATAL-ERROR            VALUE 'Y'.
      *    05  WS-PROGRAM-ID-FOUND-SW        PIC X.
      *        88  WS-PROGRAM-ID-FOUND       VALUE 'Y'.
      *    05  WS-COMPANY-FOUND-SW           PIC X.
      *        88  WS-COMPANY-FOUND          VALUE 'Y'.
      *    05  WS-RUN-DATE-FOUND-SW          PIC X.
      *        88  WS-RUN-DATE-FOUND         VALUE 'Y'.
      *    05  WS-PROCESS-DATE-FOUND-SW      PIC X.
      *        88  WS-PROCESS-DATE-FOUND     VALUE 'Y'.
      *    05  WS-ALL-COMPANY-CARDS-READ-SW  PIC X.
      *        88  WS-ALL-COMPANY-CARDS-READ VALUE 'Y'.
      *    05  WS-SWITCHES-EMBED.
      *        10  WS-SW-EMBED-WP            PIC X(01).
      *        10  WS-SW-EMBED-AD            PIC X(01).
      *
      *01  WS-PI-FIELDS                   PIC X(20).
      *    88  WS-SKIP-PI-FIELDS      VALUE 'APPNO               ',
      *                                     'CLIENT_ID           ',
      *                                     'POLICY              ',
      *                                     'POLICY_TYPE         '.
      *
010313*01  WS-CONSTANTS.
010313*    05  WS-MAX-AGENTS                PIC S9(04) COMP VALUE +3.
      *
      ****************************************************************
      * COMMON COPYBOOKS                                             *
      ****************************************************************
      *
      *COPY XCWTFCMD.
      *
      *COPY XCWWWKDT.
      *
      *****************************************************************
      * CALLED MODULE PARAMETER INFORMATION                           *
      *****************************************************************
      *
      *COPY ACWL2130.
      *COPY ACWLAPUP.
      *COPY ACWLCLIB.
      *COPY ACWLCLIO.
      *COPY ACWLCVG.
      *COPY ACWLPOL.
015508*COPY ACWLCLI.
      *
      *COPY ACWTUFCT.
      *
      *COPY ACWWAPIN.
APEX54*COPY ACWWI570.
APEX54*COPY ACWWI953.
      *
APEX54*COPY CCWL0953.
      *
      *COPY CCWL0066.
      *
APEX54*COPY CCWL0083.
      *
APEX54*COPY NCWL0302.
      *
      *COPY CCWL0183.
557700*COPY CCWL0840.
APEX53*COPY CCWL0832.
      *COPY CCWL2430.
      *
APEX54*COPY NCWL2437.
      *
      *COPY CCWL2800.
APEX53*COPY CCWL5120.
APEX54*COPY NCWL5850.
      *
APEX54*COPY CCWL6180.
      *
557700*COPY XCWL0015.
557700*COPY CCWL0620.
      *
      *COPY CCWWCVGS.
      *
      *COPY NCWL0080.
      *
APEX54*COPY NCWL0301.
APEX54*COPY CCWL0570.
      *
APEX54*COPY CCWL6060.
APEX54*COPY CCWL5950.
      *
      *COPY NCWL0760.
      *COPY NCWL0960.
      *
012148*COPY CCWL8240.
012148*
      *COPY NCWWPARM.
      *
APEX54*COPY SCWLSEGF.
APEX54*COPY SCWL0500.
      *
557698*COPY XCWL0005.
      *COPY XCWL0040.
008455*COPY XCWL0290.
      *COPY XCWL0280.
      *COPY XCWL1640.
014178*COPY XCWL1660.
      *COPY XCWL1670.
      *COPY XCWL1680.
      *COPY XCWLDTLK.
      *
      *****************************************************************
      * I/O COPYBOOKS                                                 *
      *****************************************************************
      *
      * APEX UPLOAD FILES
      *
      *COPY ACFWUFLD.
      *COPY ACFRUFLD.
      *
      *COPY ACFWUTTB.
      *COPY ACFRUTTB.
      *
      *COPY ACSW2100.
      *COPY ACSR2100.
      *
      *COPY ACSW2110.
      *COPY ACSR2110.
      *
      *COPY ACSW2120.
      *COPY ACSR2120.
      *
      *
      * ADMIN FILES
      *
APEX53*COPY CCFWBENC.
APEX53*COPY CCFWBENE.
APEX53*COPY CCFRBENE.
      *
      *COPY CCFWBNKA.
      *COPY CCFRBNKA.
      *
      *COPY CCFWBNKB.
      *COPY CCFRBNKB.
      *
015543*COPY CCFWCLIE.
      *COPY CCFWCLI.
      *COPY CCFRCLI.
      *COPY CCFHCLI.
      *
      *COPY CCFWCLIA.
      *COPY CCFRCLIA.
      *
      *COPY CCFWCLIB.
      *COPY CCFRCLIB.
      *
557700*COPY CCFWCLIC.
557700*COPY CCFRCLIC.
      *
557700*COPY CCFWCLII.
557700*COPY CCFRCLII.
      *
      *COPY CCFWCLIN.
      *
015508*COPY CCFWCLNC.
015508*COPY CCFRCLNC.
      *
015508*COPY CCFWCLNM.
015508*COPY CCFRCLNM.
      *
      *COPY CCFWCVG.
      *COPY CCFRCVG.
      *
      *COPY CCFRCVGA.
      *
      *COPY CCFWCVGC.
      *COPY CCFRCVGC.
      *
      *COPY CCFWEDIT.
      *COPY CCFREDIT.
      *
      *COPY CCFWEHST.
      *COPY CCFREHST.
      *
      *COPY CCFWIR.
      *COPY CCFRIR.
      *
      *COPY CCFWLI.
      *COPY CCFRLI.
      *
      *COPY CCFWPACH.
      *COPY CCFRPACH.
      *
      *COPY CCFWPACK.
      *COPY CCFRPACK.
      *
      *COPY CCFWPCOM.
      *COPY CCFRPCOM.
      *
007678*COPY CCFWSCOM.
007678*COPY CCFRSCOM.
007678*
555288*COPY CCFWPSYS.
555288*COPY CCFRPSYS.
      *
      *COPY CCFWPD.
      *COPY CCFRPD.
      *
      *COPY CCFWPH.
      *COPY CCFRPH.
      *
      *COPY CCFWPOL.
      *COPY CCFRPOL.
      *COPY CCFHPOL.
      *
      *COPY CCFWPOLC.
      *COPY CCFRPOLC.
      *
010302*COPY CCFWPR.
010302*COPY CCFRPR.
      *
015508*COPY CCFWPRNM.
015508*COPY CCFRPRNM.
      *
      *COPY CCFWRH.
      *COPY CCFRRH.
      *
      *COPY CCFWRT.
557660*COPY CCFRRT.
      *
      *COPY CCFWRL.
      *COPY CCFRRL.
      *
      *COPY CCFRQT.
      *
      *COPY CCFWUV.
      *COPY CCFRUV.
      *
      *COPY SCFWFC.
      *COPY SCFRFC.
      *
      *COPY SCFWFH.
      *COPY SCFRFH.
      *
      *COPY SCFWFR.
      *COPY SCFRFR.
      *
      *COPY SCFWFS.
      *COPY SCFRFS.
      *
      *COPY SCFWFX.
      *COPY SCFRFX.
      *
014178*COPY CCFRCAIN.
014178*COPY CCFWCAIN.
014178*COPY CCFRCDSI.
014178*COPY CCFWCDSI.
014178*COPY CCFWPOLP.
014178*COPY CCFRPOLP.
014178*
      *
      * NBS FILES
      *
      *COPY NCFWAPPF.
      *COPY NCFRAPPF.
      *
      *COPY NCFWAPPV.
      *COPY NCFRAPPV.
      *
      *COPY NCFWMEDI.
      *COPY NCFRMEDI.
      *
      *COPY NCFRREQT.
      *
      *COPY NCFRRTAB.
      *
      *COPY NCFWUCON.
      *COPY NCFRUCON.
      *
      *
      * TPI FILES
      *
      *COPY XCSWBCF.
      *COPY XCSRBCF.
      *
      *COPY XCSWOCF.
      *COPY XCSROCF.
      *
      *COPY XCFWUSEC.
      *COPY XCFRUSEC.
      *
      *****************************************************************
      * INPUT PARAMETER INFORMATION                                   *
      *****************************************************************
      *
      *01  WGLOB-GLOBAL-AREA.
      *COPY XCWWGLOB.
      *
      *COPY CCWLPGA.
      *
      *PROCEDURE DIVISION.
      *
      *--------------
      *0000-MAINLINE.
      *--------------
      *
      *    PERFORM  0100-OPEN-FILES
      *        THRU 0100-OPEN-FILES-X.
      *
      *    PERFORM  0200-INITIALIZE
      *        THRU 0200-INITIALIZE-X.
      *
      *    PERFORM  1000-PROCESS-TRANSACTIONS
      *        THRU 1000-PROCESS-TRANSACTIONS-X.
      *
      *    PERFORM  7500-PRINT-GRAND-TOTALS
      *        THRU 7500-PRINT-GRAND-TOTALS-X.
      *
      *    PERFORM  9999-CLOSE-FILES
      *        THRU 9999-CLOSE-FILES-X.
      *
      *    STOP RUN.
      *
      *0000-MAINLINE-X.
      *    EXIT.
      *
      *----------------
      *0100-OPEN-FILES.
      *----------------
      *
      * THESE FILES ARE REQUIRED IN ALL BATCH PROGRAMS THAT REQUIRE
      * CONTROL CARDS (I.E. EXCLUDING THE INITIALIZATION PROGRAMS)
      *
      *    PERFORM  OCF-3000-OPEN-OUTPUT
      *        THRU OCF-3000-OPEN-OUTPUT-X.
      *
      *    PERFORM  BCF-1000-OPEN-INPUT
      *        THRU BCF-1000-OPEN-INPUT-X.
      *
      * THESE FILES ARE SPECIFIC TO THIS PROGRAM
      *
      *    PERFORM  2100-1000-OPEN-INPUT
      *        THRU 2100-1000-OPEN-INPUT-X.
      *
      *    PERFORM  2110-3000-OPEN-OUTPUT
      *        THRU 2110-3000-OPEN-OUTPUT-X.
      *
      *    PERFORM  2120-3000-OPEN-OUTPUT
      *        THRU 2120-3000-OPEN-OUTPUT-X.
      *
      *0100-OPEN-FILES-X.
      *    EXIT.
      *
      *----------------
      *0200-INITIALIZE.
      *----------------
      *
APEX53*    MOVE WPGWS-CRNT-PGM-ID      TO L0960-PROGRAM-ID.
      *    MOVE SPACES                 TO L0960-COMPANY-CODE.
      *
      *    PERFORM  0960-2000-INIT-DEFAULT
      *        THRU 0960-2000-INIT-DEFAULT-X.
      *
      *    MOVE WS-USER-ID             TO WGLOB-USER-ID.
      *
557660*    INITIALIZE LSEGF-PARM-INFO.
      *
      *    PERFORM  8000-INIT-TITLES
      *        THRU 8000-INIT-TITLES-X.
      *
      *    PERFORM  PGA-1000-BUILD-PARMS
      *        THRU PGA-1000-BUILD-PARMS-X.
      *
      *0200-INITIALIZE-X.
      *    EXIT.
      *
      *--------------------------
      *1000-PROCESS-TRANSACTIONS.
      *--------------------------
      *
      * PROCESS TRANSACTIONS - EDIT CONTROL CARDS AND, IF NO ERRORS
      * ARE FOUND, PROCESS THE TRANS FILE FOR EACH COMPANY INPUT
      *
      *    PERFORM  1100-EDIT-CONTROL-CARDS
      *        THRU 1100-EDIT-CONTROL-CARDS-X.
      *
      *    IF  WS-FATAL-ERROR
      *        GO TO 1000-PROCESS-TRANSACTIONS-X
      *    END-IF.
      *
      * REPOSITION CONTROL CARD FILE AT THE BEGINNING
      *
      *    PERFORM  BCF-4000-CLOSE
      *        THRU BCF-4000-CLOSE-X.
      *
      *    PERFORM  BCF-1000-OPEN-INPUT
      *        THRU BCF-1000-OPEN-INPUT-X.
      *
      *    MOVE ZERO TO WS-PARM-CARD-COUNTER.
      *
      * BYPASS THE PROGRAM ID CARD
      *
      *    PERFORM  BCF-1000-READ
      *        THRU BCF-1000-READ-X.
      *
      * PROCESS THE PARM FILE FOR EACH COMPANY
      *
      *    PERFORM  8200-READ-CONTROL-CARD
      *        THRU 8200-READ-CONTROL-CARD-X.
      *
      *    PERFORM  2000-PROCESS-COMPANY
      *        THRU 2000-PROCESS-COMPANY-X
007684*        UNTIL WBCF-SEQ-IO-EOF.
      *
      *1000-PROCESS-TRANSACTIONS-X.
      *    EXIT.
      *
      *------------------------
      *1100-EDIT-CONTROL-CARDS.
      *------------------------
      *
      * PERFORM INITIAL EDITS ON CONTROL CARD FILE, THEN LOOP THRU
      * FILE AND EDIT EACH CARD
      *
      *    MOVE 'N'                    TO WS-FATAL-ERROR-SW.
      *    MOVE 'N'                    TO WS-COMPANY-FOUND-SW.
      *    MOVE 'N'                    TO WS-PROGRAM-ID-FOUND-SW.
      *    MOVE 'N'                    TO WS-RUN-DATE-FOUND-SW.
      *    MOVE 'N'                    TO WS-PROCESS-DATE-FOUND-SW.
      *
      *    PERFORM  8200-READ-CONTROL-CARD
      *        THRU 8200-READ-CONTROL-CARD-X.
      *
007684*    IF  WBCF-SEQ-IO-EOF
APEX54*        SET WS-FATAL-ERROR      TO TRUE
      *MSG: CONTROL CARD FILE EMPTY, NO PROCESSING DONE
      *        MOVE 'XS00000151'       TO WGLOB-MSG-REF-INFO
      *        PERFORM  0260-1000-GENERATE-MESSAGE
      *            THRU 0260-1000-GENERATE-MESSAGE-X
      *        GO TO 1100-EDIT-CONTROL-CARDS-X
      *    END-IF.
      *
007684*    MOVE RBCF-SEQ-REC-INFO      TO WPARM-CARD-AREA.
      *
      *    IF  NOT WPARM-PROGRAM-ID
APEX54*        SET WS-FATAL-ERROR      TO TRUE
      *MSG: FIRST CONTROL CARD MUST BE PROGRAM ID CARD
      *        MOVE 'XS00000147'       TO WGLOB-MSG-REF-INFO
      *        PERFORM  0260-1000-GENERATE-MESSAGE
      *            THRU 0260-1000-GENERATE-MESSAGE-X
      *    END-IF.
      *
      *    PERFORM  1110-EDIT-CONTROL-CARD
      *        THRU 1110-EDIT-CONTROL-CARD-X
007684*        UNTIL WBCF-SEQ-IO-EOF.
      *
      *    IF  NOT WS-PROGRAM-ID-FOUND
APEX54*        SET WS-FATAL-ERROR      TO TRUE
      *MSG: MISSING PROGRAM ID CARD
      *        MOVE 'XS00000122'       TO WGLOB-MSG-REF-INFO
      *        PERFORM  0260-1000-GENERATE-MESSAGE
      *            THRU 0260-1000-GENERATE-MESSAGE-X
      *    END-IF.
      *
      *    IF  NOT WS-COMPANY-FOUND
APEX54*        SET WS-FATAL-ERROR      TO TRUE
      *MSG: COMPANY CONTROL CARD MISSING
      *        MOVE 'XS00000134'       TO WGLOB-MSG-REF-INFO
      *        PERFORM  0260-1000-GENERATE-MESSAGE
      *            THRU 0260-1000-GENERATE-MESSAGE-X
      *    END-IF.
      *
      *    IF  WGLOB-SYSTEM-DATE-INT < WGLOB-PROCESS-DATE
APEX54*        SET WS-FATAL-ERROR      TO TRUE
      *MSG: SYSTEM DATE MUST BE >= PROCESS DATE
      *        MOVE 'AS21000041'       TO WGLOB-MSG-REF-INFO
      *        PERFORM  0260-1000-GENERATE-MESSAGE
      *            THRU 0260-1000-GENERATE-MESSAGE-X
      *    END-IF.
      *
      *
      * PRINT CONTROL CARD TOTALS
      *
      *MSG: TOTAL NUMBER OF PARM CARDS READ @1
      *    MOVE 'XS00000142'           TO WGLOB-MSG-REF-INFO.
      *    MOVE WS-PARM-CARD-COUNTER   TO WS-PIC-COUNTER.
      *    MOVE WS-PIC-COUNTER         TO WGLOB-MSG-PARM (1).
      *
      *    PERFORM  0260-2000-GET-MESSAGE
      *        THRU 0260-2000-GET-MESSAGE-X.
      *
      *    MOVE WGLOB-MSG-TXT          TO L0040-INPUT-LINE.
      *
      *    PERFORM  0040-3000-WRITE-OTHER
      *        THRU 0040-3000-WRITE-OTHER-X.
      *
      *    PERFORM  0040-4000-WRITE-ERROR-TOTAL
      *        THRU 0040-4000-WRITE-ERROR-TOTAL-X.
      *
      *1100-EDIT-CONTROL-CARDS-X.
      *    EXIT.
      *
      *-----------------------
      *1110-EDIT-CONTROL-CARD.
      *-----------------------
      *
      * EDIT AN INDIVIDUAL CONTROL CARD
      *
      *
      *
      * MOVE THE CONTROL CARD TO A PARAMETER LAYOUT
      *
007684*    MOVE RBCF-SEQ-REC-INFO      TO WPARM-CARD-AREA.
      *
      *
      * PRINT CONTROL CARD
      *
007684*    MOVE RBCF-SEQ-REC-INFO      TO L0040-INPUT-LINE.
      *
      *    PERFORM  0040-3000-WRITE-OTHER
      *        THRU 0040-3000-WRITE-OTHER-X.
      *
APEX54*    EVALUATE TRUE
      *
APEX54*        WHEN WPARM-PROGRAM-ID
      *             PERFORM  1111-EDIT-PROGRAM-ID
      *                 THRU 1111-EDIT-PROGRAM-ID-X
      *
APEX54*        WHEN WPARM-COMPANY-CODE
      *             PERFORM  1112-EDIT-COMPANY-CODE
      *                 THRU 1112-EDIT-COMPANY-CODE-X
      *
APEX54*        WHEN WPARM-RUN-DATE
      *             PERFORM  1114-EDIT-RUN-DATE
      *                 THRU 1114-EDIT-RUN-DATE-X
      *
APEX54*        WHEN WPARM-PROCESS-DATE
      *             PERFORM  1115-EDIT-PROCESS-DATE
      *                 THRU 1115-EDIT-PROCESS-DATE-X
      *
APEX54*        WHEN OTHER
APEX54*             SET WS-FATAL-ERROR TO TRUE
      *MSG: INVALID PARM CARD TYPE
      *             MOVE 'XS00000152'  TO WGLOB-MSG-REF-INFO
      *             PERFORM  0260-1000-GENERATE-MESSAGE
      *                 THRU 0260-1000-GENERATE-MESSAGE-X
      *
APEX54*    END-EVALUATE.
      *
      *    PERFORM  8200-READ-CONTROL-CARD
      *        THRU 8200-READ-CONTROL-CARD-X.
      *
      *1110-EDIT-CONTROL-CARD-X.
      *    EXIT.
      *
      *---------------------
      *1111-EDIT-PROGRAM-ID.
      *---------------------
      *
      * EDIT PROGRAM ID AGAINST HARD CODED VALUE AND ENSURE THAT
      * THERE IS ONLY ONE
      *
      *
      *    IF  WS-PROGRAM-ID-FOUND
APEX54*        SET WS-FATAL-ERROR      TO TRUE
      *MSG: MUST ONLY HAVE ONE PROGRAM ID CARD
      *        MOVE 'XS00000162'       TO WGLOB-MSG-REF-INFO
      *        PERFORM  0260-1000-GENERATE-MESSAGE
      *            THRU 0260-1000-GENERATE-MESSAGE-X
      *    END-IF.
      *
APEX54*    SET WS-PROGRAM-ID-FOUND     TO TRUE.
      *
APEX53*    IF  WPARM-VALUE = WPGWS-CRNT-PGM-ID
      *        NEXT SENTENCE
      *    ELSE
APEX54*        SET WS-FATAL-ERROR      TO TRUE
      *MSG: INVALID PROGRAM ID CARD
      *        MOVE 'XS00000121'       TO WGLOB-MSG-REF-INFO
      *        PERFORM  0260-1000-GENERATE-MESSAGE
      *            THRU 0260-1000-GENERATE-MESSAGE-X
      *    END-IF.
      *
      *1111-EDIT-PROGRAM-ID-X.
      *    EXIT.
      *
      *-----------------------
      *1112-EDIT-COMPANY-CODE.
      *-----------------------
      *
      * EDIT COMPANY CODE CARD DEPENDING ON WHETHER PCOM
      *
      *
      *    MOVE 'N'                    TO WS-RUN-DATE-FOUND-SW.
      *
      *    IF  WPCOM-COMPANY-NOT-REQUIRED
      *        IF  WS-COMPANY-FOUND
APEX54*            SET WS-FATAL-ERROR  TO TRUE
      *MSG: NO MULTI-COMPANY-CAN ONLY HAVE 1 COMPANY CARD
      *            MOVE 'XS00000163'   TO WGLOB-MSG-REF-INFO
      *            PERFORM  0260-1000-GENERATE-MESSAGE
      *                THRU 0260-1000-GENERATE-MESSAGE-X
      *        END-IF
      *    END-IF.
      *
APEX54*    SET WS-COMPANY-FOUND        TO TRUE.
      *
      *    IF  WPCOM-COMPANY-NOT-REQUIRED
      *        IF  WPARM-VALUE = SPACES
      *            GO TO 1112-EDIT-COMPANY-CODE-X
      *        ELSE
APEX54*            SET WS-FATAL-ERROR  TO TRUE
      *MSG: COMPANY CODE MUST EQUAL SPACES-PCOM MULTI-COMP OFF
      *            MOVE 'XS00000148'   TO WGLOB-MSG-REF-INFO
      *            PERFORM  0260-1000-GENERATE-MESSAGE
      *                THRU 0260-1000-GENERATE-MESSAGE-X
      *            GO TO 1112-EDIT-COMPANY-CODE-X
      *        END-IF
      *    END-IF.
      *
      *    MOVE WGLOB-COMPANY-CODE     TO WS-COMPANY-CODE.
      *
      *    MOVE WPARM-VALUE            TO WGLOB-COMPANY-CODE.
      *
      *    PERFORM  PCOM-1000-READ
      *        THRU PCOM-1000-READ-X.
      *
      *    MOVE WS-COMPANY-CODE        TO WGLOB-COMPANY-CODE.
      *
      *    IF  NOT WPCOM-IO-OK
APEX54*        SET WS-FATAL-ERROR      TO TRUE
      *MSG: COMPANY CODE INVALID, NO PCOM RECORD EXISTS
      *        MOVE 'XS00000149'       TO WGLOB-MSG-REF-INFO
      *        PERFORM  0260-1000-GENERATE-MESSAGE
      *            THRU 0260-1000-GENERATE-MESSAGE-X
      *    END-IF.
      *
      *1112-EDIT-COMPANY-CODE-X.
      *    EXIT.
      *
      *-------------------
      *1114-EDIT-RUN-DATE.
      *-------------------
      *
      * EDIT RUN DATE AND ENSURE THAT THERE IS ONLY ONE PER COMPANY
      *
      *
      *    IF  NOT WS-COMPANY-FOUND
APEX54*        SET WS-FATAL-ERROR      TO TRUE
      *MSG: COMPANY CARD MUST PRECEDE THIS DATA CARD
      *        MOVE 'XS00000175'       TO WGLOB-MSG-REF-INFO
      *        PERFORM  0260-1000-GENERATE-MESSAGE
      *            THRU 0260-1000-GENERATE-MESSAGE-X
      *    END-IF.
      *
      *    IF  WS-RUN-DATE-FOUND
APEX54*        SET WS-FATAL-ERROR      TO TRUE
      *MSG: MUST ONLY HAVE ONE RUN DATE CARD PER COMPANY
      *        MOVE 'XS00000164'       TO WGLOB-MSG-REF-INFO
      *        PERFORM  0260-1000-GENERATE-MESSAGE
      *            THRU 0260-1000-GENERATE-MESSAGE-X
      *    END-IF.
      *
APEX54*    SET WS-RUN-DATE-FOUND       TO TRUE.
      *
      *    MOVE WPARM-VALUE            TO L1640-EXTERNAL-DATE.
005409*    SET L1640-USE-LEAP-YEAR     TO TRUE.
      *
008453*    PERFORM  1640-3000-EXTERNAL-TO-INT
008453*        THRU 1640-3000-EXTERNAL-TO-INT-X.
008453*    PERFORM  1640-6000-CTL-CARD-TO-INT
008453*        THRU 1640-6000-CTL-CARD-TO-INT-X.
      *
      *    IF  L1640-NOT-VALID
APEX54*        SET WS-FATAL-ERROR      TO TRUE
008453*MSG: RUN DATE MUST BE A VALID DATE IN DDMMMYYYY FORMAT
008453*MSG: RUN DATE MUST BE IN VALID DATE FORMAT
      *        MOVE 'XS00000165'       TO WGLOB-MSG-REF-INFO
      *        PERFORM  0260-1000-GENERATE-MESSAGE
      *            THRU 0260-1000-GENERATE-MESSAGE-X
      *    END-IF.
      *
      *1114-EDIT-RUN-DATE-X.
      *    EXIT.
      *
      *-----------------------
      *1115-EDIT-PROCESS-DATE.
      *-----------------------
      *
      * EDIT PROCESS DATE AND ENSURE THAT THERE IS ONLY ONE PER
      * COMPANY.
      *
      *
      *    IF  NOT WS-COMPANY-FOUND
APEX54*        SET WS-FATAL-ERROR      TO TRUE
      *MSG: COMPANY CARD MUST PRECEDE THIS DATA CARD
      *        MOVE 'XS00000175'       TO WGLOB-MSG-REF-INFO
      *        PERFORM  0260-1000-GENERATE-MESSAGE
      *            THRU 0260-1000-GENERATE-MESSAGE-X
      *    END-IF.
      *
      *    IF  WS-PROCESS-DATE-FOUND
APEX54*        SET WS-FATAL-ERROR      TO TRUE
      *MSG: MUST ONLY HAVE ONE RUN DATE CARD PER COMPANY
      *        MOVE 'XS00000164'       TO WGLOB-MSG-REF-INFO
      *        PERFORM  0260-1000-GENERATE-MESSAGE
      *            THRU 0260-1000-GENERATE-MESSAGE-X
      *    END-IF.
      *
APEX54*    SET WS-PROCESS-DATE-FOUND   TO TRUE.
      *
      *    MOVE WPARM-VALUE            TO L1640-EXTERNAL-DATE.
005409*    SET L1640-USE-LEAP-YEAR     TO TRUE.
      *
008453*    PERFORM  1640-3000-EXTERNAL-TO-INT
008453*        THRU 1640-3000-EXTERNAL-TO-INT-X.
008453*    PERFORM  1640-6000-CTL-CARD-TO-INT
008453*        THRU 1640-6000-CTL-CARD-TO-INT-X.
      *
      *    IF  L1640-NOT-VALID
APEX54*        SET WS-FATAL-ERROR      TO TRUE
008453*MSG: INVALID PROCESS DATE - FORMAT MUST BE DDMMMYYYY
008453*MSG: INVALID PROCESS DATE
      *        MOVE 'XS00000159'       TO WGLOB-MSG-REF-INFO
      *        PERFORM  0260-1000-GENERATE-MESSAGE
      *            THRU 0260-1000-GENERATE-MESSAGE-X
      *    END-IF.
      *
      *1115-EDIT-PROCESS-DATE-X.
      *    EXIT.
      *
      *---------------------
      *2000-PROCESS-COMPANY.
      *---------------------
      *
      * FIRST CARD MUST BE COMPANY CARD
      *
007684*    MOVE RBCF-SEQ-REC-INFO      TO WPARM-CARD-AREA.
      *    MOVE WPARM-VALUE            TO WS-COMPANY-CODE.
      *
      * INITIALIZE GLOBAL AREA AND REPORTS FOR COMPANY
      *
      *    MOVE WS-COMPANY-CODE        TO L0960-COMPANY-CODE.
      *
      *    PERFORM  0960-3000-INIT-COMPANY
      *        THRU 0960-3000-INIT-COMPANY-X.
      *
008455*    PERFORM 0290-1000-BUILD-PARM-INFO
008455*       THRU 0290-1000-BUILD-PARM-INFO-X.
      *
      *    PERFORM  8000-INIT-TITLES
      *        THRU 8000-INIT-TITLES-X.
      *
      * GET THE REPORT HEADING
      *
      *
      *MSG: COMPANY RUN MESSAGES
      *    MOVE 'XS00000161'           TO WGLOB-MSG-REF-INFO.
      *
      *    PERFORM  0260-2000-GET-MESSAGE
      *        THRU 0260-2000-GET-MESSAGE-X.
      *
      *    MOVE WGLOB-MSG-TXT          TO L0040-HDG-LINE-3.
      *
      *    PERFORM  0040-1000-INIT-TITLE
      *        THRU 0040-1000-INIT-TITLE-X.
      *
      * PRINT COMPANY CONTROL CARDS
      *
      *    MOVE 'N'                    TO WS-ALL-COMPANY-CARDS-READ-SW.
      *
      *    PERFORM  2100-PRINT-PARM-CARD
      *        THRU 2100-PRINT-PARM-CARD-X
      *        UNTIL WS-ALL-COMPANY-CARDS-READ.
      *
      *    PERFORM  PCOM-1000-READ
      *        THRU PCOM-1000-READ-X.
      *
      *    IF  WPCOM-IO-OK
      *        MOVE RPCOM-CO-AUD-CTR-LOB-CD TO WS-PCOM-CO-AUD-CTR-LOB-CD
      *    ELSE
      *MSG: COMPANY PROFILE RECORD NOT FOUND (@1)
      *        MOVE 'AS21000050'       TO WGLOB-MSG-REF-INFO
      *        MOVE WGLOB-COMPANY-CODE TO WGLOB-MSG-PARM (1)
      *        PERFORM  0260-1000-GENERATE-MESSAGE
      *            THRU 0260-1000-GENERATE-MESSAGE-X
      *        PERFORM  0030-5000-LOGIC-ERROR
      *            THRU 0030-5000-LOGIC-ERROR-X
      *    END-IF.
      *
      *
      * PROCESS RECORDS FOR COMPANY
      *
      *    PERFORM  2400-PROCESS-COMPANY
      *        THRU 2400-PROCESS-COMPANY-X.
      *
      *2000-PROCESS-COMPANY-X.
      *    EXIT.
      *
      *---------------------
      *2100-PRINT-PARM-CARD.
      *---------------------
      *
      * IDENTIFY CARD TYPE, LOAD WORK VARIABLES, AND PRINT CARD
      *
007684*    MOVE RBCF-SEQ-REC-INFO       TO L0040-INPUT-LINE.
      *
      *    PERFORM  0040-3000-WRITE-OTHER
      *        THRU 0040-3000-WRITE-OTHER-X.
      *
      *    IF  WPARM-RUN-DATE
      *        MOVE WPARM-VALUE         TO L1640-EXTERNAL-DATE
005409*        SET L1640-USE-LEAP-YEAR  TO TRUE
008453*        PERFORM  1640-3000-EXTERNAL-TO-INT
008453*            THRU 1640-3000-EXTERNAL-TO-INT-X
008453*        PERFORM  1640-6000-CTL-CARD-TO-INT
008453*            THRU 1640-6000-CTL-CARD-TO-INT-X
      *        MOVE L1640-INTERNAL-DATE TO WGLOB-SYSTEM-DATE-INT
557658*        MOVE L1640-INTERNAL-DATE TO L1670-INTERNAL-DATE
557658*        SET L1670-USE-LEAP-YEAR  TO TRUE
557658*        PERFORM  1670-4000-INTERNAL-YYDDD
557658*            THRU 1670-4000-INTERNAL-YYDDD-X
557658*        MOVE L1670-JULIAN-DATE   TO WGLOB-SYSTEM-DATE
      *    END-IF.
      *
      *    IF  WPARM-PROCESS-DATE
      *        MOVE WPARM-VALUE         TO L1640-EXTERNAL-DATE
005409*        SET L1640-USE-LEAP-YEAR  TO TRUE
008453*        PERFORM  1640-3000-EXTERNAL-TO-INT
008453*            THRU 1640-3000-EXTERNAL-TO-INT-X
008453*        PERFORM  1640-6000-CTL-CARD-TO-INT
008453*            THRU 1640-6000-CTL-CARD-TO-INT-X
      *        MOVE L1640-INTERNAL-DATE TO WGLOB-PROCESS-DATE
      *    END-IF.
      *
      *    PERFORM  8200-READ-CONTROL-CARD
      *        THRU 8200-READ-CONTROL-CARD-X.
      *
007684*    IF  WBCF-SEQ-IO-EOF
APEX54*        SET WS-ALL-COMPANY-CARDS-READ     TO TRUE
      *    ELSE
007684*        MOVE RBCF-SEQ-REC-INFO            TO WPARM-CARD-AREA
      *        IF  WPARM-COMPANY-CODE
      *            SET WS-ALL-COMPANY-CARDS-READ TO TRUE
      *        END-IF
      *    END-IF.
      *
      *2100-PRINT-PARM-CARD-X.
      *    EXIT.
      *
      *---------------------
      *2400-PROCESS-COMPANY.
      *---------------------
      *
      *    PERFORM  7000-PRINT-PARM-TOTALS
      *        THRU 7000-PRINT-PARM-TOTALS-X.
      *
      *    IF  L0040-ERROR-CNT > ZERO
      *        GO TO 2400-PROCESS-COMPANY-X
      *    END-IF.
      *
557700*    PERFORM  8050-CHECK-UTTB-TABLE
557700*        THRU 8050-CHECK-UTTB-TABLE-X.
557700*    PERFORM  8070-CHECK-UFLD-TABLE
557700*        THRU 8070-CHECK-UFLD-TABLE-X.
      *
APEX53*    PERFORM 8100-INIT-FRENCH-UTTB-TABLE
APEX53*       THRU 8100-INIT-FRENCH-UTTB-TABLE-X.
      *
      *    MOVE ZERO                   TO WS-RECORDS-IN-APP.
      *
      *    PERFORM  APIN-0000-INIT-WORK-AREA
      *        THRU APIN-0000-INIT-WORK-AREA-X.
      *
      *    PERFORM  3000-PROCESS-EACH-APP
      *        THRU 3000-PROCESS-EACH-APP-X
      *        UNTIL WAPIN-END-OF-FILE.
      *
      *    PERFORM  7200-PRINT-COMPANY-TOTALS
      *        THRU 7200-PRINT-COMPANY-TOTALS-X.
      *
      *2400-PROCESS-COMPANY-X.
      *    EXIT.
      *
      *----------------------
      *3000-PROCESS-EACH-APP.
      *----------------------
      *
      *    MOVE 'N'                    TO WS-ERROR-SW.
      *
      *    PERFORM  APIN-4000-GET-NEXT-STRUCTURE
      *        THRU APIN-4000-GET-NEXT-STRUCTURE-X.
      *
      *    IF  WAPIN-STRUCT-ID = SPACE
      *        IF  WAPIN-END-OF-FILE
      *            GO TO 3000-PROCESS-EACH-APP-X
      *        ELSE
      *MSG: ERROR IN INPUT FILE
      *            MOVE 'AS21000057'   TO WGLOB-MSG-REF-INFO
      *            PERFORM  0260-1000-GENERATE-MESSAGE
      *                THRU 0260-1000-GENERATE-MESSAGE-X
      *            PERFORM  APIN-0000-INIT-WORK-AREA
      *                THRU APIN-0000-INIT-WORK-AREA-X
      *            GO TO 3000-PROCESS-EACH-APP-X
      *        END-IF
      *    END-IF.
      *
      *    IF  NOT WAPIN-STRUCT-APP-HEADER
      *MSG: APPLICATION MUST BEGIN WITH 'HEADER' STRUCTURE
      *        MOVE 'AS21000051'       TO WGLOB-MSG-REF-INFO
      *        PERFORM  0260-1000-GENERATE-MESSAGE
      *            THRU 0260-1000-GENERATE-MESSAGE-X
      *        PERFORM  3090-SKIP-TO-END-OF-APP
      *            THRU 3090-SKIP-TO-END-OF-APP-X
      *        GO TO 3000-PROCESS-EACH-APP-X
      *    END-IF.
      *
      *    PERFORM  3001-INIT-NEW-APP
      *        THRU 3001-INIT-NEW-APP-X.
      *
      *    PERFORM  3010-PROCESS-APP-HEADER
      *        THRU 3010-PROCESS-APP-HEADER-X
      *        UNTIL WAPIN-END-OF-STRUCTURE
      *        OR WS-ERROR-FOUND.
      *
      *    IF  WS-ERROR-FOUND
      *MSG: ERROR PROCESSING APPLICATION HEADER
      *        MOVE 'AS21000052'       TO WGLOB-MSG-REF-INFO
      *        PERFORM  0260-1000-GENERATE-MESSAGE
      *            THRU 0260-1000-GENERATE-MESSAGE-X
      *        PERFORM  3090-SKIP-TO-END-OF-APP
      *            THRU 3090-SKIP-TO-END-OF-APP-X
      *        GO TO 3000-PROCESS-EACH-APP-X
      *    END-IF.
      *
      *    PERFORM  APIN-4000-GET-NEXT-STRUCTURE
      *        THRU APIN-4000-GET-NEXT-STRUCTURE-X.
      *
      *    IF  WAPIN-ERROR-FOUND
      *MSG: ERROR READING NEXT SED AFTER APPLICATION HDR
      *        MOVE 'AS21000056'       TO WGLOB-MSG-REF-INFO
      *        PERFORM  0260-1000-GENERATE-MESSAGE
      *            THRU 0260-1000-GENERATE-MESSAGE-X
      *        PERFORM  3090-SKIP-TO-END-OF-APP
      *            THRU 3090-SKIP-TO-END-OF-APP-X
      *        GO TO 3000-PROCESS-EACH-APP-X
      *    END-IF.
      *
      *    PERFORM  3100-PROCESS-HEADER-SEDS
      *        THRU 3100-PROCESS-HEADER-SEDS-X
      *        UNTIL WAPIN-STRUCT-CLIENT-HEADER
      *        OR WS-ERROR-FOUND.
      *
      *    IF  WS-ERROR-FOUND
      *MSG: ERROR PROCESSING HEADER SEDS
      *        MOVE 'AS21000054'       TO WGLOB-MSG-REF-INFO
      *        PERFORM  0260-1000-GENERATE-MESSAGE
      *            THRU 0260-1000-GENERATE-MESSAGE-X
      *        PERFORM  3090-SKIP-TO-END-OF-APP
      *            THRU 3090-SKIP-TO-END-OF-APP-X
      *        GO TO 3000-PROCESS-EACH-APP-X
      *    END-IF.
      *
      *    PERFORM  4000-PROCESS-EACH-CLIENT
      *        THRU 4000-PROCESS-EACH-CLIENT-X
      *        UNTIL WAPIN-STRUCT-POLICY-HEADER
      *        OR WS-ERROR-FOUND.
      *
      *    IF  WS-ERROR-FOUND
      *MSG: ERROR PROCESSING CLIENT SEDS (@1)
      *        MOVE 'AS21000053'       TO WGLOB-MSG-REF-INFO
      *        MOVE RCLI-CLI-ID        TO WGLOB-MSG-PARM (1)
      *        PERFORM  0260-1000-GENERATE-MESSAGE
      *            THRU 0260-1000-GENERATE-MESSAGE-X
      *        PERFORM  3090-SKIP-TO-END-OF-APP
      *            THRU 3090-SKIP-TO-END-OF-APP-X
      *        GO TO 3000-PROCESS-EACH-APP-X
      *    END-IF.
      *
      *    MOVE CLI-SUB                TO WS-CLIENTS-IN-APP.
      *
      *    PERFORM  3300-PROCESS-OWNER-INFO
      *        THRU 3300-PROCESS-OWNER-INFO-X.
      *
      *    IF  WS-ERROR-FOUND
      *MSG: ERROR PROCESSING OWNER INFORMATION
      *        MOVE 'AS21000055'       TO WGLOB-MSG-REF-INFO
      *        PERFORM  0260-1000-GENERATE-MESSAGE
      *            THRU 0260-1000-GENERATE-MESSAGE-X
      *        PERFORM  3090-SKIP-TO-END-OF-APP
      *            THRU 3090-SKIP-TO-END-OF-APP-X
      *        GO TO 3000-PROCESS-EACH-APP-X
      *    END-IF.
      *
      *    PERFORM  5000-PROCESS-EACH-POLICY
      *        THRU 5000-PROCESS-EACH-POLICY-X
      *        UNTIL WAPIN-END-OF-APPLICATION
      *        OR WS-ERROR-FOUND.
      *
      *    IF  WS-ERROR-FOUND
      *MSG: ERROR PROCESSING POLICY SEDS
      *        MOVE 'AS21000064'       TO WGLOB-MSG-REF-INFO
      *        PERFORM  0260-1000-GENERATE-MESSAGE
      *            THRU 0260-1000-GENERATE-MESSAGE-X
      *        PERFORM  3090-SKIP-TO-END-OF-APP
      *            THRU 3090-SKIP-TO-END-OF-APP-X
      *        GO TO 3000-PROCESS-EACH-APP-X
      *    END-IF.
      *
      *    PERFORM  APIN-0000-INIT-WORK-AREA
      *        THRU APIN-0000-INIT-WORK-AREA-X.
      *
      *3000-PROCESS-EACH-APP-X.
      *    EXIT.
      *
      *------------------
      *3001-INIT-NEW-APP.
      *------------------
      *
      *    MOVE ZERO                 TO CLI-SUB.
      *    MOVE ZERO                 TO LAPUP-AGNT-CNT.
      *    MOVE ZERO                 TO WS-CLIENTS-IN-APP.
      *    MOVE ZERO                 TO WS-RECORDS-IN-APP.
      *    MOVE SPACE                TO WS-HOLD-POLICY-NUM.
      *    MOVE SPACE                TO WS-HOLD-LINE-OF-BUSINESS.
      *    MOVE ALL '0'              TO WS-CLI-ID-TABLE.
      *    MOVE SPACE                TO WS-SERV-AGT-ID.
      *    MOVE SPACE                TO WS-APPL-FORM-NO-X.
      *    MOVE SPACE                TO WS-POL-ISS-LOC-CD.
010313*    MOVE SPACE                TO WS-SBSDRY-CO-ID.
      *    MOVE SPACE                TO WS-POL-CTRY-CD.
      *    MOVE SPACE                TO LAPUP-CVGA-AGT-ID (1).
      *    MOVE ZERO                 TO LAPUP-CVGA-CVG-AGT-SHR-PCT (1).
      *    MOVE SPACE                TO LAPUP-CVGA-AGT-ID (2).
      *    MOVE ZERO                 TO LAPUP-CVGA-CVG-AGT-SHR-PCT (2).
      *    MOVE SPACE                TO LAPUP-CVGA-AGT-ID (3).
      *    MOVE ZERO                 TO LAPUP-CVGA-CVG-AGT-SHR-PCT (3).
      *    MOVE '1'                  TO LAPUP-LANG-CD.
      *    MOVE SPACE                TO LAPUP-CLI-TABLE.
      *
      *    ADD 1                     TO WS-APEX-IN-COMPANY.
      *
      *    PERFORM  3002-INIT-MESSAGE-ARRAY
      *        THRU 3002-INIT-MESSAGE-ARRAY-X
      *        VARYING X FROM 1 BY 1
      *        UNTIL X GREATER THAN 25.
      *
      *    PERFORM  3003-INIT-OWNER-ARRAY
      *        THRU 3003-INIT-OWNER-ARRAY-X
      *        VARYING X FROM 1 BY 1
      *        UNTIL X GREATER THAN 5.
      *
      *    MOVE ALL 'N'              TO TUFCT-SWITCHES.
      *    MOVE 'N'                  TO LAPUP-INVALID-PLAN-SW.
557659*    MOVE 'N'                  TO LPOL-POL-REPL-IND.
557659*    MOVE 'N'                  TO LPOL-POL-REPL-CD.
APEX53*    MOVE 'N'                  TO WS-POL-APP-SIGN-IND.
557659*    MOVE 'N'                  TO WS-POL-MIB-SIGN-IND.
557659*    MOVE 'N'                  TO WS-POL-MIB-SIGN-CD.
010313*    MOVE 'N'                  TO WS-AGT-EQUAL-SHARES-SW.
010313*    MOVE WWKDT-ZERO-DT        TO WS-POL-APP-SIGN-DT.
      *
      *
      * THE FOLLOWING FIELDS ARE INITIALIZED BECAUSE THEY APPEAR IN
      * MESSAGES, AND WE DON'T WANT TO CONFUSE THE USERS
      *
      *    MOVE SPACE                TO WPOL-POL-ID.
      *    MOVE SPACE                TO RPOL-SERV-BR-ID.
      *
      *3001-INIT-NEW-APP-X.
      *    EXIT.
      *
      *------------------------
      *3002-INIT-MESSAGE-ARRAY.
      *------------------------
      *
      *    MOVE 'N'                    TO WS-MESSAGE-SET-SW (X).
      *
      *3002-INIT-MESSAGE-ARRAY-X.
      *    EXIT.
      *
      *----------------------
      *3003-INIT-OWNER-ARRAY.
      *----------------------
      *
      *    MOVE SPACE                  TO LAPUP-POLC-OWN-REL-INFO (X).
      *
      *3003-INIT-OWNER-ARRAY-X.
      *    EXIT.
      *
      *------------------------
      *3010-PROCESS-APP-HEADER.
      *------------------------
      *
      *    PERFORM  APIN-3000-GET-FIELD-N-VALUE
      *        THRU APIN-3000-GET-FIELD-N-VALUE-X.
      *
      *    IF  WAPIN-ERROR-FOUND
      *    OR  WAPIN-FLD-ID = SPACE
      *MSG: ERROR
APEX54*        SET WS-ERROR-FOUND      TO TRUE
      *        GO TO 3010-PROCESS-APP-HEADER-X
      *    END-IF.
      *
      *    IF  WAPIN-FLD-ID = 'APP_PROD'
      *        MOVE 'N'                 TO L0280-SIGN-IND
APEX54*        SET L0280-SPACES-PERMITTED  TO TRUE
      *        MOVE 2                   TO L0280-LENGTH
      *        MOVE ZERO                TO L0280-PRECISION
      *        MOVE WAPIN-FLD-VALUE     TO L0280-INPUT-DATA
      *        PERFORM  0280-1000-NUMERIC-EDIT
      *            THRU 0280-1000-NUMERIC-EDIT-X
      *        IF  L0280-OK
      *        AND L0280-OUTPUT > ZERO
014177*        AND L0280-OUTPUT < 5
014177*        AND L0280-OUTPUT < 6
557245*            PERFORM  3011-APP-HEADER-VALUE
557245*                THRU 3011-APP-HEADER-VALUE-X
007678*            PERFORM  3012-APP-LINE-OF-BUSINESS
007678*                THRU 3012-APP-LINE-OF-BUSINESS-X
007678*            MOVE L0280-OUTPUT    TO WS-APP-PROD-CD
      *        ELSE
APEX54*            SET WS-ERROR-FOUND   TO TRUE
      *        END-IF
APEX54*        GO TO 3010-PROCESS-APP-HEADER-X
      *    END-IF.
      *
      *    IF  WAPIN-FLD-ID = 'APP_TYPE'
      *        MOVE 'N'                 TO L0280-SIGN-IND
APEX54*        SET L0280-SPACES-PERMITTED  TO TRUE
      *        MOVE 2                   TO L0280-LENGTH
      *        MOVE ZERO                TO L0280-PRECISION
      *        MOVE WAPIN-FLD-VALUE     TO L0280-INPUT-DATA
      *        PERFORM  0280-1000-NUMERIC-EDIT
      *            THRU 0280-1000-NUMERIC-EDIT-X
      *        IF  L0280-OK
      *        AND L0280-OUTPUT > ZERO
      *        AND L0280-OUTPUT < 4
      *            MOVE L0280-OUTPUT    TO WS-APP-TYP-CD
      *        ELSE
APEX54*            SET WS-ERROR-FOUND   TO TRUE
      *        END-IF
      *        GO TO 3010-PROCESS-APP-HEADER-X
      *    END-IF.
      *
      *    IF  WAPIN-FLD-ID = 'POLICY'
557698*        PERFORM  8400-TRANSLATE-UPPER-CASE-NA
557698*            THRU 8400-TRANSLATE-UPPER-CASE-NA-X
      *        MOVE WAPIN-FLD-VALUE     TO WS-HOLD-POLICY-NUM
      *        GO TO 3010-PROCESS-APP-HEADER-X
      *    END-IF.
      *
      *    IF  WAPIN-FLD-ID = 'NAME'
      *        MOVE WAPIN-FLD-VALUE     TO WS-INSURED-NAME
      *        GO TO 3010-PROCESS-APP-HEADER-X
      *    END-IF.
      *
      *    IF  WAPIN-FLD-ID = 'AGENT_CODE'
557698*        PERFORM  8400-TRANSLATE-UPPER-CASE-NA
557698*            THRU 8400-TRANSLATE-UPPER-CASE-NA-X
      *        MOVE WAPIN-FLD-VALUE     TO WS-SERV-AGT-ID
      *        GO TO 3010-PROCESS-APP-HEADER-X
      *    END-IF.
      *
      *    IF  WAPIN-FLD-ID = 'LANGUAGE'
      *        MOVE WAPIN-FLD-VALUE     TO LAPUP-LANG-CD
      *        GO TO 3010-PROCESS-APP-HEADER-X
      *    END-IF.
      *
557700*    IF  WAPIN-FLD-ID = 'SAVED'
557700*        MOVE WAPIN-FLD-VALUE        TO WS-APP-SAVE-DATE-X
557700*        MOVE WS-APP-SAVE-DATE-X-YR  TO WS-APP-SAVED-DATE-YR
557700*        MOVE WS-APP-SAVE-DATE-X-MO  TO WS-APP-SAVED-DATE-MO
557700*        MOVE WS-APP-SAVE-DATE-X-DY  TO WS-APP-SAVED-DATE-DY
557700*        GO TO 3010-PROCESS-APP-HEADER-X
557700*    END-IF.
      *
      *    IF  WAPIN-FLD-ID = 'APPNO'
      *    OR  WAPIN-FLD-ID = 'COMPANY_CODE'
557698*        PERFORM  8300-TRANSLATE-UPPER-CASE
557698*            THRU 8300-TRANSLATE-UPPER-CASE-X
557698*        GO TO 3010-PROCESS-APP-HEADER-X
557698*    END-IF.
      *
010313*    IF  WAPIN-FLD-ID = 'ISSUE_LOCATION'
010313*        MOVE 'LOCCD'            TO WUTTB-UPLD-TTBL-TYP-ID
010313*        MOVE WAPIN-FLD-VALUE    TO WUTTB-UPLD-TTBL-VALU-ID
010313*        PERFORM  UTTB-1000-LOOKUP-UTTB
010313*            THRU UTTB-1000-LOOKUP-UTTB-X
010313*        IF  WUTTB-IO-OK
010313*            MOVE RUTTB-UPLD-TTBL-VALU-TXT
010313*                               TO WS-POL-ISS-LOC-CD
010313*        ELSE
010313*            MOVE SPACE         TO WS-POL-ISS-LOC-CD
010313*        END-IF
010313*        GO TO 3010-PROCESS-APP-HEADER-X
010313*    END-IF.
      *
010313*    IF  WAPIN-FLD-ID = 'SUB_COMPANY'
010313*        MOVE 'SUBCO'            TO WUTTB-UPLD-TTBL-TYP-ID
010313*        MOVE WAPIN-FLD-VALUE    TO WUTTB-UPLD-TTBL-VALU-ID
010313*        PERFORM  UTTB-1000-LOOKUP-UTTB
010313*            THRU UTTB-1000-LOOKUP-UTTB-X
010313*        IF  WUTTB-IO-OK
010313*            MOVE RUTTB-UPLD-TTBL-VALU-TXT
010313*                               TO WS-SBSDRY-CO-ID
010313*        ELSE
010313*            MOVE SPACE         TO WS-SBSDRY-CO-ID
010313*        END-IF
010313*        GO TO 3010-PROCESS-APP-HEADER-X
010313*    END-IF.
      *
557698*    OR  WAPIN-FLD-ID = 'UW'
557698*    IF  WAPIN-FLD-ID = 'UW'
      *    OR  WAPIN-FLD-ID = 'BIRTH_DATE'
557700*    OR  WAPIN-FLD-ID = 'SAVED'
      *    OR  WAPIN-FLD-ID = 'CODE'
      *        GO TO 3010-PROCESS-APP-HEADER-X
      *    END-IF.
      *
APEX54*    SET WS-ERROR-FOUND           TO TRUE.
      *
      *3010-PROCESS-APP-HEADER-X.
      *    EXIT.
      *
557245*----------------------
557245*3011-APP-HEADER-VALUE.
557245*----------------------
557245*
557245*    MOVE L0280-OUTPUT           TO WS-APP-PROD-CD.
557245*
557245*    EVALUATE L0280-OUTPUT
557245*
557245*        WHEN 1
557245*             MOVE 'L'           TO LAPUP-POL-TYP-CD
557245*             SET LAPUP-POL-TYP-LIFE         TO TRUE
557245*
557245*        WHEN 2
557245*             MOVE 'D'           TO LAPUP-POL-TYP-CD
557245*             SET LAPUP-POL-TYP-DISABILITY   TO TRUE
557245*
557245*        WHEN 3
557245*             MOVE 'I'           TO LAPUP-POL-TYP-CD
557245*             SET LAPUP-POL-TYP-IMM-ANNUITY  TO TRUE
557245*
557245*        WHEN OTHER
557245*             MOVE 'A'           TO LAPUP-POL-TYP-CD
557245*             SET LAPUP-POL-TYP-DEF-ANNUITY  TO TRUE
557245*
557245*    END-EVALUATE.
557245*
557245*3011-APP-HEADER-VALUE-X.
557245*    EXIT.
      *
007678*--------------------------
007678*3012-APP-LINE-OF-BUSINESS.
007678*--------------------------
007678*
007678*    MOVE L0280-OUTPUT           TO WS-APP-PROD-CD.
007678*
007678*    EVALUATE L0280-OUTPUT
007678*
007678*        WHEN 1
007678*             SET WS-LIFE-POLICY             TO TRUE
007678*
007678*        WHEN 2
007678*             SET WS-DI-POLICY               TO TRUE
007678*
007678*        WHEN 3
007678*             SET WS-IMM-ANN-POLICY          TO TRUE
007678*
014177*        WHEN OTHER
014177*        WHEN 4
007678*             SET WS-DEF-ANN-POLICY          TO TRUE
014177*
014177*        WHEN 5
014177*             SET WS-DI-POLICY               TO TRUE
014177*
014177*        WHEN OTHER
014177*             MOVE SPACES TO WS-HOLD-LINE-OF-BUSINESS
007678*
007678*    END-EVALUATE.
007678*
007678*3012-APP-LINE-OF-BUSINESS-X.
007678*    EXIT.
      *
      *------------------------
      *3090-SKIP-TO-END-OF-APP.
      *------------------------
      *
      *    PERFORM  APIN-4000-GET-NEXT-STRUCTURE
      *        THRU APIN-4000-GET-NEXT-STRUCTURE-X
      *        UNTIL WAPIN-END-OF-APPLICATION
      *        OR WAPIN-END-OF-FILE.
      *
      *    PERFORM  APIN-0000-INIT-WORK-AREA
      *        THRU APIN-0000-INIT-WORK-AREA-X.
      *
      *3090-SKIP-TO-END-OF-APP-X.
      *    EXIT.
      *
      *-------------------------
      *3100-PROCESS-HEADER-SEDS.
      *-------------------------
      *
APEX54*    EVALUATE WAPIN-STRUCT-ID
      *
APEX54*        WHEN 'POLICY'
      *             PERFORM  3120-HEADER-POLICY-SED
      *                 THRU 3120-HEADER-POLICY-SED-X
      *                 UNTIL WAPIN-END-OF-STRUCTURE
      *                 OR WS-ERROR-FOUND
      *
APEX54*        WHEN 'NBSXMIT'
      *             PERFORM  3140-HEADER-NBSXMIT-SED
      *                 THRU 3140-HEADER-NBSXMIT-SED-X
      *                 UNTIL WAPIN-END-OF-STRUCTURE
      *                 OR WS-ERROR-FOUND
      *
APEX54*        WHEN 'AGENTCODE'
      *             PERFORM  3160-HEADER-AGENTCODE-SED
      *                 THRU 3160-HEADER-AGENTCODE-SED-X
      *                 UNTIL WAPIN-END-OF-STRUCTURE
      *                 OR WS-ERROR-FOUND
      *
APEX54*        WHEN 'SPLITCASE'
      *             PERFORM  3170-HEADER-SPLITCASE-SED
      *                 THRU 3170-HEADER-SPLITCASE-SED-X
      *                 UNTIL WAPIN-END-OF-STRUCTURE
      *                 OR WS-ERROR-FOUND
      *
010313*             IF WS-AGT-EQUAL-SHARES
010313*                 PERFORM  3190-SET-AGT-SHARE
010313*                     THRU 3190-SET-AGT-SHARE-X
010313*             END-IF
      *
010313*        WHEN 'SIGNATURE'
      *             PERFORM  3130-HEADER-SIGNATURE-SED
      *                 THRU 3130-HEADER-SIGNATURE-SED-X
      *                 UNTIL WAPIN-END-OF-STRUCTURE
      *                 OR WS-ERROR-FOUND
      *
APEX54*        WHEN 'AGENTRPT'
APEX54*        WHEN 'INITIALPREM'
APEX54*        WHEN 'AGENTRPTAI'
      *             PERFORM  3180-HEADER-GENERIC-SED
      *                 THRU 3180-HEADER-GENERIC-SED-X
      *                 UNTIL WAPIN-END-OF-STRUCTURE
      *                 OR WS-ERROR-FOUND
      *
APEX54*        WHEN 'PAYORBA'
APEX54*        WHEN 'OTHERBA'
010313*        WHEN 'ALTERNATEBA'
APEX54*             CONTINUE
      *
APEX54*        WHEN OTHER
      *MSG: UNKNOWN FIELD @1 FOR SED @2
      *             MOVE 'AS21000062'       TO WGLOB-MSG-REF-INFO
      *             MOVE WAPIN-FLD-ID       TO WGLOB-MSG-PARM (1)
      *             MOVE WAPIN-STRUCT-ID    TO WGLOB-MSG-PARM (2)
      *             PERFORM  0260-1000-GENERATE-MESSAGE
      *                 THRU 0260-1000-GENERATE-MESSAGE-X
APEX54*             SET WS-ERROR-FOUND      TO TRUE
      *             GO TO 3100-PROCESS-HEADER-SEDS-X
      *
APEX54*    END-EVALUATE.
      *
      *    PERFORM  APIN-4000-GET-NEXT-STRUCTURE
      *        THRU APIN-4000-GET-NEXT-STRUCTURE-X.
      *
      *3100-PROCESS-HEADER-SEDS-X.
      *    EXIT.
      *
      *-----------------------
      *3120-HEADER-POLICY-SED.
      *-----------------------
      *
      *    PERFORM  APIN-3000-GET-FIELD-N-VALUE
      *        THRU APIN-3000-GET-FIELD-N-VALUE-X.
      *
      *    IF  WAPIN-ERROR-FOUND
      *    OR  WAPIN-FLD-ID = SPACE
      *MSG: ERROR PROCESSING @1:@2 SED
      *        MOVE 'AS21000060'       TO WGLOB-MSG-REF-INFO
      *        MOVE WAPIN-FLD-ID       TO WGLOB-MSG-PARM (1)
      *        MOVE WAPIN-STRUCT-ID    TO WGLOB-MSG-PARM (2)
      *        PERFORM  0260-1000-GENERATE-MESSAGE
      *            THRU 0260-1000-GENERATE-MESSAGE-X
APEX54*        SET WS-ERROR-FOUND      TO TRUE
      *        GO TO 3120-HEADER-POLICY-SED-X
      *    END-IF.
      *
      *    IF  WAPIN-FLD-ID = 'APPNO'
      *        MOVE 'N'                 TO L0280-SIGN-IND
APEX54*        SET L0280-SPACES-PERMITTED  TO TRUE
      *        MOVE 15                  TO L0280-LENGTH
      *        MOVE ZERO                TO L0280-PRECISION
      *        MOVE WAPIN-FLD-VALUE     TO L0280-INPUT-DATA
      *        PERFORM  0280-1000-NUMERIC-EDIT
      *            THRU 0280-1000-NUMERIC-EDIT-X
      *        IF  L0280-OK
      *            MOVE L0280-OUTPUT    TO WS-APPL-FORM-NO
      *            GO TO 3120-HEADER-POLICY-SED-X
      *        ELSE
557698*            PERFORM  8300-TRANSLATE-UPPER-CASE
557698*                THRU 8300-TRANSLATE-UPPER-CASE-X
      *            MOVE WAPIN-FLD-VALUE TO WS-APPL-FORM-NO-X
      *            GO TO 3120-HEADER-POLICY-SED-X
      *        END-IF
      *    END-IF.
      *
      *    IF  WAPIN-FLD-ID = 'OWNER'
      *        IF  WAPIN-FLD-VALUE NOT = '0'
      *            MOVE WAPIN-FLD-VALUE TO LAPUP-POLC-OWN-CLI-ID (1)
      *            MOVE 'P'            TO LAPUP-POLC-OWN-TYP-CD (1)
      *            MOVE 'PR'           TO LAPUP-POLC-OWN-ADDR-TYP (1)
      *        END-IF
      *        GO TO 3120-HEADER-POLICY-SED-X
      *    END-IF.
      *
      *    IF  WAPIN-FLD-ID = 'SECOND_OWNER'
      *        IF  WAPIN-FLD-VALUE NOT = '0'
      *            MOVE WAPIN-FLD-VALUE TO LAPUP-POLC-OWN-CLI-ID (2)
      *            MOVE 'C'            TO LAPUP-POLC-OWN-TYP-CD (2)
      *            MOVE 'PR'           TO LAPUP-POLC-OWN-ADDR-TYP (2)
      *        END-IF
      *        GO TO 3120-HEADER-POLICY-SED-X
      *    END-IF.
      *
      *    IF  WAPIN-FLD-ID = 'RELOWNER'
      *        MOVE 'RELTI'            TO WUTTB-UPLD-TTBL-TYP-ID
      *        MOVE WAPIN-FLD-VALUE    TO WUTTB-UPLD-TTBL-VALU-ID
      *        PERFORM  UTTB-1000-LOOKUP-UTTB
      *            THRU UTTB-1000-LOOKUP-UTTB-X
      *        IF  WUTTB-IO-OK
      *            MOVE RUTTB-UPLD-TTBL-VALU-TXT
      *                               TO LAPUP-POLC-OWN-INSRD-REL-CD (1)
      *        ELSE
      *            MOVE SPACE         TO LAPUP-POLC-OWN-INSRD-REL-CD (1)
      *        END-IF
      *        GO TO 3120-HEADER-POLICY-SED-X
      *    END-IF.
      *
      *    IF  WAPIN-FLD-ID = 'RELSECOND'
      *        MOVE 'RELTI'            TO WUTTB-UPLD-TTBL-TYP-ID
      *        MOVE WAPIN-FLD-VALUE    TO WUTTB-UPLD-TTBL-VALU-ID
      *        PERFORM  UTTB-1000-LOOKUP-UTTB
      *            THRU UTTB-1000-LOOKUP-UTTB-X
      *        IF  WUTTB-IO-OK
      *            MOVE RUTTB-UPLD-TTBL-VALU-TXT
      *                               TO LAPUP-POLC-OWN-INSRD-REL-CD (2)
      *        ELSE
      *            MOVE SPACE         TO LAPUP-POLC-OWN-INSRD-REL-CD (2)
      *        END-IF
      *        GO TO 3120-HEADER-POLICY-SED-X
      *    END-IF.
      *
      *    IF  WAPIN-FLD-ID = 'POLICY'
557698*        PERFORM  8400-TRANSLATE-UPPER-CASE-NA
557698*            THRU 8400-TRANSLATE-UPPER-CASE-NA-X
557698*        GO TO 3120-HEADER-POLICY-SED-X
557698*    END-IF.
      *
557698*    OR  WAPIN-FLD-ID = 'HOME'
557698*    IF  WAPIN-FLD-ID = 'HOME'
      *    OR  WAPIN-FLD-ID = 'INSURED'
      *    OR  WAPIN-FLD-ID = 'OFFICE'
      *    OR  WAPIN-FLD-ID = 'BAOWNER'
      *    OR  WAPIN-FLD-ID = 'OTHER'
      *    OR  WAPIN-FLD-ID = 'BANK'
      *    OR  WAPIN-FLD-ID = 'PAYOR'
010313*    OR  WAPIN-FLD-ID = 'APPFORMNO'
010313*    OR  WAPIN-FLD-ID = 'ALTERNATE'
      *        GO TO 3120-HEADER-POLICY-SED-X
      *    END-IF.
      *
      *MSG: ERROR PROCESSING @1:@2 SED
      *    MOVE 'AS21000060'           TO WGLOB-MSG-REF-INFO.
      *    MOVE WAPIN-FLD-ID           TO WGLOB-MSG-PARM (1).
      *    MOVE WAPIN-STRUCT-ID        TO WGLOB-MSG-PARM (2).
      *    PERFORM  0260-1000-GENERATE-MESSAGE
      *        THRU 0260-1000-GENERATE-MESSAGE-X.
APEX54*    SET WS-ERROR-FOUND          TO TRUE.
      *
      *3120-HEADER-POLICY-SED-X.
      *    EXIT.
010313*
010313*--------------------------
010313*3130-HEADER-SIGNATURE-SED.
010313*--------------------------
010313*
010313*    PERFORM  APIN-3000-GET-FIELD-N-VALUE
010313*        THRU APIN-3000-GET-FIELD-N-VALUE-X.
010313*
010313*    IF  WAPIN-ERROR-FOUND
010313*    OR  WAPIN-FLD-ID = SPACE
010313*MSG: ERROR READING NEXT FIELD/VALUE ON SED (@1)
010313*        MOVE 'AS21000061'          TO WGLOB-MSG-REF-INFO
010313*        MOVE WAPIN-STRUCT-ID       TO WGLOB-MSG-PARM (1)
010313*        PERFORM  0260-1000-GENERATE-MESSAGE
010313*            THRU 0260-1000-GENERATE-MESSAGE-X
010313*        SET WS-ERROR-FOUND         TO TRUE
010313*        GO TO 3130-HEADER-SIGNATURE-SED-X
010313*    END-IF.
010313*
010313*    EVALUATE WAPIN-FLD-ID
010313*
010313*** SAVE MOST RECENT APP SIGN DATE FROM SIGNATURE SED
010313*        WHEN 'CLIENT_SIGNATURE1'
010313*        WHEN 'CLIENT_SIGNATURE2'
010313*        WHEN 'CLIENT_SIGNATURE3'
010313*        WHEN 'CLIENT_SIGNATURE4'
010313*        WHEN 'CLIENT_SIGNATURE5'
010313*        WHEN 'CLIENT_SIGNATURE6'
010313*        WHEN 'CLIENT_SIGNATURE7'
010313*        WHEN 'CLIENT_SIGNATURE8'
010313*        WHEN 'CLIENT_SIGNATURE9'
010313*        WHEN 'CLIENT_SIGNATURE10'
010313*            MOVE WAPIN-FLD-VALUE   TO WS-SIGNATURE-FLD
010313*            MOVE WWKDT-ZERO-DT     TO L1640-INTERNAL-DATE
010313*            MOVE WS-SIGNATURE-YR   TO L1640-YYYY-1
010313*            MOVE WS-SIGNATURE-MON  TO L1640-MM-1
010313*            MOVE WS-SIGNATURE-DAY  TO L1640-DD-1
010313*
010313*            IF  L1640-INTERNAL-DATE > WS-POL-APP-SIGN-DT
010313*                MOVE L1640-INTERNAL-DATE
010313*                                   TO WS-POL-APP-SIGN-DT
010313*            END-IF
010313*
010313*        WHEN OTHER
010313*MSG: UNKNOWN FIELD @1 FOR SED @2
010313*             MOVE 'AS21000062'     TO WGLOB-MSG-REF-INFO
010313*             MOVE WAPIN-FLD-ID     TO WGLOB-MSG-PARM (1)
010313*             MOVE WAPIN-STRUCT-ID  TO WGLOB-MSG-PARM (2)
010313*             PERFORM  0260-1000-GENERATE-MESSAGE
010313*                 THRU 0260-1000-GENERATE-MESSAGE-X
010313*             SET WS-ERROR-FOUND    TO TRUE
010313*
010313*    END-EVALUATE.
010313*
010313*3130-HEADER-SIGNATURE-SED-X.
010313*    EXIT.
      *
      *------------------------
      *3140-HEADER-NBSXMIT-SED.
      *------------------------
      *
      *    PERFORM  APIN-3000-GET-FIELD-N-VALUE
      *        THRU APIN-3000-GET-FIELD-N-VALUE-X.
      *
      *    IF  WAPIN-ERROR-FOUND
      *    OR  WAPIN-FLD-ID = SPACE
      *MSG: ERROR READING NEXT FIELD/VALUE ON SED (@1)
      *        MOVE 'AS21000061'       TO WGLOB-MSG-REF-INFO
      *        MOVE WAPIN-STRUCT-ID    TO WGLOB-MSG-PARM (1)
      *        PERFORM  0260-1000-GENERATE-MESSAGE
      *            THRU 0260-1000-GENERATE-MESSAGE-X
APEX54*        SET WS-ERROR-FOUND      TO TRUE
      *        GO TO 3140-HEADER-NBSXMIT-SED-X
      *    END-IF.
      *
APEX54*    EVALUATE WAPIN-FLD-ID
      *
APEX54*        WHEN 'APPLIC_SIGN'
      *             IF  WAPIN-FLD-VALUE = '1'
      *                 MOVE 'Y'          TO WS-POL-APP-SIGN-IND
      *             ELSE
      *                 MOVE 'N'          TO WS-POL-APP-SIGN-IND
      *             END-IF
      *
APEX54*        WHEN 'AUTHORIZE'
      *             IF  WAPIN-FLD-VALUE = '1'
557659*                 MOVE 'Y'          TO WS-POL-MIB-SIGN-IND
557659*                 MOVE 'Y'          TO WS-POL-MIB-SIGN-CD
      *             ELSE
557659*                 MOVE 'N'          TO WS-POL-MIB-SIGN-IND
557659*                 MOVE 'N'          TO WS-POL-MIB-SIGN-CD
      *             END-IF
      *
APEX54*        WHEN 'PAC'
APEX54*        WHEN 'CREDIT'
APEX54*        WHEN 'CIA'
APEX54*        WHEN 'AGENT_SIGN'
APEX54*        WHEN 'APPNO'
APEX54*        WHEN 'OWNER'
APEX54*             CONTINUE
      *
APEX54*        WHEN OTHER
      *MSG: UNKNOWN FIELD @1 FOR SED @2
      *             MOVE 'AS21000062'     TO WGLOB-MSG-REF-INFO
      *             MOVE WAPIN-FLD-ID     TO WGLOB-MSG-PARM (1)
      *             MOVE WAPIN-STRUCT-ID  TO WGLOB-MSG-PARM (2)
      *             PERFORM  0260-1000-GENERATE-MESSAGE
      *                 THRU 0260-1000-GENERATE-MESSAGE-X
APEX54*             SET WS-ERROR-FOUND    TO TRUE
      *
APEX54*    END-EVALUATE.
      *
      *3140-HEADER-NBSXMIT-SED-X.
      *    EXIT.
      *
      *--------------------------
      *3160-HEADER-AGENTCODE-SED.
      *--------------------------
      *
      *    PERFORM  APIN-3000-GET-FIELD-N-VALUE
      *        THRU APIN-3000-GET-FIELD-N-VALUE-X.
      *
      *    IF  WAPIN-ERROR-FOUND
      *    OR  WAPIN-FLD-ID = SPACE
      *MSG: ERROR READING NEXT FIELD/VALUE ON SED (@1)
      *        MOVE 'AS21000061'       TO WGLOB-MSG-REF-INFO
      *        MOVE WAPIN-STRUCT-ID    TO WGLOB-MSG-PARM (1)
      *        PERFORM  0260-1000-GENERATE-MESSAGE
      *            THRU 0260-1000-GENERATE-MESSAGE-X
APEX54*        SET WS-ERROR-FOUND      TO TRUE
      *        GO TO 3160-HEADER-AGENTCODE-SED-X
      *    END-IF.
      *
APEX54*    EVALUATE WAPIN-FLD-ID
      *
APEX54*        WHEN 'AGENT_CODE'
      *             IF  LAPUP-AGNT-CNT = ZERO
      *                 MOVE 1         TO LAPUP-AGNT-CNT
557698*                 PERFORM  8400-TRANSLATE-UPPER-CASE-NA
557698*                     THRU 8400-TRANSLATE-UPPER-CASE-NA-X
      *                 MOVE WAPIN-FLD-VALUE
      *                                TO LAPUP-CVGA-AGT-ID (1)
      *                 MOVE 100       TO LAPUP-CVGA-CVG-AGT-SHR-PCT(1)
      *             END-IF
      *
APEX54*        WHEN 'FIRST_NAME'
APEX54*        WHEN 'INITIAL'
APEX54*        WHEN 'LAST_NAME'
010313*        WHEN 'LICENSE_NUMBER'
APEX54*             CONTINUE
      *
APEX54*        WHEN OTHER
      *MSG: UNKNOWN FIELD @1 FOR SED @2
      *             MOVE 'AS21000062'  TO WGLOB-MSG-REF-INFO
      *             MOVE WAPIN-FLD-ID  TO WGLOB-MSG-PARM (1)
      *             MOVE WAPIN-STRUCT-ID  TO WGLOB-MSG-PARM (2)
      *             PERFORM  0260-1000-GENERATE-MESSAGE
      *                 THRU 0260-1000-GENERATE-MESSAGE-X
APEX54*             SET WS-ERROR-FOUND TO TRUE
      *
      *    END-EVALUATE.
      *
      *3160-HEADER-AGENTCODE-SED-X.
      *    EXIT.
      *
      *--------------------------
      *3170-HEADER-SPLITCASE-SED.
      *--------------------------
      *
      *    PERFORM  APIN-3000-GET-FIELD-N-VALUE
      *        THRU APIN-3000-GET-FIELD-N-VALUE-X.
      *
      *    IF  WAPIN-ERROR-FOUND
      *    OR  WAPIN-FLD-ID = SPACE
      *MSG: ERROR READING NEXT FIELD/VALUE ON SED (@1)
      *        MOVE 'AS21000061'       TO WGLOB-MSG-REF-INFO
      *        MOVE WAPIN-STRUCT-ID    TO WGLOB-MSG-PARM (1)
      *        PERFORM  0260-1000-GENERATE-MESSAGE
      *            THRU 0260-1000-GENERATE-MESSAGE-X
APEX54*        SET WS-ERROR-FOUND      TO TRUE
      *        GO TO 3170-HEADER-SPLITCASE-SED-X
      *    END-IF.
      *
APEX54*    EVALUATE WAPIN-FLD-ID
      *
APEX54***      WHEN 'CODE1'
010313*        WHEN 'AGENT_CODE1'
      *             MOVE 1               TO LAPUP-AGNT-CNT
557698*             PERFORM  8400-TRANSLATE-UPPER-CASE-NA
557698*                 THRU 8400-TRANSLATE-UPPER-CASE-NA-X
      *             MOVE WAPIN-FLD-VALUE TO LAPUP-CVGA-AGT-ID (1)
      *
APEX54***      WHEN 'CODE2'
010313*        WHEN 'AGENT_CODE2'
      *             MOVE 2               TO LAPUP-AGNT-CNT
557698*             PERFORM  8400-TRANSLATE-UPPER-CASE-NA
557698*                 THRU 8400-TRANSLATE-UPPER-CASE-NA-X
      *             MOVE WAPIN-FLD-VALUE TO LAPUP-CVGA-AGT-ID (2)
      *
APEX54***      WHEN 'CODE3'
010313*        WHEN 'AGENT_CODE3'
      *             MOVE 3               TO LAPUP-AGNT-CNT
557698*             PERFORM  8400-TRANSLATE-UPPER-CASE-NA
557698*                 THRU 8400-TRANSLATE-UPPER-CASE-NA-X
      *             MOVE WAPIN-FLD-VALUE TO LAPUP-CVGA-AGT-ID (3)
      *
APEX54***      WHEN 'PERCENT1'
APEX54***      WHEN 'PERCENT2'
APEX54***      WHEN 'PERCENT3'
010313*        WHEN 'SHARE1'
010313*        WHEN 'SHARE2'
010313*        WHEN 'SHARE3'
APEX54*             PERFORM  3171-HDR-SPLITCASE-PCNT
APEX54*                 THRU 3171-HDR-SPLITCASE-PCNT-X
      *
010313*        WHEN 'EQUAL1'
010313*        WHEN 'EQUAL2'
010313*        WHEN 'EQUAL3'
010313*             IF WAPIN-FLD-VALUE = '1'
010313*                 SET WS-AGT-EQUAL-SHARES TO TRUE
010313*             END-IF
010313*
010313***      WHEN 'FIRST_NAME1'
010313***      WHEN 'INITIAL1'
010313***      WHEN 'LAST_NAME1'
010313***      WHEN 'FIRST_NAME2'
010313***      WHEN 'INITIAL2'
010313***      WHEN 'LAST_NAME2'
010313***      WHEN 'FIRST_NAME3'
010313***      WHEN 'INITIAL3'
010313***      WHEN 'LAST_NAME3'
010313***      WHEN 'BRANCH_NAME'
010313***      WHEN 'BRANCH_NO'
010313***      WHEN 'BRANCH'
010313***      WHEN 'BR_NO'
010313*        WHEN 'PERCENT1'
010313*        WHEN 'PERCENT2'
010313*        WHEN 'PERCENT3'
      *            CONTINUE
      *
APEX54*        WHEN OTHER
      *MSG: UNKNOWN FIELD @1 FOR SED @2
      *             MOVE 'AS21000062'    TO WGLOB-MSG-REF-INFO
      *             MOVE WAPIN-FLD-ID    TO WGLOB-MSG-PARM (1)
      *             MOVE WAPIN-STRUCT-ID TO WGLOB-MSG-PARM (2)
      *             PERFORM  0260-1000-GENERATE-MESSAGE
      *                 THRU 0260-1000-GENERATE-MESSAGE-X
APEX54*             SET WS-ERROR-FOUND   TO TRUE
      *
APEX54*    END-EVALUATE.
      *
      *3170-HEADER-SPLITCASE-SED-X.
      *    EXIT.
      *
APEX54*------------------------
APEX54*3171-HDR-SPLITCASE-PCNT.
APEX54*------------------------
APEX54*
APEX54*    MOVE 'N'                    TO L0280-SIGN-IND.
APEX54*    SET L0280-SPACES-PERMITTED  TO TRUE.
APEX54*    MOVE 3                      TO L0280-LENGTH.
APEX54*    MOVE ZERO                   TO L0280-PRECISION.
APEX54*    MOVE WAPIN-FLD-VALUE        TO L0280-INPUT-DATA.
APEX54*
APEX54*    PERFORM  0280-1000-NUMERIC-EDIT
APEX54*        THRU 0280-1000-NUMERIC-EDIT-X.
APEX54*
APEX54*    IF  L0280-OK
APEX54*    AND L0280-OUTPUT < 101
APEX54*        EVALUATE WAPIN-FLD-ID
APEX54***          WHEN 'PERCENT1'
010313*            WHEN 'SHARE1'
APEX54*                 MOVE L0280-OUTPUT
APEX54*                                TO LAPUP-CVGA-CVG-AGT-SHR-PCT(1)
APEX54***          WHEN 'PERCENT2'
010313*            WHEN 'SHARE2'
APEX54*                 MOVE L0280-OUTPUT
APEX54*                                TO LAPUP-CVGA-CVG-AGT-SHR-PCT(2)
APEX54*            WHEN OTHER
APEX54*                 MOVE L0280-OUTPUT
APEX54*                                TO LAPUP-CVGA-CVG-AGT-SHR-PCT(3)
APEX54*        END-EVALUATE
APEX54*    ELSE
APEX54*        SET WS-ERROR-FOUND      TO TRUE
APEX54*    END-IF.
APEX54*
APEX54*3171-HDR-SPLITCASE-PCNT-X.
APEX54*    EXIT.
      *
      *------------------------
      *3180-HEADER-GENERIC-SED.
      *------------------------
      *
      *    PERFORM  APIN-3000-GET-FIELD-N-VALUE
      *        THRU APIN-3000-GET-FIELD-N-VALUE-X.
      *
      *    IF  WAPIN-ERROR-FOUND
      *    OR  WAPIN-FLD-ID = SPACE
      *MSG: ERROR READING NEXT FIELD/VALUE ON SED (@1)
      *        MOVE 'AS21000061'       TO WGLOB-MSG-REF-INFO
      *        MOVE WAPIN-STRUCT-ID    TO WGLOB-MSG-PARM (1)
      *        PERFORM  0260-1000-GENERATE-MESSAGE
      *            THRU 0260-1000-GENERATE-MESSAGE-X
APEX54*        SET WS-ERROR-FOUND      TO TRUE
      *    END-IF.
      *
      *3180-HEADER-GENERIC-SED-X.
      *    EXIT.
010313*
010313*-------------------
010313*3190-SET-AGT-SHARE.
010313*-------------------
010313*
010313*** AGENT SPLIT WAS SET TO EQUAL SHARES - CALCULATE SPLIT
010313*** PERCENTAGES BASED ON NUMBER OF AGENTS.NOTE THAT APEX WON'T
010313*** ALLOW BOTHEQUAL SHARES AND SPECIFIC PERCENTAGES TO BE ENTERED
010313*    IF LAPUP-AGNT-CNT = ZERO
010313*        GO TO 3190-SET-AGT-SHARE-X
010313*    END-IF.
010313*
010313*    COMPUTE WS-SHARE = 100 / LAPUP-AGNT-CNT.
010313*    MOVE ZERO TO WS-TOTAL-SHARE.
010313*    PERFORM
010313*        VARYING I FROM 1 BY 1
010313*        UNTIL I > WS-MAX-AGENTS
010313*
010313*        IF LAPUP-CVGA-AGT-ID (I) NOT = SPACE
010313*            MOVE WS-SHARE TO LAPUP-CVGA-CVG-AGT-SHR-PCT(I)
010313*            ADD WS-SHARE TO WS-TOTAL-SHARE
010313*        END-IF
010313*
010313*    END-PERFORM.
010313*
010313*** CHECK FOR ROUNDING AND ADD DIFFERENCE TO FIRST AGENT
010313*    IF WS-TOTAL-SHARE NOT = 100
010313*         COMPUTE LAPUP-CVGA-CVG-AGT-SHR-PCT(1) =
010313*             LAPUP-CVGA-CVG-AGT-SHR-PCT(1) - WS-TOTAL-SHARE
010313*             + 100
010313*    END-IF.
010313*
010313*3190-SET-AGT-SHARE-X.
010313*    EXIT.
      *
      *------------------------
      *3300-PROCESS-OWNER-INFO.
      *------------------------
      *
014177*    IF  WS-APP-IS-HEALTH-PROD
014177*        MOVE LAPUP-CLI-ID (1)   TO LAPUP-POLC-OWN-CLI-ID (1)
014177*        MOVE 'P'                TO LAPUP-POLC-OWN-TYP-CD (1)
014177*        MOVE 'SAME'          TO LAPUP-POLC-OWN-INSRD-REL-CD (1)
014177*        MOVE 'PR'               TO LAPUP-POLC-OWN-ADDR-TYP (1)
014177*        MOVE WS-CLI-ADDR-INFO (1)
014177*                                TO RCLIA-REC-INFO
014177*        MOVE RCLIA-CLI-CTRY-CD  TO WS-POL-CTRY-CD
014177*        GO TO 3300-PROCESS-OWNER-INFO-X
014177*    END-IF.
014177*
      *    MOVE 'N'                    TO WS-POL-OWNER-FOUND-IND.
      *
      *    PERFORM  3310-PROCESS-EACH-OWNER
      *        THRU 3310-PROCESS-EACH-OWNER-X
      *        VARYING X FROM 1 BY 1
      *        UNTIL X > 5.
      *
APEX54*    IF  NOT WS-POL-OWNER-FOUND
      *MSG: NO OWNER SPECIFIED, POLICY (@1)
      *        MOVE 'AS21000065'       TO WGLOB-MSG-REF-INFO
      *        MOVE RPOL-POL-ID        TO WGLOB-MSG-PARM (1)
      *        PERFORM  0260-1000-GENERATE-MESSAGE
      *            THRU 0260-1000-GENERATE-MESSAGE-X
      *    END-IF.
      *
      *3300-PROCESS-OWNER-INFO-X.
      *    EXIT.
      *
      *------------------------
      *3310-PROCESS-EACH-OWNER.
      *------------------------
      *
      *    IF  LAPUP-POLC-OWN-CLI-ID (X) = SPACE
      *        GO TO 3310-PROCESS-EACH-OWNER-X
      *    END-IF.
      *
APEX54*    SET WS-POL-OWNER-FOUND      TO TRUE.
      *    MOVE 'N'                    TO L0280-SIGN-IND.
APEX54*    SET L0280-SPACES-PERMITTED  TO TRUE.
      *    MOVE 3                      TO L0280-LENGTH.
      *    MOVE ZERO                   TO L0280-PRECISION.
      *    MOVE LAPUP-POLC-OWN-CLI-ID (X) TO L0280-INPUT-DATA.
      *
      *    PERFORM  0280-1000-NUMERIC-EDIT
      *        THRU 0280-1000-NUMERIC-EDIT-X.
      *
      *    IF  L0280-OK
      *    AND L0280-OUTPUT > ZERO
      *    AND L0280-OUTPUT NOT > WS-CLIENTS-IN-APP
      *        MOVE LAPUP-CLI-ID (L0280-OUTPUT)
      *                                TO LAPUP-POLC-OWN-CLI-ID (X)
      *        IF  WS-CLI-PRIMARY-OWNER (L0280-OUTPUT)
      *            MOVE 'P'           TO LAPUP-POLC-OWN-TYP-CD (X)
      *        ELSE
      *            NEXT SENTENCE
      *        END-IF
      *    ELSE
      *MSG: INVALID CLIENT COUNT, OR GREATER THAN CLIENTS-IN-APP
      *        MOVE 'AS21000063'       TO WGLOB-MSG-REF-INFO
      *        PERFORM  0260-1000-GENERATE-MESSAGE
      *            THRU 0260-1000-GENERATE-MESSAGE-X
APEX54*        SET WS-ERROR-FOUND      TO TRUE
      *    END-IF.
      *
      *3310-PROCESS-EACH-OWNER-X.
      *    EXIT.
      *
      *-------------------------
      *4000-PROCESS-EACH-CLIENT.
      *-------------------------
      *
      *    PERFORM  4100-INITIALIZE-CLIENT-INFO
      *        THRU 4100-INITIALIZE-CLIENT-INFO-X.
      *
      *    PERFORM  4200-PROCESS-CLIENT-HEADER
      *        THRU 4200-PROCESS-CLIENT-HEADER-X
      *        UNTIL WAPIN-END-OF-STRUCTURE.
      *
      *    PERFORM  APIN-4000-GET-NEXT-STRUCTURE
      *        THRU APIN-4000-GET-NEXT-STRUCTURE-X.
      *
      *    PERFORM  4500-PROCESS-EACH-CLIENT-SED
      *        THRU 4500-PROCESS-EACH-CLIENT-SED-X
      *        UNTIL WAPIN-STRUCT-CLIENT-HEADER
      *        OR WAPIN-STRUCT-POLICY-HEADER
      *        OR WS-ERROR-FOUND.
      *
      *    PERFORM  4900-FINALIZE-CLIENT-INFO
      *        THRU 4900-FINALIZE-CLIENT-INFO-X.
      *
      *4000-PROCESS-EACH-CLIENT-X.
      *    EXIT.
      *
      *----------------------------
      *4100-INITIALIZE-CLIENT-INFO.
      *----------------------------
      *
      *    MOVE ALL 'N'                TO TUFCT-SWITCHES.
      *    MOVE SPACE                  TO WS-CLI-ADDR-TABLE.
557700*    MOVE SPACE                  TO WS-CLI-CONTACT-TABLE.
      *    MOVE SPACE                  TO WS-REQT-TABLE.
      *    MOVE ZERO                   TO ADDR-SUB.
      *    MOVE ZERO                   TO REQT-SUB.
      *    MOVE ZERO                   TO LCLIO-JUVENILE-APP-IND.
      *    MOVE ZERO                   TO LCLIO-INFC-PEND-CNT (1).
      *    MOVE ZERO                   TO LCLIO-INFC-PEND-CNT (2).
      *
557700*    INITIALIZE WCLIC-KEY.
557700*    INITIALIZE RCLIC-REC-INFO.
015508*    INITIALIZE LCLI-PARM-AREA.
      *
      *    PERFORM  CLI-1000-CREATE
      *        THRU CLI-1000-CREATE-X.
      *
      *    PERFORM  CLIA-1000-CREATE
      *        THRU CLIA-1000-CREATE-X.
      *
557700*    PERFORM  CLII-1000-CREATE
557700*        THRU CLII-1000-CREATE-X.
      *
557700*    PERFORM  CLIC-1000-CREATE
557700*        THRU CLIC-1000-CREATE-X.
      *
015508*    PERFORM  PRNM-1000-CREATE
015508*        THRU PRNM-1000-CREATE-X.
015508*    PERFORM  CLNM-1000-CREATE
015508*        THRU CLNM-1000-CREATE-X.
015508*    MOVE RCLNM-REC-INFO         TO LCLI-CRNT-CLNM-REC.
015508*    MOVE RCLNM-REC-INFO         TO LCLI-PREV-CLNM-REC.
      *
015508*    PERFORM  CLNC-1000-CREATE
015508*        THRU CLNC-1000-CREATE-X.
      *
      *    PERFORM  APPF-1000-CREATE
      *        THRU APPF-1000-CREATE-X.
      *
APEX54*    INITIALIZE L5850-INPUT-PARM-INFO.
      *
APEX54*    PERFORM  5850-1000-LOAD-CLIO-ARRAY
APEX54*        THRU 5850-1000-LOAD-CLIO-ARRAY-X.
      *
      *    PERFORM  APPV-1000-CREATE
      *        THRU APPV-1000-CREATE-X.
      *
      *4100-INITIALIZE-CLIENT-INFO-X.
      *    EXIT.
      *
      *---------------------------
      *4200-PROCESS-CLIENT-HEADER.
      *---------------------------
      *
      *    PERFORM  APIN-3000-GET-FIELD-N-VALUE
      *        THRU APIN-3000-GET-FIELD-N-VALUE-X.
      *
      *    IF  WAPIN-ERROR-FOUND
      *    OR  WAPIN-FLD-ID = SPACE
      *MSG: ERROR READING NEXT FIELD/VALUE ON SED (@1)
      *        MOVE 'AS21000061'       TO WGLOB-MSG-REF-INFO
      *        MOVE WAPIN-STRUCT-ID    TO WGLOB-MSG-PARM (1)
      *        PERFORM  0260-1000-GENERATE-MESSAGE
      *            THRU 0260-1000-GENERATE-MESSAGE-X
APEX54*        SET WS-ERROR-FOUND      TO TRUE
      *        GO TO 4200-PROCESS-CLIENT-HEADER-X
      *    END-IF.
      *
APEX54*    EVALUATE WAPIN-FLD-ID
      *
APEX54*        WHEN 'CLIENT_ID'
APEX54*             PERFORM  4205-CLIENT-ID
APEX54*                 THRU 4205-CLIENT-ID-X
      *
APEX54*        WHEN 'CLASS'
      *             PERFORM  4220-CLIENT-CLASS
      *                 THRU 4220-CLIENT-CLASS-X
      *
APEX54*        WHEN 'TYPE'
APEX54*             PERFORM  4210-TYPE
APEX54*                 THRU 4210-TYPE-X
      *
APEX54*        WHEN 'JUVENILE'
APEX54*             PERFORM  4215-JUVENILE
APEX54*                 THRU 4215-JUVENILE-X
      *
APEX54*        WHEN 'MESSAGE'
      *             PERFORM  4250-PROCESS-MESSAGE-FIELD
      *                 THRU 4250-PROCESS-MESSAGE-FIELD-X
      *
APEX54*        WHEN 'MI'
APEX54*        WHEN 'UW'
APEX54*             CONTINUE
      *
APEX54*        WHEN OTHER
      *MSG: UNKNOWN FIELD @1 FOR SED @2
      *             MOVE 'AS21000062'     TO WGLOB-MSG-REF-INFO
      *             MOVE WAPIN-FLD-ID     TO WGLOB-MSG-PARM (1)
      *             MOVE WAPIN-STRUCT-ID  TO WGLOB-MSG-PARM (2)
      *             PERFORM  0260-1000-GENERATE-MESSAGE
      *                 THRU 0260-1000-GENERATE-MESSAGE-X
APEX54*             SET WS-ERROR-FOUND    TO TRUE
      *
APEX54*    END-EVALUATE.
      *
      *4200-PROCESS-CLIENT-HEADER-X.
      *    EXIT.
      *
      *---------------
APEX54*4205-CLIENT-ID.
      *---------------
      *
      *    MOVE 'N'                    TO L0280-SIGN-IND.
APEX54*    SET L0280-SPACES-PERMITTED  TO TRUE.
      *    MOVE 2                      TO L0280-LENGTH.
      *    MOVE ZERO                   TO L0280-PRECISION.
      *    MOVE WAPIN-FLD-VALUE        TO L0280-INPUT-DATA.
      *
      *    PERFORM  0280-1000-NUMERIC-EDIT
      *        THRU 0280-1000-NUMERIC-EDIT-X.
      *
APEX54*    IF  NOT L0280-OK
      *        SET WS-ERROR-FOUND      TO TRUE
      *        GO TO 4205-CLIENT-ID-X
APEX54*    END-IF.
      *
      *    MOVE L0280-OUTPUT           TO CLI-SUB.
      *
      *    IF  CLI-SUB > WS-CLI-ID-TABLE-MAX-SIZE
      *        MOVE 'AS21000059'       TO WGLOB-MSG-REF-INFO
      *        PERFORM  0260-1000-GENERATE-MESSAGE
      *            THRU 0260-1000-GENERATE-MESSAGE-X
APEX54*        SET WS-ERROR-FOUND      TO TRUE
      *    END-IF.
      *
APEX54*4205-CLIENT-ID-X.
      *    EXIT.
      *
      *----------
APEX54*4210-TYPE.
      *----------
      *
      *
APEX53* CLIENT TYPE: 0 = PERSON; 1 = COMPANY
      *
      *    MOVE 'N'                    TO L0280-SIGN-IND.
APEX54*    SET L0280-SPACES-PERMITTED  TO TRUE.
      *    MOVE 2                      TO L0280-LENGTH.
      *    MOVE ZERO                   TO L0280-PRECISION.
      *    MOVE WAPIN-FLD-VALUE        TO L0280-INPUT-DATA.
      *
      *    PERFORM  0280-1000-NUMERIC-EDIT
      *        THRU 0280-1000-NUMERIC-EDIT-X.
      *
      *    IF  L0280-OK
      *    AND L0280-OUTPUT < 2
      *        MOVE L0280-OUTPUT       TO WS-CLI-TYPE-CD (CLI-SUB)
      *        IF  WS-CLI-IS-COMPANY (CLI-SUB)
      *            MOVE 'C'            TO RCLI-CLI-SEX-CD
      *            MOVE SPACES         TO RCLI-CLI-SMKR-CD
      *        END-IF
      *    ELSE
APEX54*        SET WS-ERROR-FOUND      TO TRUE
      *    END-IF.
      *
APEX54*4210-TYPE-X.
      *    EXIT.
      *
      *--------------
APEX54*4215-JUVENILE.
      *--------------
      *
      *    MOVE 'N'                    TO L0280-SIGN-IND.
APEX54*    SET L0280-SPACES-PERMITTED  TO TRUE.
      *    MOVE 2                      TO L0280-LENGTH.
      *    MOVE ZERO                   TO L0280-PRECISION.
      *    MOVE WAPIN-FLD-VALUE        TO L0280-INPUT-DATA.
      *
      *    PERFORM  0280-1000-NUMERIC-EDIT
      *        THRU 0280-1000-NUMERIC-EDIT-X.
      *
      *    IF  L0280-OK
      *    AND L0280-OUTPUT < 2
      *        MOVE L0280-OUTPUT       TO LCLIO-JUVENILE-APP-IND
      *        MOVE 'YESNO'            TO WUTTB-UPLD-TTBL-TYP-ID
      *        MOVE WAPIN-FLD-VALUE    TO WUTTB-UPLD-TTBL-VALU-ID
      *        PERFORM  UTTB-1000-LOOKUP-UTTB
      *            THRU UTTB-1000-LOOKUP-UTTB-X
      *        IF  WUTTB-IO-OK
      *            MOVE RUTTB-UPLD-TTBL-VALU-TXT
      *                                TO RAPPV-CLI-JV-INSRD-IND
      *        END-IF
      *    ELSE
APEX54*        SET WS-ERROR-FOUND      TO TRUE
      *    END-IF.
      *
APEX54*4215-JUVENILE-X.
      *    EXIT.
      *
      *------------------
      *4220-CLIENT-CLASS.
      *------------------
      *
APEX53*
APEX53* APEX ASSIGNS ONE OR MORE OF THE FOLLOWING CLASSES
APEX53* TO EACH CLIENT
APEX53*
APEX53*    CLASS
557700*      1       BASE INSURED
557700*      2       RIDER INSURED
557700*      4       OWNER
557700*      8       BENEFICIARY
557700*      16      SECOND OWNER
557700*      32      SPOUSE
557700*      64      PHYSICIAN - NOT YET USED BY UPLOAD
APEX53*
APEX53* THESE CLASSES CAN BE COMBINED TO INDICATE MULTIPLE
APEX53* PROPERTIES.  EG. A VALUE OF 5 WOULD INDICATE A BASE
APEX53* INSURED WHO IS ALSO AN OWNER.
APEX53*
      *
      *    MOVE 'N'                    TO L0280-SIGN-IND.
APEX54*    SET L0280-SPACES-PERMITTED  TO TRUE.
      *    MOVE 2                      TO L0280-LENGTH.
      *    MOVE ZERO                   TO L0280-PRECISION.
      *    MOVE WAPIN-FLD-VALUE        TO L0280-INPUT-DATA.
      *
      *    PERFORM  0280-1000-NUMERIC-EDIT
      *        THRU 0280-1000-NUMERIC-EDIT-X.
      *
      *    IF  L0280-OK
      *    AND L0280-OUTPUT > ZERO
557700*    AND L0280-OUTPUT < 64
557700*    AND L0280-OUTPUT < 128
      *        NEXT SENTENCE
      *    ELSE
557700*MSG: CLIENT CLASS INVALID OR GREATER THAN 128
      *        MOVE 'AS21000058'       TO WGLOB-MSG-REF-INFO
      *        PERFORM  0260-1000-GENERATE-MESSAGE
      *            THRU 0260-1000-GENERATE-MESSAGE-X
APEX54*        SET WS-ERROR-FOUND      TO TRUE
      *        GO TO 4220-CLIENT-CLASS-X
      *    END-IF.
      *
      *    DIVIDE L0280-OUTPUT BY 2  GIVING L0280-OUTPUT
      *                        REMAINDER WS-CLI-BASE-CD (CLI-SUB).
      *    DIVIDE L0280-OUTPUT BY 2  GIVING L0280-OUTPUT
      *                        REMAINDER WS-CLI-RIDER-CD (CLI-SUB).
      *    DIVIDE L0280-OUTPUT BY 2  GIVING L0280-OUTPUT
      *                        REMAINDER WS-CLI-PRIM-OWN-CD (CLI-SUB).
557700*    DIVIDE L0280-OUTPUT BY 2  GIVING L0280-OUTPUT
557700*                        REMAINDER WS-CLI-BNFY-CD (CLI-SUB).
      *    DIVIDE L0280-OUTPUT BY 2  GIVING L0280-OUTPUT
      *                        REMAINDER WS-CLI-OTHR-OWN-CD (CLI-SUB).
      *    DIVIDE L0280-OUTPUT BY 2  GIVING L0280-OUTPUT
      *                        REMAINDER WS-CLI-SPOUS-CD (CLI-SUB).
557700*    DIVIDE L0280-OUTPUT BY 2  GIVING L0280-OUTPUT
557700*                        REMAINDER WS-CLI-BNFY-CD (CLI-SUB).
559577*    DIVIDE L0280-OUTPUT BY 2  GIVING L0280-OUTPUT
559577*                        REMAINDER WS-CLI-PHYS-CD (CLI-SUB).
      *
      *4220-CLIENT-CLASS-X.
      *    EXIT.
      *
      *---------------------------
      *4250-PROCESS-MESSAGE-FIELD.
      *---------------------------
      *
      *    MOVE 'N'                    TO L0280-SIGN-IND.
APEX54*    SET L0280-SPACES-PERMITTED  TO TRUE.
      *    MOVE 8                      TO L0280-LENGTH.
      *    MOVE ZERO                   TO L0280-PRECISION.
      *    MOVE WAPIN-FLD-VALUE        TO L0280-INPUT-DATA.
      *
      *    PERFORM  0280-1000-NUMERIC-EDIT
      *        THRU 0280-1000-NUMERIC-EDIT-X.
      *
      *    IF  NOT L0280-OK
APEX54*        SET WS-ERROR-FOUND      TO TRUE
      *        GO TO 4250-PROCESS-MESSAGE-FIELD-X
      *    ELSE
      *        IF  L0280-OUTPUT = ZERO
      *            GO TO 4250-PROCESS-MESSAGE-FIELD-X
      *        END-IF
      *    END-IF.
      *
      *
      * PARAGRAPH 4251-CHECK-MESSAGES COMPUTES THE MESSAGES THAT WILL BE
      * GENERATED DEPENDING ON THE VALUE IN THE MESSAGE FIELD.  THE
      * VALUE IS ALWAYS MADE UP OF A COMBINATION OF;
      *       VALUE    MESSAGE          OCCURANCE    MESSAGE NUMBER
      *         1      AGE MESSAGE         1           AS2100 1001
      *         2      TIA MESSAGE         2           AS2100 1002
      *         4      SMOKE MESSAGE       3           AS2100 1003
      *         8      CASH MESSAGE        4           AS2100 1004
      * FOR EXAMPLE IF THE MESSAGE FIELD HAS A VALUE OF '13' THEN A 'Y'
      * WILL BE MOVED INTO OCCURANCE 1, 3 AND 4.   MESSAGE AS2100 1001,
      * 1003 AND 1004 WILL BE GENERATED.
      *
      *
      *    PERFORM  4251-CHECK-MESSAGES
      *        THRU 4251-CHECK-MESSAGES-X
      *        VARYING X FROM 1 BY 1
      *        UNTIL X > WS-MESSAGE-ARRAY-MAX-SIZE
      *        OR L0280-OUTPUT = ZERO.
      *
      *    IF  L0280-OUTPUT NOT = ZERO
APEX54*        SET WS-ERROR-FOUND      TO TRUE
      *    END-IF.
      *
      *4250-PROCESS-MESSAGE-FIELD-X.
      *    EXIT.
      *
      *--------------------
      *4251-CHECK-MESSAGES.
      *--------------------
      *
      *    DIVIDE L0280-OUTPUT BY 2    GIVING L0280-OUTPUT
      *                                REMAINDER WS-REMAINDER.
      *
      *    IF  WS-REMAINDER = 1
APEX54*        SET WS-MESSAGE-SET (X)  TO TRUE
      *    END-IF.
      *
      *4251-CHECK-MESSAGES-X.
      *    EXIT.
      *
      *-----------------------------
      *4500-PROCESS-EACH-CLIENT-SED.
      *-----------------------------
      *
      *    IF  WAPIN-STRUCT-ID = SPACE
      *MSG: UNKNOWN @1 SED @2
APEX54*        SET WS-ERROR-FOUND      TO TRUE
      *        GO TO 4500-PROCESS-EACH-CLIENT-SED-X
      *    END-IF.
      *
APEX54*    SET LAPUP-NEW-STRUCT        TO TRUE.
      *    MOVE 'N'                    TO WS-DUPLICATE-MEDI-SW.
010313*    SET WS-MEDI-LOCKED-NO       TO TRUE.
      *
      *    PERFORM  4510-CHECK-CLIENT-SED
      *        THRU 4510-CHECK-CLIENT-SED-X.
      *
      *    IF  WS-DUPLICATE-MEDI
      * ALREADY WROTE A MEDI RECORD FOR THIS DEFINED
      * FIELD NAME, SO DON'T PROCESS UPLOADED DATA
      *        PERFORM  APIN-4000-GET-NEXT-STRUCTURE
      *            THRU APIN-4000-GET-NEXT-STRUCTURE-X
      *        GO TO 4500-PROCESS-EACH-CLIENT-SED-X
      *    END-IF.
      *
      *    PERFORM  4550-PROCESS-CLIENT-FIELD
      *        THRU 4550-PROCESS-CLIENT-FIELD-X
      *        UNTIL WAPIN-END-OF-STRUCTURE
      *        OR WS-ERROR-FOUND.
      *
      *    IF  WAPIN-STRUCT-ID = 'PI'
      *        PERFORM  4910-SCAN-CLIENT-FIELDS
      *            THRU 4910-SCAN-CLIENT-FIELDS-X
      *    END-IF.
      *
557700*    IF  WAPIN-STRUCT-ID = 'PI'
557700*    OR  WAPIN-STRUCT-ID = 'EMPLOYMENT'
557700*    OR  WAPIN-STRUCT-ID = 'EMPLOYMENTDI'
557700*    OR  WAPIN-STRUCT-ID = 'AGENTINFO'
557700*        PERFORM  4570-CONTACT-SED
557700*            THRU 4570-CONTACT-SED-X
557700*    END-IF.
      *
      *    IF  WAPIN-STRUCT-ID = 'PI'
      *    OR  WAPIN-STRUCT-ID = 'PERSPA'
      *    OR  WAPIN-STRUCT-ID = 'PAYORBA'
      *    OR  WAPIN-STRUCT-ID = 'PAYADDRESS'
      *    OR  WAPIN-STRUCT-ID = 'OTHERBA'
      *        PERFORM  4590-ADDRESS-SED
      *            THRU 4590-ADDRESS-SED-X
      *    END-IF.
      *
      *    IF  TUFCT-FILE-CHANGED (TUFCT-MEDI-SUB)
      *    AND NEW-CLIENT-CREATED
      *        MOVE WMEDI-CLI-ID       TO RMEDI-CLI-ID
      *        MOVE RMEDI-KEY          TO WMEDI-KEY
      *        IF  TUFCT-NEW-RECORD (TUFCT-MEDI-SUB)
      *            PERFORM  MEDI-1000-WRITE
      *                THRU MEDI-1000-WRITE-X
      *            MOVE 'N'   TO TUFCT-FILE-CHANGED-SW (TUFCT-MEDI-SUB)
      *        ELSE
      *            PERFORM  MEDI-2000-REWRITE
      *                THRU MEDI-2000-REWRITE-X
      *            MOVE 'N'   TO TUFCT-FILE-CHANGED-SW (TUFCT-MEDI-SUB)
010313*            SET WS-MEDI-LOCKED-NO TO TRUE
      *        END-IF
      *    END-IF.
      *
010313*** WE MAY HAVE READ MEDI FOR UPDATE BUT NOT UPDATED IT FOR
010313*** SOME REASON (E.G. ALL FIELDS WERE ERRORS)
010313*    IF  WS-MEDI-LOCKED
010313*        PERFORM  MEDI-3000-UNLOCK
010313*            THRU MEDI-3000-UNLOCK-X
010313*    END-IF.
      *
      *    PERFORM  APIN-4000-GET-NEXT-STRUCTURE
      *        THRU APIN-4000-GET-NEXT-STRUCTURE-X.
      *
      *4500-PROCESS-EACH-CLIENT-SED-X.
      *    EXIT.
      *
      *----------------------
      *4510-CHECK-CLIENT-SED.
      *----------------------
      *
      *    IF  LAPUP-CLI-ID (CLI-SUB) NOT = SPACE
      *        MOVE 'REQT'             TO WUTTB-UPLD-TTBL-TYP-ID
      *        MOVE WAPIN-STRUCT-ID    TO WUTTB-UPLD-TTBL-VALU-ID
      *        PERFORM  UTTB-1000-LOOKUP-UTTB
      *            THRU UTTB-1000-LOOKUP-UTTB-X
      *        IF  WUTTB-IO-OK
      *            PERFORM  4511-WRITE-REQUIREMENT
      *                THRU 4511-WRITE-REQUIREMENT-X
      *        END-IF
      *    END-IF.
      *
      *    IF  WAPIN-STRUCT-ID = 'PI'
      *    OR  WAPIN-STRUCT-ID = 'UWREQUIREMENTS'
557700*    OR  WAPIN-STRUCT-ID = 'EMPLOYMENT'
557700*    OR  WAPIN-STRUCT-ID = 'EMPLOYMENTDI'
557700*    OR  WAPIN-STRUCT-ID = 'AGENTINFO'
      *        PERFORM  4512-SPECIAL-CLIENT-SED
      *            THRU 4512-SPECIAL-CLIENT-SED-X
      *    ELSE
      *        MOVE 'N'                TO SED-CONTAINS-SPEC-FLD-IND
      *    END-IF.
      *
      *    MOVE WAPIN-STRUCT-ID        TO WS-STRUCTURE-NAME.
      *
      *    IF  WS-MQ-STRUCTURE
      *    OR  WS-DETAILED-MQ-STRUCTURE
      *    OR  WS-ADDL-INFO-MQ-STRUCTURE
      *        PERFORM  4513-CHECK-SET-UP-MEDI
      *            THRU 4513-CHECK-SET-UP-MEDI-X
      *    END-IF.
      *
      *4510-CHECK-CLIENT-SED-X.
      *    EXIT.
      *
      *-----------------------
      *4511-WRITE-REQUIREMENT.
      *-----------------------
      *
54-001*    MOVE SPACES                 TO L0080-PARM-INFO.
54-001*    MOVE WWKDT-ZERO-DT          TO L0080-REQIR-EFF-DT.
54-001*    MOVE WWKDT-ZERO-DT          TO L0080-REQIR-TST-DT.
54-001*    MOVE '4'                    TO L0080-RQST-CD.
54-001*    MOVE 'C'                    TO L0080-POL-CLI-CD.
54-001*    MOVE LAPUP-CLI-ID (CLI-SUB) TO L0080-CLI-ID.
      *    MOVE ZERO                   TO L0080-COVERAGE-NUMBER.
54-001*    MOVE RUTTB-UPLD-TTBL-VALU-TXT TO L0080-REQIR-CODE.
54-001*    MOVE 'RCD'                  TO L0080-REQIR-STAT-CD.
      *
APEX54*    PERFORM  0080-4000-WRITE
APEX54*        THRU 0080-4000-WRITE-X.
      *
      *4511-WRITE-REQUIREMENT-X.
      *    EXIT.
      *
      *------------------------
      *4512-SPECIAL-CLIENT-SED.
      *------------------------
      *
APEX54*    SET SED-CONTAINS-SPECIAL-FIELD TO TRUE.
      *
      *4512-SPECIAL-CLIENT-SED-X.
      *    EXIT.
      *
      *-----------------------
      *4513-CHECK-SET-UP-MEDI.
      *-----------------------
      *
      *
      * CHECK WHICH DEFINED-FIELD THE INFORMATION ON THIS STRUCTURE
      * CORRESPONDS TO, AND INITIALIZE THE MEDI RECORD
      *
      *
      *    MOVE 'N'           TO TUFCT-NEW-RECORD-SW (TUFCT-MEDI-SUB).
      *
APEX54*    SET WS-DUPLICATE-MEDI       TO TRUE.
      *    MOVE 'MEDQ'                 TO WUTTB-UPLD-TTBL-TYP-ID.
      *    MOVE WAPIN-STRUCT-ID        TO WUTTB-UPLD-TTBL-VALU-ID.
      *
      *    PERFORM  UTTB-1000-LOOKUP-UTTB
      *        THRU UTTB-1000-LOOKUP-UTTB-X.
      *
      *    IF  NOT WUTTB-IO-OK
      *MSG: UTTB ERROR: UTTB KEY (@3,@4) APEX FIELD (@1,@2)
      *        MOVE 'AS21000013'       TO WGLOB-MSG-REF-INFO
      *        MOVE WAPIN-STRUCT-ID    TO WGLOB-MSG-PARM (1)
      *        MOVE SPACE              TO WGLOB-MSG-PARM (2)
      *        MOVE 'MEDQ'             TO WGLOB-MSG-PARM (3)
      *        MOVE WAPIN-STRUCT-ID    TO WGLOB-MSG-PARM (4)
      *        PERFORM  9000-BUILD-MESSAGE-EXTRACT
      *            THRU 9000-BUILD-MESSAGE-EXTRACT-X
      *        GO TO 4513-CHECK-SET-UP-MEDI-X
      *    END-IF.
      *
      *    IF  RUTTB-UPLD-TTBL-VALU-TXT = 'UNUSED'
      *        GO TO 4513-CHECK-SET-UP-MEDI-X
      *    END-IF.
      *
      *    MOVE WCLI-CLI-ID               TO WMEDI-CLI-ID.
      *    MOVE RUTTB-UPLD-TTBL-VALU-TXT  TO WMEDI-FLD-ID.
      *
      *    PERFORM  MEDI-1000-READ
      *        THRU MEDI-1000-READ-X.
      *
      *    IF  WMEDI-IO-OK
      *        IF  OLD-CLIENT-MATCHED
      *        OR  NOT WS-ADDL-INFO-MQ-STRUCTURE
      *            MOVE 'AS21000011'      TO WGLOB-MSG-REF-INFO
      *            MOVE WAPIN-STRUCT-ID   TO WGLOB-MSG-PARM (1)
      *            MOVE WMEDI-FLD-ID      TO WGLOB-MSG-PARM (2)
      *            PERFORM  9000-BUILD-MESSAGE-EXTRACT
      *                THRU 9000-BUILD-MESSAGE-EXTRACT-X
      *        ELSE
      *            PERFORM  MEDI-1000-READ-FOR-UPDATE
      *                THRU MEDI-1000-READ-FOR-UPDATE-X
      *            MOVE 'N'               TO WS-DUPLICATE-MEDI-SW
010313*            SET WS-MEDI-LOCKED     TO TRUE
      *        END-IF
APEX54*        GO TO 4513-CHECK-SET-UP-MEDI-X
      *    END-IF.
      *
APEX54*    SET TUFCT-NEW-RECORD (TUFCT-MEDI-SUB) TO TRUE.
      *    MOVE 'N'                    TO WS-DUPLICATE-MEDI-SW.
      *
      *    PERFORM  MEDI-1000-CREATE
      *        THRU MEDI-1000-CREATE-X.
      *
      *    IF  WS-MQ-STRUCTURE
      *        GO TO 4513-CHECK-SET-UP-MEDI-X
      *    ELSE
      *        MOVE WAPIN-STRUCT-ID    TO WUTTB-UPLD-TTBL-VALU-ID
      *        MOVE 'DISOR'            TO WUTTB-UPLD-TTBL-TYP-ID
      *        PERFORM  UTTB-1000-LOOKUP-UTTB
      *            THRU UTTB-1000-LOOKUP-UTTB-X
      *        IF  WUTTB-IO-OK
      *            MOVE RUTTB-UPLD-TTBL-VALU-TXT
      *                                TO RMEDI-DISORD-TYP-TXT
      *        ELSE
      *MSG: UTTB ERROR: UTTB KEY (@3,@4) APEX FIELD (@1,@2)
      *            MOVE 'AS21000013'    TO WGLOB-MSG-REF-INFO
      *            MOVE WAPIN-STRUCT-ID TO WGLOB-MSG-PARM (1)
      *            MOVE WAPIN-FLD-ID    TO WGLOB-MSG-PARM (2)
      *            MOVE 'DISOR'         TO WGLOB-MSG-PARM (3)
      *            MOVE WAPIN-STRUCT-ID TO WGLOB-MSG-PARM (4)
      *            PERFORM  9000-BUILD-MESSAGE-EXTRACT
      *                THRU 9000-BUILD-MESSAGE-EXTRACT-X
      *        END-IF
      *    END-IF.
      *
      *4513-CHECK-SET-UP-MEDI-X.
      *    EXIT.
      *
      *--------------------------
      *4550-PROCESS-CLIENT-FIELD.
      *--------------------------
      *
      *    PERFORM  APIN-3000-GET-FIELD-N-VALUE
      *        THRU APIN-3000-GET-FIELD-N-VALUE-X.
      *
      *    IF  LAPUP-BYPASS-STRUCT
      *        GO TO 4550-PROCESS-CLIENT-FIELD-X
      *    END-IF.
      *
      *    IF  SED-CONTAINS-SPECIAL-FIELD
      *        PERFORM  4551-CHECK-SPECIAL-FIELD
      *            THRU 4551-CHECK-SPECIAL-FIELD-X
      *    END-IF.
      *
      *    MOVE WAPIN-STRUCT-ID        TO WUFLD-UPLD-FLD-STRUCT-NM.
      *    MOVE WAPIN-FLD-ID           TO WUFLD-UPLD-FLD-APEX-NM.
      *
      *    PERFORM  UFLD-1000-READ
      *        THRU UFLD-1000-READ-X.
      *
APEX54*    IF  NOT WUFLD-IO-OK
APEX54*        GO TO 4550-PROCESS-CLIENT-FIELD-X
APEX54*    END-IF.
      *
557698*    IF  RUFLD-UPLD-FLD-TYP-UPPER-CASE
557698*        PERFORM  8300-TRANSLATE-UPPER-CASE
557698*            THRU 8300-TRANSLATE-UPPER-CASE-X
557698*    END-IF.
      *
      *    IF  RUFLD-UPLD-FLD-TYP-MESSAGE
      *        PERFORM  4553-PROCESS-MESSAGE-FLD
      *            THRU 4553-PROCESS-MESSAGE-FLD-X
APEX54*        GO TO 4550-PROCESS-CLIENT-FIELD-X
APEX54*    END-IF.
      *
      *    IF  RUFLD-UPLD-FLD-TYP-UNUSED
      *        PERFORM  4554-PROCESS-UNUSED-FLD
      *            THRU 4554-PROCESS-UNUSED-FLD-X
APEX54*        GO TO 4550-PROCESS-CLIENT-FIELD-X
APEX54*    END-IF.
      *
      *    IF  RUFLD-UPLD-FLD-FILE-CD = 'REQT'
      *        PERFORM  4556-SAVE-UWRESULTS
      *            THRU 4556-SAVE-UWRESULTS-X
APEX54*        GO TO 4550-PROCESS-CLIENT-FIELD-X
APEX54*    END-IF.
      *
      *    PERFORM  APUP-1000-PROCESS-FIELD
      *        THRU APUP-1000-PROCESS-FIELD-X.
      *
      *    PERFORM  4558-CHECK-RESPONSE
      *        THRU 4558-CHECK-RESPONSE-X.
      *
      *4550-PROCESS-CLIENT-FIELD-X.
      *    EXIT.
      *
      *-------------------------
      *4551-CHECK-SPECIAL-FIELD.
      *-------------------------
      *
      *    IF  (WAPIN-STRUCT-ID = 'PI'
      *    AND WAPIN-FLD-ID = 'CLIENT_NO')
      *        PERFORM  4552-EDIT-CLIENT-ID
      *            THRU 4552-EDIT-CLIENT-ID-X
      *    END-IF.
      *
      *    IF  (WAPIN-STRUCT-ID = 'PI'
      *    AND WAPIN-FLD-ID = 'PRIMARY')
      *        PERFORM  4555-SECOND-OWNER-PRIMARY
      *            THRU 4555-SECOND-OWNER-PRIMARY-X
      *    END-IF.
      *
      *    IF  WAPIN-FLD-ID = 'MESSAGE'
      *        PERFORM  9100-PROCESS-MESSAGE-FIELD
      *            THRU 9100-PROCESS-MESSAGE-FIELD-X
      *    END-IF.
      *
557700*    IF  WAPIN-STRUCT-ID = 'PI'
557700*    AND WAPIN-FLD-ID    = 'RES_PHONE'
557700*        MOVE 'HO'               TO RCLIC-CLI-CNTCT-ID-CD
557700*    END-IF.
      *
557700*    IF  (WAPIN-STRUCT-ID = 'EMPLOYMENT'
557700*    OR  WAPIN-STRUCT-ID  = 'EMPLOYMENTDI')
557700*    AND WAPIN-FLD-ID     = 'BUS_PHONE'
557700*        MOVE 'BU'               TO RCLIC-CLI-CNTCT-ID-CD
557700*    END-IF.
      *
557700*    IF  WAPIN-STRUCT-ID = 'AGENTINFO'
557700*    AND WAPIN-FLD-ID    = 'BUSINESS'
557700*        MOVE 'BU'               TO RCLIC-CLI-CNTCT-ID-CD
557700*    END-IF.
      *
      *4551-CHECK-SPECIAL-FIELD-X.
      *    EXIT.
      *
      *--------------------
      *4552-EDIT-CLIENT-ID.
      *--------------------
      *
      *    MOVE 'N'                    TO L0280-SIGN-IND.
APEX54*    SET L0280-SPACES-PERMITTED  TO TRUE.
      *    MOVE 10                     TO L0280-LENGTH.
      *    MOVE ZERO                   TO L0280-PRECISION.
      *    MOVE WAPIN-FLD-VALUE        TO L0280-INPUT-DATA.
      *
      *    PERFORM  0280-1000-NUMERIC-EDIT
      *        THRU 0280-1000-NUMERIC-EDIT-X.
      *
      *    IF  L0280-OK
      *    AND L0280-OUTPUT = CLI-SUB
      *        MOVE SPACE            TO LAPUP-CLI-ID (CLI-SUB)
      *    ELSE
557698*        PERFORM  8300-TRANSLATE-UPPER-CASE
557698*            THRU 8300-TRANSLATE-UPPER-CASE-X
      *        MOVE WAPIN-FLD-VALUE  TO LAPUP-CLI-ID (CLI-SUB)
      *    END-IF.
      *
      *4552-EDIT-CLIENT-ID-X.
      *    EXIT.
      *
      *-------------------------
      *4553-PROCESS-MESSAGE-FLD.
      *-------------------------
      *
      *    MOVE RUFLD-UPLD-FLD-NM      TO WS-MESSAGE-CODE.
      *    MOVE 2                      TO L0280-LENGTH.
      *    MOVE ZERO                   TO L0280-PRECISION.
      *    MOVE WS-MESSAGE-LAST2       TO L0280-INPUT-DATA.
      *
      *    PERFORM  0280-1000-NUMERIC-EDIT
      *        THRU 0280-1000-NUMERIC-EDIT-X.
      *
      *    IF  L0280-OK
      *    AND L0280-OUTPUT > ZERO
      *    AND L0280-OUTPUT NOT > WS-MESSAGE-ARRAY-MAX-SIZE
APEX54*        SET WS-MESSAGE-SET (L0280-OUTPUT) TO TRUE
      *    ELSE
      *MSG: INVALID MESSAGE NUMBER (@1) ON UFLD RECORD (@2)
      *        MOVE 'AS21000014'       TO WGLOB-MSG-REF-INFO
      *        MOVE RUFLD-UPLD-FLD-NM  TO WGLOB-MSG-PARM (1)
      *        MOVE WUFLD-KEY          TO WGLOB-MSG-PARM (2)
      *        PERFORM  9000-BUILD-MESSAGE-EXTRACT
      *            THRU 9000-BUILD-MESSAGE-EXTRACT-X
      *    END-IF.
      *
      *4553-PROCESS-MESSAGE-FLD-X.
      *    EXIT.
      *
      *------------------------
      *4554-PROCESS-UNUSED-FLD.
      *------------------------
      *
      *MSG: UNUSED TRANSLATION TABLE RECORD (@1,@2)
      *    MOVE 'AS21000027'           TO WGLOB-MSG-REF-INFO.
      *    MOVE WAPIN-STRUCT-ID        TO WGLOB-MSG-PARM (1).
      *    MOVE WAPIN-FLD-ID           TO WGLOB-MSG-PARM (2).
      *
      *    PERFORM  9000-BUILD-MESSAGE-EXTRACT
      *        THRU 9000-BUILD-MESSAGE-EXTRACT-X.
      *
      *4554-PROCESS-UNUSED-FLD-X.
      *    EXIT.
      *
      *--------------------------
      *4555-SECOND-OWNER-PRIMARY.
      *--------------------------
      *
      *    IF  WAPIN-FLD-VALUE = '1'
      *        MOVE 'P'                TO LAPUP-POLC-OWN-TYP-CD (2)
      *    END-IF.
      *
      *4555-SECOND-OWNER-PRIMARY-X.
      *    EXIT.
      *
      *--------------------
      *4556-SAVE-UWRESULTS.
      *--------------------
      *
      *
      * ADD REQUIREMENT TO HOLD ARRAY AND GENERATE DEFAULT STATUS
      *
      *    IF  RUFLD-UPLD-FLD-NM = 'REQIR_ID'
      *        MOVE RUFLD-UPLD-TTBL-TYP-ID TO WUTTB-UPLD-TTBL-TYP-ID
      *        MOVE WAPIN-FLD-ID           TO WUTTB-UPLD-TTBL-VALU-ID
      *        PERFORM  UTTB-1000-LOOKUP-UTTB
      *            THRU UTTB-1000-LOOKUP-UTTB-X
      *        IF  WUTTB-IO-OK
      *            ADD 1                   TO REQT-SUB
      *            MOVE RUTTB-UPLD-TTBL-VALU-TXT
      *                                    TO WS-REQT-CODE (REQT-SUB)
      *            MOVE 'NTO'              TO WS-REQT-STATUS (REQT-SUB)
      *        ELSE
      *MSG: UTTB ERROR: UTTB KEY (@3,@4) APEX FIELD (@1,@2)
      *            MOVE 'AS21000013'       TO  WGLOB-MSG-REF-INFO
      *            MOVE WAPIN-STRUCT-ID    TO WGLOB-MSG-PARM (1)
      *            MOVE WAPIN-FLD-ID       TO WGLOB-MSG-PARM (2)
      *            MOVE RUFLD-UPLD-TTBL-TYP-ID TO WGLOB-MSG-PARM (3)
      *            MOVE WAPIN-FLD-ID       TO WGLOB-MSG-PARM (4)
      *            PERFORM  9000-BUILD-MESSAGE-EXTRACT
      *                THRU 9000-BUILD-MESSAGE-EXTRACT-X
      *        END-IF
      *    END-IF.
      *
      * OVERRIDE DEFAULT STATUS
      *
      *    IF  RUFLD-UPLD-FLD-NM = 'CPREQT_STAT_CD'
014035*    AND WAPIN-FLD-VALUE  =  '1'
014035*        IF  WAPIN-FLD-VALUE  =  '1'
APEX54*        IF  REQT-SUB = 0
APEX54*            ADD 1               TO REQT-SUB
APEX54*        END-IF
      *        MOVE 'ORD'              TO WS-REQT-STATUS (REQT-SUB)
014035*        END-IF
      *    END-IF.
      *
      *4556-SAVE-UWRESULTS-X.
      *    EXIT.
      *
      *--------------------
      *4558-CHECK-RESPONSE.
      *--------------------
      *
      *    IF  LAPUP-REC-CHANGED
      *
APEX54*        EVALUATE RUFLD-UPLD-FLD-FILE-CD
      *
APEX54*            WHEN 'APPF'
APEX54*                 SET TUFCT-FILE-CHANGED (TUFCT-APPF-SUB) TO TRUE
      *
APEX54*            WHEN 'APPV'
      *                 SET TUFCT-FILE-CHANGED (TUFCT-APPV-SUB) TO TRUE
      *
APEX54*            WHEN 'CLIO'
APEX54*                 SET TUFCT-FILE-CHANGED (TUFCT-CLIO-SUB) TO TRUE
      *
APEX54*            WHEN 'MEDI'
APEX54*                 SET TUFCT-FILE-CHANGED (TUFCT-MEDI-SUB) TO TRUE
      *
APEX54*            WHEN 'PRNM'
APEX54*                 SET TUFCT-FILE-CHANGED (TUFCT-PRNM-SUB) TO TRUE
      *
APEX54*            WHEN 'CLI'
APEX54*            WHEN 'CLIA'
557700*            WHEN 'CLII'
557700*            WHEN 'CLIC'
APEX54*                  SET TUFCT-FILE-CHANGED (TUFCT-CLI-SUB) TO TRUE
      *
APEX54*        END-EVALUATE
      *
APEX54*    END-IF.
      *
      *    IF  NOT LAPUP-GOOD-RETURN
      *        PERFORM  9200-APUP-RETURN-ERROR
      *            THRU 9200-APUP-RETURN-ERROR-X
      *    END-IF.
      *
      *4558-CHECK-RESPONSE-X.
      *    EXIT.
      *
557700*-----------------
557700*4570-CONTACT-SED.
557700*-----------------
      *
557700*    IF  RCLIC-CLI-CNTCT-ID-CD = 'HO'
557700*        MOVE RCLIC-CLI-CNTCT-ID-CD  TO WS-CLI-CNTCT-ID-CD (1)
557700*        MOVE RCLIC-CLI-CNTCT-ID-TXT TO WS-CLI-CNTCT-ID-TXT(1)
557700*    ELSE
557700*        IF  RCLIC-CLI-CNTCT-ID-CD = 'BU'
557700*            MOVE RCLIC-CLI-CNTCT-ID-CD  TO WS-CLI-CNTCT-ID-CD (2)
557700*            MOVE RCLIC-CLI-CNTCT-ID-TXT TO WS-CLI-CNTCT-ID-TXT(2)
557700*        END-IF
557700*    END-IF.
      *
557700*    PERFORM  CLIC-1000-CREATE
557700*        THRU CLIC-1000-CREATE-X.
      *
557700*4570-CONTACT-SED-X.
557700*    EXIT.
      *
      *-----------------
      *4590-ADDRESS-SED.
      *-----------------
      *
APEX54*    EVALUATE WAPIN-STRUCT-ID
      *
APEX54*        WHEN 'PI'
      *             MOVE 'PR'          TO WS-FIND-ADDR-TYP
      *
APEX54*        WHEN 'PERSPA'
      *             MOVE 'PA'          TO WS-FIND-ADDR-TYP
      *             IF  RCLI-PREV-INFO-CD = SPACE
APEX54*                 SET TUFCT-FILE-CHANGED (TUFCT-CLI-SUB) TO TRUE
      *                 MOVE 'A'       TO RCLI-PREV-INFO-CD
APEX54*             END-IF
APEX54*             IF  RCLI-PREV-INFO-CD = 'N'
APEX54*                 SET TUFCT-FILE-CHANGED (TUFCT-CLI-SUB) TO TRUE
      *                 MOVE 'B'       TO RCLI-PREV-INFO-CD
APEX54*             END-IF
      *
APEX54*        WHEN 'PAYORBA'
APEX54*        WHEN 'PAYADDRESS'
APEX54*        WHEN 'OTHERBA'
      *             MOVE 'O'           TO WS-FIND-ADDR-TYP
      *
APEX54*    END-EVALUATE.
      *
APEX54*    PERFORM
      *        VARYING X FROM ADDR-SUB BY -1
      *        UNTIL X < 1
      *        OR WS-CLI-ADDR-TYP-ID (X) = WS-FIND-ADDR-TYP
015543*        CONTINUE
APEX54*    END-PERFORM.
      *
      *    ADD +1                      TO ADDR-SUB.
      *    MOVE RCLIA-REC-INFO         TO WS-CLI-ADDR-INFO (ADDR-SUB).
      *    MOVE WS-FIND-ADDR-TYP       TO WS-CLI-ADDR-TYP-ID (ADDR-SUB).
      *
      *    IF  X < 1
      *        MOVE 1                  TO WS-CLI-ADDR-SEQ-NUM (ADDR-SUB)
      *    ELSE
APEX54*        COMPUTE WS-CLI-ADDR-SEQ-NUM (ADDR-SUB)
APEX54*                                 = WS-CLI-ADDR-SEQ-NUM (X) + 1
      *    END-IF.
      *
      *    PERFORM  CLIA-1000-CREATE
      *        THRU CLIA-1000-CREATE-X.
      *
      *4590-ADDRESS-SED-X.
      *    EXIT.
      *
      *--------------------------
      *4900-FINALIZE-CLIENT-INFO.
      *--------------------------
      *
559577*
559577* DO NOT CREATE CLIENT RECORD FOR PHYSICIAN
559577*
      *
559577*    IF  WS-CLI-PHYSICIAN (CLI-SUB)
559577*        GO TO 4900-FINALIZE-CLIENT-INFO-X
559577*    END-IF.
      *
      *    IF  WS-CLI-PRIMARY-OWNER (CLI-SUB)
      *        MOVE WS-CLI-ADDR-INFO (1)    TO RCLIA-REC-INFO
010313*        MOVE RCLIA-CLI-CRNT-LOC-CD   TO WS-POL-ISS-LOC-CD
      *        MOVE RCLIA-CLI-CTRY-CD       TO WS-POL-CTRY-CD
      *    END-IF.
      *
      *    IF  OLD-CLIENT-MATCHED
557700*        IF  TUFCT-FILE-CHANGED (TUFCT-CLI-SUB)
557700*            PERFORM  4905-WRITE-CLII-RECS
557700*                THRU 4905-WRITE-CLII-RECS-X
557700*            PERFORM  4903-WRITE-CLIC-RECS
557700*                THRU 4903-WRITE-CLIC-RECS-X
557700*        END-IF
      *        GO TO 4900-FINALIZE-CLIENT-INFO-X
      *    END-IF.
      *
      *    IF  TUFCT-FILE-CHANGED (TUFCT-PRNM-SUB)
      *        IF  RCLI-PREV-INFO-CD = SPACE
APEX54*            SET TUFCT-FILE-CHANGED (TUFCT-CLI-SUB)
APEX54*                                     TO TRUE
      *            MOVE 'N'                 TO RCLI-PREV-INFO-CD
      *        END-IF
APEX54*        IF  RCLI-PREV-INFO-CD = 'A'
APEX54*            SET TUFCT-FILE-CHANGED (TUFCT-CLI-SUB)
APEX54*                                     TO TRUE
      *            MOVE 'B'                 TO RCLI-PREV-INFO-CD
      *        END-IF
      *    END-IF.
      *
      *    IF  TUFCT-FILE-CHANGED (TUFCT-CLI-SUB)
      *        MOVE WCLI-CLI-ID             TO RCLI-CLI-ID
015508*        PERFORM  9700-UPDATE-PHONETIC
015508*            THRU 9700-UPDATE-PHONETIC-X
555288*        PERFORM  4906-SET-MIB-IND
555288*            THRU 4906-SET-MIB-IND-X
      *        PERFORM  CLI-1000-WRITE
      *            THRU CLI-1000-WRITE-X
557700*        PERFORM  4905-WRITE-CLII-RECS
557700*            THRU 4905-WRITE-CLII-RECS-X
557700*        PERFORM  4903-WRITE-CLIC-RECS
557700*            THRU 4903-WRITE-CLIC-RECS-X
      *        PERFORM  4901-WRITE-CLIA-RECS
      *            THRU 4901-WRITE-CLIA-RECS-X
      *            VARYING X FROM 1 BY 1
      *            UNTIL X > ADDR-SUB
015508*        PERFORM  4907-WRITE-CLNC-CLNM-REC
015508*            THRU 4907-WRITE-CLNC-CLNM-REC-X
015508*        PERFORM  4909-SET-EMPR-NM-ADDR
015508*            THRU 4909-SET-EMPR-NM-ADDR-X
      *    END-IF.
      *
      *    IF  TUFCT-FILE-CHANGED (TUFCT-PRNM-SUB)
015508*        MOVE WPRNM-CLI-ID            TO RPRNM-CLI-ID
015508*        MOVE '001'                   TO WPRNM-CLI-NM-SEQ-NUM
015508*        MOVE LCLI-PREV-CLNM-REC      TO RCLNM-REC-INFO
015508*        MOVE WCLNM-CLI-ID            TO RCLNM-CLI-ID
015508*        IF NOT WGLOB-COUNTRY-JAPAN
015508*            SET WCLNM-CLI-INDV-GR-ALPHA TO TRUE
015508*        ELSE
015508*            SET WCLNM-CLI-INDV-GR-KATAKANA TO TRUE
015508*        END-IF
015508*        SET WCLNM-CLI-INDV-NM-TYP-PREV  TO TRUE
015508*        MOVE +001                    TO WCLNM-CLI-INDV-SEQ-NUM
      *        PERFORM  9800-UPDATE-PREV-PHONETIC
      *            THRU 9800-UPDATE-PREV-PHONETIC-X
015508*        PERFORM  PRNM-1000-WRITE
015508*            THRU PRNM-1000-WRITE-X
015508*        PERFORM  CLNM-1000-WRITE
015508*            THRU CLNM-1000-WRITE-X
      *    END-IF.
      *
      *    IF  TUFCT-FILE-CHANGED (TUFCT-APPF-SUB)
      *        MOVE WAPPF-CLI-ID            TO RAPPF-CLI-ID
      *        PERFORM  4902-CHECK-PREG-IND
      *            THRU 4902-CHECK-PREG-IND-X
      *        PERFORM  APPF-1000-WRITE
      *            THRU APPF-1000-WRITE-X
      *    END-IF.
      *
      *    IF  TUFCT-FILE-CHANGED (TUFCT-APPV-SUB)
      *        MOVE WAPPV-CLI-ID            TO RAPPV-CLI-ID
      *        PERFORM  APPV-1000-WRITE
      *            THRU APPV-1000-WRITE-X
      *    END-IF.
      *
      *    IF  TUFCT-FILE-CHANGED (TUFCT-CLIO-SUB)
      *        PERFORM  4904-WRITE-CLIO
      *            THRU 4904-WRITE-CLIO-X
      *    END-IF.
      *
      *    PERFORM  4908-WRITE-REQT
      *        THRU 4908-WRITE-REQT-X
      *        VARYING REQT-SUB FROM 1 BY 1
      *        UNTIL REQT-SUB > WS-REQT-MAX.
      *
      *4900-FINALIZE-CLIENT-INFO-X.
      *    EXIT.
      *
      *---------------------
      *4901-WRITE-CLIA-RECS.
      *---------------------
      *
      *    MOVE WS-CLI-ADDR-INFO (X)        TO RCLIA-REC-INFO.
      *
      *    IF  (RCLIA-CLI-ADDR-LN-1-TXT = SPACES
      *    OR RCLIA-CLI-CITY-NM-TXT     = SPACES
      *    OR RCLIA-CLI-CRNT-LOC-CD     = SPACES
      *    OR RCLIA-CLI-PSTL-CD         = SPACES
      *    OR RCLIA-CLI-CTRY-CD         = SPACES)
      *        MOVE 'I'                     TO RCLIA-CLI-ADDR-STAT-CD
      *    END-IF.
      *
      *    MOVE WCLIA-CLI-ID                TO RCLIA-CLI-ID.
      *    MOVE RCLIA-KEY                   TO WCLIA-KEY.
      *
      *    PERFORM  CLIA-1000-WRITE
      *        THRU CLIA-1000-WRITE-X.
      *
      *4901-WRITE-CLIA-RECS-X.
      *    EXIT.
      *
      *--------------------
      *4902-CHECK-PREG-IND.
      *--------------------
      *
      *    IF  RCLI-CLI-SEX-CD NOT = 'F'
      *        MOVE 'N'                TO RAPPF-CLI-PREG-IND
      *    END-IF.
      *
      *4902-CHECK-PREG-IND-X.
      *    EXIT.
      *
557700*---------------------
557700*4903-WRITE-CLIC-RECS.
557700*---------------------
557700*
557700*    IF  WS-CLI-CNTCT-ID-CD (1) = SPACES
557700*        NEXT SENTENCE
557700*    ELSE
557700*        MOVE WS-CLI-CNTCT-ID-CD (1)  TO RCLIC-CLI-CNTCT-ID-CD
557700*        MOVE WS-CLI-CNTCT-ID-TXT(1)  TO RCLIC-CLI-CNTCT-ID-TXT
557700*        MOVE WCLI-CLI-ID             TO RCLIC-CLI-ID
557700*        MOVE RCLIC-KEY               TO WCLIC-KEY
557700*
557700*        PERFORM  CLIC-1000-READ
557700*            THRU CLIC-1000-READ-X
557700*
557700*        IF  WCLIC-IO-OK
557700*MSG: CLIENT CONTACT RECORD (@1) ALREADY EXISTS - @2
557700*            MOVE 'AS21000073'           TO WGLOB-MSG-REF-INFO
557700*            MOVE RCLIC-KEY              TO WGLOB-MSG-PARM (1)
557700*            MOVE RCLIC-CLI-CNTCT-ID-TXT TO WGLOB-MSG-PARM (2)
557700*            PERFORM  9000-BUILD-MESSAGE-EXTRACT
557700*                THRU 9000-BUILD-MESSAGE-EXTRACT-X
557700*        ELSE
557700*            PERFORM  CLIC-1000-WRITE
557700*                THRU CLIC-1000-WRITE-X
557700*        END-IF
557700*    END-IF.
557700*
557700*    IF  WS-CLI-CNTCT-ID-CD (2) = SPACES
557700*        NEXT SENTENCE
557700*    ELSE
557700*        MOVE WS-CLI-CNTCT-ID-CD (2)  TO RCLIC-CLI-CNTCT-ID-CD
557700*        MOVE WS-CLI-CNTCT-ID-TXT(2)  TO RCLIC-CLI-CNTCT-ID-TXT
557700*        MOVE WCLI-CLI-ID             TO RCLIC-CLI-ID
557700*        MOVE RCLIC-KEY               TO WCLIC-KEY
557700*
557700*        PERFORM  CLIC-1000-READ
557700*            THRU CLIC-1000-READ-X
557700*
557700*        IF  WCLIC-IO-OK
557700*MSG: CLIENT CONTACT RECORD (@1) ALREADY EXISTS - @2
557700*            MOVE 'AS21000073'           TO WGLOB-MSG-REF-INFO
557700*            MOVE RCLIC-KEY              TO WGLOB-MSG-PARM (1)
557700*            MOVE RCLIC-CLI-CNTCT-ID-TXT TO WGLOB-MSG-PARM (2)
557700*            PERFORM  9000-BUILD-MESSAGE-EXTRACT
557700*                THRU 9000-BUILD-MESSAGE-EXTRACT-X
557700*        ELSE
557700*            PERFORM  CLIC-1000-WRITE
557700*                THRU CLIC-1000-WRITE-X
557700*        END-IF
557700*    END-IF.
557700*
557700*4903-WRITE-CLIC-RECS-X.
557700*    EXIT.
      *
      *----------------
      *4904-WRITE-CLIO.
      *----------------
      *
APEX54*    MOVE WCLI-CLI-ID                 TO L5850-CLI-ID.
      *
557245*    IF  WS-APP-IS-LIFE-PROD
557245*        MOVE 'L'                     TO L5850-CLI-OINS-TYP-CD
557245*    ELSE
557245*        MOVE 'H'                     TO L5850-CLI-OINS-TYP-CD
557245*    END-IF.
      *
557245*    EVALUATE TRUE
      *
557245*        WHEN WS-APP-IS-LIFE-PROD
557245*             MOVE 'L'                TO L5850-CLI-OINS-TYP-CD
      *
557245*        WHEN WS-APP-IS-DISABILITY-PROD
557245*             MOVE 'H'                TO L5850-CLI-OINS-TYP-CD
      *
557245*        WHEN WS-APP-IS-IMM-ANNUITY-PROD
557245*             MOVE 'A'                TO L5850-CLI-OINS-TYP-CD
      *
557245*        WHEN WS-APP-IS-DEF-ANNUITY-PROD
557245*             MOVE 'A'                TO L5850-CLI-OINS-TYP-CD
014177*
014177*        WHEN WS-APP-IS-HEALTH-PROD
014177*             MOVE 'H'                TO L5850-CLI-OINS-TYP-CD
      *
557245*    END-EVALUATE.
      *
APEX54*    PERFORM  5850-2000-UPDATE-CLIO
APEX54*        THRU 5850-2000-UPDATE-CLIO-X.
      *
      *4904-WRITE-CLIO-X.
      *    EXIT.
      *
557700*---------------------
557700*4905-WRITE-CLII-RECS.
557700*---------------------
557700*
557700*    IF  RCLII-CLI-EARN-INCM-AMT > 0
557700*    OR  RCLII-CLI-NET-WRTH-AMT  > 0
557700*    OR  RCLII-CLI-OTHR-INCM-AMT > 0
557700*        NEXT SENTENCE
557700*    ELSE
557700*        GO TO 4905-WRITE-CLII-RECS-X
557700*    END-IF.
557700*
557700*    MOVE WCLI-CLI-ID         TO RCLII-CLI-ID.
557700*    MOVE WS-APP-SAVED-DATE   TO RCLII-CLI-INCM-EFF-DT.
557700*    MOVE RCLII-KEY           TO WCLII-KEY.
557700*
557700*    PERFORM  CLII-1000-READ
557700*        THRU CLII-1000-READ-X.
557700*
557700*    IF  WCLII-IO-OK
557700*MSG: CLIENT INCOME RECORD (@1) ALREADY EXISTS - INFO NOT SAVED
557700*        MOVE 'AS21000072'             TO WGLOB-MSG-REF-INFO
557700*        MOVE RCLII-KEY                TO WGLOB-MSG-PARM (1)
557700*        PERFORM  9000-BUILD-MESSAGE-EXTRACT
557700*            THRU 9000-BUILD-MESSAGE-EXTRACT-X
557700*    ELSE
557700*        PERFORM  CLII-1000-WRITE
557700*            THRU CLII-1000-WRITE-X
557700*    END-IF.
557700*
557700*4905-WRITE-CLII-RECS-X.
557700*    EXIT.
      *
555288*-----------------
555288*4906-SET-MIB-IND.
555288*-----------------
555288*
555288*    PERFORM  PSYS-1000-READ
555288*        THRU PSYS-1000-READ-X.
555288*
555288*    IF  WPSYS-IO-OK
555288*        IF  RPSYS-MIB-COMUN-TYP-PC
555288*        OR  RPSYS-MIB-COMUN-TYP-NONE
555288*            SET RCLI-CLI-MIB-IND-CLEAR TO TRUE
555288*        END-IF
555288*    END-IF.
555288*
555288*4906-SET-MIB-IND-X.
555288*    EXIT.
      *
015508*-------------------------
015508*4907-WRITE-CLNC-CLNM-REC.
015508*-------------------------
015508*    PERFORM  9700-UPDATE-PHONETIC
015508*        THRU 9700-UPDATE-PHONETIC-X.
      *
015508*    IF RCLI-CLI-SEX-COMPANY
015508*        MOVE WCLI-CLI-ID                 TO WCLNC-CLI-ID
015508*        IF NOT WGLOB-COUNTRY-JAPAN
015508*            SET WCLNC-CLI-CO-GR-ALPHA    TO TRUE
015508*        ELSE
015508*            SET WCLNC-CLI-CO-GR-KATAKANA TO TRUE
015508*        END-IF
015508*        SET WCLNC-CLI-CO-NM-TYP-CLI      TO TRUE
015508*        PERFORM  CLNC-1000-WRITE
015508*            THRU CLNC-1000-WRITE-X
015508*        GO TO 4907-WRITE-CLNC-CLNM-REC-X
015508*    END-IF.
      *
015508*    MOVE LCLI-CRNT-CLNM-REC            TO RCLNM-REC-INFO.
015508*    MOVE WCLI-CLI-ID                   TO WCLNM-CLI-ID.
015508*    IF NOT WGLOB-COUNTRY-JAPAN
015508*        SET WCLNM-CLI-INDV-GR-ALPHA    TO TRUE
015508*    ELSE
015508*        SET WCLNM-CLI-INDV-GR-KATAKANA TO TRUE
015508*    END-IF.
015508*    SET WCLNM-CLI-INDV-NM-TYP-CRNT  TO TRUE.
015508*    MOVE +001                       TO WCLNM-CLI-INDV-SEQ-NUM.
015508*    PERFORM  CLNM-1000-WRITE
015508*        THRU CLNM-1000-WRITE-X.
      *
015508*4907-WRITE-CLNC-CLNM-REC-X.
015508*    EXIT.
      *
      *----------------
      *4908-WRITE-REQT.
      *----------------
      *
      *    IF  WS-REQT-STATUS (REQT-SUB) = SPACE
      *        GO TO 4908-WRITE-REQT-X
      *    END-IF.
      *
54-001*    MOVE SPACES                    TO L0080-PARM-INFO.
54-001*    MOVE WWKDT-ZERO-DT             TO L0080-REQIR-EFF-DT.
54-001*    MOVE WWKDT-ZERO-DT             TO L0080-REQIR-TST-DT.
54-001*    MOVE '4'                       TO L0080-RQST-CD.
54-001*    MOVE 'C'                       TO L0080-POL-CLI-CD.
54-001*    MOVE LAPUP-CLI-ID (CLI-SUB)    TO L0080-CLI-ID.
      *    MOVE ZERO                      TO L0080-COVERAGE-NUMBER.
54-001*    MOVE WS-REQT-CODE (REQT-SUB)   TO L0080-REQIR-CODE.
54-001*    MOVE WS-REQT-STATUS (REQT-SUB) TO L0080-REQIR-STAT-CD.
      *
APEX54*    PERFORM  0080-4000-WRITE
APEX54*        THRU 0080-4000-WRITE-X.
      *
      *4908-WRITE-REQT-X.
      *    EXIT.
      *
015508*----------------------
015508*4909-SET-EMPR-NM-ADDR.
015508*----------------------
015508*
015508*    IF LCLI-CLI-EMPLR-NM NOT = SPACE
015508*        PERFORM  CLNC-1000-CREATE
015508*            THRU CLNC-1000-CREATE-X
015508*        MOVE WCLI-CLI-ID                 TO WCLNC-CLI-ID
015508*        IF NOT WGLOB-COUNTRY-JAPAN
015508*            SET WCLNC-CLI-CO-GR-ALPHA    TO TRUE
015508*        ELSE
015508*            SET WCLNC-CLI-CO-GR-KATAKANA TO TRUE
015508*        END-IF
015508*        SET WCLNC-CLI-CO-NM-TYP-CRNT-EMPLR TO TRUE
015508*        MOVE LCLI-CLI-EMPLR-NM TO RCLNC-CLI-CO-ENTR-NM
015508*        PERFORM  0005-1000-BUILD-PARM-INFO
015508*            THRU 0005-1000-BUILD-PARM-INFO-X
015508*        MOVE RCLNC-CLI-CO-ENTR-NM        TO L0005-INPUT-STRING
015508*        PERFORM  0005-1000-CONVERT-STRING
015508*            THRU 0005-1000-CONVERT-STRING-X
015508*        IF  L0005-RETRN-OK
015508*            MOVE L0005-OUTPUT-STRING     TO RCLNC-CLI-CO-NM
015508*        END-IF
015508*        PERFORM  9750-UPDATE-EMPLR-PHONETIC
015508*            THRU 9750-UPDATE-EMPLR-PHONETIC-X
015508*        PERFORM  CLNC-1000-WRITE
015508*            THRU CLNC-1000-WRITE-X
015508*    END-IF.
      *
015508*    IF LCLI-CLI-PREV-EMPLR-NM NOT = SPACE
015508*        PERFORM  CLNC-1000-CREATE
015508*            THRU CLNC-1000-CREATE-X
015508*        MOVE WCLI-CLI-ID                 TO WCLNC-CLI-ID
015508*        IF NOT WGLOB-COUNTRY-JAPAN
015508*            SET WCLNC-CLI-CO-GR-ALPHA    TO TRUE
015508*        ELSE
015508*            SET WCLNC-CLI-CO-GR-KATAKANA TO TRUE
015508*        END-IF
015508*        SET WCLNC-CLI-CO-NM-TYP-PREV-EMPLR TO TRUE
015508*        MOVE LCLI-CLI-PREV-EMPLR-NM TO RCLNC-CLI-CO-ENTR-NM
015508*        PERFORM  0005-1000-BUILD-PARM-INFO
015508*            THRU 0005-1000-BUILD-PARM-INFO-X
015508*        MOVE RCLNC-CLI-CO-ENTR-NM        TO L0005-INPUT-STRING
015508*        PERFORM  0005-1000-CONVERT-STRING
015508*            THRU 0005-1000-CONVERT-STRING-X
015508*        IF  L0005-RETRN-OK
015508*            MOVE L0005-OUTPUT-STRING     TO RCLNC-CLI-CO-NM
015508*        END-IF
015508*        PERFORM  9750-UPDATE-EMPLR-PHONETIC
015508*            THRU 9750-UPDATE-EMPLR-PHONETIC-X
015508*        PERFORM  CLNC-1000-WRITE
015508*            THRU CLNC-1000-WRITE-X
015508*    END-IF.
      *
015509*    IF  LCLI-EMPLR-ADDR-1-TXT = SPACE
015509*    AND LCLI-EMPLR-ADDR-2-TXT = SPACE
015509*        GO TO 4909-SET-EMPR-NM-ADDR-X
015509*    END-IF.
      *
015509*    PERFORM  CLIA-1000-CREATE
015509*        THRU CLIA-1000-CREATE-X.
      *
015509*    MOVE WCLI-CLI-ID    TO WCLIA-CLI-ID.
      *
015509*    IF NOT WGLOB-COUNTRY-JAPAN
015509*        SET  WCLIA-CLI-ADDR-GR-ALPHA    TO TRUE
015509*    ELSE
015509*        SET  WCLIA-CLI-ADDR-GR-KATAKANA TO TRUE
015509*    END-IF.
015509*    SET WCLIA-CLI-ADDR-TYP-CRNT-EMPLR   TO TRUE
015509*    MOVE +1             TO WCLIA-CLI-ADDR-SEQ-NUM-N.
015508*    MOVE WCLIA-KEY      TO RCLIA-KEY.
015509*    MOVE   LCLI-EMPLR-ADDR-1-TXT
015509*        TO RCLIA-CLI-ADDR-LN-1-TXT.
015509*    MOVE   LCLI-EMPLR-ADDR-2-TXT
015509*        TO RCLIA-CLI-ADDR-LN-2-TXT.
      *
015508*    PERFORM  CLIA-1000-WRITE
015508*        THRU CLIA-1000-WRITE-X.
      *
015508*4909-SET-EMPR-NM-ADDR-X.
015508*    EXIT.
      *
      *------------------------
      *4910-SCAN-CLIENT-FIELDS.
      *------------------------
      *
      *    IF  RCLI-CLI-SEX-CD = 'C'
015508*        MOVE RCLI-CLI-CO-NM          TO WS-COMPANY-NAME
015508*        MOVE WS-COMPANY-NAME-FIRST25 TO RCLI-CLI-SUR-NM
015508*        MOVE WS-COMPANY-NAME-LAST25  TO RCLI-CLI-GIV-NM
015508*        MOVE RCLNC-CLI-CO-NM         TO WS-COMPANY-NAME
      *    END-IF.
      *
APEX54*    SET OLD-CLIENT-MATCHED           TO TRUE.
      *
      * IF A CLIENT NUMBER WAS ENTERED WITH THE APEX APPLICATION,
      * VERIFY THAT THE CLIENT DATA UPLOADED MATCHES THE CLIENT
      * DATA STORED ON THE CLIENT FILE
      * (CLIENT NUMBER WILL BE RESET TO SPACES IF MATCH FAILS)
      *
      *    IF  LAPUP-CLI-ID (CLI-SUB) NOT = SPACES
      *        PERFORM  4911-CLIENT-VERIFY
      *            THRU 4911-CLIENT-VERIFY-X
      *    END-IF.
      *
      * AT THIS POINT, THE CLIENT MATCHING ROUTINE MUST BE CALLED
      *
      *    IF  LAPUP-CLI-ID (CLI-SUB) = SPACES
      *        PERFORM  4912-CLIENT-MATCH
      *            THRU 4912-CLIENT-MATCH-X
      *    END-IF.
      *
      * IF THE CLIENT COULD NOT BE MATCHED, WE MUST ASSIGN A NUMBER
      *
      *    IF  LAPUP-CLI-ID (CLI-SUB) = SPACES
      *        PERFORM  4913-GET-NEW-CLIENT
      *            THRU 4913-GET-NEW-CLIENT-X
      *    END-IF.
      *
      *    MOVE LAPUP-CLI-ID (CLI-SUB)      TO WCLI-CLI-ID.
      *
      *4910-SCAN-CLIENT-FIELDS-X.
      *    EXIT.
      *
      *-------------------
      *4911-CLIENT-VERIFY.
      *-------------------
      *
      *    MOVE RCLI-REC-INFO            TO HCLI-REC-INFO.
015508*    MOVE RCLI-CLI-SUR-NM          TO L2130-LAST-NAME.
015508*    MOVE RCLI-CLI-GIV-NM          TO L2130-FIRST-INITIAL.
015508*    MOVE RCLNM-CLI-INDV-SUR-NM    TO L2130-LAST-NAME.
015508*    MOVE RCLNM-CLI-INDV-GIV-NM    TO L2130-FIRST-INITIAL.
      *    MOVE RCLI-CLI-SEX-CD          TO L2130-SEX.
      *    MOVE RCLI-CLI-BTH-DT          TO L2130-BIRTH-DATE.
      *    MOVE LAPUP-CLI-ID (CLI-SUB)   TO WCLI-CLI-ID.
      *
      *    PERFORM  CLI-1000-READ
      *        THRU CLI-1000-READ-X.
      *
      * IF CLIENT DOES NOT EXIST - USE NUMBER UPLOADED
      * OTHERWISE - NEW INFO MUST MATCH OLD INFO
      *
      *    IF  NOT WCLI-IO-OK
557020*        PERFORM  4914-NEW-CLIENT-DALG
557020*            THRU 4914-NEW-CLIENT-DALG-X
557020*        PERFORM  4914-NEW-CLIENT-KEYS
557020*            THRU 4914-NEW-CLIENT-KEYS-X
      *    ELSE
015508*        PERFORM 4915-CLNC-CLNM-PROCESS
015508*           THRU 4915-CLNC-CLNM-PROCESS-X
015508*        MOVE RCLNM-CLI-INDV-GIV-NM      TO WS-HOLD-FIRST-INITIAL
015508*        IF  RCLI-CLI-SUR-NM       = L2130-LAST-NAME
015508*        MOVE RCLNM-CLI-INDV-GIV-NM TO WS-HOLD-FIRST-INITIAL
015508*        IF  RCLNM-CLI-INDV-SUR-NM = L2130-LAST-NAME
      *        AND WS-HOLD-FIRST-INITIAL = L2130-FIRST-INITIAL
      *        AND RCLI-CLI-SEX-CD       = L2130-SEX
      *        AND RCLI-CLI-BTH-DT       = L2130-BIRTH-DATE
      *            NEXT SENTENCE
      *        ELSE
      *MSG: CLIENT NUMBER @1 ALREADY USED BY A DIFFERENT CLIENT
      *            MOVE 'AS21000032'     TO WGLOB-MSG-REF-INFO
      *            MOVE LAPUP-CLI-ID (CLI-SUB) TO WGLOB-MSG-PARM (1)
      *            PERFORM  9000-BUILD-MESSAGE-EXTRACT
      *                THRU 9000-BUILD-MESSAGE-EXTRACT-X
      *            MOVE SPACES           TO LAPUP-CLI-ID (CLI-SUB)
      *        END-IF
      *    END-IF.
      *
      *    MOVE HCLI-REC-INFO            TO RCLI-REC-INFO.
      *
      *4911-CLIENT-VERIFY-X.
      *    EXIT.
      *
      *------------------
      *4912-CLIENT-MATCH.
      *------------------
      *
      *    MOVE SPACES                 TO L2130-PARAMETERS-IN.
015508*    MOVE RCLI-CLI-SUR-NM        TO L2130-LAST-NAME.
015508*    MOVE RCLNM-CLI-INDV-SUR-NM  TO L2130-LAST-NAME.
015508*    MOVE RCLI-CLI-GIV-NM        TO L2130-FIRST-INITIAL.
015508*    MOVE RCLNM-CLI-INDV-GIV-NM  TO L2130-FIRST-INITIAL.
      *    MOVE RCLI-CLI-SEX-CD        TO L2130-SEX.
      *    MOVE RCLI-CLI-BTH-DT        TO L2130-BIRTH-DATE.
      *
      *    PERFORM  2130-0000-ALPHA-MATCH
      *        THRU 2130-0000-ALPHA-MATCH-X.
      *
      *    IF  L2130-CLIENT-DUPLICATE
      *MSG: CLIENT (@1, @2) FOUND TWICE, NEW RECORD WILL BE CREATED
      *        MOVE 'AS21000040'        TO WGLOB-MSG-REF-INFO
      *        MOVE L2130-LAST-NAME     TO WGLOB-MSG-PARM (1)
      *        MOVE L2130-FIRST-INITIAL TO WGLOB-MSG-PARM (2)
      *        PERFORM  9000-BUILD-MESSAGE-EXTRACT
      *            THRU 9000-BUILD-MESSAGE-EXTRACT-X
      *    END-IF.
      *
      *    IF  L2130-CLIENT-MATCHED
      *MSG: CLIENT (@1, @2) MATCHED WITH CLIENT# (@3) ON FILE
      *        MOVE L2130-CLIENT-NUMBER TO LAPUP-CLI-ID (CLI-SUB)
      *        MOVE L2130-CLIENT-NUMBER TO WCLI-CLI-ID
      *        MOVE 'AS21000035'        TO WGLOB-MSG-REF-INFO
      *        MOVE L2130-LAST-NAME     TO WGLOB-MSG-PARM (1)
      *        MOVE L2130-FIRST-INITIAL TO WGLOB-MSG-PARM (2)
      *        MOVE L2130-CLIENT-NUMBER TO WGLOB-MSG-PARM (3)
      *        PERFORM  9000-BUILD-MESSAGE-EXTRACT
      *            THRU 9000-BUILD-MESSAGE-EXTRACT-X
      *    END-IF.
      *
      *4912-CLIENT-MATCH-X.
      *    EXIT.
      *
      *--------------------
      *4913-GET-NEW-CLIENT.
      *--------------------
      *
APEX54*    PERFORM  0066-1000-ASSIGN-CLI-ID
APEX54*        THRU 0066-1000-ASSIGN-CLI-ID-X.
      *
APEX54*    IF  L0066-RETRN-OK
      *        MOVE L0066-CLIENT-NUMBER  TO WCLI-CLI-ID
557020*        PERFORM  4914-NEW-CLIENT-DALG
557020*            THRU 4914-NEW-CLIENT-DALG-X
557020*        PERFORM  4914-NEW-CLIENT-KEYS
557020*            THRU 4914-NEW-CLIENT-KEYS-X
      *    ELSE
      *MSG: CLIENT NUMBER ASSIGNMENT ERROR - PROGRAM ABENDED
      *        MOVE 'AS21000030'         TO WGLOB-MSG-REF-INFO
      *        PERFORM  0260-1000-GENERATE-MESSAGE
      *            THRU 0260-1000-GENERATE-MESSAGE-X
      *        PERFORM  0030-5000-LOGIC-ERROR
      *            THRU 0030-5000-LOGIC-ERROR-X
      *    END-IF.
      *
      *    IF  CLI-SUB > 0
      *        MOVE L0066-CLIENT-NUMBER  TO LAPUP-CLI-ID (CLI-SUB)
      *    END-IF.
      *
      *4913-GET-NEW-CLIENT-X.
      *    EXIT.
      *
      *---------------------
557020*4914-NEW-CLIENT-DALG.
557020*4914-NEW-CLIENT-KEYS.
      *---------------------
      *
557020* CREATE DALG RECORD
      *
557020*    MOVE SPACES                 TO L0760-PARM-INFO.
557020*    MOVE 'CRTE'                 TO L0760-DAL-ACTION.
557020*    MOVE 'CLNT'                 TO L0760-DAL-ACTIVITY.
557020*    MOVE 'C'                    TO L0760-POL-OR-CLI-TYPE.
557020*    MOVE WCLI-CLI-ID            TO L0760-POL-OR-CLI-ID.
557020*    MOVE ZERO                   TO L0760-CVG-NUM.
      *
557020*    PERFORM  0760-1000-PROCESS-DAL-ENTRY
557020*        THRU 0760-1000-PROCESS-DAL-ENTRY-X.
      *
      * RESET OTHER CLIENT RELATED KEYS
      *
      *    MOVE WCLI-CLI-ID            TO WCLIA-CLI-ID.
015508*    MOVE WCLI-CLI-ID            TO WPRNM-CLI-ID.
015508*    MOVE WCLI-CLI-ID            TO WCLNM-CLI-ID.
      *    MOVE WCLI-CLI-ID            TO WAPPF-CLI-ID.
      *    MOVE WCLI-CLI-ID            TO WAPPV-CLI-ID.
APEX54*    MOVE WCLI-CLI-ID            TO L5850-CLI-ID.
      *    MOVE WCLI-CLI-ID            TO WMEDI-CLI-ID.
557700*    MOVE WCLI-CLI-ID            TO WCLIC-CLI-ID.
557700*    MOVE WCLI-CLI-ID            TO WCLII-CLI-ID.
APEX54*    SET NEW-CLIENT-CREATED      TO TRUE.
      *
557020*4914-NEW-CLIENT-DALG-X.
557020*4914-NEW-CLIENT-KEYS-X.
      *    EXIT.
      *
      *-----------------------
015508*4915-CLNC-CLNM-PROCESS.
      *-----------------------
      *
      *    MOVE RCLI-CLI-ID      TO WCLNM-CLI-ID.
      *
      *    IF NOT WGLOB-COUNTRY-JAPAN
      *        SET WCLNM-CLI-INDV-GR-ALPHA    TO TRUE
      *    ELSE
      *        SET WCLNM-CLI-INDV-GR-KATAKANA TO TRUE
      *    END-IF.
      *
      *    SET WCLNM-CLI-INDV-NM-TYP-CRNT TO TRUE.
      *    MOVE +001             TO WCLNM-CLI-INDV-SEQ-NUM.
      *
      *    PERFORM CLNM-1000-READ
      *       THRU CLNM-1000-READ-X.
      *
      *    IF WCLNM-IO-OK
      *        GO TO 4915-CLNC-CLNM-PROCESS-X
      *    END-IF.
      *
      *    MOVE RCLI-CLI-ID      TO WCLNC-CLI-ID.
      *
      *    IF NOT WGLOB-COUNTRY-JAPAN
      *        SET WCLNC-CLI-CO-GR-ALPHA TO TRUE
      *    ELSE
      *        SET WCLNC-CLI-CO-GR-KATAKANA TO TRUE
      *    END-IF.
      *
      *    SET WCLNC-CLI-CO-NM-TYP-CLI TO TRUE.
      *
      *    PERFORM CLNC-1000-READ
      *       THRU CLNC-1000-READ-X.
      *
015508*4915-CLNC-CLNM-PROCESS-X.
      *    EXIT.
      *
      *-------------------------
      *5000-PROCESS-EACH-POLICY.
      *-------------------------
      *
      *    PERFORM  5100-INITIALIZE-POLICY-INFO
      *        THRU 5100-INITIALIZE-POLICY-INFO-X.
      *
      *    PERFORM  5200-PROCESS-POLICY-HEADER
      *        THRU 5200-PROCESS-POLICY-HEADER-X
      *        UNTIL WAPIN-END-OF-STRUCTURE
      *        OR WS-ERROR-FOUND.
      *
      *    IF  WS-ERROR-FOUND
      *        GO TO 5000-PROCESS-EACH-POLICY-X
      *    END-IF.
      *
      *    PERFORM  5300-ASSIGN-POL-ID
      *        THRU 5300-ASSIGN-POL-ID-X.
      *
      *    PERFORM  APIN-4000-GET-NEXT-STRUCTURE
      *        THRU APIN-4000-GET-NEXT-STRUCTURE-X.
      *
557700*    IF  WAPIN-END-OF-APPLICATION
557700*        GO TO 5000-PROCESS-EACH-POLICY-X
557700*    END-IF.
      *
      *    PERFORM  5500-PROCESS-EACH-POLICY-SED
      *        THRU 5500-PROCESS-EACH-POLICY-SED-X
      *        UNTIL WAPIN-END-OF-APPLICATION
      *        OR WAPIN-STRUCT-POLICY-HEADER
      *        OR WS-ERROR-FOUND.
      *
      *    IF  WS-ERROR-FOUND
      *        GO TO 5000-PROCESS-EACH-POLICY-X
      *    END-IF.
      *
      *    PERFORM  5600-FINALIZE-POLICY-INFO
      *        THRU 5600-FINALIZE-POLICY-INFO-X.
      *
      *5000-PROCESS-EACH-POLICY-X.
      *    EXIT.
      *
      *----------------------------
      *5100-INITIALIZE-POLICY-INFO.
      *----------------------------
      *
      *    MOVE SPACE                  TO WPOL-POL-ID.
      *    MOVE SPACE                  TO WCVG-POL-ID.
      *    MOVE SPACE                  TO WFC-POL-ID.
      *    MOVE SPACE                  TO WFS-POL-ID.
      *
      *    PERFORM  POL-1000-CREATE
      *        THRU POL-1000-CREATE-X.
      *
012148*    PERFORM  8240-1000-BUILD-PARM-INFO
012148*        THRU 8240-1000-BUILD-PARM-INFO-X.
      *
      *    PERFORM  CVG-1000-CREATE
      *        THRU CVG-1000-CREATE-X.
      *
      *    PERFORM  5110-INIT-CVG-INFO
      *        THRU 5110-INIT-CVG-INFO-X
007766*        VARYING RPOL-POL-CVG-REC-CTR-N FROM 20 BY -1
007766*        VARYING RPOL-POL-CVG-REC-CTR-N
007766*        FROM  WCVGM-MAX-WCVGS-NUM BY -1
      *        UNTIL RPOL-POL-CVG-REC-CTR-N < 1.
      *
      *    PERFORM  FC-1000-CREATE
      *        THRU FC-1000-CREATE-X.
      *
      *    PERFORM  FS-1000-CREATE
      *        THRU FS-1000-CREATE-X.
      *
557700*    MOVE WS-SERV-AGT-ID         TO RPOL-SERV-AGT-ID.
012148*    MOVE WS-SERV-AGT-ID         TO RPOL-WRIT-AGT-ID.
      *
012148*    PERFORM  8240-1000-BUILD-PARM-INFO
012148*        THRU 8240-1000-BUILD-PARM-INFO-X.
012148*    MOVE WS-SERV-AGT-ID         TO L8240-AGT-ID (1).
012148*    MOVE 100.00                 TO L8240-AGT-SHR-PCT (1).
      *
      *    MOVE WS-POL-ISS-LOC-CD      TO RPOL-POL-ISS-LOC-CD.
010313*    MOVE WS-SBSDRY-CO-ID        TO RPOL-SBSDRY-CO-ID.
      *    MOVE WS-POL-CTRY-CD         TO RPOL-POL-CTRY-CD.
      *    MOVE WS-POL-APP-SIGN-IND    TO RPOL-POL-APP-SIGN-IND.
      *
APEX54*    IF  RPOL-POL-APP-SIGN-ACCEPTED
010313*        IF  WS-POL-APP-SIGN-DT = WWKDT-ZERO-DT
      *            MOVE WGLOB-PROCESS-DATE TO RPOL-POL-APP-SIGN-DT
010313*        ELSE
010313*            MOVE WS-POL-APP-SIGN-DT TO RPOL-POL-APP-SIGN-DT
010313*        END-IF
      *    END-IF.
      *
557659*    MOVE WS-POL-MIB-SIGN-IND    TO RPOL-POL-MIB-SIGN-IND.
557659*    MOVE WS-POL-MIB-SIGN-CD     TO RPOL-POL-MIB-SIGN-CD.
      *    MOVE ALL 'N'                TO TUFCT-SWITCHES.
      *    MOVE SPACE                  TO WS-SAVE-SEGFUND-AREA.
      *    MOVE SPACE                  TO LAPUP-POL-TYP-CD.
      *    MOVE SPACE                  TO LCLIB-PARM-AREA.
      *
      * REPLACEMENT INDICATOR COMES IN ON AN EARLIER CLIENT SED
      *
557659*    MOVE LPOL-POL-REPL-IND      TO RPOL-POL-REPL-IND.
010303*    MOVE LPOL-POL-REPL-CD       TO RPOL-POL-REPL-CD.
      *
APEX53***  MOVE SPACES                 TO LAPUP-BENE-REL-TABLE.
APEX53*    MOVE SPACES                 TO LAPUP-POL-NOTI-TABLE.
APEX53*    MOVE SPACES                 TO LAPUP-POLC-OTHR-REL-TABLE.
010313*    MOVE SPACES                 TO WS-BENE-HOLD-AREAS.
      *
010313*    SET LAPUP-POL-REG-FND-SRC-NOT-SET TO TRUE.
      *
APEX53***  PERFORM
APEX53***      VARYING X FROM 1 BY 1
APEX53***      UNTIL X > 12
APEX53***          INITIALIZE LAPUP-BENE-PRCDS-PCT (X)
APEX53***  END-PERFORM.
      *
APEX54*    PERFORM  0953-1000-BUILD-PARM-INFO
APEX54*        THRU 0953-1000-BUILD-PARM-INFO-X.
      *
014177*
014177* THE SEGFUND CVG ON APEX WILL ALWAYS BE COVERAGE ONE
014177*
014177*    MOVE 1                      TO WS-SEGF-CVG.
014177*
014177*    INITIALIZE WS-FUND-CTR.
014178*    INITIALIZE WS-DEST-FUND-CTR.
014178*
014178*    PERFORM
014178*        VARYING X FROM 1 BY 1
014178*        UNTIL X > 25
014178*            INITIALIZE WS-DEST-FUND-DETAILS (X)
014178*    END-PERFORM.
014178*
014178*    INITIALIZE WS-PAYO-IND.
014178*    INITIALIZE WS-DIA-CVG.
014178*    INITIALIZE WS-GIA-CVG.
014178*    INITIALIZE WS-PALC-CVG-CTR.
      *
      *5100-INITIALIZE-POLICY-INFO-X.
      *    EXIT.
      *
      *-------------------
      *5110-INIT-CVG-INFO.
      *-------------------
      *
      *    MOVE RPOL-POL-CVG-REC-CTR-N
      *         TO WCVGS-CVG-SEQ-NUM-N (RPOL-POL-CVG-REC-CTR-N).
      *    MOVE RCVG-CVG-INFO
      *         TO WCVGS-CVG-INFO (RPOL-POL-CVG-REC-CTR-N).
      *    MOVE SPACE
      *         TO LAPUP-CVGC-CLI-INFO (RPOL-POL-CVG-REC-CTR-N).
      *    MOVE ZERO
      *         TO LAPUP-CVGC-LIVES-INSRD-CD (RPOL-POL-CVG-REC-CTR-N).
      *
      *5110-INIT-CVG-INFO-X.
      *    EXIT.
      *
      *---------------------------
      *5200-PROCESS-POLICY-HEADER.
      *---------------------------
      *
      *    PERFORM  APIN-3000-GET-FIELD-N-VALUE
      *        THRU APIN-3000-GET-FIELD-N-VALUE-X.
      *
      *    IF  WAPIN-ERROR-FOUND
      *    OR  WAPIN-FLD-ID = SPACE
APEX54*        SET WS-ERROR-FOUND          TO TRUE
      *        GO TO 5200-PROCESS-POLICY-HEADER-X
      *    END-IF.
      *
APEX54*    EVALUATE WAPIN-FLD-ID
      *
APEX54*        WHEN 'CLIENT_ID'
APEX54*             PERFORM  5205-PROCESS-CLIENT-ID
APEX54*                 THRU 5205-PROCESS-CLIENT-ID-X
      *
APEX54*        WHEN 'CLASS'
      *             MOVE 'N'               TO L0280-SIGN-IND
APEX54*             SET L0280-SPACES-PERMITTED TO TRUE
      *             MOVE 2                 TO L0280-LENGTH
      *             MOVE ZERO              TO L0280-PRECISION
      *             MOVE WAPIN-FLD-VALUE   TO L0280-INPUT-DATA
      *             PERFORM  0280-1000-NUMERIC-EDIT
      *                 THRU 0280-1000-NUMERIC-EDIT-X
      *             IF  L0280-OK
      *             AND L0280-OUTPUT < 3
      *                 MOVE L0280-OUTPUT  TO WS-POL-CLASS-CD
      *             ELSE
APEX54*                 SET WS-ERROR-FOUND TO TRUE
      *             END-IF
      *
APEX54*        WHEN 'PLID'
APEX54*             CONTINUE
      *
APEX54*        WHEN OTHER
APEX54*             SET WS-ERROR-FOUND     TO TRUE
      *
APEX54*    END-EVALUATE.
      *
      *5200-PROCESS-POLICY-HEADER-X.
      *    EXIT.
      *
APEX54*-----------------------
APEX54*5205-PROCESS-CLIENT-ID.
APEX54*-----------------------
      *
      *    MOVE 'N'                    TO L0280-SIGN-IND.
APEX54*    SET L0280-SPACES-PERMITTED  TO TRUE.
      *    MOVE 2                      TO L0280-LENGTH.
      *    MOVE ZERO                   TO L0280-PRECISION.
      *    MOVE WAPIN-FLD-VALUE        TO L0280-INPUT-DATA.
      *
      *    PERFORM  0280-1000-NUMERIC-EDIT
      *        THRU 0280-1000-NUMERIC-EDIT-X.
      *
      *    IF  L0280-OK
      *    AND L0280-OUTPUT > ZERO
      *    AND L0280-OUTPUT NOT > WS-CLIENTS-IN-APP
      *    AND (WS-APP-IS-SINGLE-LIFE
      *    OR  WS-APP-IS-MULTIPLE-LIFE)
      *        MOVE LAPUP-CLI-ID (L0280-OUTPUT)
      *                                TO LAPUP-CVGC-CLI-ID (1, 1)
      *        MOVE LAPUP-CLI-ID (L0280-OUTPUT)
      *                                TO WS-PRIMARY-INSD-CLI-ID
      *        MOVE 1                  TO LAPUP-CVGC-LIVES-INSRD-CD (1)
      *        GO TO 5205-PROCESS-CLIENT-ID-X
      *    ELSE
      *        PERFORM  5210-INIT-CVGC-INFO
      *            THRU 5210-INIT-CVGC-INFO-X
      *            VARYING CLI-SUB FROM +1 BY +1
      *            UNTIL CLI-SUB > WS-CLIENTS-IN-APP
      *        MOVE LAPUP-CVGC-CLI-ID (1 1)
      *                                TO WS-PRIMARY-INSD-CLI-ID
      *        IF  LAPUP-CVGC-LIVES-INSRD-CD (1) > 1
      *            MOVE 'J'            TO WCVGS-CVG-SEX-CD (1)
      *        END-IF
      *    END-IF.
      *
APEX54*5205-PROCESS-CLIENT-ID-X.
      *    EXIT.
      *
      *--------------------
      *5210-INIT-CVGC-INFO.
      *--------------------
      *
      *    IF  WS-CLI-BASE-INSURED (CLI-SUB)
      *        IF  LAPUP-CVGC-LIVES-INSRD-CD (1) < 6
      *            ADD +1              TO LAPUP-CVGC-LIVES-INSRD-CD (1)
      *            MOVE LAPUP-CVGC-LIVES-INSRD-CD (1)
      *                                TO X
      *            MOVE LAPUP-CLI-ID (CLI-SUB)
      *                                TO LAPUP-CVGC-CLI-ID (1 X)
      *        END-IF
      *    END-IF.
      *
      *5210-INIT-CVGC-INFO-X.
      *    EXIT.
      *
      *-------------------
      *5300-ASSIGN-POL-ID.
      *-------------------
      *
      *    IF  WS-HOLD-POLICY-NUM = SPACES
APEX54*        SET ASSIGN-POLNO            TO TRUE
      *    ELSE
APEX54*        SET NO-POLNO-ASSIGN         TO TRUE
      *        PERFORM  5350-POLICY-LOOKUP
      *            THRU 5350-POLICY-LOOKUP-X
      *    END-IF.
      *
      *    IF  NO-POLNO-ASSIGN
      *        MOVE WS-HOLD-POLICY-NUM     TO WPOL-POL-ID
APEX54*        GO TO 5300-ASSIGN-POL-ID-X
APEX54*    END-IF.
      *
APEX54*    PERFORM  0301-1000-BUILD-PARM-INFO
APEX54*        THRU 0301-1000-BUILD-PARM-INFO-X.
      *
APEX54*    MOVE WS-HOLD-LINE-OF-BUSINESS   TO L0301-LINE-OF-BUSINESS.
      *
APEX54*    PERFORM  0301-1000-ASGN-POL-NUM
APEX54*        THRU 0301-1000-ASGN-POL-NUM-X.
      *
APEX54*    IF  L0301-ASSIGN-INVALID
      *MSG: POLICY NUMBER ASSIGNMENT ERROR - PROGRAM ABENDED
      *        MOVE 'AS21000031'           TO WGLOB-MSG-REF-INFO
      *        PERFORM  0260-1000-GENERATE-MESSAGE
      *            THRU 0260-1000-GENERATE-MESSAGE-X
      *        PERFORM  0030-5000-LOGIC-ERROR
      *            THRU 0030-5000-LOGIC-ERROR-X
      *    ELSE
APEX54*        MOVE L0301-POLICY-NUMBER    TO WPOL-POL-ID
557700*        MOVE L0301-POLICY-NUMBER    TO RPOL-POL-ID
      *        IF  WS-HOLD-POLICY-NUM NOT = SPACES
      *MSG: POLICY NUMBER (&1) ALREADY EXISTS ... NEW NUMBER
      *     WILL BE ASSIGNED
      *            MOVE 'AS21000033'       TO WGLOB-MSG-REF-INFO
      *            MOVE WS-HOLD-POLICY-NUM TO WGLOB-MSG-PARM (1)
      *            PERFORM  9000-BUILD-MESSAGE-EXTRACT
      *                THRU 9000-BUILD-MESSAGE-EXTRACT-X
APEX54*        END-IF
APEX54*    END-IF.
      *
      *5300-ASSIGN-POL-ID-X.
      *    EXIT.
      *
      *-------------------
      *5350-POLICY-LOOKUP.
      *-------------------
      *
      *
      * VERIFY THAT THE POLICY NUMBER DOES NOT ALREADY EXIST
      *
      *    MOVE RPOL-REC-INFO          TO HPOL-REC-INFO.
      *    MOVE WS-HOLD-POLICY-NUM     TO WPOL-POL-ID.
      *
      *    PERFORM  POL-1000-READ
      *        THRU POL-1000-READ-X.
      *
      *    IF  WPOL-IO-OK
APEX54*        SET ASSIGN-POLNO        TO TRUE
APEX54*    END-IF.
      *
      *    MOVE HPOL-REC-INFO          TO RPOL-REC-INFO.
      *
      *5350-POLICY-LOOKUP-X.
      *    EXIT.
      *
      *-----------------------------
      *5500-PROCESS-EACH-POLICY-SED.
      *-----------------------------
      *
      *    IF  WAPIN-STRUCT-ID = SPACE
      *MSG: UNKNOWN @1 SED @2
APEX54*        SET WS-ERROR-FOUND      TO TRUE
      *        GO TO 5500-PROCESS-EACH-POLICY-SED-X
      *    END-IF.
      *
      *    MOVE ZERO                   TO WS-SEGF-SUB.
      *
      *    IF  WAPIN-STRUCT-ID     = 'INSURANCE'
      *    OR  WAPIN-STRUCT-ID-1-5 = 'RIDER'
      *    OR  WAPIN-STRUCT-ID     = 'DEFERRED'
      *    OR  WAPIN-STRUCT-ID-1-4 = 'SEGF'
      *    OR  WAPIN-STRUCT-ID     = 'IMMEDIATE'
      *    OR  WAPIN-STRUCT-ID     = 'DISABILITY'
      *    OR  WAPIN-STRUCT-ID     = 'UNIVLIFE'
APEX54*    OR  WAPIN-STRUCT-ID     = 'JOINTIAPI'
010313*    OR  WAPIN-STRUCT-ID     = 'BENEF'
014177*    OR  WAPIN-STRUCT-ID     = 'NEWMONEY'
014177*    OR  WAPIN-STRUCT-ID     = 'SPOUSE'
014177*    OR  WAPIN-STRUCT-ID     = 'FUND'
014178*    OR  WAPIN-STRUCT-ID     = 'DIAFUND'
014178*    OR  WAPIN-STRUCT-ID     = 'GIAFUND'
014178*    OR  WAPIN-STRUCT-ID     = 'DOLLARCOSTAVG'
014178*    OR  WAPIN-STRUCT-ID     = 'ASSETREBAL'
014178*    OR  WAPIN-STRUCT-ID     = 'DCAFUND'
014178*    OR  WAPIN-STRUCT-ID     = 'ARBFUND'
014177*    OR  WAPIN-STRUCT-ID     = 'HEALTH'
      *        PERFORM  5510-SPECIAL-POLICY-SED
      *            THRU 5510-SPECIAL-POLICY-SED-X
      *    ELSE
      *        MOVE 'N'                TO SED-CONTAINS-SPEC-FLD-IND
      *    END-IF.
      *
010313*** CVG AND POLICY LEVEL BENE SEDS HAVE THE SAME STRUCTURE
010313*    IF  WAPIN-STRUCT-ID-1-5 = 'BENEF'
010313*    OR  WAPIN-STRUCT-ID-1-5 = 'SBENE'
010313*        MOVE SPACES             TO LAPUP-BENE-REL-TABLE
010313*        PERFORM  5530-INIT-BENE-ARRAY
010313*            THRU 5530-INIT-BENE-ARRAY-X
010313*            VARYING X FROM 1 BY 1
010313*            UNTIL X GREATER THAN 12
010313*
010313*** POLICY LEVEL BENEFICIARY INFO
010313*        IF  WAPIN-STRUCT-ID = 'BENEF'
010313*            MOVE ZERO           TO WS-BENE-CVG
010313*        ELSE
010313*** CVG LEVEL BENEFICIARY INFO (SBENE?RB OR SBENE??RB)
010313*** PUT BENE ON THE CURRENT COVERAGE AS APEX RIDER NUMBERS AND
010313*** INGENIUM COVERAGE NUMBERS MAY NOT MATCH UP (BECAUSE OF
010313*** EMBEDDED BENEFITS, ETC. IN INGENIUM)
010313*            MOVE RPOL-POL-CVG-REC-CTR-N TO WS-BENE-CVG
010313*            MOVE 'BENEF'                TO WAPIN-STRUCT-ID
010313*        END-IF
010313*    END-IF.
      *
010313*    SET LAPUP-BYPASS-SED-NO             TO TRUE.
      *    PERFORM  5550-PROCESS-POLICY-FIELD
      *        THRU 5550-PROCESS-POLICY-FIELD-X
      *        UNTIL WAPIN-END-OF-STRUCTURE
      *        OR WS-ERROR-FOUND.
      *
      *    IF  WAPIN-STRUCT-ID = 'PAC'
      *    OR  WAPIN-STRUCT-ID = 'DEFPAC'
      *        PERFORM  5580-HANDLE-PAC-INFO
      *            THRU 5580-HANDLE-PAC-INFO-X
      *    END-IF.
      *
010313*** SAVE BENEFICIARY INFO
010313*    IF  WAPIN-STRUCT-ID = 'BENEF'
010313*
010313*** SAVE POLICY LEVEL BENEFICIARY INFO
010313*        IF  WS-BENE-CVG = ZERO
010313*            MOVE LAPUP-BENE-REL-TABLE TO WS-POL-BENE-HOLD-AREA
010313*        ELSE
010313*** SAVE CVG LEVEL BENEFICIARY INFO
010313*            MOVE LAPUP-BENE-REL-TABLE TO
010313*                 WS-CVG-BENE-HOLD-AREA (WS-BENE-CVG)
010313*        END-IF
010313*
010313*    END-IF.
      *
      *    PERFORM  APIN-4000-GET-NEXT-STRUCTURE
      *        THRU APIN-4000-GET-NEXT-STRUCTURE-X.
      *
      *5500-PROCESS-EACH-POLICY-SED-X.
      *    EXIT.
      *
      *------------------------
      *5510-SPECIAL-POLICY-SED.
      *------------------------
      *
APEX54*    SET SED-CONTAINS-SPECIAL-FIELD          TO TRUE.
      *
APEX54*    EVALUATE WAPIN-STRUCT-ID
      *
APEX54*        WHEN 'INSURANCE'
APEX54*             SET LAPUP-POL-TYP-LIFE         TO TRUE
APEX54*             SET WS-LIFE-POLICY             TO TRUE
      *
APEX54*        WHEN 'DISABILITY'
APEX54*             SET LAPUP-POL-TYP-DISABILITY   TO TRUE
APEX54*             SET WS-DI-POLICY               TO TRUE
      *
APEX54*        WHEN 'DEFERRED'
APEX54*             SET LAPUP-POL-TYP-DEF-ANNUITY  TO TRUE
APEX54*             SET WS-DEF-ANN-POLICY          TO TRUE
      *
APEX54*        WHEN 'IMMEDIATE'
APEX54*             SET RPOL-POL-BILL-TYP-SINGLE   TO TRUE
APEX54*             SET LAPUP-POL-TYP-IMM-ANNUITY  TO TRUE
APEX54*             SET WS-IMM-ANN-POLICY          TO TRUE
014177*
014177*        WHEN 'HEALTH'
014177*
014177* FOR PURPOSE OF POLICY ID ASSIGNMENT, USE DI LINE OF BUSINESS
014177*
014177*             SET LAPUP-POL-TYP-DISABILITY   TO TRUE
014177*             SET WS-DI-POLICY               TO TRUE
      *
APEX54*    END-EVALUATE.
      *
      *    IF  WAPIN-STRUCT-ID-1-4 = 'SEGF'
014177*    OR  WAPIN-STRUCT-ID     = 'FUND'
      *        MOVE WAPIN-STRUCT-ID                TO WS-SEGFUND-ID
      *        MOVE ZERO                           TO WS-SEGF-SUB
APEX54*        PERFORM
      *            VARYING WS-SUB FROM 1 BY 1
      *            UNTIL WS-SUB > RPOL-POL-CVG-REC-CTR-N
      *            OR (WS-SEGFUND AND WS-SAVE-APEX-SEGFUND (WS-SUB))
      *            OR (WS-SINGLE-SEGFUND
      *            AND WS-SAVE-APEX-SINGLE-SEGFUND (WS-SUB))
      *            OR (WS-REGISTERED-SEGFUND
      *            AND WS-SAVE-APEX-REG-SEGFUND (WS-SUB))
014177*            OR (WS-SF-FUND AND WS-SAVE-APEX-SF-FUND (WS-SUB))
015543*            CONTINUE
APEX54*        END-PERFORM
      *        IF  WS-SUB NOT > RPOL-POL-CVG-REC-CTR-N
      *            MOVE WS-SUB                     TO WS-CVG
      *        END-IF
      *    END-IF.
      *
014178*    IF  WAPIN-STRUCT-ID     = 'DOLLARCOSTAVG'
014178*        PERFORM  5520-CREATE-PAYO-REC
014178*            THRU 5520-CREATE-PAYO-REC-X
014178*        SET RPOLP-POL-PAYO-TYP-DCA          TO TRUE
014178*        SET WS-PAYO-DCA                     TO TRUE
014178*    END-IF.
014178*
014178*    IF  WAPIN-STRUCT-ID     = 'ASSETREBAL'
014178*        PERFORM  5520-CREATE-PAYO-REC
014178*            THRU 5520-CREATE-PAYO-REC-X
014178*        SET RPOLP-POL-PAYO-TYP-ASSET-REBAL  TO TRUE
014178*        SET WS-PAYO-AR                      TO TRUE
014178*    END-IF.
014178*
      *5510-SPECIAL-POLICY-SED-X.
      *    EXIT.
014178*
014178*---------------------
014178*5520-CREATE-PAYO-REC.
014178*---------------------
014178*
014178* BUILD POLP KEY
014178*
014178*    MOVE RPOL-POL-ID            TO WPOLP-POL-ID.
014178*    MOVE +99999                 TO WPOLP-POL-PAYO-NUM.
014178*
014178* CREATE POLP RECORD
014178*
014178*    PERFORM  POLP-1000-CREATE
014178*        THRU POLP-1000-CREATE-X.
014178*
014178*5520-CREATE-PAYO-REC-X.
014178*    EXIT.
010313*
010313*----------------------
010313*5530-INIT-BENE-ARRAY.
010313*----------------------
010313*
010313*    MOVE ZERO                   TO LAPUP-BENE-PRCDS-PCT-N (X).
010313*    MOVE 'O'                    TO LAPUP-BENE-TYP-CD (X).
010313*    MOVE 'P'                    TO LAPUP-BENE-DESGNT-CD (X).
010313*    MOVE 'N'                    TO LAPUP-BENE-MINR-IND (X).
010313*
010313*5530-INIT-BENE-ARRAY-X.
010313*    EXIT.
      *
      *--------------------------
      *5550-PROCESS-POLICY-FIELD.
      *--------------------------
      *
      *    PERFORM  APIN-3000-GET-FIELD-N-VALUE
      *        THRU APIN-3000-GET-FIELD-N-VALUE-X.
      *
010313*** WE SOMETMES WANT TO BYPASS A SED, IN PARTICULAR, ON EMBEDDED
010313*** BENEFIT PLANS WE DON'T WANT TO BE UPDATING COVERAGE INFO AS
010313*** THE EMBEDDED BENEFIT DOESN'T HAVE ITS OWN COVERAGE!
010313*    IF  LAPUP-BYPASS-SED
010313*        GO TO 5550-PROCESS-POLICY-FIELD-X
010313*    END-IF.
      *
      *    IF  SED-CONTAINS-SPECIAL-FIELD
      *        PERFORM  5551-CHECK-SPECIAL-FIELD
      *            THRU 5551-CHECK-SPECIAL-FIELD-X
      *    END-IF.
      *
      *    MOVE WAPIN-STRUCT-ID        TO WUFLD-UPLD-FLD-STRUCT-NM.
      *    MOVE WAPIN-FLD-ID           TO WUFLD-UPLD-FLD-APEX-NM.
      *
      *    PERFORM  UFLD-1000-READ
      *        THRU UFLD-1000-READ-X.
      *
014177*
014177* NO ENTRY ON UFLD FOR 'FUND/FUND', SO BYPASS CHECK
014177*
APEX54*    IF  NOT WUFLD-IO-OK
014178*        IF  WAPIN-FLD-ID = 'FUND'
014178*            PERFORM  5553-FUND-STRUCTURE
014178*                THRU 5553-FUND-STRUCTURE-X
014178*        END-IF
APEX54*        GO TO 5550-PROCESS-POLICY-FIELD-X
APEX54*    END-IF.
      *
557698*    IF  RUFLD-UPLD-FLD-TYP-UPPER-CASE
557698*        PERFORM  8300-TRANSLATE-UPPER-CASE
557698*            THRU 8300-TRANSLATE-UPPER-CASE-X
557698*    END-IF.
      *
      *    IF  RUFLD-UPLD-FLD-TYP-MESSAGE
      *        PERFORM  4553-PROCESS-MESSAGE-FLD
      *            THRU 4553-PROCESS-MESSAGE-FLD-X
APEX54*        GO TO 5550-PROCESS-POLICY-FIELD-X
APEX54*    END-IF.
      *
      *    IF  RUFLD-UPLD-FLD-TYP-UNUSED
      *        PERFORM  4554-PROCESS-UNUSED-FLD
      *            THRU 4554-PROCESS-UNUSED-FLD-X
APEX54*        GO TO 5550-PROCESS-POLICY-FIELD-X
APEX54*    END-IF.
      *
      *    PERFORM  APUP-1000-PROCESS-FIELD
      *        THRU APUP-1000-PROCESS-FIELD-X.
      *
      *    PERFORM  5558-CHECK-RESPONSE
      *        THRU 5558-CHECK-RESPONSE-X.
      *
      *    IF  SED-CONTAINS-SPECIAL-FIELD
      *        PERFORM  5559-CHECK-SPECIAL-FIELD
      *            THRU 5559-CHECK-SPECIAL-FIELD-X
      *    END-IF.
      *
      *5550-PROCESS-POLICY-FIELD-X.
      *    EXIT.
      *
      *-------------------------
      *5551-CHECK-SPECIAL-FIELD.
      *-------------------------
      *
      *    IF  WAPIN-FLD-ID = 'MESSAGE'
      *        PERFORM  9100-PROCESS-MESSAGE-FIELD
      *            THRU 9100-PROCESS-MESSAGE-FIELD-X
      *    END-IF.
      *
010313*** SECOND ANNUITANTS ARE NOW CREATED AS CLIENTS IN APEX - THE
010313*** JOINTIAPI SED IS NO LONGER USED
010313***  IF  WAPIN-STRUCT-ID = 'JOINTIAPI'
010313***  AND WAPIN-FLD-ID = 'NAME'
010313***      MOVE 'N'                    TO L0280-SIGN-IND
010313***      SET L0280-SPACES-PERMITTED  TO TRUE
010313***      MOVE 2                      TO L0280-LENGTH
010313***      MOVE ZERO                   TO L0280-PRECISION
010313***      MOVE WAPIN-FLD-VALUE        TO L0280-INPUT-DATA
010313***      PERFORM  0280-1000-NUMERIC-EDIT
010313***          THRU 0280-1000-NUMERIC-EDIT-X
010313***      IF  L0280-OK
010313***      AND L0280-OUTPUT NOT > WS-CLIENTS-IN-APP
010313***          ADD 1                TO LAPUP-CVGC-LIVES-INSRD-CD (1)
010313***          MOVE LAPUP-CVGC-LIVES-INSRD-CD (1)
010313***                                  TO X
010313***          MOVE LAPUP-CLI-ID (L0280-OUTPUT)
010313***                                  TO LAPUP-CVGC-CLI-ID (1 X)
010313***      END-IF
010313***      GO TO 5551-CHECK-SPECIAL-FIELD-X
010313***  END-IF.
      *
      *    IF  WS-IMM-ANN-POLICY
      *        PERFORM  6250-PERFORM-QUOTE-EDITS
      *            THRU 6250-PERFORM-QUOTE-EDITS-X
      *    END-IF.
      *
      *    IF  WAPIN-STRUCT-ID-1-4 = 'SEGF'
      *    AND WAPIN-FLD-VALUE NOT =  '0'
      *    AND (WAPIN-FLD-ID = 'MONEY'
      *    OR  WAPIN-FLD-ID = 'INDEXED'
      *    OR  WAPIN-FLD-ID = 'BONDS'
      *    OR  WAPIN-FLD-ID = 'SECTOR'
      *    OR  WAPIN-FLD-ID = 'PREC_METALS')
      *
      * HERE WE MUST TRANSLATE THE FIELD RECEIVED INTO A FUND.
      * THE KEY ON UTTB WILL BE PLAN CODE AND FIELD NAME.
      *
      *        MOVE WS-SAVE-APEX-SEGF-CODE (WS-CVG)
      *                                    TO WUTTB-UPLD-TTBL-TYP-ID
      *        MOVE WAPIN-FLD-ID           TO WUTTB-UPLD-TTBL-VALU-ID
      *        PERFORM  UTTB-1000-LOOKUP-UTTB
      *            THRU UTTB-1000-LOOKUP-UTTB-X
      *        IF  WUTTB-IO-OK
      *            ADD +1                  TO WS-SEGF-SUB
      *            MOVE RUTTB-UPLD-TTBL-VALU-TXT
      *                         TO WS-SEGFUND-CODE (WS-CVG, WS-SEGF-SUB)
      *        ELSE
      *MSG: UTTB ERROR: UTTB KEY (@3,@4) APEX FIELD (@1,@2)
      *            MOVE 'AS21000013'       TO WGLOB-MSG-REF-INFO
      *            MOVE WAPIN-STRUCT-ID    TO WGLOB-MSG-PARM (1)
      *            MOVE WAPIN-FLD-ID       TO WGLOB-MSG-PARM (2)
      *            MOVE WS-SAVE-APEX-SEGF-CODE (WS-CVG)
      *                                    TO WGLOB-MSG-PARM (3)
      *            MOVE WAPIN-FLD-ID       TO WGLOB-MSG-PARM (4)
      *            PERFORM  9000-BUILD-MESSAGE-EXTRACT
      *                THRU 9000-BUILD-MESSAGE-EXTRACT-X
      *        END-IF
      *    END-IF.
      *
014177*    IF  WAPIN-STRUCT-ID     = 'FUND'
014177*    AND WAPIN-FLD-ID        = 'FUND'
014177*        MOVE WAPIN-FLD-ID           TO WUTTB-UPLD-TTBL-TYP-ID
014177*        MOVE WAPIN-FLD-VALUE        TO WUTTB-UPLD-TTBL-VALU-ID
014177*        PERFORM  UTTB-1000-LOOKUP-UTTB
014177*            THRU UTTB-1000-LOOKUP-UTTB-X
014177*        IF  WUTTB-IO-OK
014177*            ADD +1                  TO WS-SEGF-SUB
014177*            ADD WS-SEGF-SUB         TO WS-FUND-CTR
014177*            MOVE RUTTB-UPLD-TTBL-VALU-TXT
014177*                  TO WS-SEGFUND-CODE (WS-SEGF-CVG, WS-FUND-CTR)
014177*        ELSE
014177*MSG: UTTB ERROR: UTTB KEY (@3,@4) APEX FIELD (@1,@2)
014177*            MOVE 'AS21000013'       TO WGLOB-MSG-REF-INFO
014177*            MOVE WAPIN-STRUCT-ID    TO WGLOB-MSG-PARM (1)
014177*            MOVE WAPIN-FLD-ID       TO WGLOB-MSG-PARM (2)
014177*            MOVE WAPIN-FLD-ID       TO WGLOB-MSG-PARM (3)
014177*            MOVE WAPIN-FLD-VALUE    TO WGLOB-MSG-PARM (4)
014177*            PERFORM  9000-BUILD-MESSAGE-EXTRACT
014177*                THRU 9000-BUILD-MESSAGE-EXTRACT-X
014177*        END-IF
014177*    END-IF.
      *
012696*    IF  WAPIN-STRUCT-ID-1-4 = 'SEGF'
012696*    AND WAPIN-FLD-VALUE NOT =  '0'
012696*    AND (WAPIN-FLD-ID = 'MONEY'
012696*    OR  WAPIN-FLD-ID = 'INDEXED'
012696*    OR  WAPIN-FLD-ID = 'BONDS'
012696*    OR  WAPIN-FLD-ID = 'SECTOR'
012696*    OR  WAPIN-FLD-ID = 'PREC_METALS')
012696*        MOVE 8                      TO L0280-LENGTH
012696*        MOVE 4                      TO L0280-PRECISION
012696*        MOVE WAPIN-FLD-VALUE        TO L0280-INPUT-DATA
012696*        PERFORM  0280-1000-NUMERIC-EDIT
012696*            THRU 0280-1000-NUMERIC-EDIT-X
012696*        IF  L0280-OK
012696*        AND WS-SEGF-SUB > 0
012696*            MOVE L0280-OUTPUT-V04
012696*                   TO WS-SEGFUND-PERCENT (WS-CVG, WS-SEGF-SUB)
012696*                   TO WS-SEGFUND-PERCENT1 (WS-CVG, WS-SEGF-SUB)
012696*        ELSE
012696*MSG: NUMERIC FIELD (@1) RECEIVED INVALID VALUE (@2)
012696*            MOVE 'AS21000014'       TO WGLOB-MSG-REF-INFO
012696*            MOVE WAPIN-FLD-ID       TO WGLOB-MSG-PARM (1)
012696*            MOVE WAPIN-FLD-VALUE    TO WGLOB-MSG-PARM (2)
012696*            PERFORM  9000-BUILD-MESSAGE-EXTRACT
012696*                THRU 9000-BUILD-MESSAGE-EXTRACT-X
012696*        END-IF
012696*    END-IF.
      *
014177*    IF  WAPIN-STRUCT-ID     = 'FUND'
014177*    AND WAPIN-FLD-ID        = 'PERCENTAGE1'
014177*    AND WAPIN-FLD-VALUE NOT = '0'
014177*        MOVE 8                      TO L0280-LENGTH
014177*        MOVE 4                      TO L0280-PRECISION
014177*        MOVE WAPIN-FLD-VALUE        TO L0280-INPUT-DATA
014177*        PERFORM  0280-1000-NUMERIC-EDIT
014177*            THRU 0280-1000-NUMERIC-EDIT-X
014177*        IF  L0280-OK
014177*        AND WS-SEGF-SUB > 0
014177*            MOVE L0280-OUTPUT-V04
014177*                   TO WS-SEGFUND-PERCENT1
014177*                      (WS-SEGF-CVG, WS-FUND-CTR)
014177*        ELSE
014177*MSG: NUMERIC FIELD (@1) RECEIVED INVALID VALUE (@2)
014177*            MOVE 'AS21000014'       TO WGLOB-MSG-REF-INFO
014177*            MOVE WAPIN-FLD-ID       TO WGLOB-MSG-PARM (1)
014177*            MOVE WAPIN-FLD-VALUE    TO WGLOB-MSG-PARM (2)
014177*            PERFORM  9000-BUILD-MESSAGE-EXTRACT
014177*                THRU 9000-BUILD-MESSAGE-EXTRACT-X
014177*        END-IF
014177*    END-IF.
      *
014177*    IF  WAPIN-STRUCT-ID     = 'FUND'
014177*    AND WAPIN-FLD-ID        = 'PERCENTAGE2'
014177*    AND WAPIN-FLD-VALUE NOT = '0'
014177*        MOVE 8                      TO L0280-LENGTH
014177*        MOVE 4                      TO L0280-PRECISION
014177*        MOVE WAPIN-FLD-VALUE        TO L0280-INPUT-DATA
014177*        PERFORM  0280-1000-NUMERIC-EDIT
014177*            THRU 0280-1000-NUMERIC-EDIT-X
014177*        IF  L0280-OK
014177*        AND WS-SEGF-SUB > 0
014177*            MOVE L0280-OUTPUT-V04
014177*                   TO WS-SEGFUND-PERCENT2
014177*                      (WS-SEGF-CVG, WS-FUND-CTR)
014177*        ELSE
014177*MSG: NUMERIC FIELD (@1) RECEIVED INVALID VALUE (@2)
014177*            MOVE 'AS21000014'       TO WGLOB-MSG-REF-INFO
014177*            MOVE WAPIN-FLD-ID       TO WGLOB-MSG-PARM (1)
014177*            MOVE WAPIN-FLD-VALUE    TO WGLOB-MSG-PARM (2)
014177*            PERFORM  9000-BUILD-MESSAGE-EXTRACT
014177*                THRU 9000-BUILD-MESSAGE-EXTRACT-X
014177*        END-IF
014177*    END-IF.
      *
010313*
010313* CONVERT FIELD NAMES ON BENEF SED TO UPPER CASE
010313*
      *
010313*    IF  WAPIN-STRUCT-ID = 'BENEF'
010313*        PERFORM  0005-1000-BUILD-PARM-INFO
010313*            THRU 0005-1000-BUILD-PARM-INFO-X
010313*        MOVE WAPIN-FLD-ID             TO L0005-INPUT-STRING
010313*        PERFORM  0005-1000-CONVERT-STRING
010313*            THRU 0005-1000-CONVERT-STRING-X
010313*        IF  L0005-RETRN-OK
010313*            MOVE L0005-OUTPUT-STRING  TO WAPIN-FLD-ID
010313*        END-IF
010313*    END-IF.
      *
014177*    IF  WAPIN-STRUCT-ID = 'NEWMONEY'
014177*    IF  WAPIN-STRUCT-ID = 'SPOUSE'
010313*    AND WAPIN-FLD-ID = 'SPOUSE'
010313*        PERFORM  5560-ADD-SPOUSE
010313*            THRU 5560-ADD-SPOUSE-X
010313*    END-IF.
      *
014178*    IF  WAPIN-STRUCT-ID     = 'DIAFUND'
014178*    AND WAPIN-FLD-ID        = 'PERCENTAGE1'
014178*    AND WAPIN-FLD-VALUE NOT = '0'
014178*        MOVE 8                      TO L0280-LENGTH
014178*        MOVE 4                      TO L0280-PRECISION
014178*        MOVE WAPIN-FLD-VALUE        TO L0280-INPUT-DATA
014178*        PERFORM  0280-1000-NUMERIC-EDIT
014178*            THRU 0280-1000-NUMERIC-EDIT-X
014178*        IF  L0280-OK
014178*            MOVE L0280-OUTPUT-V04
014178*                   TO WS-CVG-ALLOC-PCT1 (WS-DIA-CVG)
014178*        ELSE
014178*MSG: NUMERIC FIELD (@1) RECEIVED INVALID VALUE (@2)
014178*            MOVE 'AS21000014'       TO WGLOB-MSG-REF-INFO
014178*            MOVE WAPIN-FLD-ID       TO WGLOB-MSG-PARM (1)
014178*            MOVE WAPIN-FLD-VALUE    TO WGLOB-MSG-PARM (2)
014178*            PERFORM  9000-BUILD-MESSAGE-EXTRACT
014178*                THRU 9000-BUILD-MESSAGE-EXTRACT-X
014178*        END-IF
014178*    END-IF.
      *
014178*    IF  WAPIN-STRUCT-ID     = 'DIAFUND'
014178*    AND WAPIN-FLD-ID        = 'PERCENTAGE2'
014178*    AND WAPIN-FLD-VALUE NOT = '0'
014178*        MOVE 8                      TO L0280-LENGTH
014178*        MOVE 4                      TO L0280-PRECISION
014178*        MOVE WAPIN-FLD-VALUE        TO L0280-INPUT-DATA
014178*        PERFORM  0280-1000-NUMERIC-EDIT
014178*            THRU 0280-1000-NUMERIC-EDIT-X
014178*        IF  L0280-OK
014178*            MOVE L0280-OUTPUT-V04
014178*                   TO WS-CVG-ALLOC-PCT2 (WS-DIA-CVG)
014178*        ELSE
014178*MSG: NUMERIC FIELD (@1) RECEIVED INVALID VALUE (@2)
014178*            MOVE 'AS21000014'       TO WGLOB-MSG-REF-INFO
014178*            MOVE WAPIN-FLD-ID       TO WGLOB-MSG-PARM (1)
014178*            MOVE WAPIN-FLD-VALUE    TO WGLOB-MSG-PARM (2)
014178*            PERFORM  9000-BUILD-MESSAGE-EXTRACT
014178*                THRU 9000-BUILD-MESSAGE-EXTRACT-X
014178*        END-IF
014178*    END-IF.
      *
014178*    IF  WAPIN-STRUCT-ID     = 'GIAFUND'
014178*    AND WAPIN-FLD-ID        = 'PERCENTAGE1'
014178*    AND WAPIN-FLD-VALUE NOT = '0'
014178*        MOVE 8                      TO L0280-LENGTH
014178*        MOVE 4                      TO L0280-PRECISION
014178*        MOVE WAPIN-FLD-VALUE        TO L0280-INPUT-DATA
014178*        PERFORM  0280-1000-NUMERIC-EDIT
014178*            THRU 0280-1000-NUMERIC-EDIT-X
014178*        IF  L0280-OK
014178*            MOVE L0280-OUTPUT-V04
014178*                   TO WS-CVG-ALLOC-PCT1 (WS-GIA-CVG)
014178*        ELSE
014178*MSG: NUMERIC FIELD (@1) RECEIVED INVALID VALUE (@2)
014178*            MOVE 'AS21000014'       TO WGLOB-MSG-REF-INFO
014178*            MOVE WAPIN-FLD-ID       TO WGLOB-MSG-PARM (1)
014178*            MOVE WAPIN-FLD-VALUE    TO WGLOB-MSG-PARM (2)
014178*            PERFORM  9000-BUILD-MESSAGE-EXTRACT
014178*                THRU 9000-BUILD-MESSAGE-EXTRACT-X
014178*        END-IF
014178*    END-IF.
      *
014178*    IF  WAPIN-STRUCT-ID     = 'GIAFUND'
014178*    AND WAPIN-FLD-ID        = 'PERCENTAGE2'
014178*    AND WAPIN-FLD-VALUE NOT = '0'
014178*        MOVE 8                      TO L0280-LENGTH
014178*        MOVE 4                      TO L0280-PRECISION
014178*        MOVE WAPIN-FLD-VALUE        TO L0280-INPUT-DATA
014178*        PERFORM  0280-1000-NUMERIC-EDIT
014178*            THRU 0280-1000-NUMERIC-EDIT-X
014178*        IF  L0280-OK
014178*            MOVE L0280-OUTPUT-V04
014178*                   TO WS-CVG-ALLOC-PCT2 (WS-GIA-CVG)
014178*        ELSE
014178*MSG: NUMERIC FIELD (@1) RECEIVED INVALID VALUE (@2)
014178*            MOVE 'AS21000014'       TO WGLOB-MSG-REF-INFO
014178*            MOVE WAPIN-FLD-ID       TO WGLOB-MSG-PARM (1)
014178*            MOVE WAPIN-FLD-VALUE    TO WGLOB-MSG-PARM (2)
014178*            PERFORM  9000-BUILD-MESSAGE-EXTRACT
014178*                THRU 9000-BUILD-MESSAGE-EXTRACT-X
014178*        END-IF
014178*    END-IF.
      *
014178*    IF  (WAPIN-STRUCT-ID     = 'DCAFUND'
014178*    OR   WAPIN-STRUCT-ID     = 'ARBFUND')
014178*    AND WAPIN-FLD-ID         = 'FUND'
014178*        MOVE WAPIN-FLD-ID           TO WUTTB-UPLD-TTBL-TYP-ID
014178*        MOVE WAPIN-FLD-VALUE        TO WUTTB-UPLD-TTBL-VALU-ID
014178*        PERFORM  UTTB-1000-LOOKUP-UTTB
014178*            THRU UTTB-1000-LOOKUP-UTTB-X
014178*        IF  WUTTB-IO-OK
014177*            ADD +1                  TO WS-DEST-FUND-CTR
014178*            MOVE RUTTB-UPLD-TTBL-VALU-TXT
014178*                  TO WS-DEST-FUND (WS-DEST-FUND-CTR)
014178*        ELSE
014178*MSG: UTTB ERROR: UTTB KEY (@3,@4) APEX FIELD (@1,@2)
014178*            MOVE 'AS21000013'       TO WGLOB-MSG-REF-INFO
014178*            MOVE WAPIN-STRUCT-ID    TO WGLOB-MSG-PARM (1)
014178*            MOVE WAPIN-FLD-ID       TO WGLOB-MSG-PARM (2)
014178*            MOVE WAPIN-FLD-ID       TO WGLOB-MSG-PARM (3)
014178*            MOVE WAPIN-FLD-VALUE    TO WGLOB-MSG-PARM (4)
014178*            PERFORM  9000-BUILD-MESSAGE-EXTRACT
014178*                THRU 9000-BUILD-MESSAGE-EXTRACT-X
014178*        END-IF
014178*    END-IF.
      *
014178*    IF  (WAPIN-STRUCT-ID     = 'DCAFUND'
014178*    OR   WAPIN-STRUCT-ID     = 'ARBFUND')
014178*    AND WAPIN-FLD-ID         = 'PERCENTAGE'
014178*    AND WAPIN-FLD-VALUE NOT  = '0'
014178*        IF  NOT WS-DEST-FUND-CTR > 0
014178*MSG: NO FUNDS HAVE BEEN SELECTED FOR THIS POLICY
014178*            MOVE 'AS21000028'       TO WGLOB-MSG-REF-INFO
014178*            PERFORM  9000-BUILD-MESSAGE-EXTRACT
014178*                THRU 9000-BUILD-MESSAGE-EXTRACT-X
014178*            GO TO 5551-CHECK-SPECIAL-FIELD-X
014178*        END-IF
014178*        MOVE 8                      TO L0280-LENGTH
014178*        MOVE 4                      TO L0280-PRECISION
014178*        MOVE WAPIN-FLD-VALUE        TO L0280-INPUT-DATA
014178*        PERFORM  0280-1000-NUMERIC-EDIT
014178*            THRU 0280-1000-NUMERIC-EDIT-X
014178*        IF  L0280-OK
014178*            MOVE L0280-OUTPUT-V04
014178*                   TO WS-DEST-PERCENT (WS-DEST-FUND-CTR)
014178*        ELSE
014178*MSG: NUMERIC FIELD (@1) RECEIVED INVALID VALUE (@2)
014178*            MOVE 'AS21000014'       TO WGLOB-MSG-REF-INFO
014178*            MOVE WAPIN-FLD-ID       TO WGLOB-MSG-PARM (1)
014178*            MOVE WAPIN-FLD-VALUE    TO WGLOB-MSG-PARM (2)
014178*            PERFORM  9000-BUILD-MESSAGE-EXTRACT
014178*                THRU 9000-BUILD-MESSAGE-EXTRACT-X
014178*        END-IF
014178*    END-IF.
      *
014178*    IF  (WAPIN-STRUCT-ID     = 'DCAFUND'
014178*    OR   WAPIN-STRUCT-ID     = 'ARBFUND')
014178*    AND WAPIN-FLD-ID         = 'AMOUNT'
014178*    AND WAPIN-FLD-VALUE NOT  = '0'
014178*        IF  NOT WS-DEST-FUND-CTR > 0
014178*MSG: NO DESTINATION FUNDS HAVE BEEN SELECTED FOR THIS POLICY
014178*            MOVE 'AS21000028'       TO WGLOB-MSG-REF-INFO
014178*            PERFORM  9000-BUILD-MESSAGE-EXTRACT
014178*                THRU 9000-BUILD-MESSAGE-EXTRACT-X
014178*            GO TO 5551-CHECK-SPECIAL-FIELD-X
014178*        END-IF
014178*        MOVE 11                     TO L0280-LENGTH
014178*        MOVE 2                      TO L0280-PRECISION
014178*        MOVE WAPIN-FLD-VALUE        TO L0280-INPUT-DATA
014178*        PERFORM  0280-1000-NUMERIC-EDIT
014178*            THRU 0280-1000-NUMERIC-EDIT-X
014178*        IF  L0280-OK
014178*            COMPUTE WS-DEST-AMOUNT (WS-DEST-FUND-CTR)
014178*                  = L0280-OUTPUT / (10 ** L0280-PRECISION)
014178*        ELSE
014178*MSG: NUMERIC FIELD (@1) RECEIVED INVALID VALUE (@2)
014178*            MOVE 'AS21000014'       TO WGLOB-MSG-REF-INFO
014178*            MOVE WAPIN-FLD-ID       TO WGLOB-MSG-PARM (1)
014178*            MOVE WAPIN-FLD-VALUE    TO WGLOB-MSG-PARM (2)
014178*            PERFORM  9000-BUILD-MESSAGE-EXTRACT
014178*                THRU 9000-BUILD-MESSAGE-EXTRACT-X
014178*        END-IF
014178*    END-IF.
      *
      *5551-CHECK-SPECIAL-FIELD-X.
      *    EXIT.
      *
      *--------------------
      *5552-EDIT-CLIENT-ID.
      *--------------------
      *
      *    MOVE 'N'                    TO L0280-SIGN-IND.
APEX54*    SET L0280-SPACES-PERMITTED  TO TRUE.
      *    MOVE 10                     TO L0280-LENGTH.
      *    MOVE ZERO                   TO L0280-PRECISION.
      *    MOVE WAPIN-FLD-VALUE        TO L0280-INPUT-DATA.
      *
      *    PERFORM  0280-1000-NUMERIC-EDIT
      *        THRU 0280-1000-NUMERIC-EDIT-X.
      *
      *    IF  L0280-OK
      *    AND L0280-OUTPUT = CLI-SUB
      *        MOVE SPACE              TO LAPUP-CLI-ID (CLI-SUB)
      *    ELSE
      *        MOVE WAPIN-FLD-VALUE    TO LAPUP-CLI-ID (CLI-SUB)
      *    END-IF.
      *
      *5552-EDIT-CLIENT-ID-X.
      *    EXIT.
      *
014178*--------------------
014178*5553-FUND-STRUCTURE.
014178*--------------------
014178*
014178*    INITIALIZE RUFLD-REC-INFO.
014178*
014178*    IF  SED-CONTAINS-SPECIAL-FIELD
014178*        PERFORM  5559-CHECK-SPECIAL-FIELD
014178*            THRU 5559-CHECK-SPECIAL-FIELD-X
014178*    END-IF.
014178*
014178*5553-FUND-STRUCTURE-X.
014178*    EXIT.
014178*
      *--------------------
      *5558-CHECK-RESPONSE.
      *--------------------
      *
      *    IF  NOT LAPUP-GOOD-RETURN
      *        PERFORM  9200-APUP-RETURN-ERROR
      *            THRU 9200-APUP-RETURN-ERROR-X
      *        IF  RUFLD-UPLD-FLD-NM = 'PLAN_ID'
APEX54*            SET LAPUP-INVALID-PLAN  TO TRUE
      *        END-IF
      *    END-IF.
      *
      *5558-CHECK-RESPONSE-X.
      *    EXIT.
      *
      *-------------------------
      *5559-CHECK-SPECIAL-FIELD.
      *-------------------------
      *
      *    IF  WAPIN-STRUCT-ID = 'DEFERRED'
      *    AND RPOL-POL-CVG-REC-CTR-N > ZERO
      *    AND WCVGS-CVG-INS-TYP-SEG-FUND (RPOL-POL-CVG-REC-CTR-N)
      *    AND WS-SAVE-APEX-SEGF-CODE (RPOL-POL-CVG-REC-CTR-N) = SPACE
      *        MOVE WAPIN-FLD-VALUE
      *             TO WS-SAVE-APEX-SEGF-CODE (RPOL-POL-CVG-REC-CTR-N)
      *    END-IF.
      *
014177*    IF  WAPIN-STRUCT-ID = 'FUND'
014177*    AND WAPIN-FLD-ID    = 'FUND'
014177*    AND WCVGS-CVG-INS-TYP-SEG-FUND (1)
014177*    AND WS-SAVE-APEX-SEGF-CODE (1) = SPACE
014177*        MOVE WAPIN-FLD-VALUE    TO WS-SAVE-APEX-SEGF-CODE (1)
014177*    END-IF.
014178*
014178*    IF  WAPIN-STRUCT-ID = 'DIAFUND'
014178*    AND WAPIN-FLD-ID    = 'FUND'
014178*        MOVE RPOL-POL-CVG-REC-CTR-N TO WS-DIA-CVG
014178*    END-IF.
014178*
014178*    IF  WAPIN-STRUCT-ID = 'GIAFUND'
014178*    AND WAPIN-FLD-ID    = 'FUND'
014178*        MOVE RPOL-POL-CVG-REC-CTR-N TO WS-GIA-CVG
014178*    END-IF.
      *
      *5559-CHECK-SPECIAL-FIELD-X.
      *    EXIT.
010313*
010313*----------------
010313*5560-ADD-SPOUSE.
010313*----------------
010313*
010313*    PERFORM
010313*        VARYING X FROM 1 BY 1
010313*        UNTIL X > 4
010313*        OR LAPUP-POLC-OTHR-REL-INFO (X) = SPACE
015543*        CONTINUE
010313*    END-PERFORM.
010313*
010313*    IF  X > 4
010313*        MOVE 'AS21000068'       TO WGLOB-MSG-REF-INFO
010313*        PERFORM  9000-BUILD-MESSAGE-EXTRACT
010313*            THRU 9000-BUILD-MESSAGE-EXTRACT-X
010313*        GO TO 5560-ADD-SPOUSE-X
010313*    END-IF.
010313*
010313*    MOVE 'N'                    TO L0280-SIGN-IND.
010313*    SET L0280-SPACES-PERMITTED  TO TRUE.
010313*    MOVE 3                      TO L0280-LENGTH.
010313*    MOVE ZERO                   TO L0280-PRECISION.
010313*    MOVE WAPIN-FLD-VALUE        TO L0280-INPUT-DATA.
010313*
010313*    PERFORM  0280-1000-NUMERIC-EDIT
010313*        THRU 0280-1000-NUMERIC-EDIT-X.
010313*
010313*    IF  L0280-OK
010313*    AND L0280-OUTPUT > ZERO
010313*    AND L0280-OUTPUT NOT > WS-CLIENTS-IN-APP
010313*        NEXT SENTENCE
010313*    ELSE
010313*MSG: INVALID CLIENT COUNT, OR GREATER THAN CLIENTS-IN-APP
010313*        MOVE 'AS21000063'       TO WGLOB-MSG-REF-INFO
010313*        PERFORM  0260-1000-GENERATE-MESSAGE
010313*            THRU 0260-1000-GENERATE-MESSAGE-X
010313*        SET WS-ERROR-FOUND      TO TRUE
010313*        GO TO 5560-ADD-SPOUSE-X
010313*    END-IF.
010313*
010313*    MOVE LAPUP-CLI-ID (L0280-OUTPUT)
010313*                          TO LAPUP-POLC-OTHR-CLI-ID (X).
010313*    MOVE 'S'              TO LAPUP-POLC-OTHR-INSRD-REL-CD (X).
010313*    MOVE 'PR'             TO LAPUP-POLC-OTHR-ADDR-TYP (X).
010313*
010313*5560-ADD-SPOUSE-X.
010313*    EXIT.
      *
      *---------------------
      *5580-HANDLE-PAC-INFO.
      *---------------------
      *
010313*    IF  WAPIN-STRUCT-ID = 'DEFPAC'
010313*        MOVE '4'                    TO RPOL-POL-BILL-TYP-CD
010313*    END-IF.
      *
APEX54*    SET WS-PAC-CLIENT-NOT-MATCHED   TO TRUE.
      *
      *    PERFORM  5581-MATCH-CLIENT
      *        THRU 5581-MATCH-CLIENT-X.
      *
      *    IF  WS-PAC-CLIENT-NOT-MATCHED
      *        PERFORM  5588-PAC-ERROR-MESSAGE
      *            THRU 5588-PAC-ERROR-MESSAGE-X
      *        GO TO 5580-HANDLE-PAC-INFO-X
      *    END-IF.
      *
APEX54*    PERFORM
      *        VARYING X FROM 1 BY 1
      *        UNTIL X > 4
      *        OR LAPUP-POLC-OTHR-REL-INFO (X) = SPACE
015543*        CONTINUE
APEX54*    END-PERFORM.
      *
      *    IF  X > 4
      *        PERFORM  5588-PAC-ERROR-MESSAGE
      *            THRU 5588-PAC-ERROR-MESSAGE-X
      *        GO TO 5580-HANDLE-PAC-INFO-X
      *    END-IF.
      *
      *    MOVE LCLIB-CLI-PAC-BNK-ID       TO WCLIB-BNK-ID.
      *    MOVE LCLIB-PAC-BNK-BR-ID        TO WCLIB-BNK-BR-ID.
      *    MOVE LCLIB-CLI-PAC-ACCT-ID      TO WCLIB-BNK-ACCT-ID.
APEX54*    MOVE L2437-CLI-ID               TO WCLIB-CLI-ID.
      *
      *    PERFORM  CLIB-1000-READ
      *        THRU CLIB-1000-READ-X.
      *
      *    IF  NOT WCLIB-IO-OK
      *        PERFORM  5584-BANK-RECORD-CREATE
      *            THRU 5584-BANK-RECORD-CREATE-X
      *    ELSE
      *        MOVE RCLIB-CLI-BNK-ACCT-NUM TO WS-BNK-ACCT-NUM
      *        MOVE WS-BNK-ACCT-NUM  TO LAPUP-POLC-OTHR-CLI-TYP-CD (X)
      *    END-IF.
      *
      *    IF  LAPUP-POL-TYP-IMM-ANNUITY
      *        MOVE SPACE            TO LAPUP-POLC-OTHR-CLI-TYP-CD (X)
      *    ELSE
APEX54*        MOVE L2437-CLI-ID     TO LAPUP-POLC-OTHR-CLI-ID (X)
      *        MOVE 'P'              TO LAPUP-POLC-OTHR-INSRD-REL-CD (X)
      *        MOVE 'PR'             TO LAPUP-POLC-OTHR-ADDR-TYP (X)
      *    END-IF.
      *
      *5580-HANDLE-PAC-INFO-X.
      *    EXIT.
      *
      *------------------
      *5581-MATCH-CLIENT.
      *------------------
      *
      *    PERFORM  5582-CHECK-CLI-REC
      *        THRU 5582-CHECK-CLI-REC-X
      *        VARYING CLI-SUB FROM 1 BY 1
      *        UNTIL CLI-SUB > WS-CLIENTS-IN-APP
      *        OR WS-PAC-CLIENT-MATCHED.
      *
      *5581-MATCH-CLIENT-X.
      *    EXIT.
      *
      *-------------------
      *5582-CHECK-CLI-REC.
      *-------------------
      *
APEX54*    MOVE LAPUP-CLI-ID (CLI-SUB)     TO L2437-CLI-ID.
      *
APEX54*    PERFORM  2437-1000-OBTAIN-CLI-INFO
APEX54*        THRU 2437-1000-OBTAIN-CLI-INFO-X.
      *
APEX54*    IF  L2437-RETRN-OK
      *        PERFORM  5583-CHECK-NAME-MATCH
      *            THRU 5583-CHECK-NAME-MATCH-X
      *    END-IF.
      *
      *5582-CHECK-CLI-REC-X.
      *    EXIT.
      *
      *----------------------
      *5583-CHECK-NAME-MATCH.
      *----------------------
      *
APEX54*    IF  LCLIB-PAC-FIRST-NM = L2437-CLI-GIV-NM
APEX54*    AND LCLIB-PAC-LAST-NM  = L2437-CLI-SUR-NM
      *        IF  LCLIB-PAC-INIT-NM = SPACE
      *        OR  (LCLIB-PAC-INIT-NM NOT = SPACE
APEX54*        AND LCLIB-PAC-INIT-NM = L2437-CLI-MID-INIT-NM)
APEX54*            SET WS-PAC-CLIENT-MATCHED   TO TRUE
      *        END-IF
      *    END-IF.
      *
      *5583-CHECK-NAME-MATCH-X.
      *    EXIT.
      *
      *------------------------
      *5584-BANK-RECORD-CREATE.
      *------------------------
      *
      *    MOVE LCLIB-CLI-PAC-BNK-ID       TO WBNKA-BNK-ID.
      *    MOVE LCLIB-PAC-BNK-BR-ID        TO WBNKA-BNK-BR-ID.
      *    MOVE LCLIB-CLI-PAC-ACCT-ID      TO WBNKA-BNK-ACCT-ID.
      *
      *    PERFORM  BNKA-1000-READ
      *        THRU BNKA-1000-READ-X.
      *
      *    IF  WBNKA-IO-OK
      *        PERFORM  5585-CLIB-RECORD-CREATE
      *            THRU 5585-CLIB-RECORD-CREATE-X
      *        GO TO 5584-BANK-RECORD-CREATE-X
      *    END-IF.
      *
APEX54*    IF  RPCOM-CO-BNK-EDIT-TBL
      *        MOVE LCLIB-CLI-PAC-BNK-ID   TO WBNKB-BNK-ID
      *        MOVE LCLIB-PAC-BNK-BR-ID    TO WBNKB-BNK-BR-ID
      *        PERFORM  BNKB-1000-READ
      *            THRU BNKB-1000-READ-X
      *        IF  NOT WBNKB-IO-OK
      *MSG: UNABLE TO CREATE CLIB RECORD, NO BNKB RECORD EXISTS
      *            MOVE 'AS21000008'       TO WGLOB-MSG-REF-INFO
      *            PERFORM  9000-BUILD-MESSAGE-EXTRACT
      *                THRU 9000-BUILD-MESSAGE-EXTRACT-X
      *            GO TO 5584-BANK-RECORD-CREATE-X
APEX54*        END-IF
APEX54*        IF  RBNKB-BNK-BR-STAT-CLOSED
      *MSG: UNABLE TO CREATE CLIB RECORD, BANK RECORD CLOSED
      *            MOVE 'AS21000012'       TO WGLOB-MSG-REF-INFO
      *            PERFORM  9000-BUILD-MESSAGE-EXTRACT
      *                THRU 9000-BUILD-MESSAGE-EXTRACT-X
      *            GO TO 5584-BANK-RECORD-CREATE-X
      *        END-IF
      *    END-IF.
      *
      *    PERFORM  BNKA-1000-CREATE
      *        THRU BNKA-1000-CREATE-X.
      *
      *    MOVE LCLIB-CLI-PAC-BNK-ID       TO WBNKA-BNK-ID.
      *    MOVE LCLIB-PAC-BNK-BR-ID        TO WBNKA-BNK-BR-ID.
      *    MOVE LCLIB-CLI-PAC-ACCT-ID      TO WBNKA-BNK-ACCT-ID.
      *
      *    IF  LCLIB-PAC-ACCT-TYP-CD > SPACES
      *        MOVE LCLIB-PAC-ACCT-TYP-CD  TO RBNKA-BNK-ACCT-TYP-CD
      *    END-IF.
      *
      *    IF  LCLIB-CLI-PAC-MICR-IND > SPACES
      *        MOVE LCLIB-CLI-PAC-MICR-IND TO RBNKA-BNK-ACCT-MICR-IND
      *    END-IF.
      *
557700*    MOVE LCLIB-PAC-FIRST-NM         TO L0015-COMP-AREA-IN-FIRST.
557700*    MOVE LCLIB-PAC-LAST-NM          TO L0015-COMP-AREA-IN-LAST.
      *
557700*    PERFORM  0015-1000-COMPRESS-BLANKS
557700*        THRU 0015-1000-COMPRESS-BLANKS-X.
      *
557700*    MOVE L0015-COMP-AREA-OUT        TO RBNKA-BNK-ACCT-HLDR-NM.
      *
015508*    INITIALIZE                      L0620-INPUT-PARM-INFO.
557700*    MOVE LCLIB-PAC-FIRST-NM         TO L0620-CLI-GIV-NM.
557700*    MOVE LCLIB-PAC-LAST-NM          TO L0620-CLI-SUR-NM.
      *
557700*    PERFORM  0620-1000-FORMAT-SCREEN-NAME
557700*        THRU 0620-1000-FORMAT-SCREEN-NAME-X.
      *
557700*    MOVE L0620-SCREEN-NAME          TO RBNKA-BNK-ACCT-HLDR-NM.
      *
      *    PERFORM  BNKA-1000-WRITE
      *        THRU BNKA-1000-WRITE-X.
      *
      *    PERFORM  5585-CLIB-RECORD-CREATE
      *        THRU 5585-CLIB-RECORD-CREATE-X.
      *
      *    IF  WS-SUB > 9
      *        GO TO 5584-BANK-RECORD-CREATE-X
      *    END-IF.
      *
      *5584-BANK-RECORD-CREATE-X.
      *    EXIT.
      *
      *------------------------
      *5585-CLIB-RECORD-CREATE.
      *------------------------
      *
      *    MOVE 0                          TO WCLIN-IO-STATUS.
      *    MOVE 1                          TO WS-SUB.
      *
      *    PERFORM  5586-CLIB-BNK-NUM-FIND
      *        THRU 5586-CLIB-BNK-NUM-FIND-X
      *        UNTIL NOT WCLIN-IO-OK
      *        OR WS-SUB > 9.
      *
      *    IF  WS-SUB > 9
      *MSG: UNABLE TO CREATE CLIB RECORD, ALREADY 9 OCCURANCES
      *        MOVE 'AS21000009'           TO WGLOB-MSG-REF-INFO
      *        PERFORM  9000-BUILD-MESSAGE-EXTRACT
      *            THRU 9000-BUILD-MESSAGE-EXTRACT-X
      *        GO TO 5585-CLIB-RECORD-CREATE-X
      *    END-IF.
      *
      *    PERFORM  CLIB-1000-CREATE
      *        THRU CLIB-1000-CREATE-X.
      *
      *    MOVE LCLIB-CLI-PAC-BNK-ID       TO WCLIB-BNK-ID.
      *    MOVE LCLIB-PAC-BNK-BR-ID        TO WCLIB-BNK-BR-ID.
      *    MOVE LCLIB-CLI-PAC-ACCT-ID      TO WCLIB-BNK-ACCT-ID.
557660*    MOVE RCLI-CLI-ID                TO WCLIB-CLI-ID.
557660*    MOVE L2437-CLI-ID               TO WCLIB-CLI-ID.
      *
      *    MOVE WS-SUB                     TO RCLIB-CLI-BNK-ACCT-NUM.
      *
      *    PERFORM  CLIB-1000-WRITE
      *        THRU CLIB-1000-WRITE-X.
      *
      *    MOVE RCLIB-CLI-BNK-ACCT-NUM     TO WS-BNK-ACCT-NUM.
      *    MOVE WS-BNK-ACCT-NUM      TO LAPUP-POLC-OTHR-CLI-TYP-CD (X).
      *
      *5585-CLIB-RECORD-CREATE-X.
      *    EXIT.
      *
      *-----------------------
      *5586-CLIB-BNK-NUM-FIND.
      *-----------------------
      *
557660*    MOVE RCLI-CLI-ID                TO WCLIN-CLI-ID.
557660*    MOVE L2437-CLI-ID               TO WCLIN-CLI-ID.
      *    MOVE WS-SUB                     TO WCLIN-CLI-BNK-ACCT-NUM.
      *
      *    PERFORM  CLIN-1000-READ
      *        THRU CLIN-1000-READ-X.
      *
      *    IF  WCLIN-IO-OK
      *        ADD 1                       TO WS-SUB
      *    END-IF.
      *
      *5586-CLIB-BNK-NUM-FIND-X.
      *    EXIT.
      *
      *-----------------------
      *5588-PAC-ERROR-MESSAGE.
      *-----------------------
      *
      *    IF  WS-PAC-CLIENT-NOT-MATCHED
APEX54*MSG: PAC INFO DOESN'T MATCH A CLIENT
      *        MOVE 'AS21000067'           TO WGLOB-MSG-REF-INFO
      *    ELSE
      *        IF X > 4
APEX54*MSG: ALREADY 4 'OTHER' RELATIONSHIPS
      *            MOVE 'AS21000068'       TO WGLOB-MSG-REF-INFO
      *        END-IF
      *    END-IF.
      *
      *    PERFORM  9000-BUILD-MESSAGE-EXTRACT
      *        THRU 9000-BUILD-MESSAGE-EXTRACT-X.
      *
      *    MOVE 'AS21000048'               TO WGLOB-MSG-REF-INFO.
      *    MOVE 'BRANCH CODE'              TO WGLOB-MSG-PARM (1).
      *    MOVE LCLIB-PAC-BNK-BR-ID        TO WGLOB-MSG-PARM (2).
      *
      *    PERFORM  9000-BUILD-MESSAGE-EXTRACT
      *        THRU 9000-BUILD-MESSAGE-EXTRACT-X.
      *
      *    MOVE 'BANK CODE  '              TO WGLOB-MSG-PARM (1).
      *    MOVE LCLIB-CLI-PAC-BNK-ID       TO WGLOB-MSG-PARM (2).
      *
      *    PERFORM  9000-BUILD-MESSAGE-EXTRACT
      *        THRU 9000-BUILD-MESSAGE-EXTRACT-X.
      *
      *    MOVE 'ACCT TYPE  '              TO WGLOB-MSG-PARM (1).
      *    MOVE LCLIB-PAC-ACCT-TYP-CD      TO WGLOB-MSG-PARM (2).
      *
      *    PERFORM  9000-BUILD-MESSAGE-EXTRACT
      *        THRU 9000-BUILD-MESSAGE-EXTRACT-X.
      *
      *    MOVE 'MICR ENCODE'              TO WGLOB-MSG-PARM (1).
      *    MOVE LCLIB-CLI-PAC-MICR-IND     TO WGLOB-MSG-PARM (2).
      *
      *    PERFORM  9000-BUILD-MESSAGE-EXTRACT
      *        THRU 9000-BUILD-MESSAGE-EXTRACT-X.
      *
      *    MOVE 'ACCT NUMBER'              TO WGLOB-MSG-PARM (1).
      *    MOVE LCLIB-CLI-PAC-ACCT-ID      TO WGLOB-MSG-PARM (2).
      *
      *    PERFORM  9000-BUILD-MESSAGE-EXTRACT
      *        THRU 9000-BUILD-MESSAGE-EXTRACT-X.
      *
      *5588-PAC-ERROR-MESSAGE-X.
      *    EXIT.
      *
      *--------------------------
      *5600-FINALIZE-POLICY-INFO.
      *--------------------------
      *
557700*    PERFORM  5610-GET-SERV-AGT
557700*        THRU 5610-GET-SERV-AGT-X.
      *
      *    IF  RPOL-POL-ISS-EFF-DT = WWKDT-ZERO-DT
      *        IF  RPOL-POL-APP-SIGN-DT = WWKDT-ZERO-DT
      *            MOVE WGLOB-PROCESS-DATE   TO RPOL-POL-ISS-EFF-DT
      *        ELSE
      *            MOVE RPOL-POL-APP-SIGN-DT TO RPOL-POL-ISS-EFF-DT
      *        END-IF
      *    END-IF.
      *
      *    PERFORM  5650-CHECK-CVGS
      *        THRU 5650-CHECK-CVGS-X
      *        VARYING X FROM 1 BY 1
      *        UNTIL X > RPOL-POL-CVG-REC-CTR-N.
      *
      *    IF  WS-POL-IS-BASE-POLICY
      *        MOVE WPOL-POL-ID              TO WS-CONN-OPTL-POL-ID
      *    END-IF.
      *
      *    IF  WS-POL-IS-OPTIONAL-POLICY
      *    AND WS-CONN-OPTL-POL-ID > SPACES
      *        PERFORM  5800-CONN-BASE-POL-UPDATE
      *            THRU 5800-CONN-BASE-POL-UPDATE-X
      *    END-IF.
      *
      *    PERFORM  POL-1000-WRITE
      *        THRU POL-1000-WRITE-X.
      *
012148*    PERFORM  8240-2000-UPDATE-POLW
012148*        THRU 8240-2000-UPDATE-POLW-X.
      *
APEX54*    SET L0953-NOT-FINAL-CALL          TO TRUE.
      *
APEX54*
APEX54* POLICY MUST BE LOCKED BEFORE CALLING EDIT MODULE CSRF0953
APEX54*
APEX54*    PERFORM  POL-1000-READ-FOR-UPDATE
APEX54*        THRU POL-1000-READ-FOR-UPDATE-X.
      *
APEX54*    PERFORM  6600-POLICY-EDITS
APEX54*        THRU 6600-POLICY-EDITS-X.
      *
APEX54*    SET TUFCT-FILE-CHANGED (TUFCT-POL-SUB) TO TRUE.
      * CREATE DALG RECORD
557020*    MOVE SPACES                       TO L0760-PARM-INFO.
557020*    MOVE 'CRTE'                       TO L0760-DAL-ACTION.
557020*    MOVE 'POLCY'                      TO L0760-DAL-ACTIVITY.
557020*    MOVE 'P'                          TO L0760-POL-OR-CLI-TYPE.
557020*    MOVE WPOL-POL-ID                  TO L0760-POL-OR-CLI-ID.
557020*    MOVE ZERO                         TO L0760-CVG-NUM.
      *
557020*    PERFORM  0760-1000-PROCESS-DAL-ENTRY
557020*        THRU 0760-1000-PROCESS-DAL-ENTRY-X.
      *
      *    PERFORM  6100-POLICY-RECONCILE
      *        THRU 6100-POLICY-RECONCILE-X.
      *
      *    IF  WS-PCOM-CO-AUD-CTR-LOB-CD = 'N'
      *        NEXT SENTENCE
      *    ELSE
      *        PERFORM  6300-AUDIT-CHECK
      *            THRU 6300-AUDIT-CHECK-X
      *    END-IF.
      *
      *    PERFORM  6750-WRITE-COVERAGE
      *        THRU 6750-WRITE-COVERAGE-X
      *        VARYING WS-CVG FROM 1 BY 1
      *        UNTIL WS-CVG > RPOL-POL-CVG-REC-CTR-N.
      *
557020*    MOVE RPOL-PLAN-ID                 TO WPD-PLAN-ID.
      *
557020*    PERFORM  PD-1000-READ
557020*        THRU PD-1000-READ-X.
      *
557020*    MOVE 0                            TO WS-SUB.
      *
557020*    PERFORM  1000-OBTAIN-PLAN
557020*        THRU 1000-OBTAIN-PLAN-X.
      *
      *    IF  WPD-IO-OK
APEX54*        PERFORM  6180-1000-CALC-MODE-FCTS
APEX54*            THRU 6180-1000-CALC-MODE-FCTS-X
APEX54*        PERFORM  6180-2000-CALC-PFEE-FCT
APEX54*            THRU 6180-2000-CALC-PFEE-FCT-X
      *    END-IF.
      *
      *    IF  LAPUP-INVALID-PLAN
APEX54*MSG: INVALID PLAN(S) ENTERED, COVERAGE EDITS BYPASSED
      *        MOVE 'AS21000026'             TO WGLOB-MSG-REF-INFO
      *        PERFORM  9000-BUILD-MESSAGE-EXTRACT
      *            THRU 9000-BUILD-MESSAGE-EXTRACT-X
      *        PERFORM  5700-WRITE-CVG-RELATED
      *            THRU 5700-WRITE-CVG-RELATED-X
      *            VARYING WS-CVG FROM 1 BY 1
      *            UNTIL WS-CVG > RPOL-POL-CVG-REC-CTR-N
      *    ELSE
014178*        PERFORM  6340-CALC-PALC-CVGS
014178*            THRU 6340-CALC-PALC-CVGS-X
014178*            VARYING WS-CVG FROM 1 BY 1
014178*            UNTIL WS-CVG > RPOL-POL-CVG-REC-CTR-N
      *        PERFORM  6350-CVG-EDIT-ANALYSIS-LOOP
      *            THRU 6350-CVG-EDIT-ANALYSIS-LOOP-X
      *            VARYING WS-CVG FROM 1 BY 1
      *            UNTIL WS-CVG > RPOL-POL-CVG-REC-CTR-N
      *    END-IF.
      *
014178*    IF  WS-PAYO-DCA
014178*    OR  WS-PAYO-AR
014178*        PERFORM  6520-WRITE-POLICY-PAYOUTS
014178*            THRU 6520-WRITE-POLICY-PAYOUTS-X
014178*    END-IF.
      *
      *    PERFORM  6550-WRITE-POLICY-REQTS
      *        THRU 6550-WRITE-POLICY-REQTS-X.
      *
APEX53***  PERFORM  6000-WRITE-EACH-BNFY
APEX53***      THRU 6000-WRITE-EACH-BNFY-X
APEX53***      VARYING X FROM 1 BY 1
APEX53***      UNTIL X > 12.
      *
010313*    IF  WS-POL-BENE-HOLD-AREA NOT = SPACES
010313*        MOVE WS-POL-BENE-HOLD-AREA      TO LAPUP-BENE-REL-TABLE
010313*        MOVE ZERO                       TO WS-BENE-CVG
010313*        PERFORM  5630-PROCESS-BENE-INFO
010313*            THRU 5630-PROCESS-BENE-INFO-X
010313*    END-IF.
010313*
010313*    PERFORM
010313*        VARYING WS-CVG FROM 1 BY 1
010313*        UNTIL WS-CVG > RPOL-POL-CVG-REC-CTR-N
010313*
010313*        IF  WS-CVG-BENE-HOLD-AREA (WS-CVG) NOT = SPACES
010313*            MOVE WS-CVG-BENE-HOLD-AREA (WS-CVG) TO
010313*                 LAPUP-BENE-REL-TABLE
010313*            MOVE WS-CVG                 TO WS-BENE-CVG
010313*            PERFORM  5630-PROCESS-BENE-INFO
010313*                THRU 5630-PROCESS-BENE-INFO-X
010313*        END-IF
010313*
010313*    END-PERFORM.
      *
APEX53*    PERFORM  6020-BNFY-PERC-CHECK
APEX53*        THRU 6020-BNFY-PERC-CHECK-X.
      *
      *    IF  LAPUP-INVALID-PLAN
APEX54*        SET RPOL-UNMTCH-MAIL          TO TRUE
APEX54*    ELSE
APEX54*        SET L0953-FINAL-CALL          TO TRUE
      *        PERFORM  6600-POLICY-EDITS
      *            THRU 6600-POLICY-EDITS-X
      *        PERFORM  6650-CALCULATE-PREMIUMS
      *            THRU 6650-CALCULATE-PREMIUMS-X
      *    END-IF.
      *
      *    PERFORM  POL-2000-REWRITE
      *        THRU POL-2000-REWRITE-X.
      *
012148*    PERFORM  8240-2000-UPDATE-POLW
012148*        THRU 8240-2000-UPDATE-POLW-X.
      *
      *    PERFORM  6800-REWRITE-COVERAGE
      *        THRU 6800-REWRITE-COVERAGE-X
      *        VARYING WS-CVG FROM 1 BY 1
      *        UNTIL WS-CVG > RPOL-POL-CVG-REC-CTR-N.
      *
      *    MOVE 'AS2100'                     TO WGLOB-MSG-REF-ID.
      *
      *    PERFORM  6850-PRINT-MESSAGES
      *        THRU 6850-PRINT-MESSAGES-X
      *        VARYING X FROM 1 BY 1
      *        UNTIL X > WS-MESSAGE-ARRAY-MAX-SIZE.
      *
007684*    MOVE SPACE                        TO R2110-SEQ-REC-INFO.
      *    MOVE WS-COMPANY-CODE              TO R2110-COMPANY-CODE.
      *    MOVE WPOL-POL-ID                  TO R2110-POLICY-ID.
      *    MOVE WS-APPL-FORM-NO-X            TO R2110-APP-NUMBER.
      *    MOVE WS-SERV-AGT-ID               TO R2110-AGENT-CODE.
      *    MOVE RPOL-SERV-BR-ID              TO R2110-BRANCH-CODE.
      *    MOVE WS-INSURED-NAME              TO R2110-INSURED-NAME.
      *    MOVE WS-PRIMARY-INSD-CLI-ID       TO R2110-CLIENT-NUMBER.
      *    MOVE RPOL-PLAN-ID                 TO R2110-BASE-PRODUCT.
      *    MOVE WS-RECORDS-IN-APP            TO R2110-NUMBER-OF-RECS.
      *
      *    PERFORM  2110-1000-WRITE
      *        THRU 2110-1000-WRITE-X.
      *
      *    MOVE 'AS21000003'                 TO WGLOB-MSG-REF-INFO.
      *    MOVE WS-APPL-FORM-NO-X            TO WGLOB-MSG-PARM (1).
      *
      *    PERFORM  9000-BUILD-MESSAGE-EXTRACT
      *        THRU 9000-BUILD-MESSAGE-EXTRACT-X.
      *
      *5600-FINALIZE-POLICY-INFO-X.
      *    EXIT.
      *
557700*------------------
557700*5610-GET-SERV-AGT.
557700*------------------
557700*
557700* GET CLIENT SERVICING AGENT
557700*
557700*    PERFORM  0840-0000-INIT-PARM-INFO
557700*        THRU 0840-0000-INIT-PARM-INFO-X.
557700*
557700*    PERFORM  0005-1000-BUILD-PARM-INFO
557700*        THRU 0005-1000-BUILD-PARM-INFO-X.
557700*
557700*    MOVE WS-PRIMARY-INSD-CLI-ID       TO L0005-INPUT-STRING.
557700*
557700*    PERFORM  0005-2000-CONVERT-NO-ACCENTS
557700*        THRU 0005-2000-CONVERT-NO-ACCENTS-X.
557700*
557700*    MOVE L0005-OUTPUT-STRING          TO L0840-CLI-ID.
557700*    SET L0840-REL-TYP-SERVICING       TO TRUE.
557700*
557700*    PERFORM  0840-6000-GET-CLI-REL
557700*        THRU 0840-6000-GET-CLI-REL-X.
557700*
557700*    IF  L0840-AGT-ID = SPACES
557700*        MOVE WS-SERV-AGT-ID           TO RPOL-SERV-AGT-ID
557700*    ELSE
557700*        MOVE L0840-AGT-ID             TO RPOL-SERV-AGT-ID
557700*    END-IF.
557700*
557700*    IF  RPOL-SERV-AGT-ID NOT = WS-SERV-AGT-ID
557700*MSG: CLIENT SERVICING AGENT @1 IS DIFFERENT FROM WRITING AGENT @2
557700*        MOVE 'AS21000071'             TO WGLOB-MSG-REF-INFO
557700*        MOVE RPOL-SERV-AGT-ID         TO WGLOB-MSG-PARM (1)
557700*        MOVE WS-SERV-AGT-ID           TO WGLOB-MSG-PARM (2)
557700*        PERFORM  9000-BUILD-MESSAGE-EXTRACT
557700*            THRU 9000-BUILD-MESSAGE-EXTRACT-X
557700*    END-IF.
012148*
012148*    MOVE WPOL-POL-ID                  TO L8240-POL-ID.
557700*
557700*5610-GET-SERV-AGT-X.
557700*    EXIT.
010313*
010313*-----------------------
010313*5630-PROCESS-BENE-INFO.
010313*-----------------------
010313*
010313*** SET PRIMARY BENE SHARES IF SHARE SPLIT IS EQUAL SHARES
010313*    MOVE 1                          TO WS-START.
010313*    MOVE 6                          TO WS-END.
010313*    PERFORM  5632-SET-BENE-SHARES
010313*        THRU 5632-SET-BENE-SHARES-X.
010313*
010313*** SET CONTINGENT BENE SHARES IF SHARE SPLIT IS EQUAL SHARES
010313*    MOVE 7                          TO WS-START.
010313*    MOVE 12                         TO WS-END.
010313*    PERFORM  5632-SET-BENE-SHARES
010313*        THRU 5632-SET-BENE-SHARES-X.
010313*
010313*    PERFORM  6000-WRITE-EACH-BNFY
010313*        THRU 6000-WRITE-EACH-BNFY-X
010313*        VARYING X FROM 1 BY 1
010313*        UNTIL X > 12.
010313*
010313*5630-PROCESS-BENE-INFO-X.
010313*    EXIT.
010313*
010313*****************************************************************
010313*** SET BENE SHARE PERCENTAGES IF APEX SPECIFIED EQUAL SHARES
010313*** RATHER THAN SPECIFIC PERCENTAGES
010313*****************************************************************
010313*---------------------
010313*5632-SET-BENE-SHARES.
010313*---------------------
010313*
010313*    MOVE ZERO                     TO WS-EQUAL-SHARE-CNT.
010313*
010313*    PERFORM
010313*        VARYING X FROM WS-START BY 1
010313*        UNTIL X > WS-END
010313*
010313*        IF  LAPUP-BENE-EQUAL-SHARES (X)
010313*            ADD +1                TO WS-EQUAL-SHARE-CNT
010313*        END-IF
010313*
010313*    END-PERFORM.
010313*
010313*    IF  WS-EQUAL-SHARE-CNT = 0
010313*        GO TO 5632-SET-BENE-SHARES-X
010313*    END-IF.
010313*
010313*** NOTE: APEX WON'T ALLOW BOTH EQUAL SHARE AND PERCENTAGE SPLITS
010313*    DIVIDE 100 BY WS-EQUAL-SHARE-CNT GIVING WS-SHARE.
010313*    MOVE ZERO                     TO WS-TOTAL-SHARE.
010313*
010313*    PERFORM
010313*        VARYING X FROM WS-START BY 1
010313*        UNTIL X > WS-END
010313*
010313*        IF  LAPUP-BENE-EQUAL-SHARES (X)
010313*            MOVE WS-SHARE         TO LAPUP-BENE-PRCDS-PCT-N (X)
010313*            ADD WS-SHARE          TO WS-TOTAL-SHARE
010313*        END-IF
010313*
010313*    END-PERFORM.
010313*
010313*** CHECK FOR ROUNDING ERRORS AND ADD TO FIRST BENE
010313*    IF  WS-TOTAL-SHARE NOT = 100
010313*        COMPUTE LAPUP-BENE-PRCDS-PCT-N (WS-START) =
010313*            LAPUP-BENE-PRCDS-PCT-N (WS-START) + 100 -
010313*            WS-TOTAL-SHARE
010313*    END-IF.
010313*
010313*5632-SET-BENE-SHARES-X.
010313*    EXIT.
      *
      *----------------
      *5650-CHECK-CVGS.
      *----------------
      *
      *    IF  WCVGS-PLAN-ID (X) = SPACE
APEX54*        SET LAPUP-INVALID-PLAN        TO TRUE
      *    END-IF.
      *
      *    IF  LAPUP-CVGC-CLI-INFO (X) = SPACE
      *        MOVE LAPUP-CVG-CLI-INFO (1)   TO LAPUP-CVG-CLI-INFO (X)
      *    END-IF.
      *
      *    IF  LAPUP-POL-TYP-DISABILITY
      *    AND WCVGS-CVG-STBL-4-CD (X) = SPACE
014177*    AND NOT WS-APP-IS-HEALTH-PROD
APEX54*        MOVE LAPUP-CVGC-CLI-ID (X 1)  TO L2437-CLI-ID
APEX54*        PERFORM  2437-1000-OBTAIN-CLI-INFO
APEX54*            THRU 2437-1000-OBTAIN-CLI-INFO-X
APEX54*        IF  L2437-RETRN-OK
APEX54*            MOVE L2437-CLI-OCCP-CLAS-CD
      *                                      TO WCVGS-CVG-STBL-4-CD (X)
      *        END-IF
      *    END-IF.
      *
      *5650-CHECK-CVGS-X.
      *    EXIT.
      *
      *-----------------------
      *5700-WRITE-CVG-RELATED.
      *-----------------------
      *
      *    PERFORM  5750-WRITE-EACH-CVGC
      *        THRU 5750-WRITE-EACH-CVGC-X
      *        VARYING X FROM 1 BY 1
      *        UNTIL X > LAPUP-CVGC-LIVES-INSRD-CD (WS-CVG).
      *
      *    IF  WS-SAVE-APEX-SEGF-CODE (WS-CVG) NOT = SPACES
      *        PERFORM  6400-SEG-FUND-CREATE
      *            THRU 6400-SEG-FUND-CREATE-X
      *        PERFORM  6450-RESOLVE-FC-FS-RECS
      *            THRU 6450-RESOLVE-FC-FS-RECS-X
      *    END-IF.
      *
      *5700-WRITE-CVG-RELATED-X.
      *    EXIT.
      *
      *---------------------
      *5750-WRITE-EACH-CVGC.
      *---------------------
      *
      *    MOVE WPOL-POL-ID            TO WCVGC-POL-ID.
      *    MOVE WS-CVG                 TO WCVGC-CVG-NUM-N.
      *
      *    IF  X = 1
      *        MOVE 'P'                TO WCVGC-CVG-CLI-REL-TYP-CD
      *    ELSE
      *        MOVE 'O'                TO WCVGC-CVG-CLI-REL-TYP-CD
      *    END-IF.
      *
APEX54*    MOVE LAPUP-CVGC-CLI-ID (WS-CVG, X)
      *                                TO WCVGC-INSRD-CLI-ID.
      *
      *    PERFORM  CVGC-1000-CREATE
      *        THRU CVGC-1000-CREATE-X.
      *
      *    PERFORM  CVGC-1000-WRITE
      *        THRU CVGC-1000-WRITE-X.
      *
APEX54*    PERFORM  RL-1000-CREATE
APEX54*        THRU RL-1000-CREATE-X.
      *
APEX54*    MOVE 'NEWBUS'               TO WRL-REL-SYS-ID.
APEX54*    MOVE RPOL-POL-ID            TO WRL-REL-SYS-REF-ID.
APEX54*    MOVE WGLOB-PROCESS-DATE     TO RRL-REL-PREV-UPDT-DT.
APEX54*    MOVE 'A'                    TO RRL-REL-CREAT-TYP-CD.
      *
APEX54*    IF  LAPUP-CVGC-CLI-ID (WS-CVG, X) NOT = SPACES
APEX54*        MOVE LAPUP-CVGC-CLI-ID (WS-CVG, X)
APEX54*                                TO WRL-CLI-ID
APEX54*        MOVE 'I'                TO WRL-REL-TYP-CD
APEX54*        MOVE WS-CVG             TO WRL-REL-SYS-REF-CVG-NUM
APEX54*        MOVE WRL-KEY            TO RRL-KEY
APEX54*        PERFORM  RL-1000-WRITE
APEX54*            THRU RL-1000-WRITE-X
APEX54*    END-IF.
      *
      *5750-WRITE-EACH-CVGC-X.
      *    EXIT.
      *
      *--------------------------
      *5800-CONN-BASE-POL-UPDATE.
      *--------------------------
      *
      * THE BASE POLICY HAS TO BE READ TO UPDATE THE OPTIONAL POLICY
      * CODE AND CONNECT IT WITH THE OPTIONAL POLICY ABOUT TO BE WRITTEN
      *
      *    MOVE WPOL-POL-ID                TO RPOL-POL-ID.
      *    MOVE RPOL-REC-INFO              TO HPOL-REC-INFO.
      *
      *    MOVE WS-CONN-OPTL-POL-ID        TO WPOL-POL-ID.
      *
      *    PERFORM  POL-1000-READ-FOR-UPDATE
      *        THRU POL-1000-READ-FOR-UPDATE-X.
      *
APEX54*    SET RPOL-POL-OPTL-BASE-POLICY   TO TRUE.
      *
      *    PERFORM  POL-2000-REWRITE
      *        THRU POL-2000-REWRITE-X.
      *
      *    MOVE HPOL-REC-INFO              TO RPOL-REC-INFO.
      *    MOVE RPOL-POL-ID                TO WPOL-POL-ID.
      *
      *    MOVE WS-CONN-OPTL-POL-ID        TO RPOL-CONN-OPTL-POL-ID.
APEX54*    SET RPOL-POL-OPTL-OTHER-POLICY  TO TRUE.
      *
      *5800-CONN-BASE-POL-UPDATE-X.
      *    EXIT.
      *
      *-----------------------
      *5850-WRITE-POL-CLIENTS.
      *-----------------------
      *
      *    PERFORM  5900-WRITE-EACH-OWNER
      *        THRU 5900-WRITE-EACH-OWNER-X
      *        VARYING X FROM 1 BY 1
      *        UNTIL X > 5.
      *
APEX53*    PERFORM  5950-WRITE-EACH-OTHER
APEX53*        THRU 5950-WRITE-EACH-OTHER-X
      *        VARYING X FROM 1 BY 1
      *        UNTIL X > 4.
      *
      *5850-WRITE-POL-CLIENTS-X.
      *    EXIT.
      *
      *----------------------
      *5900-WRITE-EACH-OWNER.
      *----------------------
      *
      *    IF  LAPUP-POLC-OWN-CLI-ID (X) = SPACE
      *        GO TO 5900-WRITE-EACH-OWNER-X
      *    END-IF.
      *
      *    MOVE WPOL-POL-ID                TO WPOLC-POL-ID.
      *    MOVE 'O'                        TO WPOLC-POL-CLI-REL-TYP-CD.
      *    MOVE LAPUP-POLC-OWN-CLI-ID (X)  TO WPOLC-CLI-ID.
      *
      *    PERFORM  POLC-1000-CREATE
      *        THRU POLC-1000-CREATE-X.
      *
      *    MOVE LAPUP-POLC-OWN-TYP-CD (X)  TO RPOLC-POL-CLI-REL-SUB-CD.
      *    MOVE LAPUP-POLC-OWN-ADDR-TYP (X)
      *                                    TO RPOLC-CLI-ADDR-TYP-CD.
      *    MOVE LAPUP-POLC-OWN-INSRD-REL-CD (X)
      *                                    TO RPOLC-POL-CLI-INSRD-CD.
      *
      *    PERFORM  POLC-1000-WRITE
      *        THRU POLC-1000-WRITE-X.
      *
      *5900-WRITE-EACH-OWNER-X.
      *    EXIT.
      *
      *----------------------
APEX53*5950-WRITE-EACH-OTHER.
      *----------------------
      *
      *    IF  LAPUP-POLC-OTHR-CLI-ID (X) = SPACE
APEX53*        GO TO 5950-WRITE-EACH-OTHER-X
      *    END-IF.
      *
      *    MOVE WPOL-POL-ID                TO WPOLC-POL-ID.
      *    MOVE LAPUP-POLC-OTHR-INSRD-REL-CD (X)
      *                                    TO WPOLC-POL-CLI-REL-TYP-CD.
      *    MOVE LAPUP-POLC-OTHR-CLI-ID (X) TO WPOLC-CLI-ID.
      *
      *    PERFORM  POLC-1000-CREATE
      *        THRU POLC-1000-CREATE-X.
      *
      *    MOVE LAPUP-POLC-OTHR-CLI-TYP-CD (X)
      *                                    TO RPOLC-POL-CLI-REL-SUB-CD.
      *    MOVE LAPUP-POLC-OTHR-ADDR-TYP (X)
      *                                    TO RPOLC-CLI-ADDR-TYP-CD.
      *
      *    PERFORM  POLC-1000-WRITE
      *        THRU POLC-1000-WRITE-X.
      *
APEX53*5950-WRITE-EACH-OTHER-X.
      *    EXIT.
      *
      *---------------------
APEX53*6000-WRITE-EACH-BNFY.
      *---------------------
      *
APEX53*    IF  LAPUP-BENE-CLI-ID (X)       = SPACES
010313*    AND LAPUP-BENE-BNFY-NM (X)      = SPACES
APEX53***  AND LAPUP-BENE-TYP-CD (X)       = SPACES
APEX53*    AND LAPUP-BENE-REL-INSRD-CD (X) = SPACES
APEX53*    AND LAPUP-BENE-PRCDS-PCT-N (X)  = ZERO
APEX53***  AND LAPUP-BENE-DESGNT-CD (X)    = SPACES
APEX53*        GO TO 6000-WRITE-EACH-BNFY-X
APEX53*    END-IF.
      *
APEX53*    MOVE ZEROES                     TO WS-BEN-SUB.
      *
APEX53*    MOVE WPOL-POL-ID                TO WBENE-POL-ID.
APEX54*    MOVE 001                        TO WBENE-BNFY-SEQ-NUM.
      *
APEX53*    MOVE WBENE-KEY                  TO WBENE-ENDBR-KEY.
APEX54*    MOVE 999                        TO WBENE-ENDBR-BNFY-SEQ-NUM.
      *
010310*    PERFORM  BENE-1000-BROWSE
010310*        THRU BENE-1000-BROWSE-X.
      *
APEX54*
APEX54* DETERMINE NEXT AVAILABLE BNFY-SEQ-NUM
APEX54*
      *
010310*    PERFORM  BENE-2000-READ-MAX
010310*        THRU BENE-2000-READ-MAX-X.
      *
APEX53*    IF  WBENE-IO-OK
010310*        PERFORM  6080-BENE-SEQ-NUM
010310*            THRU 6080-BENE-SEQ-NUM-X
010310*            UNTIL WBENE-IO-EOF
010310*        PERFORM  BENE-3000-END-BROWSE
010310*            THRU BENE-3000-END-BROWSE-X
010310*        MOVE WBENE-MAX-BNFY-SEQ-NUM TO WS-BEN-SUB
APEX53*    END-IF.
      *
APEX53*    IF  WS-BEN-SUB  = 999
APEX53*MSG: MAXIMUM NUMBER OF BENEFICIARY RELATIONSHIPS CREATED (999)
APEX53*        MOVE 'AS21000070'           TO WGLOB-MSG-REF-INFO
APEX53*        PERFORM  9000-BUILD-MESSAGE-EXTRACT
APEX53*            THRU 9000-BUILD-MESSAGE-EXTRACT-X
APEX53*        GO TO 6000-WRITE-EACH-BNFY-X
APEX53*    END-IF.
      *
APEX53*    ADD 1                           TO WS-BEN-SUB.
      *
APEX53*    MOVE WS-BEN-SUB                 TO WBENE-BNFY-SEQ-NUM.
      *
APEX53*    PERFORM  BENE-1000-CREATE
APEX53*        THRU BENE-1000-CREATE-X.
      *
010313*    MOVE WS-BENE-CVG                TO RBENE-CVG-NUM-N.
010313*    MOVE LAPUP-BENE-BNFY-NM (X)     TO RBENE-BNFY-NM.
APEX53*    MOVE LAPUP-BENE-CLI-ID (X)      TO RBENE-CLI-ID.
APEX53*    MOVE LAPUP-BENE-TYP-CD (X)      TO RBENE-BNFY-TYP-CD.
APEX53*    MOVE LAPUP-BENE-REL-INSRD-CD (X) TO RBENE-BNFY-REL-INSRD-CD.
APEX53*    MOVE LAPUP-BENE-DESGNT-CD (X)   TO RBENE-BNFY-DESGNT-CD.
APEX53*    MOVE LAPUP-BENE-PRCDS-PCT-N (X) TO RBENE-BNFY-PRCDS-PCT.
010313*    MOVE LAPUP-BENE-MINR-IND (X)    TO RBENE-BNFY-MINR-IND.
      *
010313*    IF RBENE-CLI-ID NOT = SPACE
APEX53*        MOVE 'PR'                   TO RBENE-CLI-ADDR-TYP-CD
APEX53*        MOVE RBENE-CLI-ID           TO L5120-CLI-ID
APEX53*        PERFORM  5120-1000-CLI-NAME-UPDATE
APEX53*            THRU 5120-1000-CLI-NAME-UPDATE-X
APEX53*        MOVE L5120-CLI-NAME         TO RBENE-BNFY-NM
010313*    END-IF.
      *
APEX53*    PERFORM  BENE-1000-WRITE
APEX53*        THRU BENE-1000-WRITE-X.
      *
010313*    IF RBENE-CLI-ID NOT = SPACE
APEX53*        MOVE SPACES                 TO RRL-REC-INFO
APEX53*        PERFORM  6010-BNFY-RELATION-CHANGE
APEX53*            THRU 6010-BNFY-RELATION-CHANGE-X
010313*    END-IF.
      *
APEX53*6000-WRITE-EACH-BNFY-X.
APEX53*    EXIT.
      *
      *--------------------------
APEX53*6010-BNFY-RELATION-CHANGE.
      *--------------------------
      *
APEX53*    MOVE RBENE-CLI-ID               TO RRL-CLI-ID.
      *
APEX53*    IF  RPOL-POL-APPL-CTL-ADMIN
APEX53*        MOVE 'ADMIN'                TO RRL-REL-SYS-ID
APEX53*    ELSE
APEX53*        IF  RPOL-POL-APPL-CTL-NBS
APEX53*            MOVE 'NEWBUS'           TO RRL-REL-SYS-ID
APEX53*        END-IF
APEX53*    END-IF.
      *
APEX53*    MOVE RBENE-POL-ID               TO RRL-REL-SYS-REF-POL-ID.
      *
APEX53*    MOVE RBENE-CVG-NUM              TO RRL-REL-SYS-REF-CVG-NUM.
      *
APEX54*    SET RRL-REL-TYP-BNFY            TO TRUE.
APEX53*    MOVE WGLOB-PROCESS-DATE         TO RRL-REL-PREV-UPDT-DT.
APEX54*    SET RRL-REL-CREAT-TYP-AUTO      TO TRUE.
      *
APEX54*    PERFORM  0832-1000-UPDATE-REL
APEX54*        THRU 0832-1000-UPDATE-REL-X.
      *
APEX53*6010-BNFY-RELATION-CHANGE-X.
APEX53*    EXIT.
      *
      *---------------------
APEX53*6020-BNFY-PERC-CHECK.
      *---------------------
      *
APEX53*    SET WS-BNFY-CONT-EXISTS-NO      TO TRUE.
APEX53*    SET WS-BNFY-PRIMARY-EXISTS-NO   TO TRUE.
APEX53*    SET WS-BNFY-PERC-CONT-ZERO      TO TRUE.
APEX53*    SET WS-BNFY-PERC-PRIMARY-ZERO   TO TRUE.
      *
APEX53*    SET WS-BNFY-PERC-ERROR-NO       TO TRUE.
      *
APEX53* THIS LOGIC CHECKS FOR BENE RECORDS AT THE POLICY LEVEL,
APEX53* COVERAGE '00'.
      *
APEX53*    MOVE ZEROES                     TO WS-LOOP-CVG-NUM.
      *
APEX53*    MOVE RPOL-POL-ID                TO WBENE-POL-ID.
APEX53*    MOVE ZEROES                     TO WBENE-BNFY-SEQ-NUM.
      *
APEX53*    MOVE WBENE-KEY                  TO WBENE-ENDBR-KEY.
APEX53*    MOVE 999                        TO WBENE-ENDBR-BNFY-SEQ-NUM.
      *
557700*    PERFORM  BENE-1000-BROWSE
557700*        THRU BENE-1000-BROWSE-X.
      *
557700*    PERFORM  BENE-2000-READ-NEXT
557700*        THRU BENE-2000-READ-NEXT-X.
      *
APEX53*    PERFORM  6030-BEN-PERC-POL-CHECK
APEX53*        THRU 6030-BEN-PERC-POL-CHECK-X
APEX53*        UNTIL WBENE-IO-EOF.
      *
APEX53*    PERFORM  6050-BNFY-PERC-ERROR-CHECK
APEX53*        THRU 6050-BNFY-PERC-ERROR-CHECK-X.
      *
557700*    PERFORM  BENE-3000-END-BROWSE
557700*        THRU BENE-3000-END-BROWSE-X.
      *
APEX53* THIS LOGIC CHECKS FOR BENE RECORDS AT COVERAGE LEVEL.
      *
APEX53*    MOVE RPOL-POL-ID                TO WBENC-POL-ID.
APEX53*    MOVE ZEROES                     TO WBENC-CVG-NUM.
APEX53*    MOVE ZEROES                     TO WBENC-BNFY-SEQ-NUM.
      *
APEX53*    MOVE WBENC-KEY                  TO WBENC-ENDBR-KEY.
APEX53*    MOVE '99'                       TO WBENC-ENDBR-CVG-NUM.
APEX53*    MOVE 999                        TO WBENC-ENDBR-BNFY-SEQ-NUM.
      *
APEX53*    PERFORM  BENC-1000-BROWSE
APEX53*        THRU BENC-1000-BROWSE-X.
      *
APEX53*    IF  WBENC-IO-OK
APEX53*        PERFORM  BENC-2000-READ-NEXT
APEX53*            THRU BENC-2000-READ-NEXT-X
APEX53*        MOVE RBENE-CVG-NUM          TO WS-LOOP-CVG-NUM
APEX53*        PERFORM  6040-BEN-PERC-CVG-CHECK
APEX53*            THRU 6040-BEN-PERC-CVG-CHECK-X
APEX53*            UNTIL WBENC-IO-EOF
APEX53*        PERFORM  BENC-3000-END-BROWSE
APEX53*            THRU BENC-3000-END-BROWSE-X
APEX53*    END-IF.
      *
APEX53*    PERFORM  6050-BNFY-PERC-ERROR-CHECK
APEX53*        THRU 6050-BNFY-PERC-ERROR-CHECK-X.
      *
APEX53*    IF  WS-BNFY-PERC-ERROR
APEX53*        SET RPOL-POL-BNFY-PCT-NOT-100 TO TRUE
APEX53*    ELSE
APEX53*        SET RPOL-POL-BNFY-PCT-100     TO TRUE
APEX53*    END-IF.
      *
APEX53*6020-BNFY-PERC-CHECK-X.
APEX53*    EXIT.
      *
APEX53*------------------------
APEX53*6030-BEN-PERC-POL-CHECK.
APEX53*------------------------
      *
APEX53*    IF  RBENE-CVG-NUM = ZEROES
APEX53*        EVALUATE TRUE
      *
APEX53*            WHEN RBENE-BNFY-DESGNT-CONTINGENT
APEX53*                 ADD RBENE-BNFY-PRCDS-PCT
APEX53*                                    TO WS-BNFY-PERC-CONTINGENT
APEX53*                 SET WS-BNFY-CONT-EXISTS TO TRUE
      *
APEX53*            WHEN RBENE-BNFY-DESGNT-PRIMARY
APEX53*                 ADD RBENE-BNFY-PRCDS-PCT
APEX53*                                    TO WS-BNFY-PERC-PRIMARY
APEX53*                 SET WS-BNFY-PRIMARY-EXISTS TO TRUE
      *
APEX53*        END-EVALUATE
APEX53*    END-IF.
      *
557700*    PERFORM  BENE-2000-READ-NEXT
557700*        THRU BENE-2000-READ-NEXT-X.
      *
APEX53*6030-BEN-PERC-POL-CHECK-X.
APEX53*    EXIT.
      *
      *------------------------
APEX53*6040-BEN-PERC-CVG-CHECK.
      *------------------------
      *
APEX53*    IF  RBENE-CVG-NUM NOT = WS-LOOP-CVG-NUM
APEX53*        PERFORM  6050-BNFY-PERC-ERROR-CHECK
APEX53*            THRU 6050-BNFY-PERC-ERROR-CHECK-X
APEX53*    END-IF.
      *
APEX53*    EVALUATE TRUE
      *
APEX53*        WHEN RBENE-BNFY-DESGNT-CONTINGENT
APEX53*             ADD RBENE-BNFY-PRCDS-PCT  TO WS-BNFY-PERC-CONTINGENT
APEX53*             SET WS-BNFY-CONT-EXISTS   TO TRUE
      *
APEX53*        WHEN RBENE-BNFY-DESGNT-PRIMARY
APEX53*             ADD RBENE-BNFY-PRCDS-PCT  TO WS-BNFY-PERC-PRIMARY
APEX53*             SET WS-BNFY-PRIMARY-EXISTS
APEX53*                                       TO TRUE
      *
APEX53*    END-EVALUATE.
      *
APEX53*    PERFORM  BENC-2000-READ-NEXT
APEX53*        THRU BENC-2000-READ-NEXT-X.
      *
APEX53*6040-BEN-PERC-CVG-CHECK-X.
APEX53*    EXIT.
      *
      *---------------------------
APEX53*6050-BNFY-PERC-ERROR-CHECK.
      *---------------------------
      *
APEX53*    IF  NOT WS-BNFY-PERC-CONT-100
APEX53*    AND WS-BNFY-CONT-EXISTS
APEX53*        PERFORM  6060-BEN-PERC-CONT-ERROR-MSG
APEX53*            THRU 6060-BEN-PERC-CONT-ERROR-MSG-X
APEX53*    END-IF.
      *
APEX53*    IF  NOT WS-BNFY-PERC-PRIMARY-100
APEX53*    AND WS-BNFY-PRIMARY-EXISTS
APEX53*        PERFORM  6070-BEN-PERC-PRIM-ERROR-MSG
APEX53*            THRU 6070-BEN-PERC-PRIM-ERROR-MSG-X
APEX53*    END-IF.
      *
APEX53*    MOVE RBENE-CVG-NUM              TO WS-LOOP-CVG-NUM.
      *
APEX53*    SET WS-BNFY-CONT-EXISTS-NO      TO TRUE.
APEX53*    SET WS-BNFY-PRIMARY-EXISTS-NO   TO TRUE.
APEX53*    SET WS-BNFY-PERC-CONT-ZERO      TO TRUE.
APEX53*    SET WS-BNFY-PERC-PRIMARY-ZERO   TO TRUE.
      *
APEX53*6050-BNFY-PERC-ERROR-CHECK-X.
APEX53*    EXIT.
      *
      *-----------------------------
APEX53*6060-BEN-PERC-CONT-ERROR-MSG.
      *-----------------------------
      *
APEX53*    IF  WS-LOOP-CVG-NUM = ZEROES
APEX53*MSG: POLICY CONTINGENT BENEFICIARY % DOES NOT TOTAL 100%
APEX53*        MOVE 'AS21000036'           TO WGLOB-MSG-REF-INFO
APEX53*        PERFORM  9000-BUILD-MESSAGE-EXTRACT
APEX53*            THRU 9000-BUILD-MESSAGE-EXTRACT-X
APEX53*    ELSE
APEX53*MSG: CONTINGENT BENEFICIARY % DOES NOT TOTAL 100% FOR COVERAGE @1
APEX53*        MOVE WS-LOOP-CVG-NUM        TO WGLOB-MSG-PARM (1)
APEX53*        MOVE 'AS21000037'           TO WGLOB-MSG-REF-INFO
APEX53*        PERFORM  9000-BUILD-MESSAGE-EXTRACT
APEX53*            THRU 9000-BUILD-MESSAGE-EXTRACT-X
APEX53*    END-IF.
      *
APEX53*    IF  WGLOB-MSG-SEVRTY-FATAL
APEX53*        SET WS-BNFY-PERC-ERROR      TO TRUE
APEX53*    END-IF.
      *
APEX53*6060-BEN-PERC-CONT-ERROR-MSG-X.
APEX53*    EXIT.
      *
      *-----------------------------
APEX53*6070-BEN-PERC-PRIM-ERROR-MSG.
      *-----------------------------
      *
APEX53*    IF  WS-LOOP-CVG-NUM = ZEROES
APEX53*MSG: POLICY PRIMARY BENEFICIARY % DOES NOT TOTAL 100%
APEX53*        MOVE 'AS21000038'           TO WGLOB-MSG-REF-INFO
APEX53*        PERFORM  9000-BUILD-MESSAGE-EXTRACT
APEX53*            THRU 9000-BUILD-MESSAGE-EXTRACT-X
APEX53*    ELSE
APEX53*MSG: PRIMARY BENEFICIARY % DOES NOT TOTAL 100% FOR COVERAGE @1
APEX53*        MOVE WS-LOOP-CVG-NUM        TO WGLOB-MSG-PARM (1)
APEX53*        MOVE 'AS21000039'           TO WGLOB-MSG-REF-INFO
APEX53*        PERFORM  9000-BUILD-MESSAGE-EXTRACT
APEX53*            THRU 9000-BUILD-MESSAGE-EXTRACT-X
APEX53*    END-IF.
      *
APEX53*    IF  WGLOB-MSG-SEVRTY-FATAL
APEX53*        SET WS-BNFY-PERC-ERROR      TO TRUE
APEX53*    END-IF.
      *
APEX53*6070-BEN-PERC-PRIM-ERROR-MSG-X.
APEX53*    EXIT.
      *
010310*------------------
010310*6080-BENE-SEQ-NUM.
010310*------------------
010310*
010310*    PERFORM  BENE-2000-READ-NEXT
010310*        THRU BENE-2000-READ-NEXT-X.
010310*
010310*    IF  WBENE-IO-OK
010310*        MOVE WBENE-BNFY-SEQ-NUM     TO WS-BEN-SUB
010310*    END-IF.
010310*
010310*6080-BENE-SEQ-NUM-X.
010310*    EXIT.
010310*
      *----------------------
      *6100-POLICY-RECONCILE.
      *----------------------
      *
      *    IF  WS-IMM-ANN-POLICY
      *        PERFORM  6150-IMM-ANNUITY-RELATION
      *            THRU 6150-IMM-ANNUITY-RELATION-X
      *    END-IF.
      *
      * LOAD THE APPLICATION NUMBER ONTO THE POLICY IF ONE HAS BEEN
      * PASSED VIA THE HEADER STRUCTURE
      *
      *    IF  WS-APPL-FORM-NO-X NOT = SPACES
      *        MOVE WS-APPL-FORM-NO-X TO RPOL-POL-APP-FORM-ID
      *    END-IF.
      *
      * ANNUITY PRODUCTS DON'T PASS UP PURPOSE OF INSURANCE.
      * IT WILL BE DEFAULTED TO A 'P' IN THIS CASE.
014177* FOR HEALTH PRODUCTS, DEFAULT PURPOSE OF INSURANCE TO 'P'.
      *
      *    IF  WS-ANNUITY-POLICY
014177*    OR  WS-APP-IS-HEALTH-PROD
APEX54*        SET RPOL-POL-INS-PURP-PERSONAL  TO TRUE
      *    END-IF.
      *
      *    PERFORM  6200-SET-CURRENCY
      *        THRU 6200-SET-CURRENCY-X.
      *
      *6100-POLICY-RECONCILE-X.
      *    EXIT.
      *
      *--------------------------
      *6150-IMM-ANNUITY-RELATION.
      *--------------------------
      *
APEX54*    PERFORM
      *        VARYING X FROM 4 BY -1
      *        UNTIL X < 1
      *        OR LAPUP-POLC-OTHR-INSRD-REL-CD (X) NOT = SPACE
015543*        CONTINUE
APEX54*    END-PERFORM.
      *
      *    ADD 1                    TO X.
      *
      *    IF  X > 4
      *        GO TO 6150-IMM-ANNUITY-RELATION-X
      *    END-IF.
      *
      *    MOVE LAPUP-CVGC-CLI-ID (1 1)
      *                             TO LAPUP-POLC-OTHR-CLI-ID (X).
      *    MOVE 'D'                 TO LAPUP-POLC-OTHR-INSRD-REL-CD (X).
      *    MOVE '1'                 TO LAPUP-POLC-OTHR-CLI-TYP-CD (X).
      *    MOVE 'PR'                TO LAPUP-POLC-OTHR-ADDR-TYP (X).
      *
      *    IF  LAPUP-CVGC-LIVES-INSRD-CD (1) = 1
      *        GO TO 6150-IMM-ANNUITY-RELATION-X
      *    END-IF.
      *
      *    ADD 1                    TO X.
      *
      *    IF  X > 4
      *        GO TO 6150-IMM-ANNUITY-RELATION-X
      *    END-IF.
      *
      *    MOVE LAPUP-CVGC-CLI-ID (1 2)
      *                             TO LAPUP-POLC-OTHR-CLI-ID (X).
      *    MOVE 'D'                 TO LAPUP-POLC-OTHR-INSRD-REL-CD (X).
      *    MOVE '1'                 TO LAPUP-POLC-OTHR-CLI-TYP-CD (X).
      *    MOVE 'PR'                TO LAPUP-POLC-OTHR-ADDR-TYP (X).
      *
      *6150-IMM-ANNUITY-RELATION-X.
      *    EXIT.
      *
      *------------------
      *6200-SET-CURRENCY.
      *------------------
      *
APEX54*    PERFORM  0083-0000-INIT-PARM-INFO
APEX54*        THRU 0083-0000-INIT-PARM-INFO-X.
      *
APEX54*    MOVE RPOL-SERV-AGT-ID           TO L0083-AGENT-ID.
      *
APEX54*    PERFORM  0083-1000-RETRIEVE-AGT-INFO
APEX54*        THRU 0083-1000-RETRIEVE-AGT-INFO-X.
      *
APEX54*    IF  L0083-RETRN-NOT-FOUND
APEX54*MSG: WARNING...AGENT (@1) NOT FOUND, CURRENCY NOT SET
      *        MOVE 'AS21000024'           TO WGLOB-MSG-REF-INFO
      *        MOVE RPOL-SERV-AGT-ID       TO WGLOB-MSG-PARM (1)
      *        PERFORM  9000-BUILD-MESSAGE-EXTRACT
      *            THRU 9000-BUILD-MESSAGE-EXTRACT-X
      *        GO TO 6200-SET-CURRENCY-X
      *    END-IF.
      *
      *
      * DEFAULT AGENT'S CURRENCY ONTO THE POLICY
      *
APEX54*    MOVE L0083-AGT-CMPNST-CRCY-CD   TO RPOL-POL-CRCY-CD.
      *
      *
      * DEFAULT AGENT'S BRANCH ID ONTO THE POLICY
      *
APEX54*    MOVE L0083-AGT-BR-ID            TO RPOL-SERV-BR-ID.
      *
      *6200-SET-CURRENCY-X.
      *    EXIT.
      *
      *-------------------------
      *6250-PERFORM-QUOTE-EDITS.
      *-------------------------
      *
      *    IF  WAPIN-FLD-ID = 'PAYMENTS'
APEX54*        MOVE 9                        TO L0280-LENGTH
APEX54*        MOVE 2                        TO L0280-PRECISION
      *        MOVE WAPIN-FLD-VALUE          TO L0280-INPUT-DATA
      *        PERFORM  0280-1000-NUMERIC-EDIT
      *            THRU 0280-1000-NUMERIC-EDIT-X
      *        IF  L0280-OK
APEX54*        AND LAPUP-VALID-QUOT
014035*        AND L0280-OUTPUT-DOLLAR NOT = RQT-IA-QUOT-INCM-AMT
014035*            IF  L0280-OUTPUT-DOLLAR NOT = RQT-IA-QUOT-INCM-AMT
APEX53*MSG: INCOME AMOUNT ON APP (@1) DIFFERENT FROM QUOTE (@2)
      *            MOVE 'AS21000045'         TO WGLOB-MSG-REF-INFO
008455*            MOVE L0280-OUTPUT-DOLLAR  TO WS-PIC-9-2
008455*            MOVE WS-PIC-9-2           TO WGLOB-MSG-PARM (1)
008455**************** OUTPUT FORMAT : ZZZZZZZZZZZZ9.99
008455*            COMPUTE L0290-INPUT-NUMBER = L0280-OUTPUT-DOLLAR
008455*                                       * 100
008455*            MOVE +2                   TO L0290-PRECISION
008455*            MOVE 16                   TO L0290-MAX-OUT-LEN
008455*            PERFORM 0290-1000-NUMERIC-FORMAT
008455*               THRU 0290-1000-NUMERIC-FORMAT-X
008455*            MOVE L0290-OUTPUT-DATA    TO WGLOB-MSG-PARM (1)
008455*            MOVE RQT-IA-QUOT-INCM-AMT TO WS-PIC-9-2
008455*            MOVE WS-PIC-9-2           TO WGLOB-MSG-PARM (2)
008455**************** OUTPUT FORMAT : ZZZZZZZZZZZZ9.99
008455*            COMPUTE L0290-INPUT-NUMBER = RQT-IA-QUOT-INCM-AMT
008455*                                       * 100
008455*            MOVE +2                   TO L0290-PRECISION
008455*            MOVE 16                   TO L0290-MAX-OUT-LEN
008455*            PERFORM 0290-1000-NUMERIC-FORMAT
008455*               THRU 0290-1000-NUMERIC-FORMAT-X
008455*            MOVE L0290-OUTPUT-DATA    TO WGLOB-MSG-PARM (2)
      *            PERFORM  9000-BUILD-MESSAGE-EXTRACT
      *                THRU 9000-BUILD-MESSAGE-EXTRACT-X
014035*            END-IF
      *        END-IF
      *    END-IF.
      *
      *    IF  WAPIN-FLD-ID = 'SPA'
APEX54*        MOVE 9                        TO L0280-LENGTH
APEX54*        MOVE 2                        TO L0280-PRECISION
      *        MOVE WAPIN-FLD-VALUE          TO L0280-INPUT-DATA
      *        PERFORM  0280-1000-NUMERIC-EDIT
      *            THRU 0280-1000-NUMERIC-EDIT-X
      *        IF  L0280-OK
APEX54*        AND LAPUP-VALID-QUOT
014035*        AND L0280-OUTPUT-DOLLAR NOT = RQT-IA-GRS-SPREM-AMT
014035*            IF  L0280-OUTPUT-DOLLAR NOT = RQT-IA-GRS-SPREM-AMT
      *MSG: PREMIUM AMOUNT ON APP (@1) DIFFERENT FROM QUOTE (@2)
      *            MOVE 'AS21000046'         TO WGLOB-MSG-REF-INFO
008455*            MOVE L0280-OUTPUT-DOLLAR  TO WS-PIC-9-2
008455*            MOVE WS-PIC-9-2           TO WGLOB-MSG-PARM (1)
008455**************** OUTPUT FORMAT : ZZZZZZZZZZZZ9.99
008455*            COMPUTE L0290-INPUT-NUMBER = L0280-OUTPUT-DOLLAR
008455*                                       * 100
008455*            MOVE +2                   TO L0290-PRECISION
008455*            MOVE 16                   TO L0290-MAX-OUT-LEN
008455*            PERFORM 0290-1000-NUMERIC-FORMAT
008455*               THRU 0290-1000-NUMERIC-FORMAT-X
008455*            MOVE L0290-OUTPUT-DATA    TO WGLOB-MSG-PARM (1)
008455*            MOVE RQT-IA-GRS-SPREM-AMT TO WS-PIC-9-2
008455*            MOVE WS-PIC-9-2           TO WGLOB-MSG-PARM (2)
008455**************** OUTPUT FORMAT : ZZZZZZZZZZZZ9.99
008455*            COMPUTE L0290-INPUT-NUMBER = RQT-IA-GRS-SPREM-AMT
008455*                                       * 100
008455*            MOVE +2                   TO L0290-PRECISION
008455*            MOVE 16                   TO L0290-MAX-OUT-LEN
008455*            PERFORM 0290-1000-NUMERIC-FORMAT
008455*               THRU 0290-1000-NUMERIC-FORMAT-X
008455*            MOVE L0290-OUTPUT-DATA    TO WGLOB-MSG-PARM (2)
      *            PERFORM  9000-BUILD-MESSAGE-EXTRACT
      *                THRU 9000-BUILD-MESSAGE-EXTRACT-X
014035*            END-IF
      *        END-IF
      *    END-IF.
      *
APEX54*    IF  WAPIN-FLD-ID = 'QUOTE_NUM'
APEX54* PAD QUOTE NUMBER WITH LEADING ZEROES
APEX54*        MOVE 6                        TO L0280-LENGTH
APEX54*        MOVE ZEROES                   TO L0280-PRECISION
APEX54*        MOVE WAPIN-FLD-VALUE          TO L0280-INPUT-DATA
APEX54*        PERFORM  0280-1000-NUMERIC-EDIT
APEX54*            THRU 0280-1000-NUMERIC-EDIT-X
APEX54*        IF  L0280-OK
APEX54*            MOVE L0280-OUTPUT         TO WS-QUOTE-NUM
APEX54*            MOVE WS-QUOTE-NUM         TO WAPIN-FLD-VALUE
APEX54*        END-IF
APEX54*    END-IF.
      *
      *6250-PERFORM-QUOTE-EDITS-X.
      *    EXIT.
      *
      *-----------------
      *6300-AUDIT-CHECK.
      *-----------------
      *
APEX54*    PERFORM  0302-1000-BUILD-PARM-INFO
APEX54*        THRU 0302-1000-BUILD-PARM-INFO-X.
      *
APEX54*    MOVE RPOL-POL-ID            TO L0302-POL-ID.
APEX54*    MOVE RPOL-POL-INS-TYP-CD    TO L0302-POL-INS-TYP-CD.
      *
APEX54*    PERFORM  0302-1000-AUDIT-UPDT
APEX54*        THRU 0302-1000-AUDIT-UPDT-X.
      *
      *6300-AUDIT-CHECK-X.
      *    EXIT.
      *
014178*--------------------
014178*6340-CALC-PALC-CVGS.
014178*--------------------
014178*
014178* CALCULATE NUMBER OF CVGS TO BE INCLUDED FOR POLICY ALLOCATIONS
014178*
014178*    IF  WCVGS-CVG-INS-TYP-UL-INS-ANTY (WS-CVG)
014178*    OR  WCVGS-CVG-INS-TYP-FPA (WS-CVG)
014178*        ADD 1                   TO WS-PALC-CVG-CTR
014178*    END-IF.
014178*
014178*6340-CALC-PALC-CVGS-X.
014178*    EXIT.
014178*
      *----------------------------
      *6350-CVG-EDIT-ANALYSIS-LOOP.
      *----------------------------
      *
014178*
014178* PERFORM SEG FUND PROCESSING OR, FOR INSURANCE TYPE D OR N,
014178* SPECIFY POLICY ALLOCATION INSTRUCTIONS HERE.
014178*
      *    IF  WS-SAVE-APEX-SEGF-CODE (WS-CVG) NOT = SPACES
      *        PERFORM  6400-SEG-FUND-CREATE
      *            THRU 6400-SEG-FUND-CREATE-X
014177*        PERFORM  6450-RESOLVE-FC-FS-RECS
014177*            THRU 6450-RESOLVE-FC-FS-RECS-X
014178*    ELSE
014178*        IF  WCVGS-CVG-INS-TYP-UL-INS-ANTY (WS-CVG)
014178*        OR  WCVGS-CVG-INS-TYP-FPA (WS-CVG)
014178*            PERFORM  6460-CREATE-PALC-INFO
014178*                THRU 6460-CREATE-PALC-INFO-X
014178*        END-IF
      *    END-IF.
      *
APEX54*    PERFORM  0570-1000-BUILD-PARM-INFO
APEX54*        THRU 0570-1000-BUILD-PARM-INFO-X.
      *
      *    PERFORM  I570-1000-INIT-L0570-VALUES
      *        THRU I570-1000-INIT-L0570-VALUES-X.
      *
APEX54*    PERFORM  0570-1000-MAINTAIN-COVERAGE
APEX54*        THRU 0570-1000-MAINTAIN-COVERAGE-X.
      *
      *6350-CVG-EDIT-ANALYSIS-LOOP-X.
      *    EXIT.
      *
      *---------------------
      *6400-SEG-FUND-CREATE.
      *---------------------
      *
      * CREATE FC/FS RECORDS
      *
54-001*    MOVE WCVGS-PLAN-ID (WS-CVG) TO LSEGF-PLAN-ID.
54-001*    MOVE RPOL-POL-ID            TO LSEGF-INPUT-POL-ID.
54-001*    MOVE WS-CVG                 TO LSEGF-INPUT-CVG-NUM.
54-001*    MOVE RPOL-POL-ID            TO LSEGF-INV-CVG-POL-ID.
54-001*    MOVE WS-CVG                 TO LSEGF-INV-CVG-CVG-NUM.
54-001*    MOVE WGLOB-PROCESS-DATE     TO LSEGF-INPUT-EFF-DT.
54-001*    MOVE WGLOB-PROCESS-DATE     TO LSEGF-INV-CVG-CFN-EFF-DT.
014177*    MOVE 'CA'                   TO LSEGF-INV-CVG-CFN-OPT-CD.
      *
014177*    PERFORM  0500-1000-PROCESS-FC-FS
014177*        THRU 0500-1000-PROCESS-FC-FS-X.
      *
014177*    IF  NOT L0500-RETRN-OK
014177*        GO TO 6400-SEG-FUND-CREATE-X
014177*    END-IF.
      *
      *
      * WRITE FC/FS RECORDS
      *
014177*    MOVE 'CW'                   TO LSEGF-INV-CVG-CFN-OPT-CD.
      *
014177*    PERFORM  0500-1000-PROCESS-FC-FS
014177*        THRU 0500-1000-PROCESS-FC-FS-X.
      *
014177*    MOVE LSEGF-INV-CVG-REC-INFO TO RFC-REC-INFO.
014177*    MOVE RFC-KEY                TO WFC-KEY.
014177*
014177*    PERFORM  FC-1000-READ-FOR-UPDATE
014177*        THRU FC-1000-READ-FOR-UPDATE-X.
014177*
014177*    IF NOT WFC-IO-OK
014177*MSG: FS/FC FILES OUT OF SEQUENCE
014177*        MOVE 'AS21000021'       TO WGLOB-MSG-REF-INFO
014177*        PERFORM  0260-1000-GENERATE-MESSAGE
014177*            THRU 0260-1000-GENERATE-MESSAGE-X
014177*        GO TO 6400-SEG-FUND-CREATE-X
014177*    END-IF.
014177*
014177*    PERFORM  6410-CREATE-FS
014177*        THRU 6410-CREATE-FS-X
014177*        VARYING WS-SEGF-SUB FROM 1 BY 1
014177*        UNTIL WS-SEGF-SUB > 5
014177*        OR WS-SEGF-SUB > WS-FUND-CTR.
014177*
014177*    IF  WS-FS-ERROR
014177*        GO TO 6400-SEG-FUND-CREATE-X
014177*    END-IF.
014177*
014177*    MOVE LSEGF-FND-CTR          TO RFC-CVG-CFN-REC-CTR.
014177*
014177*    PERFORM  FC-2000-REWRITE
014177*        THRU FC-2000-REWRITE-X.
014177*
014177*    MOVE RFC-REC-INFO           TO LSEGF-INV-CVG-REC-INFO.
014178*
014178*    PERFORM  6420-CASH-DESTINATION
014178*        THRU 6420-CASH-DESTINATION-X.
      *
      *6400-SEG-FUND-CREATE-X.
      *    EXIT.
      *
014177*---------------
014177*6410-CREATE-FS.
014177*---------------
014177*
014177*    SET WS-FS-OK                TO TRUE.
014177*    MOVE RFC-POL-ID             TO WFS-POL-ID.
014177*    MOVE RFC-CVG-NUM            TO WFS-CVG-NUM.
014177*    MOVE WS-SEGFUND-CODE (WS-CVG, WS-SEGF-SUB)
014177*                                TO WFS-FND-ID.
014177*    PERFORM  FS-1000-READ
014177*        THRU FS-1000-READ-X.
014177*
014177*    IF  WFS-IO-OK
014177*MSG: FUND (@1) ALREADY DEFINED FOR THE CONTRACT
014177*        MOVE 'AS21000022'       TO WGLOB-MSG-REF-INFO
014177*        MOVE RFS-FND-ID         TO WGLOB-MSG-PARM (1)
014177*        PERFORM  0260-1000-GENERATE-MESSAGE
014177*            THRU 0260-1000-GENERATE-MESSAGE-X
014177*        SET WS-FS-ERROR         TO TRUE
014177*    ELSE
014177*        PERFORM  FS-1000-CREATE
014177*            THRU FS-1000-CREATE-X
014177*        MOVE RFC-POL-ID         TO RFS-POL-ID
014177*        MOVE RFC-CVG-NUM        TO RFS-CVG-NUM
014177*        MOVE WS-SEGF-SUB        TO LSEGF-FND-CTR
014177*        MOVE 'U'             TO LSEGF-CFN-STAT-IND (WS-SEGF-SUB)
014177*        MOVE WS-SEGFUND-CODE (WS-CVG, WS-SEGF-SUB)
014177*                                TO RFS-FND-ID
012696*        MOVE WS-SEGFUND-PERCENT1 (WS-CVG, WS-SEGF-SUB)
012696*                                TO RFS-INIT-IN-ALLOC-PCT
012696*        MOVE WS-SEGFUND-PERCENT2 (WS-CVG, WS-SEGF-SUB)
012696*                                TO RFS-CFN-IN-ALLOC-PCT
012696*        MOVE 'Y'                TO LSEGF-ALLOC-IN-IND
012696*        MOVE ZEROS              TO RFS-CFN-OUT-ALLOC-PCT
012696*        MOVE 'Y'                TO LSEGF-ALLOC-OUT-IND
014177*
014177*        PERFORM  FS-1000-WRITE
014177*            THRU FS-1000-WRITE-X
014178*
014177*    END-IF.
014177*
014177*6410-CREATE-FS-X.
014177*    EXIT.
014177*
014178*----------------------
014178*6420-CASH-DESTINATION.
014178*----------------------
014178*
014178* THIS PARAGRAPH WILL CREATE THE CAIN AND CDSI RECORDS
014178* TO COMPLETE THE CASH DESTINATION INSTRUCTIONS FOR
014178* POLICY ALLOCATIONS
014178*
014178*    PERFORM  6425-BUILD-CAIN-KEY
014178*        THRU 6425-BUILD-CAIN-KEY-X.
014178*
014178*    MOVE 'I'                    TO WCAIN-CDI-TYP-CD.
014178*
014178*    PERFORM  6430-CREATE-CAIN
014178*        THRU 6430-CREATE-CAIN-X.
014178*
014178*    MOVE 'S'                    TO WCAIN-CDI-TYP-CD.
014178*
014178*    PERFORM  6430-CREATE-CAIN
014178*        THRU 6430-CREATE-CAIN-X.
014178*
014178*    MOVE 'I'                    TO WCDSI-CDI-TYP-CD.
014178*
014178*    PERFORM  6435-CREATE-CDSI
014178*        THRU 6435-CREATE-CDSI-X
014178*        VARYING WS-SEGF-SUB FROM 1 BY 1
014178*        UNTIL WS-SEGF-SUB > 5
014178*        OR WS-SEGF-SUB > WS-FUND-CTR.
014178*
014178*    MOVE 'S'                    TO WCDSI-CDI-TYP-CD.
014178*
014178*    PERFORM  6435-CREATE-CDSI
014178*        THRU 6435-CREATE-CDSI-X
014178*        VARYING WS-SEGF-SUB FROM 1 BY 1
014178*        UNTIL WS-SEGF-SUB > 5
014178*        OR WS-SEGF-SUB > WS-FUND-CTR.
014178*
014178*    IF  WS-DEST-FUND-CTR > 0
014178*        MOVE RPOLP-POL-PAYO-TYP-CD TO WCDSI-CDI-TYP-CD
014178*        PERFORM  6435-CREATE-CDSI
014178*            THRU 6435-CREATE-CDSI-X
014178*            VARYING WS-SEGF-SUB FROM 1 BY 1
014178*            UNTIL WS-SEGF-SUB > WS-DEST-FUND-CTR
014178*    END-IF.
014178*
014178*6420-CASH-DESTINATION-X.
014178*    EXIT.
014178*
014178*--------------------
014178*6425-BUILD-CAIN-KEY.
014178*--------------------
014178*
014178*    MOVE LOW-VALUES             TO WCAIN-KEY.
014178*    MOVE HIGH-VALUES            TO WCAIN-ENDBR-KEY.
014178*    MOVE RPOL-POL-ID            TO WCAIN-POL-ID.
014178*    MOVE RPOL-POL-ID            TO WCAIN-ENDBR-POL-ID.
014178*    MOVE RPOL-POL-ISS-EFF-DT    TO L1660-INTERNAL-DATE.
014178*    PERFORM  1660-2000-CONVERT-INT-TO-INV
014178*        THRU 1660-2000-CONVERT-INT-TO-INV-X.
014178*    MOVE L1660-INVERTED-DATE    TO WCAIN-CDI-EFF-IDT-NUM-N.
014178*    MOVE +00000                 TO WCAIN-POL-PAYO-NUM.
014178*    MOVE +99999                 TO WCAIN-ENDBR-POL-PAYO-NUM.
014178*
014178*6425-BUILD-CAIN-KEY-X.
014178*    EXIT.
014178*
014178*-----------------
014178*6430-CREATE-CAIN.
014178*-----------------
014178*
014178*    PERFORM  CAIN-1000-CREATE
014178*        THRU CAIN-1000-CREATE-X.
014178*
014178*    SET RCAIN-CDI-STAT-INCOMPLETE TO TRUE.
014178*
014178*    PERFORM  CAIN-1000-WRITE
014178*        THRU CAIN-1000-WRITE-X.
014178*
014178*6430-CREATE-CAIN-X.
014178*    EXIT.
014178*
014178*-----------------
014178*6435-CREATE-CDSI.
014178*-----------------
014178*
014178* BUILD CDSI KEY
014178*
014178*    MOVE RPOL-POL-ID            TO WCDSI-POL-ID.
014178*    MOVE RPOL-POL-ISS-EFF-DT    TO L1660-INTERNAL-DATE.
014178*    PERFORM  1660-2000-CONVERT-INT-TO-INV
014178*        THRU 1660-2000-CONVERT-INT-TO-INV-X.
014178*    MOVE L1660-INVERTED-DATE    TO WCDSI-CDI-EFF-IDT-NUM.
014178*    MOVE WS-SEGF-SUB            TO WCDSI-CDI-ALLOC-NUM.
014178*
014178*    EVALUATE WCDSI-CDI-TYP-CD
014178*
014178*        WHEN 'I'
014178*        WHEN 'S'
014178*             MOVE +00000        TO WCDSI-POL-PAYO-NUM
014178*        WHEN 'C'
014178*        WHEN 'R'
014178*             MOVE +99999        TO WCDSI-POL-PAYO-NUM
014178*
014178*    END-EVALUATE.
014178*
014178* CREATE CDSI RECORD
014178*
014178*    PERFORM  CDSI-1000-CREATE
014178*        THRU CDSI-1000-CREATE-X.
014178*
014178* BUILD CDSI DETAIL
014178*
014178*    MOVE RPOL-POL-ISS-EFF-DT    TO RCDSI-CDI-EFF-DT.
014178*    MOVE WS-CVG                 TO RCDSI-CVG-NUM.
014178*    MOVE RPOL-POL-ID            TO RCDSI-DEST-POL-ID.
014178*    MOVE WS-CVG                 TO RCDSI-DEST-CVG-NUM.
014178*
014178*    EVALUATE WCDSI-CDI-TYP-CD
014178*
014178*        WHEN 'I'
014178*             MOVE WS-SEGFUND-CODE (WS-CVG, WS-SEGF-SUB)
014178*                                TO RCDSI-DEST-FND-ID
014178*             MOVE 'P'           TO RCDSI-CDI-ALLOC-CD
014178*             MOVE WS-SEGFUND-PERCENT1 (WS-CVG, WS-SEGF-SUB)
014178*                                TO RCDSI-CDI-ALLOC-PCT
014178*        WHEN 'S'
014178*             MOVE WS-SEGFUND-CODE (WS-CVG, WS-SEGF-SUB)
014178*                                TO RCDSI-DEST-FND-ID
014178*             MOVE 'P'           TO RCDSI-CDI-ALLOC-CD
014178*             MOVE WS-SEGFUND-PERCENT2 (WS-CVG, WS-SEGF-SUB)
014178*                                TO RCDSI-CDI-ALLOC-PCT
014178*        WHEN 'C'
014178*        WHEN 'R'
014178*             MOVE WS-DEST-FUND (WS-SEGF-SUB)
014178*                                TO RCDSI-DEST-FND-ID
014178*             PERFORM  6440-MOVE-DEST-INFO
014178*                 THRU 6440-MOVE-DEST-INFO-X
014178*             SET RCDSI-CDI-PAYO-INTRA-XFER TO TRUE
014178*
014178*    END-EVALUATE.
014178*
014178* WRITE CDSI RECORD
014178*
014178*    PERFORM  CDSI-1000-WRITE
014178*        THRU CDSI-1000-WRITE-X.
014178*
014178*6435-CREATE-CDSI-X.
014178*    EXIT.
014178*
014178*--------------------
014178*6440-MOVE-DEST-INFO.
014178*--------------------
014178*
014178*    IF  WS-DEST-PERCENT (WS-SEGF-SUB) > 0
014178*        MOVE 'P'                TO RCDSI-CDI-ALLOC-CD
014178*        MOVE WS-DEST-PERCENT (WS-SEGF-SUB)
014178*                                TO RCDSI-CDI-ALLOC-PCT
014178*    END-IF.
014178*
014178*    IF  WS-DEST-AMOUNT (WS-SEGF-SUB) > 0
014178*        MOVE 'A'                TO RCDSI-CDI-ALLOC-CD
014178*        MOVE WS-DEST-AMOUNT (WS-SEGF-SUB)
014178*                                TO RCDSI-CDI-ALLOC-AMT
014178*    END-IF.
014178*
014178*6440-MOVE-DEST-INFO-X.
014178*    EXIT.
014178*
      *------------------------
      *6450-RESOLVE-FC-FS-RECS.
      *------------------------
      *
      * THIS PARAGRAPH WILL READ THE FC RECORD THAT WAS JUST CREATED
      * AND CALL THE 2070 ROUTINE TO ADD THE FUND DETAIL RECORDS  (FS)
      *
54-001*    MOVE RPOL-POL-ID            TO LSEGF-INPUT-POL-ID.
54-001*    MOVE WS-CVG                 TO LSEGF-INPUT-CVG-NUM.
54-001*    MOVE RPOL-POL-ID            TO LSEGF-INV-CVG-POL-ID.
54-001*    MOVE WS-CVG                 TO LSEGF-INV-CVG-CVG-NUM.
      *
      *
      * THE LINK TO 2070 WILL READ THE FC AND ALL FS RECORDS
      *
54-001*    MOVE  'RI'                  TO LSEGF-INV-CVG-CFN-OPT-CD.
      *
54-001*    PERFORM  0500-1000-PROCESS-FC-FS
54-001*        THRU 0500-1000-PROCESS-FC-FS-X.
      *
APEX54*    IF  NOT L0500-RETRN-OK
      *MSG: LOST RECORD (@1) IN MAINTAIN - CONTACT SYSTEMS
      *        MOVE 'XS00000006'       TO WGLOB-MSG-REF-INFO
      *        PERFORM  9000-BUILD-MESSAGE-EXTRACT
      *            THRU 9000-BUILD-MESSAGE-EXTRACT-X
      *        GO TO 6450-RESOLVE-FC-FS-RECS-X
      *    END-IF.
      *
      *    PERFORM  6500-EDIT-FS-DATA
      *        THRU 6500-EDIT-FS-DATA-X
      *        VARYING WS-SEGF-SUB FROM 1 BY 1
      *        UNTIL WS-SAVE-SEGFUND-DETAILS (WS-CVG, WS-SEGF-SUB)
      *            = SPACES
012141*        OR  WS-SEGF-SUB > 5.
012141*        OR  WS-SEGF-SUB > WFNDM-MAX-WFUND-NUM.
      *
54-001*    MOVE RPOL-POL-ISS-EFF-DT    TO LSEGF-INV-CVG-CFN-EFF-DT.
54-001*    MOVE 'UF'                   TO LSEGF-INV-CVG-CFN-OPT-CD.
      *
54-001*    PERFORM  0500-1000-PROCESS-FC-FS
54-001*        THRU 0500-1000-PROCESS-FC-FS-X.
      *
      *6450-RESOLVE-FC-FS-RECS-X.
      *    EXIT.
      *
014178*----------------------
014178*6460-CREATE-PALC-INFO.
014178*----------------------
014178*
014178* FOR DA AND UL, DEFAULT POLICY ALLOCATION INSTRUCTIONS
014178*
014178*    PERFORM  6425-BUILD-CAIN-KEY
014178*        THRU 6425-BUILD-CAIN-KEY-X.
014178*
014178* IF CAIN RECORD ALREADY EXISTS, DON'T CREATE ONE
014178*
014178*    PERFORM  CAIN-1000-BROWSE
014178*        THRU CAIN-1000-BROWSE-X.
014178*
014178*    PERFORM  CAIN-2000-READ-NEXT
014178*        THRU CAIN-2000-READ-NEXT-X.
014178*
014178*    IF  WCAIN-IO-OK
014178*        PERFORM  CAIN-3000-END-BROWSE
014178*            THRU CAIN-3000-END-BROWSE-X
014178*    ELSE
014178*        PERFORM  CAIN-3000-END-BROWSE
014178*            THRU CAIN-3000-END-BROWSE-X
014178*        MOVE 'I'                TO WCAIN-CDI-TYP-CD
014178*        PERFORM  6430-CREATE-CAIN
014178*            THRU 6430-CREATE-CAIN-X
014178*        MOVE 'S'                TO WCAIN-CDI-TYP-CD
014178*        PERFORM  6430-CREATE-CAIN
014178*            THRU 6430-CREATE-CAIN-X
014178*    END-IF.
014178*
014178*    MOVE 'I'                    TO WCDSI-CDI-TYP-CD.
014178*
014178*    PERFORM  6465-CREATE-CDSI
014178*        THRU 6465-CREATE-CDSI-X.
014178*
014178*    MOVE 'S'                    TO WCDSI-CDI-TYP-CD.
014178*
014178*    PERFORM  6465-CREATE-CDSI
014178*        THRU 6465-CREATE-CDSI-X.
014178*
014178*6460-CREATE-PALC-INFO-X.
014178*    EXIT.
014178*
014178*-----------------
014178*6465-CREATE-CDSI.
014178*-----------------
014178*
014178* BUILD CDSI KEY
014178*
014178*    MOVE RPOL-POL-ID            TO WCDSI-POL-ID.
014178*    MOVE +000                   TO WCDSI-POL-PAYO-NUM.
014178*    MOVE RPOL-POL-ISS-EFF-DT    TO L1660-INTERNAL-DATE.
014178*    PERFORM  1660-2000-CONVERT-INT-TO-INV
014178*        THRU 1660-2000-CONVERT-INT-TO-INV-X.
014178*    MOVE L1660-INVERTED-DATE    TO WCDSI-CDI-EFF-IDT-NUM.
014178*
014178* DETERMINE NEXT AVAILABLE ALLOCATION NUMBER
014178*
014178*    MOVE +001                   TO WCDSI-CDI-ALLOC-NUM.
014178*    MOVE WCDSI-KEY              TO WCDSI-ENDBR-KEY.
014178*    MOVE +999                   TO WCDSI-ENDBR-CDI-ALLOC-NUM.
014178*
014178*    PERFORM  CDSI-2000-READ-MAX
014178*        THRU CDSI-2000-READ-MAX-X.
014178*
014178*    IF  WCDSI-IO-OK
014178*        COMPUTE WCDSI-CDI-ALLOC-NUM
014178*                      = WCDSI-MAX-CDI-ALLOC-NUM + 1
014178*    END-IF.
014178*
014178* CREATE CDSI RECORD
014178*
014178*    PERFORM  CDSI-1000-CREATE
014178*        THRU CDSI-1000-CREATE-X.
014178*
014178* BUILD CDSI DETAIL
014178*
014178*    MOVE WS-CVG                 TO RCDSI-CVG-NUM.
014178*    MOVE RPOL-POL-ISS-EFF-DT    TO RCDSI-CDI-EFF-DT.
014178*    MOVE 'P'                    TO RCDSI-CDI-ALLOC-CD.
014178*    MOVE RPOL-POL-ID            TO RCDSI-DEST-POL-ID.
014178*    MOVE WS-CVG                 TO RCDSI-DEST-CVG-NUM.
014178*
014178*    PERFORM  6470-CALC-ALLOCATION
014178*        THRU 6470-CALC-ALLOCATION-X.
014178*
014178* WRITE CDSI RECORD
014178*
014178*    PERFORM  CDSI-1000-WRITE
014178*        THRU CDSI-1000-WRITE-X.
014178*
014178*6465-CREATE-CDSI-X.
014178*    EXIT.
014178*
014178*---------------------
014178*6470-CALC-ALLOCATION.
014178*---------------------
014178*
014178*    IF  WS-DIA-CVG = 0
014178*    AND WS-GIA-CVG = 0
014178*    AND WS-PALC-CVG-CTR > 0
014178*        COMPUTE WS-ALLOC-PCT = +100.00 / WS-PALC-CVG-CTR
014178*        MOVE WS-ALLOC-PCT       TO RCDSI-CDI-ALLOC-PCT
014178*    END-IF.
014178*
014178*    IF  WS-DIA-CVG = WS-CVG
014178*        IF  WS-CVG-ALLOC-PCT1 (WS-DIA-CVG) > 0
014178*        AND WCDSI-CDI-TYP-CD = 'I'
014178*            MOVE WS-CVG-ALLOC-PCT1 (WS-DIA-CVG)
014178*                                TO RCDSI-CDI-ALLOC-PCT
014178*        END-IF
014178*        IF  WS-CVG-ALLOC-PCT2 (WS-DIA-CVG) > 0
014178*        AND WCDSI-CDI-TYP-CD = 'S'
014178*            MOVE WS-CVG-ALLOC-PCT2 (WS-DIA-CVG)
014178*                                TO RCDSI-CDI-ALLOC-PCT
014178*        END-IF
014178*    END-IF.
014178*
014178*    IF  WS-GIA-CVG = WS-CVG
014178*        IF  WS-CVG-ALLOC-PCT1 (WS-GIA-CVG) > 0
014178*        AND WCDSI-CDI-TYP-CD = 'I'
014178*            MOVE WS-CVG-ALLOC-PCT1 (WS-GIA-CVG)
014178*                                TO RCDSI-CDI-ALLOC-PCT
014178*        END-IF
014178*        IF  WS-CVG-ALLOC-PCT2 (WS-GIA-CVG) > 0
014178*        AND WCDSI-CDI-TYP-CD = 'S'
014178*            MOVE WS-CVG-ALLOC-PCT2 (WS-GIA-CVG)
014178*                                TO RCDSI-CDI-ALLOC-PCT
014178*        END-IF
014178*    END-IF.
014178*
014178*6470-CALC-ALLOCATION-X.
014178*    EXIT.
014178*
      *------------------
      *6500-EDIT-FS-DATA.
      *------------------
      *
54-001*    MOVE LSEGF-INV-CVG-REC-INFO     TO RFC-REC-INFO.
54-001*    MOVE LSEGF-CFN-REC-INFO (WS-SEGF-SUB)
54-001*                                    TO RFS-REC-INFO.
      *    MOVE RFC-POL-ID                 TO RFS-POL-ID.
      *    MOVE RFC-CVG-NUM                TO RFS-CVG-NUM.
54-001*    MOVE WS-SEGF-SUB                TO LSEGF-FND-CTR.
54-001*    MOVE 'U'                TO LSEGF-CFN-STAT-IND (WS-SEGF-SUB).
      *    MOVE WS-SEGFUND-CODE (WS-CVG WS-SEGF-SUB)
      *                                    TO RFS-FND-ID.
014177*    MOVE WS-SEGFUND-PERCENT (WS-CVG, WS-SEGF-SUB)
012696*    MOVE WS-SEGFUND-PERCENT1 (WS-CVG, WS-SEGF-SUB)
012696*                                    TO RFS-INIT-IN-ALLOC-PCT.
014177*    MOVE WS-SEGFUND-PERCENT (WS-CVG, WS-SEGF-SUB)
012696*    MOVE WS-SEGFUND-PERCENT2 (WS-CVG, WS-SEGF-SUB)
012696*                                    TO RFS-CFN-IN-ALLOC-PCT.
      *
54-001*    MOVE 'Y'                        TO LSEGF-ALLOC-IN-IND.
      *
012696*    MOVE ZEROS                      TO RFS-CFN-OUT-ALLOC-PCT.
54-001*    MOVE 'Y'                        TO LSEGF-ALLOC-OUT-IND.
      *
54-001*    MOVE RFS-REC-INFO       TO LSEGF-CFN-REC-INFO (WS-SEGF-SUB).
      *
      *6500-EDIT-FS-DATA-X.
      *    EXIT.
      *
014178*--------------------------
014178*6520-WRITE-POLICY-PAYOUTS.
014178*--------------------------
014178*
014178*    MOVE RPOL-POL-ID            TO RPOLP-POL-ID.
014178*    MOVE RPOL-POL-ISS-EFF-DT    TO RPOLP-POL-PAYO-EFF-DT.
014178*    MOVE '01'                   TO RPOLP-CVG-NUM.
014178*
014178*    MOVE RPOLP-KEY              TO WPOLP-KEY.
014178*
014178*    PERFORM  POLP-1000-WRITE
014178*        THRU POLP-1000-WRITE-X.
014178*
014178*6520-WRITE-POLICY-PAYOUTS-X.
014178*    EXIT.
014178*
      *------------------------
      *6550-WRITE-POLICY-REQTS.
      *------------------------
      *
557020*
557020* IF PLAN REQUIRES CASH SET UP REQT FOR CASH WITH APPLICATION
557020*
557020*    IF  RPH-PLAN-MIN-MPREM-QTY > ZERO
557020*        PERFORM  0080-1000-BUILD-PARM-INFO
557020*            THRU 0080-1000-BUILD-PARM-INFO-X
557020*
557020*        SET  L0080-POLICY-LEVEL             TO  TRUE
557020*        SET  L0080-REQIR-STAT-SUGG-ISS-RQIR TO  TRUE
557020*        MOVE RPOL-POL-ID                    TO  L0080-POL-ID
557020*        MOVE 'CWA'                          TO  L0080-REQIR-CODE
557020*
557020*        PERFORM  0080-4000-WRITE
557020*            THRU 0080-4000-WRITE-X
557020*
557020*        IF  NOT L0080-RETRN-OK
557020*MSG: CASH WITH APPLICATION REQUIREMENT NOT GENERATED
557020*            MOVE 'XS00000183'          TO WGLOB-MSG-REF-INFO
557020*            PERFORM  0260-1000-GENERATE-MESSAGE
557020*                THRU 0260-1000-GENERATE-MESSAGE-X
557020*        END-IF
557020*    END-IF.
      *
557020*    MOVE SPACES                 TO L0080-PARM-INFO.
557020*    MOVE WWKDT-ZERO-DT          TO L0080-REQIR-EFF-DT.
557020*    MOVE WWKDT-ZERO-DT          TO L0080-REQIR-TST-DT.
557020*    MOVE '4'                    TO L0080-RQST-CD.
557020*    MOVE 'P'                    TO L0080-POL-CLI-CD.
557020*    MOVE WPOL-POL-ID            TO L0080-POL-ID.
557020*    MOVE ZERO                   TO L0080-COVERAGE-NUMBER.
557020*    MOVE 'CWA'                  TO L0080-REQIR-CODE.
557020*    MOVE 'SIR'                  TO L0080-REQIR-STAT-CD.
      *
557020*    PERFORM  0080-4000-WRITE
557020*        THRU 0080-4000-WRITE-X.
      *
010303*    IF  RPOL-POL-REPL-EXTERNAL
557020*        MOVE SPACES             TO L0080-PARM-INFO
557020*        MOVE WWKDT-ZERO-DT      TO L0080-REQIR-EFF-DT
557020*        MOVE WWKDT-ZERO-DT      TO L0080-REQIR-TST-DT
010303*        PERFORM  0080-1000-BUILD-PARM-INFO
010303*            THRU 0080-1000-BUILD-PARM-INFO-X
557020*        MOVE '4'                TO L0080-RQST-CD
557020*        MOVE 'P'                TO L0080-POL-CLI-CD
010303*        SET  L0080-POLICY-LEVEL TO TRUE
010303*        MOVE WPOL-POL-ID        TO L0080-POL-ID
010303*        MOVE ZERO               TO L0080-COVERAGE-NUMBER
010303*        MOVE 'REPLD'            TO L0080-REQIR-CODE
557020*        MOVE 'SIR'              TO L0080-REQIR-STAT-CD
010303*        SET  L0080-REQIR-STAT-SUGG-ISS-RQIR TO TRUE
010303*        PERFORM  0080-4000-WRITE
010303*            THRU 0080-4000-WRITE-X
010303*    END-IF.
      *
      *6550-WRITE-POLICY-REQTS-X.
      *    EXIT.
      *
      *------------------
      *6600-POLICY-EDITS.
      *------------------
      *
APEX54*    IF  L0953-NOT-FINAL-CALL
APEX54*        PERFORM  I953-1000-INIT-L0953-VALUES
APEX54*            THRU I953-1000-INIT-L0953-VALUES-X
557788*        MOVE SPACES             TO L0953-SPND-POL-CSTAT-CD
557788*        MOVE SPACES             TO L0953-SPND-POL-EFF-DT
APEX54*    END-IF.
      *
012148*    MOVE L8240-AGT-ID (1)       TO L0953-POLW-AGT-ID (1).
012148*    MOVE L8240-AGT-ID (2)       TO L0953-POLW-AGT-ID (2).
012148*    MOVE L8240-AGT-ID (3)       TO L0953-POLW-AGT-ID (3).
      *
APEX54*    PERFORM  0953-1000-MAINTAIN-POLICY
APEX54*        THRU 0953-1000-MAINTAIN-POLICY-X.
      *
012148*    MOVE L0953-POLW-AGT-ID (1)  TO L8240-AGT-ID (1).
012148*    MOVE L0953-POLW-AGT-ID (2)  TO L8240-AGT-ID (2).
012148*    MOVE L0953-POLW-AGT-ID (3)  TO L8240-AGT-ID (3).
      *
      *6600-POLICY-EDITS-X.
      *    EXIT.
      *
      *------------------------
      *6650-CALCULATE-PREMIUMS.
      *------------------------
      *
557660*    MOVE 1                      TO L0183-RECALC-PREM-IND.
557660*    MOVE 1                      TO L0183-OFF-ANNV-SAVE-SIDE.
557660*    MOVE 1                      TO L0183-PMT-REVRS-CD.
      *
557660*    SET  L0183-RECALC-PREM-FORCED TO TRUE.
557660*    MOVE +1                       TO L0183-OFF-ANNV-SAVE-SIDE.
557660*    SET  L0183-PMT-REVRS-PAYMENT  TO TRUE.
      *
54-001*    PERFORM  0183-1000-CALC-NXT-PREM
54-001*        THRU 0183-1000-CALC-NXT-PREM-X.
      *
      *6650-CALCULATE-PREMIUMS-X.
      *    EXIT.
      *
      *--------------------
      *6750-WRITE-COVERAGE.
      *--------------------
      *
      *    MOVE WPOL-POL-ID              TO WCVG-POL-ID.
      *    MOVE WS-CVG                   TO WCVG-CVG-NUM-N.
      *    MOVE WCVGS-CVG-INFO (WS-CVG)  TO RCVG-CVG-INFO.
      *
      *    PERFORM  CVG-1000-WRITE
      *        THRU CVG-1000-WRITE-X.
      *
      *6750-WRITE-COVERAGE-X.
      *    EXIT.
      *
      *----------------------
      *6800-REWRITE-COVERAGE.
      *----------------------
      *
      *    MOVE WPOL-POL-ID              TO WCVG-POL-ID.
      *    MOVE WS-CVG                   TO WCVG-CVG-NUM-N.
      *
      *    PERFORM  CVG-1000-READ-FOR-UPDATE
      *        THRU CVG-1000-READ-FOR-UPDATE-X.
      *
      *    MOVE WCVGS-CVG-INFO (WS-CVG)  TO RCVG-CVG-INFO.
      *
      *    PERFORM  CVG-2000-REWRITE
      *        THRU CVG-2000-REWRITE-X.
      *
      *6800-REWRITE-COVERAGE-X.
      *    EXIT.
      *
      *--------------------
      *6850-PRINT-MESSAGES.
      *--------------------
      *
      *    IF  WS-MESSAGE-SET (X)
      *        COMPUTE WS-MESSAGE-NUMBER = 1000 + X
      *        MOVE WS-MESSAGE-NUMBER-X  TO WGLOB-MSG-REF-NUM
      *        PERFORM  9000-BUILD-MESSAGE-EXTRACT
      *            THRU 9000-BUILD-MESSAGE-EXTRACT-X
      *    END-IF.
      *
      *6850-PRINT-MESSAGES-X.
      *    EXIT.
      *
      *-----------------------
      *7000-PRINT-PARM-TOTALS.
      *-----------------------
      *
      *    MOVE SPACES                   TO L0040-INPUT-LINE.
      *
      *    PERFORM  0040-3000-WRITE-OTHER
      *        THRU 0040-3000-WRITE-OTHER-X.
      *
      *MSG: TOTAL NUMBER OF PARM CARDS READ (@1)
      *    MOVE 'XS00000142'             TO WGLOB-MSG-REF-INFO.
      *    MOVE WS-PARM-CARD-COUNTER     TO WS-PIC-COUNTER.
      *    MOVE WS-PIC-COUNTER           TO WGLOB-MSG-PARM (1).
      *
      *    PERFORM  0260-1000-GENERATE-MESSAGE
      *        THRU 0260-1000-GENERATE-MESSAGE-X.
      *
      *    PERFORM  0040-4000-WRITE-ERROR-TOTAL
      *        THRU 0040-4000-WRITE-ERROR-TOTAL-X.
      *
      *    IF  L0040-ERROR-CNT > ZERO
      *MSG: INVALID PARM CARDS, NO PROCESSING PERFORMED
      *        MOVE 'XS00000120'    TO WGLOB-MSG-REF-INFO
      *        PERFORM  0260-1000-GENERATE-MESSAGE
      *            THRU 0260-1000-GENERATE-MESSAGE-X
      *    END-IF.
      *
      *7000-PRINT-PARM-TOTALS-X.
      *    EXIT.
      *
      *--------------------------
      *7200-PRINT-COMPANY-TOTALS.
      *--------------------------
      *
      *    MOVE SPACES                 TO L0040-INPUT-LINE.
      *
      *    PERFORM  0040-3000-WRITE-OTHER
      *        THRU 0040-3000-WRITE-OTHER-X.
      *
      *MSG: TOTAL NUMBER OF UPLOADED RECORDS READ
      *    MOVE 'AS21000002'           TO WGLOB-MSG-REF-INFO.
      *    MOVE WS-APEX-IN-COMPANY     TO WS-PIC-COUNTER.
      *    MOVE WS-PIC-COUNTER         TO WGLOB-MSG-PARM (1).
      *
      *    PERFORM  0260-1000-GENERATE-MESSAGE
      *        THRU 0260-1000-GENERATE-MESSAGE-X.
      *
      *    ADD WS-APEX-IN-COMPANY      TO WS-APEX-IN-TOTAL.
      *
      *    MOVE ZEROS                  TO WS-APEX-IN-COMPANY.
      *
      *    PERFORM  0040-4000-WRITE-ERROR-TOTAL
      *        THRU 0040-4000-WRITE-ERROR-TOTAL-X.
      *
      *7200-PRINT-COMPANY-TOTALS-X.
      *    EXIT.
      *
      *------------------------
      *7500-PRINT-GRAND-TOTALS.
      *------------------------
      *
      * RE-INITIALIZE TITLES/HEADINGS USING DEFAULT COMPANY CODE
      * SO THE REPORT IS NOT TIED TO ONE SPECIFIC COMPANY
      *
APEX53*    MOVE WPGWS-CRNT-PGM-ID      TO L0960-PROGRAM-ID.
      *
      *    PERFORM  0960-4000-INIT-DEFAULT-COMP
      *        THRU 0960-4000-INIT-DEFAULT-COMP-X.
      *
008455*    PERFORM 0290-1000-BUILD-PARM-INFO
008455*       THRU 0290-1000-BUILD-PARM-INFO-X.
      *
      *    PERFORM  8000-INIT-TITLES
      *        THRU 8000-INIT-TITLES-X.
      *
      *MSG: TOTAL NUMBER OF UPLOADED RECORDS READ
      *    MOVE 'AS21000002'           TO WGLOB-MSG-REF-INFO.
      *    MOVE WS-APEX-IN-TOTAL       TO WS-PIC-COUNTER.
      *    MOVE WS-PIC-COUNTER         TO WGLOB-MSG-PARM (1).
      *
      *    PERFORM  0260-1000-GENERATE-MESSAGE
      *        THRU 0260-1000-GENERATE-MESSAGE-X.
      *
      *    MOVE SPACES                 TO L0040-INPUT-LINE.
      *
      *    PERFORM  0040-3000-WRITE-OTHER
      *        THRU 0040-3000-WRITE-OTHER-X.
      *
      *7500-PRINT-GRAND-TOTALS-X.
      *    EXIT.
      *
      *-----------------
      *8000-INIT-TITLES.
      *-----------------
      *
      *    MOVE L0960-COMPANY-NAME     TO L0040-COMPANY-NAME.
      *    MOVE ZERO                   TO L0040-ERROR-CNT.
      *
      *
      * SET UP THE TITLE/HEADING LINES
      *
      * GET THE SYSTEM ID
      *
      *    MOVE 'XS00000145'           TO WGLOB-MSG-REF-INFO.
      *
      *    PERFORM  0260-2000-GET-MESSAGE
      *        THRU 0260-2000-GET-MESSAGE-X.
      *
      *    MOVE WGLOB-MSG-TXT          TO L0040-SYSTEM-ID.
      *
      * GET THE PROGRAM DESCRIPTION
      *
      *    MOVE 'AS21000004'           TO WGLOB-MSG-REF-INFO.
      *
      *    PERFORM  0260-2000-GET-MESSAGE
      *        THRU 0260-2000-GET-MESSAGE-X.
      *
      *    MOVE WGLOB-MSG-TXT          TO L0040-PROGRAM-DESC.
      *
      * GET THE DETAIL HEADINGS FOR PRINTING CONTROL CARDS
      *
      *    MOVE 'XS00000150'           TO WGLOB-MSG-REF-INFO.
      *
      *    PERFORM  0260-2000-GET-MESSAGE
      *        THRU 0260-2000-GET-MESSAGE-X.
      *
      *    MOVE WGLOB-MSG-TXT          TO L0040-HDG-LINE-3.
      *
      *    PERFORM  0040-1000-INIT-TITLE
      *        THRU 0040-1000-INIT-TITLE-X.
      *
      *8000-INIT-TITLES-X.
      *    EXIT.
      *
557700*----------------------
557700*8050-CHECK-UTTB-TABLE.
557700*----------------------
      *
557700*    MOVE LOW-VALUES             TO WUTTB-KEY.
557700*    MOVE HIGH-VALUES            TO WUTTB-ENDBR-KEY.
      *
557700*    PERFORM  UTTB-1000-TBL-BROWSE
557700*        THRU UTTB-1000-TBL-BROWSE-X.
      *
557700*    IF  NOT WUTTB-IO-EOF
557700*        PERFORM  UTTB-2000-TBL-READ-NEXT
557700*            THRU UTTB-2000-TBL-READ-NEXT-X
557700*    END-IF.
      *
557700*    IF  NOT WUTTB-IO-OK
557700*MSG: UTTB/UFLD TABLE IS EMPTY - PROGRAM ABENDING
557700*        MOVE 'AS21000099'       TO WGLOB-MSG-REF-INFO
      *
557700*        PERFORM  0260-1000-GENERATE-MESSAGE
557700*            THRU 0260-1000-GENERATE-MESSAGE-X
557700*
014590*        MOVE WUTTB-TABLE-NAME        TO WGLOB-TABLE-NAME
014590*        MOVE WUTTB-RQST-CD           TO WGLOB-IO-COMMAND
014590*        MOVE WUTTB-OPTM-SQL-STAT-CD  TO WGLOB-OPTM-SQL-STAT-CD
014590*        MOVE WUTTB-OPTM-SQL-REQIR    TO WGLOB-OPTM-SQL-REQIR
014590*        MOVE WUTTB-OPTM-SQL-EXEC     TO WGLOB-OPTM-SQL-EXEC
557700*        PERFORM  0030-1000-SQL-ERROR
557700*            THRU 0030-1000-SQL-ERROR-X
557700*    ELSE
557700*        PERFORM  UTTB-3000-TBL-END-BROWSE
557700*            THRU UTTB-3000-TBL-END-BROWSE-X
557700*    END-IF.
      *
557700*8050-CHECK-UTTB-TABLE-X.
557700*    EXIT.
      *
557700*----------------------
557700*8070-CHECK-UFLD-TABLE.
557700*----------------------
      *
557700*    MOVE LOW-VALUES             TO WUFLD-KEY.
557700*    MOVE HIGH-VALUES            TO WUFLD-ENDBR-KEY.
      *
557700*    PERFORM  UFLD-1000-TBL-BROWSE
557700*        THRU UFLD-1000-TBL-BROWSE-X.
      *
557700*    IF  NOT WUFLD-IO-EOF
557700*        PERFORM  UFLD-2000-TBL-READ-NEXT
557700*            THRU UFLD-2000-TBL-READ-NEXT-X
557700*    END-IF.
      *
557700*    IF  NOT WUFLD-IO-OK
557700*MSG: UTTB/UFLD TABLE IS EMPTY - PROGRAM ABENDING
557700*        MOVE 'AS21000099'       TO WGLOB-MSG-REF-INFO
      *
557700*        PERFORM  0260-1000-GENERATE-MESSAGE
557700*            THRU 0260-1000-GENERATE-MESSAGE-X
557700*
014590*        MOVE WUFLD-TABLE-NAME        TO WGLOB-TABLE-NAME
014590*        MOVE WUFLD-RQST-CD           TO WGLOB-IO-COMMAND
014590*        MOVE WUFLD-OPTM-SQL-STAT-CD  TO WGLOB-OPTM-SQL-STAT-CD
014590*        MOVE WUFLD-OPTM-SQL-REQIR    TO WGLOB-OPTM-SQL-REQIR
014590*        MOVE WUFLD-OPTM-SQL-EXEC     TO WGLOB-OPTM-SQL-EXEC
557700*        PERFORM  0030-1000-SQL-ERROR
557700*            THRU 0030-1000-SQL-ERROR-X
557700*    ELSE
557700*        PERFORM  UFLD-3000-TBL-END-BROWSE
557700*            THRU UFLD-3000-TBL-END-BROWSE-X
557700*    END-IF.
      *
557700*8070-CHECK-UFLD-TABLE-X.
557700*    EXIT.
      *
      *----------------------------
      *8100-INIT-FRENCH-UTTB-TABLE.
      *----------------------------
      *
      *    MOVE LOW-VALUES             TO WUTTB-KEY.
      *    MOVE HIGH-VALUES            TO WUTTB-ENDBR-KEY.
      *    MOVE 'FRNCH'                TO WUTTB-UPLD-TTBL-TYP-ID.
      *    MOVE 'FRNCH'                TO WUTTB-ENDBR-UPLD-TTBL-TYP-ID.
      *
      *    PERFORM  UTTB-1000-BROWSE
      *        THRU UTTB-1000-BROWSE-X.
      *
      *    IF  WUTTB-IO-EOF
      *        MOVE 1                  TO LAPUP-FRENCH-UTTB-CNT
      *        MOVE HIGH-VALUES     TO LAPUP-FRENCH-UTTB-TABLE-ENTRY (1)
      *        GO TO 8100-INIT-FRENCH-UTTB-TABLE-X
      *    END-IF.
      *
      *    PERFORM  UTTB-2000-READ-NEXT
      *        THRU UTTB-2000-READ-NEXT-X.
      *
      *    IF  WUTTB-IO-EOF
      *        MOVE 1                  TO LAPUP-FRENCH-UTTB-CNT
      *        MOVE HIGH-VALUES     TO LAPUP-FRENCH-UTTB-TABLE-ENTRY (1)
      *        PERFORM  UTTB-3000-END-BROWSE
      *            THRU UTTB-3000-END-BROWSE-X
      *        GO TO 8100-INIT-FRENCH-UTTB-TABLE-X
      *    END-IF.
      *
      *    MOVE ZERO                   TO LAPUP-FRENCH-UTTB-CNT.
      *
      *    PERFORM  8110-LOAD-FRENCH-UTTB
      *        THRU 8110-LOAD-FRENCH-UTTB-X
      *        UNTIL WUTTB-IO-EOF
      *        OR LAPUP-FRENCH-UTTB-CNT > 99.
      *
      *    IF  NOT WUTTB-IO-EOF
      *MSG: WARNING: INTERNAL FRENCH UTTB TABLE NEEDS EXPANSION,
      *              CONTACT SYSTEMS
      *        MOVE 'AS21000050'       TO WGLOB-MSG-REF-INFO
      *        PERFORM  0260-1000-GENERATE-MESSAGE
      *            THRU 0260-1000-GENERATE-MESSAGE-X
      *    END-IF.
      *
      *    PERFORM  UTTB-3000-END-BROWSE
      *        THRU UTTB-3000-END-BROWSE-X.
      *
      *8100-INIT-FRENCH-UTTB-TABLE-X.
      *    EXIT.
      *
      *----------------------
      *8110-LOAD-FRENCH-UTTB.
      *----------------------
      *
      *    ADD +1                        TO LAPUP-FRENCH-UTTB-CNT.
      *
      *    MOVE LAPUP-FRENCH-UTTB-CNT    TO WS-UTTB-SUB.
      *    MOVE RUTTB-UPLD-TTBL-VALU-ID
      *                      TO LAPUP-UPLD-TTBL-VALU-ID (WS-UTTB-SUB).
      *    MOVE RUTTB-UPLD-TTBL-VALU-TXT
      *                      TO LAPUP-UPLD-TTBL-VALU-TXT (WS-UTTB-SUB).
      *
      *    PERFORM  UTTB-2000-READ-NEXT
      *        THRU UTTB-2000-READ-NEXT-X.
      *
      *8110-LOAD-FRENCH-UTTB-X.
      *    EXIT.
      *
      *-----------------------
      *8200-READ-CONTROL-CARD.
      *-----------------------
      *
      *    PERFORM  BCF-1000-READ
      *        THRU BCF-1000-READ-X.
      *
007684*    IF  WBCF-SEQ-IO-OK
      *        ADD 1                   TO WS-PARM-CARD-COUNTER
      *    END-IF.
      *
      *8200-READ-CONTROL-CARD-X.
      *    EXIT.
      *
557698*--------------------------
557698*8300-TRANSLATE-UPPER-CASE.
557698*--------------------------
557698*
557698*    PERFORM  0005-1000-BUILD-PARM-INFO
557698*        THRU 0005-1000-BUILD-PARM-INFO-X.
557698*
557698*    MOVE WAPIN-FLD-VALUE            TO L0005-INPUT-STRING.
557698*
557698*    PERFORM  0005-1000-CONVERT-STRING
557698*        THRU 0005-1000-CONVERT-STRING-X.
557698*
557698*    IF  L0005-RETRN-OK
557698*        MOVE L0005-OUTPUT-STRING    TO WAPIN-FLD-VALUE
557698*    END-IF.
557698*
557698*8300-TRANSLATE-UPPER-CASE-X.
557698*    EXIT.
      *
557698*-----------------------------
557698*8400-TRANSLATE-UPPER-CASE-NA.
557698*-----------------------------
557698*
557698*    PERFORM  0005-1000-BUILD-PARM-INFO
557698*        THRU 0005-1000-BUILD-PARM-INFO-X.
557698*
557698*    MOVE WAPIN-FLD-VALUE            TO L0005-INPUT-STRING.
557698*
557698*    PERFORM  0005-2000-CONVERT-NO-ACCENTS
557698*        THRU 0005-2000-CONVERT-NO-ACCENTS-X.
557698*
557698*    IF  L0005-RETRN-OK
557698*        MOVE L0005-OUTPUT-STRING    TO WAPIN-FLD-VALUE
557698*    END-IF.
557698*
557698*8400-TRANSLATE-UPPER-CASE-NA-X.
557698*    EXIT.
      *
      *---------------------------
      *9000-BUILD-MESSAGE-EXTRACT.
      *---------------------------
      *
      *    PERFORM  0260-2000-GET-MESSAGE
      *        THRU 0260-2000-GET-MESSAGE-X.
      *
007684*    MOVE SPACES                 TO R2120-SEQ-REC-INFO.
      *    MOVE WS-COMPANY-CODE        TO R2120-COMPANY-CODE.
      *    MOVE WPOL-POL-ID            TO R2120-POLICY-ID.
      *    MOVE WS-APPL-FORM-NO-X      TO R2120-APP-NUMBER.
      *    MOVE WS-SERV-AGT-ID         TO R2120-AGENT-CODE.
      *    MOVE RPOL-SERV-BR-ID        TO R2120-BRANCH-CODE.
      *    MOVE WGLOB-MSG-REF-INFO     TO R2120-MESSAGE-NUMBER.
      *    MOVE WGLOB-MSG-TXT          TO R2120-MESSAGE-DATA.
      *
      *    PERFORM  2120-1000-WRITE
      *        THRU 2120-1000-WRITE-X.
      *
      *9000-BUILD-MESSAGE-EXTRACT-X.
      *    EXIT.
      *
      *---------------------------
      *9100-PROCESS-MESSAGE-FIELD.
      *---------------------------
      *
      *    MOVE 'N'                    TO L0280-SIGN-IND.
APEX54*    SET L0280-SPACES-PERMITTED  TO TRUE.
      *    MOVE 2                      TO L0280-LENGTH.
      *    MOVE ZERO                   TO L0280-PRECISION.
      *    MOVE WAPIN-FLD-VALUE        TO L0280-INPUT-DATA.
      *
      *    PERFORM  0280-1000-NUMERIC-EDIT
      *        THRU 0280-1000-NUMERIC-EDIT-X.
      *
APEX54*    IF  NOT L0280-OK
APEX54*        SET WS-ERROR-FOUND      TO TRUE
APEX54*    END-IF.
      *
APEX54*    IF  L0280-OK
APEX54*    AND L0280-OUTPUT NOT = ZERO
      *        IF  L0280-OUTPUT NOT > WS-MESSAGE-ARRAY-MAX-SIZE
APEX54*            SET WS-MESSAGE-SET (L0280-OUTPUT) TO TRUE
      *        ELSE
APEX54*            SET WS-ERROR-FOUND  TO TRUE
      *        END-IF
      *    END-IF.
      *
      *    IF  WS-ERROR-FOUND
APEX54*MSG: NUMERIC FIELD (@1) RECEIVED INVALID VALUE (@2)
      *        MOVE 'AS21000014'       TO WGLOB-MSG-REF-INFO
      *        MOVE RUFLD-UPLD-FLD-NM  TO WGLOB-MSG-PARM (1)
      *        MOVE WUFLD-KEY          TO WGLOB-MSG-PARM (2)
      *        PERFORM  9000-BUILD-MESSAGE-EXTRACT
      *            THRU 9000-BUILD-MESSAGE-EXTRACT-X
      *    END-IF.
      *
      *9100-PROCESS-MESSAGE-FIELD-X.
      *    EXIT.
      *
      *-----------------------
      *9200-APUP-RETURN-ERROR.
      *-----------------------
      *
APEX54*    EVALUATE TRUE
      *
APEX54*        WHEN LAPUP-TRAN-CONV-ERR
      *             PERFORM  9300-UTTB-ERROR-MSG
      *                 THRU 9300-UTTB-ERROR-MSG-X
      *
APEX54*        WHEN LAPUP-UNKNOWN-FIELD-TYPE
APEX54*MSG: INVALID UFLD RECORD (@1,@2), REASON @3
      *             MOVE 'AS21000066'          TO WGLOB-MSG-REF-INFO
      *             MOVE WAPIN-STRUCT-ID       TO WGLOB-MSG-PARM (1)
      *             MOVE WAPIN-FLD-ID          TO WGLOB-MSG-PARM (2)
      *             MOVE 'FIELD TYPE'          TO WGLOB-MSG-PARM (3)
      *             PERFORM  9000-BUILD-MESSAGE-EXTRACT
      *                 THRU 9000-BUILD-MESSAGE-EXTRACT-X
      *
APEX54*        WHEN LAPUP-UNKNOWN-FIELD-ERR
APEX54*MSG: INVALID UFLD RECORD (@1,@2), REASON @3
      *             MOVE 'AS21000066'          TO WGLOB-MSG-REF-INFO
      *             MOVE WAPIN-STRUCT-ID       TO WGLOB-MSG-PARM (1)
      *             MOVE WAPIN-FLD-ID          TO WGLOB-MSG-PARM (2)
      *             MOVE 'FIELD NAME'          TO WGLOB-MSG-PARM (3)
      *             PERFORM  9000-BUILD-MESSAGE-EXTRACT
      *                 THRU 9000-BUILD-MESSAGE-EXTRACT-X
      *
APEX54*        WHEN LAPUP-PLAN-ERR
APEX54*MSG: INVALID UFLD RECORD (@1,@2), REASON @3
      *             MOVE 'AS21000066'          TO WGLOB-MSG-REF-INFO
      *             MOVE WAPIN-STRUCT-ID       TO WGLOB-MSG-PARM (1)
      *             MOVE WAPIN-FLD-ID          TO WGLOB-MSG-PARM (2)
      *             MOVE 'PLAN'                TO WGLOB-MSG-PARM (3)
      *             PERFORM  9000-BUILD-MESSAGE-EXTRACT
      *                 THRU 9000-BUILD-MESSAGE-EXTRACT-X
      *
APEX54*        WHEN LAPUP-FIELD-CONV-ERR
APEX54*MSG: INVALID UFLD RECORD (@1,@2), REASON @3
      *             MOVE 'AS21000066'          TO WGLOB-MSG-REF-INFO
      *             MOVE WAPIN-STRUCT-ID       TO WGLOB-MSG-PARM (1)
      *             MOVE WAPIN-FLD-ID          TO WGLOB-MSG-PARM (2)
      *             MOVE 'FIELD CONVERSION'    TO WGLOB-MSG-PARM (3)
      *             PERFORM  9000-BUILD-MESSAGE-EXTRACT
      *                 THRU 9000-BUILD-MESSAGE-EXTRACT-X
      *
APEX54*        WHEN LAPUP-NUM-CONV-ERR
APEX54*MSG: INVALID UFLD RECORD (@1,@2), REASON @3
      *             MOVE 'AS21000066'          TO WGLOB-MSG-REF-INFO
      *             MOVE WAPIN-STRUCT-ID       TO WGLOB-MSG-PARM (1)
      *             MOVE WAPIN-FLD-ID          TO WGLOB-MSG-PARM (2)
      *             MOVE 'NUMERIC CONVERSION'  TO WGLOB-MSG-PARM (3)
      *             PERFORM  9000-BUILD-MESSAGE-EXTRACT
      *                 THRU 9000-BUILD-MESSAGE-EXTRACT-X
      *
APEX54*        WHEN LAPUP-PADM-READ-ERR
APEX54*MSG: INVALID UFLD RECORD (@1,@2), REASON @3
      *             MOVE 'AS21000066'          TO WGLOB-MSG-REF-INFO
      *             MOVE WAPIN-STRUCT-ID       TO WGLOB-MSG-PARM (1)
      *             MOVE WAPIN-FLD-ID          TO WGLOB-MSG-PARM (2)
      *             MOVE 'PADM READ ERROR'     TO WGLOB-MSG-PARM (3)
      *             PERFORM  9000-BUILD-MESSAGE-EXTRACT
      *                 THRU 9000-BUILD-MESSAGE-EXTRACT-X
      *
APEX54*        WHEN LAPUP-DUP-MEDI-ERR
APEX54*MSG: INVALID UFLD RECORD (@1,@2), REASON @3
      *             MOVE 'AS21000066'          TO WGLOB-MSG-REF-INFO
      *             MOVE WAPIN-STRUCT-ID       TO WGLOB-MSG-PARM (1)
      *             MOVE WAPIN-FLD-ID          TO WGLOB-MSG-PARM (2)
      *             MOVE 'DUPLICATE MEDI'      TO WGLOB-MSG-PARM (3)
      *             PERFORM  9000-BUILD-MESSAGE-EXTRACT
      *                 THRU 9000-BUILD-MESSAGE-EXTRACT-X
      *
APEX54*        WHEN LAPUP-MAX-CVG-ERR
APEX54*MSG: INVALID UFLD RECORD (@1,@2), REASON @3
      *             MOVE 'AS21000066'          TO WGLOB-MSG-REF-INFO
      *             MOVE WAPIN-STRUCT-ID       TO WGLOB-MSG-PARM (1)
      *             MOVE WAPIN-FLD-ID          TO WGLOB-MSG-PARM (2)
      *             MOVE 'MAX COVERAGE'        TO WGLOB-MSG-PARM (3)
      *             PERFORM  9000-BUILD-MESSAGE-EXTRACT
      *                 THRU 9000-BUILD-MESSAGE-EXTRACT-X
      *
APEX54*        WHEN LAPUP-MULT-VALUES-ERR
APEX54*MSG: INVALID UFLD RECORD (@1,@2), REASON @3
      *             MOVE 'AS21000066'          TO WGLOB-MSG-REF-INFO
      *             MOVE WAPIN-STRUCT-ID       TO WGLOB-MSG-PARM (1)
      *             MOVE WAPIN-FLD-ID          TO WGLOB-MSG-PARM (2)
      *             MOVE 'MULTIPLE VALUES'     TO WGLOB-MSG-PARM (3)
      *             PERFORM  9000-BUILD-MESSAGE-EXTRACT
      *                 THRU 9000-BUILD-MESSAGE-EXTRACT-X
      *
APEX54*        WHEN LAPUP-MAX-INFC-PEND-ERR
APEX54*MSG: INVALID UFLD RECORD (@1,@2), REASON @3
      *             MOVE 'AS21000066'          TO WGLOB-MSG-REF-INFO
      *             MOVE WAPIN-STRUCT-ID       TO WGLOB-MSG-PARM (1)
      *             MOVE WAPIN-FLD-ID          TO WGLOB-MSG-PARM (2)
      *             MOVE 'INFORCE PEND COUNT'  TO WGLOB-MSG-PARM (3)
      *             PERFORM  9000-BUILD-MESSAGE-EXTRACT
      *                 THRU 9000-BUILD-MESSAGE-EXTRACT-X
      *
APEX54*        WHEN LAPUP-QUOT-ERR
APEX54*MSG: INVALID UFLD RECORD (@1,@2), REASON @3
      *             MOVE 'AS21000066'          TO WGLOB-MSG-REF-INFO
      *             MOVE WAPIN-STRUCT-ID       TO WGLOB-MSG-PARM (1)
      *             MOVE WAPIN-FLD-ID          TO WGLOB-MSG-PARM (2)
      *             MOVE 'IMMEDIATE ANNUITY QUOTE' TO WGLOB-MSG-PARM (3)
      *             PERFORM  9000-BUILD-MESSAGE-EXTRACT
      *                 THRU 9000-BUILD-MESSAGE-EXTRACT-X
      *
APEX54*    END-EVALUATE.
      *
      *9200-APUP-RETURN-ERROR-X.
      *    EXIT.
      *
      *--------------------
      *9300-UTTB-ERROR-MSG.
      *--------------------
      *
      *MSG: UTTB ERROR: UTTB KEY (@3,@4) APEX FIELD (@1,@2)
      *    MOVE 'AS21000013'               TO WGLOB-MSG-REF-INFO.
      *    MOVE WAPIN-STRUCT-ID            TO WGLOB-MSG-PARM (1).
      *    MOVE WAPIN-FLD-ID               TO WGLOB-MSG-PARM (2).
      *    MOVE RUFLD-UPLD-TTBL-TYP-ID     TO WGLOB-MSG-PARM (3).
      *    MOVE WAPIN-FLD-VALUE            TO WGLOB-MSG-PARM (4).
      *
      *    PERFORM  9000-BUILD-MESSAGE-EXTRACT
      *        THRU 9000-BUILD-MESSAGE-EXTRACT-X.
      *
      *9300-UTTB-ERROR-MSG-X.
      *    EXIT.
      *
      *---------------------
      *9700-UPDATE-PHONETIC.
      *---------------------
      *
APEX54*    PERFORM  2800-1000-BUILD-PARM-INFO
APEX54*        THRU 2800-1000-BUILD-PARM-INFO-X.
      *
APEX54***  IF  RCLI-CLI-SEX-COMPANY
APEX54***      MOVE RCLI-CLI-CO-NM         TO L2800-INPUT-NAME
010154***  ELSE
      *
010154*    IF  RCLI-CLI-SEX-COMPANY
015508*        MOVE RCLI-CLI-CO-NM          TO L2800-SCAN-FIELD
015508*        MOVE RCLNC-CLI-CO-NM         TO L2800-SCAN-FIELD
010154*        PERFORM  2800-1000-CONAM-ENCODE
010154*            THRU 2800-1000-CONAM-ENCODE-X
010154*        IF  L2800-RETRN-OK
010154*            MOVE L2800-PHONETIC-NAME-N
015508*                                    TO RCLI-SUR-CO-NM-PHNT-NUM
016103*                                    TO RCLNC-CLI-CO-NM-PHNT-NUM
016103*                                    TO WS-PHNT-NUM
016103*            MOVE WS-PHNT-TXT        TO RCLNC-CLI-CO-NM-PHNT-TXT
010154*        ELSE
015508*            MOVE ZEROES             TO RCLI-SUR-CO-NM-PHNT-NUM
016103*            MOVE ZEROES             TO RCLNC-CLI-CO-NM-PHNT-NUM
016103*            MOVE ZEROES             TO WS-PHNT-NUM
016103*            MOVE WS-PHNT-TXT        TO RCLNC-CLI-CO-NM-PHNT-TXT
010154*        END-IF
015508*        GO TO 9700-UPDATE-PHONETIC-X
010154*    END-IF.
      *
010154*    IF  RCLI-CLI-SEX-CD = 'M'
010154*    OR  RCLI-CLI-SEX-CD = 'F'
015508*        MOVE RCLI-CLI-SUR-NM        TO L2800-INPUT-NAME
015508*        MOVE RCLNM-CLI-INDV-SUR-NM  TO L2800-INPUT-NAME
      *    END-IF.
      *
APEX54*    PERFORM  2800-2000-PHONETIC-ENCODE
APEX54*        THRU 2800-2000-PHONETIC-ENCODE-X.
      *
010154***  IF  L2800-PHONETIC-NAME NUMERIC
010154*    IF  L2800-RETRN-OK
015508*        MOVE L2800-PHONETIC-NAME-N  TO RCLI-SUR-CO-NM-PHNT-NUM
016103*        MOVE L2800-PHONETIC-NAME-N  TO RCLNM-SUR-NM-PHNT-NUM
016103*        MOVE L2800-PHONETIC-NAME-N  TO WS-PHNT-NUM
016103*        MOVE WS-PHNT-TXT            TO RCLNM-SUR-NM-PHNT-TXT
      *    ELSE
015508*        MOVE ZEROS                  TO RCLI-SUR-CO-NM-PHNT-NUM
016103*        MOVE ZEROS                  TO RCLNM-SUR-NM-PHNT-NUM
016103*        MOVE ZEROS                  TO WS-PHNT-NUM
016103*        MOVE WS-PHNT-TXT            TO RCLNM-SUR-NM-PHNT-TXT
      *    END-IF.
      *
APEX54***  MOVE SPACES                     TO L2800-PARM-INFO.
010154*    PERFORM  2800-1000-BUILD-PARM-INFO
010154*        THRU 2800-1000-BUILD-PARM-INFO-X.
      *
015508*    MOVE RCLI-CLI-GIV-NM            TO L2800-INPUT-NAME.
015508*    MOVE RCLNM-CLI-INDV-GIV-NM      TO L2800-INPUT-NAME.
      *
APEX54***  PERFORM  2800-1000-CONAM-ENCODE
APEX54***      THRU 2800-1000-CONAM-ENCODE-X.
010154*    PERFORM  2800-2000-PHONETIC-ENCODE
010154*        THRU 2800-2000-PHONETIC-ENCODE-X.
      *
010154***  IF  L2800-PHONETIC-NAME NUMERIC
010154*    IF  L2800-RETRN-OK
015508*        MOVE L2800-PHONETIC-NAME-N  TO RCLI-GIV-NM-PHNT-NUM
016103*        MOVE L2800-PHONETIC-NAME-N  TO RCLNM-GIV-NM-PHNT-NUM
016103*        MOVE L2800-PHONETIC-NAME-N  TO WS-PHNT-NUM
016103*        MOVE WS-PHNT-TXT            TO RCLNM-GIV-NM-PHNT-TXT
      *    ELSE
015508*        MOVE ZEROS                  TO RCLI-GIV-NM-PHNT-NUM
016103*        MOVE ZEROS                  TO RCLNM-GIV-NM-PHNT-NUM
016103*        MOVE ZEROS                  TO WS-PHNT-NUM
016103*        MOVE WS-PHNT-TXT            TO RCLNM-GIV-NM-PHNT-TXT
      *    END-IF.
      *
      *9700-UPDATE-PHONETIC-X.
      *    EXIT.
      *
      *---------------------------
015508*9750-UPDATE-EMPLR-PHONETIC.
      *---------------------------
      *
015508*    PERFORM  2800-1000-BUILD-PARM-INFO
015508*        THRU 2800-1000-BUILD-PARM-INFO-X.
      *
015508*    MOVE RCLNC-CLI-CO-NM         TO L2800-SCAN-FIELD.
015508*    PERFORM  2800-1000-CONAM-ENCODE
015508*        THRU 2800-1000-CONAM-ENCODE-X
015508*    IF  L2800-RETRN-OK
015508*        MOVE L2800-PHONETIC-NAME-N
016103*                                TO RCLNC-CLI-CO-NM-PHNT-NUM
016103*                                TO WS-PHNT-NUM
016103*        MOVE WS-PHNT-TXT        TO RCLNC-CLI-CO-NM-PHNT-TXT
015508*    ELSE
016103*        MOVE ZEROES              TO RCLNC-CLI-CO-NM-PHNT-NUM
016103*        MOVE ZEROES              TO WS-PHNT-NUM
016103*        MOVE WS-PHNT-TXT        TO RCLNC-CLI-CO-NM-PHNT-TXT
015508*    END-IF.
      *
015508*9750-UPDATE-EMPLR-PHONETIC-X.
015508*    EXIT.
      *
      *
      *--------------------------
      *9800-UPDATE-PREV-PHONETIC.
      *--------------------------
      *
APEX54***  MOVE SPACES                     TO L2800-PARM-INFO.
010154*    PERFORM  2800-1000-BUILD-PARM-INFO
010154*        THRU 2800-1000-BUILD-PARM-INFO-X.
      *
015508*    MOVE RPRNM-CLI-PREV-SUR-NM      TO L2800-INPUT-NAME.
015508*    MOVE RCLNM-CLI-INDV-SUR-NM      TO L2800-INPUT-NAME.
      *
APEX54***  PERFORM  2800-1000-CONAM-ENCODE
APEX54***      THRU 2800-1000-CONAM-ENCODE-X.
010154*    PERFORM  2800-2000-PHONETIC-ENCODE
010154*        THRU 2800-2000-PHONETIC-ENCODE-X.
      *
010154***  IF  L2800-PHONETIC-NAME NUMERIC
010154*    IF  L2800-RETRN-OK
015508*        MOVE L2800-PHONETIC-NAME-N  TO RPRNM-SUR-NM-PHNT-NUM
016103*        MOVE L2800-PHONETIC-NAME-N  TO RCLNM-SUR-NM-PHNT-NUM
016103*        MOVE L2800-PHONETIC-NAME-N  TO WS-PHNT-NUM
016103*        MOVE WS-PHNT-TXT            TO RCLNM-SUR-NM-PHNT-TXT
      *    ELSE
015508*        MOVE ZEROS                  TO RPRNM-SUR-NM-PHNT-NUM
016103*        MOVE ZEROS                  TO RCLNM-SUR-NM-PHNT-NUM
016103*        MOVE ZEROS                  TO WS-PHNT-NUM
016103*        MOVE WS-PHNT-TXT            TO RCLNM-SUR-NM-PHNT-TXT
      *    END-IF.
      *
APEX54***  MOVE SPACES                     TO L2800-PARM-INFO.
010154*    PERFORM  2800-1000-BUILD-PARM-INFO
010154*        THRU 2800-1000-BUILD-PARM-INFO-X.
      *
015508*    MOVE RPRNM-CLI-PREV-GIV-NM      TO L2800-INPUT-NAME.
015508*    MOVE RCLNM-CLI-INDV-GIV-NM      TO L2800-INPUT-NAME.
      *
APEX54***  PERFORM  2800-1000-CONAM-ENCODE
APEX54***      THRU 2800-1000-CONAM-ENCODE-X.
010154*    PERFORM  2800-2000-PHONETIC-ENCODE
010154*        THRU 2800-2000-PHONETIC-ENCODE-X.
      *
010154***  IF  L2800-PHONETIC-NAME NUMERIC
010154*    IF  L2800-RETRN-OK
015508*        MOVE L2800-PHONETIC-NAME-N  TO RPRNM-GIV-NM-PHNT-NUM
016103*        MOVE L2800-PHONETIC-NAME-N  TO RCLNM-GIV-NM-PHNT-NUM
016103*        MOVE L2800-PHONETIC-NAME-N  TO WS-PHNT-NUM
016103*        MOVE WS-PHNT-TXT            TO RCLNM-GIV-NM-PHNT-TXT
      *    ELSE
015508*        MOVE ZEROS                  TO RPRNM-GIV-NM-PHNT-NUM
016103*        MOVE ZEROS                  TO RCLNM-GIV-NM-PHNT-NUM
016103*        MOVE ZEROS                  TO WS-PHNT-NUM
016103*        MOVE WS-PHNT-TXT            TO RCLNM-GIV-NM-PHNT-TXT
      *    END-IF.
      *
      *9800-UPDATE-PREV-PHONETIC-X.
      *    EXIT.
      *
      *-----------------
      *9999-CLOSE-FILES.
      *-----------------
      *
      * THIS ROUTINE WILL CLOSE ALL FILES PRIOR TO THE PROGRAM FINISH
      *
      *    PERFORM  BCF-4000-CLOSE
      *        THRU BCF-4000-CLOSE-X.
      *
      *    PERFORM  2100-4000-CLOSE
      *        THRU 2100-4000-CLOSE-X.
      *
      *    PERFORM  2110-4000-CLOSE
      *        THRU 2110-4000-CLOSE-X.
      *
      *    PERFORM  2120-4000-CLOSE
      *        THRU 2120-4000-CLOSE-X.
      *
      *    PERFORM  OCF-4000-CLOSE
      *        THRU OCF-4000-CLOSE-X.
      *
      *9999-CLOSE-FILES-X.
      *    EXIT.
      *
      *****************************************************************
      * FILE I/O PROCESS MODULES                                      *
      *****************************************************************
      *
      *COPY ACPPAPIN.
      *COPY ACPPAPUP.
APEX54*COPY ACPPI570.
APEX54*COPY ACPPI953.
      *COPY ACPPPLNB.
      *COPY ACPPUTTB.
      *
APEX54*COPY CCPL0066.
      *
APEX54*COPY CCPL0083.
APEX54*COPY CCPS0083.
      *
APEX54*COPY NCPL0301.
APEX54*COPY NCPS0301.
      *
APEX54*COPY NCPL0302.
APEX54*COPY NCPS0302.
      *
APEX54*COPY CCPL0570.
APEX54*COPY CCPS0570.
557700*COPY CCPL0840.
557700*COPY CCPS0840.
      *
      *COPY ACPL2130.
      *COPY ACPLAPPF.
      *COPY ACPLAPPV.
APEX53*COPY ACPLBENE.
      *COPY ACPLCLI.
      *COPY ACPLCLIA.
      *COPY ACPLCLIB.
557700*COPY ACPLCLIC.
557700*COPY ACPLCLII.
      *COPY ACPLCLIO.
      *COPY ACPLCVG.
      *COPY ACPLCVGA.
      *COPY ACPLCVGC.
      *COPY ACPLMEDI.
      *COPY ACPLPOL.
014178*COPY ACPLPOLP.
      *COPY ACPLPRNM.
      *
      *COPY CCPL0183.
APEX53*COPY CCPL0832.
      *COPY CCPL2430.
APEX54*COPY NCPL2437.
      *COPY CCPL2800.
APEX54*COPY CCPS2800.
APEX53*COPY CCPL5120.
      *
APEX54*COPY CCPL0953.
APEX54*COPY CCPS0953.
      *
APEX54*COPY CCPL6180.
APEX54*COPY CCPS6180.
      *
      *COPY CCPSPGA.
      *
      *COPY NCPL0080.
557020*COPY NCPS0080.
APEX54*COPY CCPL6060.
APEX54*COPY CCPL5950.
      *COPY NCPL0760.
      *COPY NCPL0960.
APEX54*COPY NCPL5850.
      *
      *COPY NCPPRFUP.
      *COPY NCPPRVAL.
      *
      *COPY SCPL0500.
      *
012148*COPY CCPS8240.
012148*COPY CCPL8240.
012148*
557698*COPY XCPL0005.
557698*COPY XCPS0005.
      *COPY XCPL0040.
      *COPY XCPL0260.
008455*COPY XCPS0290.
008455*COPY XCPL0290.
      *COPY XCPL0280.
      *COPY XCPL1640.
014178*COPY XCPL1660.
      *COPY XCPL1670.
      *COPY XCPL1680.
557700*COPY XCPL0015.
557700*COPY CCPL0620.
      *
      **********************************
      * APEX UPLOAD FILE I/O COPYBOOKS *
      **********************************
      *
557700*COPY ACPTUFLD.
      *COPY ACPNUFLD.
      *
      *COPY ACPBUTTB.
      *COPY ACPNUTTB.
557700*COPY ACPTUTTB.
      *
      *COPY ACPN2100.
      *COPY ACPL2100.
      *COPY ACPO2100.
      *
      *COPY ACPA2110.
      *COPY ACPL2110.
      *COPY ACPO2110.
      *
      *COPY ACPA2120.
      *COPY ACPL2120.
      *COPY ACPO2120.
      *
      *******************************
      * ADMIN FILE I/O COPYBOOKS *
      *******************************
      *
APEX53*COPY CCPBBENC.
      *
APEX53*COPY CCPABENE.
557700*COPY CCPBBENE.
557700*COPY CCPTBENE.
APEX53*COPY CCPVBENE.
APEX53*COPY CCPCBENE.
010310*COPY CCPFBENE.
      *
      *COPY CCPNBNKA.
      *COPY CCPABNKA.
      *COPY CCPCBNKA.
      *
      *COPY CCPNBNKB.
      *
      *COPY CCPACLI.
      *COPY CCPNCLI.
      *COPY CCPCCLI.
      *
557700*COPY CCPACLII.
557700*COPY CCPCCLII.
557700*COPY CCPNCLII.
      *
557700*COPY CCPACLIC.
557700*COPY CCPCCLIC.
557700*COPY CCPNCLIC.
      *
      *COPY CCPACLIA.
      *COPY CCPCCLIA.
      *
      *COPY CCPNCLIB.
      *COPY CCPACLIB.
      *COPY CCPCCLIB.
      *
      *COPY CCPNCLIN.
      *
015508*COPY CCPACLNC.
015508*COPY CCPCCLNC.
015508*COPY CCPNCLNC.
      *
015508*COPY CCPACLNM.
015508*COPY CCPCCLNM.
015508*COPY CCPNCLNM.
      *
      *COPY CCPACVG.
      *COPY CCPUCVG.
      *COPY CCPCCVG.
      *
      *COPY CCPACVGC.
      *COPY CCPCCVGC.
      *
      *COPY CCPNEDIT.
      *
      *COPY CCPNPCOM.
007678*COPY CCPNSCOM.
      *
555288*COPY CCPNPSYS.
      *
      *COPY CCPNPD.
      *COPY CCPCPD.
      *
      *COPY CCPNPH.
      *COPY CCPCPH.
      *
      *COPY CCPAPOL.
      *COPY CCPNPOL.
      *COPY CCPUPOL.
      *COPY CCPCPOL.
      *
      *COPY CCPAPOLC.
      *COPY CCPCPOLC.
      *
015508*COPY CCPAPRNM.
015508*COPY CCPCPRNM.
      *
      *COPY CCPARL.
      *COPY CCPCRL.
      *
      *COPY SCPCFC.
014177*COPY SCPUFC.
      *
014177*COPY SCPAFS.
      *COPY SCPCFS.
014177*COPY SCPNFS.
      *
014178*COPY SCPNFH.
014178*
014178*COPY CCPACAIN.
014178*COPY CCPBCAIN.
014178*COPY CCPCCAIN.
014178*COPY CCPACDSI.
014178*COPY CCPBCDSI.
014178*COPY CCPCCDSI.
014178*COPY CCPFCDSI.
014178*COPY CCPUCDSI.
014178*COPY CCPAPOLP.
014178*COPY CCPCPOLP.
014178*
      **************************
      * NBS FILE I/O COPYBOOKS *
      **************************
      *
      *COPY NCPAAPPF.
      *COPY NCPCAPPF.
      *
      *COPY NCPAAPPV.
      *COPY NCPCAPPV.
      *
      *COPY NCPAMEDI.
      *COPY NCPNMEDI.
      *COPY NCPUMEDI.
      *COPY NCPCMEDI.
      *
      **************************
      * TPI FILE I/O COPYBOOKS *
      **************************
      *
      *COPY XCPLBCF.
      *COPY XCPNBCF.
      *COPY XCPOBCF.
      *
      *COPY XCPLOCF.
      *COPY XCPOOCF.
      *
      *COPY XCPL0030.
      *
      *****************************************************************
      **                 END OF PROGRAM ASBM2100                     **
      *****************************************************************
