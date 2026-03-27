      *****************************************************************
      **  MEMBER :  ACFRUCNV                                         **
      **  REMARKS:  APPLICATION UPLOAD POLICY CONVERSION TABLE LAYOUT**
      **  LENGTH :  196                                              **
      *****************************************************************
      **  RELEASE   DESCRIPTION                                      **
      **                                                             **
      **  15AUG02   CREATED FOR UCNV TABLE PROCESSING                **
      *****************************************************************

       01  RUCNV-REC-INFO.
           05  RUCNV-KEY.
               10  RUCNV-APP-ID                 PIC X(15).
               10  RUCNV-SEQ-NUM                PIC X(03).
               10  RUCNV-SEQ-NUM-N              REDEFINES
                   RUCNV-SEQ-NUM                PIC 9(03).
           05  RUCNV-CNVR-ISS-EFF-DT            PIC X(10).
           05  RUCNV-CNVR-XPRY-DT               PIC X(10).
                 05  RUCNV-CNVR-ISS-EFF-DT            PIC X(10).
           05  RUCNV-CNVR-XPRY-DT               PIC X(10).
           05  RUCNV-ORIG-SML-PROD-CD           PIC X(03).
           05  RUCNV-ORIG-POL-ID                PIC X(07).
           05  RUCNV-ORIG-POL-ISS-DT            PIC X(10).
           05  RUCNV-ORIG-PMT-MTHD-CD           PIC X(02).
           05  RUCNV-ORIG-POL-MAT-DT            PIC X(10).
           05  RUCNV-ESC-XEMP-RSRV-AMT          PIC S9(13)V9(02) COMP-3.
           05  RUCNV-ESC-XEMP-SPREM-AMT         PIC S9(13)V9(02) COMP-3.
           05  RUCNV-ESC-RSRV-AMT               PIC S9(13)V9(02) COMP-3.
           05  RUCNV-ESC-SPREM-AMT              PIC S9(13)V9(02) COMP-3.
           05  RUCNV-ESC-ADJ-CHRG-AMT           PIC S9(13)V9(02) COMP-3.
           05  RUCNV-ORIG-MO-PREM-AMT           PIC S9(13)V9(02) COMP-3.
           05  RUCNV-CNVR-ORIG-DB-AMT           PIC S9(13)V9(02) COMP-3.
           05  RUCNV-CNVR-ORIG-HOSP-AMT         PIC S9(13)V9(02) COMP-3.
           05  RUCNV-ORIG-LTD-PREM-PD           PIC S9(13)V9(02) COMP-3.
           05  RUCNV-ORIG-TOT-LOAN-AMT          PIC S9(13)V9(02) COMP-3.
           05  RUCNV-ORIG-AFYC-AMT              PIC S9(13)V9(02) COMP-3.
           05  RUCNV-ORIG-CPREM-AMT             PIC S9(13)V9(02) COMP-3.
           05  RUCNV-ORIG-COMM-FACE-AMT         PIC S9(13)V9(02) COMP-3.
           05  RUCNV-ORIG-PAY-NUM               PIC S9(03) COMP-3.
           05  FILLER                           PIC X(20).

      *****************************************************************
      **                 END OF COPYBOOK ACFRUCNV                    **
      *****************************************************************
