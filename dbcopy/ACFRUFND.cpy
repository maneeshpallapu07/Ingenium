      *****************************************************************
      **  MEMBER :  ACFRUFND                                         **
      **  REMARKS:  APPLICATION UPLOAD FUND TABLE LAYOUT             **
      *****************************************************************
      **  RELEASE   DESCRIPTION                                      **
      **                                                             **
MFFUPL**  01OCT05   CREATED FOR UFND PROCESSING                      **
      *****************************************************************

       01  RUFND-REC-INFO.
           05  RUFND-KEY.
               10  RUFND-APP-ID                 PIC X(15).
               10  RUFND-STCKR-ID               PIC X(11).
               10  RUFND-PLAN-ID                PIC X(15).
               10  RUFND-FND-ID                 PIC X(05).
           05  RUFND-SA-INIT-PREM-PCT           PIC S9(03)V9(04) COMP-3.
           05  RUFND-SA-SUBSEQ-PREM-PCT         PIC S9(03)V9(04) COMP-3.
           05  RUFND-SA-INIT-LMPSM-PCT          PIC S9(03)V9(04) COMP-3.
           05  RUFND-SA-CNVR-FND-PCT            PIC S9(03)V9(04) COMP-3.
           05  FILLER                           PIC X(20).

      *****************************************************************
      **                 END OF COPYBOOK ACFRUFND                    **
      *****************************************************************
