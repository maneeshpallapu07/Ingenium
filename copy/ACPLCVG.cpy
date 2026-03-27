      *****************************************************************
      **  MEMBER :  ACPLCVG                                          **
      **  REMARKS:  CALL TO PROGRAM ASRUCVG                          **
011930**  REMARKS:  CALL TO PROGRAM ASRSCVG                          **
      **                                                             **
      *****************************************************************
      **  DATE     AUTH.  DESCRIPTION                                **
      **                                                             **
APEX52**  30NOV94  JJS    APEX UPLOAD UPGRADE TO RELEASE 5.2         **
APEX54**  31DEC96  TJS    MODIFICATIONS FOR MAINTAINABILITY          **
557660**  30SEP97  AJP    STANDARDIZED LINKAGE                       **
010311**  30OCT98  56     CODE CLEAN-UP AND STANDARDIZATION          **
011930**  30OCT98  56     PROGRAM ASRUCVG RENAMED TO ASRSCVG         **
      *****************************************************************
 
      *---------------------------
       CVG-1000-PROCESS-CVG-FIELD.
      *---------------------------
 
557660     MOVE WGLOB-PREV-PGM-ID       TO WPGWS-PREV-PGM-ID.
557660     MOVE WPGWS-CRNT-PGM-ID       TO WGLOB-PREV-PGM-ID.
 
011930*    MOVE 'ASRUCVG'               TO WPGWS-CALL-PGM-ID.
011930     MOVE 'ASRSCVG'               TO WPGWS-CALL-PGM-ID.
557660     MOVE WPGWS-CALL-PGM-ID       TO WGLOB-CRNT-PGM-ID.
 
557660     CALL WPGWS-CALL-PGM-ID USING WGLOB-GLOBAL-AREA
557660*    CALL 'ASRUCVG' USING WGLOB-GLOBAL-AREA
010311                                  LPGA-PARM-INFO
                                        LAPUP-PARM-AREA
                                        LCVG-PARM-AREA
                                        RUFLD-REC-INFO
                                        RQT-REC-INFO
                                        RPOL-REC-INFO
                                        WCVGS-WORK-AREA.
 
557660     MOVE WPGWS-CRNT-PGM-ID       TO WGLOB-CRNT-PGM-ID.
557660     MOVE WPGWS-PREV-PGM-ID       TO WGLOB-PREV-PGM-ID.
 
       CVG-1000-PROCESS-CVG-FIELD-X.
           EXIT.
 
      *****************************************************************
      **                 END OF COPYBOOK ACPLCVG                     **
      *****************************************************************
