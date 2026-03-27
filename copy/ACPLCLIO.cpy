      *****************************************************************
      **  MEMBER :  ACPLCLIO                                         **
      **  REMARKS:  CALL TO ASRUCLIO                                 **
      **                                                             **
      *****************************************************************
      **  DATE     AUTH.  DESCRIPTION                                **
      **                                                             **
APEX52**  30NOV94  JJS    APEX UPLOAD UPGRADE TO RELEASE 5.2         **
APEX54**  31DEC96  TJS    MODIFICATIONS FOR MAINTAINABILITY          **
557660**  30SEP97  AJP    STANDARDIZED LINKAGE                       **
      *****************************************************************
 
      *-----------------------------
       CLIO-1000-PROCESS-CLIO-FIELD.
      *-----------------------------
 
557660     MOVE WGLOB-PREV-PGM-ID       TO WPGWS-PREV-PGM-ID.
557660     MOVE WPGWS-CRNT-PGM-ID       TO WGLOB-PREV-PGM-ID.
 
557660     MOVE 'ASRUCLIO'              TO WPGWS-CALL-PGM-ID.
557660     MOVE WPGWS-CALL-PGM-ID       TO WGLOB-CRNT-PGM-ID.
 
557660     CALL WPGWS-CALL-PGM-ID USING WGLOB-GLOBAL-AREA
557660*    CALL 'ASRUCLIO' USING WGLOB-GLOBAL-AREA
                                        LAPUP-PARM-AREA
                                        LCLIO-PARM-AREA
                                        RUFLD-REC-INFO
APEX54                                  L5850-PARM-INFO.
 
557660     MOVE WPGWS-CRNT-PGM-ID       TO WGLOB-CRNT-PGM-ID.
557660     MOVE WPGWS-PREV-PGM-ID       TO WGLOB-PREV-PGM-ID.
 
       CLIO-1000-PROCESS-CLIO-FIELD-X.
           EXIT.
 
      *****************************************************************
      **                 END OF COPYBOOK ACPLCLIO                    **
      *****************************************************************
