      *****************************************************************
      **  MEMBER :  ACPLCLIB                                         **
      **  REMARKS:  CALL TO ASRUCLIB                                 **
      **                                                             **
      *****************************************************************
      **  DATE     AUTH.  DESCRIPTION                                **
      **                                                             **
APEX52**  30NOV94  JJS    APEX UPLOAD UPGRADE TO RELEASE 5.2         **
APEX54**  31DEC96  TJS    MODIFICATIONS FOR MAINTAINABILITY          **
557660**  30SEP97  AJP    STANDARDIZED LINKAGE                       **
      *****************************************************************
 
      *-----------------------------
       CLIB-1000-PROCESS-CLIB-FIELD.
      *-----------------------------
 
557660     MOVE WGLOB-PREV-PGM-ID       TO WPGWS-PREV-PGM-ID.
557660     MOVE WPGWS-CRNT-PGM-ID       TO WGLOB-PREV-PGM-ID.
 
557660     MOVE 'ASRUCLIB'              TO WPGWS-CALL-PGM-ID.
557660     MOVE WPGWS-CALL-PGM-ID       TO WGLOB-CRNT-PGM-ID.
 
557660     CALL WPGWS-CALL-PGM-ID USING WGLOB-GLOBAL-AREA
557660*    CALL 'ASRUCLIB' USING WGLOB-GLOBAL-AREA
                                        LAPUP-PARM-AREA
                                        LCLIB-PARM-AREA
                                        RUFLD-REC-INFO
                                        RCLIB-REC-INFO.
 
557660     MOVE WPGWS-CRNT-PGM-ID       TO WGLOB-CRNT-PGM-ID.
557660     MOVE WPGWS-PREV-PGM-ID       TO WGLOB-PREV-PGM-ID.
 
       CLIB-1000-PROCESS-CLIB-FIELD-X.
           EXIT.
 
      *****************************************************************
      **                 END OF COPYBOOK ACPLCLIB                    **
      *****************************************************************
