      *****************************************************************
      **  MEMBER :  ACPLCVGA                                         **
      **  REMARKS:  CALL TO PROGRAM ASRUCVGA                         **
      **                                                             **
      *****************************************************************
      **  DATE     AUTH.  DESCRIPTION                                **
      **                                                             **
APEX52**  30NOV94  JJS    APEX UPLOAD UPGRADE TO RELEASE 5.2         **
APEX54**  31DEC96  TJS    MODIFICATIONS FOR MAINTAINABILITY          **
557660**  30SEP97  AJP    STANDARDIZED LINKAGE                       **
      *****************************************************************
 
      *-----------------------------
       CVGA-1000-PROCESS-CVGA-FIELD.
      *-----------------------------
 
557660     MOVE WGLOB-PREV-PGM-ID       TO WPGWS-PREV-PGM-ID.
557660     MOVE WPGWS-CRNT-PGM-ID       TO WGLOB-PREV-PGM-ID.
 
557660     MOVE 'ASRUCVGA'              TO WPGWS-CALL-PGM-ID.
557660     MOVE WPGWS-CALL-PGM-ID       TO WGLOB-CRNT-PGM-ID.
 
557660     CALL WPGWS-CALL-PGM-ID USING WGLOB-GLOBAL-AREA
557660*    CALL 'ASRUCVGA' USING WGLOB-GLOBAL-AREA
                                 LAPUP-PARM-AREA
                                 RUFLD-REC-INFO
                                 RCVGA-REC-INFO.

      557660     CALL WPGWS-CALL-PGM-ID USING WGLOB-GLOBAL-AREA
557660*    CALL 'ASRUCVGA' USING WGLOB-GLOBAL-AREA
                                 LAPUP-PARM-AREA
                                 RUFLD-REC-INFO
                                 RCVGA-REC-INFO.
 
 
557660     MOVE WPGWS-CRNT-PGM-ID       TO WGLOB-CRNT-PGM-ID.
557660     MOVE WPGWS-PREV-PGM-ID       TO WGLOB-PREV-PGM-ID.
 
       CVGA-1000-PROCESS-CVGA-FIELD-X.
           EXIT.
 
      *****************************************************************
      **                 END OF COPYBOOK ACPLCVGA                    **
      *****************************************************************

