      *****************************************************************
      **  MEMBER :  ACPLPOL                                          **
      **  REMARKS:  CALL TO PROGRAM ASRUPOL                          **
      **                                                             **
      *****************************************************************
      **  DATE     AUTH.  DESCRIPTION                                **
      **                                                             **
APEX52**  30NOV94  JJS    APEX UPLOAD UPGRADE TO RELEASE 5.2         **
APEX54**  31DEC96  TJS    MODIFICATIONS FOR MAINTAINABILITY          **
557660**  30SEP97  AJP    STANDARDIZED LINKAGE                       **
      *****************************************************************
 
      *---------------------------
       POL-1000-PROCESS-POL-FIELD.
      *---------------------------
 
557660     MOVE WGLOB-PREV-PGM-ID       TO WPGWS-PREV-PGM-ID.
557660     MOVE WPGWS-CRNT-PGM-ID       TO WGLOB-PREV-PGM-ID.
 
557660     MOVE 'ASRUPOL'               TO WPGWS-CALL-PGM-ID.
557660     MOVE WPGWS-CALL-PGM-ID       TO WGLOB-CRNT-PGM-ID.
 
557660     CALL WPGWS-CALL-PGM-ID USING WGLOB-GLOBAL-AREA
557660*    CALL 'ASRUPOL' USING WGLOB-GLOBAL-AREA
                                        LAPUP-PARM-AREA
                                        LPOL-PARM-AREA
                                        RUFLD-REC-INFO
                                        RPOL-REC-INFO.
 
557660     MOVE WPGWS-CRNT-PGM-ID       TO WGLOB-CRNT-PGM-ID.
557660     MOVE WPGWS-PREV-PGM-ID       TO WGLOB-PREV-PGM-ID.
 
       POL-1000-PROCESS-POL-FIELD-X.
           EXIT.
 
      *****************************************************************
      **                 END OF COPYBOOK ACPLPOL                     **
      *****************************************************************
