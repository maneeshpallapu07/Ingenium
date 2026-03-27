      *****************************************************************
      **  MEMBER :  ACPL2130                                         **
      **  REMARKS:  CALL TO PROGRAM ASRF2130.                        **
      **            MATCHING PROCEDURE USED TO MATCH A CLIENT NAME   **
      **            WITH NAMES CURRENTLY FOUND ON THE CLIENT FILE.   **
      **                                                             **
      *****************************************************************
      **  DATE     AUTH.  DESCRIPTION                                **
      **                                                             **
APEX52**  30NOV94  JJS    NEW VERSION FOR APEX RELEASE 5.2           **
APEX54**  31DEC96  TJS    MODIFICATIONS FOR MAINTAINABILITY          **
557660**  30SEP97  AJP    STANDARDIZED LINKAGE                       **
      *****************************************************************
 
      *----------------------
       2130-0000-ALPHA-MATCH.
      *----------------------
 
      *
      * LINK TO THE MODULE
      *
 
557660     MOVE WGLOB-PREV-PGM-ID       TO WPGWS-PREV-PGM-ID.
557660     MOVE WPGWS-CRNT-PGM-ID       TO WGLOB-PREV-PGM-ID.
 
557660     MOVE 'ASRF2130'              TO WPGWS-CALL-PGM-ID.
557660     MOVE WPGWS-CALL-PGM-ID       TO WGLOB-CRNT-PGM-ID.
 
557660     CALL WPGWS-CALL-PGM-ID USING WGLOB-GLOBAL-AREA
557660*    CALL  'ASRF2130'  USING  WGLOB-GLOBAL-AREA
                                        L2130-PARM-AREA.
 
557660     MOVE WPGWS-CRNT-PGM-ID       TO WGLOB-CRNT-PGM-ID.
557660     MOVE WPGWS-PREV-PGM-ID       TO WGLOB-PREV-PGM-ID.
 
       2130-0000-ALPHA-MATCH-X.
           EXIT.
 
      *****************************************************************
      **                 END OF COPYBOOK ACPL2130                    **
      *****************************************************************
