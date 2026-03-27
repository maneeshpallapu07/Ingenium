      *****************************************************************
      **  MEMBER :  ACPLCLI                                          **
      **  REMARKS:  CALL TO PROGRAM ASRUCLI - CLI PROCESSING PROGRAM **
      **                                                             **
      *****************************************************************
      **  DATE     AUTH.  DESCRIPTION                                **
      **                                                             **
APEX52**  30NOV94  JJS    APEX UPLOAD UPGRADE TO RELEASE 5.2         **
APEX54**  31DEC96  TJS    MODIFICATIONS FOR MAINTAINABILITY          **
557660**  30SEP97  AJP    STANDARDIZED LINKAGE                       **
015508**  15DEC99  60     CLIENT ENHANCEMENT FOR JAPAN               **
      *****************************************************************
 
      *---------------------------
       CLI-1000-PROCESS-CLI-FIELD.
      *---------------------------
 
557660     MOVE WGLOB-PREV-PGM-ID       TO WPGWS-PREV-PGM-ID.
557660     MOVE WPGWS-CRNT-PGM-ID       TO WGLOB-PREV-PGM-ID.
 
557660     MOVE 'ASRUCLI'               TO WPGWS-CALL-PGM-ID.
557660     MOVE WPGWS-CALL-PGM-ID       TO WGLOB-CRNT-PGM-ID.
 
557660     CALL WPGWS-CALL-PGM-ID USING WGLOB-GLOBAL-AREA
557660*    CALL 'ASRUCLI' USING WGLOB-GLOBAL-AREA
                                        LAPUP-PARM-AREA
                                        RUFLD-REC-INFO
015508*                                 RCLI-REC-INFO.
015508                                  RCLI-REC-INFO
015508                                  RCLNC-REC-INFO
015508                                  LCLI-PARM-AREA.
 
557660     MOVE WPGWS-CRNT-PGM-ID       TO WGLOB-CRNT-PGM-ID.
557660     MOVE WPGWS-PREV-PGM-ID       TO WGLOB-PREV-PGM-ID.
 
       CLI-1000-PROCESS-CLI-FIELD-X.
           EXIT.
 
      *****************************************************************
      **                 END OF COPYBOOK ACPLCLI                     **
      *****************************************************************
