      *****************************************************************
      **  MEMBER :  ACPLCLII                                         **
      **  REMARKS:  CALL TO PROGRAM ASRUCLII - CLII PROC PROGRAM     **
      **                                                             **
      *****************************************************************
      **  DATE     AUTH.  DESCRIPTION                                **
      **                                                             **
557700**  30SEP97  KLE    APEX UPLOAD                                **
      *****************************************************************
 
      *-----------------------------
       CLII-1000-PROCESS-CLII-FIELD.
      *-----------------------------
 
           MOVE WGLOB-PREV-PGM-ID       TO WPGWS-PREV-PGM-ID.
           MOVE WPGWS-CRNT-PGM-ID       TO WGLOB-PREV-PGM-ID.
 
           MOVE 'ASRUCLII'              TO WPGWS-CALL-PGM-ID.
           MOVE WPGWS-CALL-PGM-ID       TO WGLOB-CRNT-PGM-ID.
 
           CALL WPGWS-CALL-PGM-ID USING WGLOB-GLOBAL-AREA
                                        LAPUP-PARM-AREA
                                        RUFLD-REC-INFO
                                        RCLII-REC-INFO.
 
           MOVE WPGWS-CRNT-PGM-ID       TO WGLOB-CRNT-PGM-ID.
           MOVE WPGWS-PREV-PGM-ID       TO WGLOB-PREV-PGM-ID.
 
       CLII-1000-PROCESS-CLII-FIELD-X.
           EXIT.
 
      *****************************************************************
      **                 END OF COPYBOOK ACPLCLII                    **
      *****************************************************************
