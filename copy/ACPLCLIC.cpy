      *****************************************************************
      **  MEMBER :  ACPLCLIC                                         **
      **  REMARKS:  CALL TO PROGRAM ASRUCLIC - CLIC PROC PROGRAM     **
      **                                                             **
      *****************************************************************
      **  DATE     AUTH.  DESCRIPTION                                **
      **                                                             **
557700**  30SEP97  KLE    APEX UPLOAD                                **
      *****************************************************************
 
      *---------------------------
       CLIC-1000-PROCESS-CLIC-FIELD.
      *---------------------------
 
           MOVE WGLOB-PREV-PGM-ID       TO WPGWS-PREV-PGM-ID.
           MOVE WPGWS-CRNT-PGM-ID       TO WGLOB-PREV-PGM-ID.
 
           MOVE 'ASRUCLIC'              TO WPGWS-CALL-PGM-ID.
           MOVE WPGWS-CALL-PGM-ID       TO WGLOB-CRNT-PGM-ID.
 
           CALL WPGWS-CALL-PGM-ID USING WGLOB-GLOBAL-AREA
                                        LAPUP-PARM-AREA
                                        RUFLD-REC-INFO
                                        RCLIC-REC-INFO.
 
           MOVE WPGWS-CRNT-PGM-ID       TO WGLOB-CRNT-PGM-ID.
           MOVE WPGWS-PREV-PGM-ID       TO WGLOB-PREV-PGM-ID.
 
       CLIC-1000-PROCESS-CLIC-FIELD-X.
           EXIT.
 
      *****************************************************************
      **                 END OF COPYBOOK ACPLCLIC                    **
      *****************************************************************
