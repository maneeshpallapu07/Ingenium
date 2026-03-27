      *****************************************************************
      **  MEMBER :  ACPL9400                                         **
      **  REMARKS:  CALL TO PROGRAM ASRQ9400                         **
      **            BATCH I/O PROGRAM FOR PROCESSING THE             **
      **            APPLICATION ID'S FROM ILLUSTRATION/APPLICATION   **
      **            SYSTEM                                           **
      **                                                             **
      *****************************************************************
      **  DATE     AUTH.  DESCRIPTION                                **
      **                                                             **
      **  31AUG01  DPK    CREATED FOR '9400' FILE PROCESSING         **
      *****************************************************************
 
      *---------------
       9400-1000-LINK.
      *---------------
 
           MOVE WGLOB-PREV-PGM-ID       TO WPGWS-PREV-PGM-ID.
           MOVE WPGWS-CRNT-PGM-ID       TO WGLOB-PREV-PGM-ID.
 
           MOVE 'ASRQ9400'              TO WPGWS-CALL-PGM-ID.
           MOVE WPGWS-CALL-PGM-ID       TO WGLOB-CRNT-PGM-ID.
 
           CALL WPGWS-CALL-PGM-ID USING WGLOB-GLOBAL-AREA
                                        W9400-SEQ-IO-WORK-AREA
                                        R9400-SEQ-REC-INFO.
 
           MOVE WPGWS-CRNT-PGM-ID       TO WGLOB-CRNT-PGM-ID.
           MOVE WPGWS-PREV-PGM-ID       TO WGLOB-PREV-PGM-ID.
 
       9400-1000-LINK-X.
           EXIT.
 
      *****************************************************************
      **                 END OF COPYBOOK ACPL9400                    **
      *****************************************************************
