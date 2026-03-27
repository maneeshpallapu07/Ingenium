      *****************************************************************
      **  MEMBER :  ACPLADUP                                         **
      **  REMARKS:  CALL TO PROGRAM ASRQADUP                         **
      **            BATCH I/O PROGRAM FOR PROCESSING DUPLICATE       **
      **            APPLICATION ID'S FROM ILLUSTRATION/APPLICATION   **
      **            SYSTEM                                           **
      **                                                             **
      *****************************************************************
      **  DATE     AUTH.  DESCRIPTION                                **
      **                                                             **
P00697**  25OCT02  DPK    CREATED FOR 'ADUP' FILE PROCESSING         **
      *****************************************************************
 
      *---------------
       ADUP-1000-LINK.
      *---------------
 
           MOVE WGLOB-PREV-PGM-ID       TO WPGWS-PREV-PGM-ID.
           MOVE WPGWS-CRNT-PGM-ID       TO WGLOB-PREV-PGM-ID.
 
           MOVE 'ASRQADUP'              TO WPGWS-CALL-PGM-ID.
           MOVE WPGWS-CALL-PGM-ID       TO WGLOB-CRNT-PGM-ID.
 
           CALL WPGWS-CALL-PGM-ID USING WGLOB-GLOBAL-AREA
                                        WADUP-SEQ-IO-WORK-AREA
                                        RADUP-SEQ-REC-INFO.
 
           MOVE WPGWS-CRNT-PGM-ID       TO WGLOB-CRNT-PGM-ID.
           MOVE WPGWS-PREV-PGM-ID       TO WGLOB-PREV-PGM-ID.
 
       ADUP-1000-LINK-X.
           EXIT.
 
      *****************************************************************
      **                 END OF COPYBOOK ACPLADUP                    **
      *****************************************************************
