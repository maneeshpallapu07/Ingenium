      *****************************************************************
      **  MEMBER :  ACPL9399                                         **
      **  REMARKS:  CALL TO PROGRAM ASRQ9399                         **
      **            BATCH I/O PROGRAM FOR PROCESSING DUPLICATE       **
      **            APPLICATION ID'S FROM ILLUSTRATION/APPLICATION   **
      **            SYSTEM                                           **
      **                                                             **
      *****************************************************************
      **  DATE     AUTH.  DESCRIPTION                                **
      **                                                             **
P00697**  25OCT02  DPK    CREATED FOR '9399' FILE PROCESSING         **
      *****************************************************************
 
      *---------------
       9399-1000-LINK.
      *---------------
 
           MOVE WGLOB-PREV-PGM-ID       TO WPGWS-PREV-PGM-ID.
           MOVE WPGWS-CRNT-PGM-ID       TO WGLOB-PREV-PGM-ID.
 
           MOVE 'ASRQ9399'              TO WPGWS-CALL-PGM-ID.
           MOVE WPGWS-CALL-PGM-ID       TO WGLOB-CRNT-PGM-ID.
 
           CALL WPGWS-CALL-PGM-ID USING WGLOB-GLOBAL-AREA
                                        W9399-SEQ-IO-WORK-AREA
                                        R9399-SEQ-REC-INFO.
 
           MOVE WPGWS-CRNT-PGM-ID       TO WGLOB-CRNT-PGM-ID.
           MOVE WPGWS-PREV-PGM-ID       TO WGLOB-PREV-PGM-ID.
 
       9399-1000-LINK-X.
           EXIT.
 
      *****************************************************************
      **                 END OF COPYBOOK ACPL9399                    **
      *****************************************************************
