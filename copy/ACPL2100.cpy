      *****************************************************************
      **  MEMBER :  ACPL2100                                         **
      **  REMARKS:  CALL TO PROGRAM ASRQ2100                         **
      **            BATCH I/O PROGRAM FOR THE                        **
      **            APEX UPLOAD DATA FILE                            **
      **                                                             **
      *****************************************************************
      **  DATE     AUTH.  DESCRIPTION                                **
      **                                                             **
      **  28FEB95  DEV    CREATED FOR '2100' FILE PROCESSING         **
APEX54**  31DEC96  TJS    MODIFICATIONS FOR MAINTAINABILITY          **
557660**  30SEP97  AJP    STANDARDIZING LINKAGE                      **
007684**  30OCT98  56     CHECKPOINT/RESTART                         **
      *****************************************************************
 
      *---------------
       2100-1000-LINK.
      *---------------
 
557660     MOVE WGLOB-PREV-PGM-ID       TO WPGWS-PREV-PGM-ID.
557660     MOVE WPGWS-CRNT-PGM-ID       TO WGLOB-PREV-PGM-ID.
 
557660     MOVE 'ASRQ2100'              TO WPGWS-CALL-PGM-ID.
557660     MOVE WPGWS-CALL-PGM-ID       TO WGLOB-CRNT-PGM-ID.
 
557660*    CALL 'ASRQ2100' USING WGLOB-GLOBAL-AREA
557660     CALL WPGWS-CALL-PGM-ID USING WGLOB-GLOBAL-AREA
007684                                  W2100-SEQ-IO-WORK-AREA
007684                                  R2100-SEQ-REC-INFO.
 
557660     MOVE WPGWS-CRNT-PGM-ID       TO WGLOB-CRNT-PGM-ID.
557660     MOVE WPGWS-PREV-PGM-ID       TO WGLOB-PREV-PGM-ID.
 
       2100-1000-LINK-X.
           EXIT.
 
      *****************************************************************
      **                 END OF COPYBOOK ACPL2100                    **
      *****************************************************************
