      *****************************************************************
      **  MEMBER : ACPEPLAN                                          **
      **  REMARKS: TRANSLATE LIFE AND MEDICAL RIDERS FROM THE        **
      **           ILLUSTRATOR INTO INGENIUM.                        **
      **                                                             **
      *****************************************************************
      **  DATE     AUTH.  DESCRIPTION                                **
      **                                                             **
PR006Y**  11FEB03  DPK    CREATED TO TRANSLATE RIDER CODES FROM UCVG **
      *****************************************************************
 
      *---------------
       PLAN-1000-EDIT.
      *---------------
 
           MOVE 'APLAN'         TO WTTAB-ETBL-TYP-ID.
           MOVE 'AD'            TO WTTAB-TTAB-ADMIN-APPL-ID.
 
           PERFORM  TTAB-1000-READ
               THRU TTAB-1000-READ-X.
 
       PLAN-1000-EDIT-X.
           EXIT.
      
      *****************************************************************
      **                 END OF COPYBOOK ACPEPLAN                    **
      *****************************************************************
