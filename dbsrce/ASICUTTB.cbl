      *************************
       IDENTIFICATION DIVISION.
      *************************
 
       PROGRAM-ID. ASICUTTB.
 
       COPY XCWWCRHT.
 
      *****************************************************************
      **  MEMBER : ASICUTTB                                          **
      **  REMARKS: THIS PROGRAM IS USED TO INITIALIZE EACH FIELD IN  **
      **           THE UPLOAD TRANSLATION TABLE LAYOUT               **
      *****************************************************************
      **  DATE     AUTH.  DESCRIPTION                                **
      **                                                             **
      **  30NOV95  RLE    CREATED FOR UTTB TABLE PROCESSING          **
53-067**  30NOV95  RLE    MOVED INITIAL VALUES INTO PROGRAM          **
      *****************************************************************
      /
      **********************
       ENVIRONMENT DIVISION.
      **********************
 
      ***************
       DATA DIVISION.
      ***************
      /
      *************************
       WORKING-STORAGE SECTION.
      *************************
 
       COPY XCWWPGWS REPLACING '$VAR1' BY 'ASICUTTB'.
 
       COPY XCWWWKDT.
      /
      *****************
       LINKAGE SECTION.
      *****************
 
           EXEC SQL INCLUDE ACFWUTTB  END-EXEC.
 
           EXEC SQL INCLUDE ACFRUTTB  END-EXEC.
 
      /
       PROCEDURE DIVISION USING WUTTB-IO-WORK-AREA
                                RUTTB-REC-INFO.
 
      ***************
       0000-MAINLINE.
      ***************
 
           EVALUATE TRUE
 
               WHEN WUTTB-RQST-INIT-TBL-LAYOUT
                    PERFORM  1000-INIT-TBL-LAYOUT
                        THRU 1000-INIT-TBL-LAYOUT-X
 
               WHEN OTHER
                    SET  WUTTB-IO-ERROR      TO  TRUE
 
           END-EVALUATE.
 
 
           GOBACK.
 
 
       0000-MAINLINE-X.
           EXIT.
 
 
      **********************
       1000-INIT-TBL-LAYOUT.
      **********************
 
           MOVE SPACES           TO  RUTTB-REC-INFO.
           MOVE WWKDT-ZERO-DT    TO  RUTTB-PREV-UPDT-DT.
           MOVE WUTTB-KEY        TO  RUTTB-KEY.
 
 
       1000-INIT-TBL-LAYOUT-X.
           EXIT.
 
 
      *****************************************************************
      **                 END OF PROGRAM ASICUTTB                     **
      *****************************************************************
