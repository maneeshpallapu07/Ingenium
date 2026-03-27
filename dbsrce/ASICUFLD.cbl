      *************************
       IDENTIFICATION DIVISION.
      *************************
 
       PROGRAM-ID. ASICUFLD.
 
       COPY XCWWCRHT.
 
      *****************************************************************
      **  MEMBER : ASICUFLD                                          **
      **  REMARKS: THIS PROGRAM IS USED TO INITIALIZE EACH FIELD IN  **
      **           THE UPLOAD DEFINED FIELD TABLE LAYOUT             **
      *****************************************************************
      **  DATE     AUTH.  DESCRIPTION                                **
      **                                                             **
      **  30NOV95  RLE    CREATED FOR UFLD TABLE PROCESSING          **
53-060**  30NOV95  GPB    REMOVED FOUR FIELDS                        **
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
 
       COPY XCWWPGWS REPLACING '$VAR1' BY 'ASICUFLD'.
 
       COPY XCWWWKDT.
      /
      *****************
       LINKAGE SECTION.
      *****************
 
           EXEC SQL INCLUDE ACFWUFLD  END-EXEC.
 
           EXEC SQL INCLUDE ACFRUFLD  END-EXEC.
 
      /
       PROCEDURE DIVISION USING WUFLD-IO-WORK-AREA
                                RUFLD-REC-INFO.
 
      ***************
       0000-MAINLINE.
      ***************
 
           EVALUATE TRUE
 
               WHEN WUFLD-RQST-INIT-TBL-LAYOUT
                    PERFORM  1000-INIT-TBL-LAYOUT
                        THRU 1000-INIT-TBL-LAYOUT-X
 
               WHEN OTHER
                    SET  WUFLD-IO-ERROR      TO  TRUE
 
           END-EVALUATE.
 
 
           GOBACK.
 
 
       0000-MAINLINE-X.
           EXIT.
 
 
      **********************
       1000-INIT-TBL-LAYOUT.
      **********************
 
           MOVE SPACES           TO  RUFLD-REC-INFO.
           MOVE WWKDT-ZERO-DT    TO  RUFLD-PREV-UPDT-DT.
           MOVE 'C'              TO  RUFLD-UPLD-FLD-TYP-CD.
53-060*    MOVE ZEROS            TO  RUFLD-UPLD-FLD-OFFST-LEN-N.
53-060*    MOVE ZEROS            TO  RUFLD-UPLD-FLD-LEN-N.
53-060*    MOVE ZEROS            TO  RUFLD-UPLD-FLD-DCML-LEN-N.
53-060*    MOVE ZEROS            TO  RUFLD-UPLD-FLD-STRG-LEN-N.
           MOVE WUFLD-KEY        TO  RUFLD-KEY.
 
 
       1000-INIT-TBL-LAYOUT-X.
           EXIT.
 
 
      *****************************************************************
      **                 END OF PROGRAM ASICUFLD                     **
      *****************************************************************
