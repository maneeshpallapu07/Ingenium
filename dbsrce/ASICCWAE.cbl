      *************************
       IDENTIFICATION DIVISION.
      *************************

       PROGRAM-ID. ASICCWAE.

       COPY XCWWCRHT.

      *****************************************************************
      **  MEMBER :  ASICCWAE                                         **
      **  REMARKS:  THIS PROGRAM IS USED TO INITIALIZE EACH FIELD IN **
      **            THE CWA ERROR TABLE LAYOUT                       **
      *****************************************************************
      **  RELEASE   DESCRIPTION                                      **
      **                                                             **
      **  06AUG09   CREATED FOR CWAE PROCESSING                      **
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

       COPY XCWWPGWS REPLACING '$VAR1' BY 'ASICCWAE'.

       COPY XCWWWKDT.
      /
      *****************
       LINKAGE SECTION.
      *****************

           EXEC SQL INCLUDE ACFWCWAE  END-EXEC.

           EXEC SQL INCLUDE ACFRCWAE  END-EXEC.

      /
       PROCEDURE DIVISION USING WCWAE-IO-WORK-AREA
                                RCWAE-REC-INFO.

      ***************
       0000-MAINLINE.
      ***************

           EVALUATE TRUE

               WHEN WCWAE-RQST-INIT-TBL-LAYOUT
                    PERFORM  1000-INIT-TBL-LAYOUT
                        THRU 1000-INIT-TBL-LAYOUT-X

               WHEN OTHER
                    SET  WCWAE-IO-ERROR      TO  TRUE

           END-EVALUATE.


           GOBACK.


       0000-MAINLINE-X.
           EXIT.


      **********************
       1000-INIT-TBL-LAYOUT.
      **********************

           MOVE SPACES           TO  RCWAE-REC-INFO.
           MOVE WWKDT-ZERO-DT    TO  RCWAE-CWA-UPLD-DT.
           MOVE ZERO             TO  RCWAE-RECPT-NUM-N.
           MOVE ZERO             TO  RCWAE-RECPT-AMT.
           MOVE WWKDT-ZERO-DT    TO  RCWAE-RECPT-DT.
           MOVE WCWAE-KEY        TO  RCWAE-KEY.


       1000-INIT-TBL-LAYOUT-X.
           EXIT.


      *****************************************************************
      **                 END OF PROGRAM ASICCWAE                     **
      *****************************************************************
