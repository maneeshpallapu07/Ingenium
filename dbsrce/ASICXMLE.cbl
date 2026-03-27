      *************************
       IDENTIFICATION DIVISION.
      *************************

       PROGRAM-ID. ASICXMLE.

       COPY XCWWCRHT.

      *****************************************************************
      **  MEMBER :  ASICXMLE                                         **
      **  REMARKS:  THIS PROGRAM IS USED TO INITIALIZE EACH FIELD IN **
      **            THE XML MESSAGE EXTRACT TABLE LAYOUT             **
      *****************************************************************
      **  RELEASE   DESCRIPTION                                      **
      **                                                             **
      **  23JUL09   CREATED FOR XMLE PROCESSING                      **
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

       COPY XCWWPGWS REPLACING '$VAR1' BY 'ASICXMLE'.

       COPY XCWWWKDT.
      /
      *****************
       LINKAGE SECTION.
      *****************

           EXEC SQL INCLUDE ACFWXMLE  END-EXEC.

           EXEC SQL INCLUDE ACFRXMLE  END-EXEC.

      /
       PROCEDURE DIVISION USING WXMLE-IO-WORK-AREA
                                RXMLE-REC-INFO.

      ***************
       0000-MAINLINE.
      ***************

           EVALUATE TRUE

               WHEN WXMLE-RQST-INIT-TBL-LAYOUT
                    PERFORM  1000-INIT-TBL-LAYOUT
                        THRU 1000-INIT-TBL-LAYOUT-X

               WHEN OTHER
                    SET  WXMLE-IO-ERROR      TO  TRUE

           END-EVALUATE.


           GOBACK.


       0000-MAINLINE-X.
           EXIT.


      **********************
       1000-INIT-TBL-LAYOUT.
      **********************

           MOVE SPACES           TO  RXMLE-REC-INFO.
           MOVE WXMLE-KEY        TO  RXMLE-KEY.


       1000-INIT-TBL-LAYOUT-X.
           EXIT.


      *****************************************************************
      **                 END OF PROGRAM ASICXMLE                     **
      *****************************************************************
