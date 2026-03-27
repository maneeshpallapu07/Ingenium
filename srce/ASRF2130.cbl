      *************************
       IDENTIFICATION DIVISION.
      *************************
 
       PROGRAM-ID. ASRF2130.
 
       COPY XCWWCRHT.
 
      *****************************************************************
      **  MEMBER :  ASRF2130                                         **
      **  REMARKS:  MATCHING PROCEDURE USED TO MATCH A CLIENT NAME   **
      **            WITH NAMES CURRENTLY FOUND ON THE CLIENT FILE.   **
      **                                                             **
      **  DOMAIN :  CL                                               **
      **  CLASS  :  FD                                               **
      *****************************************************************
      **  DATE     AUTH.  DESCRIPTION                                **
      **                                                             **
APEX52**  30NOV94   JJS    NEW VERSION FOR APEX RELEASE 5.2          **
APEX53**  30NOV95   JJS    UPGRADE TO INGENIUM 5.3 & WINAPEX 1.0,    **
APEX53**                   ADD WORKING STORAGE COPYBOOK XCWWPGWS,    **
APEX53**                   CHANGES TO SUPPORT I/O PROGRAMS           **
APEX54**  31DEC96   TJS    MODIFICATIONS FOR MAINTAINABILITY         **
010375**  31MAR98   552    PERFORMANCE ENHANCEMENTS                  **
010309**  30OCT98   56     IMPROVE PERFORMANCE OF CLIENT SEARCH      **
015508**  15DEC99   60     CLIENT ENHANCEMENT FOR JAPAN              **
014590**  15DEC99  60     ARCHITECTURAL CHANGES                      **
P00213**  10DEC01   DPK    IMPROVED CLIENT SEARCH FOR KATAKANA       **
EN7281**  22DEC09   CTS    CHANGED FOR CLIENT MATCHING FOR ADDRESS   **
MP334A**  03NOV17  CTS    CHANGES DONE FOR SUCCESSOR CATGEORY CODE   **
      *****************************************************************
      /
      **********************
       ENVIRONMENT DIVISION.
      **********************
 
      ***************
       DATA DIVISION.
      ***************
 
       WORKING-STORAGE SECTION.
 
APEX53 COPY XCWWPGWS REPLACING '$VAR1' BY 'ASRF2130'.
 
       COPY SQLCA.
 
014590*COPY XCWL0030.
 
       01  WS-WORK-AREAS.
010375*    05  WS-FIRST-INITIAL                 PIC X(01).
010375     05  WS-CLI-GIV-NM.
P00213*        10  WS-CLI-GIV-NM-INIT           PIC X(01).
P00213         10  WS-CLI-GIV-NM-INIT           PIC X(25).
P00213*        10  FILLER                       PIC X(24).
010309/
010309 COPY XCWWWKDT.
      /
      *****************************************************************
      *  I/O COPYBOOKS                                                *
      *****************************************************************
015508*COPY CCFWCLIE.
015508 COPY CCFWCLNN.
015508 COPY CCFRCLNM.

015508 COPY CCFWCLID.
       COPY CCFRCLI.
MP334A COPY CCFWCLI.       
MP334A COPY CCFHCLI.       
      /
      ****************************************************************
      *  CALLED MODULE PARAMETER INFORMATION                         *
      ****************************************************************
EN7281 COPY CCWL2440.
      /
      *****************
       LINKAGE SECTION.
      *****************
 
       01  WGLOB-GLOBAL-AREA.
       COPY XCWWGLOB.
       COPY ACWL2130.
      /
       PROCEDURE DIVISION USING WGLOB-GLOBAL-AREA
                                L2130-PARM-AREA.
      *--------------
       0000-MAINLINE.
      *--------------
 
           MOVE  ZERO  TO WGLOB-RETURN-CODE.
 
           PERFORM 2130-0000-ALPHA-MATCH
              THRU 2130-0000-ALPHA-MATCH-X.
 
       0000-MAINLINE-X.
           GOBACK.
      /
      *****************************************************************
      * PROCESSING COPYBOOKS                                          *
      *****************************************************************
       COPY ACPP2130.
      /
      *****************************************************************
      *    LINKAGE PROCESSING COPYBOOKS                               *
      *****************************************************************
EN7281 COPY CCPL2440.
      /
      *****************************************************************
      *  FILE I/O PROCESS MODULES                                     *
      *****************************************************************
015508*COPY CCPBCLIE.
015508 COPY CCPBCLNN.
015508 COPY CCPBCLID.
MP334A COPY CCPNCLI.
      /
      *****************************************************************
      *  ERROR HANDLING ROUTINES                                      *
      *****************************************************************
       COPY XCPL0030.
 
      *****************************************************************
      **                 END OF PROGRAM ASRF2130                     **
      *****************************************************************
