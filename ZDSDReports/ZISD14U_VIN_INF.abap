************************************************************************
* Program Name      : ZISD14U_VIN_INF
* Author            : HONG KI KIM
* Creation Date     : 2003.09.23.
* Specifications By : HONG KI KIM
* Pattern           : 1-2
* Development Request No : UD1K904910
* Addl Documentation:
* Description       : VIN INFORMATION DOWNLOAD
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
*
************************************************************************
REPORT  ZISD14U_VIN_INF     NO STANDARD PAGE HEADING
                            MESSAGE-ID ZMSD.
*----------------------------------------------------------------------*
* TABLE
*----------------------------------------------------------------------*
TABLES: AUSP, CABN.
*----------------------------------------------------------------------
* TYPES
*----------------------------------------------------------------------
TYPE-POOLS: SLIS, KKBLO, VRM.
*----------------------------------------------------------------------*
* INTERNAL TABLE
*----------------------------------------------------------------------*
DATA : BEGIN OF ITAB1 OCCURS 0,
          OBJEK  LIKE AUSP-OBJEK,
          ATINN  LIKE AUSP-ATINN,
          ATWRT  LIKE AUSP-ATWRT,
          ATFLV  LIKE AUSP-ATFLV,
       END OF ITAB1.

DATA : BEGIN OF ITAB3 OCCURS 0,
          OBJEK  LIKE AUSP-OBJEK,
          ATINN  LIKE AUSP-ATINN,
          ATWRT  LIKE AUSP-ATWRT,
          ATFLV  LIKE AUSP-ATFLV,
       END OF ITAB3.

  DATA: BEGIN OF IT_OBJEK OCCURS 0,
            OBJEK LIKE AUSP-OBJEK,
        END OF IT_OBJEK.

  DATA: L_ATINN1 LIKE CABN-ATINN,
*      L_ATINN2 LIKE CABN-ATINN,
        L_ATINN3 LIKE CABN-ATINN,
        L_ATINN4 LIKE CABN-ATINN,
*      L_ATINN5 LIKE CABN-ATINN,
*      L_ATINN6 LIKE CABN-ATINN,
        L_ATINN7 LIKE CABN-ATINN,
        L_ATINN8 LIKE CABN-ATINN,
        L_ATINN9 LIKE CABN-ATINN,
        L_ATINN10 LIKE CABN-ATINN,
*      L_ATINN11 LIKE CABN-ATINN,
        L_ATINN12 LIKE CABN-ATINN,
        L_ATINN13 LIKE CABN-ATINN,
        L_ATINN14 LIKE CABN-ATINN,
        L_ATINN15 LIKE CABN-ATINN.

DATA : BEGIN OF ITAB2 OCCURS 0,

          F1(17), "VIN
          F2(5),  "MANUFACURER
          F3(5),  "DISTRUBUTOR CODE
          F4(8),  "PRODUCTION DATE
          F5(8),  "SHIPPING DATE
          F6(20), "VEHICLE SPEC
          F7(15), "WORK ORDER NUMBER
          F8(11), "ENGINE NUMBER
          F9(1),  "AIR/CON CODE
          F10(1), "TRANSMISSION CODE
          F11(1), "SPARE FLAG
          F12(5), "KEY NUMBER

       END OF ITAB2.

DATA : ETAB  LIKE ITAB OCCURS 0 WITH HEADER LINE.
*----------------------------------------------------------------------*
* BDC TABLE
*----------------------------------------------------------------------*
DATA : BEGIN OF BDCDATA OCCURS 0.
        INCLUDE STRUCTURE BDCDATA.
DATA : END OF BDCDATA.

DATA : BEGIN OF MESSTAB OCCURS 0.
        INCLUDE STRUCTURE BDCMSGCOLL.
DATA : END OF MESSTAB.

*----------------------------------------------------------------------*
* GLOBAL VARIABLE
*----------------------------------------------------------------------*
DATA : W_CHECK(1).
DATA : W_ANSWER(1).

DATA : W_N_8(8) TYPE N.

DATA : VARIANT LIKE INDX-SRTFD VALUE 'ISD14_01'.

DATA : EVENTID LIKE TBTCJOB-EVENTID.


*----------------------------------------------------------------------*
* SELECTION SCREEN LAYOUT
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS : S_DATE  FOR AUSP-ATWRT. " ATFLV
SELECTION-SCREEN END OF BLOCK B1 .


START-OF-SELECTION.
  PERFORM DOWNLOAD_FILE.


*&---------------------------------------------------------------------*
*&      Form  DOWNLOAD_FILE
*&---------------------------------------------------------------------*
FORM DOWNLOAD_FILE.
  EXPORT S_DATE TO   DATABASE INDX(ZS) ID VARIANT.

  MESSAGE I000 WITH 'STARTING BATCH JOB'.

  EVENTID = 'ZISD14_01'.

  CALL FUNCTION 'BP_EVENT_RAISE'
    EXPORTING
      EVENTID                      = EVENTID
*     EVENTPARM                    = ' '
*     TARGET_INSTANCE              = ' '
   EXCEPTIONS
     BAD_EVENTID                  = 1
     EVENTID_DOES_NOT_EXIST       = 2
     EVENTID_MISSING              = 3
     RAISE_FAILED                 = 4
     OTHERS                       = 5.
ENDFORM.                    " DOWNLOAD_FILE
