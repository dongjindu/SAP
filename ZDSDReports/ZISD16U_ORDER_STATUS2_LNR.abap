************************************************************************
* Program Name      : ZISD16U_ORDER_STATUS2
* Author            : jun ho choi
* Creation Date     : 2004.03.11.
* Specifications By : jun ho choi
* Pattern           : 5-2
* Development Request No : UD1K907438
* Addl Documentation:
* Description       : order status report for HMA/HAC/GLOVIS
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
*
************************************************************************
REPORT ZISD16U_ORDER_STATUS2 NO STANDARD PAGE HEADING
                             MESSAGE-ID ZMSD.


*
TABLES : ZTPP_WOSUM,
         ZTPP_PMT07JB_A,
         CABN,
         AUSP.


*
DATA : BEGIN OF IT_WOSUM OCCURS 0.
       INCLUDE STRUCTURE ZTPP_WOSUM.
DATA : END OF IT_WOSUM.

DATA : BEGIN OF IT_DOWNFILE OCCURS 0,
       RECORD(157),
       END OF IT_DOWNFILE.

DATA : BEGIN OF IT_DOWNFILE_HMA OCCURS 0,
       RECORD(157),
       END OF IT_DOWNFILE_HMA.

DATA : BEGIN OF IT_DOWNFILE_HAC OCCURS 0,
       RECORD(157),
       END OF IT_DOWNFILE_HAC.

DATA : BEGIN OF IT_AUSP OCCURS 0,
       OBJEK LIKE AUSP-OBJEK,
       ATINN LIKE AUSP-ATINN,
       ATNAM LIKE CABN-ATNAM,
       ATWRT LIKE AUSP-ATWRT,
       ATFLV LIKE AUSP-ATFLV,
       END OF IT_AUSP.

DATA : BEGIN OF IT_AUSP_ALL OCCURS 0,
       OBJEK LIKE AUSP-OBJEK,
       END OF IT_AUSP_ALL.

DATA : BEGIN OF IT_PMT07JB_A OCCURS 0,
       SQDT LIKE ZTPP_PMT07JB_A-SQDT,
       PQTY LIKE ZTPP_PMT07JB_A-PQTY,
       END OF IT_PMT07JB_A.

DATA : BEGIN OF S_WO_SER OCCURS 0,
       SIGN(1),
       OPTION(2),
       LOW LIKE ZTPP_WOSUM-WO_SER,
       HIGH LIKE ZTPP_WOSUM-WO_SER,
       END OF S_WO_SER.

DATA : BEGIN OF S_ATINN OCCURS 0,
       SIGN(1),
       OPTION(2),
       LOW LIKE AUSP-ATINN,
       HIGH LIKE AUSP-ATINN,
       END OF S_ATINN.

DATA : BEGIN OF IT_CABN OCCURS 0,
       ATINN LIKE CABN-ATINN,
       ATNAM LIKE CABN-ATNAM,
       END OF IT_CABN.

DATA : W_CNT TYPE I,
       W_N_9(9) TYPE N,
       W_N_8(8) TYPE N.

DATA : W_LASTDAY(2) TYPE N,
       W_POS TYPE I,
       W_DIST LIKE ZTPP_PMT07JB_A-DIST,
       W_DSN_B(15) VALUE '/BACKUP2/EDI/',
       W_DSN(50).

DATA : VARIANT LIKE INDX-SRTFD VALUE 'ISD16_01'.

DATA : EVENTID LIKE TBTCJOB-EVENTID.


*
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
PARAMETERS : P_DATE LIKE SY-DATUM+2(4) DEFAULT SY-DATUM+2(4) OBLIGATORY.
SELECTION-SCREEN END OF BLOCK B1.


*
START-OF-SELECTION.
  PERFORM DOWNLOAD_FILE.


*
END-OF-SELECTION.


*&---------------------------------------------------------------------*
*&      Form  DOWNLOAD_FILE
*&---------------------------------------------------------------------*
FORM DOWNLOAD_FILE.
  EXPORT P_DATE TO   DATABASE INDX(ZS) ID VARIANT.

   MESSAGE I000 WITH 'STARTING BATCH JOB'.

  EVENTID = 'ZISD16_02'.

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
