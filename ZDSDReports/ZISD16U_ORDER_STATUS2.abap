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
* Date          Developer    RequestNo    Description
* 04/26/2005    Shiva        UD1K915740   Changed parameter as sy-datum.
* 05/17/2005    chris        UD1K916079   Change the data selection
*                                         logic to pick up the unshipped
*                                         previous vehicles to show
*                                         their status.
************************************************************************
REPORT ZISD16U_ORDER_STATUS2 NO STANDARD PAGE HEADING
                             MESSAGE-ID ZMSD.


*
TABLES : ZTPP_WOSUM,
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
       W_DSN_B(50) VALUE '/sapmnt/UP2/EDI/',
       W_DSN(50),
       p_date(4) type n.

DATA : VARIANT LIKE INDX-SRTFD VALUE 'ISD16_02'.

DATA : EVENTID LIKE TBTCJOB-EVENTID.


*
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
PARAMETERS : p_date1 like sy-datum default sy-datum OBLIGATORY.
SELECTION-SCREEN END OF BLOCK B1.

p_date = p_date1+2(4).

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

  submit ZISD16U_ORDER_STATUS2_bg and return.

*  MESSAGE I000 WITH 'STARTING BATCH JOB'.
*
*  EVENTID = 'ZISD16_02'.
*
*  CALL FUNCTION 'BP_EVENT_RAISE'
*    EXPORTING
*      EVENTID                      = EVENTID
**     EVENTPARM                    = ' '
**     TARGET_INSTANCE              = ' '
*   EXCEPTIONS
*     BAD_EVENTID                  = 1
*     EVENTID_DOES_NOT_EXIST       = 2
*     EVENTID_MISSING              = 3
*     RAISE_FAILED                 = 4
*     OTHERS                       = 5.
ENDFORM.                    " DOWNLOAD_FILE
