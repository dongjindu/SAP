************************************************************************
* Program Name      : ZIPP109I_APS_7CW.
* Author            : DongYeop Han
* Creation Date     : 2003.08.25.
* Specifications By : DongYeop Han
* Pattern           : 1.1
* Development Request No :UD1K901928
* Addl Documentation:
* Description       : MITU Order (7CW).
*
* Modification Logs
* Date       Developer    RequestNo    Description
************************************************************************
REPORT ZIPP109I_APS_7CW1
               NO STANDARD PAGE HEADING
               LINE-SIZE  1023
               MESSAGE-ID zmpp.

************************************************************************
*              INCLUDE                                                 *
************************************************************************
TABLES: AUSP,
        ZTPP_PMT07CW.


*INTERNAL TABLE
DATA: BEGIN OF IT_OBJEK OCCURS 0,
      OBJEK LIKE AUSP-OBJEK,
      END OF IT_OBJEK.

DATA: IT_T07CW LIKE ZSPP_PMT07CW OCCURS 0 WITH HEADER LINE,
      IT_PMT07CW_RFC LIKE ZSPP_PMT07CW_RFC OCCURS 0 WITH HEADER LINE.

*WORK_AREA
DATA: P_ATINN LIKE   AUSP-ATINN,       "USING CONVERSION FUNTION
      P_ATWRT TYPE   AUSP-ATWRT,
      P_ATFLV TYPE   AUSP-ATFLV,
      P_ATFOR TYPE   CABN-ATFOR,
      WA_NUMERIC(15) TYPE N,
      WA_INTEGER     TYPE I.

*----------------------------------------------------------------------*
*  CONSTANS DECLARATION
*----------------------------------------------------------------------*
DATA : C_DEST(10) VALUE 'WMPP01'.   "Outbound Interface Destination


************************************************************************
*              SELECTION SCREEN LAYOUT                                 *
************************************************************************
SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME .
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: p_RUN          TYPE c AS CHECKBOX  default 'X'.
SELECTION-SCREEN COMMENT  (55) text-001 FOR FIELD p_RUN.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN: END OF BLOCK b1.

INCLUDE zipp109i_aps_7cw_fo1.
************************************************************************

************************************************************************
*              START-OF-SELECTION PROCESSING                           *
************************************************************************
START-OF-SELECTION.
  check p_run = 'X'  .
*  IF p_check IS INITIAL.
    PERFORM basic_data_select.
    PERFORM data_join.
*  ELSE.                               "RE PROCESSING
*    PERFORM error_data_select.
*  ENDIF.
* PERFORM to_legacy.
  PERFORM table_update.

END-OF-SELECTION.
* PERFORM data_write.
