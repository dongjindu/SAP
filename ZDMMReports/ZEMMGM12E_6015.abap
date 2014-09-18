************************************************************************
* Program Name      : ZEMMGM12E_6015
* Author            : Hakchin Kim
* Creation Date     : 2003.11.11.
* Specifications By : Hakchin Kim
* Pattern           : Report 1-1
* Development Request No : EMMGM12
* Addl Documentation: F/S - EMMGM12 FTZ-Out KD Calculation
* Description       : This is for Duty drawback
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
************************************************************************
REPORT  zemmgm12e_6015
                 MESSAGE-ID zmmm
                 NO STANDARD PAGE HEADING
                 LINE-SIZE 400.

INCLUDE zemmgm12e_6015top.   "Data Declaration
INCLUDE zemmgm12e_6015cla.   "Class Part
INCLUDE zemmgm12e_6015o01.   "PBO Part
INCLUDE zemmgm12e_6015i01.   "PAI Part
INCLUDE zemmgm12e_6015f01.   "Perform Library.

*--------- Report Transactin Execution --------------------------------*
INITIALIZATION.
  s_period-low    = sy-datum - 8.
  s_period-high   = sy-datum - 1.
  s_period-sign   = 'I'.
  s_period-option = 'BT'.
  APPEND s_period.

START-OF-SELECTION.
  PERFORM get_data.   "get data
*  PERFORM delete_from_ztmm_6015_02.

  IF it_ztmm_6015_02 IS INITIAL.
    MESSAGE s999(zmmm) WITH 'There is no data!'(001).
    EXIT.
  ENDIF.
  PERFORM insert_into_ztmm_6015_02.
  PERFORM display_log.    "Display Data Log

**** Write List
*  PERFORM write_list.
*---------- List Procession Events ------------------------------------*
TOP-OF-PAGE.
*  PERFORM make_col_heading.
