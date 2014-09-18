************************************************************************
* Program Name      : ZIMMGM16I_6011
* Author            : Hakchin Kim
* Creation Date     : 2003.11.29.
* Specifications By : Hakchin Kim
* Pattern           : Report 1-1
* Development Request No : IMMGM16
* Addl Documentation: F/S - IMMGM16 Raw BOM Interface(/nCS03)
* Description       : SAP MM -> HYUNDAI ULSAN BOM SYSTEM (HYSCO)
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
************************************************************************
REPORT zimmgm16i_6011
                 MESSAGE-ID zmmm
                 NO STANDARD PAGE HEADING
                 LINE-SIZE 400.

INCLUDE zimmgm16i_6011top.   "Data Declaration
INCLUDE zimmgm16i_6011cla.   "Class Part
INCLUDE zimmgm16i_6011o01.   "PBO Part
INCLUDE zimmgm16i_6011i01.   "PAI Part
INCLUDE zimmgm16i_6011f01.   "Perform Library
*--------- Report Transactin Execution --------------------------------*
START-OF-SELECTION.
  PERFORM get_data.    "get data

  IF it_ztmm_6011_01 IS INITIAL.
    MESSAGE s999(zmmm) WITH 'There is no data!'(001).
    EXIT.
  ENDIF.
  PERFORM process_data.   "Process Data
  PERFORM z_fca_eai_interface_log.
  IF sy-batch IS INITIAL.
    PERFORM display_log.    "Display Data Log
  ELSE.
    MESSAGE s999(zmmm) WITH 'Application Doc. No.'
                        w_zdocno
                        'is created !'.

  ENDIF.
*---------- List Procession Events ------------------------------------*
TOP-OF-PAGE.
*  PERFORM make_col_heading.
