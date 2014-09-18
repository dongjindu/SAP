************************************************************************
* Program Name      : ZIMMGM26I_6020
* Author            : Hakchin Kim
* Creation Date     : 2003.12.10.
* Specifications By : Hakchin Kim
* Pattern           : Report 1-1
* Development Request No : IMMGM26
* Addl Documentation: F/S - IMMGM26 Vendor Master(Outbound)
* Description       : SAP MM -> External SYSTEM(FTZ)
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
************************************************************************
REPORT zimmgm26i_6020
                 MESSAGE-ID zmmm
                 NO STANDARD PAGE HEADING
                 LINE-SIZE 400.

INCLUDE zimmgm26i_6020top.   "Data Declaration
INCLUDE zimmgm26i_6020cla.   "Class Part
INCLUDE zimmgm26i_6020o01.   "PBO Part
INCLUDE zimmgm26i_6020i01.   "PAI Part
INCLUDE zimmgm26i_6020f01.   "Perform Library
*--------- Report Transactin Execution --------------------------------*
INITIALIZATION.
  s_udate-low    = sy-datum - 1.
*  s_udate-high   = sy-datum.
  s_udate-sign   = 'I'.
  s_udate-option = 'BT'.
  APPEND s_udate.

START-OF-SELECTION.
  PERFORM get_data.    "get data

  IF it_ztmm_6020_01 IS INITIAL.
    MESSAGE s999(zmmm) WITH 'There is no data!'(001).
    EXIT.
  ENDIF.
  PERFORM process_data.   "Process Data
  PERFORM z_fca_eai_interface_log.
  IF sy-batch IS INITIAL.   "Not Backgroung Processing
    PERFORM display_log.    "Display Data Log
  ELSE.
    MESSAGE s999(zmmm) WITH 'Application Doc. No.'
                        w_zdocno
                        'is created !'.

  ENDIF.
*---------- List Procession Events ------------------------------------*
TOP-OF-PAGE.
*  PERFORM make_col_heading.
