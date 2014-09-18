************************************************************************
* Program Name      : ZIMMGM25I_6019
* Author            : Hakchin Kim
* Creation Date     : 2003.12.10.
* Specifications By : Hakchin Kim
* Pattern           : Report 1-1
* Development Request No : IMMGM25
* Addl Documentation: F/S - IMMGM25 Material_HS_InfoRecord(Outbound)
* Description       : SAP MM -> External SYSTEM(FTZ)
*
* Modification Logs
* Date       Developer    RequestNo    Description
*08/16/2004   Shiva       UD1K911897   1. For mat.type 'ROH' and 'ROH1'
*                                         check whether material status
*                                         equal to '12' or '13'.
*                                      2. Change status code to 'D' only
*                                      for mat.type 'ROH' and 'ROH1'.
*08/20/2004   Shiva       UD1K911952   Get order address from ADRC.
************************************************************************
REPORT zimmgm25i_6019
                 MESSAGE-ID zmmm
                 NO STANDARD PAGE HEADING
                 LINE-SIZE 400.

INCLUDE zimmgm25i_6019top.   "Data Declaration
INCLUDE zimmgm25i_6019cla.   "Class Part
INCLUDE zimmgm25i_6019o01.   "PBO Part
INCLUDE zimmgm25i_6019i01.   "PAI Part
INCLUDE zimmgm25i_6019f01.   "Perform Library
*--------- Report Transactin Execution --------------------------------*
INITIALIZATION.
  s_udate-low    = sy-datum - 1.
*  s_udate-high   = sy-datum - 1.
  s_udate-sign   = 'I'.
  s_udate-option = 'BT'.
  APPEND s_udate.

START-OF-SELECTION.
  PERFORM get_data.    "get data

  IF it_ztmm_6019_01 IS INITIAL.
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
