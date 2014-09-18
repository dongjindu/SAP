************************************************************************
* Program Name      : ZIMMGM24I_6017
* Author            : Hakchin Kim
* Creation Date     : 2003.12.10.
* Specifications By : Hakchin Kim
* Pattern           : Report 1-1
* Development Request No : IMMGM24
* Addl Documentation: F/S - IMMGM24 Material Master(Outbound)
* Description       : SAP MM -> External SYSTEM(FTZ)
*
* Modification Logs
* Date        Developer    RequestNo      Description
* 08/16/2004   Shiva       UD1K911897   1. For mat.type 'ROH' and 'ROH1'
*                                         check whether material status
*                                         equal to '12' or '13'.
* 04/03/2006   Manju       UD1K919940   Exclude MIP Parts / Convert IM
*                                       to NM
************************************************************************
REPORT zimmgm24i_6017
                 MESSAGE-ID zmmm
                 NO STANDARD PAGE HEADING
                 LINE-SIZE 400.

INCLUDE zimmgm24i_6017top.   "Data Declaration
INCLUDE zimmgm24i_6017cla.   "Class Part
INCLUDE zimmgm24i_6017o01.   "PBO Part
INCLUDE zimmgm24i_6017i01.   "PAI Part
INCLUDE zimmgm24i_6017f01.   "Perform Library
*--------- Report Transactin Execution --------------------------------*
INITIALIZATION.
  s_udate-low    = sy-datum - 1.
*  s_udate-high   = sy-datum - 1.
  s_udate-sign   = 'I'.
  s_udate-option = 'BT'.
  APPEND s_udate.

START-OF-SELECTION.
  PERFORM get_data.    "get data

  IF it_ztmm_6017_01 IS INITIAL.
    MESSAGE s999(zmmm) WITH 'There is no data!'(001).
    EXIT.
  ENDIF.
  PERFORM process_data.   "Process Data
  PERFORM z_fca_eai_interface_log.
  IF sy-batch IS INITIAL.   "Not Backgroung Processing
    PERFORM display_log.    "Display Data Log
  ELSE.                     "Background Processing
    MESSAGE s999(zmmm) WITH 'Application Doc. No.'
                            w_zdocno
                            'is created !'.

    SUBMIT zimmgm25i_6019
                 WITH s_udate IN s_udate
*                 VIA SELECTION-SCREEN
                 AND RETURN.
  ENDIF.

*---------- List Procession Events ------------------------------------*
TOP-OF-PAGE.
*  PERFORM make_col_heading.
