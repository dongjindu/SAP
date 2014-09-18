************************************************************************
* Program Name      : ZIMMGM28I_6022
* Author            : Hakchin Kim
* Creation Date     : 2003.12.10.
* Specifications By : Hakchin Kim
* Pattern           : Report 1-1
* Development Request No : IMMGM28
* Addl Documentation: F/S - IMMGM28 Inventory Replication (Outbound)
* Description       : SAP MM -> External SYSTEM (FTZ)
*
* Modification Logs
* Date       Developer    RequestNo     Description
* 08/16/2004   Shiva       UD1K911897   1. For mat.type 'ROH' and 'ROH1'
*                                       check whether material status
*                                       equal to '12' or '13'.
************************************************************************
REPORT zimmgm28i_6022
                 MESSAGE-ID zmmm
                 NO STANDARD PAGE HEADING
                 LINE-SIZE 400.

INCLUDE zimmgm28i_6022top.   "Data Declaration
INCLUDE zimmgm28i_6022cla.   "Class Part
INCLUDE zimmgm28i_6022o01.   "PBO Part
INCLUDE zimmgm28i_6022i01.   "PAI Part
INCLUDE zimmgm28i_6022f01.   "Perform Library
*--------- Report Transactin Execution --------------------------------*
INITIALIZATION.

START-OF-SELECTION.
  PERFORM get_data.    "get data

  IF it_ztmm_6022_01 IS INITIAL.
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
