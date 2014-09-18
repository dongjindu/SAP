************************************************************************
* Program Name      : ZIMMGM15I_6013
* Author            : Hakchin Kim
* Creation Date     : 2004.03.22
* Specifications By : Hakchin Kim
* Pattern           : Report 1-1
* Development Request No : UD1K908416
* Addl Documentation:
* Description       : IMMGM15 Requiement Info
*                     date
* Modification Logs
* Date            Developer        RequestNo      Description
* 2004.03.22.     Hakchin Kim      UD1K908416     Interface Coding
*
*
************************************************************************
REPORT ZIMMGM15I_6013
                 MESSAGE-ID zmmm
                 NO STANDARD PAGE HEADING
                 LINE-SIZE 400.

INCLUDE ZIMMGM15I_6013TOP_040323.
*INCLUDE ZIMMGM15I_6013TOP.   "Data Declaration
INCLUDE ZIMMGM15I_6013CLA_040323.
*INCLUDE ZIMMGM15I_6013CLA.   "Class Part
INCLUDE ZIMMGM15I_6013OO1_040323.
*INCLUDE ZIMMGM15I_6013OO1.   "PBO Part
INCLUDE ZIMMGM15I_6013I01_040323.
*INCLUDE ZIMMGM15I_6013I01.   "PAI Part
INCLUDE ZIMMGM15I_6013F01_040323.
*INCLUDE ZIMMGM15I_6013F01.   "Perform Library
*----------------------------------------------------------------------*
* Report Transactin Execution
*----------------------------------------------------------------------*
INITIALIZATION.
**  s_budat-low    = sy-datum - 1.
***  s_BUDAT-high   = sy-datum - 1.
**  s_budat-sign   = 'I'.
**  s_budat-option = 'BT'.
**  APPEND s_budat.

START-OF-SELECTION.
  PERFORM get_data.    "get data

* set alv parameters
  PERFORM alv_field_build.

  IF it_list[] IS INITIAL.
    MESSAGE s999(zmmm) WITH 'There is no data!'(001).
    EXIT.
  ENDIF.
  PERFORM process_data. "Process Data
  PERFORM z_fca_eai_interface_log.
  IF sy-batch IS INITIAL.   "Not Backgroung Processing
    PERFORM display_log.    "Display Data Log
  ENDIF.

*----------------------------------------------------------------------*
* List Procession Events
*----------------------------------------------------------------------*
*TOP-OF-PAGE.
**  PERFORM make_col_heading.
