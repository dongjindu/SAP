************************************************************************
* Program Name      : ZRMMGM06_COIL_REQ_SUM
* Author            : hj.song
* Creation Date     : 2003.11.27
* Specifications By : hj.song
* Pattern           : Report 1-1
* Development Request No : UD1K902172
* Addl Documentation:
* Description       : Daily Requirements Q'ty by Material and delivery
*                     date
* Modification Logs
* Date            Developer        RequestNo      Description
* 2003.11.27.     hj.song          UD1K902172     Initial Coding
* 2004.03.17.     Hakchin Kim      UD1K908283     Interface Adding
*
*
************************************************************************

REPORT zrmmgm06_coil_req_sum
                 MESSAGE-ID zmmm
                 NO STANDARD PAGE HEADING
                 LINE-SIZE 400.

INCLUDE zrmmgm06_coil_req_sumtop.   "Data Declaration
INCLUDE zrmmgm06_coil_req_sumcla.   "Class Part
INCLUDE zrmmgm06_coil_req_sumo01.   "PBO Part
INCLUDE zrmmgm06_coil_req_sumi01.   "PAI Part
INCLUDE zrmmgm06_coil_req_sumf01.   "Perform Library
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
*/Begin of Added by Hakchin(20040414)
  w_reqdate = p_date.
*/End of Added by Hakchin(20040414)

  PERFORM get_data.    "get data

* set alv parameters
  PERFORM alv_field_build.

  IF it_list[] IS INITIAL.
    MESSAGE s999(zmmm) WITH 'There is no data!'(001).
    EXIT.
  ENDIF.

  PERFORM display_list.

*  PERFORM process_data_by_section.  "Process Data by Section
*  PERFORM z_fca_eai_interface_log.
*  IF sy-batch IS INITIAL.   "Not Backgroung Processing
*    PERFORM display_log.    "Display Data Log
*  ELSE.
*    WRITE:/ sy-batch.
*  ENDIF.
*----------------------------------------------------------------------*
* List Procession Events
*----------------------------------------------------------------------*
*TOP-OF-PAGE.
**  PERFORM make_col_heading.
