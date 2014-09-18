************************************************************************
* Program Name      : ZEMMPM44E_6030
* Author            : Hakchin Kim
* Creation Date     : 2004.04.01.
* Specifications By : Hakchin Kim
* Pattern           : Report 1-1
* Development Request No : EMMPM44
* Addl Documentation: F/S - EMMPM44 Revaluation
* Description       : EMMPM44 Revaluation
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
************************************************************************
REPORT zemmpm44e_6030
                 MESSAGE-ID zmmm
                 NO STANDARD PAGE HEADING
                 LINE-SIZE 400.

INCLUDE zemmpm44e_6030top.   "Data Declaration
INCLUDE zemmpm44e_6030cla.   "Class Part
INCLUDE zemmpm44e_6030o01.   "PBO Part
INCLUDE zemmpm44e_6030i01.   "PAI Part
INCLUDE zemmpm44e_6030f01.   "Perform Library
*----------------------------------------------------------------------*
* Report Transactin Execution
*----------------------------------------------------------------------*
INITIALIZATION.
*/ processed before the selection screen is displayed.
AT SELECTION-SCREEN OUTPUT.
  IF rb1     = 'X'.   "KD
    LOOP AT SCREEN.              " IV Date is displayed.
      CHECK screen-group1 = 'M01'.
      screen-active = '0'.
      MODIFY SCREEN.
    ENDLOOP.
  ELSEIF rb2 = 'X'.   "LP
    LOOP AT SCREEN.              " GR Date is displayed.
      CHECK screen-group1 = 'M02'.
      screen-active = '0'.
      MODIFY SCREEN.
    ENDLOOP.
  ENDIF.

AT SELECTION-SCREEN.
  IF p_retro IS INITIAL.
    w_retro_flg = 'X'.
  ELSE.
    CLEAR: w_retro_flg.
  ENDIF.


START-OF-SELECTION.
  IF w_retro_flg = 'X'.
    MESSAGE s999(zmmm) WITH 'Retroactive Price is needed !'.
    EXIT.
  ENDIF.

  PERFORM get_data.    "get data

  IF it_ztmm_6030_01 IS INITIAL.
    MESSAGE s999(zmmm) WITH 'There is no data!'(001).
    EXIT.
  ENDIF.
  PERFORM process_data.   "Process Data
  IF sy-batch IS INITIAL.   "Not Backgroung Processing
    PERFORM dsp_data.       "Display Data
  ENDIF.


*----------------------------------------------------------------------*
* List Procession Events
*----------------------------------------------------------------------*
TOP-OF-PAGE.
*  PERFORM make_col_heading.
