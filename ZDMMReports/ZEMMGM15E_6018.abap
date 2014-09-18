************************************************************************
* Program Name      : ZEMMGM15E_6018
* Author            : Hakchin Kim
* Creation Date     : 2003.12.11.
* Specifications By : Hakchin Kim
* Pattern           : Report 1-1
* Development Request No : EMMGM15
* Addl Documentation: F/S - EMMGM15 PR Conversion
* Description       : This is for PR Creation
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
************************************************************************
REPORT  zemmgm15e_6018
                 MESSAGE-ID zmmm
                 NO STANDARD PAGE HEADING
                 LINE-SIZE 400.

INCLUDE zemmgm15e_6018top.   "Data Declaration
INCLUDE zemmgm15e_6018f01.   "Perform Library.

*--------- Report Transactin Execution --------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_dispo.
*F4 for MRP Controller
  PERFORM get_it_f4_dispo.
  PERFORM f4_sov_dispo         "MRP Controller
                      TABLES it_f4_dispo
                             it_ddshretval
                      USING  'DISPO'   "Itab FIELD
                             SY-REPID
                             sy-dynnr
                            'P_DISPO'  "Screen field
                             0.        "Step loop line

AT SELECTION-SCREEN.
  IF NOT ( p_dispo = 'P01' OR p_dispo = 'P02' ).
    MESSAGE e999(zmmm) WITH 'Input Error with MRP Controller'(002).
  ENDIF.

START-OF-SELECTION.
  PERFORM get_data.       "get data with filling it_plaf
*
  IF it_plaf IS INITIAL.
    MESSAGE s999(zmmm) WITH 'There is no data!'(001).
    EXIT.
  ENDIF.

  PERFORM process_data.   "Process data

  IF NOT sy-batch IS INITIAL.
    PERFORM display_log.    "Display Data Log
  ENDIF.
**---------- List Procession Events ------------------------------------
*
*TOP-OF-PAGE.
**  PERFORM make_col_heading.
