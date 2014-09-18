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
* 02/09/2005      Shiva            UD1K914270     Re-write the program
*                                         to send LTP data for 6 months.
* 02/22/2005      Shiva            UD1K914581     Fix the date format.
* 02/24/2005      Shiva            UD1K914637    Calculation of
*                                                requirement quantity.
************************************************************************
REPORT zimmgm15i_6013
                 MESSAGE-ID zmmm
                 NO STANDARD PAGE HEADING
                 LINE-SIZE 400.

INCLUDE ZIMMGM15I_6013TOP.
INCLUDE ZIMMGM15I_6013F01.
*----------------------------------------------------------------------*
* Report Transactin Execution
*----------------------------------------------------------------------*
INITIALIZATION.

START-OF-SELECTION.
  PERFORM get_data.    "get data

  IF it_mat_ltp[] IS INITIAL.
    MESSAGE s999(zmmm) WITH 'There is no data!'(001).
*    EXIT.
  ENDIF.
  PERFORM process_data. "Process Data
  perform display_data.
*----------------------------------------------------------------------*
* List Procession Events
*----------------------------------------------------------------------*
*TOP-OF-PAGE.
**  PERFORM make_col_heading.
