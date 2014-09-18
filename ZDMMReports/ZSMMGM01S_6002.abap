************************************************************************
* Program Name      : ZSMMGM01S_6002
* Author            : Hakchin Kim
* Creation Date     : 2003.08.18.
* Specifications By : Hakchin Kim
* Pattern           : Report 1-1
* Development Request No : SMMGM01 GI Request Slip
* Addl Documentation: F/S - SMMGM01  GI Request Slip V2.0.doc
* Description       : This is used for GI Smart Form Output
*                     Related to Resevation
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
*
************************************************************************
REPORT zsmmgm01s_6002.
INCLUDE zsmmgm01s_6002top.   "Data Declaration Part
INCLUDE zsmmgm01s_6002cla.   "Class Part
INCLUDE zsmmgm01s_6002o01.   "PBO Part
INCLUDE zsmmgm01s_6002i01.   "PAI Part
INCLUDE zsmmgm01s_6002f01.   "Library(Perform Collection)

********** Report Transactin Execution *************
INITIALIZATION.

*F4 for Reservation
AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_rsnum-low.
* For Reservation
  DATA:  rspos LIKE resb-rspos.
  CALL FUNCTION 'MB_SELECT_RESERVATION'
       EXPORTING
            hilfe = 'HLPR'
       IMPORTING
            rsnum = s_rsnum-low
            rspos = rspos.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_rsnum-high.
* For Reservation
  DATA:  rspos LIKE resb-rspos.
  CALL FUNCTION 'MB_SELECT_RESERVATION'
       EXPORTING
            hilfe = 'HLPR'
       IMPORTING
            rsnum = s_rsnum-high
            rspos = rspos.

START-OF-SELECTION.  "event block for creating lists
  PERFORM make_it_zsmm_6002_01.      " Data Selection
  IF it_zsmm_6002_01 IS INITIAL.
    MESSAGE s999(zmmm) WITH 'There is no data!'(001).
    EXIT.
  ENDIF.
  CALL SCREEN 0100.  " Go to Screen 0100
