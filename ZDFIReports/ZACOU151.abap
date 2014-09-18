*----------------------------------------------------------------------
* Program ID        : ZACOU151
* Title             : [FI] Direct Duty ME Posting
* Created on        : 02/07/2012
* Created by        : J.C.MOON
* Specifications By :
* Description       :
*----------------------------------------------------------------------
* Modification Logs
* Date       Developer  Request    Description
*
*----------------------------------------------------------------------
REPORT zacou151 MESSAGE-ID zmco.

INCLUDE zacoui00.
INCLUDE ZACOU151_TOP.
INCLUDE ZACOU151_F01.
*----------------------------------------------------------------------*
* Start of selection
*----------------------------------------------------------------------*
START-OF-SELECTION.

  PERFORM screen_select_check.
  IF w_check <> ''.
    EXIT.
  ENDIF.

* Gather row data
  PERFORM get_data.
  IF it_data[] IS INITIAL.
    MESSAGE i000 WITH 'Not select data'.
  ELSE.
    CALL SCREEN 100.
  ENDIF.

END-OF-SELECTION.
* nothing
