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
* 03/12/2012 Valerian   UD1K954236 Adjust logic to park document
*                                  instead of posting document
*----------------------------------------------------------------------
REPORT zacou153 MESSAGE-ID zmco.

INCLUDE zacoui00.
INCLUDE ZACOU153_TOP.
INCLUDE ZACOU153_F01.
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
