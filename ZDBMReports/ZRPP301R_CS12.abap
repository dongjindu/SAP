************************************************************************
* Program Name      : ZRPP301R_CS12
* Author            : Bongsoo, Kim
* Creation Date     : 2003.10.31.
* Specifications By : Bongsoo, Kim
* Development Request No : UD1K903583
* Addl Documentation:
* Description       : BOM list with color
*
* Modification Logs
* Date       Developer    RequestNo    Description
*2004.01.30  ZDBM         UD1K906702   BOM list with color
*2004.08.19  ZDBM         UD1K911941   ZRPP301R_CS12 CHANGING
*                                      08/19/2004 10:55:09
*
************************************************************************
REPORT ZRPP301R_CS12
                NO STANDARD PAGE HEADING
                LINE-SIZE  1023
                LINE-COUNT 65
                MESSAGE-ID ZMBM.
*----------------------------------------------------------------------*
* INCLUDE
*----------------------------------------------------------------------*
INCLUDE ZRPP301L_CS12_T.
INCLUDE ZRPP301L_CS12_F01.
*----------------------------------------------------------------------*
* EVENT
*----------------------------------------------------------------------*
INITIALIZATION.
*  PERFORM INITIALIZATION.
*
AT SELECTION-SCREEN OUTPUT.
  PERFORM SCREEN_MODIFY.
*-----> AT SELECTION-SCREEN ON VALUE-REQUEST
*AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_MATNR.
*  PERFORM HELP_REQUEST.

*-----> AT SELECTION-SCREEN ON VALUE-REQUEST
AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_STLAN.
  PERFORM HELP_REQUEST_STLAN.

*-----> AT SELECTION-SCREEN ON VALUE-REQUEST
AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_STLAL.
  PERFORM HELP_REQUEST_STLAL.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_ATWRE.
  PERFORM HELP_REQUEST_NAME.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_ATWRI.
  PERFORM HELP_REQUEST_NAME1.

AT SELECTION-SCREEN.
  PERFORM AT_SELECTION_SCREEN.

START-OF-SELECTION.
  IF P_DATUV IS INITIAL.
    MESSAGE S000 WITH 'E: Enter valid from date'.
  ELSE.
    PERFORM READ_PROCESS CHANGING WA_CHK.
    IF WA_CHK EQ ' '.
      PERFORM DATA_PROCESS.

      PERFORM WRITE_PROCESS.
      CLEAR: P_ATWRE, P_ATWRI.
    ELSE.
      MESSAGE S000 WITH 'NO BOM'.
    ENDIF.
  ENDIF.

END-OF-SELECTION.
  PERFORM INITIALIZATION.
