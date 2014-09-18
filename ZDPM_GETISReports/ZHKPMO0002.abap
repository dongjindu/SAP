*&------------------------------------------------------------------
*& Program ID     : ZHKPMO0002
*& Profram Name   : PM Enter Equipment Retooling History
*& Created by     : HS
*& Created on     : 07.15.2014
*& Development ID :
*& Reference Pgm. :
*& Description    :
*&
*& Modification Log
*&====================================================================
*& Date     Developer      Request ID      Description
*&
*&--------------------------------------------------------------------

REPORT  zhkpmo0002 NO STANDARD PAGE HEADING MESSAGE-ID zdpm_getis .

INCLUDE zhkpmo0002top . "Declare Data
INCLUDE zhkpmo0002f01 . "Process
INCLUDE zhkpmo0002o01 . "PBO
INCLUDE zhkpmo0002i01 . "PAI

*&---------------------------------------------------------------------
* # INITIALIZATION:
*&---------------------------------------------------------------------*
INITIALIZATION.
  PERFORM initialization .

*&----------------------------------------------------------------------
* # AT SELECTION-SCREEN OUTPUT
*&----------------------------------------------------------------------
AT SELECTION-SCREEN OUTPUT.
*  PERFORM MODIFY_SCREEN.

*&----------------------------------------------------------------------
* # AT SELECTION-SCREEN :
*&----------------------------------------------------------------------
AT SELECTION-SCREEN.
*  PERFORM SELECTION_SCREEN.

*&---------------------------------------------------------------------*
* # START-OF-SELECTION:
*&---------------------------------------------------------------------*
START-OF-SELECTION .

  PERFORM get_data .
  PERFORM modify_data .

*&-------------------------------------------------------------------*
* # END-OF-SELECTION :
*&-------------------------------------------------------------------*
END-OF-SELECTION.

  IF gt_data[] IS INITIAL .
    MESSAGE s000 WITH 'Not found data' .
  ELSE .
    DESCRIBE TABLE gt_data LINES sy-ffile .
    MESSAGE s001 WITH sy-ffile 'has been viewed.' .

    PERFORM display_alv_screen.
  ENDIF .
