*&------------------------------------------------------------------
*& Program ID     : ZHKPMO0004
*& Profram Name   : Maintain Material Master
*& Created by     : HS
*& Created on     : 06.20.2014
*& Development ID :
*& Reference Pgm. :
*& Description    :
*&
*& Modification Log
*&====================================================================
*& Date     Developer      Request ID      Description
*&
*&--------------------------------------------------------------------

REPORT  zhkpmo0004 NO STANDARD PAGE HEADING MESSAGE-ID zdpm_getis .

INCLUDE zhkpmo0004top. "Declare Data
INCLUDE zhkpmo0004f01. "Process
INCLUDE zhkpmo0004o01. "PBO
INCLUDE zhkpmo0004i01. "PAI

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
  PERFORM selection_screen.

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

    CALL SCREEN 0100 .
  ENDIF .
