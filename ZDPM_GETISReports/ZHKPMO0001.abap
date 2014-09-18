*&------------------------------------------------------------------
*& Program ID     : ZHKPMO0001
*& Profram Name   : PM Material PR requirement List and Create
*&                  Purchase Requisition
*& Created by     : HS
*& Created on     : 06.23.2014
*& Development ID :
*& Reference Pgm. :
*& Description    :
*&
*& Modification Log
*&====================================================================
*& Date     Developer      Request ID      Description
*&
*&--------------------------------------------------------------------

REPORT  zhkpmo0001 NO STANDARD PAGE HEADING MESSAGE-ID zdpm_getis .

INCLUDE zhkpmo0001top . "Declare Data
INCLUDE zhkpmo0001f01 . "Process
INCLUDE zhkpmo0001o01 . "PBO
INCLUDE zhkpmo0001i01 . "PAI

*&---------------------------------------------------------------------
* # INITIALIZATION:
*&---------------------------------------------------------------------*
INITIALIZATION.
*  PERFORM initialization .

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
