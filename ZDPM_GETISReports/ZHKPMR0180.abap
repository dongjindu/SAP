*&------------------------------------------------------------------
*& Program ID     : ZHKPMR0180
*& Profram Name   : PM Maintenance Plan Order List I/F to GETIS
*& Created by     : HS
*& Created on     : 06.11. 2014
*& Development ID :
*& Reference Pgm. :
*& Description    :
*&
*& Modification Log
*&====================================================================
*& Date     Developer      Request ID      Description
*&
*&--------------------------------------------------------------------

REPORT  zhkpmr0180 NO STANDARD PAGE HEADING MESSAGE-ID zdpm_getis .

INCLUDE ZHKPMR0180TOP. "Declare Data
INCLUDE ZHKPMR0180F01. "Process

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

  g_job_start_date = sy-datum.
  GET TIME FIELD g_job_start_time.
  GET TIME STAMP FIELD g_time_stamp .

  PERFORM get_data .
  PERFORM modify_data .

*&-------------------------------------------------------------------*
* # END-OF-SELECTION :
*&-------------------------------------------------------------------*
END-OF-SELECTION.

  g_job_end_date = sy-datum.
  GET TIME FIELD g_job_end_time.

  IF gt_data[] IS INITIAL .
    MESSAGE s000 WITH 'Not found data' .
  ELSE .
    IF sy-batch IS NOT INITIAL .
      PERFORM process_batch .
      PERFORM display_log.
    ELSE .
      DESCRIBE TABLE gt_data LINES sy-ffile .
      MESSAGE s001 WITH sy-ffile 'has been viewed.' .
      PERFORM display_alv_screen.
    ENDIF .
  ENDIF .
