*&---------------------------------------------------------------------*
*& Report  ZRHR_ANNU_PERF_RESULT
*&
*&---------------------------------------------------------------------*
*& Program name      : ZRHR_ANNU_PERF_RESULT
*& Creation date     : 03/15/2013
*& Writer            : T00289
*&---------------------------------------------------------------------*
*& Description       :
*& 1. Annual Performance Result Report
*&---------------------------------------------------------------------*
*& Modified date     :
*& Modified user     :
*&---------------------------------------------------------------------*

report  zrhr_annu_perf_result message-id zmhrpms.

include zrhr_annu_perf_resulttop.
include zrhr_annu_perf_resultf01.
include zrhr_annu_perf_resulto01.
include zrhr_annu_perf_resulti01.


*----------------------------------------------------------------------*
*   INITIALIZATION
*----------------------------------------------------------------------*
initialization.
  perform set_init_value.       " set initial value

*----------------------------------------------------------------------*
*   AT SELECTION-SCREEN OUTPUT
*----------------------------------------------------------------------*
at selection-screen output.
  perform set_droplist_year.    " set droplist year
  perform set_droplist_status.  " set droplist status

*----------------------------------------------------------------------*
*   AT SELECTION-SCREEN ON VALUE-REQUEST FOR
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
*   AT SELECTION-SCREEN
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
*   START-OF-SELECTION
*----------------------------------------------------------------------*
*START-OF-SELECTION.
*  PERFORM get_data.

*----------------------------------------------------------------------*
*   END-OF-SELECTION
*----------------------------------------------------------------------*
end-of-selection.
  call screen 100.
