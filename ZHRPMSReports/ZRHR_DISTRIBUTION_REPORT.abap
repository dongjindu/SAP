*&---------------------------------------------------------------------*
*& Report  ZRHR_DISTRIBUTION_REPORT
*&
*&---------------------------------------------------------------------*
*& Program name      : ZRHR_DISTRIBUTION_REPORT
*& Creation date     : 03/14/2013
*& Writer            : T00289
*&---------------------------------------------------------------------*
*& Description       :
*& 1. Appraisal Distribution Report
*&---------------------------------------------------------------------*
*& Modified date     :
*& Modified user     :
*&---------------------------------------------------------------------*

REPORT  zrhr_distribution_report MESSAGE-ID zmhrpms.

INCLUDE zrhr_distribution_reporttop.
INCLUDE zrhr_distribution_reporto01.
INCLUDE zrhr_distribution_reporti01.
INCLUDE zrhr_distribution_reportf01.


*----------------------------------------------------------------------*
*   INITIALIZATION
*----------------------------------------------------------------------*
INITIALIZATION.
  PERFORM set_init_value.       " set initial value

*----------------------------------------------------------------------*
*   AT SELECTION-SCREEN OUTPUT
*----------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
  PERFORM set_droplist_year.    " set droplist year
  PERFORM set_screen.           " set screen

*----------------------------------------------------------------------*
*   AT SELECTION-SCREEN ON VALUE-REQUEST FOR
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_disp.
  PERFORM f4_search_help_disp.  " set f4 search help of Distributor

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_adis.
  PERFORM f4_search_help_adis.  " set f4 search help of Approver

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_ddis.
  PERFORM f4_search_help_ddis.  " set f4 search help of Head of Department

*----------------------------------------------------------------------*
*   AT SELECTION-SCREEN
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
*   START-OF-SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.
  CALL SCREEN 100.

*----------------------------------------------------------------------*
*   END-OF-SELECTION
*----------------------------------------------------------------------*
