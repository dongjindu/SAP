*&---------------------------------------------------------------------*
*& Report  ZRHR_STLT_GOAL_REPORT
*&
*&---------------------------------------------------------------------*
*& Program name      : ZRHR_STLT_GOAL_REPORT
*& Creation date     : 12/09/2013
*& Writer            : T00320
*&---------------------------------------------------------------------*
*& Description       :
*& 1. Development Plan Report
*&---------------------------------------------------------------------*
*& Modified date     :
*& Modified user     :
*&---------------------------------------------------------------------*
REPORT  zrhr_rating_report MESSAGE-ID zmhrpms.

INCLUDE ZRHR_RATING_reporttop.
INCLUDE ZRHR_RATING_reportf01.
INCLUDE ZRHR_RATING_reporto01.
INCLUDE ZRHR_RATING_reporti01.


*----------------------------------------------------------------------*
*   INITIALIZATION
*----------------------------------------------------------------------*
INITIALIZATION.
  PERFORM set_init_value.       " set initial value

*----------------------------------------------------------------------*
*   AT SELECTION-SCREEN OUTPUT
*----------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
  PERFORM set_droplist_year.    " set droplist Year
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
END-OF-SELECTION.
  CALL SCREEN 100.
