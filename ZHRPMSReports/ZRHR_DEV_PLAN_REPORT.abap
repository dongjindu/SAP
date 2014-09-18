*&---------------------------------------------------------------------*
*& Report  ZRHR_DEV_PLAN_REPORT
*&
*&---------------------------------------------------------------------*
*& Program name      : ZRHR_DEV_PLAN_REPORT
*& Creation date     : 12/05/2013
*& Writer            : T00320
*&---------------------------------------------------------------------*
*& Description       :
*& 1. Develpment Plan Report
*&---------------------------------------------------------------------*
*& Modified date     :
*& Modified user     :
*&---------------------------------------------------------------------*
REPORT  ZRHR_DEV_PLAN_REPORT MESSAGE-ID ZMHRPMS.

INCLUDE ZRHR_DEV_PLAN_REPORTTOP.
INCLUDE ZRHR_DEV_PLAN_REPORTF01.
INCLUDE ZRHR_DEV_PLAN_REPORTO01.
INCLUDE ZRHR_DEV_PLAN_REPORTI01.

*----------------------------------------------------------------------*
*   INITIALIZATION
*----------------------------------------------------------------------*
INITIALIZATION.
  PERFORM SET_INIT_VALUE.       " set initial value

*----------------------------------------------------------------------*
*   AT SELECTION-SCREEN OUTPUT
*----------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
  PERFORM SET_DROPLIST_YEAR.    " set droplist Year

*----------------------------------------------------------------------*
*   END-OF-SELECTION
*----------------------------------------------------------------------*
END-OF-SELECTION.
  CALL SCREEN 100.
