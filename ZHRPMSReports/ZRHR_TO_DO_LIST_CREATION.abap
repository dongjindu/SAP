*&---------------------------------------------------------------------*
*& Report  ZRHR_TO_DO_LIST_CREATION
*&
*&---------------------------------------------------------------------*
*& Program name      : ZRHR_TO_DO_LIST_CREATION
*& Creation date     : 03/11/2013
*& Writer            : T00289
*&---------------------------------------------------------------------*
*& Description       :
*& 1. To-Do List Creation
*&---------------------------------------------------------------------*
*& Modified date     :
*& Modified user     :
*&---------------------------------------------------------------------*

REPORT  zrhr_to_do_list_creation MESSAGE-ID zmhrpms.


INCLUDE zrht_to_do_list_creationtop.
INCLUDE zrht_to_do_list_creationc01.
INCLUDE zrht_to_do_list_creationo01.
INCLUDE zrht_to_do_list_creationi01.
INCLUDE zrhr_to_do_list_creationf01.


*----------------------------------------------------------------------*
*   AT SELECTION-SCREEN OUTPUT
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
*   AT SELECTION-SCREEN ON VALUE-REQUEST FOR
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
*   AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.
  CASE sscrfields-ucomm.
    WHEN 'FC01'.
      PERFORM get_saved_data.
      IF gt_result[] IS NOT INITIAL.
        CALL SCREEN 100.
      ENDIF.
  ENDCASE.

*----------------------------------------------------------------------*
*   START-OF-SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM get_data.

*----------------------------------------------------------------------*
*   END-OF-SELECTION
*----------------------------------------------------------------------*
END-OF-SELECTION.
  IF gt_result[] IS NOT INITIAL.
    CALL SCREEN 100.
  ELSE.
    MESSAGE s027 DISPLAY LIKE 'E'.
  ENDIF.
