*&---------------------------------------------------------------------*
*& Report  ZRHR_APP_DOC_CREATE_BDC
*&
*&---------------------------------------------------------------------*
*& Program name      : ZRHR_APP_DOC_CREATE_BDC
*& Creation date     : 01/02/2013
*& Writer            : T00289
*&---------------------------------------------------------------------*
*& Description       :
*& 1. BDC for Appraisal Document Creation
*&---------------------------------------------------------------------*
*& Modified date     :
*& Modified user     :
*&---------------------------------------------------------------------*

REPORT  zrhr_app_doc_create_bdc MESSAGE-ID zmhrpms.

INCLUDE zrhr_app_doc_create_bdctop.
INCLUDE zrhr_app_doc_create_bdco01.
INCLUDE zrhr_app_doc_create_bdci01.
INCLUDE zrhr_app_doc_create_bdcf01.

*----------------------------------------------------------------------*
*   AT SELECTION-SCREEN OUTPUT
*----------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    IF screen-name = 'P_FTYPE'.
      screen-input = 0.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

*----------------------------------------------------------------------*
*   AT SELECTION-SCREEN ON VALUE-REQUEST FOR
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_fname.
  PERFORM get_file_name.

*----------------------------------------------------------------------*
*   AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.
  CASE sscrfields-ucomm.
    WHEN 'FC01'.
      PERFORM layout_header.
      PERFORM get_download_path.
      PERFORM download_layout.
    WHEN OTHERS.
  ENDCASE.

*----------------------------------------------------------------------*
*   START-OF-SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM get_file_data.
  PERFORM process_data.

*----------------------------------------------------------------------*
*   END-OF-SELECTION
*----------------------------------------------------------------------*
END-OF-SELECTION.
  IF gt_record[] IS INITIAL.
**   Success Message
*    MESSAGE s015.
  ELSE.
*   CALL ALV Error list
    CALL SCREEN 100.
  ENDIF.
