*&---------------------------------------------------------------------*
*& Report  ZQMR_COR_SCRAP_MATERIAL
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
*&--------------------------------------------------------------------*
* Program name  : ZQMR_COR_SCRAP_MATERIAL
* Description   : CHANGE OF RESPONSIBILITY SCRAP MATERIAL REPORT
* Developer     : T00281
* Creation date : 07/17/2012
*&--------------------------------------------------------------------*
* Change date |  Name     | Comment
*
*&--------------------------------------------------------------------*

REPORT  zqmr_cor_scrap_material MESSAGE-ID zmqm.

INCLUDE zqmr_cor_scrap_material_top.

INCLUDE zqmr_cor_scrap_material_f01.

INCLUDE zqmr_cor_scrap_material_o01.

INCLUDE zqmr_cor_scrap_material_i01.


*=====================================================================*
* INITIALIZATION
*=====================================================================*
INITIALIZATION.
  PERFORM set_initial.

*=====================================================================*
* AT SELECTION-SCREEN
*=====================================================================*
AT SELECTION-SCREEN.

*=====================================================================*
* AT SELECTION-SCREEN OUTPUT
*=====================================================================*
AT SELECTION-SCREEN OUTPUT.

*=====================================================================*
* START-OF-SELECTION
*=====================================================================*
START-OF-SELECTION.
  PERFORM get_data.
*=====================================================================*
* END-OF-SELECTION
*=====================================================================*
END-OF-SELECTION.

  IF lines( gt_record ) > 0.
    CALL SCREEN 100.
  ELSE.
    MESSAGE s000 WITH 'There is no data!' DISPLAY LIKE 'E'.
  ENDIF.
