*&---------------------------------------------------------------------*
*& Report  ZQMR_SHOP_TO_SHOP_SCRAP
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
*&--------------------------------------------------------------------*
* Program name  : ZQMR_SHOP_TO_SHOP_SCRAP
* Description   : SHOP TO SHOP SCRAP REPORT
* Developer     : T00281
* Creation date : 07/12/2012
*&--------------------------------------------------------------------*
* Change date |  Name     | Comment
*
*&--------------------------------------------------------------------*

REPORT  zqmr_shop_to_shop_scrap MESSAGE-ID zmqm.

INCLUDE zqmr_shop_to_shop_scrap_top.

INCLUDE zqmr_shop_to_shop_scrap_f01.

INCLUDE zqmr_shop_to_shop_scrap_o01.

INCLUDE zqmr_shop_to_shop_scrap_i01.


*=====================================================================*
* INITIALIZATION
*=====================================================================*
INITIALIZATION.
  PERFORM set_initial.

*=====================================================================*
* AT SELECTION-SCREEN
*=====================================================================*
AT SELECTION-SCREEN.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_qmgrp-low.
  PERFORM pov_qmgrp.
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
