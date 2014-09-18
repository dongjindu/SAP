************************************************************************
* Program name : ZEMMGM10E_ASSIGN_BLANK
* Created by   : Min-su Park
* Created on   : 2003.11.11.
* Pattern      : Report 1-1
* Description  : Assign BLANK to STEEL
*
* Modification Log
* Date            Developer        Request No.    Description
* 2003.11.11.     Min-su Park      UD1K901873     Initial Coding
*
************************************************************************

*----------------------------------------------------------------------*
*   INCLUDE ZEMMGM10E_ASSIGN_BLANKPBO                                  *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'MENU'.
  SET TITLEBAR 'TITLE'.
ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  SCREEN_ADJUSTMENT  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE screen_adjustment OUTPUT.
  w_loopc = sy-loopc.
*Screen Adjust for when steel is exist.
  LOOP AT SCREEN.
    IF screen-group1 = 'G1' AND NOT it_bom-steel IS INITIAL.
      screen-input = 0.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.
ENDMODULE.                 " SCREEN_ADJUSTMENT  OUTPUT
