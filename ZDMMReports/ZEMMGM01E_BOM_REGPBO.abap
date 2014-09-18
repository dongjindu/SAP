************************************************************************
* Program name : ZEMMGM01E_BOM_REG                                     *
* Created by   : Min-su Park                                           *
* Created on   : 2003.11.10.                                           *
* Pattern      :                                                       *
* Description  : BOM Registration Request Program                      *
* Modification Log                                                     *
* Date            Developer        Request No.    Description          *
* 2003.11.10.     Min-su Park      UD1K901873     Initial Coding       *
*                                                                      *
************************************************************************
*----------------------------------------------------------------------*
***INCLUDE ZEMMGM01E_BOM_REGPBO .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  CASE w_sel.
    WHEN r1.
      SET PF-STATUS 'MENU' EXCLUDING 'DISP'.
      SET TITLEBAR 'T1'.
    WHEN r2.
      SET TITLEBAR 'T2'.
      SET PF-STATUS 'MENU' EXCLUDING 'SAVE'.
    WHEN r3.
      SET PF-STATUS 'MENU'.
      SET TITLEBAR 'T3'.
  ENDCASE.
ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  SCREEN_ADJUST  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE screen_adjust OUTPUT.
  w_loopc = sy-loopc.
  IF w_sel <> r3.
    LOOP AT SCREEN.
      IF screen-group1 = 'G1'.
        screen-input = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.
  IF w_sel = r2 OR w_sel = r3.
    LOOP AT SCREEN.
      IF screen-group1 = 'G2'.
        screen-input = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.
ENDMODULE.                 " SCREEN_ADJUST  OUTPUT
