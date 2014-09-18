*&---------------------------------------------------------------------*
*&  Include           ZRDA_QA_DOC_ARCH_O01
*&---------------------------------------------------------------------*


*&---------------------------------------------------------------------*
*&      Module  STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0200 OUTPUT.
  SET PF-STATUS 'ST200'.
  SET TITLEBAR 'ST200'.
ENDMODULE.                 " STATUS_0200  OUTPUT



*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0200 INPUT.
  CASE ok_code.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0200  INPUT



*&---------------------------------------------------------------------*
*&      Module  STATUS_0210  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0210 OUTPUT.

  SET PF-STATUS '0210'.
***  SET TITLEBAR  '210'.
  CASE g_proc.
    WHEN 'B'.
      SET TITLEBAR  '210'.
    WHEN 'L'.
      SET TITLEBAR  '220'.
    WHEN 'C'.
      SET TITLEBAR  '230'.
    WHEN 'R'.
      SET TITLEBAR  '240'.
    WHEN 'V'.
      SET TITLEBAR  '250'.
  ENDCASE.

ENDMODULE.                 " STATUS_0210  OUTPUT



*&---------------------------------------------------------------------*
*&      Module  INIT_0210  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE init_0210 OUTPUT.

  IF is_buyback-aclonly IS NOT INITIAL.
    LOOP AT SCREEN.
      IF screen-group4 = 'GR1'.
        screen-active = 0.
      ENDIF.
      MODIFY SCREEN.
    ENDLOOP.
  ENDIF.

  IF g_flg = 'R'.
    is_buyback-title  = g_title_o.
    LOOP AT SCREEN.
      IF screen-group4 = 'GR2'.
        screen-input = 0.
      ENDIF.
      MODIFY SCREEN.
    ENDLOOP.
  ENDIF.

  IF g_fldname IS NOT INITIAL.
    SET CURSOR FIELD g_fldname.
  ENDIF.

ENDMODULE.                 " STATUS_0210  OUTPUT



*&---------------------------------------------------------------------*
*&      Module  INIT_0220  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE init_0220 OUTPUT.

  IF is_buyback-aclonly IS NOT INITIAL.
    LOOP AT SCREEN.
      IF screen-group4 = 'GR1'.
        screen-active = 0.
      ENDIF.
      MODIFY SCREEN.
    ENDLOOP.
  ENDIF.

  IF g_flg = 'R'.
    is_buyback-title  = g_title_o.
    LOOP AT SCREEN.
      IF screen-group4 = 'GR2'.
        screen-input = 0.
      ENDIF.
      MODIFY SCREEN.
    ENDLOOP.
  ENDIF.

  IF g_fldname IS NOT INITIAL.
    SET CURSOR FIELD g_fldname.
  ENDIF.

ENDMODULE.                 " INIT_0220  OUTPUT



*&---------------------------------------------------------------------*
*&      Module  STATUS_0230  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0230 OUTPUT.

  SET PF-STATUS '0210'.
  CASE g_proc.
    WHEN 'B'.
      SET TITLEBAR  c_buyback.
    WHEN 'L'.
      SET TITLEBAR  c_legal.
    WHEN 'C'.
      SET TITLEBAR  '230'.
    WHEN 'R'.
      SET TITLEBAR  '240'.

  ENDCASE.

ENDMODULE.                 " STATUS_0230  OUTPUT



*&---------------------------------------------------------------------*
*&      Module  INIT_0230  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE init_0230 OUTPUT.

  IF g_flg = 'R'.
    is_legal-title  = g_title_o.
    LOOP AT SCREEN.
      IF screen-group4 = 'GR2'.
        screen-input = 0.
      ENDIF.
      MODIFY SCREEN.
    ENDLOOP.
  ENDIF.

  IF g_fldname IS NOT INITIAL.
    SET CURSOR FIELD g_fldname.
  ENDIF.

ENDMODULE.                 " INIT_0230  OUTPUT



*&---------------------------------------------------------------------*
*&      Module  INIT_0240  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE init_0240 OUTPUT.

  LOOP AT SCREEN.
    IF screen-group4 = 'GR1'.
      screen-input = 0.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.

ENDMODULE.                 " INIT_0240  OUTPUT
