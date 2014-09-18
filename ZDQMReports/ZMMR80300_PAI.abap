*&---------------------------------------------------------------------*
*&  Include           ZKEMMR04011_PAI
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  save_okcode  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE save_okcode INPUT.

  s_ok_code = ok_code.
  CLEAR ok_code.

ENDMODULE.                 " save_okcode  INPUT


*&---------------------------------------------------------------------*
*&      Module  user_command_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  IF NOT g_grid IS INITIAL.
    CALL METHOD g_grid->check_changed_data .
  ENDIF.

  CASE sy-ucomm.
    WHEN 'DISP'.
      PERFORM p0000_indicator.
      PERFORM p1000_initial_data.
      PERFORM p1000_select_data.
      PERFORM p2000_process_data.
      PERFORM refresh_table_display.
  ENDCASE.

ENDMODULE.                 " user_command_0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  exit_command  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit_command INPUT.

  CASE sy-ucomm.
    WHEN 'CANC'.
      LEAVE TO SCREEN 0.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
  ENDCASE.

ENDMODULE.                 " exit_command  INPUT
*&---------------------------------------------------------------------*
*&      Module  exit_command  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit_command2 INPUT.

  CASE sy-ucomm.
    WHEN 'BACK' OR 'CANC'.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
  ENDCASE.

ENDMODULE.                 " exit_command2  INPUT


*&---------------------------------------------------------------------*
*&      Module  check_chain  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_chain INPUT.

  DATA : l_pspnr LIKE prps-pspnr,
        l_ebeln LIKE eban-ebeln,
        l_kunnr LIKE kna1-kunnr.


ENDMODULE.                 " check_chain  INPUT

*&---------------------------------------------------------------------*
*&      Module  help_KOSTL_code  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE help_kostl_code INPUT.

  PERFORM pov_clear.


  DATA : gt_kostl LIKE cskt OCCURS 0 WITH HEADER LINE.

  CLEAR : gt_kostl, gt_kostl[].
  SELECT kostl
  verak AS kostln
  INTO CORRESPONDING FIELDS OF TABLE gt_kostl
  FROM csks
  WHERE kokrs = 'T100'.

  LOOP AT gt_kostl.
    gt_valuetab-value = gt_kostl-kostl.
    APPEND gt_valuetab.
    CLEAR gt_valuetab.

    gt_valuetab-value = gt_kostl-ktext.
    APPEND gt_valuetab.
    CLEAR gt_valuetab.
  ENDLOOP.

  PERFORM add_fields USING : 'CSKS' 'KOSTL'  'X',
        'CSKS' 'VERAK'  ' '.

  PERFORM help_values_get.

  IF g_lno > 0.
    READ TABLE gt_kostl INDEX g_lno.
    IF sy-subrc = 0.
      PERFORM value_update USING: ' ' 'gt_itab-KOSTL' gt_kostl-kostl 0,
            'X' 'gt_itab-KTEXT'  gt_kostl-ktext 0.

    ENDIF.
    CLEAR : gt_kostl, gt_kostl[].
  ENDIF.

ENDMODULE.                 " help_KOSTL_code  INPUT

*&---------------------------------------------------------------------*
*&      Module  get_cursor_field  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE get_cursor_field INPUT.

  GET CURSOR FIELD g_field.
  IF sy-subrc = 0                           .
    MOVE: g_field TO gs_cursor-field ,
    '0200'  TO gs_cursor-screen.
    GET CURSOR LINE gs_cursor-lines         .
  ENDIF                                     .

ENDMODULE.                 " get_cursor_field  INPUT
*&---------------------------------------------------------------------*
*&      Module  check_chain_PSPNR  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_chain_pspnr INPUT.



ENDMODULE.                 " check_chain_PSPNR  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_CHAIN_KOSTL  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_chain_kostl INPUT.

ENDMODULE.                 " CHECK_CHAIN_KOSTL  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0050  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0050 INPUT.

  CASE sy-ucomm.
  WHEN 'OKAY'.
    g_rc = 'X'.
    LEAVE TO SCREEN 0.
  ENDCASE.


ENDMODULE.                 " USER_COMMAND_0050  INPUT
