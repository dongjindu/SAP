*&---------------------------------------------------------------------*
*&  Include           ZHKPMO0002I01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  MODIFY_TC_0001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE modify_tc_0001 INPUT.

  MODIFY gt_item FROM gt_item INDEX gt_cont-current_line.

ENDMODULE.                 " MODIFY_TC_0001  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0001 INPUT.

  CASE sy-ucomm .
    WHEN 'ZSAVE' .
      PERFORM save_data .
      LEAVE TO SCREEN 0 .
    WHEN 'ZCANC' OR 'BACK' OR 'EXIT' OR 'CANC' .
       LEAVE TO SCREEN 0 .
  ENDCASE .

ENDMODULE.                 " USER_COMMAND_0001  INPUT
