*&---------------------------------------------------------------------*
*&  Include           ZHKPMR0005I01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  CASE sy-ucomm.
    WHEN 'BACK' OR 'EXIT' OR  'CANC' OR 'CANCEL'.

      LEAVE TO SCREEN 0.

    WHEN 'ZSAVE' . "Save
      PERFORM process_zsave .

    WHEN 'ZOPE' . "Add Operation
      PERFORM process_zope .

    WHEN 'ZADD' . "Conponent Alssign
      PERFORM process_zadd .

    WHEN OTHERS .
      PERFORM get_others.

  ENDCASE .

ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0200 INPUT.

  CASE sy-ucomm.
    WHEN 'BACK' OR 'EXIT' OR  'CANC' OR 'CANCEL'.

      PERFORM get_data .
      PERFORM modify_data .

      LEAVE TO SCREEN 0.

    WHEN 'ZOPSAVE' . "Save
      PERFORM process_zopsave .


    WHEN OTHERS .
*      PERFORM get_others_0200 .

  ENDCASE .

ENDMODULE.                 " USER_COMMAND_0200  INPUT
