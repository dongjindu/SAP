*&---------------------------------------------------------------------*
*&  Include           ZHKPMR0001I01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.


  CASE sy-ucomm.
    WHEN 'BACK' OR 'EXIT' OR  'CANC' OR 'CANCEL'.

      PERFORM get_data .
      PERFORM modify_data .

      LEAVE TO SCREEN 0.

    WHEN 'ZSAVE' . "Save
      PERFORM process_save .

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
    WHEN 'BACK' OR 'EXIT' OR  'CANC' OR 'ZCANC'.

      CLEAR g_enter .
      LEAVE TO SCREEN 0.

    WHEN 'ZENTER' . "

      PERFORM check_data_0200 .

      IF g_err IS INITIAL .
        LEAVE TO SCREEN 0.
      ENDIF .

  ENDCASE .

ENDMODULE.                 " USER_COMMAND_0200  INPUT
