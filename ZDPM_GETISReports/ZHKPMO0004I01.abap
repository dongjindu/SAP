*&---------------------------------------------------------------------*
*&  Include           ZHKPMR0004I01
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

    WHEN 'COMM_A' . "Change Material type
      PERFORM process_comm_a .

    WHEN 'COMM_B' . "Delete material
      PERFORM process_comm_b .

    WHEN 'COMM_C' . "Change Material
      PERFORM process_comm_c .

  ENDCASE .

ENDMODULE.                 " USER_COMMAND_0100  INPUT
