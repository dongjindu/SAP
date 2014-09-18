*&---------------------------------------------------------------------*
*&  Include           ZRHR_DISTRIBUTION_REPORTI01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit INPUT.

  CASE ok_code.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN 'CANC'.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.                 " EXIT  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  CASE ok_code.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.

    WHEN 'EXEC'.
      IF p_type1 EQ 'X'.
        PERFORM get_dis_pool. " get Distribution Pool Report

      ELSEIF p_type2 EQ 'X'.
        PERFORM get_app_dis.  " get Approver Distribution Report

      ELSEIF p_type3 EQ 'X'.
        PERFORM get_dep_dis.  " get Department Distribution Report

      ENDIF.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0100  INPUT
