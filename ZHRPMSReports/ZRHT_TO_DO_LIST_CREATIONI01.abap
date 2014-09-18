*&---------------------------------------------------------------------*
*&  Include           ZRHT_TO_DO_LIST_CREATIONI01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit INPUT.

  CASE ok_code.
    WHEN 'CANC'.
      CLEAR: g_change_flag.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
      CLEAR: g_change_flag.
      LEAVE PROGRAM.
  ENDCASE.

ENDMODULE.                 " EXIT  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  CASE ok_code.
    WHEN 'BACK'.      " Back
      CLEAR: g_change_flag.
      LEAVE TO SCREEN 0.

    WHEN 'CHANGE'.    " Begin change mode
      g_change_flag = 'X'.
      SET PF-STATUS 'G101'.

    WHEN 'ECHANGE'.   " End change mode
      CLEAR g_change_flag.

    WHEN 'APPEND'.    " Apend row
      APPEND INITIAL LINE TO gt_result[].

    WHEN 'DELETE'.    " Delete rows
      PERFORM delete_rows.

    WHEN 'SAVE'.      " Save rows
      PERFORM save.

    WHEN 'CREATE'.
      PERFORM create. " Create appraisal document

  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0100  INPUT
