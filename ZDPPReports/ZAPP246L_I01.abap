....
**&---------------------------------------------------------------------
*
**&      Module  modify_data_APP246  INPUT
**&---------------------------------------------------------------------
*
**       text
**----------------------------------------------------------------------
*
*MODULE modify_data_APP246 INPUT.
*  MODIFY IT_APP246 INDEX TC_APP246-current_line.
*
*ENDMODULE.                 " modify_data_APP246  INPUT
**&---------------------------------------------------------------------
*
**&      Module  user_command_APP246  INPUT
**&---------------------------------------------------------------------
*
**       text
**----------------------------------------------------------------------
*
MODULE user_command_app246 INPUT.

ENDMODULE.                 " user_command_APP246  INPUT
*&---------------------------------------------------------------------*
*&      Module  exit  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit INPUT.
  LEAVE PROGRAM.
ENDMODULE.                 " exit  INPUT
*&---------------------------------------------------------------------*
*&      Module  read_data_app246  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE read_data_app246 INPUT.
  IF NOT ( gs_custom_container IS INITIAL ).
    CALL  METHOD gs_custom_container->free.
    FREE  gs_custom_container.
  ENDIF.
  PERFORM check_and_read_data_app246 .
ENDMODULE.                 " read_data_app246  INPUT
*&---------------------------------------------------------------------*
*&      Module  get_cursor_field_app246  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE get_cursor_field_app246 INPUT.
  CLEAR: wa_fname_tx, wa_saveline_ix.
  GET CURSOR FIELD wa_fname_tx LINE wa_saveline_ix.
ENDMODULE.                 " get_cursor_field_app246  INPUT
