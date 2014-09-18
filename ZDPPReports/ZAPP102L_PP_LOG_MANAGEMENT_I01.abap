*----------------------------------------------------------------------*
*   INCLUDE ZAPP102L_PP_LOG_MANAGEMENT_I01                             *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit INPUT.
  CASE ok_code.
    WHEN 'BACK' OR 'EXIT' OR 'CANC'.
      IF sy-dynnr EQ '9100'.
        CALL  METHOD gs_custom_container->free.
        FREE  gs_custom_container.
      ENDIF.
      LEAVE TO SCREEN 0.
    WHEN OTHERS.
  ENDCASE.
ENDMODULE.                 " EXIT  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_CURSOR_FIELD  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE get_cursor_field INPUT.
  IF sy-dynnr EQ '9000'.
    CLEAR: wa_txt9000, wa_line9000.
    GET CURSOR FIELD wa_txt9000 LINE wa_line9000.
  ELSEIF sy-dynnr EQ '9100'.
    CLEAR: wa_txt9100, wa_line9100.
    GET CURSOR FIELD wa_txt9100 LINE wa_line9100.
  ENDIF.
ENDMODULE.                 " GET_CURSOR_FIELD  INPUT
*&---------------------------------------------------------------------*
*&      Module  MODIFY_DATA  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE modify_data INPUT.
  MOVE : chk                      TO   it_screen-chk     ,
         ztpp_if_status-tabname   TO   it_screen-tabname ,
         wa_error                 TO   it_screen-error   ,
         wa_success               TO   it_screen-success ,
         wa_total                 TO   it_screen-total   .
  MODIFY it_screen  INDEX  tc_9000-current_line .
ENDMODULE.                 " MODIFY_DATA  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9000 INPUT.
  save_ok_code = ok_code.
  CLEAR ok_code.
  CASE save_ok_code.
    WHEN 'ZGO'.   "Select
      CLEAR : wa_line , wa_screen.
      wa_line = wa_line9000 + tc_9000-top_line - 1 .
      IF wa_line LE 0.
      ELSE.
        READ TABLE it_screen INTO wa_screen INDEX wa_line .
        CALL SCREEN 9100 .
      ENDIF.
    WHEN 'INST' .  " Table Register..
      CLEAR: ZTPP_IF_STATUS-TABNAME, ZTPP_IF_STATUS-ZGO.
      CALL SCREEN 9200 .
    WHEN 'TOGL' .  "Toggle
      PERFORM toggle_process.
    when 'ZDEL' .  " Old Data Delete...( 7 Day's Data will be remain..)
      perform DELETE_PROCESS.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*&      Module  CREATE_ALV_GRID  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE create_alv_grid OUTPUT.
  IF gs_custom_container IS INITIAL.
*-----> CREATE OBJECT
*    CREATE OBJECT GS_APPLICATION.

    CREATE OBJECT gs_custom_container
        EXPORTING container_name = wa_container.

    CREATE OBJECT alv_grid
        EXPORTING i_parent = gs_custom_container.

    PERFORM  build_variant.
    PERFORM  build_layout.
    PERFORM  build_fieldcat.
*-----> SET OBJECT
    PERFORM  set_object .

  ENDIF.
ENDMODULE.                 " CREATE_ALV_GRID  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  READ_COUNT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE read_count INPUT.
  r_datum-sign   = 'I'.
  r_datum-option = 'EQ'.
  APPEND r_datum .
  PERFORM read_count_record .
ENDMODULE.                 " READ_COUNT  INPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9200 INPUT.
  save_ok_code = ok_code.
  CLEAR: ok_code.
  CASE save_ok_code .
    WHEN 'SAVE' .
      PERFORM save_process.
      PERFORM read_tables .
    WHEN 'BACK' .
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_9200  INPUT
