*----------------------------------------------------------------------*
*   INCLUDE YAPP_ROUTING_I01                                           *
*----------------------------------------------------------------------*
MODULE modify_data INPUT.
  MODIFY IT_app802 INDEX tc_app802-current_line.
*  if sy-subrc <> 0 and tc_app802-current_line = 1 .
*    append IT_app802.
*  endif.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  user_command_0110  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0110 INPUT.
  DATA: l_error .
  CASE ok_code.
    WHEN 'SEA'.  "Search
      CLEAR: ok_code,
             wa_ins_flg,
             wa_del_flg,
             wa_upd_flg,
             IT_app802, IT_app802[],
             it_del, it_del[].
      PERFORM search_data.

    WHEN 'INS'.  "Create
      CLEAR ok_code.
      wa_ins_flg = 'X'.
      PERFORM insert_new_line.

    WHEN 'UPD'.  "Update
      CLEAR ok_code.
      wa_upd_flg = 'X'.
      PERFORM mark_upd_flag.

    WHEN 'DEL'.  "Delete
      CLEAR ok_code.
      PERFORM delete_data.

    WHEN 'CON'.  "Confirm
      CLEAR: ok_code, l_error.
*      PERFORM modify_sap USING l_error.
*      IF l_error <> space.
*        CLEAR: wa_ins_flg,
*               wa_del_flg,
*               wa_upd_flg,
*               IT_app802, IT_app802[],
*               it_upd, it_upd[],
*               it_del, it_del[].
*        PERFORM search_data.
*        EXIT.
*      ENDIF.
      PERFORM update_table.
      CLEAR: wa_ins_flg,
             wa_del_flg,
             wa_upd_flg,
             IT_app802, IT_app802[],
             it_upd, it_upd[],
             it_del, it_del[].
      PERFORM search_data.

    WHEN 'ACD'.  "Ascending
      clear ok_code.
      perform sort_ascending.

    WHEN 'DES'.  "Descending
      clear ok_code.
      perform sort_descending.

  ENDCASE.
ENDMODULE.                 " user_command_0110  INPUT
*&---------------------------------------------------------------------*
*&      Module  exit  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit INPUT.
  LEAVE PROGRAM.
ENDMODULE.                 " exit  INPUT
