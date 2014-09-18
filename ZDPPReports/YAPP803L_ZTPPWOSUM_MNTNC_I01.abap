*----------------------------------------------------------------------*
*   YAPP803L_ZTPPWOSUM_MNTNC_I01
*----------------------------------------------------------------------*
module modify_data input.
  modify it_app803 index tc_app803-current_line.
*  if sy-subrc <> 0 and TC_APP803-current_line = 1 .
*    append IT_APP803.
*  endif.
endmodule.
*&---------------------------------------------------------------------*
*&      Module  user_command_0110  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module user_command_0110 input.
  data: l_error .
  case ok_code.
    when 'SEA'.  "Search
      clear: ok_code,
             wa_ins_flg,
             wa_del_flg,
             wa_upd_flg,
             it_app803, it_app803[],
             it_del, it_del[].
      perform search_data.

    when 'INS'.  "Create
      clear ok_code.
      wa_ins_flg = 'X'.
      perform insert_new_line.

    when 'UPD'.  "Update
      clear ok_code.
      wa_upd_flg = 'X'.
      perform mark_upd_flag.

    when 'DEL'.  "Delete
      clear ok_code.
      perform delete_data.

    when 'CON'.  "Confirm
      clear: ok_code, l_error.
      perform update_table.
      clear: wa_ins_flg,
             wa_del_flg,
             wa_upd_flg,
             it_app803, it_app803[],
             it_upd, it_upd[],
             it_del, it_del[].
      perform search_data.

    when 'ACD'.  "Ascending
      clear ok_code.
      perform sort_ascending.

    when 'DES'.  "Descending
      clear ok_code.
      perform sort_descending.

    when 'POS'.  "Searching The Line
      clear ok_code.
      perform popup_positionieren.
  endcase.
endmodule.                 " user_command_0110  INPUT
*&---------------------------------------------------------------------*
*&      Module  exit  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module exit input.
  leave program.
endmodule.                 " exit  INPUT
