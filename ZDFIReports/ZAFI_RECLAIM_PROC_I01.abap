*&---------------------------------------------------------------------*
*&  Include           ZAFI_RECLAIM_PROC_I01
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  EXIT_COMMAND  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module exit_command input.

  case ok_code.
    when 'BACK'.
      leave to screen 0.
    when 'EXIT' or 'CANC'.
      leave program.
  endcase.

endmodule.                 " EXIT_COMMAND  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module user_command_0100 input.

  data : lv_flag type char01,
         lv_ans  type char01.

  clear: ty_rows, ty_rows[], ty_row,  ty_roid, ty_roid[].

  g_status = '2'.

*  CALL METHOD cl_gui_cfw=>flush
*    EXCEPTIONS
*      cntl_system_error = 1
*      cntl_error        = 2.
*
*  CALL METHOD cl_gui_cfw=>update_view
*    EXCEPTIONS
*      cntl_system_error = 1
*      cntl_error        = 2.

  call method cl_gui_cfw=>dispatch
    importing
      return_code = g_return.

  call method g_grid1->dispatch
    exporting
      cargo         = sy-ucomm
      eventid       = cl_gui_alv_grid=>mc_evt_modified
      is_shellevent = ''.

  call method g_grid1->get_selected_rows
    importing
      et_index_rows = gt_row.


  case ok_code.

***
***      call method g_grid1->get_selected_rows
***        importing
***          et_index_rows = ty_rows       "ALV INDEX INFO
***          et_row_no     = ty_roid.      "ALV ROW INFO
***
***      loop at ty_rows into ty_row.
***        read table gt_grid  index ty_row.
***
***        if sy-subrc = 0.
***          if gt_grid-belnr is not initial.
***            message s411 with gt_grid-belnr.
***          endif.
***
***          check gt_grid-belnr is initial.
***
***          gv_index =  ty_row.
***
***          perform posting_data.
***
***          read table gt_grid  index ty_row.
***          if gt_grid-belnr is not initial.
***            message s024 with gt_grid-belnr '? ?? ?????'.
***          endif.
***        endif.
***
***      endloop.
***
***      if ty_rows[] is initial.
***        message e026.
****       exit.
***      endif.

    WHEN 'CHECK'.
      PERFORM get_selected_rows.
      PERFORM fill_bapi_structures USING 'S'.
*      PERFORM display_grid.
    WHEN 'POST'.
      PERFORM get_selected_rows.
      PERFORM fill_bapi_structures USING 'P'.
*      PERFORM display_grid.
    WHEN 'REVERSAL'.
      PERFORM get_selected_rows.
      PERFORM reversal_bapi_post.
*      PERFORM display_grid.
*-Start of changes +UD1K942469
    WHEN 'POST_FLAG'.
      PERFORM get_selected_rows.
      IF NOT it_output[] IS INITIAL.
        w_msg = text-001.
        PERFORM update_table USING 'M' w_msg.
*        PERFORM display_grid.
      ENDIF.
    WHEN 'POST_UNFLA'.
      PERFORM get_selected_rows.
      IF NOT it_output[] IS INITIAL.
        w_msg = space.
        PERFORM update_table USING ' ' w_msg.
*        PERFORM display_grid.
      ENDIF.
    WHEN 'DELE'.
      PERFORM get_selected_rows.
      IF NOT it_output[] IS INITIAL.
        w_msg = text-005.
        PERFORM update_table USING 'D' w_msg.
*        PERFORM display_grid.
      ENDIF.
    WHEN 'UNDELE'.
      PERFORM get_selected_rows.
      IF NOT it_output[] IS INITIAL.
        w_msg = space.
        PERFORM update_table USING ' ' w_msg.
*        PERFORM display_grid.
      ENDIF.
*-End of changes +UD1K942469

  endcase.

  perform refresh_grid  using g_grid1 'X'.

endmodule.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Form  SETTING_F4_FIELD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form setting_f4_field .

  data: ls_f4          type lvc_s_f4,
        lt_f4          type lvc_t_f4.

*  clear ls_f4.
*  ls_f4-fieldname  = 'ZGL_ACCT'.
*  ls_f4-register   = 'X'.
*  ls_f4-getbefore  = ' '.
*  ls_f4-chngeafter = 'X'.
*  append ls_f4 to lt_f4.


  call method g_grid1->register_f4_for_fields
    exporting
      it_f4 = lt_f4.

endform.                    " SETTING_F4_FIELD
