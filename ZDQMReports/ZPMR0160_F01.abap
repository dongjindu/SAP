*&---------------------------------------------------------------------*
*&      Module  STATUS_0050  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0050 OUTPUT.
  SET PF-STATUS '0050'.
  SET TITLEBAR '0050'.

ENDMODULE.                 " STATUS_0050  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  exit_command  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit_command2 INPUT.

  CASE sy-ucomm.
    WHEN 'BACK' OR 'CANC'.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
  ENDCASE.

ENDMODULE.                 " exit_command2  INPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0050  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0050 INPUT.

  CASE sy-ucomm.
    WHEN 'OKAY'.
      g_rc = 'X'.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0050  INPUT
*&---------------------------------------------------------------------*
*&      Form  get_selected_rows
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM get_selected_rows USING p_chk.
  DATA : lt_rows TYPE lvc_t_row.
  DATA : l_row   TYPE lvc_s_row.

* Get Selected Rows
  CALL METHOD grid1->get_selected_rows
    IMPORTING
      et_index_rows = lt_rows.

  DESCRIBE TABLE lt_rows.
  IF sy-tfill = 0 OR
     sy-tfill > 1.
    p_chk = 'X'.
    MESSAGE i999(zmmm) WITH text-m22 .
  ENDIF.

  LOOP AT lt_rows INTO l_row.
    READ TABLE g_alv1_t INDEX l_row-index.
    g_alv1_t-box = 'X'.
    MODIFY g_alv1_t INDEX l_row-index.
  ENDLOOP.

ENDFORM.                    " get_selected_rows
*&---------------------------------------------------------------------*
*&      Form  set_cell_attribute
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM set_cell_attribute .
* 각 Cell의 특성 제어
  DATA : lt_color   TYPE lvc_t_scol,
         lt_celltab TYPE lvc_t_styl.

  DATA: l_ebeln         LIKE bseg-ebeln.

  LOOP AT g_alv1_t.
    g_index = sy-tabix.

    CLEAR: lt_celltab[], lt_color[].

    PERFORM fill_celltab CHANGING lt_celltab.
    PERFORM fill_color   CHANGING lt_color.

    CLEAR: g_alv1_t-celltab[], g_alv1_t-f_col[].

    INSERT LINES OF lt_celltab INTO TABLE g_alv1_t-celltab.
    INSERT LINES OF lt_color   INTO TABLE g_alv1_t-f_col.

    MODIFY g_alv1_t INDEX g_index.
    CLEAR  g_alv1_t.

  ENDLOOP.

ENDFORM.                    " set_cell_attribute

*&---------------------------------------------------------------------*
*&      Form  set_cell_attribute
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM set_cell_attribute_2.
* 각 Cell의 특성 제어
  DATA : lt_color   TYPE lvc_t_scol,
         lt_celltab TYPE lvc_t_styl.

  DATA: l_ebeln         LIKE bseg-ebeln.

  LOOP AT g_alv2_t.
    g_index = sy-tabix.

    CLEAR: lt_celltab[], lt_color[].

    PERFORM fill_celltab_2 CHANGING lt_celltab.
    PERFORM fill_color_2   CHANGING lt_color.

    CLEAR: g_alv2_t-celltab[], g_alv2_t-f_col[].

    INSERT LINES OF lt_celltab INTO TABLE g_alv2_t-celltab.
    INSERT LINES OF lt_color   INTO TABLE g_alv2_t-f_col.

    MODIFY g_alv2_t INDEX g_index.
    CLEAR  g_alv2_t.

  ENDLOOP.

ENDFORM.                    " set_cell_attribute_2


*&---------------------------------------------------------------------*
*&      Form  fill_celltab
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fill_celltab  CHANGING pt_celltab  TYPE lvc_t_styl.

* F4 control# ##
  DATA : ls_celltab TYPE lvc_s_styl,
        l_mode     TYPE raw4.

  DATA : l_fieldcat TYPE lvc_s_fcat.

  LOOP AT gt_lvc1 INTO l_fieldcat.

    ls_celltab-fieldname = l_fieldcat-fieldname.
*// Display ## ## ##
    IF  (  ls_celltab-fieldname  = 'BOX'
         ).
      ls_celltab-style = cl_gui_alv_grid=>mc_style_enabled.
      INSERT ls_celltab INTO TABLE pt_celltab.
    ELSE.
      ls_celltab-style = cl_gui_alv_grid=>mc_style_disabled +
      cl_gui_alv_grid=>mc_style_f4_no.
      INSERT ls_celltab INTO TABLE pt_celltab.
    ENDIF.

  ENDLOOP.

ENDFORM.                    " fill_celltab


*&---------------------------------------------------------------------*
*&      Form  fill_color
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fill_color   CHANGING pt_color  TYPE lvc_t_scol.

  DATA : ls_celltab TYPE lvc_s_styl,
        ls_color   TYPE lvc_s_scol,
        l_mode     TYPE raw4.

  DATA : l_fieldcat TYPE lvc_s_fcat.

  LOOP AT gt_lvc1 INTO l_fieldcat.
    ls_color-fname = l_fieldcat-fieldname.
    IF g_alv1_t-box = 'X'.
      ls_color-color-col = 4.
      ls_color-color-int = 0.
      INSERT ls_color INTO TABLE pt_color.
    ELSE.
*// ### ##### ####...
      IF ( ls_color-fname  = 'BOX'         OR
           ls_color-fname  = 'GRUND'
          ).
      ELSE.
        ls_color-color-col = 2.
        ls_color-color-int = 0.
        INSERT ls_color INTO TABLE pt_color.
      ENDIF.
*// ######
    ENDIF.
  ENDLOOP.

ENDFORM.                    " fill_color_200


*&---------------------------------------------------------------------*
*&      Form  fill_celltab
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fill_celltab_2  CHANGING pt_celltab  TYPE lvc_t_styl.

* F4 control# ##
  DATA : ls_celltab TYPE lvc_s_styl,
        l_mode     TYPE raw4.

  DATA : l_fieldcat TYPE lvc_s_fcat.

  LOOP AT gt_lvc2 INTO l_fieldcat.

    ls_celltab-fieldname = l_fieldcat-fieldname.

    IF  ( ls_celltab-fieldname  = 'LFDAT'  OR
          ls_celltab-fieldname  = 'ZREQTY'  OR
          ls_celltab-fieldname  = 'VERPR'  OR
          ls_celltab-fieldname  = 'WAERS'  OR
          ls_celltab-fieldname  = 'EKGRP'  OR
          ls_celltab-fieldname  = 'PEINH'
         ).
      ls_celltab-style = cl_gui_alv_grid=>mc_style_enabled.
      INSERT ls_celltab INTO TABLE pt_celltab.
    ELSE.
      ls_celltab-style = cl_gui_alv_grid=>mc_style_disabled +
      cl_gui_alv_grid=>mc_style_f4_no.
      INSERT ls_celltab INTO TABLE pt_celltab.
    ENDIF.
*    ENDIF.
  ENDLOOP.

ENDFORM.                    " fill_celltab_2

*&---------------------------------------------------------------------*
*&      Form  fill_color
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fill_color_2  CHANGING pt_color  TYPE lvc_t_scol.

  DATA : ls_celltab TYPE lvc_s_styl,
        ls_color   TYPE lvc_s_scol,
        l_mode     TYPE raw4.

  DATA : l_fieldcat TYPE lvc_s_fcat.

  LOOP AT gt_lvc2 INTO l_fieldcat.
    ls_color-fname = l_fieldcat-fieldname.

    IF ( ls_color-fname  = 'LFDAT'  OR
         ls_color-fname  = 'ZREQTY'  OR
         ls_color-fname  = 'VERPR'  OR
         ls_color-fname  = 'WAERS'  OR
         ls_color-fname  = 'EKGRP'  OR
         ls_color-fname  = 'PEINH'

    ).
    ELSE.
      ls_color-color-col = 2.
      ls_color-color-int = 0.
      INSERT ls_color INTO TABLE pt_color.
    ENDIF.

  ENDLOOP.

ENDFORM.                    " fill_color_2
*---------------------------------------------------------------------*
*       FORM f4help_layout                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  CS_LAYOUT                                                     *
*---------------------------------------------------------------------*
FORM f4help_layout CHANGING cs_layout LIKE disvariant-variant.

  DATA: ls_variant LIKE disvariant.

  ls_variant-report      = sy-repid.
  ls_variant-handle      = space.
  ls_variant-log_group   = space.
  ls_variant-username    = space.
  ls_variant-variant     = cs_layout.
  ls_variant-text        = space.
  ls_variant-dependvars  = space.

  DATA: lt_dynpfields LIKE dynpread OCCURS 0 WITH HEADER LINE.

  CLEAR lt_dynpfields.
  lt_dynpfields-fieldname = 'P_LAYOUT'.
  APPEND lt_dynpfields.

  READ TABLE lt_dynpfields WITH KEY fieldname = 'P_LAYOUT'.
  IF sy-subrc EQ 0.
    IF NOT lt_dynpfields-fieldvalue(1) CA '0123456789/' AND
       NOT lt_dynpfields-fieldvalue IS INITIAL.
      ls_variant-username = sy-uname.
    ENDIF.
    ls_variant-variant = lt_dynpfields-fieldvalue.
  ENDIF.

  DATA: lt_fcat TYPE lvc_t_fcat.
  CALL FUNCTION 'LVC_VARIANT_SAVE_LOAD'
    EXPORTING
      i_save_load = 'F'
      i_tabname   = '1'
    CHANGING
      cs_variant  = ls_variant
      ct_fieldcat = lt_fcat[].

  cs_layout = ls_variant-variant.

ENDFORM.                    "f4help_layout

*&---------------------------------------------------------------------*
*&      Form  select_all_item
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM select_all_item .

  IF g_check       = 'X'.

    g_check         = ''.
    g_alv1_t-box    = ''.
    MODIFY g_alv1_t TRANSPORTING box WHERE box  EQ 'X'.
*                                         and flag ne 'X'.

  ELSE.

    g_alv1_t-box   = 'X'.
    g_check        = 'X'.
    MODIFY g_alv1_t TRANSPORTING box WHERE box  EQ ' '.
*                                         and flag ne 'X'.

  ENDIF.

ENDFORM.                    " select_all_item

*&---------------------------------------------------------------------*
*&      Form  refresh_table_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM refresh_table_data .

  CALL METHOD grid1->refresh_table_display.
  CALL METHOD grid2->refresh_table_display.
*  CALL METHOD grid3->refresh_table_display.

ENDFORM.                    " refresh_table_data
*&---------------------------------------------------------------------*
*&      Form  popup_confirm
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM popup_confirm USING p_answer
                         p_line1
                         p_line2
                         p_title.

  CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
    EXPORTING
      defaultoption = 'Y'
      textline1     = p_line1
      textline2     = p_line2
      titel         = p_title
    IMPORTING
      answer        = p_answer.

ENDFORM.                    " popup_confirm
*&---------------------------------------------------------------------*
*&      Form  select_all_item_DETAIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM select_all_item_detail .

  IF g_check2       = 'X'.
    g_check2          = ''.
    g_alv2_t-mark     = ''.
    MODIFY g_alv2_t TRANSPORTING mark WHERE mark  EQ 'X'.
  ELSE.

    g_alv2_t-mark   = 'X'.
    g_check2        = 'X'.
    MODIFY g_alv2_t TRANSPORTING mark WHERE mark  EQ ' '.
  ENDIF.

ENDFORM.                    " select_all_item_DETAIL
*&---------------------------------------------------------------------*
*&      Form  PROGRESS_INDICATOR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM progress_indicator .

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      text   = 'Data Loading....'
    EXCEPTIONS
      OTHERS = 1.

ENDFORM.                    " PROGRESS_INDICATOR
*&---------------------------------------------------------------------*
*&      Form  data_changed
*&---------------------------------------------------------------------*
FORM data_changed  USING    rr_data_changed
                            TYPE REF TO cl_alv_changed_data_protocol
                            e_onf4         TYPE char1
                            e_ucomm        TYPE sy-ucomm.
  DATA: l_planetype TYPE s_planetye,
        ls_saplane TYPE saplane,
        ls_good_seatsocc TYPE lvc_s_modi.

  DATA : ls_mod_cells    TYPE lvc_s_modi,
         ls_goods         TYPE lvc_s_modi,
         ls_inserted_row TYPE lvc_s_moce,
         ls_cells        TYPE lvc_s_modi,
         x_iwerk         LIKE t001w-iwerk.

*  error_in_data = space.
*
*  LOOP AT rr_data_changed->mt_good_cells INTO ls_goods.
*
*    CLEAR ls_mod_cells.
*    READ TABLE rr_data_changed->mt_good_cells
*               WITH KEY row_id    = ls_goods-row_id
*                        INTO ls_mod_cells.
*    IF NOT ls_mod_cells-value IS INITIAL.
*      CASE ls_mod_cells-fieldname.
*        WHEN 'ZTPLT'.
*          CALL METHOD rr_data_changed->get_cell_value
*            EXPORTING
*              i_row_id    = ls_mod_cells-row_id
*              i_fieldname = ls_mod_cells-fieldname
*            IMPORTING
*              e_value     = l_planetype.
*
*          SELECT SINGLE * FROM saplane INTO ls_saplane WHERE
*                                           planetype = l_planetype.
*          IF sy-subrc NE 0.
** In case of error, create a protocol entry in the application log.
** Possible values for message type ('i_msgty'):
**
**    'A': Abort (Stop sign)
**    'E': Error (red LED)
**    'W': Warning (yellow LED)
**    'I': Information (green LED)
**
*            CALL METHOD rr_data_changed->add_protocol_entry
*              EXPORTING
*                i_msgid     = '0K'
*                i_msgno     = '000'
*                i_msgty     = 'E'
*                i_msgv1     = text-m03           "Flugzeugtyp
*                i_msgv2     = l_planetype
*                i_msgv3     = text-m05           "exitstiert nicht
*                i_fieldname = ' '
*                i_row_id    = ' '.
*
*            error_in_data = 'X'.
*            EXIT.
*          ENDIF.
*
*      ENDCASE.
*
*    ENDIF.
*  ENDLOOP.
*
**?.Display application log if an error has occured.
*  IF error_in_data EQ 'X'.
*    CALL METHOD rr_data_changed->display_protocol.
*  ENDIF.


ENDFORM.                    " data_changed
*&---------------------------------------------------------------------*
*&      Form  data_changed2
*&---------------------------------------------------------------------*
FORM data_changed2  USING    rr_data_changed
                            TYPE REF TO cl_alv_changed_data_protocol
                            e_onf4         TYPE char1
                            e_ucomm        TYPE sy-ucomm.

  DATA : ls_mod_cells    TYPE lvc_s_modi,
         ls_goods         TYPE lvc_s_modi,
         ls_inserted_row TYPE lvc_s_moce,
         ls_cells        TYPE lvc_s_modi.

ENDFORM.                    " data_changed2
*&---------------------------------------------------------------------*
*&      Form  on_f4
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM on_f4  USING sender         TYPE REF TO cl_gui_alv_grid
                  e_fieldname    TYPE lvc_fname
                  e_fieldvalue   TYPE lvc_value
                  es_row_no      TYPE lvc_s_roid
                  er_event_data  TYPE REF TO cl_alv_event_data
                  et_bad_cells   TYPE lvc_t_modi
                  e_display      TYPE char01.

  DATA : selectfield   LIKE  help_info-fieldname,
         it_fields     LIKE  help_value OCCURS 1 WITH HEADER LINE,
         select_value  LIKE  help_info-fldvalue,
         ld_tabix      LIKE  sy-tabix,
         ls_modi       TYPE  lvc_s_modi,
         f_date        LIKE sy-datum.
  FIELD-SYMBOLS : <f4tab> TYPE lvc_t_modi.

  IF  e_fieldname = 'ZFODATE'.

    CALL FUNCTION 'F4_DATE'
      EXPORTING
        date_for_first_month = sy-datum
      IMPORTING
        select_date          = f_date.

    ASSIGN er_event_data->m_data->* TO <f4tab>.

    ls_modi-row_id    = es_row_no-row_id.
    ls_modi-fieldname =  e_fieldname.
    ls_modi-value     = f_date.
    ls_modi-error     = 'X'.
    APPEND ls_modi TO <f4tab>.

  ENDIF.

ENDFORM.                                                    " on_f4
*&---------------------------------------------------------------------*
*&      Form  register_events
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM register_events .

  DATA: lt_f4 TYPE lvc_t_f4 WITH HEADER LINE.

  CALL METHOD grid1->register_f4_for_fields
    EXPORTING
      it_f4 = lt_f4[].

*/ ALV
  CREATE OBJECT event_receiver.

  SET HANDLER event_receiver->handle_user_command           FOR grid1.
  SET HANDLER event_receiver->handle_hotspot_click          FOR grid1.
  SET HANDLER event_receiver->handle_toolbar                FOR grid1.
  SET HANDLER event_receiver->handle_double_click           FOR grid1.
  SET HANDLER event_receiver->handle_data_changed_finished  FOR grid1.
  SET HANDLER event_receiver->handle_onf4                   FOR grid1.

  CALL METHOD grid1->set_toolbar_interactive.

ENDFORM.                    " register_events

*&---------------------------------------------------------------------*
*&      Form  clear_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM clear_data .


ENDFORM.                    " clear_data
*&---------------------------------------------------------------------*
*&      Form  CALL_MESSAGE_SCREEN.
*&---------------------------------------------------------------------*
FORM call_message_screen.
  DATA lc_message_count TYPE i.
  DESCRIBE TABLE gt_bdcmsg LINES lc_message_count.
  CHECK lc_message_count > 0.
  CALL SCREEN 2000 STARTING AT 10 10
                   ENDING   AT 89 25.

ENDFORM.                    " CALL_MESSAGE_SCREEN.

*&---------------------------------------------------------------------*
*&      Module  LIST_MESSAGES  OUTPUT
*&---------------------------------------------------------------------*
MODULE list_messages OUTPUT.
  PERFORM show_messages.
ENDMODULE.                 " LIST_MESSAGES  OUTPUT

*&---------------------------------------------------------------------*
*&      Form  SHOW_MESSAGES
*&---------------------------------------------------------------------*
FORM show_messages.
  SET PF-STATUS space.
  SET TITLEBAR 'TT_MESSAGE'.
  SUPPRESS DIALOG.
  LEAVE TO LIST-PROCESSING AND RETURN TO SCREEN 0.
  PERFORM write_message_top.
  PERFORM write_message_line.
ENDFORM.                    " SHOW_MESSAGES

*&---------------------------------------------------------------------*
*&      Form  WRITE_MESSAGE_TOP
*&---------------------------------------------------------------------*
FORM write_message_top.
  FORMAT COLOR COL_HEADING.
  WRITE: /03(02) 'St',
          06(20) 'Message ID',
          27(03) 'No.'
                      ,
          31(50) 'Message'.
  ULINE.
ENDFORM.                    " WRITE_MESSAGE_TOP

*&---------------------------------------------------------------------*
*&      Form  WRITE_MESSAGE_LINE
*&---------------------------------------------------------------------*
FORM write_message_line.
  DATA: lc_message TYPE bapiret2-message,
        lc_odd.
  LOOP AT gt_bdcmsg.
    AT NEW banfn.
      WRITE: /01(10) gt_bdcmsg-banfn COLOR COL_KEY
                                     INTENSIFIED ON.
      CASE gt_bdcmsg-updkz.
        WHEN 'I'.
          WRITE: 12(02) icon_create AS ICON.
        WHEN 'U'.
          WRITE: 12(02) icon_change AS ICON.
        WHEN 'D'.
          WRITE: 12(02) icon_delete AS ICON.
      ENDCASE.
    ENDAT.
    IF lc_odd IS INITIAL.
      lc_odd = 'X'.
      FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
    ELSE.
      CLEAR lc_odd.
      FORMAT COLOR COL_NORMAL INTENSIFIED ON.
    ENDIF.
    CASE gt_bdcmsg-type.
      WHEN 'S'.
        WRITE: /03(02) icon_led_green  AS ICON.
      WHEN 'W'.
        WRITE: /03(02) icon_led_yellow AS ICON.
      WHEN 'E' OR 'A' OR 'X'.
        WRITE: /03(02) icon_led_red    AS ICON.
      WHEN OTHERS.
        WRITE: /03(02) icon_space      AS ICON.
    ENDCASE.

    WRITE: 06(20) gt_bdcmsg-id,
           27(03) gt_bdcmsg-number,
           31(200) gt_bdcmsg-message.
  ENDLOOP.
ENDFORM.                    " WRITE_MESSAGE_LINE
*&---------------------------------------------------------------------*
*&      Form  CALL_MESSAGE_SCREEN.
*&---------------------------------------------------------------------*
*       처리결과 메시지 보이기.
*----------------------------------------------------------------------*
FORM call_message_screen2.
  DATA lc_message_count TYPE i.
  DESCRIBE TABLE gt_bdcmsg2 LINES lc_message_count.
  CHECK lc_message_count > 0.
  CALL SCREEN 2100 STARTING AT 10 10
                   ENDING   AT 89 25.

ENDFORM.                    " CALL_MESSAGE_SCREEN.
*&---------------------------------------------------------------------*
*&      Module  LIST_MESSAGES  OUTPUT
*&---------------------------------------------------------------------*
*       결과 메시지 리스트.
*----------------------------------------------------------------------*
MODULE list_messages2 OUTPUT.
  PERFORM show_messages2.
ENDMODULE.                 " LIST_MESSAGES  OUTPUT

*&---------------------------------------------------------------------*
*&      Form  SHOW_MESSAGES
*&---------------------------------------------------------------------*
FORM show_messages2.
  SET PF-STATUS space.
  SET TITLEBAR 'TT_MESSAGE'.
  SUPPRESS DIALOG.
  LEAVE TO LIST-PROCESSING AND RETURN TO SCREEN 0.
  PERFORM write_message_top2.
  PERFORM write_message_line2.
ENDFORM.                    " SHOW_MESSAGES

*&---------------------------------------------------------------------*
*&      Form  WRITE_MESSAGE_TOP
*&---------------------------------------------------------------------*
*       메시지 헤더 쓰기.
*----------------------------------------------------------------------*
FORM write_message_top2.
  FORMAT COLOR COL_HEADING.
  WRITE: /03(02) 'St',
          06(20) 'Message ID',
          27(03) 'No.'
                      ,
          31(50) 'Message'.
  ULINE.
ENDFORM.                    " WRITE_MESSAGE_TOP
*&---------------------------------------------------------------------*
*&      Form  WRITE_MESSAGE_LINE
*&---------------------------------------------------------------------*
*       메시지 내역 쓰기.
*----------------------------------------------------------------------*
FORM write_message_line2.
  DATA: lc_message TYPE bapiret2-message,
        lc_odd.
  LOOP AT gt_bdcmsg2.
    AT NEW vbeln.
      WRITE: /01(10) gt_bdcmsg2-vbeln COLOR COL_KEY
                                     INTENSIFIED ON.
      CASE gt_bdcmsg2-updkz.
        WHEN 'I'.
          WRITE: 12(02) icon_create AS ICON.
        WHEN 'U'.
          WRITE: 12(02) icon_change AS ICON.
        WHEN 'D'.
          WRITE: 12(02) icon_delete AS ICON.
      ENDCASE.
    ENDAT.
    IF lc_odd IS INITIAL.
      lc_odd = 'X'.
      FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
    ELSE.
      CLEAR lc_odd.
      FORMAT COLOR COL_NORMAL INTENSIFIED ON.
    ENDIF.
    CASE gt_bdcmsg2-msgtyp.
      WHEN 'S'.
        WRITE: /03(02) icon_led_green  AS ICON.
      WHEN 'W'.
        WRITE: /03(02) icon_led_yellow AS ICON.
      WHEN 'E' OR 'A' OR 'X'.
        WRITE: /03(02) icon_led_red    AS ICON.
      WHEN OTHERS.
        WRITE: /03(02) icon_space      AS ICON.
    ENDCASE.
    CALL FUNCTION 'MESSAGE_TEXT_BUILD'
      EXPORTING
        msgid               = gt_bdcmsg2-msgid
        msgnr               = gt_bdcmsg2-msgnr
        msgv1               = gt_bdcmsg2-msgv1
        msgv2               = gt_bdcmsg2-msgv2
        msgv3               = gt_bdcmsg2-msgv3
        msgv4               = gt_bdcmsg2-msgv4
      IMPORTING
        message_text_output = lc_message.

    WRITE: 06(20) gt_bdcmsg2-msgid,
           27(03) gt_bdcmsg2-msgnr,
           31(50) lc_message.
  ENDLOOP.
ENDFORM.                    " WRITE_MESSAGE_LINE
*&---------------------------------------------------------------------*
*&      Form  hotspot_click
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_ROW_ID  text
*      -->P_E_COLUMN_ID  text
*----------------------------------------------------------------------*
FORM hotspot_click  USING    e_row_id
                             e_column_id.
  DATA : lv_bukrs LIKE t001-bukrs.

  READ TABLE g_alv1_t INDEX e_row_id.

  CASE e_column_id.
    WHEN 'MATNR'.
      PERFORM maintain_display USING 'X'
                                     g_alv1_t-matnr
*                                     'KVA1'
                                     s_werks-low
                                     g_alv1_t-lgort
                                     '29'
                                     'MM03'.
  ENDCASE.

ENDFORM.                    " hotspot_click
*&---------------------------------------------------------------------*
*&      Form  hotspot_click
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_ROW_ID  text
*      -->P_E_COLUMN_ID  text
*----------------------------------------------------------------------*
FORM hotspot_click2  USING    e_row_id
                             e_column_id.
  DATA : lv_bukrs LIKE t001-bukrs.

  READ TABLE g_alv2_t INDEX e_row_id.

ENDFORM.                    " hotspot_click2
*&---------------------------------------------------------------------*
*&      Module  modify_screen  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE modify_screen OUTPUT.

  LOOP AT SCREEN.
    IF screen-group1 = 'GP'.
      screen-invisible = 0.
      screen-active    = 0.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

ENDMODULE.                 " modify_screen  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  read_text
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0757   text
*      -->P_IT_LIST_LIFNR  text
*      <--P_IT_LIST_LIFNX  text
*----------------------------------------------------------------------*
FORM read_text  USING    u_id
                         u_value
                CHANGING c_text.
  CASE u_id.
    WHEN 'LGORT'.
      SELECT SINGLE lgobe INTO c_text
             FROM t001l
              WHERE werks = s_werks-low
                AND lgort = u_value.
    WHEN 'FING'.
      SELECT SINGLE fing INTO c_text
                  FROM t357
                  WHERE beber = u_value
                  AND   werks = s_werks-low.

    WHEN 'MATNR'.
      SELECT SINGLE maktx
      FROM makt
      INTO c_text
      WHERE matnr = u_value
      AND spras = sy-langu.
    WHEN 'KUNNR'.
      SELECT SINGLE name1
      INTO c_text
      FROM kna1
      WHERE kunnr = u_value.

  ENDCASE.

ENDFORM.                    " read_text

*&---------------------------------------------------------------------*
*&      Form  save_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM save_data USING p_stat
             CHANGING p_flg.

  DATA : l_flg,
         it_m009 LIKE ztpm0009 OCCURS 0 WITH HEADER LINE.

  CLEAR : gt_bdcmsg, gt_bdcmsg[].

  PERFORM check_main_rtn  CHANGING p_flg.

  IF p_flg EQ 'X'.
    EXIT.
  ENDIF.

  PERFORM pr_create_rtn .

  IF NOT v_banfn IS INITIAL.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.

    LOOP AT gt_return WHERE type = 'S'.
      MOVE-CORRESPONDING gt_return TO gt_bdcmsg.
      gt_bdcmsg-banfn = v_banfn.
      gt_bdcmsg-updkz = 'I'.
      APPEND gt_bdcmsg. CLEAR : gt_bdcmsg.
    ENDLOOP.

  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    LOOP AT gt_return WHERE type = 'E'.
      MOVE-CORRESPONDING gt_return TO gt_bdcmsg.
      gt_bdcmsg-updkz = 'I'.
      APPEND gt_bdcmsg. CLEAR : gt_bdcmsg.
    ENDLOOP.
  ENDIF.

  PERFORM call_message_screen.


ENDFORM.                    " save_data

*&---------------------------------------------------------------------*
*&      Form  check_main_rtn
*&---------------------------------------------------------------------*
FORM check_main_rtn CHANGING p_flg.

  DATA :  l_chk,
          l_chk2,
          time_stamp  TYPE timestamp,
          time_stamp2 TYPE timestamp,
          l_reltime   TYPE jit_relrdate,
          l_tzone     LIKE ttzz-tzone VALUE 'UTC',
          tz          TYPE ttzz-tzone.


  LOOP AT g_alv1_t WHERE box = 'X'.
*    LOOP AT g_alv2_t INTO g_alv2_s.
*      CONVERT DATE g_alv2_s-ausbs TIME g_alv2_s-auztb
*            INTO TIME STAMP time_stamp2 TIME ZONE l_tzone.
*      IF time_stamp EQ time_stamp2.
*        l_chk = 'X'.
*        EXIT.
*      ENDIF.
*    ENDLOOP.
*
*    LOOP AT g_alv2_t INTO g_alv2_s.
*      CONVERT DATE g_alv2_s-ausbs TIME g_alv2_s-auztb
*            INTO TIME STAMP time_stamp2 TIME ZONE l_tzone.
*
*      IF time_stamp2 > time_stamp.
*        l_chk2 = 'X'.
*        EXIT.
*      ENDIF.
*
*      IF g_alv2_s-aufnr IS INITIAL OR
*         g_alv2_s-equnr IS INITIAL.
*        MESSAGE i999 WITH text-m13.
*        p_flg ='X'.
*        EXIT.
*      ENDIF.
*
*      IF g_alv2_s-dt_cat IS INITIAL .
*        MESSAGE i999 WITH text-m14.
*        p_flg ='X'.
*        EXIT.
*      ENDIF.
*
*    ENDLOOP.

  ENDLOOP.

ENDFORM.                    " check_main_rtn

*&---------------------------------------------------------------------*
*&      Form  GET_DATA_GRID3
*&---------------------------------------------------------------------*
FORM get_data_grid3 .

  DATA :   timediff               TYPE tvro-fahztd
         , timediffn(11)          TYPE n
         , l_ausvn                TYPE ztpm0009-ausvn
         , l_auztv                TYPE ztpm0009-auztv
         , l_ausbs                TYPE ztpm0009-ausbs
         , l_auztb                TYPE ztpm0009-auztb
         , l_tabix                TYPE sy-tabix
         .

ENDFORM.                    " GET_DATA_GRID3

*&---------------------------------------------------------------------*
*&      Form  INIT_DATA
*&---------------------------------------------------------------------*
FORM init_data .

  h_shop  = text-t01.
  h_pernr = text-t02.

  SELECT SINGLE pernr ename
         INTO (p_pernr, t_pernr)
         FROM pa0001
       WHERE pernr = sy-uname
         AND endda >= sy-datum
         AND begda <= sy-datum.

ENDFORM.                    " INIT_DATA
*&---------------------------------------------------------------------*
*&      Form  READ_OPEN_PR_DATA
*&---------------------------------------------------------------------*
FORM read_open_pr_data .

  CLEAR : it_preq, it_preq[],
          it_preq_t, it_preq_t[].

  CHECK NOT g_itab_t[] IS INITIAL.

  SELECT a~matnr a~meins a~menge
        INTO CORRESPONDING FIELDS OF TABLE it_preq
         FROM eban AS a
*          FOR ALL ENTRIES IN g_itab_t
         WHERE a~matnr IN s_matnr
           AND a~werks IN s_werks
           AND a~lgort IN s_lgort
           AND a~loekz = space
           AND a~ebeln = space.
*           AND NOT EXISTS ( SELECT ebeln
*                              FROM ekpo
*                              WHERE banfn = a~banfn
*                                AND bnfpo = a~bnfpo
*                                AND loekz = space
*                                AND elikz = space ).

  LOOP AT it_preq.
    MOVE-CORRESPONDING it_preq TO it_preq_t.
    COLLECT it_preq_t. CLEAR it_preq_t.
  ENDLOOP.

ENDFORM.                    " READ_OPEN_PR_DATA
*&---------------------------------------------------------------------*
*&      Form  MAINTAIN_MASTER_DATA
*&---------------------------------------------------------------------*
FORM maintain_master_data .

  DATA : l_return LIKE zmms0053.

  READ TABLE g_alv1_t INTO g_alv1_s WITH KEY box = 'X'.

  MOVE-CORRESPONDING g_alv1_s TO zspm0019.

  CLEAR : g_lminb, g_lbstf, g_rc.

  CALL SCREEN 0050 STARTING AT 10 10.

  CHECK NOT g_rc IS INITIAL.

  zspm0019-lminb = g_lminb.
  zspm0019-lbstf = g_lbstf.
  zspm0019-lgort = s_lgort-low.
  zspm0019-werks = s_werks-low.

  PERFORM update_mm02_data CHANGING l_return.

  IF l_return-type = 'E'.
    MESSAGE e999(zmmm) WITH l_return-message.
  ELSE.
    MESSAGE s999(zmmm) WITH l_return-message.
  ENDIF.

ENDFORM.                    " MAINTAIN_MASTER_DATA

*&---------------------------------------------------------------------*
*&      Form  UPDATE_MM02_DATA
*&---------------------------------------------------------------------*
FORM update_mm02_data  CHANGING e_return STRUCTURE zmms0053.

  CALL FUNCTION 'Z_MM_MATERIAL_MASTER_UPDATE_2'
    EXPORTING
      i_body   = zspm0019
    IMPORTING
      e_return = e_return.

ENDFORM.                    " UPDATE_MM02_DATA

*&---------------------------------------------------------------------*
*&      Form  PR_CREATE_RTN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM pr_create_rtn .

  DATA : l_count(4) TYPE n,
         l_cnt(4)   TYPE n,
         l_message(100) TYPE c.
  CLEAR : l_count, v_banfn.
  DATA: wa_exten LIKE bapi_te_requisition_item.


  DESCRIBE TABLE g_alv2_t LINES l_count.

  CLEAR: it_eban, it_eban[].
  CLEAR: it_ebkn, it_ebkn[].
  CLEAR: v_return, v_return[].

  CLEAR : gs_prheader, gs_prheaderx,
           gt_return,   gt_return[],
           gt_pritem,   gt_pritem[],
           gt_pritemx,  gt_pritemx[],
           gt_pritemexp, gt_pritemexp[],
           gt_pracct,   gt_pracct[],
           gt_pracctx,  gt_pracctx[].

  CLEAR : it_exten, it_exten[].

  LOOP AT g_alv2_t INTO g_alv2_s.


    MOVE :  'PM' TO gs_prheader-pr_type.


    MOVE : sy-tabix TO gt_pritem-preq_item,
           ' ' TO gt_pritem-acctasscat,
           g_alv2_s-matnr TO gt_pritem-material,
           g_alv2_s-zreqty TO gt_pritem-quantity,
           g_alv2_s-meins TO gt_pritem-unit,
*<-- KHS Add
           sy-datum TO gt_pritem-preq_date,
*--> KHS Add
           g_alv2_s-lfdat TO gt_pritem-deliv_date,
           g_alv2_s-ekgrp TO gt_pritem-pur_group,
           p_pernr       TO gt_pritem-preq_name,
           s_werks-low   TO gt_pritem-plant,
           g_alv2_s-verpr TO gt_pritem-preq_price,
           g_alv2_s-waers TO gt_pritem-currency,
           g_alv2_s-peinh TO gt_pritem-price_unit,
           s_lgort-low    TO gt_pritem-store_loc.

    MOVE : 'HMMA'  TO gt_pritem-funds_ctr,
           '137000' TO gt_pritem-cmmt_item.

    APPEND gt_pritem.  CLEAR gt_pritem.

    MOVE : 'X' TO gs_prheaderx-pr_type.

    MOVE : sy-tabix TO gt_pritemx-preq_item,
           'X' TO gt_pritemx-preq_itemx,
           'X' TO gt_pritemx-acctasscat,
*           '' TO gt_pritemx-acctasscat,
           'X' TO gt_pritemx-material,
           'X' TO gt_pritemx-quantity,
           'X' TO gt_pritemx-unit,
           'X' TO gt_pritemx-deliv_date,
           'X' TO gt_pritemx-pur_group,
           'X' TO gt_pritemx-preq_name,
           'X' TO gt_pritemx-plant,
           'X' TO gt_pritemx-preq_price,
*           '' TO gt_pritemx-preq_price,
           'X' TO gt_pritemx-currency,
           'X' TO gt_pritemx-price_unit,
           'X' TO gt_pritemx-store_loc.

    MOVE : 'X' TO gt_pritemx-funds_ctr,
           'X' TO gt_pritemx-cmmt_item.

    APPEND gt_pritemx.  CLEAR gt_pritemx.

    MOVE : sy-tabix TO gt_pracct-preq_item,
           sy-tabix TO gt_pracctx-preq_item,
           'X' TO gt_pracctx-preq_itemx,

           '01' TO gt_pracct-serial_no,
           '01' TO gt_pracctx-serial_no,
           'X'  TO gt_pracctx-serial_nox,
           g_alv2_s-zreqty TO gt_pracct-quantity,
           'X' TO gt_pracctx-quantity,
           g_alv2_s-verpr TO gt_pracct-net_value,
           'X' TO gt_pracctx-net_value.

    CASE gt_pritem-acctasscat.
      WHEN 'A'.

      WHEN 'K'.

      WHEN ' ' OR space.
        MOVE : 'HMMA'  TO gt_pracct-funds_ctr,
               '137000' TO gt_pracct-cmmt_item.
        MOVE : 'X' TO gt_pracctx-funds_ctr,
               'X' TO gt_pracctx-cmmt_item.

    ENDCASE.
    APPEND gt_pracct.   CLEAR gt_pracct.
    APPEND gt_pracctx.  CLEAR gt_pracctx.
  ENDLOOP.

  PERFORM call_bapi_transaction.

ENDFORM.                    " PR_CREATE_RTN

*&---------------------------------------------------------------------*
*&      Form  DATA_MOVE_BAPI
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM data_move_bapi USING p_banfn.

  DATA: wa_exten LIKE bapi_te_requisition_item.

  CLEAR : it_exten, it_exten[], g_afnam.

  LOOP AT gt_itab WHERE banfn = p_banfn.


    g_afnam =  gt_itab-afnam.

    MOVE :  gt_itab-bsart TO gs_prheader-pr_type,
            'X' TO gs_prheader-general_release.

    MOVE : gt_itab-bnfpo TO gt_pritem-preq_item,
           gt_itab-knttp TO gt_pritem-acctasscat,
           gt_itab-matnr TO gt_pritem-material,
           gt_itab-menge TO gt_pritem-quantity,
           gt_itab-meins TO gt_pritem-unit,
           gt_itab-lfdat TO gt_pritem-deliv_date,
           gt_itab-ekgrp TO gt_pritem-pur_group,
           gt_itab-afnam TO gt_pritem-preq_name,
           gt_itab-werks TO gt_pritem-plant,
           gt_itab-preis TO gt_pritem-preq_price,
           gt_itab-waers TO gt_pritem-currency,
           gt_itab-peinh TO gt_pritem-price_unit,
           gt_itab-bednr TO gt_pritem-trackingno.

    APPEND gt_pritem.  CLEAR gt_pritem.

    MOVE : 'X' TO gs_prheaderx-pr_type,
           'X' TO gs_prheaderx-general_release.

    MOVE : gt_itab-bnfpo TO gt_pritemx-preq_item,
           'X' TO gt_pritemx-preq_itemx,
           'X' TO gt_pritemx-acctasscat,
           'X' TO gt_pritemx-material,
           'X' TO gt_pritemx-quantity,
           'X' TO gt_pritemx-unit,
           'X' TO gt_pritemx-deliv_date,
           'X' TO gt_pritemx-pur_group,
           'X' TO gt_pritemx-preq_name,
           'X' TO gt_pritemx-plant,
           'X' TO gt_pritemx-preq_price,
           'X' TO gt_pritemx-currency,
           'X' TO gt_pritemx-price_unit,
           'X' TO gt_pritemx-trackingno.

    CASE gt_itab-knttp.
      WHEN ' ' OR space.
        MOVE : gt_itab-fistl TO gt_pritem-funds_ctr,
               gt_itab-fipos TO gt_pritem-cmmt_item.
        MOVE : 'X' TO gt_pritemx-funds_ctr,
               'X' TO gt_pritemx-cmmt_item.
    ENDCASE.

    APPEND gt_pritemx.  CLEAR gt_pritemx.

    MOVE : gt_itab-bnfpo TO gt_pracct-preq_item,
           gt_itab-bnfpo TO gt_pracctx-preq_item,
           'X' TO gt_pracctx-preq_itemx,
           '01' TO gt_pracct-serial_no,
           '01' TO gt_pracctx-serial_no,
           'X'  TO gt_pracctx-serial_nox,
           gt_itab-menge TO gt_pracct-quantity,
           'X' TO gt_pracctx-quantity,
           gt_itab-preis TO gt_pracct-net_value,
           'X' TO gt_pracctx-net_value.

    CASE gt_itab-knttp.
      WHEN 'A'.
        MOVE : gt_itab-anln1 TO gt_pracct-asset_no,
               gt_itab-aufnr TO gt_pracct-orderid.
        MOVE : 'X' TO gt_pracctx-asset_no,
               'X' TO gt_pracctx-orderid.
      WHEN 'K'.
        MOVE : gt_itab-sakto TO gt_pracct-gl_account,
               gt_itab-kostl TO gt_pracct-costcenter.
        MOVE : 'X' TO gt_pracctx-gl_account,
               'X' TO gt_pracctx-costcenter.

      WHEN ' ' OR space.
        MOVE : gt_itab-fistl TO gt_pracct-funds_ctr,
               gt_itab-fipos TO gt_pracct-cmmt_item.
        MOVE : 'X' TO gt_pracctx-funds_ctr,
               'X' TO gt_pracctx-cmmt_item.

    ENDCASE.
    APPEND gt_pracct.   CLEAR gt_pracct.
    APPEND gt_pracctx.  CLEAR gt_pracctx.
  ENDLOOP.
ENDFORM.                    " DATA_MOVE_BAPI_2
*&---------------------------------------------------------------------*
*&      Form  CALL_BAPI_TRANSACTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM call_bapi_transaction.


  CALL FUNCTION 'BAPI_PR_CREATE'
    EXPORTING
      prheader    = gs_prheader
      prheaderx   = gs_prheaderx
    IMPORTING
      number      = v_banfn
    TABLES
      return      = gt_return
      pritem      = gt_pritem
      pritemx     = gt_pritemx
      praccount   = gt_pracct
      praccountx  = gt_pracctx
      extensionin = it_exten.

ENDFORM.                    " CALL_BAPI_TRANSACTION

*&---------------------------------------------------------------------*
*&      Form  display_status
*&---------------------------------------------------------------------*
*       ## ### ### ####
*----------------------------------------------------------------------*
FORM display_status  USING    p_text.
*...
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = 0
      text       = p_text
    EXCEPTIONS
      OTHERS     = 1.
ENDFORM.                    " display_status

*&---------------------------------------------------------------------*
*&      Form  maintain_display
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM maintain_display  USING  change_m
                              iv_matnr
                              iv_werks
                              iv_lgort
                              iv_auswg
                              iv_tcode_m.

  PERFORM maintain_material_view USING iv_matnr
                                       iv_werks
                                       iv_lgort
                                       iv_auswg
                                       iv_tcode_m.

ENDFORM.                    " maintain_display

*&---------------------------------------------------------------------*
*&      Form  MAINTAIN_MATERIAL_VIEW
*&---------------------------------------------------------------------*
*       Maintain Material View
*----------------------------------------------------------------------*
*      -->IV_MATNR  Material
*      -->IV_WERKS  Plant
*      -->IV_AUSWG  Screen Sequence
*      -->IV_TCODE  Transaction Code
*----------------------------------------------------------------------*
FORM maintain_material_view  USING    iv_matnr
                                      iv_werks
                                      iv_lgort
                                      iv_auswg
                                      iv_tcode.
  DATA: lv_bilds LIKE t133a-bilds,
        ls_t130m LIKE t130m,
        ls_rmmg1 LIKE rmmg1,
        lwa_view TYPE mbildtab,
        lwa_auswg TYPE mgauswg,
        lf_updateok TYPE t130f-kzref,
        lt_views TYPE STANDARD TABLE OF mbildtab INITIAL SIZE 0,
        lt_auswg TYPE STANDARD TABLE OF mgauswg INITIAL SIZE 0.

  CLEAR : ls_t130m, lv_bilds, lt_views[], lt_views,
          ls_rmmg1, lt_auswg, lt_auswg[].

  SELECT SINGLE * FROM mara WHERE matnr EQ iv_matnr.
  CHECK sy-subrc EQ 0.

  CALL FUNCTION 'T130M_SINGLE_READ'
    EXPORTING
      tcode      = iv_tcode
      kzrfb      = 'X'
    IMPORTING
      wt130m     = ls_t130m
    EXCEPTIONS
      not_found  = 1
      wrong_call = 2
      OTHERS     = 3.

  CALL FUNCTION 'BILDSEQUENZ_IDENTIFY'
    EXPORTING
      branche     = mara-mbrsh
      materialart = mara-mtart
      tcode_ref   = ls_t130m-trref
    IMPORTING
      bildsequenz = lv_bilds
    EXCEPTIONS
      wrong_call  = 1
      not_found   = 2
      OTHERS      = 3.

  CALL FUNCTION 'SELECTION_VIEWS_FIND'
    EXPORTING
      bildsequenz     = lv_bilds
      pflegestatus    = mara-pstat
    TABLES
      bildtab         = lt_views[]
    EXCEPTIONS
      call_wrong      = 1
      empty_selection = 2
      OTHERS          = 3.

  ls_rmmg1-matnr = mara-matnr.
  ls_rmmg1-werks = iv_werks.
  ls_rmmg1-lgort = iv_lgort.
*  READ TABLE lt_views INTO lwa_view WITH KEY dytxt = p_dytxt.
*  CHECK sy-subrc EQ 0.
  lwa_auswg-auswg = iv_auswg.
  APPEND lwa_auswg TO lt_auswg.

  CALL FUNCTION 'MATERIAL_MAINTAIN_DIALOGUE'
    EXPORTING
      irmmg1       = ls_rmmg1
      kz_ein_dark  = 'X'
      t_tcode      = iv_tcode
      p_pstat      = mara-pstat
    IMPORTING
      update_ok    = lf_updateok
    TABLES
      iauswg       = lt_auswg[]
    EXCEPTIONS
      no_authority = 1
      OTHERS       = 2.

  IF sy-subrc  EQ 0.
    IF lf_updateok EQ 'X'.
      COMMIT WORK.
    ENDIF.
  ENDIF.
ENDFORM.                    " MAINTAIN_MATERIAL_VIEW
*&---------------------------------------------------------------------*
*&      Form  HELP_REQUEST_S_LGORT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM help_request_s_lgort.
  DATA: BEGIN OF lt_t001l OCCURS 0,
          werks TYPE t001l-werks,
          lgort TYPE t001l-lgort,
          lgobe TYPE t001l-lgobe,
        END OF lt_t001l.
  DATA l_tabix TYPE sy-tabix.
  CLEAR dynpread. REFRESH dynpread.
  CLEAR valuetab. REFRESH valuetab.
  CLEAR fields.   REFRESH fields.

  PERFORM value_read USING: 'S_WERKS-LOW'.
  LOOP AT dynpread.
    CASE sy-tabix.
      WHEN 1. s_werks-low = dynpread-fieldvalue.
    ENDCASE.
  ENDLOOP.

  SELECT werks
         lgort
         lgobe
         INTO TABLE lt_t001l
         FROM t001l
         WHERE werks EQ s_werks-low.
  IF sy-subrc EQ 0.
    LOOP AT lt_t001l.
      l_tabix = sy-tabix.
      IF lt_t001l-werks EQ 'P001'.
        IF NOT lt_t001l-lgort BETWEEN 'P600' AND 'P699'.
          DELETE lt_t001l INDEX l_tabix.
        ENDIF.
      ELSEIF lt_t001l-werks EQ 'E001'.
        IF NOT lt_t001l-lgort BETWEEN 'E600' AND 'E699'.
          DELETE lt_t001l INDEX l_tabix.
        ENDIF.
** Furong on 02/08/12
      ELSEIF lt_t001l-werks EQ 'E002'.
        IF NOT lt_t001l-lgort BETWEEN 'N600' AND 'N699'.
          DELETE lt_t001l INDEX l_tabix.
        ENDIF.
** end on 02/08/12
      ELSE.
        DELETE lt_t001l INDEX l_tabix.
      ENDIF.
    ENDLOOP.
  ENDIF.

  LOOP AT lt_t001l.
    valuetab-value = lt_t001l-werks.
    APPEND valuetab. CLEAR valuetab.
    valuetab-value = lt_t001l-lgort.
    APPEND valuetab. CLEAR valuetab.
    valuetab-value = lt_t001l-lgobe.
    APPEND valuetab. CLEAR valuetab.
  ENDLOOP.

  PERFORM add_fields USING: 'T001L' 'WERKS' ' ',
                            'T001L' 'LGORT' 'X',
                            'T001L' 'LGOBE' ' '.
  PERFORM help_values_get.


  IF select_index > 0.
    READ TABLE lt_t001l   INDEX select_index.
    PERFORM value_update USING:
            'X'   'S_LGORT-LOW' lt_t001l-lgort 0.
  ENDIF.
ENDFORM.                    " HELP_REQUEST_S_LGORT
*&---------------------------------------------------------------------*
*&      Form  VALUE_READ
*&---------------------------------------------------------------------*
FORM value_read USING  p_name.
  dynpread-fieldname = p_name. APPEND dynpread.
  CALL FUNCTION 'DYNP_VALUES_READ'
       EXPORTING
            dyname                   = sy-cprog
            dynumb                   = sy-dynnr
       TABLES
            dynpfields               = dynpread
*      EXCEPTIONS
*           INVALID_ABAPWORKAREA     = 1
*           INVALID_DYNPROFIELD      = 2
*           INVALID_DYNPRONAME       = 3
*           INVALID_DYNPRONUMMER     = 4
*           INVALID_REQUEST          = 5
*           NO_FIELDDESCRIPTION      = 6
*           INVALID_PARAMETER        = 7
*           UNDEFIND_ERROR           = 8
*           DOUBLE_CONVERSION        = 9
*           OTHERS                   = 10
            .
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDFORM.                    " VALUE_READ
*&---------------------------------------------------------------------*
*&      Form  ADD_FIELDS
*&---------------------------------------------------------------------*
FORM add_fields USING  p_tabname p_fieldname p_flag.
  fields-tabname = p_tabname.
  fields-fieldname = p_fieldname.
  fields-selectflag = p_flag.
  APPEND fields.      CLEAR fields.
ENDFORM.                    " ADD_FIELDS
*&---------------------------------------------------------------------*
*&      Form  HELP_VALUES_GET
*&---------------------------------------------------------------------*
FORM help_values_get.
  CALL FUNCTION 'HELP_VALUES_GET_WITH_TABLE_EXT'
    EXPORTING
      display                   = ' '
    IMPORTING
      index                     = select_index
    TABLES
      fields                    = fields
      select_values             = select_values
      valuetab                  = valuetab
    EXCEPTIONS
      field_not_in_ddic         = 1
      more_then_one_selectfield = 2
      no_selectfield            = 3
      OTHERS                    = 4.
ENDFORM.                    " HELP_VALUES_GET
*&---------------------------------------------------------------------*
*&      Form  VALUE_UPDATE
*&---------------------------------------------------------------------*
FORM value_update USING  p_process
                         p_fieldname
                         p_fieldvalue
                         p_stepl.
  CLEAR dynpfields.
  dynpfields-fieldname = p_fieldname.
  dynpfields-fieldvalue = p_fieldvalue.
  IF p_stepl > 0.
    dynpfields-stepl = p_stepl.
  ENDIF.
  APPEND dynpfields.      CLEAR dynpfields.

  IF p_process EQ 'X'.
    CALL FUNCTION 'DYNP_VALUES_UPDATE'
      EXPORTING
        dyname               = sy-cprog
        dynumb               = sy-dynnr
      TABLES
        dynpfields           = dynpfields
      EXCEPTIONS
        invalid_abapworkarea = 1
        invalid_dynprofield  = 2
        invalid_dynproname   = 3
        invalid_dynpronummer = 4
        invalid_request      = 5
        no_fielddescription  = 6
        undefind_error       = 7
        OTHERS               = 8.
    REFRESH dynpfields.
  ENDIF.

ENDFORM.                    " VALUE_UPDATE
*&---------------------------------------------------------------------*
*&      Module  STATUS_0060  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0060 OUTPUT.
  SET PF-STATUS '0050'.
  SET TITLEBAR  '0060'.

ENDMODULE.                 " STATUS_0060  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  MAINTAIN_MASTER_DATA_ABC
*&---------------------------------------------------------------------*
FORM maintain_master_data_abc .
  DATA : l_return LIKE zmms0053.

  READ TABLE g_alv1_t INTO g_alv1_s WITH KEY box = 'X'.

  MOVE-CORRESPONDING g_alv1_s TO zspm0019.

  CLEAR : g_maabc, g_rc.

  CALL SCREEN 0060 STARTING AT 10 10.

  CHECK NOT g_rc IS INITIAL.

  zspm0019-maabc = g_maabc.
  zspm0019-lgort = s_lgort-low.
  zspm0019-werks = s_werks-low.

  PERFORM update_mm02_data CHANGING l_return.

  IF l_return-type = 'E'.
    MESSAGE e999(zmmm) WITH l_return-message.
  ELSE.
    MESSAGE s999(zmmm) WITH l_return-message.
  ENDIF.
ENDFORM.                    " MAINTAIN_MASTER_DATA_ABC
