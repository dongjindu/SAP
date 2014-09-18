*&---------------------------------------------------------------------*
*&  Include           ZAFIU210F01
*&---------------------------------------------------------------------*
FORM create_alv_grid.

  DATA : ls_layout TYPE lvc_s_layo,
         lt_exclud TYPE ui_functions,
         lt_fldcat TYPE lvc_t_fcat,
         l_handle  TYPE REF TO cl_gui_alv_grid,
         lt_sortab TYPE lvc_t_sort.
  DATA : lv_top TYPE i.

  IF g_alvgrd IS INITIAL.

*  Create a custom container control for our ALV Control
    PERFORM create_custom_container    USING c_cont_0100 g_cuctnr
                                      CHANGING g_alvgrd.
    PERFORM build_layout USING c_cont_0100
                      CHANGING ls_layout.
    PERFORM build_grid_exclude USING c_cont_0100
                            CHANGING lt_exclud.
    PERFORM build_fieldcat USING c_cont_0100
                        CHANGING lt_fldcat.
    PERFORM build_event_handler USING c_cont_0100
                             CHANGING l_handle.
    PERFORM build_sort USING c_cont_0100
                    CHANGING lt_sortab.
    PERFORM display_alv_grid USING ls_layout lt_exclud g_alvgrd
                          CHANGING it_list[] lt_fldcat lt_sortab.

    CALL METHOD g_alvgrd->set_frontend_layout
      EXPORTING
        is_layout = ls_layout.
    CALL METHOD g_alvgrd->set_ready_for_input
      EXPORTING
        i_ready_for_input = 0.

  ELSE.

    PERFORM build_layout USING c_cont_0100 CHANGING ls_layout.
    CALL METHOD g_alvgrd->set_frontend_layout
      EXPORTING
        is_layout = ls_layout.
    PERFORM refresh_alv_grid USING g_alvgrd.

  ENDIF.

ENDFORM.                    "CREATE_ALV_GRID_MAIN
*&---------------------------------------------------------------------*
*&      Form  BUILD_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->PV_CTNRNM  text
*      -->PS_LAYOUT  text
*----------------------------------------------------------------------*
FORM build_layout   USING pv_ctnrnm TYPE scrfname
                 CHANGING ps_layout TYPE lvc_s_layo.
  DATA : ls_layout TYPE lvc_s_layo.

  CASE pv_ctnrnm.
    WHEN c_cont_0100.
      ps_layout-box_fname  = 'MARK'.
      ps_layout-sel_mode   = 'A'.
      ps_layout-zebra      = 'X'.
      ps_layout-cwidth_opt = 'X'.
      ps_layout-stylefname = 'STYLE'.
*      PS_LAYOUT-EXCP_FNAME = 'LIGHT'.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.                    "BUILD_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  BUILD_GRID_EXCLUDE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->PV_CTNRNM  text
*      -->PT_EXCLUD  text
*----------------------------------------------------------------------*
FORM build_grid_exclude USING pv_ctnrnm TYPE scrfname
                     CHANGING pt_exclud TYPE ui_functions.
  CASE pv_ctnrnm.
    WHEN c_cont_0100.
      APPEND :
        cl_gui_alv_grid=>mc_fc_loc_append_row    TO pt_exclud,
        cl_gui_alv_grid=>mc_fc_loc_copy          TO pt_exclud,
        cl_gui_alv_grid=>mc_fc_loc_copy_row      TO pt_exclud,
        cl_gui_alv_grid=>mc_fc_loc_cut           TO pt_exclud,
        cl_gui_alv_grid=>mc_fc_loc_delete_row    TO pt_exclud,
        cl_gui_alv_grid=>mc_fc_loc_insert_row    TO pt_exclud,
        cl_gui_alv_grid=>mc_fc_loc_move_row      TO pt_exclud,
        cl_gui_alv_grid=>mc_fc_loc_paste         TO pt_exclud,
        cl_gui_alv_grid=>mc_fc_loc_paste_new_row TO pt_exclud,
        cl_gui_alv_grid=>mc_fc_loc_undo          TO pt_exclud,
        cl_gui_alv_grid=>mc_fc_graph             TO pt_exclud,
        cl_gui_alv_grid=>mc_fc_subtot            TO pt_exclud,
        cl_gui_alv_grid=>mc_mb_sum               TO pt_exclud.
    WHEN OTHERS.
  ENDCASE.
ENDFORM.                    "BUILD_GRID_EXCLUDE
*&---------------------------------------------------------------------*
*&      Form  BUILD_EVENT_HANDLER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->PV_CTNRNM  text
*      -->PR_HANDLE  text
*----------------------------------------------------------------------*
FORM build_event_handler USING pv_ctnrnm TYPE scrfname
                      CHANGING pr_handle TYPE REF TO cl_gui_alv_grid.

*  case pv_ctnrnm.
*    when c_cont_0100.
*      if pr_handle is initial.
*        create object pr_handle.
*      endif.
**      set handler pr_handle->handle_data_changed for g_alvgrd.
*
*      call method g_alvgrd->register_edit_event
*        exporting
*          i_event_id = cl_gui_alv_grid=>mc_evt_modified.
*
*    when others.
*  endcase.
ENDFORM.                    "BUILD_EVENT_HANDLER
*&---------------------------------------------------------------------*
*&      Form  BUILD_SORT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->PR_SORT    text
*----------------------------------------------------------------------*
FORM build_sort USING pv_ctnrnm TYPE scrfname
             CHANGING pr_sort TYPE lvc_t_sort.

  CASE pv_ctnrnm.
    WHEN c_cont_0100.
*          perform build_sort_value using: pr_sort 'MATNR' 4 '0' ' '.
*      perform build_sort_value using: pr_sort 'MAKTX' 4 '0' 'X'.
  ENDCASE.

ENDFORM.                    "build_sort
*&---------------------------------------------------------------------*
*&      Form  BUILD_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->PV_CTNRNM  text
*      -->ET_FLDCAT  text
*----------------------------------------------------------------------*
FORM build_fieldcat USING pv_ctnrnm TYPE scrfname
                 CHANGING et_fldcat TYPE lvc_t_fcat.

  DATA: l_tabix TYPE sy-tabix,
        ls_fdcat LIKE lvc_s_fcat.

  DATA: lv_edit.
  DATA: l_strlen TYPE i.

  CASE pv_ctnrnm.
    WHEN c_cont_0100.
      CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
        EXPORTING
          i_buffer_active    = 'X'
          i_bypassing_buffer = 'X'
          i_structure_name   = 'ZSFIU20'
        CHANGING
          ct_fieldcat        = et_fldcat[].
  ENDCASE.


  LOOP AT et_fldcat INTO ls_fdcat.
    l_tabix = sy-tabix.

    CASE ls_fdcat-fieldname.
      WHEN 'MANDT'.
        ls_fdcat-no_out = 'X'.
      WHEN 'MESSAGE'.
        ls_fdcat-reptext = 'Message'.
        ls_fdcat-scrtext_l = ls_fdcat-reptext.
        ls_fdcat-scrtext_m = ls_fdcat-reptext.
        ls_fdcat-scrtext_s = ls_fdcat-reptext.
*      when 'BDATJ' or 'POPER' or 'WAERS' or
*           'EXTC'  or 'MEINS'.
*        ls_fdcat-no_out = 'X'.
      WHEN OTHERS.
    ENDCASE.

    MODIFY et_fldcat FROM ls_fdcat INDEX l_tabix.
    CLEAR ls_fdcat.

  ENDLOOP.

ENDFORM.                   " BUILD_FIELDCAT
*&--------------------------------------------------------------------*
*&      Form  make_field_cat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM set_fieldcat_fields    USING p_fieldname
                                  p_scrtext
                                  p_datatype
                                  p_outputlen
                                  p_key
                                  p_just
                                  p_noout
                                  p_edit
                                  p_f4availabl
                                  p_reffield
                                  p_reftable
                         CHANGING et_fldcat TYPE lvc_t_fcat.

  DATA: ls_fcat TYPE lvc_s_fcat.

  g_colpos = g_colpos + 1.

  ls_fcat-col_pos       = g_colpos.
  ls_fcat-fieldname     = p_fieldname.
  ls_fcat-datatype      = p_datatype.
  ls_fcat-outputlen     = p_outputlen.
  ls_fcat-scrtext_l     = p_scrtext.
  ls_fcat-colddictxt    = 'L'.
  ls_fcat-key           = p_key.
  ls_fcat-f4availabl    = p_f4availabl.
  ls_fcat-ref_field     = p_reffield.
  ls_fcat-ref_table     = p_reftable.
  ls_fcat-no_out        = p_noout.
  ls_fcat-just          = p_just.
  ls_fcat-edit          = p_edit.

  APPEND ls_fcat TO et_fldcat.

ENDFORM.            " MAKE_FIELD_CAT
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_ALV_GRID
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->PS_LAYOUT  text
*      -->PT_EXCLU   text
*      -->PR_ALVGRD  text
*      -->CT_OUTALV  text
*      -->CT_FLDCAT  text
*      -->CT_SORT    text
*----------------------------------------------------------------------*
FORM display_alv_grid   USING ps_layout TYPE lvc_s_layo
                              pt_exclu  TYPE ui_functions
                              pr_alvgrd TYPE REF TO cl_gui_alv_grid
                     CHANGING ct_outalv
                              ct_fldcat TYPE lvc_t_fcat
                              ct_sort   TYPE lvc_t_sort.

  DATA: ls_variant TYPE disvariant.

  ls_variant-report   = sy-repid.
  ls_variant-username = sy-uname.

  CALL METHOD pr_alvgrd->set_table_for_first_display
    EXPORTING
      is_layout            = ps_layout
      it_toolbar_excluding = pt_exclu
      is_variant           = ls_variant
      i_save               = 'A'
      i_default            = 'X'
    CHANGING
      it_outtab            = ct_outalv
      it_fieldcatalog      = ct_fldcat
      it_sort              = ct_sort
    EXCEPTIONS
      OTHERS               = 4.

ENDFORM.                    "DISPLAY_ALV_GRID
*&---------------------------------------------------------------------*
*&      Form  REFRESH_ALV_GRID
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM refresh_alv_grid USING pr_alvgrd TYPE REF TO cl_gui_alv_grid.

  DATA : ls_stabl TYPE lvc_s_stbl.

  ls_stabl-row = 'X'.
  ls_stabl-col = 'X'.
  pr_alvgrd->refresh_table_display( EXPORTING
                                      is_stable      = ls_stabl
                                      i_soft_refresh = ' ' ).

ENDFORM.                    "REFRESH_ALV_GRID
*&---------------------------------------------------------------------*
*&      Form  GET_AR_MONTHLY_AMOUNT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_ar_monthly_amount.

  DATA: lt_impr TYPE TABLE OF impr WITH HEADER LINE.
  DATA: l_tabix TYPE stabix.
  DATA: BEGIN OF lt_posnr OCCURS 0,
          posid TYPE im_posid,
        END OF lt_posnr.
  DATA: lt_imak TYPE TABLE OF imak,
        ls_imak TYPE imak.
  RANGES: r_versi FOR ztfiu132-versi.

  IF NOT p_versi IS INITIAL.
    r_versi = 'IEQ'.
    r_versi-low = p_versi.
    APPEND r_versi.
  ENDIF.

  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE it_list
    FROM ztfiu132
   WHERE ayear EQ p_ayear
     AND posnr IN s_posid
     AND versi IN r_versi.

  IF NOT it_list[] IS INITIAL.

    LOOP AT it_list INTO st_list.
      lt_posnr-posid = st_list-posnr.
      APPEND lt_posnr.
    ENDLOOP.

    SELECT *
      INTO CORRESPONDING FIELDS OF TABLE lt_impr
      FROM impr
       FOR ALL ENTRIES IN lt_posnr
     WHERE gjahr = p_ayear
       AND posid = lt_posnr-posid.
    SORT lt_impr BY posnr.

    SELECT *
      INTO CORRESPONDING FIELDS OF TABLE lt_imak
      FROM imak
       FOR ALL ENTRIES IN it_list
     WHERE posnr = it_list-posnr
       AND stratflg = p_stflg.
    SORT lt_imak BY posnr .
  ENDIF.


  LOOP AT it_list INTO st_list.
    l_tabix = sy-tabix.

    READ TABLE lt_imak INTO ls_imak WITH KEY posnr = st_list-posnr
                                    BINARY SEARCH.
    IF sy-subrc NE 0.
      DELETE it_list INDEX l_tabix.
      CONTINUE.
    ELSE.
      st_list-imak = ls_imak.
    ENDIF.

    READ TABLE lt_impr WITH KEY gjahr = p_ayear
                                posid = st_list-posnr.
    IF sy-subrc NE 0.
      DELETE it_list INDEX l_tabix.
      CONTINUE.
    ELSE.
      st_list-objnr = lt_impr-objnr.
      st_list-prnam = lt_impr-prnam.
      st_list-kostl = lt_impr-kostl.
    ENDIF.
    st_list-icon = icon_led_yellow.

    MODIFY it_list FROM st_list INDEX l_tabix.
  ENDLOOP.

ENDFORM.                    " GET_AR_MONTHLY_AMOUNT

*&---------------------------------------------------------------------*
*&      Form  SHOW_SE16N
*&---------------------------------------------------------------------*
FORM show_se16n .
  DATA: lt_ofields    LIKE se16n_output OCCURS 0 WITH HEADER LINE.
  DATA: lt_se16event  TYPE TABLE OF se16n_events_type. " occurs 0.
  DATA: ls_se16event  TYPE se16n_events_type.

  PERFORM se16stab_init.
  PERFORM se16stab_append
          USING 'AYEAR' 'I' 'EQ' p_ayear ''.
  PERFORM se16stab_append
          USING 'GUBUN' 'I' 'EQ' p_gubun ''.
  IF NOT p_versi IS INITIAL.
    PERFORM se16stab_append
            USING 'VERSI' 'I' 'EQ' p_versi ''.
  ENDIF.
  LOOP AT s_posid.
    PERFORM se16stab_append
            USING 'POSID' s_posid-sign s_posid-option s_posid-low s_posid-high.
  ENDLOOP.

* output fields
*  lt_ofields-FIELD  = 'AYEAR'.  append lt_ofields.
  lt_ofields-field  = 'POSID'.  APPEND lt_ofields.
  lt_ofields-field  = 'GJAHR'.  APPEND lt_ofields.
  lt_ofields-field  = 'SEQ'.    APPEND lt_ofields.
  lt_ofields-field  = 'TOT'.    APPEND lt_ofields.
  lt_ofields-field  = 'WTP01'.  APPEND lt_ofields.
  lt_ofields-field  = 'WTP02'.  APPEND lt_ofields.
  lt_ofields-field  = 'WTP03'.  APPEND lt_ofields.
  lt_ofields-field  = 'WTP04'.  APPEND lt_ofields.
  lt_ofields-field  = 'WTP05'.  APPEND lt_ofields.
  lt_ofields-field  = 'WTP06'.  APPEND lt_ofields.
  lt_ofields-field  = 'WTP07'.  APPEND lt_ofields.
  lt_ofields-field  = 'WTP08'.  APPEND lt_ofields.
  lt_ofields-field  = 'WTP09'.  APPEND lt_ofields.
  lt_ofields-field  = 'WTP10'.  APPEND lt_ofields.
  lt_ofields-field  = 'WTP11'.  APPEND lt_ofields.
  lt_ofields-field  = 'WTP12'.  APPEND lt_ofields.
  lt_ofields-field  = 'TEXT'.   APPEND lt_ofields.
  lt_ofields-field  = 'POSNR'.  APPEND lt_ofields.

*call-back event - refer LSE16NF30
  ls_se16event-callback_program = sy-cprog.
  ls_se16event-callback_form    = 'SE16_CALLBACK'.
  ls_se16event-callback_event   = 'SAVE'.         "refer LSE16NTOP
  APPEND ls_se16event TO lt_se16event.

* copied from SE16N_INTERFACE
  CALL FUNCTION 'Z_SE16N_INTERFACE'
    EXPORTING
      i_tab                       = 'ZTFI_IMFM'
      i_edit                      = 'X'
*   I_SAPEDIT                   = ' '
*   I_NO_TXT                    = ' '
      i_max_lines                 = p_max
*   I_LINE_DET                  = ' '
*   I_DISPLAY                   = 'X'
*   I_CLNT_SPEZ                 = ' '
    i_clnt_dep                  = 'X'
*   I_VARIANT                   = ' '
*   I_OLD_ALV                   = ' '
    i_checkkey                  = 'X'
*   I_TECH_NAMES                = ' '
*   I_CWIDTH_OPT_OFF            = ' '
*   I_SCROLL                    = ' '
*   I_NO_CONVEXIT               = ' '
*   I_LAYOUT_GET                = ' '
*   I_ADD_FIELD                 =
*   I_ADD_FIELDS_ON             =
*   I_UNAME                     =
* IMPORTING
*   E_LINE_NR                   =
*   E_DREF                      =
   TABLES
     it_selfields                = gt_se16stab
     it_output_fields            = lt_ofields
*   IT_OR_SELFIELDS             =
     it_callback_events          = lt_se16event
*   IT_ADD_UP_CURR_FIELDS       =
*   IT_ADD_UP_QUAN_FIELDS       =
* EXCEPTIONS
*   NO_VALUES                   = 1
*   OTHERS                      = 2
            .
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.
ENDFORM.                    " SHOW_SE16N

*---------------------------------------------------------------------*
*       FORM se16stab_init                                            *
*---------------------------------------------------------------------*
FORM se16stab_init.
  REFRESH gt_se16stab.
  CLEAR gt_se16stab.
ENDFORM.                    "SE16STAB_INIT
*---------------------------------------------------------------------*
*       FORM se16stab_append                                          *
*---------------------------------------------------------------------*
FORM se16stab_append
     USING i_field LIKE se16n_seltab-field
           i_sign   TYPE char1
           i_option TYPE char2
           i_low  TYPE any
           i_high TYPE any.
  CLEAR gt_se16stab.
  gt_se16stab-field = i_field.
  gt_se16stab-sign = 'I'.

  gt_se16stab-option = 'EQ'.
  gt_se16stab-low    = i_low.
  APPEND gt_se16stab.
ENDFORM.                    "SE16STAB_APPEND
*&---------------------------------------------------------------------*
* call back form from SE16N
*&---------------------------------------------------------------------*
FORM se16_callback USING   p_exit_point
                           p_add_info
                           p_tab
                  CHANGING p_tabref.

  FIELD-SYMBOLS: <wa> TYPE ztfi_imfm.
  FIELD-SYMBOLS: <field>.

  ASSIGN p_tabref->* TO <wa>.
  ASSIGN COMPONENT 'UNAME' OF STRUCTURE <wa> TO <field>.
  <field> = sy-uname.
  ASSIGN COMPONENT 'DDATE' OF STRUCTURE <wa> TO <field>.
  <field> = sy-datum.

  ASSIGN COMPONENT 'TWAER' OF STRUCTURE <wa> TO <field>.
  <field> = gv_waers.

* Position external - internal.
  <wa>-posnr = <wa>-posid.

* month total
  ASSIGN COMPONENT 'TOT' OF STRUCTURE <wa> TO <field>.
  <field> = <wa>-wtp01 + <wa>-wtp02 + <wa>-wtp03 +
            <wa>-wtp04 + <wa>-wtp05 + <wa>-wtp06 +
            <wa>-wtp07 + <wa>-wtp08 + <wa>-wtp09 +
            <wa>-wtp10 + <wa>-wtp11 + <wa>-wtp12.

  DATA: l_seq LIKE ztfi_imfm-seq.
* process data for insert/modify/delete record
  CASE p_add_info.
    WHEN 'MODIFY'.

* if insert, default value...
    WHEN 'INSERT'.
      <wa>-ayear = p_ayear.
      <wa>-gubun = p_gubun.
      <wa>-versi = p_versi.

      IF <wa>-gjahr IS INITIAL.
        <wa>-gjahr = p_ayear. "default year if not entered.
      ENDIF.

* "budget sup,return,transfer - keep line item, otherwise overwrite
      IF p_gubun CA '234'.
        SELECT MAX( seq ) INTO l_seq FROM ztfi_imfm
          WHERE ayear = <wa>-ayear
            AND posid = <wa>-posid.
        IF sy-subrc = 0.
          <wa>-seq = l_seq + 1.
        ENDIF.
      ENDIF.

    WHEN 'DELETE'.

  ENDCASE.


ENDFORM.                    "SE16_CALLBACK
*&---------------------------------------------------------------------*
*&      Form  copy_plan_to_pi
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM copy_plan_to_pi.

*  data: begin of lt_imak occurs 0,
*          posnr type ima_posnr,
*          stratflg type im_stratflg,
*        end of lt_imak.
*
*  data: lt_imfm type table of ztfi_imfm,
*        ls_imfm type ztfi_imfm.
*
*  select posnr stratflg
*    into table lt_imak
*    from imak
*   where stratflg eq 'X'.
*
*    if not lt_imak[] is INITIAL.
*    select *
*      from ztfiu132
*
*  loop at lt_imak.
*    clear ls_imfm.
*    ls_imfm-posnr = lt_imak-posnr.
*    ls_imfm-ayear = lt_imak-gjahr.
*    ls_imfm-seq = lt_imak-seq.
*    ls_imfm-posnr = lt_imak-posnr.
*    ls_imfm-posnr = lt_imak-posnr.
*  endloop.


ENDFORM.                    "copy_plan_to_pi
*&---------------------------------------------------------------------*
*&      Form  COPY_TO_PI
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM copy_to_pi.

  PERFORM check_rows_selected.
  CHECK g_error = space.
  PERFORM check_confirm_result.
  CHECK g_answer = c_yes.
  PERFORM copy_to_pi_processing.

ENDFORM.                    "COPY_TO_PI.
*&---------------------------------------------------------------------*
*&      Form  copy_to_budget
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM copy_to_budget.
ENDFORM.                    "copy_to_budget
*&---------------------------------------------------------------------*
*&      Form  check_rows_selected
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM check_rows_selected.

  DATA: l_index TYPE i,
        l_cnt TYPE i.

  CLEAR: it_rowno, st_rowno,
         g_error.

* Get rows selected
  PERFORM get_selected_rows TABLES it_rowno
                             USING g_alvgrd
                          CHANGING l_index l_cnt.
  IF l_cnt = 0.
    MESSAGE s001 WITH 'successful'.
    g_error = 'X'.
    EXIT.
  ENDIF.

ENDFORM.                    "check_rows_selected
*&---------------------------------------------------------------------*
*&      Form  check_confirm_result
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM check_confirm_result.

  DATA: l_text(100) TYPE c,
        l_select_row TYPE string.

  CLEAR g_answer.

  DESCRIBE TABLE it_rowno LINES l_select_row.
  LOOP AT it_rowno INTO st_rowno WHERE row_id <= 0.
    l_select_row = l_select_row - 1.
  ENDLOOP.
  CONCATENATE l_select_row text-004 INTO l_text SEPARATED BY space.

  PERFORM popup_to_confirm_step USING text-005 l_text text-006
                             CHANGING g_answer.

ENDFORM.                    "check_confirm_result
*&---------------------------------------------------------------------*
*&      Form  COPY_TO_PI_PROCESSING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM copy_to_pi_processing.

  DATA: lt_imfm TYPE TABLE OF ztfi_imfm,
        ls_imfm TYPE ztfi_imfm.
  DATA: ls_imak TYPE imak.
  DATA: l_objnr TYPE j_objnr.

  DATA: l_total TYPE i,
        l_count TYPE i,
        l_tabix TYPE stabix.

* Tabellen für die Programmhierarchie
  DATA: lt_progtreevalues	TYPE TABLE OF bapiprogval,
        ls_progtreevalues	TYPE bapiprogval.
  DATA: lt_return LIKE STANDARD TABLE OF bapiret2
                  WITH HEADER LINE.
  DATA: ls_imtp TYPE imtp.
  DATA: l_flg_error.
  DATA: l_lines_in TYPE i,
        l_lines_out TYPE i.
  DATA: l_gubun TYPE z_budget.

  DATA: l_position LIKE bapiprogposid-position.

  DATA: l_no(2) TYPE n,
        l_text(50),
        l_tot LIKE it_list-WTP01.

  FIELD-SYMBOLS: <fs>.

  IF p_plan EQ 'X'.
    g_wrttp = '48'.
    l_gubun = 'P'.
  ELSE.
    g_wrttp = '47'.
    l_gubun = '1'.
  ENDIF.

  DESCRIBE TABLE it_list LINES l_total.


  LOOP AT it_rowno INTO st_rowno WHERE row_id > 0.

*   Create BAPI
    ADD 1 TO l_count.
    PERFORM progress_indicator USING l_count l_total '...'.

    READ TABLE it_list INTO st_list INDEX st_rowno-row_id.
    l_tabix = sy-tabix.

    CLEAR: st_list-icon,
           st_list-message.

    CLEAR: lt_progtreevalues, ls_progtreevalues.
    ls_progtreevalues-position = st_list-posnr.
    ls_progtreevalues-budget_category = '1'.
    ls_progtreevalues-approval_period = 'CURR'.
    IF p_plan EQ 'X'.
      ls_progtreevalues-value_type = 'PLAN'.
    ELSE.
      ls_progtreevalues-value_type = 'BUDG'.
      ls_progtreevalues-activity = 'ORIGINAL'.
    ENDIF.
    ls_progtreevalues-fiscal_year = st_list-gjahr.

    PERFORM conversion_exit_alpha_input CHANGING st_list-versi.

    ls_progtreevalues-version = st_list-versi.
    ls_progtreevalues-value = st_list-tot.
    ls_progtreevalues-currency_trans = gv_waers.

** By Furong on 02/19/14
*    CLEAR: l_no, l_tot.
*    DO 12 TIMES.
*      l_no = l_no + 1.
*      CONCATENATE 'ST_LIST-WTP' l_no INTO l_text.
*      ASSIGN (l_text) TO <fs>.
*      IF sy-subrc = 0.
*        l_tot = l_tot + <fs>.
*      ENDIF.
*    ENDDO.
*    ls_progtreevalues-value = l_tot.
*    APPEND ls_progtreevalues TO lt_progtreevalues.
*    CLEAR ls_progtreevalues-fiscal_year.
*    ls_progtreevalues-value = st_list-tot.
*    APPEND ls_progtreevalues TO lt_progtreevalues.

    append ls_progtreevalues to lt_progtreevalues.
    clear ls_progtreevalues-fiscal_year.
    append ls_progtreevalues to lt_progtreevalues.
** )

    l_position = st_list-posnr.

    IF st_list-gjahr IS INITIAL.
      st_list-icon = icon_led_red.
      st_list-message = 'Fiscal Year is required'.
      MODIFY it_list FROM st_list INDEX l_tabix.
      CONTINUE.
    ENDIF.
    IF st_list-tot IS INITIAL.
      st_list-icon = icon_led_red.
      st_list-message = 'Amount is required'.
      MODIFY it_list FROM st_list INDEX l_tabix.
      CONTINUE.
    ENDIF.

    CALL FUNCTION 'Z_FI_CHANGE_BUDGET_PLAN'
      EXPORTING
        program         = st_list-prnam
        approvalyear    = p_ayear
        position        = l_position
        line_item_text  = st_list-text
        rollup          = 'X'
        wrttp           = g_wrttp
      TABLES
        return          = lt_return
        progtreevalues  = lt_progtreevalues
      EXCEPTIONS
        no_value_change = 1.

    IF sy-subrc EQ 1.
      st_list-icon = icon_led_red.
      st_list-message = 'No Chane in Value'.
      MODIFY it_list FROM st_list INDEX l_tabix.
      CONTINUE.
    ENDIF.
*
    LOOP AT lt_return
      FROM l_lines_in
      WHERE type = con_msgty_abort
      OR    type = con_msgty_error.
      l_flg_error = 'X'.
      st_list-icon = icon_led_red.
      st_list-message = lt_return-message.
      MODIFY it_list FROM st_list INDEX l_tabix.
      EXIT.
    ENDLOOP.

*   Starte Verbuchung, wenn Anlegen fehlerfrei
    IF l_flg_error = space.
      COMMIT WORK.
    ELSE.
      CONTINUE.
    ENDIF.

    CLEAR ls_imfm.

    MOVE-CORRESPONDING st_list TO ls_imfm.
    ls_imfm-posid = st_list-posnr.
    ls_imfm-gubun = l_gubun.
    ls_imfm-reson = '01'.
*    ls_imfm-kostl = st_list-kostl.
    ls_imfm-prnam = st_list-prnam.
    ls_imfm-type = '1'.
    ls_imfm-twaer = gv_waers.
    ls_imfm-uname = sy-uname.
    ls_imfm-zdate = sy-datum.

    PERFORM get_status USING st_list-imak
                    CHANGING ls_imfm-status.

** By Furong on 02/17/14 ( check Imak exist for the same data
    SELECT SINGLE * FROM imak
          WHERE posid = ls_imfm-posid.
    IF sy-subrc = 0.
** End )
      APPEND ls_imfm TO lt_imfm.
    ENDIF.
    st_list-icon = icon_led_green.
    st_list-message = 'Processed Successfully!'.
    MODIFY it_list FROM st_list INDEX l_tabix.
  ENDLOOP.

  IF NOT lt_imfm[] IS INITIAL.
    MODIFY ztfi_imfm FROM TABLE lt_imfm.
  ENDIF.

ENDFORM.                    " COPY_TO_PI_PROCESSING
*&---------------------------------------------------------------------*
*&      Form  get_status
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->ID_OBJNR   text
*----------------------------------------------------------------------*
FORM get_status USING ps_imak TYPE imak
                CHANGING p_status.

  TYPES:  BEGIN OF lt_status_type,
          stab      TYPE bapiappreqstatus OCCURS 5, "Systemstatus
          atab      TYPE bapiappreqstatus OCCURS 5, "Anwenderstatus
        END OF lt_status_type.
  DATA: lt_status TYPE lt_status_type.
  DATA: ls_stab TYPE bapiappreqstatus.

  DATA: lt_imav TYPE STANDARD TABLE OF imav.
  DATA: lt_dd07t TYPE TABLE OF dd07t WITH HEADER LINE.

  CALL FUNCTION 'AIA_TOOL_GET_STATUS'
    EXPORTING
      i_appreq              = ps_imak
      i_language            = sy-langu
      i_only_active         = 'X'
      i_with_text           = 'X'
    TABLES
      it_appreqvarnt        = lt_imav
      et_appreq_status      = lt_status-stab
      et_appreq_user_status = lt_status-atab.
*      et_appreqvarnt_status      = lt_status_var-stab
*      et_appreqvarnt_user_status = lt_status_var-atab.

  SELECT *
    INTO TABLE lt_dd07t
    FROM dd07t
   WHERE domname = 'Z_D_STATUS'
     AND ddlanguage = sy-langu
     AND as4local = 'A'.

  LOOP AT lt_status-stab FROM 1 TO 4 INTO ls_stab
                                WHERE status <> 'I0360'.
*    if ls_stab-status = 'I0013' or "Löschkennzeichen
*       ls_stab-status = 'I0076' or "Löschvormerkung
*       ls_stab-status = 'I0065'.   "Stammdaten-Sperre
*      ld_tabix = '2'.
*    elseif ls_stab-status = 'I0355' or   "Maßnahme angelegt
*           ls_stab-status = 'I0356'. "durch Maßnahme ersetzt
*      ld_tabix = '3'.
*    else.
*      ld_tabix = '1'.
*    endif.

    CHECK NOT ls_stab-description IS INITIAL.
    LOOP AT lt_dd07t.
      TRANSLATE ls_stab-description TO UPPER CASE.
      TRANSLATE lt_dd07t-ddtext TO UPPER CASE.
      FIND ls_stab-description IN lt_dd07t-ddtext.
      IF sy-subrc EQ 0.
        p_status = lt_dd07t-domvalue_l.
        EXIT.
      ENDIF.
    ENDLOOP.
    IF p_status IS INITIAL.
      p_status = 'A'.
    ENDIF.
  ENDLOOP.

ENDFORM.                    "get_status
*&---------------------------------------------------------------------*
*&      Form  free_object
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM free_object.

  IF NOT g_alvgrd IS INITIAL.
    CALL METHOD g_alvgrd->free.
    CALL METHOD g_cuctnr->free.
    FREE: g_alvgrd, g_cuctnr.
  ENDIF.

ENDFORM.                    "free_object
