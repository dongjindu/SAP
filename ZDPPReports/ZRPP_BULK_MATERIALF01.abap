*&---------------------------------------------------------------------*
*&  Include           ZRCO_BULK_MATERIALF01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&  Include           ZHUSSDR71023F01
*&---------------------------------------------------------------------*
FORM create_alv_grid.

  DATA : ls_layout TYPE lvc_s_layo,
         lt_exclud TYPE ui_functions,
         lt_fldcat TYPE lvc_t_fcat,
         l_handle  TYPE REF TO lcl_custom_alv_grid,
         lt_sortab TYPE lvc_t_sort.
  DATA : lv_top TYPE i.

  IF g_alvgrd IS INITIAL.

*  Create a custom container control for our ALV Control
    PERFORM z_create_custom_container    USING c_cont_0100 g_cuctnr
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
    PERFORM z_display_alv_grid USING ls_layout lt_exclud g_alvgrd
                          CHANGING it_list[]  lt_fldcat lt_sortab.

    CALL METHOD g_alvgrd->set_frontend_layout
      EXPORTING
        is_layout = ls_layout.
    CALL METHOD g_alvgrd->set_ready_for_input
      EXPORTING
        i_ready_for_input = 0.

  ELSE.

*    perform build_layout using c_cont_0100 changing ls_layout.
*    call method g_alvgrd->set_frontend_layout
*      exporting
*        is_layout = ls_layout.
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
      ps_layout-box_fname  = 'CHECK'.
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
                      CHANGING pr_handle TYPE REF TO lcl_custom_alv_grid.

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

      CASE p_zresp.
        WHEN 'V'.
          PERFORM build_sort_value USING: pr_sort 'ZGRP3' 1 '0' 'X'.
          PERFORM build_sort_value USING: pr_sort 'ZRESP' 2 '0' 'X'.
          PERFORM build_sort_value USING: pr_sort 'ZGRP1' 3 '0' 'X'.
          PERFORM build_sort_value USING: pr_sort 'MAKTX' 4 '0' ' '.
          PERFORM build_sort_value USING: pr_sort 'MATNR' 5 '0' ' '.
        WHEN 'E'.
          PERFORM build_sort_value USING: pr_sort 'ZRESP' 1 '0' 'X'.
          PERFORM build_sort_value USING: pr_sort 'ZGRP1' 2 '0' 'X'.
          PERFORM build_sort_value USING: pr_sort 'MAKTX' 3 '0' ' '.
          PERFORM build_sort_value USING: pr_sort 'MATNR' 4 '0' ' '.
      ENDCASE.
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
          i_structure_name   = 'ZSCO_BULK_MATERIAL'
        CHANGING
          ct_fieldcat        = et_fldcat[].
  ENDCASE.


  LOOP AT et_fldcat INTO ls_fdcat.
    l_tabix = sy-tabix.

    CASE ls_fdcat-datatype.
      WHEN 'QUAN' OR 'CURR'.
        ls_fdcat-do_sum = 'X'.
    ENDCASE.

    CASE ls_fdcat-fieldname.
      WHEN 'ZGRP3'.
        ls_fdcat-key = 'X'.
      WHEN 'ZRESP'.
        ls_fdcat-reptext = 'Shop'.
        ls_fdcat-scrtext_l = ls_fdcat-reptext.
        ls_fdcat-scrtext_m = ls_fdcat-reptext.
        ls_fdcat-scrtext_s = ls_fdcat-reptext.
        ls_fdcat-key = 'X'.
      WHEN 'ZGRP1'.
        ls_fdcat-reptext = 'Component Group'.
        ls_fdcat-scrtext_l = ls_fdcat-reptext.
        ls_fdcat-scrtext_m = ls_fdcat-reptext.
        ls_fdcat-scrtext_s = ls_fdcat-reptext.
        ls_fdcat-key = 'X'.
      WHEN 'MAKTX'.
        ls_fdcat-reptext = 'Component Description'.
        ls_fdcat-scrtext_l = ls_fdcat-reptext.
        ls_fdcat-scrtext_m = ls_fdcat-reptext.
        ls_fdcat-scrtext_s = ls_fdcat-reptext.
        ls_fdcat-key = 'X'.
      WHEN 'MATNR'.
        ls_fdcat-reptext = 'Component'.
        ls_fdcat-scrtext_l = ls_fdcat-reptext.
        ls_fdcat-scrtext_m = ls_fdcat-reptext.
        ls_fdcat-scrtext_s = ls_fdcat-reptext.
        ls_fdcat-key = 'X'.
      WHEN 'UNITS'.
        ls_fdcat-reptext = 'UM'.
        ls_fdcat-scrtext_l = ls_fdcat-reptext.
        ls_fdcat-scrtext_m = ls_fdcat-reptext.
        ls_fdcat-scrtext_s = ls_fdcat-reptext.
      WHEN 'SORTF'.
        ls_fdcat-reptext = 'RP Point'.
        ls_fdcat-scrtext_l = ls_fdcat-reptext.
        ls_fdcat-scrtext_m = ls_fdcat-reptext.
        ls_fdcat-scrtext_s = ls_fdcat-reptext.
      WHEN 'BDATJ' OR 'POPER' OR 'WAERS' OR
           'EXTC'  OR 'MEINS'.
        ls_fdcat-no_out = 'X'.
      WHEN OTHERS.
    ENDCASE.

    CASE p_zresp.
      WHEN 'V'.
        CASE ls_fdcat-fieldname.
*          WHEN   'ZGRP6' OR 'ZGRP7'.
          WHEN   'ZGRP7'.
            ls_fdcat-no_out = 'X'.
        ENDCASE.
      WHEN 'E'.
        CASE ls_fdcat-fieldname.
          WHEN 'ZGRP3' OR 'SORTF'.
            ls_fdcat-no_out = 'X'.
        ENDCASE.
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
FORM refresh_alv_grid USING pr_alvgrd TYPE REF TO lcl_custom_alv_grid.

  DATA : ls_stabl TYPE lvc_s_stbl.

  ls_stabl-row = 'X'.
  ls_stabl-col = 'X'.
  pr_alvgrd->refresh_table_display( EXPORTING
                                      is_stable      = ls_stabl
                                      i_soft_refresh = ' ' ).

ENDFORM.                    "REFRESH_ALV_GRID
*&---------------------------------------------------------------------*
*&      Form  FREE_OBJECT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM free_object.

  CASE sy-dynnr.
    WHEN 100.
      IF NOT g_alvgrd IS INITIAL.
        CALL METHOD g_alvgrd->free.
        CALL METHOD g_cuctnr->free.
        FREE: g_alvgrd, g_cuctnr.
      ENDIF.
  ENDCASE.

ENDFORM.                    "FREE_OBJECT
*&---------------------------------------------------------------------*
*&      Form  CHECK_ROWS_SELECTED
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM check_rows_selected.

*  data: l_index type i,
*        l_cnt type i.
*
*  clear: it_rowno, st_rowno,
*         g_error.
*
** Get rows selected
*  perform get_selected_rows tables it_rowno
*                             using g_alvgrd
*                          changing l_index l_cnt.
*  if l_cnt = 0.
*    message s016.
*    g_error = 'X'.
*    exit.
*  endif.

ENDFORM.                    "CHECK_ROWS_SELECTED
*&---------------------------------------------------------------------*
*&      Form  f4_layout_variant
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f4_layout_variant.

  DATA: st_variant LIKE disvariant,
        l_exit TYPE c.

  st_variant-report = sy-repid.
  st_variant-username = sy-uname.

  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
    EXPORTING
      is_variant    = st_variant
      i_save        = 'A'
    IMPORTING
      es_variant    = st_variant
    EXCEPTIONS
      not_found     = 1
      program_error = 2
      OTHERS        = 3.

  p_varia = st_variant-variant.

ENDFORM.                    "f4_layout_variant
*&---------------------------------------------------------------------*
*&      Form  HANDLE_DATA_CHANGED
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_CHANGED  text
*      -->P_UCOMM    text
*----------------------------------------------------------------------*
FORM handle_data_changed  USING p_changed TYPE REF TO cl_alv_changed_data_protocol
                                p_ucomm TYPE sy-ucomm.

  DATA: ls_mod_cell  TYPE lvc_s_modi,
        ls_cell      TYPE lvc_s_modi.
  DATA: lt_ref  TYPE REF TO data.

  FIELD-SYMBOLS : <fs_t_ref> TYPE STANDARD TABLE.

  lt_ref = p_changed->mp_mod_rows.

  LOOP AT p_changed->mt_good_cells INTO ls_mod_cell.
    CASE ls_mod_cell-fieldname.

    ENDCASE.
  ENDLOOP.

ENDFORM.                    "HANDLE_DATA_CHANGED
*&---------------------------------------------------------------------*
*&      Form  SET_PF_STATUS_100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM set_pf_status_100.

  DATA: lt_ftab LIKE sy-ucomm OCCURS 0 WITH HEADER LINE.

*    append 'UPLOAD' to lt_ftab.

  SET PF-STATUS 'G_0100' EXCLUDING lt_ftab.
  SET TITLEBAR  'T_0100'.

ENDFORM.                    "SET_PF_STATUS_100
*&---------------------------------------------------------------------*
*&      Form  INITIALIZATION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM initialization.

*  perform get_parameter changing p_bukrs.
  p_bdatj = sy-datum+0(4).
  p_poper = sy-datum+4(2).

ENDFORM.                    "INITIALIZATION
*&---------------------------------------------------------------------*
*&      Form  get_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM get_data.

  CLEAR: r_poper, r_poper[].
  r_poper = 'IBT'.
  r_poper-low = p_poper.
  r_poper-high = p_poper.
  APPEND r_poper.

* Year and Quater
  PERFORM set_year_quater.

* Get initial Data according to Shop
  PERFORM get_initial_data.
* Get Actual Data
  PERFORM get_actual_data.
* Get Target Data
  PERFORM get_target_variance_data.

ENDFORM.                    "get_data
*&---------------------------------------------------------------------*
*&      Form  get_data_from_bw
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM get_data_from_bw.

  DATA: lt_bulk_bw_v TYPE TABLE OF ztpp_bulk_bw_v
                                   WITH HEADER LINE,
        lt_bulk_bw_e TYPE TABLE OF ztpp_bulk_bw_e
                                   WITH HEADER LINE.

  CASE p_zresp.
    WHEN 'V'.
      SELECT *
       INTO CORRESPONDING FIELDS OF TABLE it_list
       FROM ztpp_bulk_bw_v
      WHERE bdatj = p_bdatj
        AND poper = p_poper
        AND zgrp3 IN s_zgrp3
        AND zgrp1 IN s_zgrp1
        AND matnr IN s_matnr.
    WHEN 'E'.
      SELECT *
       INTO CORRESPONDING FIELDS OF TABLE it_list
       FROM ztpp_bulk_bw_e
      WHERE bdatj = p_bdatj
        AND poper = p_poper
        AND zgrp1 IN s_zgrp1
        AND matnr IN s_matnr.
  ENDCASE.

  LOOP AT it_list INTO st_list.
    st_list-units = st_list-meins.
    CLEAR st_list-meins.
    MODIFY it_list FROM st_list.
  ENDLOOP.

ENDFORM.                    "get_data_from_bw
*&---------------------------------------------------------------------*
*&      Form  get_initial_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM get_initial_data.

  RANGES: r_zresp FOR ztco_mat-zresp,
          r_matnr FOR mara-matnr.

  DATA: BEGIN OF lt_ztco_mat OCCURS 0.
  .INCLUDE STRUCTURE ztco_mat.
  DATA: meins LIKE mara-meins.
  DATA: END OF lt_ztco_mat.

  DATA: BEGIN OF lt_stpo OCCURS 0,
          idnrk TYPE idnrk,
          datuv TYPE datuv,
          sortf TYPE sortp,
        END OF lt_stpo.

  DATA: BEGIN OF lt_mseg OCCURS 0,
          matnr  TYPE matnr,
          zgrp3  TYPE zdzeinr03,
          zgrp6  TYPE zdzeinr06,
          zgrp7  TYPE zdzeinr07,
          plnbez TYPE matnr,
          bwart  TYPE bwart,
          menge  TYPE menge_d,
          mblnr  TYPE mblnr,
          lfbnr  TYPE lfbnr,
        END OF lt_mseg.

  DATA: BEGIN OF lt_model OCCURS 0,
          matnr TYPE matnr,
          zgrp3 TYPE zdzeinr03,
        END OF lt_model.

  " Engine or Vehicle
  CASE p_zresp.
    WHEN 'V'.
      r_zresp = 'EEQ'.
      r_zresp-low = 'ENGINE'.
      APPEND r_zresp.
      r_zresp = 'EEQ'.
      r_zresp-low = ' '.
      APPEND r_zresp.
    WHEN 'E'.
      r_zresp = 'IEQ'.
      r_zresp-low = 'ENGINE'.
      APPEND r_zresp.
  ENDCASE.


  SELECT DISTINCT t3~matnr t3~maktx  t3~mtart
         t3~matkl t3~zgrp1 t3~zgrp2  t3~zgrp3
         t3~zgrp4 t3~zgrp5 t3~zgrp6  t3~zgrp7
         t3~zgrp8 t3~zgrp9 t3~zgrp10 t3~zresp
         t3~zgrp6 t3~zgrp7 t1~meins
    INTO CORRESPONDING FIELDS OF TABLE lt_ztco_mat
    FROM mara AS t1 INNER JOIN mbew AS t2
                       ON t1~matnr = t2~matnr
                    INNER JOIN ztco_mat AS t3
                       ON t1~matnr = t3~matnr
   WHERE t1~mtart = 'ROH1'
     AND t2~bklas = '3004'
     AND t3~zresp IN r_zresp
     AND t3~zgrp3 IN s_zgrp3
     AND t3~zgrp1 IN s_zgrp1
     AND t1~matnr IN s_matnr.

  IF NOT lt_ztco_mat[] IS INITIAL.
*-- for RP Point
    SELECT idnrk datuv sortf
      INTO CORRESPONDING FIELDS OF TABLE lt_stpo
      FROM stpo AS t1 INNER JOIN mast AS t2
                         ON t1~stlnr = t2~stlnr
                        AND t2~stlan = 1
    FOR ALL ENTRIES IN lt_ztco_mat
  WHERE idnrk = lt_ztco_mat-matnr.
    SORT lt_stpo BY idnrk ASCENDING datuv DESCENDING.
    DELETE lt_stpo WHERE sortf IS INITIAL.
    DELETE ADJACENT DUPLICATES FROM lt_stpo COMPARING idnrk.


*-- for Price per UM
    r_matnr = 'IEQ'.

*-- for backflush
    IF p_zresp = 'V'.
      SELECT t1~matnr
           t3~zgrp3
           t2~plnbez
           t1~bwart
           t1~menge
           t1~mblnr
           t3~zgrp6
           t3~zgrp5 AS zgrp7
           t1~lfbnr
      INTO CORRESPONDING FIELDS OF TABLE lt_mseg
      FROM mseg AS t1 INNER JOIN afko AS t2
                         ON t1~aufnr = t2~aufnr
                      INNER JOIN ztco_mat AS t3
                         ON t2~plnbez = t3~matnr
                      INNER JOIN mara AS t4
                         ON t3~matnr = t4~matnr
       FOR ALL ENTRIES IN lt_ztco_mat
     WHERE t1~bwart IN ('261','262')
       AND t1~matnr = lt_ztco_mat-matnr
       AND t1~zbudat IN r_period
       AND t4~mtart = 'FERT'.
    ELSE.
      SELECT t1~matnr
             t3~zgrp3
             t2~plnbez
             t1~bwart
             t1~menge
             t1~mblnr
             t3~zgrp6
             t3~zgrp5 AS zgrp7
             t1~lfbnr
        INTO CORRESPONDING FIELDS OF TABLE lt_mseg
        FROM mseg AS t1 INNER JOIN afko AS t2
                           ON t1~aufnr = t2~aufnr
                        INNER JOIN ztco_mat AS t3
                           ON t2~plnbez = t3~matnr
         FOR ALL ENTRIES IN lt_ztco_mat
       WHERE t1~bwart IN ('261','262')
         AND t1~matnr = lt_ztco_mat-matnr
         AND t1~zbudat IN r_period.
    ENDIF.

    CASE p_zresp.
      WHEN 'V'.
*        SORT lt_mseg BY matnr zgrp3 plnbez.
        SORT lt_mseg BY matnr zgrp3 zgrp6 plnbez.
      WHEN 'E'.
        CLEAR lt_mseg-zgrp3.
        MODIFY lt_mseg TRANSPORTING zgrp3
                       WHERE zgrp3 IS NOT INITIAL.
        SORT lt_mseg BY matnr zgrp6 zgrp7 plnbez.
    ENDCASE.

    CLEAR: gt_color[].
    SELECT *
      INTO TABLE gt_color
      FROM ztco_bulk_pcolor
       FOR ALL ENTRIES IN lt_ztco_mat
     WHERE matnr = lt_ztco_mat-matnr.
    SORT gt_color BY matnr.
  ENDIF.


  LOOP AT lt_ztco_mat.

* Price per UM
    r_matnr-low = lt_ztco_mat-matnr.
    APPEND r_matnr.
***

    CLEAR st_list.

    MOVE-CORRESPONDING lt_ztco_mat TO st_list.
    st_list-bdatj = p_bdatj.
    st_list-poper = p_poper.
    st_list-units = st_list-meins.
    CLEAR st_list-meins.

* Get RP Point
    READ TABLE lt_stpo WITH KEY idnrk = lt_ztco_mat-matnr
                            BINARY SEARCH.
    IF sy-subrc EQ 0.
      st_list-sortf = lt_stpo-sortf.
    ENDIF.

* Exterior Color for Paint Shop
    IF st_list-zresp EQ 'PAINT'.
      READ TABLE gt_color WITH KEY matnr = lt_ztco_mat-matnr BINARY SEARCH.
      IF sy-subrc EQ 0.
        LOOP AT gt_color FROM sy-tabix
                        WHERE matnr = lt_ztco_mat-matnr.
          APPEND gt_color-zgrp2 TO st_list-color.
        ENDLOOP.
      ENDIF.
    ENDIF.


* Get Model and Actual Backflush Data
    READ TABLE lt_mseg WITH KEY matnr = st_list-matnr
                         BINARY SEARCH.
    IF sy-subrc EQ 0.
      LOOP AT lt_mseg FROM sy-tabix
                     WHERE matnr = st_list-matnr.

        AT NEW plnbez.
          APPEND lt_mseg-plnbez TO st_list-fsc.
        ENDAT.

        IF lt_mseg-bwart EQ 261.
          ADD lt_mseg-menge TO st_list-zabfs.
        ELSE.
          SUBTRACT lt_mseg-menge FROM st_list-zabfs.
        ENDIF.

*        st_list-zgrp6 = lt_mseg-zgrp6.  "04.29.2014 Victor

        CASE p_zresp.
          WHEN 'V'.
*            AT END OF zgrp3.
*              st_list-zgrp3 = lt_mseg-zgrp3.
            AT END OF zgrp6.
              st_list-zgrp3 = lt_mseg-zgrp3.
              st_list-zgrp6 = lt_mseg-zgrp6.  "04.29.2014 Victor
              APPEND st_list TO it_list.
              CLEAR: st_list-zabfs, st_list-fsc.
            ENDAT.
          WHEN 'E'.
            AT END OF zgrp7.
              st_list-zgrp6 = lt_mseg-zgrp6.
              st_list-zgrp7 = lt_mseg-zgrp7.
              APPEND st_list TO it_list.
              CLEAR: st_list-zabfs, st_list-fsc.
            ENDAT.
        ENDCASE.

      ENDLOOP.
    ENDIF.

  ENDLOOP.

  SORT it_list BY zgrp3.


*-- Get List of Actual Price per UM
  EXPORT bulk = 'X' TO MEMORY ID 'BULK'.

  SUBMIT zaco11r_ml01_new USING SELECTION-SCREEN '1000' WITH p_kokrs = 'H201'
                                                    WITH p_bdatj = p_bdatj
                                                    WITH s_poper IN r_poper
                                                    WITH s_matnr IN r_matnr
                                                    AND RETURN.
  IMPORT tab = gt_bulk_um FROM MEMORY ID 'BULK_PER_UM'.
  FREE MEMORY ID 'BULK_PER_UM'.

  SORT gt_bulk_um BY matnr.
*****

ENDFORM.                    "get_initial_data
*&---------------------------------------------------------------------*
*&      Form  get_actual_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM get_actual_data.

  DATA: BEGIN OF lt_abispost OCCURS 0,
          matnr  TYPE matnr,
          fsc_matnr  TYPE matnr,
          stype  TYPE zstype,
          mbgbtr TYPE mbgxxx,
        END OF lt_abispost.

  DATA: l_tabix  TYPE stabix,
        l_tabix2 TYPE stabix.
  DATA: lt_bulk_um TYPE TABLE OF zsco_bulk_per_um
                      WITH HEADER LINE.
  DATA: l_fsc TYPE zfsc.
  DATA: l_mbgbtr TYPE mbgxxx.
  DATA: l_peinh  TYPE mbew-peinh. "02.14.2014
  RANGES: r_matnr FOR mara-matnr.


  IF NOT it_list[] IS INITIAL.
*-- for Actual BOM Adj & OS&D
    SELECT matnr fsc_matnr stype mbgbtr
      INTO CORRESPONDING FIELDS OF TABLE lt_abispost
      FROM ztco_abispost
       FOR ALL ENTRIES IN it_list
     WHERE gjahr = p_bdatj
       AND period = p_poper
       AND matnr = it_list-matnr
       AND stype IN ('KEYIN', 'OS&D').
    SORT lt_abispost BY matnr stype.
  ENDIF.

  LOOP AT it_list INTO st_list.
    l_tabix = sy-tabix.

* Get Actual Price per UM
    CLEAR gt_bulk_um.
    READ TABLE gt_bulk_um WITH KEY matnr = st_list-matnr
                               BINARY SEARCH.
    IF sy-subrc EQ 0.
* ZN_LBKUM (GR MM)
* ZN_SALK3 ($GR MMS)
* ZN_RD ($GR MMV)
      st_list-zapum = gt_bulk_um-zn_salk3 + gt_bulk_um-zn_rd.
      IF st_list-zapum NE 0 AND gt_bulk_um-zn_lbkum NE 0.
        st_list-zapum = st_list-zapum / gt_bulk_um-zn_lbkum.
      ENDIF.
    ENDIF.

*    if st_list-zapum is initial.
    IF st_list-zapum <= 0.
      CASE p_zresp.
        WHEN 'V'.
          SELECT SINGLE verpr peinh
            INTO (st_list-zapum, l_peinh)
            FROM mbew
           WHERE matnr = st_list-matnr
             AND bwkey = 'P001'.
        WHEN 'E'.
          SELECT SINGLE verpr peinh
            INTO (st_list-zapum, l_peinh)
            FROM mbew
           WHERE matnr = st_list-matnr
             AND bwkey = 'E001'.
      ENDCASE.

*-<   added on 02.14.2014   calculate per 1 unit
      IF l_peinh > 0.
        st_list-zapum = st_list-zapum / l_peinh.
      ENDIF.
*->

    ENDIF.

* Get Unit Produced
    CASE p_zresp.
      WHEN 'V'.
        PERFORM get_unit_produced USING st_list-zresp
                                        st_list-color
                                        st_list-fsc
                                        st_list-sortf
                               CHANGING st_list-zbiup.
        IF NOT st_list-zabfs IS INITIAL
           AND NOT st_list-zbiup IS INITIAL.
          st_list-zabfs = st_list-zabfs / st_list-zbiup.
        ENDIF.
      WHEN 'E'.

*-- Get Unit Produced for Engine
        EXPORT bulk = 'X' TO MEMORY ID 'BULK'.
        CLEAR: r_matnr, r_matnr[].
        r_matnr = 'IEQ'.
        LOOP AT st_list-fsc INTO l_fsc.
          r_matnr-low = l_fsc.
          APPEND r_matnr.
        ENDLOOP.
        SUBMIT zaco11r_ml01_new USING SELECTION-SCREEN '1000' WITH p_kokrs = 'H201'
                                                          WITH p_bdatj = p_bdatj
                                                          WITH s_poper IN r_poper
                                                          WITH s_matnr IN r_matnr
                                                          AND RETURN.
        IMPORT tab = lt_bulk_um FROM MEMORY ID 'BULK_PER_UM'.
        FREE MEMORY ID 'BULK_PER_UM'.

        LOOP AT lt_bulk_um.
          ADD lt_bulk_um-zf_lbkum TO st_list-zbiup.
        ENDLOOP.

        IF NOT st_list-zabfs IS INITIAL
           AND NOT st_list-zbiup IS INITIAL.
* ZF_LBKUM (GR PP)
          st_list-zabfs = st_list-zabfs / st_list-zbiup.
        ENDIF.
    ENDCASE.

* Get Actual BOM Adj
    READ TABLE lt_abispost WITH KEY matnr = st_list-matnr
                                    stype = 'KEYIN'
                                BINARY SEARCH.
    IF sy-subrc EQ 0.
      l_tabix2 = sy-tabix.
      CLEAR l_mbgbtr.
      LOOP AT st_list-fsc INTO l_fsc.
        LOOP AT lt_abispost FROM l_tabix2 WHERE matnr = st_list-matnr
                                            AND fsc_matnr = l_fsc
                                            AND stype = 'KEYIN'.
          ADD lt_abispost-mbgbtr TO l_mbgbtr.
        ENDLOOP.
      ENDLOOP.
      IF NOT l_mbgbtr IS INITIAL
         AND NOT st_list-zbiup IS INITIAL.
        st_list-zabom = l_mbgbtr / st_list-zbiup.
      ENDIF.
    ENDIF.

* Get Actual OS&D
    READ TABLE lt_abispost WITH KEY matnr = st_list-matnr
                                    stype = 'OS&D'
                                BINARY SEARCH.
    IF sy-subrc EQ 0.
      l_tabix2 = sy-tabix.
      CLEAR l_mbgbtr.
      LOOP AT st_list-fsc INTO l_fsc.
        LOOP AT lt_abispost FROM l_tabix2 WHERE matnr = st_list-matnr
                                            AND fsc_matnr = l_fsc
                                            AND stype = 'OS&D'.
          ADD lt_abispost-mbgbtr TO l_mbgbtr.
        ENDLOOP.
      ENDLOOP.
      IF NOT l_mbgbtr IS INITIAL
         AND NOT st_list-zbiup IS INITIAL.
        st_list-zaosd = l_mbgbtr / st_list-zbiup.
      ENDIF.
    ENDIF.

* Get Actual Total
    st_list-zatot = st_list-zabfs
                  + st_list-zabom
                  + st_list-zaosd.
* Get Amount Per
    st_list-zaapv = st_list-zapum * st_list-zatot.
    MODIFY it_list FROM st_list INDEX l_tabix.

  ENDLOOP.


** Furong on 06/04/14 - if backflush amount is same, no split
  DATA: BEGIN OF lt_list_temp OCCURS 0.
          INCLUDE STRUCTURE zsco_bulk_material.
  DATA: END OF lt_list_temp.
  DATA:  lw_temp LIKE lt_list_temp,
          lw_pre_temp LIKE lt_list_temp,
          l_lines TYPE i.

  SORT it_list BY zgrp3 zresp zgrp1 matnr zabfs zgrp6 DESCENDING .

  IF p_zresp =  'V'.
    LOOP AT it_list.
      MOVE-CORRESPONDING it_list TO lw_temp.
      CLEAR: lw_temp-zbiup, lw_pre_temp-zbiup.
      CLEAR: lw_temp-zgrp6, lw_pre_temp-zgrp6.
      CLEAR: lw_temp-zapum, lw_pre_temp-zapum.
*      clear: lw_temp-zabfs, lw_pre_temp-zabfs.
      CLEAR: lw_temp-zabom, lw_pre_temp-zabom.
      CLEAR: lw_temp-zaosd, lw_pre_temp-zaosd.
      CLEAR: lw_temp-zatot, lw_pre_temp-zatot.
      CLEAR: lw_temp-zaapv, lw_pre_temp-zaapv.
      IF lw_temp = lw_pre_temp.
        DESCRIBE TABLE lt_list_temp LINES l_lines.
        READ TABLE lt_list_temp INDEX l_lines.
        lt_list_temp-zbiup = lt_list_temp-zbiup +  it_list-zbiup.
        CLEAR: lt_list_temp-zgrp6. " lt_list_temp-zgrp7.
        MODIFY lt_list_temp INDEX l_lines TRANSPORTING zgrp6 zbiup.
*      COLLECT lw_temp into LT_LISt_temp.
      ELSE.
        MOVE-CORRESPONDING it_list TO lw_temp.
        APPEND lw_temp TO lt_list_temp.
      ENDIF.
      MOVE-CORRESPONDING it_list TO lw_pre_temp.
    ENDLOOP.

    REFRESH: it_list.
    LOOP AT lt_list_temp.
      MOVE-CORRESPONDING lt_list_temp TO it_list.
      APPEND  it_list.
    ENDLOOP.
    REFRESH: lt_list_temp.
  ENDIF.
** End on 06/04/14

ENDFORM.                    "get_actual_data
*&---------------------------------------------------------------------*
*&      Form  get_target_variance_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM get_target_variance_data.

  DATA: BEGIN OF lt_target OCCURS 0,
          zgrp3   TYPE zdzeinr03,
          zgrp6   TYPE zdzeinr06,
          zgrp7   TYPE zdzeinr07,
          matnr   TYPE matnr,
          zprice  TYPE zprice,
          zbfqty  TYPE zbfqty,
          zbomqty TYPE zbomqty,
          zosdqty TYPE zosdqty,
        END OF lt_target.

  DATA: l_tabix TYPE stabix.
  DATA: l_div(5) TYPE p DECIMALS 4.


  IF NOT it_list[] IS INITIAL.
*-- for Target Data
    SELECT zgrp3
           zgrp6
           zgrp7
           matnr
           zprice
           zbfqty
           zbomqty
           zosdqty
      INTO TABLE lt_target
      FROM ztco_bulk_target
       FOR ALL ENTRIES IN it_list
     WHERE gjahr = p_bdatj
*       and perio = p_poper
*       and zgrp3 = it_list-zgrp3
       AND matnr = it_list-matnr.
    SORT lt_target BY zgrp3 zgrp6 zgrp7 matnr.
  ENDIF.


  LOOP AT it_list INTO st_list.
    l_tabix = sy-tabix.

    CASE p_zresp.
      WHEN 'V'.
        READ TABLE lt_target WITH KEY zgrp3 = st_list-zgrp3
                                      zgrp6 = st_list-zgrp6
                                      matnr = st_list-matnr BINARY SEARCH.

        IF sy-subrc NE 0.
          READ TABLE lt_target WITH KEY zgrp3 = st_list-zgrp3
                                        matnr = st_list-matnr.
        ENDIF.
      WHEN 'E'.
        READ TABLE lt_target WITH KEY zgrp6 = st_list-zgrp6
                                      zgrp7 = st_list-zgrp7
                                      matnr = st_list-matnr BINARY SEARCH.
    ENDCASE.

    IF sy-subrc EQ 0.
* Get Target Price per UM
      st_list-ztpum = lt_target-zprice.
* Get Target Backflush
      st_list-ztbfs = lt_target-zbfqty.
* Get Target BOM Adj
      st_list-ztbom = lt_target-zbomqty.
* Get Target OS&D
      st_list-ztosd = lt_target-zosdqty.
* Get Target Total
      st_list-zttot = st_list-ztbfs
                    + st_list-ztbom
                    + st_list-ztosd.
* Get Target Amount Per Vehicle
      st_list-ztapv = st_list-ztpum * st_list-zttot.
    ENDIF.

****  Variance
* Variance Price per UM
    st_list-zvpum = st_list-zapum - st_list-ztpum.
* Variance Qty per Vehicle
    st_list-zvqty = st_list-zatot - st_list-zttot.
* Variance Amount per Veh ($)
    st_list-zvapv = st_list-zaapv - st_list-ztapv.
* Variance Amount per Veh (%)
    IF NOT st_list-zbiup IS INITIAL.
      IF NOT st_list-zvapv IS INITIAL
         AND NOT st_list-ztapv IS INITIAL.
        l_div = st_list-zvapv / st_list-ztapv.
        st_list-zvapp = l_div * 100.
      ENDIF.
    ENDIF.

* Price
    st_list-zbipr = st_list-zbiup * st_list-zvpum * st_list-zatot.
* Usage
    st_list-zbiug = st_list-zbiup * st_list-zvqty * st_list-ztpum.
* Total
    st_list-zbtot = st_list-zbipr + st_list-zbiug.

    MODIFY it_list FROM st_list INDEX l_tabix.

  ENDLOOP.

ENDFORM.                    "get_target_variance_data
*&---------------------------------------------------------------------*
*&      Form  set_droplist_shop
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM set_droplist_shop.

  DATA: lt_values     TYPE vrm_values,
        ls_value      TYPE LINE OF vrm_values,
        l_fieldname   TYPE vrm_id.
  DATA: lt_values_tab TYPE TABLE OF dd07v WITH HEADER LINE.

  l_fieldname = 'P_ZRESP'.

  ls_value-key = 'V'.
  ls_value-text = 'Vehicle'.
  APPEND ls_value TO lt_values.

  ls_value-key = 'E'.
  ls_value-text = 'Engine'.
  APPEND ls_value TO lt_values.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = l_fieldname
      values = lt_values.

ENDFORM.                    "set_droplist_shop
*&---------------------------------------------------------------------*
*&      Form  get_unit_produced
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ZRESP    text
*      -->P_FSC      text
*      -->P_CODE     text
*      -->C_UNIT_P   text
*----------------------------------------------------------------------*
FORM get_unit_produced USING p_zresp
                             pt_color TYPE tt_color
                             pt_fsc TYPE tt_fsc
                             p_code
                    CHANGING c_unit_p.

  DATA: lt_wosum TYPE TABLE OF ty_wosum,
        st_wosum TYPE ty_wosum.
  DATA: lt_atwrt TYPE TABLE OF ty_atwrt,
        st_atwrt TYPE ty_atwrt.

  DATA: l_tabix TYPE stabix.

  DATA: ls_fsc TYPE ty_fsc,
        ls_color TYPE ty_color.

  DATA: l_work_atwrt TYPE atwrt.

  CLEAR c_unit_p.

  READ TABLE gt_wosum WITH KEY sortf = p_code BINARY SEARCH.
  IF sy-subrc NE 0.
    PERFORM get_erp_wo_qty_summary USING p_zresp
                                         p_code.
    READ TABLE gt_wosum WITH KEY sortf = p_code BINARY SEARCH.
  ENDIF.

  IF sy-subrc EQ 0.
    lt_wosum = gt_wosum-wosum.
    lt_atwrt = gt_wosum-atwrt.

    IF p_zresp EQ 'PAINT' AND NOT pt_color[] IS INITIAL.

      LOOP AT pt_color INTO ls_color.
        LOOP AT pt_fsc INTO ls_fsc.
          READ TABLE lt_wosum INTO st_wosum WITH KEY fsc = ls_fsc
                                                 BINARY SEARCH.
          IF sy-subrc EQ 0.
            l_tabix = sy-tabix.
            LOOP AT lt_wosum INTO st_wosum FROM l_tabix
                                          WHERE fsc = ls_fsc
                                            AND extc = ls_color-color.

              CONCATENATE st_wosum-wo_ser st_wosum-nation st_wosum-dealer
                     INTO l_work_atwrt.

              READ TABLE lt_atwrt INTO st_atwrt WITH KEY work_atwrt = l_work_atwrt
                                                         ext_atwrt = st_wosum-extc
                                                         int_atwrt = st_wosum-intc
                                                     BINARY SEARCH.
              IF sy-subrc EQ 0.
                l_tabix = sy-tabix.
                LOOP AT lt_atwrt INTO st_atwrt FROM l_tabix WHERE work_atwrt = l_work_atwrt
                                                              AND ext_atwrt = st_wosum-extc
                                                              AND int_atwrt = st_wosum-intc.
                  ADD 1 TO c_unit_p.
                ENDLOOP.
              ENDIF.
            ENDLOOP.
          ENDIF.
        ENDLOOP.
      ENDLOOP.

    ELSE.
      LOOP AT pt_fsc INTO ls_fsc.
        READ TABLE lt_wosum INTO st_wosum WITH KEY fsc = ls_fsc
                                               BINARY SEARCH.
        IF sy-subrc EQ 0.
          l_tabix = sy-tabix.
          LOOP AT lt_wosum INTO st_wosum FROM l_tabix
                                        WHERE fsc = ls_fsc.

            CONCATENATE st_wosum-wo_ser st_wosum-nation st_wosum-dealer
                   INTO l_work_atwrt.

            READ TABLE lt_atwrt INTO st_atwrt WITH KEY work_atwrt = l_work_atwrt
                                                       ext_atwrt = st_wosum-extc
                                                       int_atwrt = st_wosum-intc
                                                   BINARY SEARCH.
            IF sy-subrc EQ 0.
              l_tabix = sy-tabix.
              LOOP AT lt_atwrt INTO st_atwrt FROM l_tabix WHERE work_atwrt = l_work_atwrt
                                                            AND ext_atwrt = st_wosum-extc
                                                            AND int_atwrt = st_wosum-intc.
                ADD 1 TO c_unit_p.
              ENDLOOP.
            ENDIF.
          ENDLOOP.
        ENDIF.
      ENDLOOP.

    ENDIF.
  ENDIF.

ENDFORM.                    "get_unit_produced
*&---------------------------------------------------------------------*
*&      Form  get_erp_wo_qty_summary
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_CODE     text
*----------------------------------------------------------------------*
FORM get_erp_wo_qty_summary USING p_zresp TYPE zdfiresp
                                  p_code  TYPE sortp.

  DATA: lt_value TYPE TABLE OF zsca_char_value WITH HEADER LINE,
        lt_vehicle TYPE TABLE OF zsca_vehicle_char_value WITH HEADER LINE,
        lt_condition TYPE TABLE OF zsca_characteristic_value WITH HEADER LINE.

  DATA: l_atnam TYPE atnam,
        l_atwrt_s TYPE atwrt,
        l_atwrt_e TYPE atwrt,
        l_count TYPE i.

  DATA: lt_atwrt TYPE TABLE OF ty_atwrt WITH HEADER LINE,
        lt_wosum TYPE TABLE OF ty_wosum WITH HEADER LINE.

  DATA: l_tabix TYPE stabix.

  CONCATENATE 'P_RP' p_code '_SHOP_DATE' INTO l_atnam.

  lt_value-atnam = 'P_WORK_ORDER'.
  APPEND lt_value.
  lt_value-atnam = 'P_EXT_COLOR'.
  APPEND lt_value.
  lt_value-atnam = 'P_INT_COLOR'.
  APPEND lt_value.

  READ TABLE r_period INDEX 1.
  l_atwrt_s = r_period-low.
  l_atwrt_e = r_period-high.
  IF l_atwrt_e IS INITIAL.
    l_atwrt_e = l_atwrt_s.
  ENDIF.

  CALL FUNCTION 'Z_FCA_GET_VEHICLE_MASTER'
    EXPORTING
      i_atnam     = l_atnam
      i_atwrt_s   = l_atwrt_s
      i_atwrt_e   = l_atwrt_e
    TABLES
      t_condition = lt_condition
      t_value     = lt_value
      t_vehicle   = lt_vehicle.


  LOOP AT lt_vehicle WHERE atnam NE l_atnam.
    CASE lt_vehicle-atnam.
      WHEN 'P_WORK_ORDER'.
        l_count = 1.
        lt_atwrt-work_atwrt = lt_vehicle-atwrt.
      WHEN 'P_EXT_COLOR'.
        ADD 1 TO l_count.
        lt_atwrt-ext_atwrt = lt_vehicle-atwrt.
      WHEN 'P_INT_COLOR'.
        ADD 1 TO l_count.
        lt_atwrt-int_atwrt = lt_vehicle-atwrt.
    ENDCASE.

    IF l_count EQ 3.
      APPEND lt_atwrt. CLEAR lt_atwrt.
    ENDIF.
  ENDLOOP.


  CHECK NOT lt_atwrt[] IS INITIAL.
  SORT lt_atwrt BY work_atwrt ext_atwrt int_atwrt.

  SELECT fsc wo_ser nation
         dealer extc intc
    INTO CORRESPONDING FIELDS OF TABLE lt_wosum
    FROM ztpp_wosum
     FOR ALL ENTRIES IN lt_atwrt
   WHERE wo_ser = lt_atwrt-work_atwrt+0(9)
     AND nation = lt_atwrt-work_atwrt+9(3)
     AND dealer = lt_atwrt-work_atwrt+12(2)
     AND extc = lt_atwrt-ext_atwrt+0(3)
     AND intc = lt_atwrt-int_atwrt+0(3).
  SORT lt_wosum BY fsc wo_ser nation dealer extc intc.

*  endif.

  gt_wosum-sortf = p_code.
  gt_wosum-wosum = lt_wosum[].
  gt_wosum-atwrt = lt_atwrt[].
  APPEND gt_wosum.
  SORT gt_wosum BY sortf.

ENDFORM.                    "get_erp_wo_qty_summary
*&---------------------------------------------------------------------*
*&      Form  SET_YEAR_QUATER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_year_quater.

  r_period+0(3) = 'IBT'.

  r_period-low+0(4) = p_bdatj.
  r_period-low+4(2) = p_poper.
  r_period-low+6(2) = '01'.
  PERFORM conversion_exit_alpha_input CHANGING r_period-low+4(2).
* For test
*  r_period-high = r_period-low.
  PERFORM get_last_date_of_the_month USING r_period-low
                                  CHANGING r_period-high.
  APPEND r_period.

ENDFORM.                    " SET_YEAR_QUATER
*&---------------------------------------------------------------------*
*&      Form  SAVE_DATA_TO_TABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM save_data.

  DATA: l_tabix TYPE stabix.

  DATA: lt_bulk_bw_v TYPE TABLE OF ztpp_bulk_bw_v
                                   WITH HEADER LINE,
        lt_bulk_bw_e TYPE TABLE OF ztpp_bulk_bw_e
                                   WITH HEADER LINE.

*  data l_valid type char01.
*
*  call method g_alvgrd->check_changed_data
*    importing
*      e_valid = l_valid.

  LOOP AT it_list INTO st_list.
    CASE p_zresp.
      WHEN 'V'.
        MOVE-CORRESPONDING st_list TO lt_bulk_bw_v.
        lt_bulk_bw_v-meins = st_list-units.
        lt_bulk_bw_v-waers = 'USD'.
        APPEND lt_bulk_bw_v.
      WHEN 'E'.
        MOVE-CORRESPONDING st_list TO lt_bulk_bw_e.
        lt_bulk_bw_e-meins = st_list-units.
        lt_bulk_bw_e-waers = 'USD'.
        APPEND lt_bulk_bw_e.
    ENDCASE.
  ENDLOOP.

  CASE p_zresp.
    WHEN 'V'.
      DELETE FROM ztpp_bulk_bw_v WHERE bdatj = p_bdatj
                                    AND poper = p_poper.
      MODIFY ztpp_bulk_bw_v FROM TABLE lt_bulk_bw_v.
    WHEN 'E'.
      DELETE FROM ztpp_bulk_bw_e WHERE bdatj = p_bdatj
                                    AND poper = p_poper.
      MODIFY ztpp_bulk_bw_e FROM TABLE lt_bulk_bw_e.
  ENDCASE.

  IF sy-subrc EQ 0.
    MESSAGE s009.
  ENDIF.

ENDFORM.                    "SAVE_DATA_TO_TABLE
*&---------------------------------------------------------------------*
*&      Form  create_custom_container
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->PI_CONT_NAME  text
*      -->PR_CUCTNR     text
*      -->PR_ALVGRD     text
*----------------------------------------------------------------------*
FORM z_create_custom_container USING pi_cont_name
                                   pr_cuctnr TYPE REF TO cl_gui_custom_container
                                   pr_alvgrd TYPE REF TO lcl_custom_alv_grid.

  CREATE OBJECT pr_cuctnr
    EXPORTING
      container_name              = pi_cont_name
    EXCEPTIONS
      cntl_error                  = 1
      cntl_system_error           = 2
      create_error                = 3
      lifetime_error              = 4
      lifetime_dynpro_dynpro_link = 5
      OTHERS                      = 6.

  IF sy-subrc NE 0.
    CALL FUNCTION 'POPUP_TO_INFORM'
      EXPORTING
        titel = sy-repid
        txt2  = sy-subrc
        txt1  = 'The control could not be created'(510).
  ENDIF.

*.. Create an instance of alv control
  CREATE OBJECT pr_alvgrd
    EXPORTING
      i_parent      = pr_cuctnr
      i_appl_events = 'X'.

ENDFORM.                    "create_custom_container
*&---------------------------------------------------------------------*
*&      Form  z_display_alv_grid
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
FORM z_display_alv_grid   USING ps_layout TYPE lvc_s_layo
                              pt_exclu  TYPE ui_functions
                              pr_alvgrd TYPE REF TO lcl_custom_alv_grid
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

ENDFORM.                    "z_display_alv_grid
