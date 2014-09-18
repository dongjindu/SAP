*&---------------------------------------------------------------------*
*&  Include           ZRFI_OPEN_POF01
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
      PERFORM build_sort_value USING: pr_sort 'EBELN' 1 '0' 'X'.
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
          i_structure_name   = 'ZSFI_OPEN_PO'
        CHANGING
          ct_fieldcat        = et_fldcat[].
  ENDCASE.


  LOOP AT et_fldcat INTO ls_fdcat.
    l_tabix = sy-tabix.

    CASE ls_fdcat-fieldname.
      WHEN 'BUKRS'.
        ls_fdcat-key = 'X'.
      WHEN 'BSTYP'.
        ls_fdcat-reptext = 'Purchase Order Category'.
        ls_fdcat-key = 'X'.
      WHEN 'EBELN'.
        ls_fdcat-reptext = 'Purchase Order No.'.
        ls_fdcat-key = 'X'.
      WHEN 'EBELP'.
        ls_fdcat-reptext = 'Line Item'.
        ls_fdcat-key = 'X'.
      WHEN 'STUNR'.
        ls_fdcat-reptext = 'Condition (Step Number)'.
      WHEN 'LIFNR'.
        ls_fdcat-reptext = 'Vendor'.
      WHEN 'FDDBT'.
        ls_fdcat-reptext = 'Open Amount'.
        ls_fdcat-do_sum  = 'X'.
      WHEN 'AEDAT'.
        ls_fdcat-reptext = 'Purchase Order created on'.
      WHEN 'GMENGE'.
        ls_fdcat-reptext = 'GR Qty'.
      WHEN 'GWRBTR'.
        ls_fdcat-reptext = 'GR Amount'.
      WHEN 'GRATE'.
        ls_fdcat-reptext = 'GR Compl Rate'.
      WHEN 'GLDATE'.
        ls_fdcat-reptext = 'Last GR On'.
      WHEN 'IMENGE'.
        ls_fdcat-reptext = 'IR Qty'.
      WHEN 'IWRBTR'.
        ls_fdcat-reptext = 'IR Amount'.
      WHEN 'IRATE'.
        ls_fdcat-reptext = 'IV Compl Rate'.
      WHEN 'ILDATE'.
        ls_fdcat-reptext = 'Last IR On'.
      WHEN 'AMENGE'.
        ls_fdcat-reptext = 'Acct.Maint. Qty'.
      WHEN 'ALDATE'.
        ls_fdcat-reptext = 'Last Acct.Maint. On'.
      WHEN 'MEINS'.
        ls_fdcat-reptext = 'Unit'.
      WHEN 'KNTTP'.
        ls_fdcat-reptext = 'Account Assignment Category'.
      WHEN 'POAGS'.
        ls_fdcat-reptext = 'PO Created Days as on'.
      WHEN 'NETWR'.
        ls_fdcat-reptext = 'PO Amount'.
      WHEN 'PSTYP'.
        ls_fdcat-reptext = 'PO Type'.
      WHEN ''.
        ls_fdcat-no_out = 'X'.
      WHEN OTHERS.
    ENDCASE.

    ls_fdcat-scrtext_l = ls_fdcat-reptext.
    ls_fdcat-scrtext_m = ls_fdcat-reptext.
    ls_fdcat-scrtext_s = ls_fdcat-reptext.

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

  p_bukrs = 'H201'.
  s_bstyp = 'IEQ'.
  s_bstyp-low = 'F'.
  APPEND s_bstyp.

ENDFORM.                    "INITIALIZATION
*&---------------------------------------------------------------------*
*&      Form  get_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM get_data.

  DATA: l_tabix TYPE stabix.
  DATA: lt_ekbe TYPE TABLE OF ekbe WITH HEADER LINE,
        lt_konv TYPE TABLE OF konv WITH HEADER LINE.

  DATA: l_grate TYPE p DECIMALS 2,
         l_irate TYPE p DECIMALS 2 .

  SELECT t1~bukrs t1~bstyp t1~ebeln t1~ebelp
         t1~lifnr t1~fddbt t2~aedat
         t1~stunr t2~knumv t1~zaehk
         t3~matnr t3~txz01 t3~menge
         t3~meins t3~knttp t2~bedat
** Changed on 04/23/14 (
         t3~netwr t3~pstyp t3~knttp
** )
    INTO CORRESPONDING FIELDS OF TABLE it_list
    FROM fdm1 AS t1 INNER JOIN ekko AS t2
                       ON t1~ebeln = t2~ebeln
                    INNER JOIN ekpo AS t3
                       ON t1~ebeln = t3~ebeln
                      AND t1~ebelp = t3~ebelp
   WHERE t1~ebeln IN s_ebeln
     AND t1~ebelp IN s_ebelp
     AND t1~bstyp IN s_bstyp
     AND t2~aedat IN s_aedat
     and knttp in s_knttp.


  IF NOT it_list[] IS INITIAL.
    SELECT *
      INTO CORRESPONDING FIELDS OF TABLE lt_ekbe
      FROM ekbe
       FOR ALL ENTRIES IN it_list
     WHERE ebeln = it_list-ebeln
       AND ebelp = it_list-ebelp.
    SORT lt_ekbe BY ebeln ebelp bewtp.

*    select *
*      into corresponding fields of table lt_konv
*      from konv
*       for all entries in it_list
*     where knumv = it_list-knumv.
**       and stunr = it_list-stunr
**       and zaehk = it_list-zaehk.
*    sort lt_konv by knumv kposn stunr zaehk.
  ENDIF.


  LOOP AT it_list INTO st_list.
    CLEAR: l_grate, l_irate.

    l_tabix = sy-tabix.

    SELECT SINGLE kschl
      INTO st_list-kschl
      FROM konv
     WHERE knumv = st_list-knumv
       AND kposn = st_list-ebelp
       AND stunr = st_list-stunr
       AND zaehk = st_list-zaehk.

*    read table lt_konv with key knumv = st_list-knumv
*                                kposn = st_list-ebelp
*                                stunr = st_list-stunr
*                                zaehk = st_list-zaehk binary search.
*    if sy-subrc eq 0.
*      st_list-kschl = lt_konv-kschl.
*    endif.

    IF st_list-kschl IS INITIAL.
      READ TABLE lt_ekbe WITH KEY ebeln = st_list-ebeln
                                  ebelp = st_list-ebelp BINARY SEARCH.
*     GR Qty
      LOOP AT lt_ekbe FROM sy-tabix WHERE ebeln = st_list-ebeln
                        AND ebelp = st_list-ebelp
                        AND bewtp = 'E'.
        IF st_list-gldate < lt_ekbe-budat.
          st_list-gldate = lt_ekbe-budat.
        ENDIF.
        CASE lt_ekbe-shkzg.
          WHEN 'S'.
            ADD lt_ekbe-menge TO st_list-gmenge.
** Changed on 04/23/14 (
            ADD lt_ekbe-wrbtr TO st_list-gwrbtr.
** )
          WHEN 'H'.
            SUBTRACT lt_ekbe-menge FROM st_list-gmenge.
** Changed on 04/23/14 (
            SUBTRACT lt_ekbe-wrbtr FROM st_list-gwrbtr.
** )
        ENDCASE.
      ENDLOOP.
*     IR Qty
      LOOP AT lt_ekbe FROM sy-tabix WHERE ebeln = st_list-ebeln
                        AND ebelp = st_list-ebelp
                        AND bewtp = 'Q'.
        IF st_list-ildate < lt_ekbe-budat.
          st_list-ildate = lt_ekbe-budat.
        ENDIF.
        CASE lt_ekbe-shkzg.
          WHEN 'S'.
            ADD lt_ekbe-menge TO st_list-imenge.
** Changed on 04/23/14 (
            ADD lt_ekbe-wrbtr TO st_list-iwrbtr.
** )
          WHEN 'H'.
            SUBTRACT lt_ekbe-menge FROM st_list-imenge.
** Changed on 04/23/14 (
            SUBTRACT lt_ekbe-wrbtr FROM st_list-iwrbtr.
** )
        ENDCASE.
      ENDLOOP.
*     Acct.Maint.Qty
      LOOP AT lt_ekbe FROM sy-tabix WHERE ebeln = st_list-ebeln
                        AND ebelp = st_list-ebelp
                        AND bewtp = 'K'.
        IF st_list-aldate < lt_ekbe-budat.
          st_list-aldate = lt_ekbe-budat.
        ENDIF.
        CASE lt_ekbe-shkzg.
          WHEN 'S'.
            ADD lt_ekbe-menge TO st_list-amenge.
          WHEN 'H'.
            SUBTRACT lt_ekbe-menge FROM st_list-amenge.
        ENDCASE.
      ENDLOOP.
    ENDIF.

    st_list-poags = sy-datum - st_list-bedat.

** Changed on 04/23/14 (
    IF st_list-netwr <> 0.
      l_grate =  ( st_list-netwr - st_list-gwrbtr ) / st_list-netwr.
      l_irate =  ( st_list-netwr - st_list-iwrbtr ) / st_list-netwr.
    ENDIF.
    IF l_irate < p_irate.
      DELETE it_list INDEX l_tabix.
    ELSE.
      it_list-grate = l_grate.
      it_list-irate = l_irate.
      MODIFY it_list FROM st_list INDEX l_tabix.
    ENDIF.

*    modify it_list from st_list index l_tabix.
** )

  ENDLOOP.

ENDFORM.                    "get_data
*&---------------------------------------------------------------------*
*&      Form  set_selection_screen
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM set_selection_screen.

  LOOP AT SCREEN.
    IF screen-group1 = 'MAN'.
      screen-input = 0.
      screen-intensified = 1.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

ENDFORM.                    "set_selection_screen
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
