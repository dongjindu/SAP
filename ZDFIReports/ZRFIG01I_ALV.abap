*----------------------------------------------------------------------*
*   INCLUDE ZRFIG01I_ALV                                               *
*----------------------------------------------------------------------*
*TYPE-POOLS: slis.
*INCLUDE <icon>.
*INCLUDE <symbol>.
*CLASS cl_gui_resources DEFINITION LOAD.
CONSTANTS:
  c_f2code               LIKE sy-ucomm                    VALUE '&ETA'.


DATA: gt_fieldcat TYPE slis_t_fieldcat_alv,
      gs_layout   TYPE slis_layout_alv,
      gt_sp_group TYPE slis_t_sp_group_alv,
      gt_events   TYPE slis_t_event,
      gt_sorts    TYPE slis_t_sortinfo_alv WITH HEADER LINE,
      gs_prnt     TYPE slis_print_alv.

DATA: wa_repid LIKE sy-repid,
      wa_var_save(1) TYPE c             VALUE  'A',
      wa_default(1)  TYPE c,
      wa_exit(1) TYPE c,
      wa_variant LIKE disvariant,
      wa_var LIKE disvariant,
      wa_alv_function_name(30) TYPE c VALUE 'REUSE_ALV_GRID_LIST',
      wa_alv_get_info_name(40) TYPE c.

*---------------------------------------------------------------------*
*  FORM alv_event_pf_status_set
*---------------------------------------------------------------------*
FORM alv_event_pf_status_set USING rt_extab TYPE slis_t_extab.
                                                            "#EC *
  SET PF-STATUS 'STANDARD' EXCLUDING rt_extab.
  SET TITLEBAR  'STANDARD'.

ENDFORM.                    "alv_event_pf_status_set
*---------------------------------------------------------------------*
*  FORM alv_event_user_command
*---------------------------------------------------------------------*
FORM alv_event_user_command USING r_ucomm     LIKE sy-ucomm
                                      rs_selfield TYPE slis_selfield.
                                                            "#EC *
  CASE r_ucomm.
*   ---------------------------------- processing on double click.
    WHEN '&IC1'.
      READ TABLE it_bkpf INDEX rs_selfield-tabindex.
      SET PARAMETER ID 'GJR' FIELD it_bkpf-gjahr.
      SET PARAMETER ID 'BLN' FIELD it_bkpf-belnr.
      SET PARAMETER ID 'BUK' FIELD it_bkpf-bukrs.
      IF NOT r_a IS INITIAL.
        CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
*Issue Number : FI-20040729-001, Requested by GHLEE
*Changed on 2004/08/25, by WSKIM
*---Start
      ELSEIF NOT r_b IS INITIAL.
       CALL TRANSACTION 'KB13N' AND SKIP FIRST SCREEN.
      ENDIF.
*---End
*   ---------------------------------- Write Account Doc.s
    WHEN '&PRT'.
      LOOP AT it_bkpf WHERE chkbox EQ 'X'.
      ENDLOOP.
      IF sy-subrc EQ 0.
        rs_selfield-exit = 'X'.
*       PERFORM write_documents.
      ELSE.
        MESSAGE i000 WITH 'First, You have to check in list'.

      ENDIF.

*     perform Write_Documents.
*   ---------------------------------- switching view type grid or list
    WHEN 'LIST' OR 'GRID'.
      PERFORM switch_list_or_grid USING r_ucomm.



* + ig.moon 4/6/2009 {
    WHEN 'PRI_BAR'.

      LOOP AT it_bkpf WHERE chkbox EQ 'X'.
      ENDLOOP.
      IF sy-subrc EQ 0.
        rs_selfield-exit = 'X'.
*       PERFORM write_documents.
      ELSE.
        MESSAGE i000 WITH 'First, You have to check in list'.

      ENDIF.

* }
  ENDCASE.

  CHECK r_ucomm EQ 'LIST' OR
        r_ucomm EQ 'GRID'.


  rs_selfield-exit = 'X'.

ENDFORM.                    "alv_event_user_command
*&---------------------------------------------------------------------
*&      Form  set_variant
*&---------------------------------------------------------------------
*FORM set_variant CHANGING cs_vari TYPE disvariant.
*
*  CHECK p_layout NE space.
*
*  cs_vari-report      = sy-repid.
*  cs_vari-handle      = space.
*  cs_vari-log_group   = space.
*  cs_vari-username    = space.
*  cs_vari-variant     = p_layout.
*  cs_vari-text        = space.
*  cs_vari-dependvars  = space.
*
*ENDFORM.                    " set_variant

*&---------------------------------------------------------------------
*&      Form  set_events
*&---------------------------------------------------------------------
FORM set_events CHANGING ct_events TYPE slis_t_event.

  FIELD-SYMBOLS: <ls_event> TYPE slis_alv_event.

  DATA: l_event TYPE lvc_fname.

  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
       EXPORTING
            i_list_type     = 0
       IMPORTING
            et_events       = ct_events
       EXCEPTIONS
            list_type_wrong = 1
            OTHERS          = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    DELETE ct_events WHERE name NE 'END_OF_PAGE'
                       AND name NE 'TOP_OF_PAGE'
                       AND name NE 'TOP_OF_LIST'
                       AND name NE 'END_OF_LIST'.
    LOOP AT ct_events ASSIGNING <ls_event>.
      CONCATENATE 'ALV_EVENT_'
                  <ls_event>-name
                  INTO <ls_event>-form.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " f01_set_evts


*&---------------------------------------------------------------------
*&      Form  set_layout
*&---------------------------------------------------------------------
FORM set_layout CHANGING cs_layo TYPE slis_layout_alv.

*... Display options
  cs_layo-colwidth_optimize      = space. "'X'. "==> ???? ???
  "?????
  cs_layo-no_colhead             = space.
  cs_layo-no_hotspot             = space.
  cs_layo-zebra                  = ' '. "==> ???? ??
  cs_layo-no_vline               = space.
  cs_layo-cell_merge             = space.
  cs_layo-no_min_linesize        = space.
  cs_layo-min_linesize           = space.
  cs_layo-max_linesize           = space.
  cs_layo-window_titlebar        = space.
  cs_layo-no_uline_hs            = space.
*... Edit
  cs_layo-edit                   = ' '."space.
  cs_layo-edit_mode              = ' '."space.
*... Exceptions
  cs_layo-lights_fieldname       = ' '.
  "=> ??? ??? ???
  cs_layo-lights_tabname         = space.
  cs_layo-lights_rollname        = space.
  cs_layo-lights_condense        = space.
*... Sums
  cs_layo-no_sumchoice           = space.
  cs_layo-no_totalline           = space.
  cs_layo-totals_before_items    = space.
  cs_layo-totals_only            = space.
  cs_layo-totals_text            = space.
  cs_layo-no_subchoice           = space.
  cs_layo-no_subtotals           = space.
  cs_layo-subtotals_text         = space.
  cs_layo-numc_sum               = 'X'.
  cs_layo-no_unit_splitting      = space.
*... Interaction
  cs_layo-box_fieldname          = 'CHKBOX'.
  cs_layo-box_tabname            = space.
  cs_layo-box_rollname           = space.
  cs_layo-expand_fieldname       = space.
  cs_layo-hotspot_fieldname      = space.
  cs_layo-no_input               = ' '.
  cs_layo-f2code                 = space.
  cs_layo-confirmation_prompt    = space.
  cs_layo-key_hotspot            = space.
  cs_layo-flexible_key           = space.
  cs_layo-reprep                 = space.
  cs_layo-group_buttons          = 'X'.
  cs_layo-no_keyfix              = space.
  cs_layo-get_selinfos           = space.
  cs_layo-group_change_edit      = 'X'.
  cs_layo-no_scrolling           = space.
  cs_layo-expand_all             = space.
  cs_layo-no_author              = space.
*... Detailed screen
  cs_layo-detail_popup           = 'X'.
  cs_layo-detail_initial_lines   = space.
  cs_layo-detail_titlebar        = space.
*... PF-status
  cs_layo-def_status             = space.
*... Display variants
  cs_layo-header_text            = space.
  cs_layo-item_text              = space.
  cs_layo-default_item           = space.
*... colour
  cs_layo-info_fieldname         = space.
  cs_layo-coltab_fieldname       = 'TABCOLOR'.
*... others
  cs_layo-list_append            = space.

ENDFORM.                    " set_layout


*---------------------------------------------------------------------*
*  FORM f01_alv_event_top_of_page
*---------------------------------------------------------------------*
FORM alv_event_top_of_page.                                 "#EC CALLED

ENDFORM.                    "alv_event_top_of_page

*---------------------------------------------------------------------*
*       FORM alv_event_top_of_LIST                                    *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM alv_event_top_of_list.                                 "#EC CALLED


ENDFORM.                    "alv_event_top_of_page

*---------------------------------------------------------------------*
*  FORM f01_alv_event_end_of_page
*---------------------------------------------------------------------*
FORM alv_event_end_of_page.
  NEW-LINE.
  ULINE.
  DATA: l_page(10).
  WRITE : sy-pagno TO l_page.
  WRITE: /(120) l_page CENTERED.

ENDFORM.                    "alv_event_end_of_page

*---------------------------------------------------------------------*
*  FORM f01_alv_event_end_of_list
*---------------------------------------------------------------------*
FORM alv_event_end_of_list.


ENDFORM.                    "alv_event_end_of_list

*&---------------------------------------------------------------------
*&      Form  switch_list_or_grid
*&---------------------------------------------------------------------
FORM switch_list_or_grid USING r_ucomm.

  DATA: ls_vari      TYPE disvariant,
       ls_slis_layo TYPE slis_layout_alv,
       lt_slis_fcat TYPE slis_t_fieldcat_alv,
       lt_slis_sort TYPE slis_t_sortinfo_alv,
       lt_slis_filt TYPE slis_t_filter_alv,
       ls_slis_prnt TYPE slis_print_alv.


  IF r_ucomm = 'LIST' AND
     wa_alv_function_name = 'REUSE_ALV_LIST_DISPLY'.
    EXIT.
  ENDIF.
  IF r_ucomm = 'GRID' AND
     wa_alv_function_name = 'REUSE_ALV_GRID_DISPLAY'.
    EXIT.
  ENDIF.
  CASE wa_alv_function_name.
    WHEN 'REUSE_ALV_LIST_DISPLAY'.
      wa_alv_get_info_name = 'REUSE_ALV_LIST_LAYOUT_INFO_GET'.
    WHEN 'REUSE_ALV_GRID_DISPLAY'.
      wa_alv_get_info_name = 'REUSE_ALV_GRID_LAYOUT_INFO_GET'.

  ENDCASE.

  CALL FUNCTION wa_alv_get_info_name
       IMPORTING
            es_layout     = ls_slis_layo
            et_fieldcat   = lt_slis_fcat
            et_sort       = lt_slis_sort
            et_filter     = lt_slis_filt
            es_variant    = ls_vari
       EXCEPTIONS
            no_infos      = 1
            program_error = 2
            OTHERS        = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
  IF r_ucomm = 'LIST'.
    wa_alv_function_name = 'REUSE_ALV_LIST_DISPLAY'.
    CALL FUNCTION wa_alv_function_name
         EXPORTING
              i_callback_program       = wa_repid
              i_callback_pf_status_set = 'ALV_EVENT_PF_STATUS_SET'
              i_callback_user_command  = 'ALV_EVENT_USER_COMMAND'
              is_layout                = ls_slis_layo
              it_fieldcat              = lt_slis_fcat
              it_sort                  = lt_slis_sort
              it_filter                = lt_slis_filt
              i_default                = ' '  "gs_test-vari_default
              i_save                   = wa_var_save
              is_variant               = ls_vari
              is_print                 = ls_slis_prnt
              it_events                = gt_events[]
         TABLES
              t_outtab                 = it_bkpf
         EXCEPTIONS
              program_error            = 1
              OTHERS                   = 2.
  ENDIF.
  IF r_ucomm = 'GRID'.
    wa_alv_function_name = 'REUSE_ALV_GRID_DISPLAY'.
    CALL FUNCTION wa_alv_function_name
         EXPORTING
              i_callback_program       = wa_repid
              i_callback_pf_status_set = 'ALV_EVENT_PF_STATUS_SET'
              i_callback_user_command  = 'ALV_EVENT_USER_COMMAND'
              is_layout                = ls_slis_layo
              it_fieldcat              = lt_slis_fcat
              it_sort                  = lt_slis_sort
              it_filter                = lt_slis_filt
              i_default                = ' '  "gs_test-vari_default
              i_save                   = wa_var_save
              is_variant               = ls_vari
              is_print                 = ls_slis_prnt
*                it_events               = gt_events[]
*                ?????? ??? ??? ??

         TABLES
              t_outtab                 = it_bkpf
         EXCEPTIONS
              program_error            = 1
              OTHERS                   = 2.

  ENDIF.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.


ENDFORM.                    " switch_list_or_grid
*&--------------------------------------------------------------------
*&      Form  make_field_category
*&--------------------------------------------------------------------
FORM build_field_category USING
                                  p_fieldname       " ????
                                  p_title           " ??????
                                  p_outputlen       " ????
                                  p_key             " ???
                                  p_just            " ??
                                  p_noout           " ??????
                                  p_edit            " ????
                                  p_cfield          " currency field nam
                                  p_qfield          " quantity field nam
                                  .

  DATA: ls_fieldcat TYPE slis_fieldcat_alv.
  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = p_fieldname.
*  ls_fieldcat-seltext_s = p_title.
*  ls_fieldcat-seltext_m = p_title.
  ls_fieldcat-seltext_l = p_title.
  ls_fieldcat-outputlen = p_outputlen.
  ls_fieldcat-key       = p_key.
  ls_fieldcat-just      = p_just.
  ls_fieldcat-edit      = p_edit.
  ls_fieldcat-no_out     = p_noout.
  ls_fieldcat-cfieldname = p_cfield.
  ls_fieldcat-qfieldname = p_qfield.
*  if p_fieldname = 'KUNNR'.
*    ls_fieldcat-emphasize = 'C100'.
*  endif.
  APPEND ls_fieldcat TO gt_fieldcat.

ENDFORM.                    " fill_field_category

*&---------------------------------------------------------------------*
*&      Form  f4_variant
*&---------------------------------------------------------------------*
FORM f4_variant CHANGING c_variant TYPE disvariant-variant.

  DATA: ls_variant TYPE disvariant,
        l_exit     TYPE char1.

  ls_variant-report = sy-repid.

  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
       EXPORTING
            is_variant          = ls_variant
            i_save              = 'A'
*           it_default_fieldcat =
       IMPORTING
            e_exit              = l_exit
            es_variant          = ls_variant
       EXCEPTIONS
            not_found = 2.
  IF sy-subrc = 2.
    MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    IF l_exit EQ space.
      c_variant = ls_variant-variant.
    ENDIF.
  ENDIF.

ENDFORM.                    " f4_variant
*&---------------------------------------------------------------------*
*&      Form  build_sort_table
*&---------------------------------------------------------------------*
FORM build_sort_table USING  p_spos
                             p_fieldname
                             p_up
                             p_subtot
                             p_group.
  DATA: ls_sort TYPE slis_sortinfo_alv.

  ls_sort-spos      = p_spos.
  ls_sort-fieldname = p_fieldname.
  ls_sort-up        = p_up.
  ls_sort-subtot    = p_subtot.
  ls_sort-group     = p_group.
  APPEND ls_sort TO gt_sorts.
ENDFORM.                    " build_sort_table
*&---------------------------------------------------------------------*
*&      Form  set_line_color
*&---------------------------------------------------------------------*
FORM set_line_color USING    p_color.
  DATA: ls_fieldcat   TYPE slis_fieldcat_alv,
        lt_color      TYPE slis_t_specialcol_alv,
        ls_color      TYPE slis_specialcol_alv.

  REFRESH lt_color.
  CLEAR   lt_color.
  LOOP AT gt_fieldcat INTO ls_fieldcat.
    ls_color-fieldname = ls_fieldcat-fieldname.
    ls_color-color-col = p_color.
*    "cl_gui_resources=>list_col_positive.
    ls_color-color-int = cl_gui_resources=>list_intensified.
    ls_color-color-inv = 0.
    ls_color-nokeycol  = 'X'.
    APPEND ls_color TO lt_color.
    it_bkpf-tabcolor = lt_color.
  ENDLOOP.

ENDFORM.                    " set_line_color
*&---------------------------------------------------------------------*
*&      Form  MAKE_ALV_BKPF
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM make_alv_bkpf.
* ==> 5. build field category
  PERFORM build_field_category
  USING :
   'BUKRS'     'CoCd'     '04' ' ' 'L'  ' '  ' '  '  ' '  ' ,
   'BELNR'     'Doc.no.'  '10' ' ' 'L'  ' '  ' '  '  ' '  ' ,
   'BLART'     'DT'       '02' ' ' 'L'  ' '  ' '  '  ' '  ' ,
   'CPUDT'     'Entry dt' '10' ' ' 'L'  ' '  ' '  '  ' '  ' ,
   'BLDAT'     'Doc.date' '10' ' ' 'L'  ' '  ' '  '  ' '  ' ,
   'BUDAT'     'Post.dte' '10' ' ' 'L'  ' '  ' '  '  ' '  ' ,
   'BKTXT'     'Doc.Head text'  '20' ' ' 'L'  ' '  ' '  '  ' '  ' ,
   'AWKEY'     'Ref.Doc.Num'  '10' ' ' 'L'  ' '  ' '  '  ' '  ' .

* ==> 6. build sorts info
*  REFRESH gt_sorts.
*  PERFORM build_sort_table
*    USING :
*       '1'    'VTWEG'   'X'   'X'   '*'.
* ==> 2. set variant default
*  PERFORM set_variant CHANGING wa_var.
* ==> 3. set layout for alv style
  PERFORM set_layout CHANGING gs_layout.
* ==> 4. set events for alv
  PERFORM set_events CHANGING gt_events.
* ==> 7. call function display alv.

  CALL FUNCTION wa_alv_function_name
       EXPORTING
            i_callback_program       = wa_repid
            i_callback_pf_status_set = 'ALV_EVENT_PF_STATUS_SET'
            i_callback_user_command  = 'ALV_EVENT_USER_COMMAND'
            is_layout                = gs_layout
            it_fieldcat              = gt_fieldcat[]
            it_special_groups        = gt_sp_group[]
            it_sort                  = gt_sorts[]
            i_default                = wa_default
            i_save                   = wa_var_save
            is_variant               = wa_var
            it_events                = gt_events[]
            is_print                 = gs_prnt
       TABLES
            t_outtab                 = it_bkpf.
ENDFORM.                    " MAKE_ALV_BKPF
