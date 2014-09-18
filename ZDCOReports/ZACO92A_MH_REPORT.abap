*&--------------------------------------------------------------------
*& REPORT                 : ZACO92A_MH_REPORT
*& Author                 : WSKIM
*& Creation Date          : 02/19/2005
*& Specification By       :
*& Pattern                : Report 1-1
*& Development Request No :
*& Addl documentation     :
*& Description            : M/H Report
*& Modification Log
*& Date     Developer      Request ID      Description
*&
*&--------------------------------------------------------------------
INCLUDE zaco92a_mh_report_top.
*&----------------------------------------------------------------------
*  Initialization
*&----------------------------------------------------------------------
INITIALIZATION.
  PERFORM initialization.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.

AT SELECTION-SCREEN.
  PERFORM check_period.
*----------------------------------------------------------------------*
* Start-of-selection
*----------------------------------------------------------------------*
START-OF-SELECTION.
*READ : actual data 'ztco_stmh_at'
  PERFORM get_data_ztco_stmh_at.
*Read : standard & business data 'ztco_stmh'
  PERFORM get_data_ztco_stmh.
*Merge
  PERFORM merge_both_data.
*----------------------------------------------------------------------*
* END-of-selection
*----------------------------------------------------------------------*
END-OF-SELECTION.
*Display
  PERFORM display.
*&---------------------------------------------------------------------*
*&      Form  initialization
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM initialization.
* ==> Change Variant saving type
  wa_var_save = 'A'.
* ==> Change first mode   GRID or LIST
  wa_alv_function_name = 'REUSE_ALV_GRID_DISPLAY'.
* wa_alv_function_name = 'REUSE_ALV_LIST_DISPLAY'.
  REFRESH : gt_fieldcat.
  CLEAR   : gs_layout.
  wa_repid = sy-repid.

ENDFORM.                    " initialization
*&---------------------------------------------------------------------*
*&      Form  check_period
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_period.

ENDFORM.                    " check_period
*&---------------------------------------------------------------------*
*&      Form  get_data_ztco_stmh_at
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data_ztco_stmh_at.

  REFRESH it_stmh_at.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_stmh_at
    FROM ztco_stmh_at
     WHERE kokrs EQ p_kokrs
       AND gubun EQ 'F'
       AND bdatj EQ p_bdatj
       AND poper EQ p_poper.

  DESCRIBE TABLE it_stmh_at LINES w_int.
  IF w_int = 0.
    MESSAGE s000 WITH 'There is no data'.
    STOP.
  ENDIF.

ENDFORM.                    " get_data_ztco_stmh_at
*&---------------------------------------------------------------------*
*&      Form  get_data_ztco_stmh
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data_ztco_stmh.
  REFRESH it_stmh.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_stmh
    FROM ztco_stmh
     WHERE kokrs EQ p_kokrs
       AND bdatj EQ p_bdatj
       AND poper EQ p_poper.

  DESCRIBE TABLE it_stmh LINES w_int.
  IF w_int = 0.
    MESSAGE s000 WITH 'There is no data'.
    STOP.
  ENDIF.

ENDFORM.                    " get_data_ztco_stmh
*&---------------------------------------------------------------------*
*&      Form  merge_both_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM merge_both_data.
  DATA : p_yymm LIKE ztco_wip-yymm.
  REFRESH it_out.
  CLEAR: it_stmh_at,it_stmh,it_out.

  LOOP AT it_stmh_at.
    MOVE-CORRESPONDING it_stmh_at TO it_out.
    APPEND it_out.clear it_out.
  ENDLOOP.

  LOOP AT it_stmh.
    READ TABLE it_stmh_at WITH KEY fsc_matnr = it_stmh-fsc_matnr
                                   shop      = it_stmh-shop
                                   llv_matnr = it_stmh-llv_matnr
                                   kostl     = it_stmh-kostl.
    IF sy-subrc <> 0.
      MOVE :it_stmh-kokrs     TO it_out-kokrs,
            it_stmh-bdatj     TO it_out-bdatj,
            it_stmh-poper     TO it_out-poper,
            it_stmh-fsc_matnr TO it_out-fsc_matnr,
            it_stmh-shop      TO it_out-shop,
            it_stmh-llv_matnr TO it_out-llv_matnr,
            it_stmh-kostl     TO it_out-kostl.
      APPEND it_out.CLEAR :it_stmh,it_out.
    ENDIF.
  ENDLOOP.

  LOOP AT it_out.
*Standard qty
    READ TABLE it_stmh WITH KEY klvar     = 'PPC1'
                                fsc_matnr = it_out-fsc_matnr
                                shop      = it_out-shop
                                llv_matnr = it_out-llv_matnr
                                kostl     = it_out-kostl.
    IF sy-subrc = 0.
      MOVE it_stmh-menge TO it_out-standard_qty .
    ENDIF.
*annual business qty
    READ TABLE it_stmh WITH KEY klvar     = 'ZPCP'
                                fsc_matnr = it_out-fsc_matnr
                                shop      = it_out-shop
                                llv_matnr = it_out-llv_matnr
                                kostl     = it_out-kostl.
    IF sy-subrc = 0.
      MOVE it_stmh-menge TO it_out-annul_qty .
    ENDIF.
    IF it_out-gr_qty = 0.
      CONCATENATE p_bdatj p_poper+1(2) INTO p_yymm.
      SELECT SUM( outqty ) INTO it_out-gr_qty
          FROM ztco_wip
           WHERE yymm = p_yymm
             AND gubun = 'F'
             AND matnr EQ it_out-fsc_matnr.
    ENDIF.
    MODIFY it_out FROM it_out.CLEAR :it_out.
  ENDLOOP.

ENDFORM.                    " merge_both_data
*&---------------------------------------------------------------------*
*&      Form  display
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display.
* ==> 5. build field category
  PERFORM field_category.
* ==> 2. set variant default
  PERFORM set_variant CHANGING wa_var.
* ==> 3. set layout for alv style
  PERFORM set_layout CHANGING gs_layout.
* ==> 4. set events for alv
  PERFORM set_events CHANGING gt_events.
*===> 5. set event for top-of-page grid.
  PERFORM set_build_event.
*===>
  PERFORM comment_build USING  w_top_of_page[].

* ==> 7. call function display alv.

  CALL FUNCTION wa_alv_function_name
    EXPORTING
         i_callback_program      = wa_repid
         i_callback_pf_status_set = 'ALV_EVENT_PF_STATUS_SET'
         i_callback_user_command  = 'ALV_EVENT_USER_COMMAND'
         is_layout               = gs_layout
         it_fieldcat             = gt_fieldcat[]
         it_special_groups       = gt_sp_group[]
         it_sort                 = gt_sorts[]
*         IT_FILTER               =
         i_default               = wa_default
         i_save                  = wa_var_save
         is_variant              = wa_var
*         it_events               = gt_events[]
         it_events               =  w_eventcat[]
         is_print                = gs_prnt
*        IT_EVENT_EXIT           =
*           I_SCREEN_START_COLUMN   = 10
*           I_SCREEN_START_LINE     = 2
*           I_SCREEN_END_COLUMN     = 80
*           I_SCREEN_END_LINE       = 23
    TABLES
         t_outtab                = it_out.

ENDFORM.                    " display
*&---------------------------------------------------------------------*
*&      Form  field_category
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM field_category.

  PERFORM build_field_category  USING :
   'BDATJ'        'Year'        '4' 'X' 'L'  ' '  ' '  '  ' '  ' ,
   'POPER'        'Period'      '6'  'X' 'L'  ' '  ' '  '  ' '  ' ,
   'FSC_MATNR'    'Material'    '15' ' ' 'L'  ' '  ' '  '  ' '  ' ,
   'SHOP'         'Shop'        '4' ' ' ' '  ' '  ' '  '  ' '  ' ,
   'LLV_MATNR'    'Material'    '15' ' ' 'L'  ' '  ' '  '  ' '  ' ,
   'KSTAR'        'CostElement' '10' ' ' 'L'  ' '  ' '  '  ' '  ' ,
   'KOSTL'        'CostCenter'  '10' ' ' 'L'  ' '  ' '  '  ' '  ' ,
   'LSTAR'        'ActivityType'    '08' ' ' 'L'  ' '  ' '  '  ' '  ' ,
   'ANNUAL_QTY'   'Annual Qty'  '10' ' ' 'R'  ' '  ' '  '  ' '  ' ,
   'STANDARD_QTY' 'StandardQty' '10' ' ' 'R'  ' '  ' '  '  ' '  ' ,
   'MBGBTR'       'CurrentQty'  '10' ' ' 'R'  ' '  ' '  '  ' '  ' ,
   'ADD_MBGBTR'   'Add Qty'     '10' ' ' 'R'  ' '  ' '  '  ' '  ' ,
   'WIP_QUANTITY' 'Wip Qty'     '10' ' ' 'R'  ' '  ' '  '  ' '  ' ,
   'ACTUAL_SCRAP' 'ActScrapQty' '10' ' ' 'R'  ' '  ' '  '  ' '  ' ,
   'MANU_QTY'     'ActualQty'   '10' ' ' 'R'  ' '  ' '  '  ' '  ' ,
   'GR_QTY'       'M/L Qty'     '10' ' ' 'R'  ' '  ' '  '  ' '  ' .


ENDFORM.                    " field_category
*&---------------------------------------------------------------------*
*&      Form  set_variant
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_WA_VAR  text
*----------------------------------------------------------------------*
FORM set_variant CHANGING cs_vari TYPE disvariant.

ENDFORM.                    " set_variant
*&---------------------------------------------------------------------*
*&      Form  set_layout
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_GS_LAYOUT  text
*----------------------------------------------------------------------*
FORM set_layout CHANGING cs_layo TYPE slis_layout_alv.

*... Display options
  cs_layo-colwidth_optimize      = space.
  cs_layo-no_colhead             = space.
  cs_layo-no_hotspot             = space.
  cs_layo-zebra                  = ' '.
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
*&---------------------------------------------------------------------*
*&      Form  set_events
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_GT_EVENTS  text
*----------------------------------------------------------------------*
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

ENDFORM.                    " set_events
*&---------------------------------------------------------------------*
*&      Form  build_field_category
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0335   text
*      -->P_0336   text
*      -->P_0337   text
*      -->P_0338   text
*      -->P_0339   text
*      -->P_0340   text
*      -->P_0341   text
*      -->P_0342   text
*      -->P_0343   text
*----------------------------------------------------------------------*
FORM build_field_category USING
                                  p_fieldname       " field name
                                  p_title           " field title
                                  p_outputlen       " length
                                  p_key             "
                                  p_just            "
                                  p_noout           "
                                  p_edit            "
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
  IF p_fieldname = 'TOT'.
    ls_fieldcat-emphasize = 'C700'.
  ENDIF.
  APPEND ls_fieldcat TO gt_fieldcat.

ENDFORM.                    " build_field_category
*&---------------------------------------------------------------------*
*&      Form  set_build_event
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_build_event.
  w_eventcat-name = 'TOP_OF_PAGE'.
  w_eventcat-form = 'DISPLAY_HEADER'.
  APPEND w_eventcat.

ENDFORM.                    " set_build_event
*&---------------------------------------------------------------------*
*&      Form  comment_build
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_W_TOP_OF_PAGE[]  text
*----------------------------------------------------------------------*
FORM comment_build USING  lt_top_of_page TYPE slis_t_listheader.
  DATA: ls_line TYPE slis_listheader,
        l_title(50),
        l_date(50).
*-------------- HEADER
  l_title = 'M/H Report'.
  CLEAR ls_line.
  ls_line-typ  = 'H'.
  ls_line-info = l_title .    "HEADER TITLE (H001)
  APPEND ls_line TO lt_top_of_page.

  l_date(6) = 'Year :'.
  l_date+7(4) = p_bdatj.
  l_date+19(7) = 'Period :'.
  l_date+28(3) = p_poper.

  ls_line-typ  = 'S'.
  ls_line-key  = 'Period'.
  ls_line-info = l_date.
  APPEND ls_line TO lt_top_of_page.

ENDFORM.                    " comment_build
*---------------------------------------------------------------------*
*  FORM alv_event_pf_status_set
*---------------------------------------------------------------------*
FORM alv_event_pf_status_set USING rt_extab TYPE slis_t_extab.

  IF wa_alv_function_name = 'REUSE_ALV_GRID_DISPLAY'.
    SET PF-STATUS 'STANDARD_GRID' EXCLUDING rt_extab.
  ELSE.
    SET PF-STATUS 'STANDARD' EXCLUDING rt_extab.
  ENDIF.
  SET TITLEBAR  'STANDARD'.


ENDFORM.                    "alv_event_pf_status_set

*---------------------------------------------------------------------*
*  FORM alv_event_user_command
*---------------------------------------------------------------------*
FORM alv_event_user_command USING r_ucomm     LIKE sy-ucomm
                                      rs_selfield TYPE slis_selfield.

  DATA : seltab LIKE rsparams OCCURS 0 WITH HEADER LINE.
  REFRESH seltab.
*  CASE r_ucomm.
**   ---------------------------------- processing on double click.
*    WHEN '&IC1'.
**      READ TABLE it_out INDEX rs_selfield-tabindex.
**      CASE rs_selfield-fieldname.
*
*      ENDCASE.
**---------------------------------- switching view type grid or list
*    WHEN 'LIST' OR 'GRID'.
*      PERFORM switch_list_or_grid USING r_ucomm.
*  ENDCASE.
*
*  CHECK r_ucomm EQ 'LIST' OR
*        r_ucomm EQ 'GRID'.
*
*  rs_selfield-exit = 'X'.

ENDFORM.                    "alv_event_user_command
*---------------------------------------------------------------------*
*  FORM f01_alv_event_top_of_page
*---------------------------------------------------------------------*
FORM alv_event_top_of_page.
*  WRITE : /(10) 'nvestment Program' , p_prnam.
*          /(10) 'BBBBBBB',  BKPF-BUKRS INVERSE COLOR 1 INPUT ON,
*           (20) 'CCCCCCC',  BKPF-BELNR INPUT ON.
ENDFORM.                    "alv_event_top_of_page

*---------------------------------------------------------------------*
*       FORM alv_event_top_of_LIST                                    *
*---------------------------------------------------------------------*
FORM alv_event_top_of_list.


ENDFORM.                    "alv_event_top_of_page
*&---------------------------------------------------------------------*
*&      Form  dispaly_heager
*----------------------------------------------------------------------*
FORM display_header.
  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
         EXPORTING
           i_logo             = 'ZHMMA_LOGO'
*           i_logo             = 'ENJOYSAP_LOGO'
              it_list_commentary = w_top_of_page.
ENDFORM.                    " top_of_page

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
              t_outtab                 = it_out
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
         TABLES
              t_outtab                 = it_out
         EXCEPTIONS
              program_error            = 1
              OTHERS                   = 2.

  ENDIF.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.


ENDFORM.                    " switch_list_or_grid
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
*    gt_out-tabcolor = lt_color.
  ENDLOOP.

ENDFORM.                    " set_line_color
