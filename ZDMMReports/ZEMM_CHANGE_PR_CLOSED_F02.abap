*&---------------------------------------------------------------------*
*&  Include           ZEMM_CHANGE_PR_CLOSED_F02
*&---------------------------------------------------------------------*


*&---------------------------------------------------------------------*
*&      Form  CREATE_AND_INIT_CONTROLS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_and_init_controls .

  CHECK : g_docking_container IS INITIAL.

*// Create an Instance Of Container
  CREATE OBJECT g_docking_container
    EXPORTING
      dynnr                       = '0100'
      repid                       = sy-repid
      side                        = g_docking_container->dock_at_top
      extension                   = 2000
    EXCEPTIONS
      cntl_error                  = 1
      cntl_system_error           = 2
      create_error                = 3
      lifetime_error              = 4
      lifetime_dynpro_dynpro_link = 5.

  IF sy-subrc <> 0.
    MESSAGE a003.
  ENDIF.

*// Create an Instance of ALV Control
  CREATE OBJECT g_grid
    EXPORTING
      i_parent      = g_docking_container
*     i_shellstyle  = ws_thickframe  "??????
      i_appl_events = 'X'.

* Tool Bar
  PERFORM exclude_of_toolbar_button USING 'GT_EXCLUDE'.

  PERFORM set_field_catalogs.
  PERFORM set_cell_attribute.
  PERFORM display_layout_attribute USING gs_layocat.

  PERFORM event_handler_register.

* F4 help
*  PERFORM set_f4_field.

*// ALV Grid Display
  PERFORM alv_grid_display.

ENDFORM.                    " CREATE_AND_INIT_CONTROLS


*&---------------------------------------------------------------------*
*&      Form  ALV_GRID_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM alv_grid_display .

*  g_repid = sy-repid.
*// Display
  CALL METHOD g_grid->set_table_for_first_display
    EXPORTING
      is_layout            = gs_layocat
      it_toolbar_excluding = gt_exclude
      i_save               = 'A'   " ??????.
      i_default            = 'X'   " ???? ???? ??.
      is_variant           = alv_variant  " ?? ?? display
    CHANGING
      it_outtab            = gt_display[]
*     it_sort              = gt_sort
      it_fieldcatalog      = gt_fieldcat[].

ENDFORM.                    " ALV_GRID_DISPLAY

*&---------------------------------------------------------------------*
*&      Form  SET_FIELD_CATALOGS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_field_catalogs .

  CLEAR : gt_fieldcat, gt_fieldcat[].

  PERFORM fill_field_catalogs
   USING:
      'S' 'FIELDNAME'   'CHECK',
      ' ' 'COLTEXT'     'SELECT',
      ' ' 'JUST'        'C',
      ' ' 'KEY'         'X',
      ' ' 'CHECKBOX'    'X',
      ' ' 'EDIT'        'X',
      'E' 'OUTPUTLEN'   '3',

      'S' 'FIELDNAME'   'ICON',
      ' ' 'COLTEXT'     'STATUS',
      ' ' 'JUST'        'C',
      ' ' 'KEY'         'X',
      'E' 'OUTPUTLEN'   '05'.

  PERFORM fill_field_catalogs
   USING:
      'S' 'FIELDNAME'   'BANFN',
      ' ' 'COLTEXT'     'PR No.',
      ' ' 'JUST'        'C',
      ' ' 'KEY'         'X',
      'E' 'OUTPUTLEN'   '15',

      'S' 'FIELDNAME'   'BNFPO',
      ' ' 'COLTEXT'     'PR item No.',
      ' ' 'JUST'        'C',
      ' ' 'KEY'         'X',
      'E' 'OUTPUTLEN'   '10',

      'S' 'FIELDNAME'   'ERDAT',
      ' ' 'COLTEXT'     'PR Created Date',
      ' ' 'JUST'        'C',
      'E' 'OUTPUTLEN'   '15',

      'S' 'FIELDNAME'   'MATNR',
      ' ' 'COLTEXT'     'Material No.',
*      ' ' 'NO_ZERO'     'X',
      'E' 'OUTPUTLEN'   '18',

      'S' 'FIELDNAME'   'MAKTX',
      ' ' 'COLTEXT'     'Material Description',
      'E' 'OUTPUTLEN'   '40',

      'S' 'FIELDNAME'   'EBAKZ',
      ' ' 'COLTEXT'     'PR Closed',
      ' ' 'CHECKBOX'    'X',
      ' ' 'JUST'        'C',
      'E' 'OUTPUTLEN'   '9',

      'S' 'FIELDNAME'   'RESULT_MSG',
      ' ' 'COLTEXT'     'Job Result',
      'E' 'OUTPUTLEN'   '36'.
*
  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
*    EXPORTING
*      i_structure_name       = c_st_nm
    CHANGING
      ct_fieldcat            = gt_fieldcat
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.

*  PERFORM change TABLES gt_fieldcat.

ENDFORM.                    " SET_FIELD_CATALOGS

*&---------------------------------------------------------------------*
*&      Form  FILL_FIELD_CATALOGS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0338   text
*      -->P_0339   text
*      -->P_0340   text
*----------------------------------------------------------------------*
*FORM FILL_FIELD_CATALOGS  USING    value(p_0338)
*                                   value(p_0339)
*                                   value(p_0340).
FORM fill_field_catalogs  USING  p_gub  p_fname  p_value.

  IF p_gub = 'S'.
    CLEAR gs_fieldcat.
  ENDIF.

*  DATA          g_fname(40).
*  FIELD-SYMBOLS <fs>        TYPE any.
  CLEAR : g_fname.
  CONCATENATE 'GS_FIELDCAT-' p_fname INTO g_fname.

  ASSIGN (g_fname) TO <fs>.
  <fs> = p_value.

  IF p_gub = 'E'.
    APPEND gs_fieldcat TO gt_fieldcat.
  ENDIF.

ENDFORM.                    " FILL_FIELD_CATALOGS

*&---------------------------------------------------------------------*
*&      Form  GET_ETC_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_etc_data .

  DATA : lt_makt TYPE TABLE OF makt  WITH HEADER LINE.

  IF NOT it_pr[] IS INITIAL.
*   "material description
    SELECT * FROM makt
      INTO CORRESPONDING FIELDS OF TABLE lt_makt
       FOR ALL ENTRIES IN it_pr
     WHERE matnr = it_pr-matnr
       AND spras = 'E'.

    SORT lt_makt BY matnr.      "Sort for Binary search
  ENDIF.


  CLEAR : it_pr.
  LOOP AT it_pr.

    CLEAR : gt_display.
    MOVE-CORRESPONDING it_pr  TO gt_display.

*   "material description
    CLEAR : lt_makt.
    READ TABLE lt_makt WITH KEY matnr = it_pr-matnr BINARY SEARCH.
    IF sy-subrc = 0.
      gt_display-maktx = lt_makt-maktx.
    ENDIF.

*   "icon
    IF gt_display-ebakz = space.   "Purchase Requisition Closed
*      gt_display-icon = icon_red_light.
      gt_display-icon = icon_yellow_light.
    ELSE.
      gt_display-icon = icon_green_light.
    ENDIF.

    APPEND  gt_display.

*   "collect PR No.
    it_pr_num-banfn = it_pr-banfn. "PR No.
    COLLECT it_pr_num.

    CLEAR : it_pr.
  ENDLOOP.


ENDFORM.                    " GET_ETC_DATA


*&---------------------------------------------------------------------*
*&      Form  SET_CELL_ATTRIBUTE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_cell_attribute .

  DATA : lt_celltab TYPE lvc_t_styl,
         lt_color   TYPE lvc_t_scol.

  DATA: l_ebeln         LIKE bseg-ebeln,
        l_index           LIKE  sy-tabix.

  LOOP AT gt_display.
    l_index = sy-tabix.

    CLEAR: lt_celltab[], lt_color[].

    PERFORM fill_celltab CHANGING lt_celltab.
*    PERFORM fill_color   CHANGING lt_color.

    CLEAR: gt_display-celltab[], gt_display-f_col[].

    INSERT LINES OF lt_celltab INTO TABLE gt_display-celltab.
    INSERT LINES OF lt_color   INTO TABLE gt_display-f_col.

    MODIFY gt_display INDEX l_index.
    CLEAR  gt_display.

  ENDLOOP.

ENDFORM.                    " SET_CELL_ATTRIBUTE

*&---------------------------------------------------------------------*
*&      Form  fill_celltab
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fill_celltab  CHANGING pt_celltab  TYPE lvc_t_styl.

  DATA : ls_celltab TYPE lvc_s_styl.
*         l_mode     TYPE raw4.

  DATA : l_fieldcat TYPE lvc_s_fcat.

  LOOP AT gt_fieldcat INTO l_fieldcat.

    ls_celltab-fieldname = l_fieldcat-fieldname.

*   "set mode : Display/Edit
    IF ls_celltab-fieldname = 'CHECK'.

      IF gt_display-icon = icon_green_light.
*       "set display mode
        ls_celltab-style = cl_gui_alv_grid=>mc_style_disabled.
      ELSE.
*       "set edit mode
        ls_celltab-style = cl_gui_alv_grid=>mc_style_enabled.
      ENDIF.
      INSERT ls_celltab INTO TABLE pt_celltab.
    ELSE.
*     "set display mode
      ls_celltab-style = cl_gui_alv_grid=>mc_style_disabled +
      cl_gui_alv_grid=>mc_style_f4_no.
      INSERT ls_celltab INTO TABLE pt_celltab.
    ENDIF.

  ENDLOOP.

*** gt_display

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

  LOOP AT gt_fieldcat INTO l_fieldcat.
    ls_color-fname = l_fieldcat-fieldname.
    IF gt_display-check = 'X'.
      ls_color-color-col = 4.
      ls_color-color-int = 0.
      INSERT ls_color INTO TABLE pt_color.
    ELSE.
      IF ( ls_color-fname  = 'EBAKZ' OR
           ls_color-fname  = 'CHECK'    ).
*        "???
      ELSE.
        ls_color-color-col = 2.
        ls_color-color-int = 0.
        INSERT ls_color INTO TABLE pt_color.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " fill_color

*&---------------------------------------------------------------------*
*&      Form  display_layout_attribute
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM display_layout_attribute  USING  p_layocat TYPE lvc_s_layo.

*// General display options
*  p_layocat-cwidth_opt = 'X'.
  p_layocat-sel_mode   = 'D'.   "A or D ???
**  p_layocat-edit       = 'E'.
  p_layocat-stylefname = 'CELLTAB'.
  p_layocat-ctab_fname = 'F_COL'.
  p_layocat-zebra = 'X'.

ENDFORM.                    " display_layout_attribute


*&---------------------------------------------------------------------*
*&      Form  EVENT_HANDLER_REGISTER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM event_handler_register .

  CALL METHOD g_grid->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_modified.

  CALL METHOD g_grid->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_enter.

*// Event Handler
  CREATE OBJECT g_event_handler.

  SET HANDLER g_event_handler->handle_toolbar            FOR g_grid.
  SET HANDLER g_event_handler->handle_user_command       FOR g_grid.
*  SET HANDLER g_event_handler->handle_data_changed       FOR g_grid.
*  SET HANDLER g_event_handler->handle_data_changed_finished
*                                                         FOR g_grid.
*  SET HANDLER g_event_handler->handle_onf4               FOR g_grid.
*  SET HANDLER g_event_handler->handle_double_click       FOR g_grid.
  SET HANDLER g_event_handler->handle_after_user_command FOR g_grid.
*  SET HANDLER g_event_handler->handle_hotspot_click      FOR g_grid.


ENDFORM.                    " EVENT_HANDLER_REGISTER

*&---------------------------------------------------------------------*
*&      Form  after_user_command
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM after_user_command  USING    e_ucomm.

  l_scroll-row = 'X'.
  l_scroll-col = 'X'.

  CALL METHOD g_grid->refresh_table_display
    EXPORTING
      i_soft_refresh = 'X'
      is_stable      = l_scroll.     "refresh

*  CALL METHOD g_grid->set_optimizer.


ENDFORM.                    " after_user_command

*&---------------------------------------------------------------------*
*&      Form  data_changed
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM data_changed
     USING rr_data_changed
                      TYPE REF TO cl_alv_changed_data_protocol
           e_onf4     TYPE char1
           e_ucomm    TYPE sy-ucomm.

  DATA: ls_mod_cells    TYPE lvc_s_modi,
        ls_inserted_row TYPE lvc_s_moce,
        ls_cells        TYPE lvc_s_modi,
        x_iwerk         LIKE t001w-iwerk.

*// ROWS CREATE,INSERT,COPY.
  IF  NOT rr_data_changed->mt_inserted_rows[] IS INITIAL.

*    LOOP AT rr_data_changed->mt_inserted_rows INTO ls_inserted_row.
*
*      CLEAR ls_mod_cells.
*      READ TABLE rr_data_changed->mt_good_cells
*      WITH KEY fieldname = 'STATUS'
*      row_id    = ls_inserted_row-row_id
*      INTO ls_mod_cells.
**// COPY
*      IF NOT ls_mod_cells-value IS INITIAL.
*
*        IF  ls_mod_cells-value = icon_green_light.
*
*          CALL METHOD rr_data_changed->modify_cell
*            EXPORTING
*              i_row_id    = ls_mod_cells-row_id
*              i_fieldname = 'STATUS'
*              i_value     = icon_red_light.
*
*          CALL METHOD rr_data_changed->modify_cell
*            EXPORTING
*              i_row_id    = ls_mod_cells-row_id
*              i_fieldname = 'EQUNR'
*              i_value     = ''.
*
*        ENDIF.
*
*        CALL METHOD rr_data_changed->modify_cell
*          EXPORTING
*            i_row_id    = ls_mod_cells-row_id
*            i_fieldname = 'MSG'
*            i_value     = ''.
*
*      ELSE.
*
*        CALL METHOD rr_data_changed->modify_cell
*          EXPORTING
*            i_row_id    = ls_mod_cells-row_id
*            i_fieldname = 'STATUS'
*            i_value     = icon_red_light.
*
*      ENDIF.
*
*    ENDLOOP.

  ENDIF.

ENDFORM.                    " data_changed

*&---------------------------------------------------------------------*
*&      Form  data_changed_finished
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM data_changed_finished
                     USING e_modified    TYPE char01
                           et_good_cells TYPE lvc_t_modi.

*  DATA : ls_good_cells LIKE LINE OF et_good_cells.
****  CLEAR : g_tabix2.
*
*  READ TABLE et_good_cells INTO ls_good_cells INDEX 1.
*
*  g_tabix = ls_good_cells-row_id.
*
*  CASE ls_good_cells-fieldname.
*    WHEN 'CHECK'.
*      PERFORM data_changed_grid.
*      PERFORM refresh_table_display.
*  ENDCASE.

ENDFORM.                    " data_changed_finished

*&---------------------------------------------------------------------*
*&      Form  data_changed_grid
*&---------------------------------------------------------------------*
FORM data_changed_grid.

  READ TABLE gt_display INTO gs_display INDEX g_tabix.

ENDFORM.                    " data_changed_grid


*&---------------------------------------------------------------------*
*&      Form  REFRESH_TABLE_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM refresh_table_display .

  CHECK NOT g_grid IS INITIAL.

  PERFORM set_cell_attribute.

  l_scroll-row = 'X'.
  l_scroll-col = 'X'.

  CALL METHOD g_grid->refresh_table_display
    EXPORTING
      i_soft_refresh = 'X'
      is_stable      = l_scroll.     "refresh

*  CALL METHOD g_grid->set_optimizer.

ENDFORM.                    " REFRESH_TABLE_DISPLAY


*&---------------------------------------------------------------------*
*&      Form  double_click
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM double_click  USING    e_row
      e_column.
* ??/???? ??????? ??->???? ????? ??
  DATA: l_htype(4).
  CLEAR: l_htype.
  PERFORM p0000_check_chracter USING e_row l_htype.
  CHECK l_htype EQ 'NUMC'.


ENDFORM.                    " double_click

*&---------------------------------------------------------------------*
*&      Form  p0000_check_chracter
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM p0000_check_chracter USING e_row p_htype .
  DATA: l_string(50).

  CLEAR: l_string, p_htype.
  WRITE e_row           TO l_string.

  CALL FUNCTION 'NUMERIC_CHECK'
    EXPORTING
      string_in  = l_string
    IMPORTING
*     STRING_OUT =
      htype      = p_htype.

ENDFORM.                    " p0000_check_chracter

*&---------------------------------------------------------------------*
*&      Form  hotspot_click
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM hotspot_click  USING    e_row_id
      e_column_id.

  READ TABLE gt_display INDEX e_row_id.

  CASE e_column_id.
*    WHEN 'EBELN'.
*      SET PARAMETER ID 'BES'  FIELD gt_display-ebeln.
*      CALL TRANSACTION 'ME23N'.
*
*    WHEN 'MATNR'.
*      SET PARAMETER ID 'MAT'  FIELD gt_display-matnr.
*      CALL TRANSACTION 'MM03' AND SKIP FIRST SCREEN.
*

  ENDCASE.


ENDFORM.                    " hotspot_click

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
        l_matnr       LIKE  mara-matnr.

  RANGES: ls_lgort FOR mard-lgort.

  FIELD-SYMBOLS : <f4tab> TYPE lvc_t_modi.

*  IF  e_fieldname = 'LGORT'.
*
*    DATA: BEGIN OF f4_arbpl OCCURS 0,
*      lgort     LIKE t001l-lgort,
*      lgobe     LIKE t001l-lgobe,
*    END   OF f4_arbpl.
*
*    CLEAR : f4_arbpl[].
*
*    CASE p_werks.
*      WHEN 'P001'.
*        REFRESH: ls_lgort.
*        ls_lgort-sign = 'I'.
*        ls_lgort-option = 'EQ'.
*        ls_lgort-low = 'P600'. APPEND ls_lgort.
*        ls_lgort-low = 'P610'. APPEND ls_lgort.
*        ls_lgort-low = 'P620'. APPEND ls_lgort.
*        ls_lgort-low = 'P630'. APPEND ls_lgort.
*        ls_lgort-low = 'P640'. APPEND ls_lgort.
*        ls_lgort-low = 'P690'. APPEND ls_lgort.
*        SELECT *
*              INTO CORRESPONDING FIELDS OF TABLE f4_arbpl
*              FROM t001l
*              WHERE  lgort IN ls_lgort.
*      WHEN 'E001'.
*        REFRESH: ls_lgort.
*        ls_lgort-sign = 'I'.
*        ls_lgort-option = 'EQ'.
*        ls_lgort-low = 'E650'. APPEND ls_lgort.
*        ls_lgort-low = 'E660'. APPEND ls_lgort.
*        SELECT *
*              INTO CORRESPONDING FIELDS OF TABLE f4_arbpl
*              FROM t001l
*              WHERE  lgort IN ls_lgort.
*     WHEN 'E002'.
*        REFRESH: ls_lgort.
*        ls_lgort-sign = 'I'.
*        ls_lgort-option = 'EQ'.
*        ls_lgort-low = 'N650'. APPEND ls_lgort.
*        ls_lgort-low = 'N660'. APPEND ls_lgort.
*        SELECT *
*              INTO CORRESPONDING FIELDS OF TABLE f4_arbpl
*              FROM t001l
*              WHERE  lgort IN ls_lgort.
*
*      WHEN OTHERS.
*        SELECT *
*            INTO CORRESPONDING FIELDS OF TABLE f4_arbpl
*            FROM t001l.
**           WHERE  lgort BETWEEN 'M100' AND 'M400'.
*    ENDCASE.
*
*    it_fields-tabname    = 'T001L'.
*    it_fields-fieldname  = 'LGORT'.
*    it_fields-selectflag = 'X'.
*    APPEND it_fields.
*
*    it_fields-tabname    = 'T001L'.
*    it_fields-fieldname  = 'LGOBE'.
*    it_fields-selectflag = ' '.
*    APPEND it_fields.
*
*    CALL FUNCTION 'HELP_VALUES_GET_NO_DD_NAME'
*      EXPORTING
*        selectfield                  = selectfield
*        cucol                        = 50
*        curow                        = 2
*      IMPORTING
*        ind                          = ld_tabix
*        select_value                 = select_value
*      TABLES
*        fields                       = it_fields
*        full_table                   = f4_arbpl
*      EXCEPTIONS
*        full_table_empty             = 1
*        no_tablestructure_given      = 2
*        no_tablefields_in_dictionary = 3
*        more_then_one_selectfield    = 4
*        no_selectfield               = 5
*        OTHERS                       = 6.
*
*    IF sy-subrc <> 0.
**      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
**      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*    ELSE.
*
**- assign the cell table fieldsymbol to the dereferenced data table and
**- fill the table.
*      CHECK NOT select_value IS INITIAL.
*
*      ASSIGN er_event_data->m_data->* TO <f4tab>.
*
*      ls_modi-row_id    = es_row_no-row_id.
*      ls_modi-fieldname = 'LGORT'.
*      ls_modi-value     = select_value.
*      APPEND ls_modi TO <f4tab>.
*
*      er_event_data->m_event_handled = 'X'.
*
*    ENDIF.
*
*  ENDIF.

ENDFORM.                                                    " on_f4
*&---------------------------------------------------------------------*
*&      Form  toolbar_pros
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM toolbar_pros
        USING r_object TYPE REF TO cl_alv_event_toolbar_set
              r_interactive.

  DATA : ls_toolbar  TYPE stb_button.

* "Select All
  CLEAR ls_toolbar.
  MOVE 'SALL'               TO  ls_toolbar-function.
  MOVE icon_select_all      TO  ls_toolbar-icon.
  MOVE 'Select All'         TO  ls_toolbar-quickinfo.
  MOVE 'Select All'         TO  ls_toolbar-text.
  MOVE ' '                  TO  ls_toolbar-disabled.
  APPEND ls_toolbar         TO   r_object->mt_toolbar.

* "Deselect All
  CLEAR ls_toolbar.
  MOVE 'DALL'               TO  ls_toolbar-function.
  MOVE icon_deselect_all    TO  ls_toolbar-icon.
  MOVE 'Deselect All'       TO  ls_toolbar-quickinfo.
  MOVE 'Deselect All'       TO  ls_toolbar-text.
  MOVE ' '                  TO  ls_toolbar-disabled.
  APPEND ls_toolbar         TO  r_object->mt_toolbar.

* "separated bar
  MOVE   3                  TO   ls_toolbar-butn_type.
  APPEND ls_toolbar         TO   r_object->mt_toolbar.

* "
  CLEAR ls_toolbar.
  MOVE 'PR_CLOSE_PROCESS'   TO  ls_toolbar-function.
  MOVE icon_mass_change     TO  ls_toolbar-icon.
  MOVE 'Change PR Closed Status' TO  ls_toolbar-quickinfo.
  MOVE 'Change PR Closed'        TO  ls_toolbar-text.
  MOVE ' '                  TO  ls_toolbar-disabled.
  APPEND ls_toolbar         TO  r_object->mt_toolbar.

ENDFORM.                    " toolbar_pros


*&---------------------------------------------------------------------*
*&      Form  user_command_pros
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM user_command_pros  USING    r_ucomm.

  CASE r_ucomm.
    WHEN 'PR_CLOSE_PROCESS'.   "Change PR Close Status

      READ TABLE gt_display WITH KEY check = 'X'.
      IF sy-subrc <> 0.
        MESSAGE i001.

      ELSE.
        READ TABLE gt_display WITH KEY check = 'X'
                                       icon = icon_green_light.
        IF sy-subrc = 0.
*          "skip (already processed)
        ELSE.
          CLEAR : g_continue.
          PERFORM message_popup_screen USING text-008 text-009
                                    CHANGING g_continue.

          CHECK : g_continue = '1'.    "want to update

          PERFORM display_progress_bar  USING text-011.
          PERFORM change_pr_status.    "BAPI

        ENDIF.
      ENDIF.

      PERFORM set_cell_attribute.

    WHEN 'SALL'.       "Select All
      PERFORM select_all_item.
      PERFORM set_cell_attribute.

    WHEN 'DALL'.       "Deselect All
      PERFORM deselect_all_item.
      PERFORM set_cell_attribute.

  ENDCASE.


ENDFORM.                    " user_command_pros


*&---------------------------------------------------------------------*
*&      Form  select_all_item
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM select_all_item .

  LOOP AT gt_display.
    IF ( gt_display-check = ''  AND
         gt_display-icon  <> icon_green_light ).

      gt_display-check = 'X'.
      MODIFY gt_display.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " select_all_item

*&---------------------------------------------------------------------*
*&      Form  deselect_all_item
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM deselect_all_item .

  LOOP AT gt_display.
    IF ( gt_display-check = 'X'  AND
         gt_display-icon  <> icon_green_light ).

      gt_display-check = ''.
      MODIFY gt_display.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " deselect_all_item

*&---------------------------------------------------------------------*
*&      Form  exclude_of_toolbar_button
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0756   text
*----------------------------------------------------------------------*
FORM exclude_of_toolbar_button  USING    p_tabname..
  DATA : l_tab_name LIKE feld-name.

  FIELD-SYMBOLS : <table> TYPE ui_functions.

  CONCATENATE p_tabname '[]' INTO  l_tab_name.
  ASSIGN     (l_tab_name)    TO <table>.

*
  PERFORM add_exclude_toolbar_button
  TABLES <table>
*        USING : cl_gui_alv_grid=>mc_fc_excl_all.
  USING : cl_gui_alv_grid=>mc_fc_loc_undo,   "LOCAL&UNDO
          cl_gui_alv_grid=>mc_fc_auf,        "AUF
          cl_gui_alv_grid=>mc_fc_average,    "AVERAGE
*           cl_gui_alv_grid=>mc_fc_back_classic,
*           cl_gui_alv_grid=>mc_fc_call_abc, " &ABC
*           cl_gui_alv_grid=>mc_fc_call_chain,
*           cl_gui_alv_grid=>mc_fc_call_crbatch,
*           cl_gui_alv_grid=>mc_fc_call_crweb,
*           cl_gui_alv_grid=>mc_fc_call_lineitems,
*           cl_gui_alv_grid=>mc_fc_call_master_data,
*           cl_gui_alv_grid=>mc_fc_call_more,
*           cl_gui_alv_grid=>mc_fc_call_report,
*           cl_gui_alv_grid=>mc_fc_call_xint,
*           cl_gui_alv_grid=>mc_fc_call_xxl,
*           cl_gui_alv_grid=>mc_fc_col_invisible,
*           cl_gui_alv_grid=>mc_fc_col_optimize,
*           cl_gui_alv_grid=>mc_fc_current_variant,
*           cl_gui_alv_grid=>mc_fc_data_save,
*           cl_gui_alv_grid=>mc_fc_delete_filter,
*           cl_gui_alv_grid=>mc_fc_deselect_all,
        cl_gui_alv_grid=>mc_fc_detail,
*           cl_gui_alv_grid=>mc_fc_expcrdata,
*           cl_gui_alv_grid=>mc_fc_expcrdesig,
*           cl_gui_alv_grid=>mc_fc_expcrtempl,
*           cl_gui_alv_grid=>mc_fc_expmdb,
*           cl_gui_alv_grid=>mc_fc_extend,
*           cl_gui_alv_grid=>mc_fc_f4,
*           cl_gui_alv_grid=>mc_fc_filter,
*           cl_gui_alv_grid=>mc_fc_find,
*           cl_gui_alv_grid=>mc_fc_fix_columns,
        cl_gui_alv_grid=>mc_fc_graph,
*           cl_gui_alv_grid=>mc_fc_help,
        cl_gui_alv_grid=>mc_fc_info,
*           cl_gui_alv_grid=>mc_fc_load_variant,
*           cl_gui_alv_grid=>mc_fc_loc_copy,
*           cl_gui_alv_grid=>mc_fc_html,
        cl_gui_alv_grid=>mc_fc_loc_copy_row,
        cl_gui_alv_grid=>mc_fc_loc_cut,
        cl_gui_alv_grid=>mc_fc_loc_delete_row,
        cl_gui_alv_grid=>mc_fc_loc_insert_row,
        cl_gui_alv_grid=>mc_fc_loc_move_row,
        cl_gui_alv_grid=>mc_fc_loc_append_row,
        cl_gui_alv_grid=>mc_fc_loc_paste,
        cl_gui_alv_grid=>mc_fc_loc_paste_new_row,
*           cl_gui_alv_grid=>mc_fc_maintain_variant,
*           cl_gui_alv_grid=>mc_fc_maximum,
*           cl_gui_alv_grid=>mc_fc_minimum,
*           cl_gui_alv_grid=>mc_fc_pc_file,
*           cl_gui_alv_grid=>mc_fc_print,
*           cl_gui_alv_grid=>mc_fc_print_back,
*           cl_gui_alv_grid=>mc_fc_print_prev,
        cl_gui_alv_grid=>mc_fc_refresh.
*           cl_gui_alv_grid=>mc_fc_reprep,
*           cl_gui_alv_grid=>mc_fc_save_variant,
*           cl_gui_alv_grid=>mc_fc_select_all,
*           cl_gui_alv_grid=>mc_fc_send,
*           cl_gui_alv_grid=>mc_fc_separator,
*           cl_gui_alv_grid=>mc_fc_sort,
*           cl_gui_alv_grid=>mc_fc_sort_asc,
*           cl_gui_alv_grid=>mc_fc_sort_dsc,
*           cl_gui_alv_grid=>mc_fc_subtot,
*           cl_gui_alv_grid=>mc_mb_sum,
*           cl_gui_alv_grid=>mc_fc_sum.
*           cl_gui_alv_grid=>mc_fc_to_office,
*           cl_gui_alv_grid=>mc_fc_to_rep_tree,
*           cl_gui_alv_grid=>mc_fc_unfix_columns,
*           cl_gui_alv_grid=>mc_fc_views,
*           cl_gui_alv_grid=>mc_fc_view_crystal,
*           cl_gui_alv_grid=>mc_fc_view_excel,
*           cl_gui_alv_grid=>mc_fc_view_grid,
*           cl_gui_alv_grid=>mc_fc_word_processor.

ENDFORM.                    " exclude_of_toolbar_button

*&---------------------------------------------------------------------*
*&      Form  add_exclude_toolbar_button
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM add_exclude_toolbar_button  TABLES   p_table
USING    p_value.

  DATA: l_exclude TYPE ui_func.

  l_exclude = p_value.
  APPEND l_exclude TO p_table.  "GT_EXCLUDE

ENDFORM.                    " add_exclude_toolbar_button

*&---------------------------------------------------------------------*
*&      Form  SET_F4_FIELD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_f4_field .

*  CLEAR : gt_f4, gt_f4[].
**-- F4 FIELD
**-- [Caution]???? ABC??? ??
*  gs_f4-fieldname  = 'BANFN'.
*  gs_f4-register   = 'X'.
*  APPEND gs_f4 TO gt_f4.
*
*  CALL METHOD g_grid->register_f4_for_fields
*    EXPORTING
*      it_f4 = gt_f4.

ENDFORM.                    " SET_F4_FIELD
