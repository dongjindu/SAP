*&---------------------------------------------------------------------*
*&  Include           ZITBPMANALYSIS_ALV
*&---------------------------------------------------------------------*
* Define Variables and Internal Tables for ALV
TYPE-POOLS: slis.

INCLUDE <icon>.
INCLUDE <symbol>.
CONSTANTS: gc_formname_top_of_page TYPE slis_formname
                                   VALUE 'TOP_OF_PAGE',
           gc_var_save       TYPE c VALUE  'A',
           gc_pf_status_set  TYPE slis_formname VALUE 'PF_STATUS_SET',
           gc_user_command   TYPE slis_formname VALUE 'USER_COMMAND',
           gc_tvers          TYPE ck_tvers      VALUE '01'.

DATA: gt_list_top_of_page  TYPE slis_t_listheader,
      gt_list_top_of_page1 TYPE slis_t_listheader,
      gt_fieldcat          TYPE slis_t_fieldcat_alv,
      gs_fieldcat          TYPE slis_fieldcat_alv,
      gs_layout            TYPE slis_layout_alv,
      gt_events            TYPE slis_t_event,
      gt_specialcol        TYPE slis_t_specialcol_alv,
      gs_specialcol        TYPE slis_specialcol_alv.

DATA: gv_default(1)  TYPE c,
      gs_variant  LIKE disvariant,
      gs_variant1 LIKE disvariant,
      gv_repid    LIKE sy-repid.

* for ALV Grid
DATA : gt_exclude   TYPE ui_functions,
       gt_exclude1  TYPE ui_functions,
       gs_print     TYPE lvc_s_prnt,
       container    TYPE scrfname VALUE 'G_CUSTOM_CONTAINER',
       container1   TYPE scrfname VALUE 'G_CUSTOM_CONTAINER1',
       gs_fcat      TYPE lvc_s_fcat,
       gt_fcat      TYPE lvc_t_fcat,
       gs_layo      TYPE lvc_s_layo,
       gs_fcat1     TYPE lvc_s_fcat,
       gt_fcat1     TYPE lvc_t_fcat,
       gs_layo1     TYPE lvc_s_layo,
       gs_f4        TYPE lvc_s_f4,
       gt_f4        TYPE lvc_t_f4,
       gs_sort      TYPE lvc_s_sort,
       gt_sort      TYPE lvc_t_sort,
       gs_sort1      TYPE lvc_s_sort,
       gt_sort1      TYPE lvc_t_sort,
       gs_sort_alv  TYPE slis_sortinfo_alv,
       gt_sort_alv  TYPE slis_t_sortinfo_alv.

DATA : g_parent_html TYPE REF TO cl_gui_container,
       g_html_cntl      TYPE REF TO cl_gui_html_viewer,
       g_document       TYPE REF TO cl_dd_document.
DATA : gt_commentary TYPE slis_t_listheader,
       gs_commentary    TYPE slis_listheader.


DATA : ok_code      TYPE sy-ucomm,
       save_ok_code TYPE sy-ucomm.

* Define internal tables &sstructures for Possible Entry
DATA : gs_values TYPE seahlpres,
       gt_fields TYPE TABLE OF dfies WITH HEADER LINE,
       gt_values TYPE TABLE OF seahlpres WITH HEADER LINE,
       gs_fields TYPE dfies,
       ls_f4     TYPE ddshretval,
       ls_modi   TYPE lvc_s_modi.

* define fields and field-symbols for data-update
FIELD-SYMBOLS : <f4tab> TYPE lvc_t_modi.

* reference to custom container: neccessary to bind ALV Control
CLASS cl_gui_resources DEFINITION LOAD.
*CLASS LCL_EVENT_RECEIVER DEFINITION DEFERRED.

* Reference Variable for Docking Container
*DATA : G_dock_container    TYPE REF TO cl_gui_docking_container.
* Custom container
DATA : g_custom_container  TYPE REF TO cl_gui_docking_container, "CL_GUI_CUSTOM_CONTAINER,
       g_con_sp            TYPE REF TO cl_gui_splitter_container,
       g_custom_container1 TYPE REF TO cl_gui_custom_container,
       g_custom_container2 TYPE REF TO cl_gui_custom_container,
       g_con_grid      TYPE REF TO cl_gui_container,
       g_con_grid1     TYPE REF TO cl_gui_container,
       g_grid              TYPE REF TO cl_gui_alv_grid,
       g_grid1             TYPE REF TO cl_gui_alv_grid.

DATA: gt_row     TYPE lvc_t_row,
      gs_row     TYPE lvc_s_row,
      gt_roid    TYPE lvc_t_roid.

* define internal table for BDC
DATA: gt_bdc TYPE TABLE OF bdcdata    WITH HEADER LINE,
      gt_msg TYPE TABLE OF bdcmsgcoll WITH HEADER LINE,
      gs_opt LIKE ctu_params.

* for possible entry
DATA: BEGIN OF dynpfields OCCURS 3.
        INCLUDE STRUCTURE dynpread.
DATA: END OF dynpfields.

  DATA : l_address TYPE bapiaddr3,
         it_return TYPE TABLE OF bapiret2.

DATA: dyname         TYPE progname,
      dynumb         TYPE sychar04,
      exc_exctab     TYPE slis_t_extab,
      popup_fieldcat TYPE slis_t_fieldcat_alv,
      f              TYPE slis_fieldcat_alv,
      selfield       TYPE slis_selfield,
      exitfield,
      color_active(3)  VALUE 'C50',
      tabix LIKE sy-tabix.

DATA : gc_true   VALUE 'X',
       gc_false VALUE ''.

* possible entry for reason code
TYPES: BEGIN OF ty_ztcoum02,
         rgrp2 TYPE zrgrp2,
         text  TYPE zrtext,
       END OF ty_ztcoum02.

TYPES: BEGIN OF ty_rsn,
         kzust TYPE kzust,
         text  TYPE zrtext,
       END OF ty_rsn.

DATA: gt_ztcoum02 TYPE TABLE OF ty_ztcoum02 WITH HEADER LINE,
      gt_rsn      TYPE TABLE OF ty_rsn      WITH HEADER LINE.

DATA: stable        TYPE lvc_s_stbl.

DEFINE __set_refresh_mode.
  stable-row = &1.
  stable-col = &1.
END-OF-DEFINITION.

*&---------------------------------------------------------------------*
*&      Form  FILL_FIELD_CATEGORY
*&---------------------------------------------------------------------*
*       Build field catalog for ALV grid
*----------------------------------------------------------------------*
*      -->P_FNAME Field name
*      -->P_TXT   Column heading
*      -->P_LEN   Column width
*      -->P_TYPE  Data type
*----------------------------------------------------------------------*
FORM fill_field_category USING p_pos   TYPE lvc_colpos
                               p_fname TYPE lvc_fname
                               p_txt   TYPE lvc_txtcol
                               p_len   TYPE lvc_outlen
                               p_type  TYPE datatype_d.

  CLEAR gs_fcat.

  gs_fcat-col_pos   = p_pos.     " Column position
  gs_fcat-fieldname = p_fname.   " Field name
  gs_fcat-coltext   = p_txt.     " Column heading
  gs_fcat-outputlen = p_len.     " Column width
  gs_fcat-datatype  = p_type.    " Data type

  APPEND gs_fcat TO gt_fcat.

ENDFORM.                    " FILL_FIELD_CATEGORY
*&---------------------------------------------------------------------*
*&      Form  APPEND_EXCLUDE_FUNCTIONS
*&---------------------------------------------------------------------*
*       Append excluding functions
*----------------------------------------------------------------------*
*      -->P_TABNAME   Table name
*      -->P_VALUE     Excluding value
*----------------------------------------------------------------------*
FORM append_exclude_functions TABLES p_table
                              USING p_value.
  DATA ls_exclude TYPE ui_func.

  ls_exclude = p_value.
  APPEND ls_exclude TO p_table.

ENDFORM.                    " APPEND_EXCLUDE_FUNCTIONS
*&---------------------------------------------------------------------*
*&      Form  BUILD_FIELD_CATEGORY
*&---------------------------------------------------------------------*
*       Build field catalog for ALV list
*----------------------------------------------------------------------*
FORM build_field_category USING p_fieldname TYPE slis_fieldname
                                p_key       TYPE c
                                p_text      TYPE scrtext_l
                                p_len       TYPE outputlen
                                p_type      TYPE datatype_d.

  CLEAR gs_fieldcat.

  gs_fieldcat-fieldname = p_fieldname.
  gs_fieldcat-key       = p_key.
  gs_fieldcat-seltext_l = p_text.
  gs_fieldcat-outputlen = p_len.
  gs_fieldcat-datatype  = p_type.

  APPEND gs_fieldcat TO gt_fieldcat.


ENDFORM.                    " BUILD_FIELD_CATEGORY
*&---------------------------------------------------------------------*
*&      Form  SET_LAYOUT
*&---------------------------------------------------------------------*
FORM set_layout USING    p_edit_mode
                         p_box_fname
                CHANGING cs_layo TYPE slis_layout_alv.
  cs_layo-edit_mode         = p_edit_mode.
  cs_layo-numc_sum          = 'X'.
  cs_layo-box_fieldname     = p_box_fname.
  cs_layo-group_buttons     = 'X'.
  cs_layo-group_change_edit = 'X'.
  cs_layo-coltab_fieldname  = 'TABCOLOR'.
  cs_layo-colwidth_optimize = 'X'.

ENDFORM.                    " SET_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  SET_EVENTS
*&---------------------------------------------------------------------*
FORM set_events CHANGING ct_events TYPE slis_t_event.
  DATA ls_event TYPE slis_alv_event.

  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
    EXPORTING
      i_list_type = 0
    IMPORTING
      et_events   = ct_events.

  READ TABLE ct_events WITH KEY name =  slis_ev_top_of_page
                            INTO ls_event.
  IF     sy-subrc = 0.
    MOVE   gc_formname_top_of_page TO ls_event-form.
    APPEND ls_event TO ct_events.
  ENDIF.

ENDFORM.                    " SET_EVENTS
*&---------------------------------------------------------------------*
*&      Form  COMMENT_BUILD
*&---------------------------------------------------------------------*
FORM comment_build USING lt_top_of_page TYPE slis_t_listheader.
  DATA ls_line TYPE slis_listheader.

  CLEAR ls_line.
  ls_line-typ  = 'S'.
  ls_line-key  = 'Date:'.
  CONCATENATE sy-datum+0(4) sy-datum+4(2) sy-datum+6(2)
         INTO ls_line-info SEPARATED BY '.'.

  APPEND ls_line TO lt_top_of_page.

  ls_line-key  = 'User:'.
  ls_line-info = sy-uname.

  APPEND ls_line TO lt_top_of_page.

  ls_line-key  = ''.
  ls_line-info = ''.
  APPEND ls_line TO lt_top_of_page.

ENDFORM.                    " COMMENT_BUILD
*&---------------------------------------------------------------------*
*&  FORM TOP_OF_LIST
*&---------------------------------------------------------------------*
FORM top_of_list.
  NEW-LINE  NO-SCROLLING.
  WRITE : /1 'PGID: ', 8 sy-repid, 33 'Date:', sy-datum,
          62 'Time:', sy-uzeit, 92 sy-uname.

ENDFORM.                    "TOP_OF_LIST
*&---------------------------------------------------------------------*
*&      Form  TOP_OF_PAGE
*&---------------------------------------------------------------------*
FORM top_of_page.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = gt_list_top_of_page.
ENDFORM.                    "TOP_OF_PAGE
*&---------------------------------------------------------------------*
*&      Form  END_OF_PAGE
*&---------------------------------------------------------------------*
FORM alv_event_end_of_page.
  DATA lv_page(10).

  NEW-LINE.
  ULINE.

  WRITE: sy-pagno TO lv_page,
         /(120) lv_page CENTERED.

ENDFORM.                    "END_OF_PAGE
*&---------------------------------------------------------------------*
*&      Form  REFRESH_FIELD
*&---------------------------------------------------------------------*
*       Refresh for display
*----------------------------------------------------------------------*
FORM refresh_field.
  CALL METHOD g_grid->set_frontend_fieldcatalog
    EXPORTING
      it_fieldcatalog = gt_fcat.

  CALL METHOD g_grid->set_frontend_layout
    EXPORTING
      is_layout = gs_layo.

  __set_refresh_mode 'X'.

  CALL METHOD g_grid->refresh_table_display
    EXPORTING
      is_stable = stable.

  CALL METHOD cl_gui_cfw=>flush.

ENDFORM.                    " REFRESH_FIELD
*&---------------------------------------------------------------------*
*&      Form  CREATE_OBJECT
*&---------------------------------------------------------------------*
*       Create custom container control & instance
*----------------------------------------------------------------------*
FORM create_object.

* Create a Docking container and dock the control at top side of screen
  CHECK g_custom_container IS  INITIAL.
  CREATE OBJECT g_custom_container
    EXPORTING
      side                        = cl_gui_docking_container=>dock_at_top
      extension                   = 780
*     caption                     = 'Performance Plan'
    EXCEPTIONS
      cntl_error                  = 1
      cntl_system_error           = 2
      create_error                = 3
      lifetime_error              = 4
      lifetime_dynpro_dynpro_link = 5
      OTHERS                      = 6.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

**-.create split container which parent is docking
  CREATE OBJECT g_con_sp
    EXPORTING
      parent  = g_custom_container
      rows    = 3
      columns = 1.

  CALL METHOD g_con_sp->set_row_height
    EXPORTING
      id     = 1
      height = 9.

  CALL METHOD g_con_sp->set_row_height
    EXPORTING
      id     = 2
      height = 40.

  CALL METHOD g_con_sp->set_row_height
    EXPORTING
      id     = 3
      height = 50.

  CALL METHOD g_con_sp->set_border
    EXPORTING
      border = gc_false.

  CALL METHOD g_con_sp->get_container
    EXPORTING
      row       = 1
      column    = 1
    RECEIVING
      container = g_parent_html.

  CALL METHOD g_con_sp->get_container
    EXPORTING
      row       = 2
      column    = 1
    RECEIVING
      container = g_con_grid.

  CALL METHOD g_con_sp->get_container
    EXPORTING
      row       = 3
      column    = 1
    RECEIVING
      container = g_con_grid1.

* Create an Instance of ALV Control : Performance
  CREATE OBJECT g_grid
    EXPORTING
      i_parent       = g_con_grid
      i_appl_events  = gc_true.

*-Not used
  CREATE OBJECT g_grid1
    EXPORTING
      i_parent       = g_con_grid1
      i_appl_events  = gc_true.

*-Create Header Text
PERFORM  create_html.

ENDFORM.                    " CREATE_OBJECT

* For BDC
*---------------------------------------------------------------------*
*       Form DYNPRO                                                   *
*---------------------------------------------------------------------*
FORM dynpro USING p_dynbegin p_name p_value.
  CLEAR gt_bdc.

  IF p_dynbegin = 'X'.
    gt_bdc-program = p_name.
    gt_bdc-dynpro = p_value.
    gt_bdc-dynbegin = p_dynbegin.
  ELSE.
    gt_bdc-fnam = p_name.
    gt_bdc-fval = p_value.
  ENDIF.

  APPEND gt_bdc.

ENDFORM.                    " DYNPRO
*---------------------------------------------------------------------*
*       Form GET_OPT                                                   *
*---------------------------------------------------------------------*
FORM get_opt USING p_abpe.
  CLEAR gs_opt.

  gs_opt-dismode  = p_abpe.
  gs_opt-updmode  = 'X'.
  gs_opt-racommit = 'X'.
  gs_opt-nobinpt  = 'X'.

ENDFORM.                    " GET_OPT
*---------------------------------------------------------------------*
*       Form GET_MSG                                                   *
*---------------------------------------------------------------------*
FORM get_msg CHANGING p_msg.
  CALL FUNCTION 'RKC_MSG_STRING'
    EXPORTING
      id      = sy-msgid
      mtype   = sy-msgty
      number  = sy-msgno
      par1    = sy-msgv1
      par2    = sy-msgv2
      par3    = sy-msgv3
      par4    = sy-msgv4
    IMPORTING
      msg_lin = p_msg
    EXCEPTIONS
      OTHERS  = 1.

ENDFORM.                    " RKC_MSG_STRING
*&---------------------------------------------------------------------*
*&      Form  DATA_INPUT_ERROR
*&---------------------------------------------------------------------*
*       Error Message Display
*----------------------------------------------------------------------*
FORM data_input_error
        USING rr_data_changed TYPE REF TO cl_alv_changed_data_protocol
              rs_mod_cells    TYPE lvc_s_modi
              p_msgty         TYPE symsgty
              p_msgv1         TYPE symsgv
              p_fieldname     TYPE lvc_fname.

* Error Message Display
  CALL METHOD rr_data_changed->add_protocol_entry
    EXPORTING
      i_msgid     = '0K'
      i_msgno     = '000'
      i_msgty     = p_msgty
      i_msgv1     = p_msgv1
      i_msgv2     = ' '
      i_msgv3     = ' '
      i_fieldname = p_fieldname
      i_row_id    = rs_mod_cells-row_id.

ENDFORM.                    " DATA_INPUT_ERROR

*&---------------------------------------------------------------------*
*&      Form  POPUP_KALKA
*&---------------------------------------------------------------------*
*       Possible Enter for Costing types
*----------------------------------------------------------------------*
FORM popup_kalka USING pa_kalka     TYPE ck_kalka
                       p_fieldname TYPE dynfnam.
  DATA: BEGIN OF lt_tck02 OCCURS 0,
           kalka TYPE ck_kalka,
           txkla TYPE ck_txkla,
         END OF lt_tck02.

  DATA: BEGIN OF fields_tab OCCURS 1,
            kalka TYPE ck_kalka,
            txkla TYPE ck_txkla,
            color(3),
         END OF fields_tab.

  CLEAR: dynpfields, dyname, dynumb, exc_exctab, popup_fieldcat,
         f, selfield, exitfield, color_active, tabix, fields_tab.
  REFRESH: dynpfields, fields_tab.

  dynpfields-fieldname = p_fieldname.
  APPEND dynpfields.

  dyname = sy-repid.
  dynumb = sy-dynnr.

  CALL FUNCTION 'DYNP_VALUES_READ'
    EXPORTING
      dyname             = dyname
      dynumb             = dynumb
      translate_to_upper = 'X'
    TABLES
      dynpfields         = dynpfields
    EXCEPTIONS
      OTHERS             = 9.

  CLEAR lt_tck02.
  REFRESH lt_tck02.

  SELECT kalka txkla INTO TABLE lt_tck02
    FROM tck02
   WHERE spras = sy-langu.

  DELETE lt_tck02
    WHERE NOT ( kalka+0(1) = 'U' OR
                kalka+0(1) = 'M' OR
                kalka+0(1) = 'B' OR
                kalka+0(1) = 'R' ).

  SORT lt_tck02 BY kalka.

  f-reptext_ddic  = 'Costing Type'.
  f-fieldname = 'KALKA'.
  f-outputlen = 2.
  APPEND f TO popup_fieldcat.
  CLEAR f.

  f-reptext_ddic = 'Desc.'.
  f-fieldname = 'TXKLA'.

  DESCRIBE FIELD fields_tab-txkla LENGTH f-outputlen.
  APPEND f TO popup_fieldcat.

* Excluding-Table
  APPEND: '%SC ' TO exc_exctab,       " Search
          '%SC+' TO exc_exctab,       " Search+
          '&OUP' TO exc_exctab,       " Sort Up
          '&ODN' TO exc_exctab,       " Sort Dn
          '&ILT' TO exc_exctab,       " Filter
          '&OL0' TO exc_exctab.

* Popup
  tabix = sy-tabix.

  LOOP AT lt_tck02.
    fields_tab-kalka = lt_tck02-kalka.
    fields_tab-txkla = lt_tck02-txkla.
    APPEND fields_tab.
    CLEAR fields_tab.
  ENDLOOP.

  CALL FUNCTION 'REUSE_ALV_POPUP_TO_SELECT'
    EXPORTING
      i_linemark_fieldname    = 'COLOR'
      i_tabname               = 'FIELDS_TAB'
      it_fieldcat             = popup_fieldcat
      i_callback_user_command = 'USER_COMMAND_POPUP_LIGHTS_N'
      i_callback_program      = dyname
      it_excluding            = exc_exctab
    IMPORTING
      es_selfield             = selfield
      e_exit                  = exitfield
    TABLES
      t_outtab                = fields_tab.

  READ TABLE fields_tab INDEX tabix.
  CLEAR fields_tab-color.
  MODIFY fields_tab INDEX tabix.

  IF exitfield IS INITIAL.
    READ TABLE fields_tab INDEX selfield-tabindex.
    pa_kalka = fields_tab-kalka.

    dynpfields-fieldname = p_fieldname.
    dynpfields-fieldvalue = fields_tab-kalka.
    APPEND dynpfields.

    dyname = sy-repid.
    dynumb = sy-dynnr.

    CALL FUNCTION 'DYNP_VALUES_UPDATE'
      EXPORTING
        dyname     = dyname
        dynumb     = dynumb
      TABLES
        dynpfields = dynpfields.

  ENDIF.

ENDFORM.                    " POPUP_KALKA

*&---------------------------------------------------------------------*
*       Possible Enter for Reason
*----------------------------------------------------------------------*
FORM popup_rsn USING p_rsn       TYPE kzust
                     p_fieldname TYPE dynfnam.

  DATA: BEGIN OF fields_tab OCCURS 1,
           kzust TYPE kzust,
           text  TYPE zrtext,
           color(3),
         END OF fields_tab.

  CLEAR: dynpfields, dyname, dynumb, exc_exctab, popup_fieldcat,
         f, selfield, exitfield, color_active, tabix, fields_tab.
  REFRESH: dynpfields, fields_tab.

  dynpfields-fieldname = p_fieldname.
  APPEND dynpfields.

  dyname = sy-repid.
  dynumb = sy-dynnr.

  CALL FUNCTION 'DYNP_VALUES_READ'
    EXPORTING
      dyname             = dyname
      dynumb             = dynumb
      translate_to_upper = 'X'
    TABLES
      dynpfields         = dynpfields
    EXCEPTIONS
      OTHERS             = 9.

  PERFORM get_reason_for_possible_entry.

  f-reptext_ddic  = 'Reason'.
  f-fieldname = 'KZUST'.
  f-outputlen = 3.
  APPEND f TO popup_fieldcat.
  CLEAR f.

  f-reptext_ddic = 'Desc.'.
  f-fieldname = 'TEXT'.
  DESCRIBE FIELD fields_tab-text LENGTH f-outputlen.
  APPEND f TO popup_fieldcat.

* Excluding-Table
  APPEND: '%SC ' TO exc_exctab,       " Search
          '%SC+' TO exc_exctab,       " Search+
          '&OUP' TO exc_exctab,       " Sort Up
          '&ODN' TO exc_exctab,       " Sort Dn
          '&ILT' TO exc_exctab,       " Filter
          '&OL0' TO exc_exctab.

* Popup
  tabix = sy-tabix.

  LOOP AT gt_rsn.
    fields_tab-kzust = gt_rsn-kzust.
    fields_tab-text = gt_rsn-text.
    APPEND fields_tab.
    CLEAR fields_tab.
  ENDLOOP.

  CALL FUNCTION 'REUSE_ALV_POPUP_TO_SELECT'
    EXPORTING
      i_linemark_fieldname    = 'COLOR'
      i_tabname               = 'FIELDS_TAB'
      it_fieldcat             = popup_fieldcat
      i_callback_user_command = 'USER_COMMAND_POPUP_LIGHTS_N'
      i_callback_program      = dyname
      it_excluding            = exc_exctab
    IMPORTING
      es_selfield             = selfield
      e_exit                  = exitfield
    TABLES
      t_outtab                = fields_tab.

  READ TABLE fields_tab INDEX tabix.
  CLEAR fields_tab-color.
  MODIFY fields_tab INDEX tabix.

  IF exitfield IS INITIAL.
    READ TABLE fields_tab INDEX selfield-tabindex.
    p_rsn = fields_tab-kzust.

    dynpfields-fieldname = p_fieldname.
    dynpfields-fieldvalue = fields_tab-kzust.
    APPEND dynpfields.

    dyname = sy-repid.
    dynumb = sy-dynnr.

    CALL FUNCTION 'DYNP_VALUES_UPDATE'
      EXPORTING
        dyname     = dyname
        dynumb     = dynumb
      TABLES
        dynpfields = dynpfields.

  ENDIF.

ENDFORM.                    " POPUP_RSN

*&---------------------------------------------------------------------*
*       Possible Enter for Reason in ALV Grid
*----------------------------------------------------------------------*
FORM f4_reason USING e_fieldname   TYPE lvc_fname.
* Fill internal table for possible entry
  CLEAR  : gt_values, gt_fields.
  REFRESH: gt_values, gt_fields.

  PERFORM get_reason_for_possible_entry.

  LOOP AT gt_rsn.
    gt_values-string = gt_rsn-kzust.
    APPEND gt_values.

    gt_values-string = gt_rsn-text.
    APPEND gt_values.
  ENDLOOP.

  CLEAR gt_fields.
  REFRESH gt_fields.

  gt_fields-fieldname = e_fieldname.
  gt_fields-position  = 1.
  gt_fields-intlen    = 3.
  gt_fields-outputlen = 3.
  gt_fields-reptext   = 'Reason Code'.
  APPEND gt_fields.
  CLEAR gt_fields.

  gt_fields-fieldname = 'TEXT'.
  gt_fields-position  = 2.
  gt_fields-intlen    = 50.
  gt_fields-outputlen = 50.
  gt_fields-reptext = 'Desc.'.
  APPEND gt_fields.
  CLEAR gt_fields.

ENDFORM.                                                    " F4_REASON

*&---------------------------------------------------------------------*
*&      Form  GET_REASON_FOR_POSSIBLE_ENTRY
*&---------------------------------------------------------------------*
*       Get Posssible entry data for reason code
*----------------------------------------------------------------------*
FORM get_reason_for_possible_entry.
  TYPES: BEGIN OF ty_t686d,
           kzust TYPE kzust,
           vtext TYPE vtext,
         END OF ty_t686d.

  DATA lt_t686d TYPE TABLE OF ty_t686d WITH HEADER LINE.

  CLEAR: gt_ztcoum02, lt_t686d, gt_rsn.
  REFRESH: gt_ztcoum02, lt_t686d, gt_rsn.

  SELECT rgrp2 text INTO TABLE gt_ztcoum02
    FROM ztcoum02
   WHERE grp1 <> 'Z'.

  LOOP AT gt_ztcoum02.
    gt_rsn-kzust = gt_ztcoum02-rgrp2.
    gt_rsn-text  = gt_ztcoum02-text.

    IF gt_ztcoum02-rgrp2 <> 'XX'.
      APPEND gt_rsn.
    ENDIF.

*proxy price
    CONCATENATE 'X' gt_ztcoum02-rgrp2 INTO gt_rsn-kzust.
    APPEND gt_rsn.

    CLEAR gt_rsn.
  ENDLOOP.

*  SELECT KZUST VTEXT
*    INTO TABLE LT_T686D
*    FROM T686D
*   WHERE SPRAS = SY-LANGU
*     AND KZUST LIKE 'X%'.
*
*  LOOP AT LT_T686D.
*    GT_RSN-KZUST = LT_T686D-KZUST.
*    GT_RSN-TEXT  = LT_T686D-VTEXT.
*
*    APPEND GT_RSN.
*    CLEAR GT_RSN.
*  ENDLOOP.

  SORT gt_rsn BY kzust.

ENDFORM.                    " GET_REASON_FOR_POSSIBLE_ENTRY
*&---------------------------------------------------------------------*
*&      Form  ALV_VARIANT_F4
*&---------------------------------------------------------------------*
*FORM alv_variant_f4 CHANGING p_vari.
*  DATA: rs_variant LIKE disvariant,
*        lv_nof4 TYPE c.
*
*  CLEAR lv_nof4.
*  LOOP AT SCREEN.
*    IF screen-name = 'PA_VARI'.
*      IF screen-input = 0.
*        lv_nof4 = 'X'.
*      ENDIF.
*    ENDIF.
*  ENDLOOP.
*
*  CLEAR rs_variant.
*  rs_variant-report   = sy-repid.
*  rs_variant-username = sy-uname.
*
*  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
*    EXPORTING
*      is_variant = rs_variant
*      i_save     = 'A'
*    IMPORTING
*      es_variant = rs_variant
*    EXCEPTIONS
*      OTHERS     = 1.
*
*  IF sy-subrc = 0 AND lv_nof4 = space.
*    p_vari = rs_variant-variant.
*  ENDIF.
*
*ENDFORM.                    " ALV_VARIANT_F4
