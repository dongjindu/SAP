
DATA: g_temp_ok_code TYPE sy-ucomm.
*----------------------------------------------------------------------*
*   INCLUDE ZRPP_COMMON_ALVC                                           *
*----------------------------------------------------------------------*
CLASS lcl_application01    DEFINITION DEFERRED.        " TREE

*----------------------------------------------------------------------*
*   INCLUDE TREE_EVENT                                                 *
*----------------------------------------------------------------------*
CLASS lcl_application01 DEFINITION.

  PUBLIC SECTION.
*   double click item
    METHODS handle_double_click
      FOR EVENT node_double_click OF cl_gui_alv_tree
      IMPORTING node_key.
*   Drag

ENDCLASS.                    "LCL_APPLICATION01 DEFINITION
*---------------------------------------------------------------------*
*       CLASS CL_TREE_EVENT_RECEIVER IMPLEMENTATION
*---------------------------------------------------------------------*
CLASS lcl_application01 IMPLEMENTATION.
* handle double_click

  METHOD handle_double_click.
    CHECK NOT node_key IS INITIAL.
  ENDMETHOD.                    "HANDLE_DOUBLE_CLICK



ENDCLASS.                    "LCL_APPLICATION01 IMPLEMENTATION


*DATA: TREE_EVENT_RECEIVER TYPE REF TO CL_TREE_EVENT_RECEIVER.

CLASS cl_gui_cfw         DEFINITION LOAD.

*---------------------------------------------------------------------*
*       CLASS LCL_EVENTS DEFINITION
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
CLASS lcl_events DEFINITION.
  PUBLIC SECTION.
    DATA: mr_data_changed TYPE REF TO cl_alv_changed_data_protocol.

    METHODS:
    user_command         FOR EVENT user_command
                         OF cl_gui_alv_grid
                         IMPORTING e_ucomm sender,
    before_user_command  FOR EVENT before_user_command
                         OF cl_gui_alv_grid
                         IMPORTING e_ucomm,
    after_user_command   FOR EVENT after_user_command
                         OF cl_gui_alv_grid
                         IMPORTING e_ucomm,
    double_click         FOR EVENT double_click
                         OF cl_gui_alv_grid
                         IMPORTING e_row
                                   e_column
                                   es_row_no,
    hotspot_click        FOR EVENT hotspot_click
                         OF cl_gui_alv_grid
                         IMPORTING e_row_id
                                   e_column_id
                                   es_row_no,
    menu_button          FOR EVENT menu_button
                         OF cl_gui_alv_grid
                         IMPORTING e_object
                                   e_ucomm,
    toolbar              FOR EVENT toolbar
                         OF cl_gui_alv_grid
                         IMPORTING e_object
                                   e_interactive,
    context_menu_request FOR EVENT context_menu_request
                         OF cl_gui_alv_grid
                         IMPORTING e_object,
    top_of_page          FOR EVENT top_of_page
                         OF cl_gui_alv_grid
                         IMPORTING e_dyndoc_id,
    end_of_list          FOR EVENT end_of_list
                         OF cl_gui_alv_grid
                         IMPORTING e_dyndoc_id,
    print_top_of_page    FOR EVENT print_top_of_page
                         OF cl_gui_alv_grid,
    print_end_of_page    FOR EVENT print_end_of_page
                         OF cl_gui_alv_grid,
    print_top_of_list    FOR EVENT print_top_of_list
                         OF cl_gui_alv_grid,
    print_end_of_list    FOR EVENT print_end_of_list
                         OF cl_gui_alv_grid,
    after_refresh        FOR EVENT after_refresh
                         OF cl_gui_alv_grid,
    delayed_callback     FOR EVENT delayed_callback
                         OF cl_gui_alv_grid,
    delayed_changed_sel_callback
                         FOR EVENT delayed_changed_sel_callback
                         OF cl_gui_alv_grid,
    subtotal_text        FOR EVENT subtotal_text
                         OF cl_gui_alv_grid
                         IMPORTING es_subtottxt_info
                                   ep_subtot_line
                                   e_event_data,
    ondrag               FOR EVENT ondrag
                         OF cl_gui_alv_grid
                         IMPORTING e_row
                                   e_column
                                   es_row_no
                                   e_dragdropobj,
    ondrop               FOR EVENT ondrop
                         OF cl_gui_alv_grid
                         IMPORTING e_row
                                   e_column
                                   es_row_no
                                   e_dragdropobj,
    ondropcomplete       FOR EVENT ondropcomplete
                         OF cl_gui_alv_grid
                         IMPORTING e_row
                                   e_column
                                   es_row_no
                                   e_dragdropobj,
    ondropgetflavor      FOR EVENT ondropgetflavor
                         OF cl_gui_alv_grid
                         IMPORTING e_row
                                   e_column
                                   es_row_no
                                   e_dragdropobj
                                   e_flavors,
    data_changed         FOR EVENT data_changed
                         OF cl_gui_alv_grid
                         IMPORTING er_data_changed
                                   e_onf4
                                   e_onf4_before
                                   e_onf4_after,
    data_changed_finished
                         FOR EVENT data_changed_finished
                         OF cl_gui_alv_grid,
    button_click         FOR EVENT button_click
                         OF cl_gui_alv_grid
                         IMPORTING es_col_id
                                   es_row_no,
    onf1                 FOR EVENT onf1
                         OF cl_gui_alv_grid
                         IMPORTING e_fieldname
                                   es_row_no
                                   er_event_data,
    onf4                 FOR EVENT onf4
                         OF cl_gui_alv_grid
                         IMPORTING e_fieldname
                                   e_fieldvalue
                                   es_row_no
                                   er_event_data
                                   et_bad_cells
                                   e_display.

    METHODS : on_f4 FOR EVENT onf4 OF cl_gui_alv_grid
      IMPORTING sender
                e_fieldname
                e_fieldvalue
                es_row_no
                er_event_data
                et_bad_cells
                e_display.

    TYPES : ddshretval_table TYPE TABLE OF ddshretval.

    METHODS : my_f4
          IMPORTING sender         TYPE REF TO cl_gui_alv_grid
                    et_bad_cells   TYPE lvc_t_modi
                    es_row_no      TYPE lvc_s_roid
                    er_event_data  TYPE REF TO cl_alv_event_data
                    e_display      TYPE c
                    e_fieldname    TYPE lvc_fname
          EXPORTING lt_f4          TYPE ddshretval_table.

ENDCLASS.                    "lcl_events DEFINITION
*---------------------------------------------------------------------*
*       CLASS lcl_events_d0100 IMPLEMENTATION
*---------------------------------------------------------------------*
CLASS lcl_events IMPLEMENTATION.

*---------------------------------------------------------------
*_METHOD user_command
  METHOD user_command.

    PERFORM event_ucomm IN PROGRAM (sy-cprog)
            USING e_ucomm 'X' .

  ENDMETHOD.                    "user_command

*---------------------------------------------------------------
*_METHOD before_user_command
  METHOD before_user_command.
*    PERFORM D0100_EVENT_BEFORE_UCOMM
*            USING E_UCOMM.
  ENDMETHOD.                    "before_user_command

*---------------------------------------------------------------
*_METHOD after_user_command
  METHOD after_user_command.
*    PERFORM D0100_EVENT_AFTER_UCOMM
*            USING E_UCOMM.
  ENDMETHOD.                    "after_user_command

*---------------------------------------------------------------
*_METHOD double_click
  METHOD double_click.
*    PERFORM D0100_EVENT_DOUBLE_CLICK
*            USING E_ROW
*            E_COLUMN.
    PERFORM event_double_click
            IN PROGRAM (sy-cprog)
            USING e_row
                  e_column
                  es_row_no.                   .
  ENDMETHOD.                    "double_click

*---------------------------------------------------------------
*_METHOD hotspot_click
  METHOD hotspot_click.
    PERFORM event_hotspot_click
            USING e_row_id
                  e_column_id.
  ENDMETHOD.                    "hotspot_click

*---------------------------------------------------------------
*_METHOD menu_button
  METHOD menu_button.
*    PERFORM D0100_EVENT_MENU_BUTTON
*            USING E_OBJECT
*                  E_UCOMM.
  ENDMETHOD.                    "menu_button

*---------------------------------------------------------------
*_METHOD toolbar
  METHOD toolbar.
    PERFORM event_toolbar
            IN PROGRAM (sy-cprog)
            USING e_object
                  e_interactive
                  'X'.
  ENDMETHOD.                    "toolbar

*---------------------------------------------------------------
*_METHOD context_menu_request
  METHOD context_menu_request.
*    PERFORM D0100_EVENT_CONTEXT_MENU_REQST
*            USING E_OBJECT.
  ENDMETHOD.                    "context_menu_request

*---------------------------------------------------------------
*_METHOD top_of_page
  METHOD top_of_page.
*    PERFORM D0100_EVENT_TOP_OF_PAGE
*            USING E_DYNDOC_ID.
  ENDMETHOD.                    "top_of_page

*---------------------------------------------------------------
*_METHOD end_of_list
  METHOD end_of_list.
*    PERFORM D0100_EVENT_END_OF_LIST
*            USING E_DYNDOC_ID.
  ENDMETHOD.                    "end_of_list

*---------------------------------------------------------------
*_METHOD print_top_of_page
  METHOD print_top_of_page.
*    PERFORM D0100_EVENT_PRINT_TOP_OF_PAGE.
  ENDMETHOD.                    "print_top_of_page

*---------------------------------------------------------------
*_METHOD print_end_of_page
  METHOD print_end_of_page.
*    PERFORM D0100_EVENT_PRINT_END_OF_PAGE.
  ENDMETHOD.                    "print_end_of_page

*---------------------------------------------------------------
*_METHOD print_top_of_list
  METHOD print_top_of_list.
*    PERFORM D0100_EVENT_PRINT_TOP_OF_LIST.
  ENDMETHOD.                    "print_top_of_list

*---------------------------------------------------------------
*_METHOD print_end_of_list
  METHOD print_end_of_list.
*    PERFORM D0100_EVENT_PRINT_END_OF_LIST.
  ENDMETHOD.                    "print_end_of_list

*---------------------------------------------------------------
*_METHOD after_refresh
  METHOD after_refresh.
    PERFORM event_after_refresh
            IN PROGRAM (sy-cprog)
            USING 'X'.
  ENDMETHOD.                    "after_refresh

*---------------------------------------------------------------
*_METHOD delayed_callback
  METHOD delayed_callback.
*    PERFORM D0100_EVENT_DELAYED_CALLBACK.
  ENDMETHOD.                    "delayed_callback

*---------------------------------------------------------------
*_METHOD delayed_changed_sel_callba
  METHOD delayed_changed_sel_callback.
*    PERFORM D0100_EVENT_CHANGED_SEL_CALLBA.
  ENDMETHOD.                    "delayed_changed_sel_callback

*---------------------------------------------------------------
*_METHOD subtotal_text
  METHOD subtotal_text.
*    PERFORM D0100_EVENT_SUBTOTAL_TEXT
*            USING ES_SUBTOTTXT_INFO
*                  EP_SUBTOT_LINE
*                  E_EVENT_DATA.
  ENDMETHOD.                    "subtotal_text

*---------------------------------------------------------------
*_METHOD ondrag
  METHOD ondrag.
*    PERFORM D0100_EVENT_ONDRAG
*            USING E_ROW
*                  E_COLUMN
*                  E_DRAGDROPOBJ.
  ENDMETHOD.                    "ondrag

*---------------------------------------------------------------
*_METHOD ondrop
  METHOD ondrop.
*    PERFORM D0100_EVENT_ONDROP
*            USING E_ROW
*            E_COLUMN
*            E_DRAGDROPOBJ.
  ENDMETHOD.                    "ondrop

*---------------------------------------------------------------
*_METHOD ondropcomplete
  METHOD ondropcomplete.
*    PERFORM D0100_EVENT_ONDROPCOMPLETE
*            USING E_ROW
*                  E_COLUMN
*                  E_DRAGDROPOBJ.
  ENDMETHOD.                    "ondropcomplete

*---------------------------------------------------------------
*_METHOD ondropgetflavor
  METHOD ondropgetflavor.

  ENDMETHOD.                    "ondropgetflavor

*---------------------------------------------------------------
*_METHOD data_changed
  METHOD data_changed.
    PERFORM event_data_changed
            IN PROGRAM (sy-cprog)
             USING er_data_changed
                   e_onf4
                   e_onf4_before
                   e_onf4_after
                   'X' .
  ENDMETHOD.                    "data_changed

*---------------------------------------------------------------
*       METHOD data_changed_finished
  METHOD data_changed_finished.
    PERFORM event_data_changed_finis
            IN PROGRAM (sy-cprog)
            USING 'X'.
  ENDMETHOD.                    "data_changed_finished

*---------------------------------------------------------------
*_METHOD button_click
  METHOD button_click.
*    PERFORM D0100_EVENT_BUTTON_CLICK
*            USING ES_COL_ID
*                  ES_ROW_NO.
  ENDMETHOD.                    "button_click

*---------------------------------------------------------------
*_METHOD onf1
  METHOD onf1.
*    PERFORM D0100_EVENT_ONF1
*            USING E_FIELDNAME
*                  ES_ROW_NO
*                  ER_EVENT_DATA.
  ENDMETHOD.                                                "ONF1

*---------------------------------------------------------------
*_METHOD onf4
  METHOD onf4.
    PERFORM event_onf4
            IN PROGRAM (sy-cprog)
            USING e_fieldname
                  e_fieldvalue
                  es_row_no
                  er_event_data
                  et_bad_cells
                  e_display
                  'X'      .
  ENDMETHOD.                                                "ONF4

*---------------------------------------------------------------
  METHOD on_f4.
    PERFORM event_on_f4
            IN PROGRAM (sy-cprog)
            USING sender
                 e_fieldname
                 e_fieldvalue
                 es_row_no
                 er_event_data
                 et_bad_cells
                 e_display
                 'X'.
  ENDMETHOD.                                                "ON_F4

*---------------------------------------------------------------
  METHOD my_f4.
    PERFORM event_my_f4
            IN PROGRAM (sy-cprog)
            TABLES lt_f4
            USING sender
                  et_bad_cells
                  es_row_no
                  er_event_data
                  e_display
                  e_fieldname.
  ENDMETHOD.                                                "MY_F4

ENDCLASS.                    "lcl_events_d0100 IMPLEMENTATION







*&**********************************************************************
*  CLASS DATA                                                          *
*&**********************************************************************
DATA : g_application01 TYPE REF TO lcl_application01.      " Tree

DATA : g_custom_container TYPE REF TO cl_gui_custom_container,
       g_alv_container    TYPE REF TO cl_gui_custom_container,

       g_docking_container     TYPE REF TO cl_gui_docking_container,
       g_docking_container_top TYPE REF TO cl_gui_docking_container,

       g_split_container  TYPE REF TO cl_gui_splitter_container,

       g_gui_container1   TYPE REF TO cl_gui_container ,
       g_gui_container2   TYPE REF TO cl_gui_container ,



       g_container        TYPE scrfname VALUE 'CONTAINER',
       g_grid             TYPE REF TO cl_gui_alv_grid,
       g_grid1            TYPE REF TO cl_gui_alv_grid,
       g_grid2            TYPE REF TO cl_gui_alv_grid,
       g_events           TYPE REF TO lcl_events,
       g_document    TYPE REF TO cl_dd_document.


* TABLE TYPE.
DATA :  gt_fieldcat    TYPE lvc_t_fcat,
        gt_fieldcat2    TYPE lvc_t_fcat,
        gt_sort        TYPE lvc_t_sort,
        gt_styl        TYPE lvc_t_styl,
        gt_tabcolor    TYPE lvc_t_scol,
        gt_excl_func   TYPE ui_functions.
DATA : g_field_ct      TYPE slis_t_fieldcat_alv.
* STRUCTURE TYPE
DATA :  gs_layout   TYPE lvc_s_layo,
        gs_sort     TYPE lvc_s_sort,
        gs_styl     TYPE lvc_s_styl,
        gs_tabcolor TYPE lvc_s_scol,
        gs_toolbar  TYPE stb_button,
        gs_curr_col       TYPE lvc_s_col,
        gs_curr_row       TYPE lvc_s_roid.

DATA :  gs_o_layout TYPE disvariant.      "for parameter IS_VARIANT


DATA : ok_code_c LIKE sy-ucomm ,
       g_change_data,
       g_error ,
       g_save_c  ,
       g_input TYPE i VALUE 0,
       g_answer,
       g_tot_cnt TYPE i,
       g_percent TYPE p,
       g_message(20)   .

DATA : g_repid_c TYPE sy-repid.

*_F4 ????
DATA : gs_f4        TYPE lvc_s_f4,
       gt_f4        TYPE lvc_t_f4.


* message popup.
*DATA : GT_MESSAGE_C TYPE BAPIRETTAB.
*DATA : GS_MESSAGE_C LIKE BAPIRET2.

* ????
DATA : g_erdat LIKE sy-datum,
       g_erzet LIKE sy-uzeit,
       g_ernam LIKE sy-uname.




*---------------------------------------------------------------
FIELD-SYMBOLS : <f4tab> TYPE lvc_t_modi.

*---------------------------------------------------------------
*------Possible Entry
DATA : lt_values TYPE TABLE OF seahlpres,
       lt_fields TYPE TABLE OF dfies,
       ls_value  TYPE seahlpres,
       ls_field  TYPE dfies,

       ls_f4     TYPE ddshretval,
       ls_modi   TYPE lvc_s_modi.

************************************************************************
* DEFINE...
************************************************************************
DEFINE d_toolbar.
  clear gs_toolbar.
  gs_toolbar-function  = &1.
  gs_toolbar-icon      = &2.
  gs_toolbar-butn_type = &3.
  gs_toolbar-disabled  = &4.
  gs_toolbar-text      = &5.
  gs_toolbar-quickinfo = &6.
  gs_toolbar-checked   = &7.
  append gs_toolbar to e_object->mt_toolbar.
END-OF-DEFINITION.



*&---------------------------------------------------------------------*
*&      Form  SET_SORT
*&---------------------------------------------------------------------*
FORM set_sort_c  USING  p_spos
                      p_field
                      p_up
                      p_down
                      p_group
                      p_subtot
                      p_comp
                      p_expa.

  DATA: ls_sort TYPE lvc_s_sort.
*
  ls_sort-spos      = p_spos .
  ls_sort-fieldname = p_field.
  ls_sort-up        = p_up.
  ls_sort-down      = p_down.
  ls_sort-group     = p_group.
  ls_sort-subtot    = p_subtot.
  ls_sort-comp      = p_comp.
  ls_sort-expa      = p_expa.
*
  INSERT ls_sort INTO TABLE gt_sort.

ENDFORM.                    " SET_SORT
*&---------------------------------------------------------------------*
*&      Form  SET_LAYOUT
*&---------------------------------------------------------------------*
FORM set_layout_c .
*  INPUT.
*  GS_LAYOUT-EDIT = 'X' .
  gs_layout-stylefname = 'H_STYLE'.
*  GS_LAYOUT-ZEBRA      = 'X'.
* B:single C:multi D:cell A:rowcol
  gs_layout-sel_mode   = 'B'.
* ROW COLOR
  gs_layout-info_fname = 'COLOR'.
* CELL COLOR
  gs_layout-ctab_fname = 'TABCOLOR'.
** BOX
*  GS_LAYOUT-BOX_FNAME  = 'MARK'.
* OPTIMAZE
  gs_layout-cwidth_opt = 'X'.
* Title
  gs_layout-grid_title = text-t01 .
ENDFORM.                    " SET_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  CALL_FIRST_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM call_first_display  TABLES   p_table .
*---------------------------------------------------------------
  CALL METHOD g_grid->set_table_for_first_display
    EXPORTING
      i_save               = 'A'
      i_default            = 'X'
      is_layout            = gs_layout
      is_variant           = gs_o_layout "&see below
      it_toolbar_excluding = gt_excl_func
    CHANGING
      it_fieldcatalog      = gt_fieldcat
      it_sort              = gt_sort
      it_outtab            = p_table[].

ENDFORM.                    " CALL_FIRST_DISPLAY

*&---------------------------------------------------------------------*
*&      Form  CALL_FIRST_DISPLAY_1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM call_first_display_1  TABLES   p_table .
*---------------------------------------------------------------
  CALL METHOD g_grid->set_table_for_first_display
    EXPORTING
      i_save               = 'A'
      i_default            = 'X'
      is_layout            = gs_layout
      is_variant           = gs_o_layout "&see below
      it_toolbar_excluding = gt_excl_func
    CHANGING
      it_fieldcatalog      = gt_fieldcat
      it_sort              = gt_sort
      it_outtab            = p_table[].

ENDFORM.                    " CALL_FIRST_DISPLAY_1

*&---------------------------------------------------------------------*
*&      Form  SET_STYLE
*&---------------------------------------------------------------------*
*       CELL EDIT.
*----------------------------------------------------------------------*
FORM set_style USING pt_styl TYPE lvc_t_styl
                     p_field
                     p_maxlen.
  CLEAR : gs_styl.
  gs_styl-fieldname  = p_field.
  gs_styl-style      = cl_gui_alv_grid=>mc_style_enabled.
  gs_styl-maxlen     = p_maxlen.
  INSERT gs_styl INTO TABLE pt_styl .
*
ENDFORM.                    " SET_STYLE
*&---------------------------------------------------------------------*
*&      Form  SET_CEL_COLOR
*&---------------------------------------------------------------------*
*       CELL COLOR
*----------------------------------------------------------------------*
FORM set_cel_color  USING    pt_tablcolor TYPE lvc_t_scol
                             p_field
                             p_color.
*
  gs_tabcolor-fname     = p_field.
  gs_tabcolor-color-col = p_color+0(1).
  gs_tabcolor-color-int = p_color+1(1).
  gs_tabcolor-color-inv = p_color+2(1).
  gs_tabcolor-nokeycol  = 'X'.
  INSERT gs_tabcolor INTO TABLE gt_tabcolor.
ENDFORM.                    " SET_CEL_COLOR

*&---------------------------------------------------------------------*
*&      Form  SET_GRID_TOOLBAR
*&---------------------------------------------------------------------*
FORM set_grid_toolbar CHANGING ct_excl_func
                                              TYPE ui_functions.
  DATA: ls_func TYPE lvc_s_excl.

  APPEND cl_gui_alv_grid=>mc_fc_excl_all TO ct_excl_func.
*
*  LOOP AT GS_GRID_OPT-TOOLBAR-TOOLBAR_EXCL_FUNC INTO LS_FUNC.
*    APPEND LS_FUNC TO CT_EXCL_FUNC.
*  ENDLOOP.
ENDFORM.                    " SET_GRID_TOOLBAR
*&---------------------------------------------------------------------*
*&      Form  SET_GRID_TOOLBAR
*&---------------------------------------------------------------------*
FORM set_grid_toolbar_con1 CHANGING ct_excl_func
                                              TYPE ui_functions.
  DATA: ls_func TYPE lvc_s_excl.

  LOOP AT gt_excl_func INTO ls_func.
    APPEND ls_func TO ct_excl_func.
  ENDLOOP.
*  APPEND CL_GUI_ALV_GRID=>MC_FC_EXCL_ALL TO CT_EXCL_FUNC.

ENDFORM.                    " SET_GRID_TOOLBAR

*&---------------------------------------------------------------------*
*&      Form  REFRESH_DISPLAY
*&---------------------------------------------------------------------*
FORM refresh_display .
  DATA : l_scroll TYPE lvc_s_stbl.
  l_scroll-row = 'X'.
  l_scroll-col = 'X'.

  CALL METHOD g_grid->refresh_table_display      "Victor 06.27.2011
    EXPORTING
      is_stable      =  l_scroll
      i_soft_refresh =  'X'     .              .


**  SCROLL ????? ??.
*  CALL METHOD G_GRID->GET_SCROLL_INFO_VIA_ID
*    IMPORTING
*      ES_COL_INFO = GS_CURR_COL
*      ES_ROW_NO   = GS_CURR_ROW.
** Layout
*  CALL METHOD G_GRID->SET_FRONTEND_LAYOUT
*    EXPORTING
*      IS_LAYOUT = GS_LAYOUT.
**  REFRESH DISPLAY
*  CALL METHOD G_GRID->REFRESH_TABLE_DISPLAY.
**REFRESH ? SCROLL ??? ??.
*  CALL METHOD G_GRID->SET_SCROLL_INFO_VIA_ID
*    EXPORTING
*      IS_COL_INFO = GS_CURR_COL
*      IS_ROW_NO   = GS_CURR_ROW.		

ENDFORM.

*---------------------------------------------------------------------*
*       FORM REFRESH_DISPLAY_200                                      *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM refresh_display_200 .
*  SCROLL ????? ??.
  CALL METHOD g_grid1->get_scroll_info_via_id
    IMPORTING
      es_col_info = gs_curr_col
      es_row_no   = gs_curr_row.
* Layout
  CALL METHOD g_grid1->set_frontend_layout
    EXPORTING
      is_layout = gs_layout.
*  REFRESH DISPLAY
  CALL METHOD g_grid1->refresh_table_display.
*REFRESH ? SCROLL ??? ??.
  CALL METHOD g_grid1->set_scroll_info_via_id
    EXPORTING
      is_col_info = gs_curr_col
      is_row_no   = gs_curr_row.

ENDFORM.                    " REFRESH_DISPLAY

*&---------------------------------------------------------------------*
*&      Form  POPUP_TO_CONFIRM_1
*&---------------------------------------------------------------------*
FORM popup_to_confirm_1 USING p_default
                            p_title
                            p_text1
                            p_text2
                            p_display
                      CHANGING p_answer.

  CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
       EXPORTING
            defaultoption  = p_default
            textline1      = p_text1
            textline2      = p_text2
            titel          = p_title
            cancel_display = p_display
       IMPORTING
            answer         = p_answer
       EXCEPTIONS
            text_not_found.

ENDFORM.                    " POPUP_TO_CONFIRM_1
*FORM POPUP_TO_CONFIRM_1 USING P_DEFAULT
*                            P_TITLE
*                            P_TEXT1
*                            P_TEXT2
*                            P_DISPLAY
*                      CHANGING P_ANSWER.
*
*  CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
*    EXPORTING
*       DEFAULTOPTION           = P_DEFAULT
*       TEXTLINE1               = P_TEXT1
*       TEXTLINE2               = P_TEXT2
*       TITEL                   = P_TITLE
*       CANCEL_DISPLAY          = P_DISPLAY
*    IMPORTING
*       ANSWER                  = P_ANSWER
*    EXCEPTIONS
*        TEXT_NOT_FOUND                      .
*
*ENDFORM.                    " POPUP_TO_CONFIRM

*&---------------------------------------------------------------------*
*&      Form  SET_LAYOUT_CON
*&---------------------------------------------------------------------*
FORM set_layout_con USING p_style
                          p_edit
                          p_sel
                          p_stylefname
                          p_color
                          p_tabc
                          p_box
                          p_opt.
*_Lights
  IF p_style = 'LIGHT'.
    gs_layout-excp_fname = p_style.
  ELSE.
    gs_layout-stylefname = p_style.
  ENDIF.

*_INPUT.
  gs_layout-edit = p_edit .

*  GS_LAYOUT-ZEBRA      = 'X'.

  gs_layout-sel_mode   = p_sel.

*_
  gs_layout-stylefname = p_stylefname.

*_ROW COLOR
  gs_layout-info_fname = p_color.

*_CELL COLOR
  gs_layout-ctab_fname = p_tabc .

*_BOX
  gs_layout-box_fname  = p_box.

*_OPTIMAZE
  gs_layout-cwidth_opt = p_opt.

*_Title
  gs_layout-grid_title = text-t01 .

ENDFORM.                    " SET_LAYOUT_CON
*&---------------------------------------------------------------------*
*&      Form  SET_INPUT_MET_CON
*&---------------------------------------------------------------------*
FORM set_input_met_con
     USING  p_gb
            p_in.

  IF p_gb EQ 'X'.
*___ENTER EVENT
    CALL METHOD g_grid->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.
  ELSE.
*___DATA MODIFY EVENT
    CALL METHOD g_grid->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.
  ENDIF.

*_INPUT EVENT
  CALL METHOD g_grid->set_ready_for_input
    EXPORTING
      i_ready_for_input = p_in.

ENDFORM.                    " SET_INPUT_MET_CON
**&---------------------------------------------------------------------
*
**&      Form  SET_GRID_TOOLBAR_CON
**&---------------------------------------------------------------------
*
FORM set_grid_toolbar_con
     CHANGING ct_excl_func TYPE ui_functions.


  PERFORM local_tool_con IN PROGRAM (sy-cprog)
          CHANGING ct_excl_func.


ENDFORM.                    " SET_GRID_TOOLBAR
*&---------------------------------------------------------------------*
*&      Form  FREE_RTN
*&---------------------------------------------------------------------*
FORM free_rtn .
  CALL METHOD g_grid->free.
  CALL METHOD g_custom_container->free.
  CALL METHOD cl_gui_cfw=>flush.
  CLEAR : g_custom_container.
ENDFORM.                    " FREE_RTN
*&---------------------------------------------------------------------*
*&      Form  PROGRESS_BAR
*&---------------------------------------------------------------------*
FORM progress_bar USING p_tot_cnt .

  g_percent = ( sy-tabix / g_tot_cnt ) * 100.
  MOVE : g_percent TO g_message+0(3).
  g_message+3(17) = '% ?? ???...' .
*
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
       EXPORTING
            percentage = g_percent
            text       = g_message
       EXCEPTIONS
            OTHERS     = 1.
ENDFORM.                    " PROGRESS_BAR
*&---------------------------------------------------------------------*
*&      Form  SET_INPUT_CON
*&---------------------------------------------------------------------*
FORM set_input_con USING    p_grid TYPE REF TO cl_gui_alv_grid
                            p_gb
                            p_in.

  IF p_gb EQ 'X'.
*___ENTER EVENT
    CALL METHOD p_grid->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.
  ELSE.
*___DATA MODIFY EVENT
    CALL METHOD p_grid->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.
  ENDIF.

*__INPUT EVENT
  CALL METHOD p_grid->set_ready_for_input
    EXPORTING
      i_ready_for_input = p_in.


ENDFORM.                    " SET_INPUT_CON
*&---------------------------------------------------------------------*
*&      Form  CALL_GRID_DISPLAY
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  CALL_GRID_DISPLAY_1
*&---------------------------------------------------------------------*
FORM call_grid_display    TABLES   p_table
                          USING    p_grid TYPE REF TO cl_gui_alv_grid
                          .

  CALL METHOD p_grid->set_table_for_first_display
    EXPORTING
      i_save               = 'A'
      i_default            = 'X'
      is_layout            = gs_layout
      is_variant           = gs_o_layout
      it_toolbar_excluding = gt_excl_func
    CHANGING
      it_fieldcatalog      = gt_fieldcat
      it_sort              = gt_sort
      it_outtab            = p_table[].

ENDFORM.                    " CALL_GRID_DISPLAY
*&---------------------------------------------------------------------*
*&      Form  CALL_GRID_DISPLAY
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  CALL_GRID_DISPLAY_1
*&---------------------------------------------------------------------*
FORM call_grid_display_fcat    TABLES   p_table
                          USING    p_grid TYPE REF TO cl_gui_alv_grid
                                   p_fieldcat
                          .

  CALL METHOD p_grid->set_table_for_first_display
    EXPORTING
      i_save               = 'A'
      i_default            = 'X'
      is_layout            = gs_layout
      is_variant           = gs_o_layout
      it_toolbar_excluding = gt_excl_func
    CHANGING
      it_fieldcatalog      = p_fieldcat
      it_sort              = gt_sort
      it_outtab            = p_table[].

ENDFORM.                    " CALL_GRID_DISPLAY
*&---------------------------------------------------------------------*
*&      Form  ALV_MODIFY_CELL
*&---------------------------------------------------------------------*
FORM alv_modify_cell_c  USING rr_data_changed  TYPE REF TO
                                      cl_alv_changed_data_protocol
                            p_index TYPE sy-tabix p_field p_value.

  CALL METHOD rr_data_changed->modify_cell
    EXPORTING
      i_row_id    = p_index
      i_fieldname = p_field
      i_value     = p_value.

ENDFORM.                    " ALV_MODIFY_CELL
*&---------------------------------------------------------------------*
*&      Form  MULTI_MSG_POPUP_CLASS
*&---------------------------------------------------------------------*
FORM multi_msg_popup_class  USING pt_message ."TYPE BAPIRETTAB.
  CALL FUNCTION 'OXT_MESSAGE_TO_POPUP'
       EXPORTING
            it_message = pt_message
       EXCEPTIONS
            bal_error  = 1
            OTHERS     = 2.
ENDFORM.                    " MULTI_MSG_POPUP_CLASS
*&---------------------------------------------------------------------*
*&      Form  MOVE_MSG_CLASS
*&---------------------------------------------------------------------*
FORM move_msg_class  USING    p_type
                              p_id
                              p_num
                              p_v1
                              p_v2
                              p_v3
                              p_v4.

*  MOVE: P_TYPE TO GS_MESSAGE_C-TYPE ,
*        P_ID   TO GS_MESSAGE_C-ID ,
*        P_NUM  TO GS_MESSAGE_C-NUMBER ,
*        P_V1   TO GS_MESSAGE_C-MESSAGE_V1 ,
*        P_V2   TO GS_MESSAGE_C-MESSAGE_V2 ,
*        P_V3   TO GS_MESSAGE_C-MESSAGE_V3 ,
*        P_V4   TO GS_MESSAGE_C-MESSAGE_V4 .
*  INSERT GS_MESSAGE_C INTO TABLE GT_MESSAGE_C.

ENDFORM.                    " MOVE_MSG_CLASS
*&---------------------------------------------------------------------*
*&      Form  pro_apply_change
*&---------------------------------------------------------------------*
FORM pro_apply_change.
*desc: Verification of Changes and Triggering of Event DATA_CHANGED
  CALL METHOD g_grid->check_changed_data.
  CALL METHOD cl_gui_cfw=>flush.
ENDFORM.                    " pro_apply_change
