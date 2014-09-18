*&---------------------------------------------------------------------*
*&  Include           ZLER0026_CLASS                                   *
*&---------------------------------------------------------------------*
DATA : BEGIN OF g_alv1_t OCCURS 0.
        INCLUDE STRUCTURE g_header_t.
DATA :   f_col    TYPE lvc_t_scol,
         celltab TYPE lvc_t_styl,
       END OF g_alv1_t,
       g_alv1_s LIKE g_alv1_t.

DATA : BEGIN OF g_alv2_t OCCURS 0.
        INCLUDE STRUCTURE g_detail_t.
DATA :   flag,
         f_col    TYPE lvc_t_scol,
         celltab TYPE lvc_t_styl,
       END OF g_alv2_t,
       g_alv2_s LIKE g_alv2_t.

*DATA : BEGIN OF g_alv3_t OCCURS 0.
*        INCLUDE STRUCTURE g_detail2_t.
*DATA :   alv_col TYPE slis_t_specialcol_alv,
*       END OF g_alv3_t,
*       g_alv3_s LIKE g_alv3_t.

CLASS lcl_event_receiver  DEFINITION DEFERRED.

*----------------------------------------------------------------------*
* DOCK DATA DECLARATION
*----------------------------------------------------------------------*
DATA: splitter            TYPE REF TO cl_gui_splitter_container,
      container_spliter   TYPE REF TO cl_gui_custom_container,
      container_0         TYPE REF TO cl_gui_container,
      container_1         TYPE REF TO cl_gui_container,
      container_2         TYPE REF TO cl_gui_container.

** Docking Container
DATA: docking             TYPE REF TO cl_gui_docking_container.

*----------------------------------------------------------------------*
* ALV DATA DECLARATION
*----------------------------------------------------------------------*
DATA: grid1               TYPE REF TO cl_gui_alv_grid,      " ALV1
      grid2               TYPE REF TO cl_gui_alv_grid,      " ALV2
      grid3               TYPE REF TO cl_gui_alv_grid,      " ALV3
      gs_sort             TYPE lvc_s_sort,
      gt_sort             TYPE lvc_t_sort WITH HEADER LINE,
      gt_tbar_excluding1  TYPE ui_functions,
      gt_tbar_excluding2  TYPE ui_functions,
      gt_tbar_excluding3  TYPE ui_functions,
      gs_layout           TYPE lvc_s_layo,         " ALV_LAYOUT
      gs_layout2           TYPE lvc_s_layo,         " ALV_LAYOUT
      gt_lvc1             TYPE lvc_t_fcat,      " ALV FIELD ATALOGUE
*                          WITH HEADER LINE,
      gt_lvc2             TYPE lvc_t_fcat ,     " ALV FIELD CATALOGUE
*                          WITH HEADER LINE,
      gt_lvc3             TYPE lvc_t_fcat  ,    " ALV FIELD CATALOGUE
*                          WITH HEADER LINE,
      event_receiver      TYPE REF TO              " ALV EVENT
                          lcl_event_receiver.
*// ALV Variant
DATA: alv_variant   LIKE disvariant,
      alv_repid     LIKE sy-repid,
      g_repid      LIKE sy-repid.

DATA: gs_lvc1             TYPE lvc_s_fcat,         " ALV FIELD CATALOGUE
      gs_lvc2             TYPE lvc_s_fcat,         " ALV FIELD CATALOGUE
      gs_lvc3             TYPE lvc_s_fcat.         " ALV FIELD CATALOGUE

DATA: gt_rows             TYPE lvc_t_row WITH HEADER LINE.

*----------------------------------------------------------------------*
* CLASS Definition
*----------------------------------------------------------------------*
CLASS lcl_event_receiver DEFINITION.

  PUBLIC SECTION.
* 헤더.
    METHODS: handle_toolbar
             FOR EVENT toolbar OF cl_gui_alv_grid
             IMPORTING e_object e_interactive,

    handle_data_changed
             FOR EVENT data_changed OF cl_gui_alv_grid
             IMPORTING er_data_changed e_onf4 e_ucomm,

    handle_data_changed_finished
             FOR EVENT data_changed_finished OF cl_gui_alv_grid
             IMPORTING e_modified et_good_cells,

    handle_user_command
             FOR EVENT user_command OF cl_gui_alv_grid
             IMPORTING e_ucomm,

    handle_hotspot_click
             FOR EVENT hotspot_click OF cl_gui_alv_grid
             IMPORTING e_row_id e_column_id es_row_no,

    handle_double_click
             FOR EVENT double_click OF cl_gui_alv_grid
             IMPORTING e_row e_column,

    handle_onf4 FOR EVENT onf4 OF cl_gui_alv_grid
                  IMPORTING sender
                            e_fieldname
                            e_fieldvalue
                            es_row_no
                            er_event_data
                            et_bad_cells
                            e_display,

* 아이템.
    handle_toolbar2
             FOR EVENT toolbar OF cl_gui_alv_grid
             IMPORTING e_object e_interactive,

    handle_data_changed2
             FOR EVENT data_changed OF cl_gui_alv_grid
             IMPORTING er_data_changed e_onf4 e_ucomm,

    handle_data_changed_finished2
             FOR EVENT data_changed_finished OF cl_gui_alv_grid
             IMPORTING e_modified et_good_cells,

    handle_user_command2
             FOR EVENT user_command OF cl_gui_alv_grid
             IMPORTING e_ucomm,

    handle_onf4_2 FOR EVENT onf4 OF cl_gui_alv_grid
                  IMPORTING sender
                            e_fieldname
                            e_fieldvalue
                            es_row_no
                            er_event_data
                            et_bad_cells
                            e_display,

    handle_hotspot_click2
             FOR EVENT hotspot_click OF cl_gui_alv_grid
             IMPORTING e_row_id e_column_id es_row_no,

    handle_double_click2
             FOR EVENT double_click OF cl_gui_alv_grid
             IMPORTING e_row e_column.



ENDCLASS.                    "LCL_EVENT_RECEIVER DEFINITION

*---------------------------------------------------------------------*
*********> CLASS LCL_EVENT_RECEIVER IMPLEMENTATION
*---------------------------------------------------------------------*
CLASS lcl_event_receiver IMPLEMENTATION.

*---------------------------------------------------------------------*
*/ DEFINE TOOBAR
*---------------------------------------------------------------------*
  METHOD handle_toolbar.

    DATA: ls_toolbar  TYPE stb_button.
    CLEAR ls_toolbar.
    MOVE 3 TO ls_toolbar-butn_type.
    APPEND ls_toolbar   TO e_object->mt_toolbar.

    CLEAR ls_toolbar.
    MOVE 'SALL'               TO  ls_toolbar-function.
    MOVE icon_select_all      TO  ls_toolbar-icon.
    MOVE 'select_all'           TO  ls_toolbar-quickinfo.
    MOVE 'select_all'           TO  ls_toolbar-text.
    MOVE ' '                  TO  ls_toolbar-disabled.
    APPEND ls_toolbar         TO  e_object->mt_toolbar.

    IF p_r10 EQ 'X' OR
       p_r20 EQ 'X'.
      CLEAR ls_toolbar.
      MOVE 3 TO ls_toolbar-butn_type.
      APPEND ls_toolbar   TO e_object->mt_toolbar.
      CLEAR ls_toolbar.
      MOVE 'CHAN'         TO ls_toolbar-function.
      MOVE icon_change  TO ls_toolbar-icon.
      MOVE 'Min/Max'   TO ls_toolbar-quickinfo.
      MOVE 'Min/Max'   TO ls_toolbar-text.
      MOVE ' '            TO ls_toolbar-disabled.
      APPEND ls_toolbar   TO e_object->mt_toolbar.
    ENDIF.

    IF p_r10 EQ 'X' OR
       p_r40 EQ 'X'.
      CLEAR ls_toolbar.
      MOVE 3 TO ls_toolbar-butn_type.
      APPEND ls_toolbar   TO e_object->mt_toolbar.
      CLEAR ls_toolbar.
      MOVE 'ABC'         TO ls_toolbar-function.
      MOVE icon_change  TO ls_toolbar-icon.
      MOVE ' ABC'   TO ls_toolbar-quickinfo.
      MOVE ' ABC'   TO ls_toolbar-text.
      MOVE ' '            TO ls_toolbar-disabled.
      APPEND ls_toolbar   TO e_object->mt_toolbar.
    ENDIF.

  ENDMETHOD.                    "HANDLE_TOOLBAR

  METHOD handle_toolbar2.

    DATA: ls_toolbar  TYPE stb_button.

    IF p_r30 EQ 'X'.
      CLEAR ls_toolbar.
      MOVE 3 TO ls_toolbar-butn_type.
      APPEND ls_toolbar   TO e_object->mt_toolbar.

      CLEAR ls_toolbar.
      MOVE 'PREQ'               TO  ls_toolbar-function.
      MOVE icon_create         TO  ls_toolbar-icon.
      MOVE 'P/Req.'         TO  ls_toolbar-quickinfo.
      MOVE 'P/Req.'         TO  ls_toolbar-text.
      MOVE ' '                  TO  ls_toolbar-disabled.
      APPEND ls_toolbar         TO  e_object->mt_toolbar.
    ENDIF.

  ENDMETHOD.                    "HANDLE_TOOLBAR

*---------------------------------------------------------------------*
*/ HANDLE DATA CHANGED FINISHED
*---------------------------------------------------------------------*
** 헤더별 선택 제어
  METHOD  handle_data_changed_finished.

    DATA : ls_good_cells LIKE LINE OF et_good_cells.
    CLEAR : gv_index.

    READ TABLE et_good_cells INTO ls_good_cells INDEX 1.

    gv_index = ls_good_cells-row_id.

    IF  ls_good_cells-fieldname = 'BOX'.
      IF ls_good_cells-value = 'X'.
        PERFORM get_data_grid2.
      ELSE.
        PERFORM get_data_grid2_2.
      ENDIF.
*      PERFORM set_cell_attribute.
      PERFORM set_cell_attribute_2.

*      CALL METHOD grid1->refresh_table_display.
      CALL METHOD grid2->refresh_table_display.

    ELSEIF  ls_good_cells-fieldname  =  'MARK'.

      PERFORM set_cell_attribute.
      CALL METHOD grid2->refresh_table_display.
    ENDIF.


  ENDMETHOD.                    "HANDLE_DATA_CHANGED_FINISHED

  METHOD  handle_data_changed_finished2.

    DATA : ls_good_cells2 LIKE LINE OF et_good_cells.
    CLEAR : gv_index2.

    READ TABLE et_good_cells INTO ls_good_cells2 INDEX 1.

    gv_index2 = ls_good_cells2-row_id.

*    READ TABLE g_alv2_t INTO g_alv2_s INDEX gv_index2.
*
*    CHECK sy-subrc = 0.
*
*    IF  ls_good_cells2-fieldname = 'AUSBS' OR
*        ls_good_cells2-fieldname = 'AUZTB'.
*
*      PERFORM get_data_grid3.
*      PERFORM set_cell_attribute_2.
*
*      CALL METHOD grid2->refresh_table_display.
*    ENDIF.

  ENDMETHOD.                    "HANDLE_DATA_CHANGED_FINISHED2

*---------------------------------------------------------------------*
* USER COMMAND
*---------------------------------------------------------------------*
  METHOD handle_user_command.
    DATA : l_answer,
           l_chk,
           l_line1(70),
           l_line2(70),
           l_title(50).

** READ SELECTED ROW FROM INTERNAL TABLE G_ALVLIST_T
    CASE e_ucomm.
      WHEN 'CHAN'.
        PERFORM get_selected_rows USING l_chk.
        IF l_chk EQ 'X'.
          EXIT.
        ENDIF.
*        l_line1 = 'Maintain Min/Max'.
*        l_line2 = 'Are you maintain Min/Max ?'.
*        l_title = 'Maintain Min/Max'.
*        PERFORM popup_confirm USING l_answer
*                                    l_line1
*                                    l_line2
*                                    l_title.
*
*        IF l_answer EQ 'J'.
        PERFORM maintain_master_data.
*        ENDIF.

        PERFORM query_data.
        CALL METHOD grid1->refresh_table_display.

      WHEN 'ABC'.       "03.11.2014 Victor
        PERFORM get_selected_rows USING l_chk.
        IF l_chk EQ 'X'.
          EXIT.
        ENDIF.

        PERFORM maintain_master_data_abc.

        PERFORM query_data.
        CALL METHOD grid1->refresh_table_display.

      WHEN 'SALL'.
        PERFORM select_all_item.
        PERFORM refresh_table_data.
    ENDCASE.

  ENDMETHOD.                           "handle_user_command

  METHOD handle_user_command2.
    DATA : l_flg,
           l_answer,
           l_line1(70),
           l_line2(70),
           l_title(50).

** READ SELECTED ROW FROM INTERNAL TABLE G_ALVLIST_T
    CASE e_ucomm.
      WHEN 'SALL'.
        PERFORM select_all_item_detail.
        PERFORM refresh_table_data.
      WHEN 'PREQ'.
        l_line1 = 'Save '.
        l_line2 = 'Are you create Purchase Requisition ?'.
        l_title = 'Downtime analysis'.
        PERFORM popup_confirm USING l_answer
                                    l_line1
                                    l_line2
                                    l_title.

        IF l_answer EQ 'J'.
          PERFORM save_data USING 'SAVD'
                         CHANGING l_flg.
          IF l_flg = ' '.
            CLEAR : g_alv2_t, g_alv2_t[].
            PERFORM query_data.
            PERFORM refresh_table_data.
            g_check = ' '.
          ENDIF.
        ENDIF.

    ENDCASE.


  ENDMETHOD.                           "handle_user_command2

*---------------------------------------------------------------------*
* HOTSPOR CLICK
*---------------------------------------------------------------------*
*  METHOD handle_hotspot_click.
*
*  ENDMETHOD.                    "HANDLE_HOTSPOT_CLICK
*---------------------------------------------------------------------*
  METHOD handle_data_changed.

    PERFORM data_changed  USING er_data_changed e_onf4 e_ucomm.

  ENDMETHOD.                    "handle_data_changed

  METHOD handle_data_changed2.

    PERFORM data_changed2  USING er_data_changed e_onf4 e_ucomm.

  ENDMETHOD.                    "handle_data_changed2
*---------------------------------------------------------------------*
*-- On help f4 - Search Help
*---------------------------------------------------------------------*
  METHOD handle_onf4.
    PERFORM on_f4 USING sender
                        e_fieldname
                        e_fieldvalue
                        es_row_no
                        er_event_data
                        et_bad_cells
                        e_display.
  ENDMETHOD.                    "HANDLE_ONF4

  METHOD handle_onf4_2.
    PERFORM on_f4 USING sender
                        e_fieldname
                        e_fieldvalue
                        es_row_no
                        er_event_data
                        et_bad_cells
                        e_display.
  ENDMETHOD.                    "HANDLE_ONF4
*---------------------------------------------------------------------*
* DOUBLE CLICK
*---------------------------------------------------------------------*
  METHOD handle_double_click.

** READ SELECTED ROW FROM INTERNAL TABLE G_ALVLIST_T
    READ TABLE g_alv1_t INDEX e_row-index INTO g_alv1_s.

    CHECK sy-subrc = 0.

    CASE e_column.
      WHEN 'VBELN'.
*        SET PARAMETER ID 'AUN' FIELD g_alv1_s-vbeln.
*        CALL TRANSACTION 'VA02'
*                      AND SKIP  FIRST SCREEN.

    ENDCASE.

  ENDMETHOD.                    "HANDLE_DOUBLE_CLICK

  METHOD handle_double_click2.

** READ SELECTED ROW FROM INTERNAL TABLE G_ALVLIST_T
    READ TABLE g_alv2_t INDEX e_row-index INTO g_alv2_s.

    CHECK sy-subrc = 0.

    CASE e_column.
      WHEN 'VBELN'.
*        SET PARAMETER ID 'AUN' FIELD g_alv2_s-vbeln.
*        CALL TRANSACTION 'VA02'
*                      AND SKIP  FIRST SCREEN.

    ENDCASE.

  ENDMETHOD.                    "HANDLE_DOUBLE_CLICK2

*---------------------------------------------------------------------*
* HOTSPOR CLICK
*---------------------------------------------------------------------*
  METHOD handle_hotspot_click.

    PERFORM hotspot_click USING e_row_id e_column_id.

  ENDMETHOD.                    "HANDLE_HOTSPOT_CLICK

*---------------------------------------------------------------------*
* HOTSPOR CLICK2
*---------------------------------------------------------------------*
  METHOD handle_hotspot_click2.

    PERFORM hotspot_click2 USING e_row_id e_column_id.

  ENDMETHOD.                    "HANDLE_HOTSPOT_CLICK

ENDCLASS.                    "LCL_EVENT_RECEIVER IMPLEMENTATION
