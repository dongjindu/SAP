************************************************************************
* Program Name      : ZAHRU012
* Author            : IG.Moon
* Creation Date     : 7/20/2009
* Specifications By : Euna Lee
* Description       : [ESS] Update Bank Information for ESS system
* Modifications Log
* Date        Developer   Request ID    Description
* 10/20/2011  Valerian    UD1K953234    Prevent both amount and percent
*                                       age to be updated together
************************************************************************
REPORT zahru012 MESSAGE-ID zmco.
INCLUDE zahrui00.
*----------------------------------------------------------------------*
*   Data Definition
*----------------------------------------------------------------------*
*Type-Pools
TYPE-POOLS: kcde.

* Tables
TABLES : zess_bank_if_req, pa0009, pa0001,*zess_bank_if_req.

TYPES: BEGIN OF ty_row_tab,
        zname(70),
        chk(1),
        bnksa TYPE bnksa,
        bkont TYPE bkont,
        acct_type(30).
        INCLUDE STRUCTURE zess_bank_if_req.
TYPES: END OF ty_row_tab.

TYPES BEGIN OF ty_out.
INCLUDE  TYPE ty_row_tab.
TYPES celltab  TYPE lvc_t_styl.
TYPES tabcolor TYPE slis_t_specialcol_alv.
TYPES END OF ty_out.

DATA  : it_row_tab TYPE TABLE OF ty_row_tab WITH HEADER LINE,
        gt_out     TYPE TABLE OF ty_out     WITH HEADER LINE.
DATA  $gt_out LIKE gt_out OCCURS 0 WITH HEADER LINE.
DATA  $gt_out_tmp LIKE gt_out OCCURS 0 WITH HEADER LINE.

DATA  itab LIKE zess_emp_bank_detail OCCURS 0 WITH HEADER LINE.
DATA i_return LIKE bapireturn OCCURS 0 WITH HEADER LINE.
DATA: g_error(1),
      g_repid  LIKE sy-repid,
      g_ix     LIKE sy-tabix.

DATA : icon_red_scr TYPE icon_d,
       icon_green_scr TYPE icon_d,
       icon_yellow_scr TYPE icon_d,
       icon_gray_scr  TYPE icon_d.

DEFINE __process.
  perform show_progress using &1 &2.
END-OF-DEFINITION.
DEFINE __cls.                          " clear & refresh
  clear &1.refresh &1.
END-OF-DEFINITION.
DEFINE __focus.
  call method cl_gui_control=>set_focus
      exporting
        control = &1 .
END-OF-DEFINITION.

DEFINE __u_break.
  if err_brk eq true.
    break-point.
  endif.
END-OF-DEFINITION.

DATA: r_date TYPE datum,
      r_user TYPE uname.

*----------------------------------------------------------------------*
* Define local class
*----------------------------------------------------------------------*
CLASS lcl_event_receiver DEFINITION.
  PUBLIC SECTION.

    TYPES: BEGIN OF ztcou131_k,
              co_area   TYPE kokrs,
              fisc_year TYPE gjahr,
              version   TYPE versn,
              kostl     TYPE kostl,
              kstar     TYPE kstar,
           END OF ztcou131_k.

    TYPES: ztcou131_key   TYPE STANDARD TABLE OF ztcou131_k,
           ztcou131_table TYPE STANDARD TABLE OF ztcou131.

    METHODS:
      handle_data_changed
         FOR EVENT data_changed OF cl_gui_alv_grid
             IMPORTING er_data_changed,
                       get_deleted_rows
             EXPORTING
                       deleted_rows TYPE ztcou131_table,

      refresh_delta_tables.


    METHODS:
      handle_double_click FOR EVENT double_click OF cl_gui_alv_grid
              IMPORTING e_row
                        e_column
                        es_row_no.

  PRIVATE SECTION.
    DATA deleted_rows TYPE STANDARD TABLE OF ztcou131.

* This flag is set if any error occured in one of the
* following methods:
    DATA: error_in_data TYPE c.
    METHODS:
      update_delta_tables
         IMPORTING
            pr_data_changed TYPE REF TO cl_alv_changed_data_protocol.

ENDCLASS.                   " LCL_EVENT_RECEIVER Definition

*----------------------------------------------------------------------*
* Implementation local class
*----------------------------------------------------------------------*
CLASS lcl_event_receiver IMPLEMENTATION.

* Setting for Change data
  METHOD handle_data_changed.

* remember deleted lines for saving
    CALL METHOD update_delta_tables( er_data_changed ).

    PERFORM data_changed USING er_data_changed.
  ENDMETHOD.                    " handle_data_changed

  METHOD get_deleted_rows.
    deleted_rows = me->deleted_rows.
  ENDMETHOD.

  METHOD refresh_delta_tables.
    CLEAR me->deleted_rows[].
  ENDMETHOD.

* Double Click
  METHOD handle_double_click.
    PERFORM double_click USING e_row
                               e_column
                               es_row_no.
  ENDMETHOD.                    " handle_double_click

  METHOD update_delta_tables.
    DATA: l_del_row TYPE lvc_s_moce,
          ls_ztcou131 TYPE ztcou131,
          ls_outtab LIKE LINE OF gt_out.

    LOOP AT pr_data_changed->mt_deleted_rows INTO l_del_row.
      READ TABLE gt_out INTO ls_outtab INDEX l_del_row-row_id.
      IF sy-subrc NE 0.
        MESSAGE i000(0k) WITH text-e01. "Internal error
      ELSE.
        MOVE-CORRESPONDING ls_outtab TO ls_ztcou131.
        APPEND ls_ztcou131 TO deleted_rows.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


ENDCLASS.                   " LCL_EVENT_RECEIVER Implementation

DATA g_event_receiver  TYPE REF TO lcl_event_receiver.

************************************************************************
DATA  : flag_data_changed,
        info(80).
DATA: BEGIN OF ftab OCCURS 10,
        fcode(6),
      END OF ftab.
****************************** constants *******************************
CONSTANTS:  false VALUE ' ',
            true  VALUE 'X'.

*----------------------------------------------------------------------*
*   Selection Condition                                                *
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK bl1 WITH FRAME TITLE text-001.
SELECT-OPTIONS s_date FOR sy-datum DEFAULT sy-datum.
SELECT-OPTIONS s_pernr FOR pa0009-pernr.
SELECTION-SCREEN END OF BLOCK bl1.

*&----------------------------------------------------------------------
*  Initialization
*&----------------------------------------------------------------------
INITIALIZATION.
  __cls s_date.

  s_date-sign = 'I'.
  s_date-option = 'BT'.
  s_date-low = sy-datum - 1.
  s_date-high = sy-datum.


  APPEND s_date.
*----------------------------------------------------------------------*
* Start-of-selection
*----------------------------------------------------------------------*
START-OF-SELECTION.

  PERFORM get_row_data.
  PERFORM move_out.
  PERFORM set_output .

*----------------------------------------------------------------------*
* END-of-selection
*----------------------------------------------------------------------*
END-OF-SELECTION.

* Log.
*  PERFORM DISPLAY_LOG.


*----------------------------------------------------------------------*
* Sub-Rutines
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  SHOW_PROGRESS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TEXT_S01  text
*      -->P_&1  text
*----------------------------------------------------------------------*
FORM show_progress USING    pf_text
                            value(pf_val).
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
       EXPORTING
            percentage = pf_val
            text       = pf_text.

ENDFORM.                    " SHOW_PROGRESS
*&---------------------------------------------------------------------*
*&      Form  DATA_CHANGED
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ER_DATA_CHANGED  text
*----------------------------------------------------------------------*
FORM data_changed USING rr_data_changed
                        TYPE REF TO cl_alv_changed_data_protocol.

  flag_data_changed = true.

  DATA: ls_mod_cells TYPE lvc_s_modi,
        ls_cells     TYPE lvc_s_modi,
        lt_values TYPE TABLE OF bapi_char_values WITH HEADER LINE.

  LOOP AT rr_data_changed->mt_good_cells INTO ls_mod_cells.
    READ TABLE gt_out INDEX ls_mod_cells-row_id.
    IF sy-subrc = 0.
      CALL METHOD rr_data_changed->modify_cell
                EXPORTING i_row_id    = ls_mod_cells-row_id
                          i_fieldname = ls_mod_cells-fieldname
                          i_value     = ls_mod_cells-value.
    ENDIF.
  ENDLOOP.

  __set_refresh_mode true.
  CALL METHOD g_grid->refresh_table_display
       EXPORTING is_stable = stable.

ENDFORM.                    " DATA_CHANGED
*&---------------------------------------------------------------------*
*&      Form  MOVE_OUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM move_out.

  __process 'Preparing output...' '95'.

  __cls gt_out.
  LOOP AT it_row_tab.
    MOVE-CORRESPONDING it_row_tab TO gt_out.
    SELECT SINGLE sname INTO gt_out-zname
    FROM pa0001 WHERE pernr EQ it_row_tab-pernr
                  AND endda EQ '99991231'.

    SELECT SINGLE stext INTO gt_out-bank_type_text
    FROM t591s WHERE infty EQ '0009'
                 AND subty = it_row_tab-bank_type_code
                 AND sprsl EQ sy-langu.

    IF it_row_tab-bank_type_code EQ '0'.
      IF it_row_tab-bank_type_text EQ 'Main Bank Savings'.
        gt_out-bkont = '02'.
        gt_out-acct_type = 'Savings'.
      ELSE.
        gt_out-bkont = ' '.
        gt_out-acct_type = 'Checking'.
      ENDIF.

    ELSEIF it_row_tab-bank_type_code EQ '1'.
      gt_out-bkont = '02'.
      gt_out-acct_type = 'Savings'.
    Else.
      gt_out-bkont = ' '.
      gt_out-acct_type = 'Checking'.
    endif.

    if it_row_tab-status eq 'C'.
      if it_row_tab-msg eq 'Success!' or it_row_tab-msg eq 'SUCCESS!'.
      else.
        clear : gt_out-icon, gt_out-msg.
      endif.
    else.
      clear : gt_out-icon, gt_out-msg.
    endif.

    APPEND gt_out.
  ENDLOOP.

ENDFORM.                    " MOVE_OUT
*&---------------------------------------------------------------------*
*&      Form  SET_OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_output.
  CHECK : g_error IS INITIAL.
  CLEAR flag_data_changed.
  PERFORM apply_icon.

  CALL SCREEN 100.

ENDFORM.                    " SET_OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET TITLEBAR '100'.
*   Exclude toolbar
  PERFORM exclude_functions.

  icon_red_scr = icon_led_red.
  icon_green_scr = icon_led_green.
  icon_gray_scr = icon_led_yellow.

ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  EXCLUDE_FUNCTIONS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM exclude_functions.
  PERFORM append_exclude_functions
           TABLES gt_exclude[]
           USING: cl_gui_alv_grid=>mc_fc_loc_undo,
                  cl_gui_alv_grid=>mc_fc_average,
                  cl_gui_alv_grid=>mc_fc_graph,
                  cl_gui_alv_grid=>mc_fc_info,
                  cl_gui_alv_grid=>mc_fc_loc_copy_row,
                  cl_gui_alv_grid=>mc_fc_loc_append_row,
                  cl_gui_alv_grid=>mc_fc_loc_cut,
                  cl_gui_alv_grid=>mc_fc_loc_insert_row,
                  cl_gui_alv_grid=>mc_fc_loc_move_row,
                  cl_gui_alv_grid=>mc_fc_loc_paste_new_row.

ENDFORM.                    " EXCLUDE_FUNCTIONS
*&---------------------------------------------------------------------*
*&      Module  DISPLAY_ALV_100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE display_alv_100 OUTPUT.
  IF g_custom_container IS INITIAL.
    PERFORM create_and_init_alv.
*   Display alv grid
    CALL METHOD g_grid->set_table_for_first_display
         EXPORTING is_layout            = gs_layo
                   it_toolbar_excluding = gt_exclude
                   i_save               = gc_var_save
                   is_variant           = gs_variant
         CHANGING  it_outtab            = gt_out[]
                   it_fieldcatalog      = gt_fcat[]
                   it_sort              = gt_sort[].
  ELSE.
    CALL METHOD g_grid->refresh_table_display.
  ENDIF.
  __focus g_grid.
  PERFORM user_status.

ENDMODULE.                 " DISPLAY_ALV_100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  CLEAR : g_error.

  ok_code = sy-ucomm.
  CLEAR sy-ucomm.
  CASE ok_code.

    WHEN 'BACK' OR 'CANC'.
      PERFORM free_container.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
      LEAVE PROGRAM.

    WHEN 'SAVE'.
      CHECK sy-dynnr EQ '0100'.
      PERFORM really?.
      CHECK g_error NE true.

      PERFORM : apply_data,
                refresh_alv.
      __focus g_grid.

*    WHEN 'SWITCH'.
*      IF sy-dynnr EQ '0100'.
*        PERFORM switch_edit_mode.
*      ENDIF.
*      __focus g_grid.

    WHEN 'DELE'.
      CHECK sy-dynnr EQ '0100'.
      PERFORM really_dele?.
      CHECK g_error NE true.

      PERFORM : dele_data,
                refresh_alv.
      __focus g_grid.

    WHEN 'LOGV'.
      CALL SCREEN '300'.
  ENDCASE.


ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Form  FREE_CONTAINER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM free_container.
  IF NOT g_event_receiver IS INITIAL.
    FREE g_event_receiver.
  ENDIF.

  IF NOT g_grid IS INITIAL.
    CALL METHOD g_grid->free.
  ENDIF.

  IF NOT g_custom_container IS INITIAL.
    CALL METHOD g_custom_container->free.
  ENDIF.

  FREE : g_grid,g_custom_container.

  CLEAR :  gs_layo,gt_exclude,gt_out[],gt_fcat[],gt_sort[].


ENDFORM.                    " FREE_CONTAINER
*&---------------------------------------------------------------------*
*&      Form  REALLY?
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM really?.
  DATA $exists(1).
  DATA l_answer(1).

  PERFORM pop_up USING
      'This will change the Bank information for selected employee.'
      'Do you really want to proceed?' ' '
                 CHANGING l_answer.

  IF l_answer NE 'J'.
    g_error = true.
    MESSAGE s000 WITH 'Processing was canceled by user.'.
  ENDIF.
ENDFORM.                    " REALLY?
*&---------------------------------------------------------------------*
*&      Form  POP_UP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1886   text
*      -->P_1887   text
*      -->P_1888   text
*      <--P_L_ANSWER  text
*----------------------------------------------------------------------*
FORM pop_up USING    p_text p_text2 p_canc
            CHANGING p_answer.

  CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
       EXPORTING
            textline1      = p_text
            textline2      = p_text2
            titel          = 'Check!'
            cancel_display = p_canc
       IMPORTING
            answer         = p_answer.


ENDFORM.                    " POP_UP
*&---------------------------------------------------------------------*
*&      Form  REFRESH_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM refresh_alv.
  __set_refresh_mode true.
  CALL METHOD g_grid->refresh_table_display
       EXPORTING is_stable = stable.

ENDFORM.                    " REFRESH_ALV
*&---------------------------------------------------------------------*
*&      Form  CREATE_AND_INIT_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_and_init_alv.

*   Create object
  PERFORM create_object.

*  Create Object to verify input values.
  CREATE OBJECT g_event_receiver.
  SET HANDLER : g_event_receiver->handle_data_changed FOR g_grid,
                g_event_receiver->handle_double_click FOR g_grid.

*   Create field category
  PERFORM create_field_category USING false.

*  CALL METHOD g_grid->register_edit_event
*       EXPORTING i_event_id = cl_gui_alv_grid=>mc_evt_modified.

  CALL METHOD g_grid->set_ready_for_input
     EXPORTING
            i_ready_for_input = 0.

  PERFORM sort_build USING gt_sort[].

*   Setting for layout
  PERFORM set_lvc_layout.

*   Set colors
*  PERFORM set_color.

*   Set variant
  gv_repid = gs_variant-report = sy-repid.
*  gs_variant-variant = p_vari.

*   Define cell attribute
  PERFORM build_cell_attr.


ENDFORM.                    " CREATE_AND_INIT_ALV
*&---------------------------------------------------------------------*
*&      Form  CREATE_FIELD_CATEGORY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_FALSE  text
*----------------------------------------------------------------------*
FORM create_field_category USING mode_edit.
  DATA: l_pos       TYPE i.
  DEFINE __catalog.
    l_pos = l_pos + 1.
    clear gs_fcat.
    gs_fcat-col_pos       = l_pos.
    gs_fcat-key           = &1.
    gs_fcat-fieldname     = &2.
    gs_fcat-coltext       = &3.     " Column heading
    gs_fcat-outputlen     = &4.     " Column width
    gs_fcat-datatype      = &5.     " Data type
    gs_fcat-emphasize     = &6.
    append gs_fcat to gt_fcat.
  END-OF-DEFINITION.

  DATA : $ix(2) TYPE n,
         $mtxt(6).

  __catalog :
    'X'  'REQDATE'           'Req.Date'          8 'DATS' '',
    'X'  'PERNR'             'Emp.No.'           8 'NUMC' '',
    'X'  'ZNAME'             'Name'             20 'CHAR' '',
    ' '  'DELETE_ADDTIONAL'  'Del/Upd'           1 'CHAR' '',
    ' '  'STATUS'            'Status'            1 'CHAR' '',
*    ' '  'BANK_TYPE_CODE'    'Bank Type'         4 'CHAR' '',
    ' '  'BANK_TYPE_TEXT'    'Bank Type'        15 'CHAR' '',
    ' '  'ACCT_TYPE'         'Acct.Type'        30 'CHAR' '',
    ' '  'PAYEE'             'Payee'            15 'CHAR' '',
*    ' '  'ZIP_CODE'          'Zip'              10 'CHAR' '',
*    ' '  'CITY'              'City'             25 'CHAR' '',
*    ' '  'BANK_COUNTRY'      'Country'           3 'CHAR' '',
*    ' '  'BANK_CNTRY_NAME'   'Country Name'     15 'CHAR' '',
*    ' '  'BANK_KEY'          'Bank Key'         15 'CHAR' '',
    ' '  'BANK_NAME'         'Bank Name'        20 'CHAR' '',
    ' '  'BANK_ACCOUNT'      'Account'          15 'CHAR' '',
    ' '  'AMOUNT'            'Amount'           15 'CURR' '',
    ' '  'PERCENTAGE'        'PCNT(%)'          15 'DEC' '',
    ' '  'PURPOSE'           'Purpose'          20 'CHAR' '',
    ' '  'ICON'              'f'                 4 'ICON' '',
    ' '  'MSG'               'Message'          30 'CHAR' ''.

  LOOP AT gt_fcat INTO gs_fcat.
    gs_fcat-tabname = 'ZESS_BANK_IF_REQ'.
    gs_fcat-ref_table = 'ZESS_BANK_IF_REQ'.
    gs_fcat-ref_field = gs_fcat-fieldname.

    IF gs_fcat-fieldname EQ 'DELETE_ADDTIONAL'.
      gs_fcat-f4availabl = true.
    ENDIF.

    MODIFY gt_fcat FROM gs_fcat.
  ENDLOOP.

ENDFORM.                    " CREATE_FIELD_CATEGORY
*&---------------------------------------------------------------------*
*&      Form  SORT_BUILD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_SORT[]  text
*----------------------------------------------------------------------*
FORM sort_build USING ft_sort TYPE lvc_t_sort.
  DEFINE sort_tab.
    clear gs_sort.
    gs_sort-fieldname = &1.
    gs_sort-spos      = &2.
    gs_sort-up        = &3.
    gs_sort-group     = &4.
    gs_sort-subtot    = &5.
    gs_sort-comp      = &6.
    append gs_sort to ft_sort.
  END-OF-DEFINITION.

  sort_tab :
      'REQDATE'       ' ' 'X' '' 'X' '',
      'PERNR'         ' ' 'X' '' 'X' ''.

ENDFORM.                    " SORT_BUILD
*&---------------------------------------------------------------------*
*&      Form  SET_LVC_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_lvc_layout.

  CLEAR gs_layo.
  gs_layo-edit       = 'X'.
  gs_layo-zebra      = 'X'.
  gs_layo-sel_mode   = 'A'.       " Column and row selection
  gs_layo-cwidth_opt = 'X'.
  gs_layo-ctab_fname = 'TABCOLOR'.
  gs_layo-stylefname = 'CELLTAB'.

ENDFORM.                    " SET_LVC_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  SET_COLOR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_color.
  DATA : $ix(2) TYPE n,
         $mtxt(6).

  CLEAR: gs_specialcol, gt_specialcol[], gt_out-tabcolor[].

  DEFINE __color.
    gs_specialcol-fieldname = &1 .
    gs_specialcol-color-col = &2 .
    gs_specialcol-color-int = &3 .
    append gs_specialcol to gt_specialcol .
  END-OF-DEFINITION.

  __color :
            'VIN'              '2' 0,
            'MATNR'            '2' 0,
            'KBETR'            '3' 0,
            'FKDAT'            '1' 0,
            'ICON'             '1' 0,
            'MSG'              '1' 0.

  gt_out-tabcolor[] = gt_specialcol[].
  MODIFY gt_out TRANSPORTING tabcolor WHERE tabcolor IS initial.

ENDFORM.                    " SET_COLOR
*&---------------------------------------------------------------------*
*&      Form  BUILD_CELL_ATTR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_cell_attr.
  DATA: lt_celltab TYPE lvc_t_styl,
        ls_celltab TYPE lvc_s_styl.

  CLEAR lt_celltab.
  REFRESH lt_celltab.

  CLEAR gs_fcat.

  LOOP AT gt_fcat INTO gs_fcat.
    ls_celltab-fieldname = gs_fcat-fieldname.
    IF ls_celltab-fieldname EQ 'KBETR' OR
          ls_celltab-fieldname EQ 'VIN'.

      ls_celltab-style = cl_gui_alv_grid=>mc_style_enabled.
    ELSE.
      ls_celltab-style = cl_gui_alv_grid=>mc_style_disabled.
    ENDIF.
    INSERT ls_celltab INTO TABLE lt_celltab.
  ENDLOOP.

  CLEAR gt_out-celltab.
  INSERT LINES OF lt_celltab INTO TABLE gt_out-celltab.
  MODIFY gt_out TRANSPORTING celltab WHERE celltab IS initial.
  PERFORM build_cell_attr1_lock.

ENDFORM.                    " BUILD_CELL_ATTR
*&---------------------------------------------------------------------*
*&      Form  BUILD_CELL_ATTR1_LOCK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_cell_attr1_lock.

*  DATA: LT_CELLTAB TYPE LVC_T_STYL,
*        LS_CELLTAB TYPE LVC_S_STYL.
*
*  CLEAR LT_CELLTAB.
*  REFRESH LT_CELLTAB.
*
*  __CLS GT_OUT-CELLTAB.
*  MODIFY GT_OUT TRANSPORTING CELLTAB WHERE FEVOR = SPACE.
*
*  CLEAR GS_FCAT.
*
*  LOOP AT GT_FCAT INTO GS_FCAT.
*    LS_CELLTAB-FIELDNAME = GS_FCAT1-FIELDNAME.
*    LS_CELLTAB-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
*    INSERT LS_CELLTAB INTO TABLE LT_CELLTAB.
*  ENDLOOP.
*
*  INSERT LINES OF LT_CELLTAB INTO TABLE GT_OUT-CELLTAB.
*  MODIFY GT_OUT TRANSPORTING CELLTAB WHERE FEVOR = SPACE.
*

ENDFORM.                    " BUILD_CELL_ATTR1_LOCK
*&---------------------------------------------------------------------*
*&      Form  SWITCH_EDIT_MODE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM switch_edit_mode.
  DATA answer.
  IF g_grid->is_ready_for_input( ) EQ 0.
    CALL METHOD g_grid->set_ready_for_input
                     EXPORTING i_ready_for_input = 1.

    PERFORM info_text_set USING true.
  ELSE.
    CALL METHOD g_grid->set_ready_for_input
                     EXPORTING i_ready_for_input = 0.
    PERFORM info_text_set USING false.
  ENDIF.

  PERFORM build_cell_attr.
ENDFORM.                    " SWITCH_EDIT_MODE
*&---------------------------------------------------------------------*
*&      Form  INFO_TEXT_SET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TRUE  text
*----------------------------------------------------------------------*
FORM info_text_set USING    p_true.
  IF p_true EQ true.
    info = text-015.
  ELSE.
    info = text-015.
  ENDIF.

ENDFORM.                    " INFO_TEXT_SET
*&---------------------------------------------------------------------*
*&      Form  user_status
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM user_status.
  __cls ftab.

  IF g_grid->is_ready_for_input( ) EQ 1.
    ftab-fcode = 'SAVE'.
    APPEND ftab.
  ENDIF.

  ftab-fcode = 'LOGV'.
  APPEND ftab.
  SET PF-STATUS '100' EXCLUDING ftab.
ENDFORM.                    " user_status
*&---------------------------------------------------------------------*
*&      Form  APPLY_CCA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM apply_data.
  __cls $gt_out.

  DATA: lt_row   TYPE lvc_t_row,
        ls_row   TYPE lvc_s_row,
        lt_roid  TYPE lvc_t_roid,
        lv_cnt(5),
        lv_dcnt(5),
        lv_msg(200).                 " Message

  DATA  lt_messages      LIKE messages       OCCURS 0 WITH HEADER LINE.
  DATA  $messages      LIKE messages       OCCURS 0 WITH HEADER LINE.
  DATA  l_both(1) TYPE c.

  CLEAR: lv_cnt, lt_row[], lt_roid[].

  PERFORM get_selected_rows TABLES $gt_out.
  __cls $gt_out_tmp.
  $gt_out_tmp[] = $gt_out[].
  data $skip.

  LOOP AT $gt_out.

    clear $skip.
    clear l_both.

* checking one more addtional bank
    __cls : itab.
    loop at $gt_out_tmp.
      if $gt_out_tmp-pernr = $gt_out-pernr
       and ( $gt_out_tmp-BANK_TYPE_CODE = '1'
       or $gt_out_tmp-BANK_TYPE_CODE = '9' ) .
        MOVE-CORRESPONDING $gt_out_tmp TO itab.
        append itab.
      endif.
    endloop.
    read table itab index 2.
    if sy-subrc eq 0.
      $skip = true.
    endif.
* BEGIN OF UD1K953234
    if not $gt_out-amount is initial and
       not $gt_out-percentage is initial.
      l_both = true.
    endif.
* END OF UD1K953234
    __cls : itab, i_return.
    MOVE-CORRESPONDING $gt_out TO itab.
    APPEND itab.

    if $skip eq false and l_both = false.                   "UD1K953234
*   if $skip eq false.                                      "UD1K953234
      CALL FUNCTION 'Z_HR_ESS_UPD_EMP_BANK_INFO'
           EXPORTING
                employee_number      = $gt_out-pernr
                delete_addtional     = $gt_out-delete_addtional
           TABLES
                zess_emp_bank_detail = itab
                return               = i_return.
* BEGIN OF UD1K953234
    elseif l_both = true.
      i_return-type = 'E'.
      i_return-message =
        'Either Amount or Percentage can only be updated'.
      APPEND i_return.
* END OF UD1K953234
    else.
      i_return-type = 'E'.
      i_return-message = 'Multiple Addtional Bank Accounts!'.
      APPEND i_return.
    endif.

    READ TABLE i_return WITH KEY type = 'E'.
    IF sy-subrc EQ 0.
      gt_out-icon = icon_led_red.
      clear gt_out-status.
    ELSE.
      READ TABLE i_return WITH KEY type = 'I'.
      IF sy-subrc EQ 0.
        gt_out-icon = icon_led_red.
        clear gt_out-status.
      ELSE.
        gt_out-icon = icon_led_green.
        gt_out-status = 'C'.
      ENDIF.
    ENDIF.
    gt_out-msg = i_return-message.

    if $gt_out-percentage > '0'.

      MODIFY gt_out TRANSPORTING icon msg status
                       WHERE pernr EQ $gt_out-pernr
                         AND reqdate EQ $gt_out-reqdate
                         AND bank_type_code EQ $gt_out-bank_type_code
                         and BANK_ACCOUNT eq $gt_out-BANK_ACCOUNT
                         and percentage eq $gt_out-percentage.

      UPDATE zess_bank_if_req SET icon = gt_out-icon
                              msg = gt_out-msg
                              status = gt_out-status
       WHERE pernr EQ $gt_out-pernr
       and BANK_ACCOUNT eq $gt_out-BANK_ACCOUNT
       and percentage eq $gt_out-percentage
       AND reqdate EQ $gt_out-reqdate
       AND bank_type_code EQ $gt_out-bank_type_code.
    else.

      MODIFY gt_out TRANSPORTING icon msg status
                       WHERE pernr EQ $gt_out-pernr
                         AND reqdate EQ $gt_out-reqdate
                         AND bank_type_code EQ $gt_out-bank_type_code
                         and BANK_ACCOUNT eq $gt_out-BANK_ACCOUNT
                         and AMOUNT eq $gt_out-AMOUNT.

*        UPDATE zess_bank_if_req SET icon = gt_out-icon
*                                msg = gt_out-msg
*                                status = gt_out-status

      UPDATE zess_bank_if_req SET icon = gt_out-icon
                              msg = gt_out-msg
                              status = gt_out-status
       WHERE pernr EQ $gt_out-pernr
       and BANK_ACCOUNT eq $gt_out-BANK_ACCOUNT
       and AMOUNT eq $gt_out-AMOUNT
       AND reqdate EQ $gt_out-reqdate
       AND bank_type_code EQ $gt_out-bank_type_code.

      if sy-subrc ne 0.
        select single * from zess_bank_if_req
         WHERE pernr EQ $gt_out-pernr
         and BANK_ACCOUNT eq $gt_out-BANK_ACCOUNT
         and AMOUNT eq $gt_out-AMOUNT
         AND reqdate EQ $gt_out-reqdate
         AND bank_type_code EQ $gt_out-bank_type_code.
        if sy-subrc eq 0.
           *zess_bank_if_req = zess_bank_if_req.

          delete from zess_bank_if_req
           WHERE pernr EQ $gt_out-pernr
           and BANK_ACCOUNT eq $gt_out-BANK_ACCOUNT
           and AMOUNT eq $gt_out-AMOUNT
           AND reqdate EQ $gt_out-reqdate
           AND bank_type_code EQ $gt_out-bank_type_code.

           *zess_bank_if_req-icon = gt_out-icon.
           *zess_bank_if_req-msg = gt_out-msg.
           *zess_bank_if_req-status = gt_out-status.
          zess_bank_if_req = *zess_bank_if_req.
          insert zess_bank_if_req.

        endif.

      endif.

    endif.

    IF sy-subrc NE 0 .
    ELSE.
      COMMIT WORK.
    ENDIF.


  ENDLOOP.

ENDFORM.                    " APPLY_CCA
*&---------------------------------------------------------------------*
*&      Form  GET_SELECTED_ROWS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_$GT_OUT  text
*----------------------------------------------------------------------*
FORM get_selected_rows TABLES $gt_out STRUCTURE gt_out.

  DATA: lt_rows TYPE lvc_t_row WITH HEADER LINE,
        lt_row_no TYPE lvc_t_roid. "Numeric IDs of Selected Rows

  CALL METHOD g_grid->get_selected_rows
           IMPORTING et_index_rows = lt_rows[]
                     et_row_no     = lt_row_no.

  CALL METHOD cl_gui_cfw=>flush.

  IF sy-subrc NE 0.
    MESSAGE e000
    WITH 'Error founded during flushing of ALV Grid Control'.
    EXIT.
  ENDIF.

  READ TABLE lt_rows INDEX 1.
  IF sy-subrc NE 0.
    gt_out-chk = true .
    MODIFY gt_out TRANSPORTING chk WHERE chk EQ false.
    $gt_out[] = gt_out[].
  ELSE.
    LOOP AT lt_rows WHERE rowtype IS initial.
      READ TABLE gt_out INDEX lt_rows-index.
      gt_out-chk = true .
      MODIFY gt_out INDEX lt_rows-index .
    ENDLOOP.
    LOOP AT gt_out.
      CHECK gt_out-chk EQ true.
      $gt_out = gt_out.
      APPEND $gt_out.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " get_selected_rows
*&---------------------------------------------------------------------*
*&      Form  convert_date
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_POST_FKDAT  text
*      <--P_STR_DATE  text
*----------------------------------------------------------------------*
FORM convert_date  USING    f_date  LIKE sy-datum
                   CHANGING f_dtout TYPE char10.
  CALL FUNCTION 'CONVERT_DATE_TO_EXTERNAL'
       EXPORTING
            date_internal            = f_date
       IMPORTING
            date_external            = f_dtout
       EXCEPTIONS
            date_internal_is_invalid = 1
            OTHERS                   = 2.

ENDFORM.                    " CONVERT_DATE
*&---------------------------------------------------------------------*
*&      Form  make_msg_string
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_MSG  text
*----------------------------------------------------------------------*
FORM make_msg_string USING    p_msg.

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
            msg_lin = p_msg.

ENDFORM.                    " MAKE_MSG_STRING
*&---------------------------------------------------------------------*
*&      Form  apply_icon
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM apply_icon.
  DATA $ix LIKE sy-tabix.

  LOOP AT gt_out.
    $ix = sy-tabix.

    MODIFY gt_out INDEX $ix TRANSPORTING icon.
  ENDLOOP.

ENDFORM.                    " apply_icon
*---------------------------------------------------------------------*
*       FORM double_click                                             *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  E_ROW                                                         *
*  -->  E_COLUMN                                                      *
*  -->  ES_ROW_NO                                                     *
*---------------------------------------------------------------------*
FORM double_click USING  e_row     TYPE lvc_s_row
                         e_column  TYPE lvc_s_col
                         es_row_no TYPE lvc_s_roid.
  DATA l_index TYPE i.

  l_index = e_row-index.

ENDFORM.                    " DOUBLE_CLICK
*&---------------------------------------------------------------------*
*&      Form  save_log
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  get_row_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_row_data.

  __cls it_row_tab.

  SELECT * FROM zess_bank_if_req
  INTO CORRESPONDING FIELDS OF TABLE it_row_tab
  WHERE reqdate IN s_date
    AND pernr IN s_pernr.

ENDFORM.                    " get_row_data
*&---------------------------------------------------------------------*
*&      Form  dele_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form dele_data.
  __cls $gt_out.

  DATA: lt_row   TYPE lvc_t_row,
        ls_row   TYPE lvc_s_row,
        lt_roid  TYPE lvc_t_roid,
        lv_cnt(5),
        lv_dcnt(5),
        lv_msg(200).                 " Message

  DATA  lt_messages      LIKE messages       OCCURS 0 WITH HEADER LINE.
  DATA  $messages      LIKE messages       OCCURS 0 WITH HEADER LINE.


  CLEAR: lv_cnt, lt_row[], lt_roid[].

  PERFORM get_selected_rows TABLES $gt_out.


  LOOP AT $gt_out.
    delete from zess_bank_if_req
                         WHERE pernr EQ $gt_out-pernr
                           AND reqdate EQ $gt_out-reqdate
                           AND bank_type_code EQ $gt_out-bank_type_code.
    IF sy-subrc NE 0 .
    ELSE.
      COMMIT WORK.

      delete gt_out  WHERE pernr = $gt_out-pernr
                       AND reqdate = $gt_out-reqdate
                       AND bank_type_code = $gt_out-bank_type_code.


    ENDIF.
  ENDLOOP.


endform.                    " dele_data
*&---------------------------------------------------------------------*
*&      Form  really_dele?
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form really_dele?.
  DATA $exists(1).
  DATA l_answer(1).

  PERFORM pop_up USING
      'This will delete the queue line(s) that you selected.'
      'Do you really want to proceed?' ' '
                 CHANGING l_answer.

  IF l_answer NE 'J'.
    g_error = true.
    MESSAGE s000 WITH 'Processing was canceled by user.'.
  ENDIF.

endform.                    " really_dele?
