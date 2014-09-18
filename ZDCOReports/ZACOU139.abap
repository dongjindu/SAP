*----------------------------------------------------------------------
* Program ID        : ZACOU139
* Title             : [CO] Create Costing Cockpit Variant
* Created on        : Feb-12. 2008
* Created by        : I.G.MOON
* Specifications By : Andy Choi
* Description       : BDC for Cost Estimate
*----------------------------------------------------------------------
REPORT zafiu139 MESSAGE-ID zmco.
TABLES: keko,                      " Product Costing - Header Data
        tka01, ztcou100 , kala.

INCLUDE zacoui00.
TABLES : marc,sscrfields.
INCLUDE <icon>.                        " icon

*----------------------------------------------------------------------
* Selection-Screen
*----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.
PARAMETERS: p_ml AS CHECKBOX.
PARAMETERS: p_sl NO-DISPLAY.
PARAMETERS: p_xl AS CHECKBOX.

SELECT-OPTIONS: s_matnr FOR keko-matnr.
SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN BEGIN OF BLOCK b0 WITH FRAME TITLE text-001.
PARAMETER p_kokrs LIKE keko-kokrs OBLIGATORY MEMORY ID cac.
*select-options s_kalka for ztcou100-kalka no intervals
*                      obligatory memory id kka.
PARAMETERS: p_klvar TYPE ck_klvar MEMORY ID krt,
            p_year  LIKE keko-bdatj OBLIGATORY MEMORY ID bdtj,
            p_poper LIKE keko-poper OBLIGATORY MEMORY ID popr.
SELECTION-SCREEN END OF BLOCK b0.

SELECTION-SCREEN BEGIN OF BLOCK b1s WITH FRAME TITLE text-0s2.
PARAMETERS confmat TYPE ck_select_confmat_only DEFAULT 'X'.
PARAMETERS noueb TYPE ck_noueb.

SELECTION-SCREEN END OF BLOCK b1s.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-002.
PARAMETERS: p_day1  RADIOBUTTON GROUP r1,
            p_day15 RADIOBUTTON GROUP r1,
            p_lday  RADIOBUTTON GROUP r1 DEFAULT 'X'.
PARAMETERS: p_valdt LIKE keko-bwdat,
            p_aldat LIKE keko-aldat.

PARAMETER   p_mgtyp LIKE cki94a-mgtyp DEFAULT 'NAFTA'.

SELECTION-SCREEN END OF BLOCK b1.

* Layout
SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME TITLE text-01s.
PARAMETER p_vari TYPE slis_vari.
SELECTION-SCREEN END OF BLOCK b4.
PARAMETERS p_mode DEFAULT 'N'.

*/////////////////////

*Create Session
PARAMETERS session(1) TYPE c DEFAULT 'X' NO-DISPLAY .
*Call transaction
PARAMETERS ctu(1)     TYPE c DEFAULT ' '  NO-DISPLAY.
PARAMETERS p_scount TYPE i DEFAULT 50000 NO-DISPLAY.
"No of records per session
PARAMETERS group(12) DEFAULT 'CO_139_ANDY' NO-DISPLAY.
"Group name of session
* Run-mode
* A: show all bdc_fields
* E: show bdc_field on error only
* N: do not display bdc_field
PARAMETERS ctumode LIKE ctu_params-dismode DEFAULT 'N' NO-DISPLAY.

* user for session in batch
PARAMETERS user(12) DEFAULT sy-uname NO-DISPLAY.
PARAMETERS cupdate LIKE ctu_params-updmode DEFAULT 'A' NO-DISPLAY.
"S: synchronously
"A: asynchronously
"L: local
PARAMETERS holddate  LIKE sy-datum NO-DISPLAY.
* 'X' = keep   session if finished
PARAMETERS keep(1)   TYPE c DEFAULT ' '    NO-DISPLAY.
"' ' = delete session if finished
PARAMETERS nodata    DEFAULT '/' LOWER CASE NO-DISPLAY.          "nodata

* 'X' = no transaction logging
PARAMETERS smalllog(1) TYPE c DEFAULT 'X' NO-DISPLAY.

*       Batchinputdata of single transaction
DATA:   bdcdata LIKE bdcdata    OCCURS 0 WITH HEADER LINE.
*       messages of call transaction
DATA:   messtab LIKE bdcmsgcoll OCCURS 0 WITH HEADER LINE.
*       error session opened (' ' or 'X')
DATA:   e_group_opened.
*       message texts


*----------------------------------------------------------------------*
* Macros
*----------------------------------------------------------------------*

DEFINE __cls.                          " clear & refresh
  clear &1.refresh &1.
END-OF-DEFINITION.

DATA: BEGIN OF gt_rsparams OCCURS 0.
        INCLUDE STRUCTURE rsparams.
DATA: END OF gt_rsparams.

DEFINE __add_sel_tab .
  gt_rsparams-selname = &1.
  gt_rsparams-kind    = &2.
  gt_rsparams-sign    = ' '.
  gt_rsparams-option  = ' '.
  gt_rsparams-low     = &3.
  clear gt_rsparams-high.
  append gt_rsparams.
END-OF-DEFINITION.

DEFINE __add_sel_tab_mat .
  gt_rsparams-selname = &1.
  gt_rsparams-kind    = &2.
  gt_rsparams-sign    = &3.
  gt_rsparams-option  = &4.
  gt_rsparams-low     = &5.
  gt_rsparams-high    = ' '.
  append gt_rsparams.
END-OF-DEFINITION.

DEFINE __message.
  call function 'POPUP_TO_INFORM'
       exporting
            titel = &1
            txt1  = &2
            txt2  = sy-subrc.
END-OF-DEFINITION.

DEFINE __focus.
  call method cl_gui_control=>set_focus
      exporting
        control = &1 .
END-OF-DEFINITION.

DEFINE __process.
  perform show_progress using &1 &2.
END-OF-DEFINITION.

DEFINE __set_icon_no_error.
  perform set_icon using pr_data_changed
                         ls_good-row_id
                         icon_led_yellow.
END-OF-DEFINITION.

DEFINE __set_icon_error.
  perform set_icon using pr_data_changed
                         ls_good-row_id
                         icon_led_red.
END-OF-DEFINITION.

DEFINE __set_error.

  call method pr_data_changed->add_protocol_entry
               exporting
    i_msgid = '0K' i_msgno = '000'  i_msgty = 'E'
    i_msgv1 = &1
    i_fieldname = ls_good-fieldname
    i_row_id = ls_good-row_id.

  error_in_data = 'X'.

END-OF-DEFINITION.

****************************** constants *******************************
* constants
CONSTANTS: out_to_screen  VALUE '1',
           out_to_printer VALUE '2',
           out_to_pc_file VALUE '3',
           dialog VALUE ' ',
           no_dialog VALUE 'X',
           directory(80) VALUE 'c:\temp\'. " Down directory

CONSTANTS:  false VALUE ' ',
            true  VALUE 'X'.

DATA: BEGIN OF fields OCCURS 10.
        INCLUDE STRUCTURE help_value.
DATA: END OF fields.
DATA: BEGIN OF valuetab OCCURS 10,
        line(80),
END OF valuetab.

DATA: pripar LIKE pri_params,          " ImageLink structure
      arcpar LIKE arc_params,          " ImageLink structure
      val,
      lay(16),
      lines TYPE i,
      rows TYPE i.
DATA numc1(1) TYPE n.

DATA: g_error(1),
      g_repid  LIKE sy-repid,
      g_ix     LIKE sy-tabix,
      gv_index      TYPE i.

DATA : dir.
DATA cursor_f(10).
DATA $sheet(3) TYPE n.
FIELD-SYMBOLS <f>.

****************************** Global Data *****************************

TYPES: BEGIN OF ty_row_tab.
        INCLUDE STRUCTURE ztcou100.
TYPES:  maktx TYPE maktx.
TYPES:  lfimg TYPE lfimg.
TYPES:  source(2).
TYPES: END OF ty_row_tab.

TYPES: BEGIN OF ty_out.
INCLUDE  TYPE ty_row_tab.
TYPES:
     icon TYPE icon-id,
     icon2 TYPE icon-id,
     chk(1),
     err(1),
     err2(1),
     idx TYPE i,
     msg LIKE cfgnl-msglin,
     msg2 LIKE cfgnl-msglin.
TYPES   celltab  TYPE lvc_t_styl.
TYPES   tabcolor TYPE slis_t_specialcol_alv.
TYPES: END OF ty_out.

DATA  : itab       TYPE TABLE OF ty_row_tab WITH HEADER LINE,
        it_row_tab TYPE TABLE OF ty_row_tab WITH HEADER LINE,
        gt_out     TYPE TABLE OF ty_out     WITH HEADER LINE,
        it_sales   TYPE TABLE OF ty_row_tab WITH HEADER LINE.

DATA: BEGIN OF it_anlz OCCURS 0,
        bukrs	TYPE bukrs,      " Company Code
        anln1	TYPE anln1,      " Main asset number
        anln2	TYPE anln2,      " Asset sub-number
        kostl	TYPE kostl,
        kostlv TYPE kostlv,
        adatu TYPE adatu,
        bdatu TYPE bdatu,      " Date validity ends
        stort TYPE stort,
      END   OF it_anlz.

DATA: BEGIN OF it_anlc OCCURS 0,
        bukrs	TYPE bukrs,      " Company Code
        anln1	TYPE anln1,      " Main asset number
        anln2	TYPE anln2,      " Asset sub-number
        answl	TYPE answl,
      END   OF it_anlc.

DATA $it_anlc LIKE it_anlc OCCURS 0 WITH HEADER LINE.

* Printing option for Label
DATA: zoptions LIKE	itcpo OCCURS 0 WITH HEADER LINE.
DATA: zprinter(4) VALUE 'RFL'.

* temp table for selected rows
DATA  $gt_out LIKE gt_out OCCURS 0 WITH HEADER LINE.

TYPES: BEGIN OF ty_verid,
         verid TYPE verid,
         text1 TYPE vers_text,
         bstmi TYPE sa_losvn,
         bstma TYPE sa_losbs,
         adatu TYPE adatm,
         bdatu TYPE bdatm,
       END OF ty_verid.

TYPES ddshretval_table TYPE TABLE OF ddshretval.

DATA: gv_cnt      TYPE i,              " Total count
      gv_ldate    TYPE sydatum,        " last date of month
      gv_pyear    TYPE bdatj,          " Pervious year
      gv_ppoper   TYPE poper,          " Pervious period
      gt_verid    TYPE TABLE OF ty_verid    WITH HEADER LINE.

DATA: gv_valdt TYPE sydatum,  "valuation date
      gv_cstdt TYPE sydatum,  "Costing run date
      gv_aldat TYPE sydatum,  "Qty date
      gv_tcnt TYPE i,
      gv_ccnt TYPE i,
      gv_rcnt TYPE i,
      gv_lcnt TYPE i,
      gv_mm02 TYPE i,
      gv_lock,
      gv_but  TYPE i VALUE 1,
      gv_klvar TYPE ck_klvar,
      gv_cnt_f TYPE i,
      gv_cnt_m TYPE i.


*----------------------------------------------------------------------*
* Define local class
*----------------------------------------------------------------------*
CLASS lcl_event_receiver DEFINITION.
  PUBLIC SECTION.
    TYPES: BEGIN OF ztcou100_k,
              werks TYPE werks_d,
              matnr TYPE matnr,
           END OF ztcou100_k.

    TYPES: ztcou100_key   TYPE STANDARD TABLE OF ztcou100_k,
           ztcou100_table TYPE STANDARD TABLE OF ztcou100.

    METHODS:
      handle_data_changed
         FOR EVENT data_changed OF cl_gui_alv_grid
             IMPORTING er_data_changed.

    METHODS:
      get_inserted_rows
           EXPORTING
              inserted_rows TYPE ztcou100_key.

    METHODS:
      get_deleted_rows
          EXPORTING
              deleted_rows TYPE ztcou100_table.

    METHODS:
       refresh_delta_tables.

    METHODS: set_table_is_initial.

    METHODS: set_table_is_not_initial.

    METHODS: table_is_initial
                RETURNING value(initial) TYPE char01.

    METHODS:
    handle_double_click FOR EVENT double_click OF cl_gui_alv_grid
            IMPORTING e_row
                      e_column
                      es_row_no.

    METHODS:
             on_f4 FOR EVENT onf4 OF cl_gui_alv_grid
                   IMPORTING sender
                             e_fieldname
                             e_fieldvalue
                             es_row_no
                             er_event_data
                             et_bad_cells
                             e_display,

             my_f4 IMPORTING sender        TYPE REF TO cl_gui_alv_grid
                             et_bad_cells  TYPE lvc_t_modi
                             es_row_no     TYPE lvc_s_roid
                             er_event_data TYPE REF TO cl_alv_event_data
                             e_display     TYPE c
                             e_fieldname   TYPE lvc_fname
                   EXPORTING lt_f4         TYPE ddshretval_table.

  PRIVATE SECTION.
    DATA: inserted_rows TYPE ztcou100_key,
          deleted_rows TYPE STANDARD TABLE OF ztcou100.
    DATA  error_in_data TYPE c.
    DATA  initial_table TYPE c.

** Methods to modularize event handler method HANDLE_DATA_CHANGED:
*
    METHODS:
      update_delta_tables
         IMPORTING
            pr_data_changed TYPE REF TO cl_alv_changed_data_protocol.

    METHODS:
      get_cell_values
           IMPORTING
             row_id          TYPE int4
             pr_data_changed TYPE REF TO cl_alv_changed_data_protocol
           EXPORTING
             key             TYPE ztcou100_k.

ENDCLASS.                   " LCL_EVENT_RECEIVER Definition

*----------------------------------------------------------------------*
* Implementation local class
*----------------------------------------------------------------------*
CLASS lcl_event_receiver IMPLEMENTATION.

* Setting for Change data
  METHOD handle_data_changed.
    error_in_data = space.

    CALL METHOD update_delta_tables( er_data_changed ).

    IF error_in_data = 'X'.
      CALL METHOD er_data_changed->display_protocol.
    ENDIF.

  ENDMETHOD.                    " handle_data_changed

* Get values of possible entries
  METHOD on_f4.
    PERFORM on_f4 USING sender
                        e_fieldname
                        e_fieldvalue
                        es_row_no
                        er_event_data
                        et_bad_cells
                        e_display.
  ENDMETHOD.                                                "on_f4

  METHOD my_f4.
    PERFORM my_f4 TABLES lt_f4
                  USING  sender
                         et_bad_cells
                         es_row_no
                         er_event_data
                         e_display
                         e_fieldname.
  ENDMETHOD.

*-------------------------------------------------------
  METHOD update_delta_tables.

    DATA: lt_good_cells TYPE lvc_t_modi,
          ls_good TYPE lvc_s_modi,
          ls_key TYPE ztcou100_k,
          ls_ztcou100 TYPE ztcou100,
          l_werks LIKE gt_out-werks,
          l_row_id LIKE sy-tabix,
          lt_ins_rows TYPE ztcou100_key,
          lt_key TYPE ztcou100_k,
          dup_chk(1),
          l_matnr TYPE matnr.

    DATA : flag_inserted, flag_deleted.

    DATA: l_ins_row TYPE lvc_s_moce,
          l_del_row TYPE lvc_s_moce,
          ls_outtab LIKE LINE OF gt_out.

    LOOP AT pr_data_changed->mt_deleted_rows INTO l_del_row.
      READ TABLE gt_out INTO ls_outtab INDEX l_del_row-row_id.
      IF sy-subrc NE 0.
        MESSAGE i000(0k) WITH text-e01.
      ELSE.
        MOVE-CORRESPONDING ls_outtab TO ls_ztcou100.
        APPEND ls_ztcou100 TO deleted_rows.
        DELETE me->inserted_rows
             WHERE werks  = ls_outtab-werks
             AND   matnr = ls_outtab-matnr.
      ENDIF.
      flag_deleted = true.
    ENDLOOP.

    CHECK flag_deleted NE true.

    IF me->table_is_initial( ) EQ 'X'.
      CALL METHOD get_cell_values
            EXPORTING row_id          = 1
                      pr_data_changed = pr_data_changed
            IMPORTING key             = ls_key.

      APPEND ls_key TO inserted_rows.
      CALL METHOD me->set_table_is_not_initial.
    ENDIF.

*    field-symbols: <fs> type table.    " Output table
*
*    loop at pr_data_changed->mt_inserted_rows into l_ins_row.
*
*      assign pr_data_changed->mp_mod_rows->* to <fs>.
*
*      loop at <fs> into ls_outtab.
*        modify <fs> from ls_outtab index sy-tabix.
*      endloop.
*
*      call method get_cell_values
*              exporting row_id          = l_ins_row-row_id
*                        pr_data_changed = pr_data_changed
*              importing key             = ls_key.
*
*      append ls_key to inserted_rows.
*      flag_inserted = true.
*
*    endloop.
*
*    check flag_inserted ne true.

    LOOP AT pr_data_changed->mt_good_cells INTO ls_good.
      l_row_id = ls_good-row_id.

      CASE ls_good-fieldname.
        WHEN 'WERKS' OR 'MATNR' OR 'VERID'.

*          call method pr_data_changed->modify_cell
*                 exporting i_row_id    = ls_good-row_id
*                           i_fieldname = ls_good-fieldname
*                           i_value     = space.
*
*          call method pr_data_changed->get_cell_value
*                      exporting
*                            i_row_id = ls_good-row_id
*                            i_fieldname = ls_good-fieldname
*                      importing e_value = l_matnr.
*
      ENDCASE.
      IF ls_good-fieldname EQ 'WERKS' OR
         ls_good-fieldname EQ 'MATNR'.
        dup_chk = true.
      ENDIF.
      EXIT.
    ENDLOOP.

    CHECK error_in_data NE true.

    CALL METHOD get_cell_values
         EXPORTING row_id          = l_row_id
                   pr_data_changed = pr_data_changed
         IMPORTING key             = ls_key.

*    check g_bukrs ne space
*      and ls_key-werks ne space
*      AND ls_key-matnr NE space
*      and dup_chk eq true.
*
*    select single * from ztfiu128 into ls_ztfiu128
*              where bukrs  = g_bukrs
*                and anln1  = ls_key-anln1
*                and anln2 =  ls_key-anln2.
*
*    if sy-subrc = 0.
*      __set_error text-m03.
*    else.
*
*      __set_icon_no_error.
*
*    endif.

    READ TABLE gt_out WITH KEY werks  = ls_key-werks
                               matnr  = ls_key-matnr
                               TRANSPORTING NO FIELDS.

    IF sy-subrc = 0 AND l_row_id NE sy-tabix.
      __set_error text-m04.

    ELSE.
      __set_icon_no_error.
    ENDIF.



  ENDMETHOD.
*---------------------------------------------------------

  METHOD get_cell_values.
* get values of key cells of row ROW_ID

    CALL METHOD pr_data_changed->get_cell_value
          EXPORTING
                 i_row_id    = row_id
                 i_fieldname = 'WERKS'
               IMPORTING
                 e_value = key-werks.

    IF sy-subrc NE 0.
      MESSAGE i000(0k) WITH text-e02.
    ENDIF.

* MATNR
    CALL METHOD pr_data_changed->get_cell_value
          EXPORTING
                 i_row_id    = row_id
                 i_fieldname = 'MATNR'
               IMPORTING
                 e_value = key-matnr.

    IF sy-subrc NE 0.
      MESSAGE i000(0k) WITH text-e02.
    ENDIF.

  ENDMETHOD.

  METHOD get_inserted_rows.
    inserted_rows = me->inserted_rows.
  ENDMETHOD.
*------------------------------------------------------

  METHOD get_deleted_rows.
    deleted_rows = me->deleted_rows.
  ENDMETHOD.
*------------------------------------------------------
  METHOD refresh_delta_tables.
    CLEAR me->inserted_rows[].
    CLEAR me->deleted_rows[].
  ENDMETHOD.
*------------------------------------------------------
  METHOD set_table_is_initial.
    initial_table = 'X'.
  ENDMETHOD.
*------------------------------------------------------
  METHOD set_table_is_not_initial.
    initial_table = space.
  ENDMETHOD.
*------------------------------------------------------
  METHOD table_is_initial.
    IF initial_table = 'X'.
      initial = 'X'.
    ELSE.
      initial = space.
    ENDIF.
  ENDMETHOD.

  METHOD handle_double_click.
    PERFORM double_click USING e_row
                               e_column
                               es_row_no.
  ENDMETHOD.                    " handle_double_click

ENDCLASS.                   " LCL_EVENT_RECEIVER Implementation

DATA g_event_receiver  TYPE REF TO lcl_event_receiver.

************************************************************************
DATA  : flag_data_changed,
    info(80).

DATA: BEGIN OF ftab OCCURS 10,
    fcode(6),
  END OF ftab.
*----------------------------------------------------------------------*
* INITIALIZATION
*----------------------------------------------------------------------*
INITIALIZATION.

*  sy-title = '[CO] Create Costing Cockpit Variant'.
  PERFORM make_button.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_vari.
  PERFORM alv_variant_f4 CHANGING p_vari.

*----------------------------------------------------------------------*
AT SELECTION-SCREEN.
*----------------------------------------------------------------------*
  CLEAR g_error.

  CASE sscrfields-ucomm.
    WHEN 'TRAF'.
      PERFORM initialize.
      PERFORM transfer_.
      PERFORM move_out.
      PERFORM set_output .

    WHEN 'PSET' OR 'FC04'.
      PERFORM get_print USING dialog.
  ENDCASE.

*----------------------------------------------------------------------*
START-OF-SELECTION.
*----------------------------------------------------------------------*

  PERFORM initialize.

  PERFORM get_row_data.
  CHECK g_error EQ space.
  PERFORM move_out.
  PERFORM set_output .

END-OF-SELECTION.

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
  CALL SCREEN 100.

ENDFORM.                    " SET_OUTPUT
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
     'WERKS'  ' ' 'X' ' ' 'X' ' ',
     'MATNR'  ' ' 'X' ' ' 'X' ' ',
     'MAKTX'  ' ' 'X' ' ' 'X' ' ',
     'VERID'  ' ' 'X' ' ' 'X' ' '.
ENDFORM.                    " SORT_BUILD

*&---------------------------------------------------------------------*
*&      Form  INITIALIZE_
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM initialize.

  CLEAR : g_error.
  __cls : it_row_tab.

* Check Controlling Area
  SELECT SINGLE lmona INTO tka01-lmona FROM tka01
    WHERE kokrs = p_kokrs.

  IF sy-subrc <> 0.
    MESSAGE e038 WITH p_kokrs.
  ENDIF.

* Set Validity Date (Start)
  CALL FUNCTION 'LAST_DAY_IN_PERIOD_GET'
       EXPORTING
            i_gjahr        = p_year
            i_periv        = tka01-lmona
            i_poper        = p_poper
       IMPORTING
            e_date         = gv_ldate
       EXCEPTIONS
            input_false    = 1
            t009_notfound  = 2
            t009b_notfound = 3
            OTHERS         = 4.

  PERFORM get_date.

* Get BDC Options
  PERFORM get_opt USING p_mode.


ENDFORM.                    " INITIALIZE_
*&---------------------------------------------------------------------*
*&      Form  refine_row_itab
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM refine_row_itab.
  CHECK g_error EQ space.
ENDFORM.                    " refine_row_itab
*&---------------------------------------------------------------------*
*&      Form  MOVE_OUT_
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM move_out.
  __cls gt_out.
  LOOP AT it_row_tab.
    MOVE-CORRESPONDING it_row_tab TO gt_out.
    APPEND gt_out.
  ENDLOOP.

  PERFORM apply_icon.

ENDFORM.                    " MOVE_OUT_
*&---------------------------------------------------------------------*
*&      Form  TRANSFER_
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM transfer_.

ENDFORM.                    " TRANSFER_
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET TITLEBAR '100'.
*   Exclude toolbar
  PERFORM exclude_functions.
ENDMODULE.                 " STATUS_0100  OUTPUT
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
  PERFORM user_status.

  __focus g_grid.
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

    WHEN 'PRNT'.
      PERFORM : bar_print,
                clear_chk,
                refresh_alv.
      __focus g_grid.

    WHEN 'SWITCH'.
      IF sy-dynnr EQ '0100'.
        PERFORM switch_edit_mode.
      ENDIF.
      __focus g_grid.

    WHEN 'SAVE'.
      CLEAR g_error.

      PERFORM really?.
      CHECK g_error NE true.

      PERFORM : save_table,
                refresh_alv,
                clear_chk.

      __focus g_grid.

    WHEN 'SAV2'.
      CLEAR g_error.

      PERFORM really?.
      CHECK g_error NE true.

      PERFORM : save_table_2,
                refresh_alv,
                clear_chk.

      __focus g_grid.
    WHEN 'INIT'.
      PERFORM init_ck40.

    WHEN 'CRUN'.
      CLEAR g_error.

      PERFORM really?.
      CHECK g_error NE true.

      PERFORM : pre_crun,
                refresh_alv,
                clear_chk.

      __focus g_grid.

  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0100  INPUT
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
                g_event_receiver->handle_double_click FOR g_grid,
                g_event_receiver->on_f4               FOR g_grid.

*   Create field category
  PERFORM create_field_category USING false.

  CALL METHOD g_grid->register_edit_event
        EXPORTING
          i_event_id = cl_gui_alv_grid=>mc_evt_enter.

  CALL METHOD g_grid->register_edit_event
       EXPORTING i_event_id = cl_gui_alv_grid=>mc_evt_modified.

  CALL METHOD g_grid->set_ready_for_input
     EXPORTING
            i_ready_for_input = 0.

  PERFORM sort_build USING gt_sort[].

*   Setting for layout
  PERFORM set_lvc_layout.

*   Set colors
  PERFORM set_color.

*   Define cell attribute
  PERFORM build_cell_attr.

*   Define possible entry fields
  PERFORM create_f4_fields.

*   Set variant
  gv_repid = gs_variant-report = sy-repid.
  gs_variant-variant = p_vari.

ENDFORM.                    " CREATE_AND_INIT_ALV
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
           USING: cl_gui_alv_grid=>mc_fc_average,
                  cl_gui_alv_grid=>mc_fc_graph.
*                  cl_gui_alv_grid=>mc_fc_loc_undo,
*                  cl_gui_alv_grid=>mc_fc_info,
*                  cl_gui_alv_grid=>mc_fc_loc_copy_row,
*                  cl_gui_alv_grid=>mc_fc_loc_append_row,
*                  cl_gui_alv_grid=>mc_fc_loc_cut,
*                  cl_gui_alv_grid=>mc_fc_loc_insert_row,
*                  cl_gui_alv_grid=>mc_fc_loc_move_row,
*                  cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
*
ENDFORM.                    " EXCLUDE_FUNCTIONS
*&---------------------------------------------------------------------*
*&      Form  CREATE_FIELD_CATEGORY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_FALSE  text
*----------------------------------------------------------------------*
FORM create_field_category USING mode_edit.

  DATA lv_cnt TYPE i.
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

  __catalog :
    ' ' 'WERKS'  'Plant'              '5'  'CHAR' '',
         ' ' 'MATNR'  'Product'            '18' 'CHAR' '',
         ' ' 'VERID'  'Vers'               '5'  'CHAR' '',
         ' ' 'MAKTX'  'Description'        '30' 'CHAR' '',
         ' ' 'SOURCE' 'Src'                '2'  'CHAR' '',
         ' ' 'LFIMG' 'QTY'                 '15' 'QUAN' '',
        ' '  'ICON'   'S'                  '3'  'ICON' '',
 ' '  'MSG'    'Remarks for Cst.Est              >'  '40' 'CHAR' '',
        ' '  'ICON2'  'S'                  '3'  'ICON' '',
 ' '  'MSG2'   'Remarks for Cst.Run              >'  '40' 'CHAR' ''.

  LOOP AT gt_fcat INTO gs_fcat.
    lv_cnt = lv_cnt + 1.
    CASE gs_fcat-fieldname.

      WHEN 'WERKS'.
        gs_fcat-ref_field = gs_fcat-fieldname.
        gs_fcat-ref_table = 'T001W'.
        MODIFY gt_fcat INDEX lv_cnt FROM gs_fcat
               TRANSPORTING ref_field ref_table.

      WHEN 'MATNR'.
        gs_fcat-ref_field = gs_fcat-fieldname.
        gs_fcat-ref_table = 'MARA'.
        MODIFY gt_fcat INDEX lv_cnt FROM gs_fcat
               TRANSPORTING ref_field ref_table.

      WHEN 'VERID'.
        gs_fcat-ref_field = gs_fcat-fieldname.
        gs_fcat-f4availabl = 'X'.
        MODIFY gt_fcat INDEX lv_cnt FROM gs_fcat
          TRANSPORTING ref_field ref_table f4availabl.
    ENDCASE.

  ENDLOOP.

ENDFORM.                    " CREATE_FIELD_CATEGORY
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
  CLEAR: gs_specialcol, gt_specialcol[], gt_out-tabcolor[].

  DEFINE __color.
    gs_specialcol-fieldname = &1 .
    gs_specialcol-color-col = &2 .
    gs_specialcol-color-int = &3 .
    append gs_specialcol to gt_specialcol .
  END-OF-DEFINITION.

  __color :
   'WERKS'  '1' 0,
   'MATNR'  '2' 0,
   'MAKTX'  '2' 0,
   'VERID'  '3' 0,
   'ICON'   '2' 0,
   'MSG'    '2' 0.

  gt_out-tabcolor[] = gt_specialcol[].
  MODIFY gt_out TRANSPORTING tabcolor WHERE tabcolor IS initial.

ENDFORM.                    " SET_COLOR
*&---------------------------------------------------------------------*
*&      Form  INFO_TEXT_SET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_FALSE  text
*----------------------------------------------------------------------*
FORM info_text_set USING p_true.

  IF p_true EQ true.
    info = text-015.
  ELSE.
    info = text-015.
  ENDIF.

ENDFORM.                    " info_text_set
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
*&      Form  get_selected_rows
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_$GT_OUT  text
*----------------------------------------------------------------------*
FORM get_selected_rows TABLES $gt_out STRUCTURE gt_out.

  PERFORM clear_chk.

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

  __cls $gt_out.

  READ TABLE lt_rows INDEX 1.
  IF sy-subrc NE 0.
    $gt_out[] = gt_out[].
    gt_out-chk = true .
    MODIFY gt_out TRANSPORTING chk WHERE chk EQ false.
  ELSE.
    LOOP AT lt_rows WHERE rowtype IS initial.
      READ TABLE gt_out INDEX lt_rows-index.
      gt_out-chk = true .
      MODIFY gt_out INDEX lt_rows-index .
    ENDLOOP.
    LOOP AT gt_out.
      CHECK gt_out-chk EQ true.
      $gt_out = gt_out.
      $gt_out-idx = sy-tabix.
      APPEND $gt_out.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " get_selected_rows
*&---------------------------------------------------------------------*
*&      Form  UPDATE_SELECTED_ROWS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM update_selected_rows USING p_flag.

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


  ELSE.
    LOOP AT lt_rows WHERE rowtype IS initial.

    ENDLOOP.
  ENDIF.

ENDFORM.                    " UPDATE_SELECTED_ROWS
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

  __cls lt_celltab.
  __cls gt_out-celltab.

  CLEAR gs_fcat.

  LOOP AT gt_fcat INTO gs_fcat.
    ls_celltab-fieldname = gs_fcat-fieldname.
    IF ls_celltab-fieldname = 'MAKTX' OR
       ls_celltab-fieldname = 'ICON' OR
       ls_celltab-fieldname = 'MSG'.
      ls_celltab-style = cl_gui_alv_grid=>mc_style_disabled.
    ELSE.
      ls_celltab-style = cl_gui_alv_grid=>mc_style_enabled.
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

*  data: lt_celltab type lvc_t_styl,
*        ls_celltab type lvc_s_styl.
*
*  __cls lt_celltab.
*
*  __cls gt_out-celltab.
*  modify gt_out transporting celltab where zastfix eq false.
*
*  clear gs_fcat.
*
*  loop at gt_fcat into gs_fcat.
*    ls_celltab-fieldname = gs_fcat1-fieldname.
*
*
*    if ls_celltab-fieldname = 'ZMEMO'.
*      ls_celltab-style = cl_gui_alv_grid=>mc_style_enabled.
*    else.
*      ls_celltab-style = cl_gui_alv_grid=>mc_style_disabled.
*    endif.
*
*    insert ls_celltab into table lt_celltab.
*  endloop.
*
*  insert lines of lt_celltab into table gt_out-celltab.
*  modify gt_out transporting celltab where zastfix eq false.
*
ENDFORM.                    " BUILD_CELL_ATTR1_LOCK
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
    IF flag_data_changed EQ true.
      CALL FUNCTION 'POPUP_TO_CONFIRM_LOSS_OF_DATA'
           EXPORTING
                textline1     = 'Data has not been saved yet.'
                textline2     = 'Do you want to continue anyway? '
                titel         = 'Confirmation'
                defaultoption = 'N'
           IMPORTING
                answer        = answer.
      CHECK answer EQ 'J'.
    ENDIF.
    CLEAR flag_data_changed.
    CALL METHOD g_grid->set_ready_for_input
                     EXPORTING i_ready_for_input = 0.
    PERFORM info_text_set USING false.
  ENDIF.

  PERFORM user_status.
  PERFORM build_cell_attr.

ENDFORM.                    " SWITCH_EDIT_MODE
*&---------------------------------------------------------------------*
*&      Form  get_row_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_row_data.
  __cls : it_row_tab, it_sales .

  IF NOT s_matnr[] IS INITIAL
  AND p_ml EQ space AND p_xl EQ space.
    PERFORM get_data.
  ENDIF.

  IF p_ml = 'X'.
    __process 'Read from ML...' '5'.
    PERFORM fill_from_ml.
  ENDIF.

*  IF p_sl = 'X'.
*    __process 'Read from Outbound...' '50'.
*    PERFORM fill_from_sl.
*  ENDIF.

  IF p_xl = 'X'.
    IF p_mgtyp IS INITIAL.
      MESSAGE s000 WITH 'Please enter the Qty.Structure Type!'.
      g_error = true.
      EXIT.
    ENDIF.
    __process 'Read from Outbound...' '5'.
    PERFORM fill_from_xl.
  ENDIF.

  DESCRIBE TABLE it_row_tab LINES gv_cnt.

ENDFORM.                    " get_row_data
*&---------------------------------------------------------------------*
*&      Form  APPLY_ICON
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

    CASE gt_out-err.
      WHEN 'X'.
        gt_out-icon = icon_led_red.
      WHEN 'N'.
        gt_out-icon = icon_led_green.
      WHEN OTHERS.
        CLEAR gt_out-icon.
    ENDCASE.

    MODIFY gt_out INDEX $ix TRANSPORTING icon .
  ENDLOOP.

ENDFORM.                    " APPLY_ICON

*---------------------------------------------------------------------*
*       FORM apply_icon2                                              *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM apply_icon2.

  DATA $ix LIKE sy-tabix.

  LOOP AT gt_out.
    $ix = sy-tabix.

    CASE gt_out-err2.
      WHEN 'X'.
        gt_out-icon2 = icon_led_red.
      WHEN 'N'.
        gt_out-icon2 = icon_led_green.
      WHEN OTHERS.
        CLEAR gt_out-icon2.
    ENDCASE.

    MODIFY gt_out INDEX $ix TRANSPORTING icon2 .
  ENDLOOP.

ENDFORM.                    " APPLY_ICON

*&---------------------------------------------------------------------*
*&      Form  printing
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM printing.

*  call function 'OPEN_FORM'
*       exporting
*            device                      = 'PRINTER'
*            dialog                      = 'X'
*            form                        = 'ZFI_ASSET_FLABEL'
*            language                    = sy-langu
*            options                     = zoptions
*            raw_data_interface          = '*'
*       exceptions
*            canceled                    = 1
*            device                      = 2
*            form                        = 3
*            options                     = 4
*            unclosed                    = 5
*            mail_options                = 6
*            archive_error               = 7
*            invalid_fax_number          = 8
*            more_params_needed_in_batch = 9
*            spool_error                 = 10
*            others                      = 11.
*
*  if sy-subrc <> 0.
*    message id sy-msgid type 'S' number sy-msgno
*            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*    exit.
*  endif.
*
*  loop at $gt_out.
*
*    perform get_ktext using $gt_out-kostl
*                            $gt_out-stand
*                   changing $gt_out-ktext.
*
*    concatenate $gt_out-anln1+4 '-' $gt_out-anln2 into $gt_out-$number.
*    call function 'WRITE_FORM'
*         exporting
*              element  = 'PRINT_LABEL'
*              function = 'SET'
*              type     = 'BODY'
*              window   = 'MAIN'.
*
*    if sy-subrc <> 0.
** MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*    endif.
*  endloop.
*
*  call function 'CLOSE_FORM'.
*  if sy-subrc <> 0.
** MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*  endif.
*
ENDFORM.                    " printing
*&---------------------------------------------------------------------*
*&      Form  INIT_PRINT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM init_print.

*move zqmnum+6(6)  to zoptions-TDDATASET. "UD1K940504
  MOVE '1' TO zoptions-tdcopies.
  MOVE 'X' TO zoptions-tdimmed.
  MOVE 'X' TO zoptions-tdnewid.
  MOVE zprinter TO zoptions-tddest.
  APPEND zoptions.

ENDFORM.                    " INIT_PRINT
*&---------------------------------------------------------------------*
*&      Form  MAKE_BUTTON
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM make_button.

*  WRITE:
*          ICON_DISPLAY_MORE AS ICON TO PSET,
*         'Print Setup' TO PSET+4(21),
*          ICON_PRINT_WITH_PARAMETERS AS ICON TO DIRP,
*         'Dirct Print' TO DIRP+4(21),
*          ICON_EXPORT AS ICON TO LOCF,
*         'Export to file' TO LOCF+4(21),
*          ICON_LIST AS ICON TO SCRN,
*         'Display ALV' TO SCRN+4(21).
*  WRITE:
*         'ALV Screen'  TO SSCRFIELDS-FUNCTXT_01,
*         'Download'    TO SSCRFIELDS-FUNCTXT_02,
*         'Dirct Print' TO SSCRFIELDS-FUNCTXT_03,
*         'Print Setup' TO SSCRFIELDS-FUNCTXT_04.
*  write:
*          icon_import as icon to traf,
*         'Transfer from Fixed Asset' to traf+4(25).
*
ENDFORM.                    " MAKE_BUTTON
*&---------------------------------------------------------------------*
*&      Form  GET_PRINT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_DIALOG  text
*----------------------------------------------------------------------*
FORM get_print USING $no_dialog.

  lay = 'X_70_100'.
  lines = 70.
  rows  = 100.
  CALL FUNCTION 'GET_PRINT_PARAMETERS'
       EXPORTING
            in_archive_parameters  = arcpar
            in_parameters          = pripar
            layout                 = lay
            line_count             = lines
            line_size              = rows
            no_dialog              = $no_dialog
       IMPORTING
            out_archive_parameters = arcpar
            out_parameters         = pripar
            valid                  = val
       EXCEPTIONS
            archive_info_not_found = 1
            invalid_print_params   = 2
            invalid_archive_params = 3
            OTHERS                 = 4.

ENDFORM.                    " GET_PRINT
*&---------------------------------------------------------------------*
*&      Form  SHOW_PROGRESS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_&1  text
*      -->P_&2  text
*----------------------------------------------------------------------*
FORM show_progress USING    pf_text
                            value(pf_val).

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
       EXPORTING
            percentage = pf_val
            text       = pf_text.

ENDFORM.                    " SHOW_PROGRESS
*&---------------------------------------------------------------------*
*&      Form  USER_STATUS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM user_status.
  __cls ftab.
*  if g_grid->is_ready_for_input( ) eq 1.
*  else.
  ftab-fcode = 'SAVE'.
  APPEND ftab.
  ftab-fcode = 'SAV2'.
  APPEND ftab.
*  endif.

  SET PF-STATUS '100' EXCLUDING ftab.

ENDFORM.                    " USER_STATUS
*&---------------------------------------------------------------------*
*&      Form  REALLY?
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM really?.
  DATA l_answer(1).

  PERFORM pop_up USING
      'Please confirm!'
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
*      -->P_1802   text
*      -->P_1803   text
*      -->P_1804   text
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
*&      Form  SAVE_TABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM save_table.

  DATA: lv_cnt(5),
        lv_dcnt(5),
        lv_msg(200).                 " Message

  DATA: l_cstdt(10),        " Costing Run Date
        l_valdt(10),        " Costing Run Date
        l_aldat(10).        " Costing Run Date


* Save seleted data to table ztcou100
  CLEAR: lv_cnt.

  PERFORM get_selected_rows TABLES $gt_out.

  DATA  : i_ztcou100 LIKE ztcou100 OCCURS 0 WITH HEADER LINE,
          ls_ztcou100 LIKE ztcou100.

  DATA: msg LIKE cfgnl-msglin.

  CLEAR : l_cstdt, l_valdt .

  PERFORM convert_date USING: gv_cstdt CHANGING l_cstdt,
                              gv_valdt CHANGING l_valdt,
                              gv_aldat CHANGING l_aldat.

  DATA $ix LIKE sy-tabix.
  LOOP AT $gt_out.

    IF $gt_out-idx IS INITIAL.
      $ix = sy-tabix.
    ELSE.
      $ix = $gt_out-idx.
    ENDIF.

    REFRESH: gt_bdc, gt_msg.
    CLEAR  : gt_bdc, gt_msg.

* Create Cost Estimates for Material

    PERFORM dynpro USING:
       'X'  'SAPLCKDI'        '0111',
       ' '  'CKI64A-KLVAR'    p_klvar,
       ' '  'CKI64A-MATNR'    $gt_out-matnr,     " Material
       ' '  'CKI64A-WERKS'    $gt_out-werks.     " Plant

    IF $gt_out-verid <> space.
      PERFORM dynpro USING:
       ' '  'CKI64A-FVIDK'    $gt_out-verid.

    ELSE.
    ENDIF.

    PERFORM dynpro USING:
       ' '  'BDC_OKCODE'      '/00',            " [Enter]
       'X'  'SAPLCKDI'        '0400',
       ' '  'CKI64A-KADAT'    l_valdt,          " Costing date from
       ' '  'CKI64A-BIDAT'    l_valdt,          " Costing date to
       ' '  'CKI64A-ALDAT'    l_aldat,          " Qty structure date
       ' '  'CKI64A-BWDAT'    l_valdt,          " Valuation date
       ' '  'BDC_OKCODE'       '=ENTR'.         " [Enter]


    PERFORM dynpro USING:
                             'X'  'SAPLCKDI'        '2100',
                             ' '  'BDC_OKCODE'      '=USR1',

                             'X'  'SAPLSPO1'        '0500',
                             ' '  'BDC_OKCODE'      '=OPT1',


                             'X'  'SAPLCKDI'        '2100',
                             ' '  'BDC_OKCODE'      '/EBACK',

                             'X'  'SAPLSPO1'        '0100',
                             ' '  'BDC_OKCODE'      '=OPT2'.

    CALL TRANSACTION 'CK11'   USING         gt_bdc
                              OPTIONS FROM  gs_opt
                              MESSAGES INTO gt_msg.

    CLEAR msg.
    IF sy-msgno EQ '419'.
      msg = 'success'.
      CLEAR gt_out-err.
      gt_out-err = 'N'.
    ELSE.
      PERFORM make_msg_string USING msg.
      gt_out-err = true.
    ENDIF.
    gt_out-msg = msg.
    MODIFY gt_out INDEX $ix TRANSPORTING err msg.

    lv_cnt = lv_cnt + 1.

  ENDLOOP.

  IF lv_dcnt > 0 OR lv_cnt > 0.
    CONCATENATE 'Data''s been processed;'
                 lv_cnt  'rec(s).'
            INTO lv_msg SEPARATED BY space.
    MESSAGE s000 WITH lv_msg.
  ENDIF.

  CLEAR flag_data_changed.

  PERFORM  apply_icon.

ENDFORM.                    " SAVE_TABLE
*&---------------------------------------------------------------------*
*&      Form  REALLY_ACT_OR_DACT?
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_OK_CODE  text
*----------------------------------------------------------------------*
FORM really_act_or_dact? USING  p_ok_code.

  DATA l_answer(1).

  PERFORM pop_up USING
      'The data will be changed!'
      'Do you really want to change status?' ' '
                 CHANGING l_answer.

  IF l_answer NE 'J'.
    g_error = true.
    MESSAGE s000 WITH 'Processing was canceled by user.'.
  ENDIF.

ENDFORM.                    " REALLY_ACT_OR_DACT?
*&---------------------------------------------------------------------*
*&      Form  clear_chk
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM clear_chk.
  CLEAR gt_out-chk.
  MODIFY gt_out TRANSPORTING chk WHERE chk EQ true.
ENDFORM.                    " clear_chk
*&---------------------------------------------------------------------*
*&      Form  bar_print
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM bar_print.
  PERFORM get_selected_rows TABLES $gt_out.
  READ TABLE $gt_out INDEX 1.
  IF sy-subrc EQ 0.
    PERFORM init_print.
    PERFORM printing.
  ELSE.
    MESSAGE s000 WITH 'No data to print!'.
  ENDIF.

ENDFORM.                    " bar_print
*&---------------------------------------------------------------------*
*&      Form  get_ktext
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_$GT_OUT_KOSTL  text
*      -->P_$GT_OUT_STAND  text
*      <--P_$GT_OUT_KTEXT  text
*----------------------------------------------------------------------*
FORM get_ktext USING    p_kostl
                        p_stand
               CHANGING p_ktext.

  CLEAR p_ktext.
  CHECK p_kostl NE space.
  CHECK p_stand NE space.
  DATA $werks TYPE werks_d.

*  select single werks into $werks
*          from csks
*          where kokrs eq *tka02-kokrs
*            and kostl eq p_kostl
*            and datbi >= sy-datum
*            and datab <= sy-datum.
*
*  check sy-subrc eq 0.

  SELECT SINGLE ktext INTO p_ktext
          FROM t499s
          WHERE " werks eq $werks  and
            stand EQ p_stand.

ENDFORM.                    " get_ktext
*&---------------------------------------------------------------------*
*&      Form  double_click
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_ROW  text
*      -->P_E_COLUMN  text
*      -->P_ES_ROW_NO  text
*----------------------------------------------------------------------*
FORM double_click USING  e_row     TYPE lvc_s_row
                         e_column  TYPE lvc_s_col
                         es_row_no TYPE lvc_s_roid.

*  clear gv_index.
*  gv_index = e_row-index.
*
*  read table gt_out index gv_index.
*
*  if sy-subrc eq 0.
**    if e_column = 'ANLN1' or e_column = 'ANLN2'.
*    check gt_out-anln1 ne space.
*    set parameter id : 'AN1'  field gt_out-anln1,
*                       'AN2'  field gt_out-anln2.
*    call transaction 'AS03' and skip first screen.
**    endif.
*  endif.
*
*  call method cl_gui_control=>set_focus exporting control = g_grid.

ENDFORM.                    " DOUBLE_CLICK

*&---------------------------------------------------------------------*
*&      Form  set_icon
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LS_GOOD_ROW_ID  text
*      -->P_ICON_LED_RED  text
*----------------------------------------------------------------------*
FORM set_icon USING pr_data_changed
                    TYPE REF TO cl_alv_changed_data_protocol
                    p_row_id
                    p_icon.

  CALL METHOD pr_data_changed->modify_cell
        EXPORTING i_row_id    = p_row_id
                  i_fieldname = 'ICON'
                  i_value     = p_icon.

  IF p_icon EQ icon_led_red.
    CALL METHOD pr_data_changed->modify_cell
          EXPORTING i_row_id    = p_row_id
                    i_fieldname = 'ERR'
                    i_value     = true.
  ELSE.
    CALL METHOD pr_data_changed->modify_cell
          EXPORTING i_row_id    = p_row_id
                    i_fieldname = 'ERR'
                    i_value     = space.

  ENDIF.

ENDFORM.                    " set_icon
*&---------------------------------------------------------------------*
*&      Form  fill_from_ml
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_from_ml.

  DATA : BEGIN OF it_ckmlmv003 OCCURS 0,
           bwkey      LIKE ckmlmv001-bwkey,
           matnr      LIKE ckmlmv001-matnr,
           aufnr      LIKE ckmlmv013-aufnr,
           verid_nd   LIKE ckmlmv001-verid_nd,
           meinh      LIKE ckmlmv003-meinh,
           out_menge  LIKE ckmlmv003-out_menge,
           maktg TYPE maktg,
           mtart TYPE mtart,
         END OF  it_ckmlmv003.

  DATA : BEGIN OF it_proc_gr OCCURS 0,
          werks LIKE ztco_shop_sum-bwkey,
          artnr     LIKE ztco_shop_sum-artnr,
        END OF it_proc_gr.

  DATA : it_ckmlmv003_temp LIKE it_ckmlmv003 OCCURS 0 WITH HEADER LINE.

  SELECT  b~bwkey b~matnr b~verid_nd
          c~aufnr
          a~out_menge a~meinh d~maktg e~mtart
    INTO CORRESPONDING FIELDS OF TABLE it_ckmlmv003
    FROM
    ( ( ckmlmv003 AS a
    INNER JOIN ckmlmv001 AS b
       ON b~kalnr = a~kalnr_bal )
    INNER JOIN ckmlmv013 AS c
       ON c~kalnr_proc = a~kalnr_in )
    INNER JOIN makt AS d
       ON d~matnr = b~matnr
       AND d~spras = sy-langu
    INNER JOIN mara AS e
       ON e~matnr = d~matnr
   WHERE a~mgtyp EQ '00001'
     AND a~gjahr EQ p_year
     AND a~perio EQ p_poper
     AND a~matnr IN s_matnr
     AND b~btyp  =  'BF'
     AND b~bwkey = a~werks
     AND c~flg_wbwg = 'X'
     AND c~autyp = '05' .

  SORT it_ckmlmv003 BY matnr.
*
  DATA: lv_stdpd     TYPE stdpd,          " Confi.Material
        lv_beskz     TYPE beskz,          " Procurement type
        lv_verid     TYPE verid,          " Production version
        lv_stlal     TYPE stalt,          " Alternative BOM
        lv_stlan     TYPE stlan,
        lv_cuobj     TYPE cuobm,          " Object
        lv_maktx     TYPE maktx,          " Material Description
        lv_werks     TYPE werks_d,        " Plant
        lt_values TYPE TABLE OF bapi_char_values WITH HEADER LINE,
        lw_msg       TYPE symsgv.

  LOOP AT it_ckmlmv003.

    IF p_sl = 'X'.
      CHECK it_ckmlmv003-mtart NE 'FERT'.
    ENDIF.

    CLEAR it_row_tab.
    it_row_tab-kokrs = p_kokrs.
    it_row_tab-bdatj = p_year.
    it_row_tab-poper = p_poper.
    it_row_tab-matnr = it_ckmlmv003-matnr.
    it_row_tab-werks = it_ckmlmv003-bwkey.

    it_row_tab-verid = it_ckmlmv003-verid_nd.
    it_row_tab-maktx = it_ckmlmv003-maktg.
    it_row_tab-source = 'ML'.
    it_row_tab-lfimg  =  it_ckmlmv003-out_menge .
    APPEND it_row_tab.

  ENDLOOP.


ENDFORM.                    " fill_from_ml
*&---------------------------------------------------------------------*
*&      Form  chk_mat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_CKMLMV003_MATNR  text
*      <--P_LV_STDPD  text
*      <--P_LV_VERID  text
*      <--P_LV_BESKZ  text
*      <--P_LV_STLAL  text
*      <--P_LV_STLAN  text
*      <--P_LV_CUOBJ  text
*      <--P_LV_MAKTX  text
*      <--P_LV_WERKS  text
*----------------------------------------------------------------------*
FORM chk_mat USING    p_matnr TYPE matnr
             CHANGING p_stdpd TYPE stdpd
                      p_verid TYPE verid
                      p_beskz TYPE beskz
                      p_stlal TYPE stalt
                      p_stlan TYPE stlan
                      p_cuobj TYPE cuobm
                      p_maktx TYPE maktx
                      p_werks TYPE werks_d.

  TYPES: BEGIN OF ty_mkal,
           verid TYPE verid,   " Production Version
           stlal TYPE stalt,   " Alternative BOM
           stlan TYPE stlan,
         END OF ty_mkal.

  DATA: lt_mkal  TYPE TABLE OF ty_mkal WITH HEADER LINE,
        lv_date  TYPE sydatum,
        l_stlnr  TYPE stnum,
        l_stlal  TYPE stalt,
        l_datuv  TYPE datuv.

  RANGES: r_verid  FOR mkal-verid.

* [FSC]
*   1. Material type: HALB or FERT or KMAT(Configurable)
*   2. Costing date in the period of validity
*   3. has BOM : exist BOM Usage(MKAL-STLAN)
*   4. has Routing
*     : task list type(MKAL-PLTYG):R, exist task list group(MKAL-PLNNG)
*   5. Status: Green or Yellow (MKAL-PRTG_S: 1 or 2)
*   6. Unchecked 'No Costing' (MARC-NCOST)

* [MIP]
*   - Material type: HALB
*   - other conditions are same FSC

  CLEAR:  lv_date, lt_mkal.
  REFRESH lt_mkal.

*valid on middle of period; FIXME - ANDY
  CONCATENATE p_year  p_poper+1(2) '15' INTO lv_date.

* Material Description
  SELECT SINGLE maktx INTO p_maktx
    FROM makt
   WHERE matnr = p_matnr
     AND spras = sy-langu.

  SELECT SINGLE b~beskz b~werks b~fvidk b~stdpd
    INTO (p_beskz, p_werks, p_verid, p_stdpd)
    FROM mara AS a
    JOIN marc AS b
      ON b~matnr = a~matnr
   WHERE a~matnr = p_matnr
     AND ( b~beskz = 'E' OR b~beskz = 'X' )
     AND ( a~mtart = 'HALB' OR        "1. Material Type
           a~mtart = 'FERT' OR
           a~mtart = 'KMAT' )
     AND b~ncost <> 'X'.              "6. Unchecked 'No Costing'

  REFRESH r_verid.
  IF NOT p_verid IS INITIAL.
    r_verid-low = p_verid.
    r_verid-option = 'EQ'. r_verid-sign = 'I'.
    APPEND r_verid.
  ENDIF.

  SELECT SINGLE verid stlal stlan INTO lt_mkal
    FROM mkal
   WHERE matnr  = p_matnr
     AND werks  = p_werks
     AND bdatu >= gv_ldate         " 2. period of validity
     AND adatu <= gv_ldate         " 2. period of validity
     AND stlan <> space            " 3. exist BOM Usage
 AND prfg_s <> '3'                                          " 5. Status
     AND verid IN r_verid.

  IF sy-subrc = 0.
    p_verid = lt_mkal-verid.            " Production Version
    p_stlal = lt_mkal-stlal.            " Alternative BOM
    p_stlan = '1'.                      " production
  ENDIF.


ENDFORM.                    " CHK_MAT
*&---------------------------------------------------------------------*
*&      Form  get_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data.

  SELECT a~matnr maktx c~werks
    INTO CORRESPONDING FIELDS OF TABLE it_row_tab
    FROM mara AS a
    JOIN makt AS b
      ON b~matnr = a~matnr
     AND b~spras = sy-langu
    JOIN marc AS c
      ON c~matnr = a~matnr
   WHERE a~matnr IN s_matnr.

  DESCRIBE TABLE it_row_tab LINES gv_cnt.

  IF gv_cnt = 0.
    MESSAGE s000 WITH 'Data not found.'.
  ENDIF.

ENDFORM.                    " get_data
*&---------------------------------------------------------------------*
*&      Form  create_f4_fields
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_f4_fields.

  gs_f4-fieldname  = 'VERID'.
  gs_f4-register   = 'X'.
  APPEND gs_f4 TO gt_f4.

  CALL METHOD g_grid->register_f4_for_fields
         EXPORTING it_f4 = gt_f4.

ENDFORM.                    " create_f4_fields
*&---------------------------------------------------------------------*
*&      Form  on_f4
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SENDER  text
*      -->P_E_FIELDNAME  text
*      -->P_E_FIELDVALUE  text
*      -->P_ES_ROW_NO  text
*      -->P_ER_EVENT_DATA  text
*      -->P_ET_BAD_CELLS  text
*      -->P_E_DISPLAY  text
*----------------------------------------------------------------------*
FORM on_f4 USING sender         TYPE REF TO cl_gui_alv_grid
                 e_fieldname    TYPE lvc_fname
                 e_fieldvalue   TYPE lvc_value
                 es_row_no      TYPE lvc_s_roid
                 er_event_data  TYPE REF TO cl_alv_event_data
                 et_bad_cells   TYPE lvc_t_modi
                 e_display      TYPE char01.

  DATA lt_f4 TYPE TABLE OF ddshretval.

* Call my personal f4-help
  CALL METHOD g_event_receiver->my_f4
    EXPORTING
      sender        = sender
      es_row_no     = es_row_no
      er_event_data = er_event_data
      et_bad_cells  = et_bad_cells
      e_display     = e_display
      e_fieldname   = e_fieldname
    IMPORTING
      lt_f4         = lt_f4.

* Assign the cell table fieldsymbol to the dereferenced data table and
* fill the table.
  ASSIGN er_event_data->m_data->* TO <f4tab>.

  READ TABLE lt_f4 INTO ls_f4 INDEX 1.

  CHECK NOT ls_f4 IS INITIAL.

  PERFORM f4_aply USING es_row_no-row_id
                        ls_f4-fieldname.

* To avoid standard f4-help.
  er_event_data->m_event_handled = 'X'.

ENDFORM.                                                    " ON_F4
*&---------------------------------------------------------------------*
*&      Form  my_f4
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_F4  text
*      -->P_SENDER  text
*      -->P_ET_BAD_CELLS  text
*      -->P_ES_ROW_NO  text
*      -->P_ER_EVENT_DATA  text
*      -->P_E_DISPLAY  text
*      -->P_E_FIELDNAME  text
*----------------------------------------------------------------------*
FORM my_f4 TABLES et_f4         STRUCTURE ddshretval
           USING  sender        TYPE REF TO cl_gui_alv_grid
                  et_bad_cells  TYPE lvc_t_modi
                  es_row_no     TYPE lvc_s_roid
                  er_event_data TYPE REF TO cl_alv_event_data
                  e_display     TYPE c
                  e_fieldname   TYPE lvc_fname.

  DATA : ls_out        LIKE LINE OF gt_out,
         lt_fcat       TYPE lvc_t_fcat,
         ls_fieldcat   TYPE lvc_s_fcat,
         lv_tabname    TYPE dd03v-tabname,
         lv_fieldname  TYPE dd03v-fieldname,
         lv_help_value TYPE help_info-fldvalue,
         lt_bad_cell   TYPE lvc_t_modi,
         l_wa          TYPE REF TO data.

  FIELD-SYMBOLS : <l_field_value> TYPE ANY,
                  <ls_wa>         TYPE ANY.

  CALL METHOD sender->get_frontend_fieldcatalog
    IMPORTING
      et_fieldcatalog = lt_fcat.

  READ TABLE gt_out INDEX es_row_no-row_id INTO ls_out.

  IF sy-subrc = 0.
    CREATE DATA l_wa LIKE LINE OF gt_out.
    ASSIGN l_wa->* TO <ls_wa>.
    <ls_wa> = ls_out.
  ENDIF.

  READ TABLE lt_fcat WITH KEY fieldname = e_fieldname
                     INTO ls_fieldcat.
  IF sy-subrc = 0.
    lv_tabname = ls_fieldcat-ref_table.
    lv_fieldname = ls_fieldcat-fieldname.

    ASSIGN COMPONENT ls_fieldcat-fieldname
                  OF STRUCTURE ls_out TO <l_field_value>.

    WRITE <l_field_value> TO lv_help_value.
  ENDIF.

  PERFORM f4_set IN PROGRAM bcalv_f4
                 USING sender
                       lt_fcat
                       lt_bad_cell
                       es_row_no-row_id
                       <ls_wa>.

* Fill internal table for possible entry
  CLEAR  : gs_values, gt_values, gs_fields, gt_fields.
  REFRESH: gt_values, gt_fields.

  IF e_fieldname = 'VERID'.
    PERFORM get_verid USING es_row_no-row_id e_fieldname.
  ENDIF.
* end

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
       EXPORTING
            retfield        = e_fieldname
       TABLES
            field_tab       = gt_fields
            value_tab       = gt_values
            return_tab      = et_f4[]
       EXCEPTIONS
            parameter_error = 1
            no_values_found = 2
            OTHERS          = 3.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                                                    " MY_F4
*&---------------------------------------------------------------------*
*&      Form  f4_aply
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ES_ROW_NO_ROW_ID  text
*      -->P_LS_F4_FIELDNAME  text
*----------------------------------------------------------------------*
FORM f4_aply USING  es_row_no_row_id
                    e_fieldname TYPE fieldname.
  ls_modi-row_id    = es_row_no_row_id.
  ls_modi-fieldname = e_fieldname.
  ls_modi-value     = ls_f4-fieldval.
  APPEND ls_modi TO <f4tab>.

  CASE e_fieldname.
    WHEN 'VERID'.
      gt_out-verid = ls_f4-fieldval.
  ENDCASE.

  READ TABLE gt_out INDEX es_row_no_row_id.

ENDFORM.                                                    " F4_APLY
*&---------------------------------------------------------------------*
*&      Form  get_verid
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ES_ROW_NO_ROW_ID  text
*      -->P_E_FIELDNAME  text
*----------------------------------------------------------------------*
FORM get_verid USING    p_row_id
                        p_e_fieldname.
* UD1K940522
  DATA : $matnr LIKE ztcou100-matnr,
         $werks LIKE ztcou100-werks.

  READ TABLE gt_out INDEX p_row_id.
  IF sy-subrc EQ 0.
    $matnr = gt_out-matnr.
    $werks = gt_out-werks.
  ELSE.
    EXIT.
  ENDIF.

  CLEAR gt_verid.
  REFRESH gt_verid.

  SELECT verid
         text1
         bstmi
         bstma
         adatu
         bdatu
    INTO TABLE gt_verid
    FROM mkal
   WHERE matnr EQ $matnr
     AND werks EQ $werks.
  LOOP AT gt_verid.
    gt_values-string = gt_verid-verid.
    APPEND gt_values.

    gt_values-string = gt_verid-text1.
    APPEND gt_values.

    gt_values-string = gt_verid-bstmi.
    APPEND gt_values.

    gt_values-string = gt_verid-bstma.
    APPEND gt_values.

    gt_values-string = gt_verid-adatu.
    APPEND gt_values.

    gt_values-string = gt_verid-bdatu.
    APPEND gt_values.
  ENDLOOP.

  CLEAR gt_fields.REFRESH gt_fields.

  gt_fields-fieldname = p_e_fieldname.
  gt_fields-position  = 1.
  gt_fields-intlen    = 4.
  gt_fields-outputlen = 4.
  gt_fields-reptext   = 'Ver.'.
  APPEND gt_fields.CLEAR gt_fields.

  gt_fields-fieldname = 'TEXT1'.
  gt_fields-position  = 2.
  gt_fields-intlen    = 30.
  gt_fields-outputlen = 30.
  gt_fields-reptext = 'Desc.'.
  APPEND gt_fields.CLEAR gt_fields.

  gt_fields-fieldname = 'BSTMI'.
  gt_fields-position  = 3.
  gt_fields-intlen    = 13.
  gt_fields-outputlen = 13.
  gt_fields-reptext = 'Lower'.
  APPEND gt_fields.CLEAR gt_fields.

  gt_fields-fieldname = 'BSTMA'.
  gt_fields-position  = 4.
  gt_fields-intlen    = 13.
  gt_fields-outputlen = 13.
  gt_fields-reptext = 'Upper'.
  APPEND gt_fields.CLEAR gt_fields.

  gt_fields-fieldname = 'ADATU'.
  gt_fields-position  = 5.
  gt_fields-intlen    = 8.
  gt_fields-outputlen = 8.
  gt_fields-reptext = 'Valid-from'.
  APPEND gt_fields.CLEAR gt_fields.

  gt_fields-fieldname = 'BDATM'.
  gt_fields-position  = 6.
  gt_fields-intlen    = 8.
  gt_fields-outputlen = 8.
  gt_fields-reptext = 'Valid-to'.

  APPEND gt_fields.CLEAR gt_fields.
* END

ENDFORM.                    " get_verid
*&---------------------------------------------------------------------*
*&      Form  get_date
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_date.
  CLEAR: gv_cstdt, gv_valdt.

  CONCATENATE p_year p_poper+1(2) '01' INTO gv_cstdt.

  IF p_valdt IS INITIAL.
    IF p_day1 = 'X'.
      CONCATENATE p_year p_poper+1(2) '01' INTO gv_valdt.
    ELSEIF p_day15 = 'X'.
      CONCATENATE p_year p_poper+1(2) '15' INTO gv_valdt.
    ELSEIF p_lday = 'X'.
      CALL FUNCTION 'LAST_DAY_IN_PERIOD_GET'
           EXPORTING
                i_gjahr = p_year
                i_periv = 'K0'
                i_poper = p_poper
           IMPORTING
                e_date  = gv_valdt.
    ENDIF.
  ELSE.
    gv_valdt = p_valdt.      " Valuation date
  ENDIF.

  IF p_aldat IS INITIAL.
    gv_aldat = gv_valdt.     " Same with val.date
  ELSE.
    gv_aldat = p_aldat.      " Qty structure date
  ENDIF.

ENDFORM.                    " GET_DATE
*&---------------------------------------------------------------------*
*&      Form  convert_date
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GV_CSTDT  text
*      <--P_L_CSTDT  text
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
*&      Form  MAKE_MSG_STRING
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
*&      Form  fill_from_sl
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_from_sl.

  DATA : BEGIN OF it_likp OCCURS 0,
*          vbeln like likp-vbeln,
*          vkorg like likp-vkorg,
          werks LIKE likp-werks,
          matnr LIKE lips-matnr,
*          kunag like likp-kunag,
          lfimg LIKE lips-lfimg,
*          wadat_ist like likp-wadat_ist,
          atwrt LIKE ausp-atwrt,
          maktg TYPE maktg,
         END OF it_likp.

  DATA : l_fdate TYPE datum,
         l_tdate TYPE datum.

  PERFORM get_last_date CHANGING l_fdate l_tdate.
  DATA l_atinn TYPE atinn.


  SELECT SINGLE atinn INTO l_atinn FROM cabn WHERE
  atnam EQ 'P_VERSION'.

  CHECK sy-subrc EQ 0.
  SELECT b~werks b~matnr c~atwrt d~maktg
*  a~vbeln a~kunag a~vkorg
  SUM( b~lfimg ) AS lfimg
*  a~wadat_ist
   INTO CORRESPONDING FIELDS OF TABLE it_likp
   FROM likp AS a INNER JOIN lips AS b
    ON a~vbeln = b~vbeln
    INNER JOIN ausp AS c
    ON c~objek = a~vbeln
    AND c~atinn = l_atinn
    INNER JOIN makt AS d
    ON d~matnr = b~matnr
    AND d~spras = sy-langu
    WHERE a~wadat_ist BETWEEN l_fdate AND l_tdate
      AND a~lfart = 'ZVLF'   "Vehicle Delivery
      AND a~fkarv <> space
      AND b~matnr IN s_matnr
      AND b~kokrs = p_kokrs
      GROUP by b~werks b~matnr c~atwrt d~maktg.


  LOOP AT it_likp.

    CLEAR it_row_tab.
    it_row_tab-kokrs = p_kokrs.
    it_row_tab-bdatj = p_year.
    it_row_tab-poper = p_poper.
    it_row_tab-matnr = it_likp-matnr.
    it_row_tab-werks = it_likp-werks.

    it_row_tab-verid = it_likp-atwrt+1(2).
    it_row_tab-maktx = it_likp-maktg.
    it_row_tab-source = 'SL'.
    it_row_tab-lfimg = it_likp-lfimg.

    APPEND it_row_tab.

  ENDLOOP.


ENDFORM.                    " fill_from_sl
*&---------------------------------------------------------------------*
*&      Form  get_last_date
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_L_FDATE  text
*      <--P_L_TDATE  text
*----------------------------------------------------------------------*
FORM get_last_date CHANGING p_fdate p_tdate .

  DATA : l_temp_date(8) TYPE n,
         l_yearmn(6) TYPE n,
         l_lastday(2) TYPE n.

  CONCATENATE p_year  p_poper+1(2) INTO l_yearmn.
  CONCATENATE l_yearmn '01' INTO l_temp_date.
  p_fdate = l_temp_date.

  CLEAR : l_temp_date.
  CALL FUNCTION 'RE_LAST_DAY_OF_MONTH'
       EXPORTING
            i_datum = p_fdate
       IMPORTING
            e_tt    = l_lastday.

  CONCATENATE l_yearmn l_lastday INTO l_temp_date.
  p_tdate = l_temp_date.

ENDFORM.                    " get_last_date

*----------------------------------------------------------------------*
*   create batchinput session                                          *
*   (not for call transaction using...)                                *
*----------------------------------------------------------------------*
FORM open_group.
  IF session = 'X'.
*    SKIP.
*    WRITE: /(20) 'Create group'(i01), group.
*    SKIP.
**   open batchinput group
    CALL FUNCTION 'BDC_OPEN_GROUP'
         EXPORTING
              client   = sy-mandt
              group    = group
              user     = user
              keep     = keep
              holddate = holddate.
*    WRITE: /(30) 'BDC_OPEN_GROUP'(i02),
*            (12) 'returncode:'(i05),
*                 sy-subrc.
  ENDIF.
ENDFORM.

*----------------------------------------------------------------------*
*   end batchinput session                                             *
*   (call transaction using...: error session)                         *
*----------------------------------------------------------------------*
FORM close_group.
  IF session = 'X'.
*   close batchinput group
    CALL FUNCTION 'BDC_CLOSE_GROUP'.
    WRITE: /(30) 'Close session',
            (12) 'Return code =',
                 sy-subrc.
  ELSE.
    IF e_group_opened = 'X'.
      CALL FUNCTION 'BDC_CLOSE_GROUP'.
      WRITE: /.
      WRITE: /(30) 'Error session created'.
    ENDIF.
  ENDIF.
ENDFORM.

*----------------------------------------------------------------------*
*        Start new transaction according to parameters                 *
*----------------------------------------------------------------------*
FORM bdc_transaction USING tcode.
  DATA: l_mstring(480).
  DATA: l_subrc LIKE sy-subrc.
* batch input session
  IF session = 'X'.
    CALL FUNCTION 'BDC_INSERT'
         EXPORTING
              tcode     = tcode
         TABLES
              dynprotab = bdcdata.
    IF smalllog <> 'X'.
      WRITE: / 'Insert transaction',
               tcode,
               'Return code =',
               sy-subrc,
               'RECORD:',
               sy-index.
    ENDIF.
* call transaction using
  ELSE.
    REFRESH messtab.
    CALL TRANSACTION tcode USING bdcdata
                     MODE   ctumode
                     UPDATE cupdate
                     MESSAGES INTO messtab.
    l_subrc = sy-subrc.
    IF smalllog <> 'X'.
    ELSE.
      FORMAT COLOR COL_KEY.
      WRITE: / 'Return code =',
               l_subrc,
               'RECORD:',
               sy-index.
      FORMAT COLOR COL_NORMAL.
      LOOP AT messtab.
        REPLACE '&' WITH messtab-msgv1 INTO l_mstring.
        REPLACE '&' WITH messtab-msgv2 INTO l_mstring.
        REPLACE '&' WITH messtab-msgv3 INTO l_mstring.
        REPLACE '&' WITH messtab-msgv4 INTO l_mstring.
        CONDENSE l_mstring.
        WRITE: / messtab-msgtyp, l_mstring(150).
      ENDLOOP.

    ENDIF.
*********************************************************
** Erzeugen fehlermappe ************************************************
    IF l_subrc <> 0 AND group <> space.
      IF e_group_opened = ' '.
        CALL FUNCTION 'BDC_OPEN_GROUP'
             EXPORTING
                  client   = sy-mandt
                  group    = group
                  user     = user
                  keep     = keep
                  holddate = holddate.
        e_group_opened = 'X'.
      ENDIF.
      CALL FUNCTION 'BDC_INSERT'
           EXPORTING
                tcode        = tcode
           TABLES
                bdc_fieldtab = bdcdata.
    ENDIF.
  ENDIF.
  REFRESH bdcdata.
ENDFORM.

*----------------------------------------------------------------------*
*        Insert field                                                  *
*----------------------------------------------------------------------*
FORM bdc_field  USING fbegin fnam fval.
  CLEAR bdcdata.

  IF fbegin = 'X'.
    MOVE : fnam  TO bdcdata-program,
           fval  TO bdcdata-dynpro,
           'X'   TO bdcdata-dynbegin.
  ELSE.
    bdcdata-fnam = fnam.
    bdcdata-fval = fval.
  ENDIF.
  APPEND bdcdata.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  save_table_2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM save_table_2.
  DATA: lv_cnt(5),
        lv_dcnt(5),
        lv_msg(200).                 " Message

  DATA: l_cstdt(10),        " Costing Run Date
        l_valdt(10),        " Costing Run Date
        l_aldat(10).        " Costing Run Date


* Save seleted data to table ztcou100
  CLEAR: lv_cnt.

  PERFORM get_selected_rows TABLES $gt_out.

  DATA  : i_ztcou100 LIKE ztcou100 OCCURS 0 WITH HEADER LINE,
          ls_ztcou100 LIKE ztcou100.

  DATA: msg LIKE cfgnl-msglin.

  CLEAR : l_cstdt, l_valdt .

  PERFORM convert_date USING: gv_cstdt CHANGING l_cstdt,
                              gv_valdt CHANGING l_valdt,
                              gv_aldat CHANGING l_aldat.

  DATA $ix LIKE sy-tabix.

  READ TABLE $gt_out INDEX 1.

  IF sy-subrc EQ 0.
    PERFORM open_group.
  ELSE.
    EXIT.
  ENDIF.

  LOOP AT $gt_out.

    __cls bdcdata.

    IF $gt_out-idx IS INITIAL.
      $ix = sy-tabix.
    ELSE.
      $ix = $gt_out-idx.
    ENDIF.

    REFRESH: gt_bdc, gt_msg.
    CLEAR  : gt_bdc, gt_msg.


* Create Cost Estimates for Material

    PERFORM bdc_field USING:
       'X'  'SAPLCKDI'        '0111',
       ' '  'CKI64A-KLVAR'    p_klvar,
       ' '  'CKI64A-MATNR'    $gt_out-matnr,     " Material
       ' '  'CKI64A-WERKS'    $gt_out-werks.     " Plant

    IF $gt_out-verid <> space.
      PERFORM bdc_field USING:
       ' '  'CKI64A-FVIDK'    $gt_out-verid.

    ELSE.
    ENDIF.

    PERFORM bdc_field USING:
       ' '  'BDC_OKCODE'      '/00',            " [Enter]
       'X'  'SAPLCKDI'        '0400',
       ' '  'CKI64A-KADAT'    l_valdt,          " Costing date from
       ' '  'CKI64A-BIDAT'    l_valdt,          " Costing date to
       ' '  'CKI64A-ALDAT'    l_aldat,          " Qty structure date
       ' '  'CKI64A-BWDAT'    l_valdt,          " Valuation date
       ' '  'BDC_OKCODE'       '=ENTR'.         " [Enter]


    PERFORM bdc_field USING:
                             'X'  'SAPLCKDI'        '2100',
                             ' '  'BDC_OKCODE'      '=USR1',

                             'X'  'SAPLSPO1'        '0500',
                             ' '  'BDC_OKCODE'      '=OPT1',

*                             'X'  'SAPLCKDI'        '2100',
*                             ' '  'BDC_OKCODE'      '/EBACK',

                             'X'  'SAPLCKDI'        '2100',
                             ' '  'BDC_OKCODE'      '/EENDE'.


*                             'X'  'SAPLSPO1'        '0100',
*                             ' '  'BDC_OKCODE'      '=OPT2'.


    PERFORM bdc_transaction USING 'CK11'.

    CLEAR msg.
*    IF sy-msgno EQ '419'.
    msg = 'Job was created!'.
    CLEAR gt_out-err.
    gt_out-err = 'N'.
*    ELSE.
*      PERFORM make_msg_string USING msg.
*      gt_out-err = true.
*    ENDIF.
    gt_out-msg = msg.
    MODIFY gt_out INDEX $ix TRANSPORTING err msg.

*    lv_cnt = lv_cnt + 1.

  ENDLOOP.

*  IF lv_dcnt > 0 OR lv_cnt > 0.
*    CONCATENATE 'Data''s been processed;'
*                 lv_cnt  'rec(s).'
*            INTO lv_msg SEPARATED BY space.
*    MESSAGE s000 WITH lv_msg.
*  ENDIF.

  CLEAR flag_data_changed.

  PERFORM close_group.

*  PERFORM  apply_icon.


ENDFORM.                    " save_table_2
*&---------------------------------------------------------------------*
*&      Form  pre_crun
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM pre_crun.

  DATA: lv_cnt(5),
        lv_dcnt(5),
        lv_msg(200).                 " Message

  DATA: l_cstdt(10),        " Costing Run Date
        l_valdt(10),        " Costing Run Date
        l_aldat(10).        " Costing Run Date


* Save seleted data to table ztcou100
  CLEAR: lv_cnt.

  PERFORM get_selected_rows TABLES $gt_out.

  DATA  : i_ztcou100 LIKE ztcou100 OCCURS 0 WITH HEADER LINE,
          ls_ztcou100 LIKE ztcou100.

  DATA: msg LIKE cfgnl-msglin.

  CLEAR : l_cstdt, l_valdt .

  PERFORM convert_date USING: gv_cstdt CHANGING l_cstdt,
                              gv_valdt CHANGING l_valdt,
                              gv_aldat CHANGING l_aldat.

* Create Cost Estimates for Material

  PERFORM dynpro USING: 'X'  'SAPLCKCC01'      '2000',
                        ' '  'BDC_OKCODE'      '=CREATE',

                        'X'  'SAPLCKCC01'      '2000',
                        ' '  'BDC_OKCODE'      '=ENTR',
                        ' '  'KALA-KALAID'      p_klvar,
                        ' '  'KALA-KALABEZ'    'ANDY',
                        ' '  'KALA-KALADAT'     l_cstdt,

                        ' '  'KALV-KLVAR'       p_klvar,
                        ' '  'KALA-TVERS'       '01',
                        ' '  'KALA-KOKRS'       'H201',
                        ' '  'KALA-BUKRS'       'H201',
                        ' '  'KALA-UEBID'       'ZPC1',
                        ' '  'KALA-RFC_GROUP'   'PG_FI',

                        'X'  'SAPLCKCC01'       '2000',
                        ' '  'BDC_OKCODE'       '=BUCA',
                        ' '  'KALA-KADAT'        l_cstdt,
                        ' '  'KALA-BIDAT'        l_cstdt,
                        ' '  'KALA-ALDAT'        l_aldat,
                        ' '  'KALA-BWDAT'        l_valdt.

  CALL TRANSACTION 'CK40N'  USING         gt_bdc
                            OPTIONS FROM  gs_opt
                            MESSAGES INTO gt_msg.

  DATA $ix LIKE sy-tabix.
  LOOP AT $gt_out.

    IF $gt_out-idx IS INITIAL.
      $ix = sy-tabix.
    ELSE.
      $ix = $gt_out-idx.
    ENDIF.

    READ TABLE gt_msg WITH KEY msgtyp = 'E'.
    IF sy-subrc NE 0.
      msg = 'success'.
      CLEAR gt_out-err2.
      gt_out-err2 = 'N'.
    ELSE.
      PERFORM make_msg_string USING msg.
      gt_out-err2 = true.
    ENDIF.
    gt_out-msg2 = msg.
    MODIFY gt_out INDEX $ix TRANSPORTING err2 msg2.

    lv_cnt = lv_cnt + 1.

  ENDLOOP.

  IF lv_dcnt > 0 OR lv_cnt > 0.
    CONCATENATE 'Data''s been processed;'
                 lv_cnt  'rec(s).'
            INTO lv_msg SEPARATED BY space.
    MESSAGE s000 WITH lv_msg.
  ENDIF.

  IF gt_out-err2 EQ 'N'.

* create variants
    DATA r_c TYPE i.
    PERFORM create_run_variant TABLES $gt_out
                               USING gv_cstdt
                               CHANGING r_c.

  ENDIF.

  CLEAR flag_data_changed.

  PERFORM  apply_icon_2 USING r_c.


ENDFORM.                    " pre_crun
*&---------------------------------------------------------------------*
*&      Form  init_ck40
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM init_ck40.
  DATA lv_answer.

  CLEAR lv_answer.
  CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
       EXPORTING
            textline1      = 'Do you really want to delete '
            textline2      = 'the Costing Data(CK40)?'
            titel          = 'Check!'
            cancel_display = 'X'
       IMPORTING
            answer         = lv_answer.

  CHECK lv_answer = 'J'.

  SELECT COUNT( * ) INTO sy-dbcnt FROM kala
     WHERE kalaid  = p_klvar
       AND kaladat = gv_cstdt.

  IF sy-subrc = 0.
    SUBMIT saprck44 WITH kalaid  = p_klvar
                    WITH kaladat = gv_cstdt
                    WITH backgr  = 'X'
                AND RETURN.
  ELSE.
    MESSAGE s000 WITH 'Could not find Costing Run data.'.
  ENDIF.

ENDFORM.                                                    " init_ck40
*&---------------------------------------------------------------------*
*&      Form  apply_icon_2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM apply_icon_2 USING p_r_c.
  DATA $ix LIKE sy-tabix.

  LOOP AT gt_out.
    $ix = sy-tabix.

    CASE gt_out-err2.
      WHEN 'X'.
        gt_out-icon2 = icon_led_red.
      WHEN 'N'.
        gt_out-icon2 = icon_led_green.
      WHEN OTHERS.
        CLEAR gt_out-icon2.
    ENDCASE.

    IF gt_out-icon2 EQ icon_led_green AND p_r_c EQ 4.
      gt_out-icon2 = icon_led_yellow.
      gt_out-msg2 = 'Could not create the Cst.Variants.'.
      MODIFY gt_out INDEX $ix TRANSPORTING icon2 msg2.
    ELSE.
      MODIFY gt_out INDEX $ix TRANSPORTING icon2.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " apply_icon_2
*&---------------------------------------------------------------------*
*&      Form  create_run_variant
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_R_C  text
*----------------------------------------------------------------------*
FORM create_run_variant   TABLES $gt_out STRUCTURE gt_out
                          USING  p_date
                          CHANGING p_r_c.

  DATA:      x_date       TYPE sy-datum,
             x_intdate    TYPE i,
             x_variant    TYPE variant.
  DATA g_report TYPE rsvar-report.
  DATA       p_kaladat   TYPE kala-kaladat.

  DATA: BEGIN OF lt_mat OCCURS 0,
          matnr TYPE matnr,
        END   OF lt_mat.

  CHECK NOT $gt_out[] IS INITIAL.
  CLEAR p_r_c.

  p_kaladat = p_date.
  x_date = p_kaladat.
*  Convert Date of Costingrun to a internal number

  x_intdate         = x_date.
  x_variant+8(6)    = x_intdate.
  x_variant(4)      = p_klvar.

  SELECT  * FROM  kala
         WHERE  kalaid    = p_klvar
         AND    kaladat   = p_kaladat.
  ENDSELECT.

  IF sy-subrc EQ 0.
  ELSE.
    MESSAGE s000 WITH 'No Name of costing run!' .
    p_r_c = 4.
    EXIT.
  ENDIF.

  __cls gt_rsparams.
  __add_sel_tab 'BACKGR'   'P' 'X'.

  LOOP AT $gt_out.
    lt_mat-matnr = $gt_out-matnr.
    APPEND lt_mat.
  ENDLOOP.

  SORT lt_mat.
  DELETE ADJACENT DUPLICATES FROM lt_mat
      COMPARING matnr .


  LOOP AT lt_mat.
    __add_sel_tab_mat 'CK_MATNR' 'S' 'I' 'EQ' lt_mat-matnr.
  ENDLOOP.

  __add_sel_tab 'CONFMAT'  'P' confmat.
  __add_sel_tab 'NOUEB'    'P' noueb.
  __add_sel_tab 'KALADAT'  'P' p_kaladat.
  __add_sel_tab 'KALAID'   'P' p_klvar.
  __add_sel_tab 'PROTDR'   'P' 'X'.
  __add_sel_tab 'P_COCKP'  'P' 'X'.

  PERFORM create_var_by_ig USING 'SAPRCK60' x_variant
                           CHANGING p_r_c.

  __cls gt_rsparams.
  __add_sel_tab 'BACKGR'   'P' 'X'.
  __add_sel_tab 'KALADAT'  'P' p_kaladat.
  __add_sel_tab 'KALAID'   'P' p_klvar.
  __add_sel_tab 'PROTDR'   'P' 'X'.
  __add_sel_tab 'P_COCKP'  'P' 'X'.

  PERFORM create_var_by_ig USING 'SAPRCK62' x_variant
                           CHANGING p_r_c.

  __cls gt_rsparams.
  __add_sel_tab 'BACKGR'   'P' 'X'.
  __add_sel_tab 'KALADAT'  'P' p_kaladat.
  __add_sel_tab 'KALAID'   'P' p_klvar.
  __add_sel_tab 'LLC_LOG'  'P' 'X'.
  __add_sel_tab 'ONLYKF'   'P' 'X'.
  __add_sel_tab 'PARALLEL' 'P' 'X'.
  __add_sel_tab 'PROTDR'   'P' 'X'.
  __add_sel_tab 'P_COCKP'  'P' 'X'.
  __add_sel_tab 'SERVNUM'  'P' '07'.

  PERFORM create_var_by_ig USING 'SAPRCK10' x_variant
                           CHANGING p_r_c.

ENDFORM.                    " create_run_variant
*&---------------------------------------------------------------------*
*&      Form  create_var_by_ig
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_5678   text
*      -->P_X_VARIANT  text
*----------------------------------------------------------------------*
FORM create_var_by_ig USING    p_report
                               p_variant
                      CHANGING p_r_c .

  DATA p_rc LIKE sy-subrc.

  CALL FUNCTION 'RS_VARIANT_EXISTS'
       EXPORTING
            report              = p_report
            variant             = p_variant
       IMPORTING
            r_c                 = p_rc
       EXCEPTIONS
            not_authorized      = 1
            no_report           = 2
            report_not_existent = 3
            report_not_supplied = 4
            OTHERS              = 5.

  IF p_rc EQ 0.

    CALL FUNCTION 'RS_VARIANT_DELETE'
         EXPORTING
              report               = p_report
              variant              = p_variant
              flag_confirmscreen   = 'N'
              flag_delallclient    = 'X'
         EXCEPTIONS
              not_authorized       = 1
              not_executed         = 2
              no_report            = 3
              report_not_existent  = 4
              report_not_supplied  = 5
              variant_locked       = 6
              variant_not_existent = 7
              no_corr_insert       = 8
              variant_protected    = 9
              OTHERS               = 10.
    IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ELSE.
*      MESSAGE s000 WITH p_report p_variant 'was deleted'.
    ENDIF.

  ENDIF.

  DATA: BEGIN OF lt_varit OCCURS 0.
          INCLUDE STRUCTURE varit.
  DATA: END OF lt_varit.
  DATA: ld_variant LIKE rsvar-variant.
  DATA: ld_repid LIKE sy-repid.
  DATA: ls_varid LIKE varid.

  ld_repid = p_report.
  ld_variant = p_variant.

*...fill table with variant texts......................................
*
  lt_varit-langu   = sy-langu.
  lt_varit-report  = p_report .
  lt_varit-variant = p_variant.
  lt_varit-vtext   = 'Generated variant (do not change manually!)'.
  APPEND lt_varit.

*...fill structure ls_varid (variant attributes).......................
*
  ls_varid-report    = p_report.
  ls_varid-variant   = p_variant.
  ls_varid-protected = space.
  ls_varid-ename     = sy-uname.
  ls_varid-edat      = sy-datum.
  ls_varid-etime     = sy-uzeit.

*...store variant......................................................
*
  CALL FUNCTION 'RS_CREATE_VARIANT'
       EXPORTING
            curr_report   = p_report
            curr_variant  = p_variant
            vari_desc     = ls_varid
       TABLES
            vari_contents = gt_rsparams
            vari_text     = lt_varit
       EXCEPTIONS
            OTHERS        = 1.

  IF sy-subrc <> 0.
    p_r_c = 4.
  ENDIF.

  CALL FUNCTION 'RS_VARIANT_EXISTS'
       EXPORTING
            report              = p_report
            variant             = p_variant
       IMPORTING
            r_c                 = p_rc
       EXCEPTIONS
            not_authorized      = 1
            no_report           = 2
            report_not_existent = 3
            report_not_supplied = 4
            OTHERS              = 5.

  IF p_rc EQ 0.
    MESSAGE s000 WITH p_report p_variant 'was created successfully.'.
  ELSE.
    MESSAGE s000 WITH 'Error was occurred when create variant.'.
    p_r_c = 4.
  ENDIF.

ENDFORM.                    " create_var_by_ig
*&---------------------------------------------------------------------*
*&      Form  fill_from_xl
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_from_xl.

  DATA : BEGIN OF it_ckmlmv003 OCCURS 0,
           bwkey      LIKE ckmlmv001-bwkey,
           matnr      LIKE ckmlmv001-matnr,
           aufnr      LIKE ckmlmv013-aufnr,
           verid_nd   LIKE ckmlmv001-verid_nd,
           meinh      LIKE ckmlmv003-meinh,
           out_menge  LIKE ckmlmv003-out_menge,
           maktg TYPE maktg,
           mtart TYPE mtart,
         END OF  it_ckmlmv003.

  DATA : BEGIN OF it_proc_gr OCCURS 0,
          werks LIKE ztco_shop_sum-bwkey,
          artnr     LIKE ztco_shop_sum-artnr,
        END OF it_proc_gr.

  DATA : it_ckmlmv003_temp LIKE it_ckmlmv003 OCCURS 0 WITH HEADER LINE.

  SELECT  b~bwkey b~matnr b~verid_nd
          c~aufnr
          a~misch_verh AS out_menge a~meins_pc AS meinh
          d~maktg e~mtart
    INTO CORRESPONDING FIELDS OF TABLE it_ckmlmv003
    FROM
    ( ( ckmlmv003 AS a
    INNER JOIN ckmlmv001 AS b
       ON b~kalnr = a~kalnr_bal )
    INNER JOIN ckmlmv013 AS c
       ON c~kalnr_proc = a~kalnr_in )
    INNER JOIN makt AS d
       ON d~matnr = b~matnr
       AND d~spras = sy-langu
    INNER JOIN mara AS e
       ON e~matnr = d~matnr
   WHERE a~mgtyp EQ p_mgtyp
     AND a~gjahr EQ p_year
     AND a~perio EQ p_poper
     AND a~matnr IN s_matnr
     AND b~btyp  =  'BF'
     AND b~bwkey = a~werks
     AND c~flg_wbwg = 'X'
     AND c~autyp = '05' .

  SORT it_ckmlmv003 BY matnr.
*
  DATA: lv_stdpd     TYPE stdpd,          " Confi.Material
        lv_beskz     TYPE beskz,          " Procurement type
        lv_verid     TYPE verid,          " Production version
        lv_stlal     TYPE stalt,          " Alternative BOM
        lv_stlan     TYPE stlan,
        lv_cuobj     TYPE cuobm,          " Object
        lv_maktx     TYPE maktx,          " Material Description
        lv_werks     TYPE werks_d,        " Plant
        lt_values TYPE TABLE OF bapi_char_values WITH HEADER LINE,
        lw_msg       TYPE symsgv.

  LOOP AT it_ckmlmv003.

    IF p_sl = 'X'.
      CHECK it_ckmlmv003-mtart NE 'FERT'.
    ENDIF.

    CLEAR it_row_tab.
    it_row_tab-kokrs = p_kokrs.
    it_row_tab-bdatj = p_year.
    it_row_tab-poper = p_poper.
    it_row_tab-matnr = it_ckmlmv003-matnr.
    it_row_tab-werks = it_ckmlmv003-bwkey.

    it_row_tab-verid = it_ckmlmv003-verid_nd.
    it_row_tab-maktx = it_ckmlmv003-maktg.
    it_row_tab-source = 'XL'.
    it_row_tab-lfimg  =  it_ckmlmv003-out_menge .
    APPEND it_row_tab.

  ENDLOOP.


ENDFORM.                    " fill_from_xl
