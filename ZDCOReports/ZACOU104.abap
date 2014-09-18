*----------------------------------------------------------------------
* Program ID        : ZACOU104 - V2
* Title             : [CO] Register Analysis Codes - Version 2
* Created on        : Mar.12. 2008
* Created by        : I.G.MOON
* Specifications By : Andy Choi
* Description       : Register Analysis Codes
*----------------------------------------------------------------------
REPORT zacou104 MESSAGE-ID zmco.
TABLES : ztcou104, *ztcou104, zscou140, keko, mvke ,tka01, a005 ,
         ztcou103, ztcou100, mara, *ztcou104lock.


INCLUDE zacoui00.
TABLES : marc,sscrfields, *mara ,a004.
INCLUDE <icon>.                        " icon

DATA fname0(30).
DATA fname1(30).
DATA fname2(30).
DATA fname3(30).
FIELD-SYMBOLS : <f_lock>,<f_to>,<f_from>.

*--------------------------------------------------------------------*
* SELECTION-SCREEN
*--------------------------------------------------------------------*
*************************** Selections *********************************
SELECTION-SCREEN BEGIN OF BLOCK bl1 WITH FRAME TITLE text-001.
PARAMETERS: p_kokrs LIKE ztcou104-kokrs OBLIGATORY MEMORY ID cac
                                        MATCHCODE OBJECT fc_kokrs,
            p_year  LIKE ztcou104-bdatj OBLIGATORY MEMORY ID bdtj,
            p_kalka LIKE ztcou104-kalka OBLIGATORY MEMORY ID kka
                                        DEFAULT 'U1',
            p_poper LIKE keko-poper OBLIGATORY MEMORY ID popr.

SELECT-OPTIONS s_id FOR zscou140-matnr MATCHCODE OBJECT mat.
SELECTION-SCREEN END OF BLOCK bl1.

SELECTION-SCREEN BEGIN OF BLOCK b5 WITH FRAME TITLE text-01f.
SELECTION-SCREEN PUSHBUTTON  1(30) fsc12 USER-COMMAND fsc12.
SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN PUSHBUTTON  1(30) flock USER-COMMAND flock.
SELECTION-SCREEN END OF BLOCK b5.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.
PARAMETER p_tr AS CHECKBOX. "DEFAULT 'X'.
SELECT-OPTIONS: s_matnr FOR keko-matnr,
                s_mtart FOR mara-mtart DEFAULT 'FERT',
                s_werks FOR keko-werks DEFAULT 'P001'.
SELECTION-SCREEN END OF BLOCK b2.

* Layout
SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME TITLE text-01s.
PARAMETER p_vari TYPE slis_vari.
SELECTION-SCREEN END OF BLOCK b4.

SELECTION-SCREEN FUNCTION KEY : 5.

*----------------------------------------------------------------------*
* Macros
*----------------------------------------------------------------------*

DEFINE __cls.                          " clear & refresh
  clear &1.refresh &1.
END-OF-DEFINITION.

DEFINE __get_cell_value.

  call method pr_data_changed->get_cell_value
        exporting
               i_row_id    = row_id
               i_fieldname = &1
             importing
               e_value = &2.

  if sy-subrc ne 0.
    message i000(0k) with text-e02.
  endif.

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
      g_ix     LIKE sy-tabix.

DATA : dir.
DATA cursor_f(10).
DATA $sheet(3) TYPE n.
FIELD-SYMBOLS <f>.

DATA : it_tvm3t LIKE tvm3t OCCURS 0 WITH HEADER LINE,
       it_tvm4t LIKE tvm4t OCCURS 0 WITH HEADER LINE,
       it_tvm5t LIKE tvm5t OCCURS 0 WITH HEADER LINE,
       it_t179t LIKE t179t OCCURS 0 WITH HEADER LINE.

TYPES: BEGIN OF ty_ztcou100.
        INCLUDE STRUCTURE ztcou100.
TYPES:  maktx TYPE maktx,
       END OF ty_ztcou100.
TYPES: BEGIN OF ty_row_tab.
        INCLUDE STRUCTURE zscou140.
TYPES  eaufn TYPE am_aufnr.
TYPES: END OF ty_row_tab.

TYPES: BEGIN OF ty_out.
INCLUDE  TYPE ty_row_tab.
TYPES:
     icon TYPE icon-id,
     status(1),
     chk(1),
     err(1),
    $number(13),
* by ig.moon 5/22/2008 {
        lock,
        lock_ico TYPE icon.
* }
TYPES   celltab  TYPE lvc_t_styl.
TYPES   tabcolor TYPE slis_t_specialcol_alv.
TYPES: END OF ty_out.

DATA  : itab       TYPE TABLE OF ty_row_tab WITH HEADER LINE,
        it_row_tab TYPE TABLE OF ty_row_tab WITH HEADER LINE,
        gt_out     TYPE TABLE OF ty_out     WITH HEADER LINE,
        gt_ztfiu128 TYPE TABLE OF ty_row_tab WITH HEADER LINE.

DATA: BEGIN OF it_anlz OCCURS 0,
        bukrs	TYPE bukrs,      " Company Code
        anln1	TYPE anln1,      " Main asset number
        anln2	TYPE anln2,      " Asset sub-number
        kostl	TYPE kostl,
        kostlv TYPE kostlv,
        bdatu TYPE bdatu,      " Date validity ends
      END   OF it_anlz.

DATA: BEGIN OF it_stand OCCURS 0,
        werks	TYPE werks_d,
        stand	TYPE stort_t499s,
      END   OF it_stand.

DATA: BEGIN OF it_t499s OCCURS 0,
        werks	TYPE werks_d,
        stand	TYPE stort_t499s,
        ktext TYPE text40,
      END   OF it_t499s.

DATA: BEGIN OF it_gewrk OCCURS 0,
        objid	TYPE cr_objid,
      END   OF it_gewrk.

DATA: BEGIN OF it_crhd OCCURS 0,
        objid	TYPE objid,
        arbpl TYPE arbpl,
        ktext TYPE cr_ktext,
      END   OF it_crhd.

DATA: BEGIN OF it_equi OCCURS 0,
        equnr	TYPE equnr,
        ansdt TYPE andti,
        answt	TYPE answt,
        elief TYPE elief,
        herst TYPE herst,
        eqktx TYPE ktx01,
      END   OF it_equi.

DATA: BEGIN OF it_iloan OCCURS 0,
        iloan	TYPE iloan,
      END   OF it_iloan.

DATA: BEGIN OF it_equnr OCCURS 0,
        iloan TYPE iloan,
        equnr TYPE equnr,
        gewrk TYPE gewrk,
      END   OF it_equnr.

DATA: BEGIN OF it_eqart OCCURS 0,
        eqart	TYPE eqart,
      END   OF it_eqart.

DATA: BEGIN OF it_all_equi OCCURS 0,
        equnr TYPE equnr,
        eqtyp TYPE eqtyp,
        eqart TYPE eqart,
        ansdt TYPE andti,
        answt TYPE answt,
        waers TYPE waers,
        elief TYPE elief,
        herst TYPE herst,
        baumm TYPE baumm,
        matnr TYPE matnr,
        sernr TYPE gernr,
        werk TYPE werks_d,
        meins TYPE meins,
        gewrk TYPE gewrk,
        swerk TYPE swerk,
        stort	TYPE pmloc,
        iloan TYPE iloan,
        eqktx	TYPE ktx01,
      END   OF it_all_equi.

DATA: BEGIN OF it_anlc OCCURS 0,
        bukrs	TYPE bukrs,      " Company Code
        anln1	TYPE anln1,      " Main asset number
        anln2	TYPE anln2,      " Asset sub-number
        answl	TYPE answl,
      END   OF it_anlc.

DATA : gt_ztcou100 TYPE TABLE OF ty_ztcou100 WITH HEADER LINE,
       $gt_out LIKE gt_out OCCURS 0 WITH HEADER LINE,
       g_kokrs LIKE ztcou104-kokrs,
       g_year LIKE ztcou104-bdatj ,
       g_kalka LIKE ztcou104-kalka,
       gv_ldate    TYPE sydatum,        " last date of month
       gt_ztcou104 TYPE TABLE OF ztcou104 WITH HEADER LINE.

*----------------------------------------------------------------------*
* Define local class
*----------------------------------------------------------------------*
CLASS lcl_event_receiver DEFINITION.
  PUBLIC SECTION.

    TYPES: BEGIN OF zscou140_k,
            prodh LIKE zscou140-prodh,
            model LIKE zscou140-model,
            kunnr LIKE zscou140-kunnr,
            mvgr4 LIKE zscou140-mvgr4,
            mvgr3 LIKE zscou140-mvgr3,
            mvgr5 LIKE zscou140-mvgr5,
            matnr LIKE zscou140-matnr,
           END OF zscou140_k.

    TYPES: zscou140_key   TYPE STANDARD TABLE OF zscou140_k,
           zscou140_table TYPE STANDARD TABLE OF zscou140.

    METHODS:
      handle_data_changed
         FOR EVENT data_changed OF cl_gui_alv_grid
             IMPORTING er_data_changed.

    METHODS:
      get_inserted_rows
           EXPORTING
              inserted_rows TYPE zscou140_key.

    METHODS:
      get_deleted_rows
          EXPORTING
              deleted_rows TYPE zscou140_table.

    METHODS:
       refresh_delta_tables.

    METHODS: set_table_is_initial.

    METHODS: set_table_is_not_initial.

    METHODS: table_is_initial
                RETURNING value(initial) TYPE char01.

    METHODS: chk_screen
               RETURNING value(error) TYPE char01.

  PRIVATE SECTION.
    DATA: inserted_rows TYPE zscou140_key,
          deleted_rows TYPE STANDARD TABLE OF zscou140.
    DATA  error_in_data TYPE c.
    DATA  initial_table TYPE c.

    METHODS:
      values_check
         IMPORTING
            pr_data_changed TYPE REF TO cl_alv_changed_data_protocol.

    METHODS:
      update_delta_tables
         IMPORTING
            pr_data_changed TYPE REF TO cl_alv_changed_data_protocol.

    METHODS:
      get_key_values
           IMPORTING
             row_id          TYPE int4
             pr_data_changed TYPE REF TO cl_alv_changed_data_protocol
           EXPORTING
             key             TYPE zscou140_k.

ENDCLASS.                   " LCL_EVENT_RECEIVER Definition

*----------------------------------------------------------------------*
* Implementation local class
*----------------------------------------------------------------------*
CLASS lcl_event_receiver IMPLEMENTATION.

  METHOD chk_screen.
    READ TABLE gt_out WITH KEY err = true TRANSPORTING NO FIELDS.
    IF sy-subrc EQ 0.
      error = true.
    ELSE.
      error = space.
    ENDIF.
  ENDMETHOD.

* Setting for Change data
  METHOD handle_data_changed.
    error_in_data = space.

* check mt_good_cells semantically
    CALL METHOD values_check( er_data_changed ).

* remember new or deleted lines for saving
    CALL METHOD update_delta_tables( er_data_changed ).

    IF error_in_data = 'X'.
      CALL METHOD er_data_changed->display_protocol.
    ENDIF.

  ENDMETHOD.                    " handle_data_changed

*-------------------------------------------------------
  METHOD update_delta_tables.
  ENDMETHOD.
*---------------------------------------------------------

  METHOD get_key_values.

    __get_cell_value : 'PRODH' key-prodh,
                       'MODEL' key-model,
                       'KUNNR' key-kunnr,
                       'MVGR4' key-mvgr4,
                       'MVGR3' key-mvgr3,
                       'MVGR5' key-mvgr5,
                       'MATNR' key-matnr.

  ENDMETHOD.

*---------------------------------------------------------
  METHOD values_check.

    DATA: lt_good_cells TYPE lvc_t_modi,
          ls_good TYPE lvc_s_modi,
          ls_key TYPE zscou140_k,
          ls_zscou140 TYPE zscou140,
          l_row_id LIKE sy-tabix,
          lt_ins_rows TYPE zscou140_key,
          lt_key TYPE zscou140_k,
          dup_chk(1).

    DATA:  l_fsc LIKE zscou140-fsc,
           l_matnr LIKE zscou140-matnr,
           l_model LIKE  zscou140-model,
           l_kunnr LIKE  zscou140-kunnr,
           l_base_year  LIKE  zscou140-base_year,
           l_base_poper LIKE  zscou140-base_poper.

    DATA ls_out TYPE ty_out.

    LOOP AT pr_data_changed->mt_good_cells INTO ls_good.
      l_row_id = ls_good-row_id.

      CASE ls_good-fieldname.
        WHEN 'MAKTX'.
        WHEN 'FSC' OR 'ZABP_FSC'.

          CALL METHOD get_key_values
               EXPORTING row_id          = l_row_id
                         pr_data_changed = pr_data_changed
               IMPORTING key             = ls_key.

          CALL METHOD pr_data_changed->get_cell_value
                      EXPORTING
                            i_row_id = ls_good-row_id
                            i_fieldname = ls_good-fieldname
                      IMPORTING e_value = l_matnr.


          IF ls_good-fieldname EQ 'ZABP_FSC'.

            ls_out-zabp_fsc = l_matnr.
            PERFORM fill_gt_out USING ls_key ls_out l_matnr.

            ls_out-mvgr5abp = ls_out-mvgr5.
            ls_out-mvgr5abptxt = ls_out-mvgr5txt.

            CALL METHOD pr_data_changed->modify_cell
                   EXPORTING i_row_id    = ls_good-row_id
                             i_fieldname = 'MVGR5ABP'
                             i_value     = ls_out-mvgr5abp.

            CALL METHOD pr_data_changed->modify_cell
                   EXPORTING i_row_id    = ls_good-row_id
                             i_fieldname = 'MVGR5ABPTXT'
                             i_value     = ls_out-mvgr5abptxt.

          ENDIF.

*          IF ls_good-fieldname EQ 'FSC'.
*
*            IF l_matnr NE space.
*              ls_out-fsc = l_matnr.
*              PERFORM fill_gt_out USING ls_key ls_out l_matnr.
*            ELSE.
*              CLEAR ls_out.
*            ENDIF.
*            MOVE-CORRESPONDING ls_out TO ls_key.
*
*            CALL METHOD pr_data_changed->modify_cell
*                   EXPORTING i_row_id    = ls_good-row_id
*                             i_fieldname = 'PRODH'
*                             i_value     = ls_out-prodh.
*
*            CALL METHOD pr_data_changed->modify_cell
*                   EXPORTING i_row_id    = ls_good-row_id
*                             i_fieldname = 'PRODHTXT'
*                             i_value     = ls_out-prodhtxt.
*
*            CALL METHOD pr_data_changed->modify_cell
*                   EXPORTING i_row_id    = ls_good-row_id
*                             i_fieldname = 'MODEL'
*                             i_value     = ls_out-model.
*
*            CALL METHOD pr_data_changed->modify_cell
*                   EXPORTING i_row_id    = ls_good-row_id
*                             i_fieldname = 'KUNNR'
*                             i_value     = ls_out-kunnr.
*
*            CALL METHOD pr_data_changed->modify_cell
*                   EXPORTING i_row_id    = ls_good-row_id
*                             i_fieldname = 'MVGR4'
*                             i_value     = ls_out-mvgr4.
*
*            CALL METHOD pr_data_changed->modify_cell
*                   EXPORTING i_row_id    = ls_good-row_id
*                             i_fieldname = 'MVGR3'
*                             i_value     = ls_out-mvgr3.
*
*            CALL METHOD pr_data_changed->modify_cell
*                   EXPORTING i_row_id    = ls_good-row_id
*                             i_fieldname = 'MVGR5'
*                             i_value     = ls_out-mvgr5.
*
*            CALL METHOD pr_data_changed->modify_cell
*                   EXPORTING i_row_id    = ls_good-row_id
*                             i_fieldname = 'MVGR4TXT'
*                             i_value     = ls_out-mvgr4txt.
*
*            CALL METHOD pr_data_changed->modify_cell
*                   EXPORTING i_row_id    = ls_good-row_id
*                             i_fieldname = 'MVGR3TXT'
*                             i_value     = ls_out-mvgr3txt.
*
*            CALL METHOD pr_data_changed->modify_cell
*                   EXPORTING i_row_id    = ls_good-row_id
*                             i_fieldname = 'MVGR5TXT'
*                             i_value     = ls_out-mvgr5txt.
*
*
*            CONTINUE.
*          ENDIF.

          CALL METHOD pr_data_changed->modify_cell
                 EXPORTING i_row_id    = ls_good-row_id
                           i_fieldname = 'FEVOR'
                           i_value     = ls_out-fevor.

          SELECT SINGLE * FROM mvke INTO mvke WHERE matnr EQ l_matnr .

          IF sy-subrc NE 0 AND l_matnr NE space.
            __set_error text-e01.
          ELSE.
            IF ls_key-prodh NE mvke-prodh.
              __set_error text-e22.
            ENDIF.

            IF ls_out-fevor NE 'SEA'.
              PERFORM get_code USING l_matnr
                            CHANGING l_model
                                     l_kunnr .
            ENDIF.

            CALL METHOD pr_data_changed->modify_cell
                   EXPORTING i_row_id    = ls_good-row_id
                             i_fieldname = 'MTART'
                             i_value     = ls_out-mtart.

            CALL METHOD pr_data_changed->modify_cell
                   EXPORTING i_row_id    = ls_good-row_id
                             i_fieldname = 'WERKS'
                             i_value     = ls_out-werks.

            CALL METHOD pr_data_changed->modify_cell
                   EXPORTING i_row_id    = ls_good-row_id
                             i_fieldname = 'MATKL'
                             i_value     = ls_out-matkl.

            IF ls_out-mtart EQ 'HALB'.
              l_model = ls_out-werks.
            ENDIF.

            IF ls_out-matkl EQ 'A/S'.
              l_kunnr = 'MOBIS'.
            ELSE.

              PERFORM fill_kunnr USING l_matnr
                              CHANGING l_kunnr.

            ENDIF.

            IF ls_key-model NE l_model.
              __set_error text-e23.
            ENDIF.

            IF ls_key-kunnr NE l_kunnr.
              __set_error text-e24.
            ENDIF.

            IF ls_key-mvgr4 NE mvke-mvgr4.
              __set_error text-e25.
            ENDIF.
            IF ls_key-mvgr3 NE mvke-mvgr3.
              __set_error text-e26.
            ENDIF.

*            if ls_key-mvgr5 ne mvke-mvgr5.
*              __set_error text-e27.
*            endif.

          ENDIF.

        WHEN 'ZBASE_FSC' OR 'BASE_YEAR' OR 'BASE_POPER'.
          CALL METHOD pr_data_changed->get_cell_value
                      EXPORTING
                            i_row_id = ls_good-row_id
                            i_fieldname = 'ZBASE_FSC'
                      IMPORTING e_value = l_matnr.
          CALL METHOD pr_data_changed->get_cell_value
                      EXPORTING
                            i_row_id = ls_good-row_id
                            i_fieldname = 'BASE_YEAR'
                      IMPORTING e_value = l_base_year.
          CALL METHOD pr_data_changed->get_cell_value
                      EXPORTING
                            i_row_id = ls_good-row_id
                            i_fieldname = 'BASE_POPER'
                      IMPORTING e_value = l_base_poper.

          IF l_matnr NE space AND
             l_base_year NE space AND
             l_base_poper NE space.

            SELECT SINGLE * FROM ztcou103 INTO ztcou103
            WHERE kokrs EQ g_kokrs
              AND bdatj EQ l_base_year
              AND kalka EQ p_kalka
              AND poper EQ l_base_poper
              AND artnr EQ l_matnr.

            IF sy-subrc NE 0.

              SELECT SINGLE * FROM ztcou100 INTO ztcou100
              WHERE kokrs EQ g_kokrs
                AND kalka EQ p_kalka
                AND bdatj EQ l_base_year
                AND poper EQ l_base_poper
                AND matnr EQ l_matnr.
              IF sy-subrc NE 0.
                DATA : l_etext(120).
                CONCATENATE 'Invalid Roll-Up Info. for' l_matnr
                 'with base period'
                 INTO l_etext SEPARATED BY space.
                CALL METHOD pr_data_changed->add_protocol_entry
                             EXPORTING
                  i_msgid = '0K' i_msgno = '000'  i_msgty = 'E'
                  i_msgv1 = l_etext                         "text-e28
                  i_fieldname = 'ZBASE_FSC'
                  i_row_id = ls_good-row_id.

                error_in_data = 'X'.

              ENDIF.
            ENDIF.

          ENDIF.

        WHEN 'ZCOMAT'.

      ENDCASE.

    ENDLOOP.

  ENDMETHOD.
*------------------------------------------------------
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

ENDCLASS.                   " LCL_EVENT_RECEIVER Implementation

DATA g_event_receiver  TYPE REF TO lcl_event_receiver.

************************************************************************
DATA  : flag_data_changed,
    info(80).

DATA: BEGIN OF ftab OCCURS 10,
fcode(6),
END OF ftab.

DATA g_tr.

*----------------------------------------------------------------------*
* INITIALIZATION
*----------------------------------------------------------------------*
INITIALIZATION.

*  sy-title = '[CO] Register Analysis Codes - V2'.
  PERFORM make_button.

  IF p_vari IS INITIAL.
    PERFORM get_default_variant_f14 USING p_vari.
  ENDIF.

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
    WHEN 'FSC12' OR  'FC05'.
      SUBMIT zacou104_fsc WITH p_kokrs EQ p_kokrs
                          WITH p_year  EQ p_year
                          WITH p_kalka EQ p_kalka
                          WITH s_id IN s_id
                          WITH p_poper EQ p_poper
                          AND RETURN.
    WHEN 'FLOCK'.
      SUBMIT zacou104_lck WITH p_kokrs EQ p_kokrs
                          WITH p_year  EQ p_year
                          WITH p_kalka EQ p_kalka
                          WITH s_id IN s_id
                          WITH p_poper EQ p_poper
                          WITH p_lock EQ true
                          AND RETURN.
  ENDCASE.

*----------------------------------------------------------------------*
START-OF-SELECTION.
*----------------------------------------------------------------------*

  PERFORM initialize.
  CHECK g_error EQ  false.

  IF p_tr EQ true.
    PERFORM transfer_from_mat_ledger.
  ENDIF.

  PERFORM get_row_data.
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

    'PRODHTXT'       ' ' 'X' ' ' 'X' ' ',
    'MODEL'       ' ' 'X' ' ' 'X' ' ',
    'KUNNR'       ' ' 'X' ' ' 'X' ' ',
    'MVGR4TXT'       ' ' 'X' ' ' 'X' ' ',
    'MVGR3TXT'       ' ' 'X' ' ' 'X' ' ',
    'MVGR5TXT'       ' ' 'X' ' ' 'X' ' ',
    'MATNR'       ' ' 'X' ' ' 'X' ' '.

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

  g_kokrs = p_kokrs.

  CLEAR g_tr.
  g_tr = p_tr.

  PERFORM get_text.


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

  DATA locked.

  __cls gt_out.
  LOOP AT it_row_tab.

    MOVE-CORRESPONDING it_row_tab TO gt_out.

    CALL FUNCTION 'Z_CHK_LOCK_STATUS_FSC'
         EXPORTING
              kokrs         = p_kokrs
              bdatj         = p_year
              kalka         = p_kalka
              id            = gt_out-fsc
              poper         = p_poper
         IMPORTING
              locked        = locked
         EXCEPTIONS
              invalid_poper = 1
              OTHERS        = 2.
    IF locked EQ true.
      gt_out-lock     = true.
      gt_out-lock_ico = icon_locked.
    ENDIF.

    APPEND gt_out.
  ENDLOOP.
  PERFORM apply_icon.

ENDFORM.                    " MOVE_OUT_
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

    WHEN 'SWITCH'.
      IF sy-dynnr EQ '0100'.
        PERFORM switch_edit_mode.
      ENDIF.
      __focus g_grid.

    WHEN 'SAVE'.
      CLEAR g_error.

      IF g_event_receiver->chk_screen( ) EQ true.
        MESSAGE i000 WITH text-e03.
        EXIT.
      ENDIF.

      PERFORM really?.
      CHECK g_error NE true.

      PERFORM : save_table,
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
  SET HANDLER : g_event_receiver->handle_data_changed FOR g_grid.

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
                  cl_gui_alv_grid=>mc_fc_graph,
                  cl_gui_alv_grid=>mc_fc_loc_undo,
                  cl_gui_alv_grid=>mc_fc_info,
                  cl_gui_alv_grid=>mc_fc_loc_copy_row,
                  cl_gui_alv_grid=>mc_fc_loc_append_row,
                  cl_gui_alv_grid=>mc_fc_loc_cut,
                  cl_gui_alv_grid=>mc_fc_loc_insert_row,
                  cl_gui_alv_grid=>mc_fc_loc_delete_row,
                  cl_gui_alv_grid=>mc_fc_loc_move_row,
                  cl_gui_alv_grid=>mc_fc_loc_paste_new_row.

ENDFORM.                    " EXCLUDE_FUNCTIONS
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

  __catalog :
    ' '  'PRODHTXT'    'Prd.hr'        20  'char' '',
    ' '  'MODEL'       'Model'          2   'CHAR' '',
    ' '  'KUNNR'       'Cus.#'         10  'char' '',
    ' '  'MVGR4TXT'       'Grp 4'      20   'char' '',
    ' '  'MVGR3TXT'       'Grp 3'      20   'char' '',
    ' '  'MVGR5TXT'       'Grp 5'      20   'char' '',
    ' '  'MATNR'       'ID'            18  'CHAR' '',
    ' '  'MAKTX'       'ID Description'  25  'CHAR' '',
    ' '  'FSC'         'FSC'             18  'CHAR' '',
    ' '  'ZABP_FSC'    'ABP:FSC'         18  'CHAR' '',
    ' '  'MVGR5ABP'    'Grp5 '           10  'CHAR' '',
    ' '  'MVGR5ABPTXT' '(ABP:Grp5)'         20  'CHAR' '',
    ' '  'ZBASE_FSC'   'Base:FSC'        18  'CHAR' '',
    ' '  'BASE_YEAR'   'BaseYr'          4   'NUMC' '',
    ' '  'BASE_POPER'  'Mon'             3   'NUMC' '',
    ' '  'ZCOMAT'      'Man'             1   'CHAR' ''.

  LOOP AT gt_fcat INTO gs_fcat.
    CASE gs_fcat-fieldname.
      WHEN 'PRODH'.
        gs_fcat-no_zero = 'X'.
      WHEN 'ZCOMAT'.
        gs_fcat-checkbox = 'X'.
      WHEN 'MVGR5ABP'.
        gs_fcat-no_out = 'X'.
    ENDCASE.

    IF gs_fcat-fieldname NE 'ICON'.
      gs_fcat-ref_table = 'ZSCOU140'.
      gs_fcat-ref_field = gs_fieldcat-fieldname.
      MODIFY gt_fcat FROM gs_fcat.
    ENDIF.

    IF gs_fcat-fieldname EQ 'ERR'.
      gs_fcat-no_out = 'X'.
      gs_fcat-ref_field = gs_fieldcat-fieldname.
      MODIFY gt_fcat FROM gs_fcat.
    ENDIF.
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

   'PRODHTXT'   '1' 0,
   'MODEL'      '1' 0,
   'KUNNR'      '1' 0,
   'MVGR4TXT'   '1' 0,
   'MVGR3TXT'   '1' 0,
   'MVGR5TXT'   '1' 0,
   'MATNR'      '1' 0,
   'MAKTX'      '2' 0,
   'FSC'        '3' 0,
   'ZABP_FSC'   '4' 0,
   'MVGR5ABP'   '4' 0,
   'ZBASE_FSC'  '5' 0,
   'BASE_YEAR'  '6' 0,
   'BASE_POPER' '6' 0,
   'ZCOMAT'     '7' 0,
   'MVGR5ABPTXT' '1' 0.
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
      APPEND $gt_out.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " get_selected_rows
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
  DATA $idx(2) TYPE n.
  DATA locked.
  DATA $ix TYPE i.

  LOOP AT gt_out.
    $ix = sy-tabix.
    __cls lt_celltab.

    ls_celltab-fieldname = 'PRODHTXT'.
    ls_celltab-style = cl_gui_alv_grid=>mc_style_disabled.
    INSERT ls_celltab INTO TABLE lt_celltab.

    ls_celltab-fieldname = 'MODEL'.
    ls_celltab-style = cl_gui_alv_grid=>mc_style_disabled.
    INSERT ls_celltab INTO TABLE lt_celltab.

    ls_celltab-fieldname = 'KUNNR'.
    ls_celltab-style = cl_gui_alv_grid=>mc_style_disabled.
    INSERT ls_celltab INTO TABLE lt_celltab.

    ls_celltab-fieldname = 'MVGR4TXT'.
    ls_celltab-style = cl_gui_alv_grid=>mc_style_disabled.
    INSERT ls_celltab INTO TABLE lt_celltab.

    ls_celltab-fieldname = 'MVGR3TXT'.
    ls_celltab-style = cl_gui_alv_grid=>mc_style_disabled.
    INSERT ls_celltab INTO TABLE lt_celltab.

    ls_celltab-fieldname = 'MVGR5TXT'.
    ls_celltab-style = cl_gui_alv_grid=>mc_style_disabled.
    INSERT ls_celltab INTO TABLE lt_celltab.

    ls_celltab-fieldname = 'MATNR'.
    ls_celltab-style = cl_gui_alv_grid=>mc_style_disabled.
    INSERT ls_celltab INTO TABLE lt_celltab.

    ls_celltab-fieldname = 'MVGR5ABPTXT'.
    ls_celltab-style = cl_gui_alv_grid=>mc_style_disabled.
    INSERT ls_celltab INTO TABLE lt_celltab.

    CLEAR *ztcou104.
    SELECT SINGLE * INTO *ztcou104
          FROM ztcou104
    WHERE kokrs EQ p_kokrs
      AND bdatj EQ p_year
      AND kalka EQ p_kalka
      AND id    EQ gt_out-fsc.

    IF sy-subrc EQ 0.
      CLEAR *ztcou104lock.
      SELECT SINGLE * INTO *ztcou104lock
            FROM ztcou104lock
      WHERE kokrs EQ p_kokrs
        AND bdatj EQ p_year
        AND kalka EQ p_kalka
        AND id    EQ gt_out-fsc.

      IF sy-subrc EQ 0.
        IF *ztcou104lock-lock00 EQ true.
          ls_celltab-fieldname = 'ZBASE_FSC'.
          ls_celltab-style = cl_gui_alv_grid=>mc_style_disabled.
          INSERT ls_celltab INTO TABLE lt_celltab.
        ENDIF.

        $idx = p_poper.
        CONCATENATE '*ZTCOU104LOCK-LOCK' $idx INTO fname0.
        ASSIGN (fname0) TO <f_from>.
        ASSIGN (fname2) TO <f_to>.

        IF <f_from> EQ true AND <f_to> NE space.
          ls_celltab-fieldname = 'FSC'.
          ls_celltab-style = cl_gui_alv_grid=>mc_style_disabled.
          INSERT ls_celltab INTO TABLE lt_celltab.
        ENDIF.

      ENDIF.
    ENDIF.
    __cls gt_out-celltab.
    INSERT LINES OF lt_celltab INTO TABLE gt_out-celltab.
    MODIFY gt_out INDEX $ix TRANSPORTING celltab.
  ENDLOOP.

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

  CLEAR : gs_layo,gt_exclude,gt_out[],gt_fcat[],gt_sort[].

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
**    IF FLAG_DATA_CHANGED EQ TRUE.
**      CALL FUNCTION 'POPUP_TO_CONFIRM_LOSS_OF_DATA'
**           EXPORTING
**                TEXTLINE1     = 'Data has not been saved yet.'
**                TEXTLINE2     = 'Do you want to continue anyway? '
**                TITEL         = 'Confirmation'
**                DEFAULTOPTION = 'N'
**           IMPORTING
**                ANSWER        = ANSWER.
**      CHECK ANSWER EQ 'J'.
**    ENDIF.
**    CLEAR FLAG_DATA_CHANGED.
    CALL METHOD g_grid->set_ready_for_input
                     EXPORTING i_ready_for_input = 0.
    PERFORM info_text_set USING false.
  ENDIF.

  PERFORM user_status.
  PERFORM build_cell_attr.

ENDFORM.                    " SWITCH_EDIT_MODE
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

*ICON_LED_GREEN
*ICON_LED_RED
*ICON_LED_YELLOW

  LOOP AT gt_out.
    $ix = sy-tabix.
    IF gt_out-status EQ true.
      gt_out-icon = icon_led_green.
    ELSE.
      gt_out-icon = icon_led_yellow.
    ENDIF.
    MODIFY gt_out INDEX $ix TRANSPORTING icon.
  ENDLOOP.

ENDFORM.                    " APPLY_ICON
*&---------------------------------------------------------------------*
*&      Form  MAKE_BUTTON
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM make_button.
  WRITE:
          icon_pm_order AS ICON TO fsc12,
         'View 1~12 FSC' TO fsc12+4(21).

  WRITE:
          icon_locked AS ICON TO flock,
         'Lock Management' TO flock+4(21).

  WRITE:
         'View 1~12 FSC'  TO sscrfields-functxt_05.

ENDFORM.                    " MAKE_BUTTON
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
  IF g_grid->is_ready_for_input( ) EQ 1.
  ELSE.
    ftab-fcode = 'SAVE'.
    APPEND ftab.
  ENDIF.
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
      'Please confirm to again!'
      'Do you really want to save?' ' '
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

* PERFORM get_selected_rows TABLES $gt_out.
  $gt_out[] = gt_out[].

  DATA  : i_ztcou104 LIKE ztcou104 OCCURS 0 WITH HEADER LINE,
          ls_ztcou104 LIKE ztcou104,
          $field(20).

  CALL METHOD g_event_receiver->refresh_delta_tables.

  LOOP AT $gt_out.

* fill old data for other period {
    CLEAR *ztcou104.
    SELECT SINGLE * INTO *ztcou104
    FROM ztcou104
    WHERE kokrs EQ p_kokrs
      AND bdatj EQ p_year
      AND kalka EQ p_kalka
      AND id    EQ $gt_out-matnr.
* }

    MOVE-CORRESPONDING $gt_out TO *ztcou104.

     *ztcou104-kokrs = p_kokrs.
     *ztcou104-bdatj = p_year.
     *ztcou104-kalka = p_kalka.
     *ztcou104-id    = $gt_out-matnr.
     *ztcou104-idtext = $gt_out-maktx.

    CONCATENATE '*ZTCOU104-FSC' p_poper+1(2) INTO $field.
    ASSIGN ($field) TO <f>.
    <f> = $gt_out-fsc.
     *ztcou104-aedat = sy-datum.
     *ztcou104-aenam = sy-uname.
    i_ztcou104 = *ztcou104.
    APPEND i_ztcou104.
    lv_cnt = lv_cnt + 1.

  ENDLOOP.

  READ TABLE i_ztcou104 INDEX 1.
  IF sy-subrc EQ 0.
    MODIFY ztcou104 FROM TABLE i_ztcou104.
    COMMIT WORK.
  ENDIF.

  CONCATENATE 'Data''s been saved;'
               lv_cnt  'rec(s).'
          INTO lv_msg SEPARATED BY space.

  IF lv_cnt > 0.
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
*&      Form  get_row_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_row_data.
  DATA $ix LIKE sy-tabix.

  g_kokrs = p_kokrs.
  g_year  = p_year.
  g_kalka = p_kalka.

  __cls : gt_ztcou104,it_row_tab.

* Get data from table ZTCOU104
  SELECT * INTO TABLE gt_ztcou104
    FROM ztcou104
   WHERE kokrs = p_kokrs
     AND bdatj = p_year
     AND kalka = p_kalka
     AND id IN s_id.

  IF sy-subrc NE 0.
    MESSAGE s000 WITH 'No Data was found.'.
    g_error = true.
    EXIT.
  ENDIF.

  DATA: BEGIN OF $matnr OCCURS 0,
          matnr  LIKE mvke-matnr,
        END OF $matnr.

  DATA: BEGIN OF lt_mvke OCCURS 0,
          matnr  LIKE mvke-matnr,
          prodh  LIKE mvke-prodh,
          mvgr3  LIKE mvke-mvgr3,
          mvgr4  LIKE mvke-mvgr4,
          mvgr5  LIKE mvke-mvgr5,
          werks  LIKE marc-werks,
          fevor  LIKE marc-fevor,
          mtart  LIKE mara-mtart,
          matkl  LIKE mara-matkl,
        END OF lt_mvke.

  DATA: BEGIN OF lt_a005 OCCURS 0,
          matnr   LIKE a005-matnr,
          kunnr   LIKE a005-kunnr,
        END   OF lt_a005.

  DATA $field(20).

  CONCATENATE 'GT_ZTCOU104-FSC' p_poper+1(2) INTO $field.
  ASSIGN ($field) TO <f>.

  LOOP AT gt_ztcou104.
    MOVE-CORRESPONDING gt_ztcou104 TO it_row_tab.
    MOVE <f> TO it_row_tab-fsc.
    it_row_tab-matnr = gt_ztcou104-id.

    SELECT SINGLE maktg INTO it_row_tab-maktx
            FROM makt
            WHERE matnr EQ gt_ztcou104-id
              AND spras EQ sy-langu.

*    it_row_tab-maktx = gt_ztcou104-idtext.

    APPEND it_row_tab.

    $matnr-matnr = it_row_tab-matnr.
*    $matnr-matnr = it_row_tab-fsc.
    APPEND $matnr.
    $matnr-matnr = it_row_tab-zabp_fsc.
    APPEND $matnr.
  ENDLOOP.

  SORT $matnr.
  DELETE ADJACENT DUPLICATES FROM $matnr .

  READ TABLE $matnr INDEX 1.
  IF sy-subrc EQ 0.

    __cls : lt_mvke, lt_a005.

    SELECT a~matnr a~prodh a~mvgr3 a~mvgr4 a~mvgr5 b~werks b~fevor
    c~mtart c~matkl
    INTO TABLE lt_mvke
    FROM mvke AS a
    INNER JOIN marc AS b
    ON b~matnr EQ a~matnr
    INNER JOIN mara AS c
    ON c~matnr EQ b~matnr
    FOR ALL ENTRIES IN $matnr
    WHERE a~matnr EQ $matnr-matnr.

    SELECT matnr kunnr INTO TABLE lt_a005
    FROM a005
    FOR ALL ENTRIES IN $matnr
    WHERE matnr EQ $matnr-matnr.

  ENDIF.

  SORT : lt_mvke BY matnr,
         lt_a005 BY matnr .

  LOOP AT it_row_tab.

    $ix = sy-tabix.
*    read table lt_mvke with key matnr = it_row_tab-matnr binary search.

*    READ TABLE lt_mvke WITH KEY matnr = it_row_tab-fsc BINARY SEARCH.
    READ TABLE lt_mvke WITH KEY matnr = it_row_tab-matnr BINARY SEARCH.
    IF sy-subrc EQ 0.
      it_row_tab-prodh = lt_mvke-prodh.
      it_row_tab-mvgr3 = lt_mvke-mvgr3.
      it_row_tab-mvgr4 = lt_mvke-mvgr4.
      it_row_tab-mvgr5 = lt_mvke-mvgr5.
      it_row_tab-werks = lt_mvke-werks.
      it_row_tab-fevor = lt_mvke-fevor.
      it_row_tab-mtart = lt_mvke-mtart.
      it_row_tab-matkl = lt_mvke-matkl.

      READ TABLE it_tvm3t WITH KEY mvgr3 = it_row_tab-mvgr3
      BINARY SEARCH.
      IF sy-subrc EQ 0.
        it_row_tab-mvgr3txt = it_tvm3t-bezei.
      ENDIF.

      READ TABLE it_tvm4t WITH KEY mvgr4 = it_row_tab-mvgr4
      BINARY SEARCH.
      IF sy-subrc EQ 0.
        it_row_tab-mvgr4txt = it_tvm4t-bezei.
      ENDIF.

      READ TABLE it_tvm5t WITH KEY mvgr5 = it_row_tab-mvgr5
      BINARY SEARCH.
      IF sy-subrc EQ 0.
        it_row_tab-mvgr5txt = it_tvm5t-bezei.
      ENDIF.

      READ TABLE it_t179t WITH KEY prodh = it_row_tab-prodh
      BINARY SEARCH.
      IF sy-subrc EQ 0.
        it_row_tab-prodhtxt = it_t179t-vtext.
      ENDIF.

    ENDIF.

    READ TABLE lt_mvke WITH KEY matnr = it_row_tab-zabp_fsc
    BINARY SEARCH.

    IF sy-subrc EQ 0.
      it_row_tab-mvgr5abp = lt_mvke-mvgr5.
      READ TABLE it_tvm5t WITH KEY mvgr5 = it_row_tab-mvgr5abp
      BINARY SEARCH.
      IF sy-subrc EQ 0.
        it_row_tab-mvgr5abptxt = it_tvm5t-bezei.
      ENDIF.
    ENDIF.

    IF it_row_tab-fevor NE 'SEA'.
      PERFORM get_code USING it_row_tab-matnr "it_row_tab-fsc
                    CHANGING it_row_tab-model
                             it_row_tab-kunnr .
    ENDIF.
    IF it_row_tab-mtart EQ 'HALB'.
      it_row_tab-model = it_row_tab-werks.
    ENDIF.
    IF it_row_tab-matkl EQ 'A/S'.
      it_row_tab-kunnr = 'MOBIS'.
    ELSE.

      READ TABLE lt_a005 WITH KEY
            matnr = it_row_tab-matnr "it_row_tab-fsc
      BINARY SEARCH.
      IF sy-subrc EQ 0.
        it_row_tab-kunnr = lt_a005-kunnr.
      ENDIF.

    ENDIF.

    MODIFY it_row_tab INDEX $ix.

  ENDLOOP.

ENDFORM.                    " get_row_data
*&---------------------------------------------------------------------*
*&      Form  get_code
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_ROW_TAB_ID  text
*      <--P_IT_ROW_TAB_PRODH  text
*      <--P_IT_ROW_TAB_MODEL  text
*      <--P_IT_ROW_TAB_KUNNR  text
*----------------------------------------------------------------------*
FORM get_code USING    p_matnr
              CHANGING p_model
                       p_kunnr.

  IF p_matnr+13(1) EQ space.    " old
    p_model = p_matnr+6(2).
    p_kunnr = p_matnr+1(3).
  ELSE.
    p_model = p_matnr+5(2).     " eBOM
    p_kunnr = p_matnr+1(3).
  ENDIF.

ENDFORM.                    " get_code
*&---------------------------------------------------------------------*
*&      Form  get_text
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_text.

  __cls : it_tvm3t, it_tvm4t, it_tvm5t.

  SELECT * FROM tvm3t INTO TABLE it_tvm3t
                WHERE spras EQ sy-langu.
  SORT it_tvm3t BY mvgr3.

  SELECT * FROM tvm4t INTO TABLE it_tvm4t
                WHERE spras EQ sy-langu.
  SORT it_tvm4t BY mvgr4.

  SELECT * FROM tvm5t INTO TABLE it_tvm5t
                WHERE spras EQ sy-langu.
  SORT it_tvm5t BY mvgr5.

  SELECT * FROM t179t INTO TABLE it_t179t
                WHERE spras EQ sy-langu.
  SORT it_t179t BY prodh.

ENDFORM.                    " get_text
*&---------------------------------------------------------------------*
*&      Form  transfer_from_mat_ledger
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM transfer_from_mat_ledger.

  __cls gt_ztcou100.

  DATA : BEGIN OF it_ckmlmv003 OCCURS 0,
           bwkey      LIKE ckmlmv001-bwkey,
           matnr      LIKE ckmlmv001-matnr,
           aufnr      LIKE ckmlmv013-aufnr,
           verid_nd   LIKE ckmlmv001-verid_nd,
           meinh      LIKE ckmlmv003-meinh,
           out_menge  LIKE ckmlmv003-out_menge,
         END OF  it_ckmlmv003.

  DATA : BEGIN OF it_proc_gr OCCURS 0,
          werks LIKE ztco_shop_sum-bwkey,
          artnr     LIKE ztco_shop_sum-artnr,
        END OF it_proc_gr.

  DATA : it_ckmlmv003_temp LIKE it_ckmlmv003 OCCURS 0 WITH HEADER LINE.

  SELECT  b~bwkey b~matnr b~verid_nd
          c~aufnr
          a~out_menge a~meinh
    INTO CORRESPONDING FIELDS OF TABLE it_ckmlmv003_temp
    FROM
    ( ( ckmlmv003 AS a
    INNER JOIN ckmlmv001 AS b
       ON b~kalnr = a~kalnr_bal )
    INNER JOIN ckmlmv013 AS c
       ON c~kalnr_proc = a~kalnr_in
    INNER JOIN mara AS d
       ON d~matnr = c~pmatn )
   WHERE a~mgtyp EQ '00001'
     AND a~gjahr EQ p_year
     AND a~perio EQ p_poper
     AND a~matnr IN s_matnr
     AND a~werks IN s_werks
     AND d~mtart IN s_mtart
     AND b~btyp  =  'BF'
     AND b~bwkey = a~werks
     AND c~flg_wbwg = 'X'
     AND c~autyp = '05' .

  LOOP AT it_ckmlmv003_temp.
    MOVE-CORRESPONDING it_ckmlmv003_temp TO it_ckmlmv003.
    CLEAR: it_ckmlmv003-verid_nd,
           it_ckmlmv003-aufnr.
    COLLECT it_ckmlmv003. CLEAR it_ckmlmv003.
  ENDLOOP.

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
    SELECT SINGLE * INTO CORRESPONDING FIELDS OF gt_ztcou100
       FROM ztcou100
       WHERE kokrs = p_kokrs
         AND kalka = p_kalka
         AND bdatj = p_year
         AND poper = p_poper
         AND matnr = it_ckmlmv003-matnr.

    CHECK sy-subrc = 0.
    APPEND gt_ztcou100.

  ENDLOOP.

* check entry... stop process
  READ TABLE gt_ztcou100 INDEX 1.
  IF sy-subrc <> 0.
    MESSAGE s000 WITH 'No Data was found.'.
    g_error = true.
    EXIT. "LEAVE PROGRAM.
  ENDIF.

  DATA lt_ztcou104 LIKE ztcou104 OCCURS 0 WITH HEADER LINE.
  DATA $poper LIKE keko-poper.
  DATA $field(20).
  DATA $idx(2) TYPE n.

  LOOP AT gt_ztcou100.

    SELECT SINGLE * FROM ztcou104 WHERE kokrs EQ gt_ztcou100-kokrs
                                   AND  bdatj EQ gt_ztcou100-bdatj
                                   AND  kalka EQ gt_ztcou100-kalka
                                   AND  id    EQ gt_ztcou100-matnr .

*-- ID exist... update period only
    IF sy-subrc EQ 0.

      CLEAR lt_ztcou104.
      MOVE-CORRESPONDING ztcou104 TO lt_ztcou104.

      CLEAR *ztcou104lock.
      SELECT SINGLE * INTO *ztcou104lock
            FROM ztcou104lock
      WHERE kokrs EQ gt_ztcou100-kokrs
        AND bdatj EQ gt_ztcou100-bdatj
        AND kalka EQ gt_ztcou100-kalka
        AND id    EQ gt_ztcou100-matnr.

      IF sy-subrc EQ 0.
        DO 12 TIMES.
          $idx = sy-index.
          CONCATENATE '*ZTCOU104LOCK-LOCK' $idx INTO fname0.
          CONCATENATE 'ZTCOU104-FSC' $idx INTO fname1.
          CONCATENATE 'FSC' $idx INTO fname2.

          ASSIGN (fname0) TO <f_lock>.
          ASSIGN (fname1) TO <f_from>.

          IF <f_lock> EQ true AND <f_from> NE space. "locked
          ELSE.
            CONCATENATE 'LT_ZTCOU104-FSC' p_poper+1(2) INTO $field.
            ASSIGN ($field) TO <f>.
            IF <f> = space.
              <f> = lt_ztcou104-id.

              lt_ztcou104-aedat = sy-datum.
              lt_ztcou104-aenam = sy-uname.
              APPEND lt_ztcou104.
              CLEAR lt_ztcou104.
            ENDIF.
          ENDIF.
        ENDDO.

      ENDIF.

      CONCATENATE 'LT_ZTCOU104-FSC' p_poper+1(2) INTO $field.
      ASSIGN ($field) TO <f>.
      IF <f> = space.
        <f> = lt_ztcou104-id.

        lt_ztcou104-aedat = sy-datum.
        lt_ztcou104-aenam = sy-uname.
        APPEND lt_ztcou104.
        CLEAR lt_ztcou104.
      ENDIF.

*-- New... add ID...
    ELSE.


      CLEAR *mara.
      SELECT SINGLE mtart INTO *mara-mtart
          FROM mara WHERE matnr EQ gt_ztcou100-matnr.
      IF *mara-mtart EQ 'FERT'.
        SELECT SINGLE * FROM a004  WHERE kappl EQ 'V'
                                     AND kschl EQ 'ZV00'
                                     AND matnr EQ  gt_ztcou100-matnr .
      ELSE.
        SELECT SINGLE * FROM a005  WHERE kappl EQ 'V'
                                     AND kschl EQ 'ZP00'
                                     AND matnr EQ  gt_ztcou100-matnr .
      ENDIF.
      IF sy-subrc EQ 0.
        CLEAR lt_ztcou104.
        MOVE-CORRESPONDING gt_ztcou100 TO lt_ztcou104.
        lt_ztcou104-id     = gt_ztcou100-matnr.
        lt_ztcou104-idtext = gt_ztcou100-maktx.

        CONCATENATE 'LT_ZTCOU104-FSC' p_poper+1(2) INTO $field.
        ASSIGN ($field) TO <f>.

        <f> = lt_ztcou104-id.

        lt_ztcou104-aedat = sy-datum.
        lt_ztcou104-aenam = sy-uname.
        APPEND lt_ztcou104.
        CLEAR lt_ztcou104.
      ENDIF.
    ENDIF.
  ENDLOOP.

  DATA l_lines TYPE i.
  DESCRIBE TABLE lt_ztcou104 LINES l_lines.

  IF l_lines > 0.
    MODIFY ztcou104 FROM TABLE lt_ztcou104.
    COMMIT WORK.
    MESSAGE s000 WITH 'Successfully transfered :' l_lines 'recored(s).'.
  ENDIF.

ENDFORM.                    " transfer_from_mat_ledger
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

* EXCEPT MODULE COSTING
  CASE p_kalka.

* [Module]
*   - Procurement type : F ONLY; X-ERROR DURING COSTING
*     BOM Usage: 2
    WHEN 'M1'.
      SELECT SINGLE a~beskz fvidk a~werks
        INTO (p_beskz, p_verid, p_werks)
        FROM marc AS a
        JOIN mast AS b
          ON b~matnr = a~matnr
         AND b~werks = a~werks
         AND b~stlan = '2'                      " BOM Usage
       WHERE a~matnr = p_matnr
         AND a~beskz = 'F'.                     "MUST be 'F'

      p_stlan = '2'.

    WHEN 'BP'.
      SELECT SINGLE a~beskz a~werks a~stdpd
                    b~stlnr b~stlal
        INTO (p_beskz, p_werks, p_stdpd,
              l_stlnr, l_stlal)
        FROM marc AS a
        JOIN mast AS b
          ON b~matnr = a~matnr
         AND b~werks = a~werks
         AND b~stlan = '6'                      " BOM Usage : ABP
       WHERE a~matnr = p_matnr
         AND ( a~beskz = 'E' OR a~beskz = 'X' ).

      IF sy-subrc = 0.
        CLEAR l_datuv.
        SELECT MAX( datuv ) INTO l_datuv FROM stko
          WHERE stlty = 'M'
            AND stlnr = l_stlnr
            AND stlal = l_stlal.
        IF l_datuv <= lv_date.
          p_stlan = '6'.
        ELSE.
*          p_stlan = '1'.  "???
        ENDIF.
      ENDIF.

* UNIT COST
    WHEN OTHERS.
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
     AND prfg_s <> '3'                                      " 5. Status
         AND verid IN r_verid.

      IF sy-subrc = 0.
        p_verid = lt_mkal-verid.            " Production Version
        p_stlal = lt_mkal-stlal.            " Alternative BOM
        p_stlan = '1'.                      " production
      ENDIF.

  ENDCASE.



ENDFORM.                    " CHK_MAT
*&---------------------------------------------------------------------*
*&      Form  GET_DEFAULT_VARIANT_F14
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_VARI  text
*----------------------------------------------------------------------*
FORM get_default_variant_f14 USING p_variant.

  DATA: gx_variant         LIKE disvariant.

  gx_variant-report   = sy-repid.
  gx_variant-username = sy-uname.

  IF NOT p_variant IS INITIAL.
    gx_variant-variant = p_variant.
  ENDIF.

  CALL FUNCTION 'REUSE_ALV_VARIANT_DEFAULT_GET'
       EXPORTING
            i_save        = 'U'
       CHANGING
            cs_variant    = gx_variant
       EXCEPTIONS
            wrong_input   = 1
            not_found     = 2
            program_error = 3
            OTHERS        = 4.

  CASE sy-subrc.
    WHEN 0.
      p_variant = gx_variant-variant.
    WHEN 2.
      CLEAR p_variant.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  fill_gt_out
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_ROW_ID  text
*      -->P_LS_KEY  text
*----------------------------------------------------------------------*
FORM fill_gt_out USING    ls_key
                          ls_out STRUCTURE gt_out
                          l_matnr.

  DATA: BEGIN OF lt_a005 OCCURS 0,
          matnr   LIKE a005-matnr,
          kunnr   LIKE a005-kunnr,
        END   OF lt_a005.

  SELECT SINGLE a~prodh a~mvgr3 a~mvgr4 a~mvgr5 b~werks b~fevor
      c~mtart c~matkl
      INTO (ls_out-prodh,ls_out-mvgr3,ls_out-mvgr4,ls_out-mvgr5,
ls_out-werks,ls_out-fevor,ls_out-mtart,ls_out-matkl)
      FROM mvke AS a
      INNER JOIN marc AS b
      ON b~matnr EQ a~matnr
      INNER JOIN mara AS c
      ON c~matnr EQ b~matnr
      WHERE a~matnr EQ l_matnr.

  IF sy-subrc EQ 0.

    READ TABLE it_tvm3t WITH KEY mvgr3 = ls_out-mvgr3
    BINARY SEARCH.
    IF sy-subrc EQ 0.
      ls_out-mvgr3txt = it_tvm3t-bezei.
    ENDIF.

    READ TABLE it_tvm4t WITH KEY mvgr4 = ls_out-mvgr4
    BINARY SEARCH.
    IF sy-subrc EQ 0.
      ls_out-mvgr4txt = it_tvm4t-bezei.
    ENDIF.

    READ TABLE it_tvm5t WITH KEY mvgr5 = ls_out-mvgr5
    BINARY SEARCH.
    IF sy-subrc EQ 0.
      ls_out-mvgr5txt = it_tvm5t-bezei.
    ENDIF.

    READ TABLE it_t179t WITH KEY prodh = ls_out-prodh
    BINARY SEARCH.
    IF sy-subrc EQ 0.
      ls_out-prodhtxt = it_t179t-vtext.
    ENDIF.

    IF ls_out-fevor NE 'SEA'.

      PERFORM get_code USING l_matnr
                    CHANGING ls_out-model
                             ls_out-kunnr .

    ENDIF.
    IF ls_out-mtart EQ 'HALB'.
      ls_out-model = ls_out-werks.
    ENDIF.
    IF ls_out-matkl EQ 'A/S'.
      ls_out-kunnr = 'MOBIS'.
    ELSE.

      SELECT matnr kunnr INTO TABLE lt_a005
      FROM a005
      WHERE matnr EQ ls_out-fsc.

      READ TABLE lt_a005 WITH KEY matnr = l_matnr
      BINARY SEARCH.
      IF sy-subrc EQ 0.
        ls_out-kunnr = lt_a005-kunnr.
      ENDIF.

    ENDIF.

  ENDIF.

ENDFORM.                    " fill_gt_out
*&---------------------------------------------------------------------*
*&      Form  fill_kunnr
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_MATNR  text
*      <--P_L_KUNNR  text
*----------------------------------------------------------------------*
FORM fill_kunnr USING    l_matnr
                CHANGING l_kunnr.

  DATA: BEGIN OF lt_a005 OCCURS 0,
          matnr   LIKE a005-matnr,
          kunnr   LIKE a005-kunnr,
        END   OF lt_a005.


  SELECT matnr kunnr INTO TABLE lt_a005
  FROM a005
  WHERE matnr EQ l_matnr.

  READ TABLE lt_a005 WITH KEY matnr = l_matnr
  BINARY SEARCH.
  IF sy-subrc EQ 0.
    l_kunnr = lt_a005-kunnr.
  ENDIF.

ENDFORM.                    " fill_kunnr
