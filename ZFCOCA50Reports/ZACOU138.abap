*----------------------------------------------------------------------
* Program ID        : ZACOU138
* Title             : [CO] CO WIP Clear (CPZP)
* Created on        : 01/23/2008
* Created by        : I.G.MOON
* Specifications By : Andy Choi
* Description       : CO WIP Clear (CPZP)
*----------------------------------------------------------------------
REPORT zacou138 MESSAGE-ID zmco.

TABLES :                                                    " ztcou138,
          sscrfields.
TABLES: ppc_head, ppc_conf_act_var,ppc_ord_inf,qrp002,
        ppc_act, ppc_mat_det, ppc_mat, cpzp, ckmlmv013, mbew, marv.

INCLUDE zacoui00.

*----------------------------------------------------------------------*
* Macros
*----------------------------------------------------------------------*

DEFINE __define_not_important.
* { not important
* Total Doc. Count to be created.
  data  : total_doc_cnt type i,
          current_doc_cnt type i.
  data : percentage type p,$mod type i,
         $current_cnt(10),$total_cnt(10),$text(30) .
  clear : total_doc_cnt,current_doc_cnt.
* }
END-OF-DEFINITION.

DEFINE __not_important.
  add 1 to current_doc_cnt.
  $mod = current_doc_cnt mod 100.
  if $mod eq 0.
    $current_cnt = current_doc_cnt.
    concatenate $current_cnt '/' $total_cnt
    into $text.
    condense $text.
    percentage = current_doc_cnt / total_doc_cnt * 100.
    perform show_progress using $text percentage.
  endif.
END-OF-DEFINITION.

DEFINE __not_important_pre.
  add 1 to current_doc_cnt.
  $current_cnt = current_doc_cnt.
  concatenate 'Get PO' $current_cnt '/' $total_cnt
  into $text.
  condense $text.
  percentage = current_doc_cnt / total_doc_cnt * 100.
  perform show_progress using $text percentage.
END-OF-DEFINITION.

DEFINE __cls.                          " clear & refresh
  clear &1.refresh &1.
END-OF-DEFINITION.

DEFINE __process.
  perform show_progress using &1 &2.
END-OF-DEFINITION.

DEFINE __message.
  call function 'POPUP_TO_INFORM'
       exporting
            titel = &1
            txt1  = &2
            txt2  = sy-subrc.
END-OF-DEFINITION.

DEFINE u_break.
  if p_debug eq true.
    break-point.
  endif.
END-OF-DEFINITION.

DATA: BEGIN OF ftab OCCURS 10,
        fcode(6),
      END OF ftab.

DEFINE __focus.
  call method cl_gui_control=>set_focus
      exporting
        control = &1 .
END-OF-DEFINITION.

****************************** constants *******************************
CONSTANTS:  false VALUE ' ',
            true  VALUE 'X'.

*--------------------------------------------------------------------*
* SELECTION-SCREEN
*--------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK clr WITH FRAME TITLE text-t05.
PARAMETERS p_bukrs TYPE bukrs MEMORY ID buk.
PARAMETERS: t_gjper TYPE co_gjper   OBLIGATORY.
PARAMETERS: p_test AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK clr.

SELECTION-SCREEN BEGIN OF BLOCK etc WITH FRAME TITLE text-t04.
SELECT-OPTIONS:
            s_aufnr   FOR ckmlmv013-aufnr OBLIGATORY.

SELECTION-SCREEN SKIP 1.

SELECT-OPTIONS:
            s_objnr   FOR cpzp-objnr    ,
            s_fobjn   FOR cpzp-f_objnr  ,
            s_gjper   FOR cpzp-gjper    ,
            s_pmatn   FOR ckmlmv013-pmatn ,
            s_verid   FOR ckmlmv013-verid ,
            s_matnr   FOR mbew-matnr.
PARAMETERS:
            p_bwkey  LIKE mbew-bwkey DEFAULT 'P001'.
SELECTION-SCREEN END OF BLOCK etc.

* Layout
SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME TITLE text-010.
PARAMETER : p_vari TYPE slis_vari.
SELECTION-SCREEN END OF BLOCK b4.

PARAMETER : p_debug AS CHECKBOX.

*----------------------------------------------------------------------*
* Global Data
*----------------------------------------------------------------------*
RANGES : gr_matnr FOR ekbe-matnr.

TYPES: BEGIN OF ty_row_tab,

*        include structure ztcou137.

        objnr   LIKE cpzp-objnr ,
        f_objnr LIKE cpzp-f_objnr ,
        gjper   LIKE cpzp-gjper ,   "yyyymmm
        istmn   LIKE cpzp-istmn ,   "PrevWIP + Current Input
        gmper   LIKE cpzp-gmper ,   "Current Input
        gmsum   LIKE cpzp-gmsum ,   "Current Output
        varmn   LIKE cpzp-varmn ,   "Variance
        xmper   LIKE cpzp-xmper ,   "current scrap
        xmsum   LIKE cpzp-xmsum ,   "total scrap
        meinh   LIKE cpzp-meinh ,   "Unit of measure for operation
        aufnr   LIKE aufk-aufnr ,
        pmatn   LIKE ckmlmv013-pmatn ,
        verid   LIKE ckmlmv013-verid,
        wip     LIKE cpzp-gmsum ,   "WIP=ISTMN-GMSUM
        typps(2) TYPE c,            "VS-material, KL-activity
        compn(18) TYPE c,
        vspvb(10) TYPE c,
        rp(2)     TYPE c,

 END OF ty_row_tab.

TYPES: BEGIN OF ty_out.
INCLUDE  TYPE ty_row_tab.
TYPES:
       icon TYPE icon-id,
       chk(1).
TYPES   remarks(30).
TYPES   ok?(1).
TYPES   celltab  TYPE lvc_t_styl.
TYPES   tabcolor TYPE slis_t_specialcol_alv.
TYPES: END OF ty_out.

DATA  : it_row_tab TYPE TABLE OF ty_row_tab WITH HEADER LINE,
        gt_out     TYPE TABLE OF ty_out     WITH HEADER LINE,
        i_ztcou137 TYPE TABLE OF ztcou137    WITH HEADER LINE.

DATA  $gt_out LIKE gt_out OCCURS 0 WITH HEADER LINE.

TYPES: BEGIN OF ty_t001k,
         bwkey TYPE bwkey,                " Plant
       END OF ty_t001k.

DATA: g_error(1),
      g_repid  LIKE sy-repid.

DATA  : flag_data_changed,
        info(80).

RANGES: r_bwkey FOR t001k-bwkey.           " Plant

*data : begin of it_fsc_rp   occurs 0,
*        sa    like plpo-usr00,  "Def.Supply area
*        rp    like plpo-usr01,  "RP
*        wc    like crhd-arbpl,  "WC
*        shop  like plpo-usr02,  "SHOP
*       end of it_fsc_rp.
*
DATA: BEGIN OF it_itab OCCURS 0,
        objnr   LIKE cpzp-objnr ,
        f_objnr LIKE cpzp-f_objnr ,
        gjper   LIKE cpzp-gjper ,   "yyyymmm
        istmn   LIKE cpzp-istmn ,   "PrevWIP + Current Input
        gmper   LIKE cpzp-gmper ,   "Current Input
        gmsum   LIKE cpzp-gmsum ,   "Current Output
        varmn   LIKE cpzp-varmn ,   "Variance
        xmper   LIKE cpzp-xmper ,   "current scrap
        xmsum   LIKE cpzp-xmsum ,   "total scrap
        meinh   LIKE cpzp-meinh ,   "Unit of measure for operation
        aufnr   LIKE aufk-aufnr ,
        pmatn   LIKE ckmlmv013-pmatn ,
        verid   LIKE ckmlmv013-verid,
        wip     LIKE cpzp-gmsum ,   "WIP=ISTMN-GMSUM
        typps(2)  TYPE c,            "VS-material, KL-activity
        compn(18) TYPE c,
        vspvb(10) TYPE c,
      END OF it_itab.

DATA: BEGIN OF it_itab_tmp OCCURS 0,
        pmatn   LIKE ckmlmv013-pmatn ,
        verid   LIKE ckmlmv013-verid,
        typps(2) TYPE c,            "VS-material, KL-activity
        compn(18) TYPE c,
        gjper   LIKE cpzp-gjper ,   "yyyymmm
        wip     LIKE cpzp-gmsum ,   "WIP=ISTMN-GMSUM

        objnr   LIKE cpzp-objnr ,
        f_objnr LIKE cpzp-f_objnr ,
        istmn   LIKE cpzp-istmn ,   "PrevWIP + Current Input
        gmper   LIKE cpzp-gmper ,   "Current Input
        gmsum   LIKE cpzp-gmsum ,   "Current Output
        varmn   LIKE cpzp-varmn ,   "Variance
        xmper   LIKE cpzp-xmper ,   "current scrap
        xmsum   LIKE cpzp-xmsum ,   "total scrap
        meinh   LIKE cpzp-meinh ,   "Unit of measure for operation
        aufnr   LIKE aufk-aufnr ,
        vspvb(10) TYPE c,
      END OF it_itab_tmp.

DATA: BEGIN OF it_kal OCCURS 0,
        kaln1   LIKE mbew-kaln1,
        f_objnr LIKE cpzp-f_objnr,
      END OF it_kal.

DATA: BEGIN OF it_mbew OCCURS 0,
        kaln1   LIKE mbew-kaln1,
        matnr   LIKE mbew-matnr,
        vspvb   TYPE vspvb,
        f_objnr LIKE cpzp-f_objnr,
      END OF it_mbew.

TYPES: BEGIN OF ty_ord.
        INCLUDE STRUCTURE ppc_ord_inf.
TYPES:
        aufnr   LIKE ckmlmv013-aufnr ,
        problem(1),
       END OF ty_ord.

DATA: lt_hdr LIKE ppc_head OCCURS 0,
      ls_hdr LIKE ppc_head,
      lt_ord TYPE TABLE OF ty_ord,
      ls_ord TYPE ty_ord,
      ls_cpzp LIKE cpzp,
      ls_cpzp_tmp LIKE cpzp,
      lt_cpzp LIKE cpzp OCCURS 0,
      lt_cpzp_tmp LIKE cpzp OCCURS 0,
      l_accassobj TYPE ppc_accassobj_int,
      lf_aufnr TYPE aufnr,
      lf_objnr TYPE j_objnr,
      lf_pkosa_error TYPE c,
      l_d TYPE ist_menge,
      l_mng1 TYPE ist_menge,
      l_mng2 TYPE ist_menge,
      l_problem(1).

INCLUDE <icon>.                        " icon


*----------------------------------------------------------------------*
* Define local class
*----------------------------------------------------------------------*
CLASS lcl_event_receiver DEFINITION.
  PUBLIC SECTION.

    TYPES: BEGIN OF ztcou135_k,
               werks TYPE werks_d,
               fevor TYPE fevor,
           END OF ztcou135_k.

    TYPES: ztcou135_key   TYPE STANDARD TABLE OF ztcou135_k,
           ztcou135_table TYPE STANDARD TABLE OF ztcou135.

    METHODS:
      handle_data_changed
         FOR EVENT data_changed OF cl_gui_alv_grid
             IMPORTING er_data_changed,
                       get_deleted_rows
             EXPORTING
                       deleted_rows TYPE ztcou135_table,

      refresh_delta_tables.

  PRIVATE SECTION.
    DATA deleted_rows TYPE STANDARD TABLE OF ztcou135.

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

  METHOD update_delta_tables.
    DATA: l_del_row TYPE lvc_s_moce,
          ls_ztcou135 TYPE ztcou135,
          ls_outtab LIKE LINE OF gt_out.

    LOOP AT pr_data_changed->mt_deleted_rows INTO l_del_row.
      READ TABLE gt_out INTO ls_outtab INDEX l_del_row-row_id.
      IF sy-subrc NE 0.
        MESSAGE i000(0k) WITH text-e01. "Internal error
      ELSE.
        MOVE-CORRESPONDING ls_outtab TO ls_ztcou135.
        APPEND ls_ztcou135 TO deleted_rows.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.                   " LCL_EVENT_RECEIVER Implementation

DATA g_event_receiver  TYPE REF TO lcl_event_receiver.

*----------------------------------------------------------------------*
* INITIALIZATION
*----------------------------------------------------------------------*
INITIALIZATION.
  sy-title = '[CO] WIP Clear (CPZP)'.
  PERFORM default_.
*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_vari.
  PERFORM alv_variant_f4 CHANGING p_vari.

*----------------------------------------------------------------------*
START-OF-SELECTION.
*----------------------------------------------------------------------*

*  data : $year(4) type n,
*         $peri(3) type n,
*         yvper type co_gjper.
*
*  yvper = sy-datum(6).
*
*  $year = yvper div 100.
*  $peri = yvper - ( $year * 100 ).
*
*  concatenate $year $peri into yvper.
*  if t_gjper < yvper.
* message s000 with 'The date must be greater or equal to current Date!'
*.
*    exit.
*  endif.

  PERFORM check_date.

  PERFORM :
            initialize,
            get_row_data,
            refine_row_itab.

  PERFORM view_from_memory.

*----------------------------------------------------------------------*
END-OF-SELECTION.
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
             'PMATN'    ' '  'X' '' 'X' '',
             'VERID'    ' '  'X' '' 'X' '',
             'TYPPS'    ' '  'X' '' 'X' '',
             'COMPN'    ' '  'X' '' 'X' ''.
*             'GJPER'    ' '  'X' '' 'X' ''.

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

ENDFORM.                    " INITIALIZE_
*&---------------------------------------------------------------------*
*&      Form  default_
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM default_.

ENDFORM.                    " default_
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
  __process 'Refining data' '70'.

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

ENDFORM.                    " MOVE_OUT_
*&---------------------------------------------------------------------*
*&      Form  VIEW_
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM view_.
  CLEAR g_error.
  __cls : it_row_tab,gt_out.

ENDFORM.                    " VIEW_
*&---------------------------------------------------------------------*
*&      Form  POP_UP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0811   text
*      -->P_0812   text
*      -->P_0813   text
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
*&      Form  DATA_DELETE_CONFIRM
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
      ' (CPZP) data will be changed!'
      'Do you really want to proceed?' ' '
                 CHANGING l_answer.

  IF l_answer NE 'J'.
    g_error = true.
    MESSAGE s000 WITH 'Processing was canceled by user.'.
  ENDIF.


ENDFORM.                    " DATA_DELETE_CONFIRM
*&---------------------------------------------------------------------*
*&      Form  convert_period
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_PLAF_PERIO  text
*      <--P_IT_ROW_TAB_PERIO  text
*----------------------------------------------------------------------*
FORM convert_period USING    p_perio
                    CHANGING p_jahrper.

  DATA : $year(4) TYPE n,
         $period(3) TYPE n.

  $year = p_perio DIV 100.
  $period = p_perio - ( $year * 100 ).

  CONCATENATE $year $period INTO p_jahrper.

ENDFORM.                    " convert_period
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET TITLEBAR '100'.
*   Exclude toolbar
  PERFORM exclude_functions.
  __cls ftab.
  SET PF-STATUS '100' EXCLUDING ftab.

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

    WHEN 'SAVE'.
      CHECK sy-dynnr EQ '0100'.
      PERFORM really?.
      CHECK g_error NE true.

      PERFORM : save_to_cpzp,
                refresh_alv.
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
       EXPORTING i_event_id = cl_gui_alv_grid=>mc_evt_modified.

  CALL METHOD g_grid->set_ready_for_input
     EXPORTING
            i_ready_for_input = 0.

  PERFORM sort_build USING gt_sort[].

*   Setting for layout
  PERFORM set_lvc_layout.

*   Set colors
  PERFORM set_color.

*   Set variant
  gv_repid = gs_variant-report = sy-repid.
  gs_variant-variant = p_vari.

*   Define cell attribute
  PERFORM build_cell_attr.

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
    gs_fcat-just          = &5.
    append gs_fcat to gt_fcat.
  END-OF-DEFINITION.

  __catalog :
 'X' 'PMATN'     'Product'      '18'  'L',
 'X' 'VERID'     'VERID'        '04'  'L',
 'X' 'TYPPS'     'Type'         '02'  'L',
 'X' 'COMPN'     'Component'    '18'  'L',
 ' ' 'GJPER'     'YYYYMM'       '07'  'L',
 ' ' 'ISTMN'     'PrWIP+In'     '14'  'R',
 ' ' 'GMPER'     'Input'        '14'  'R',
 ' ' 'GMSUM'     'Output'       '14'  'R',
 ' ' 'WIP'       'WIP'          '14'  'R',
 ' ' 'MEINH'     'MEINH'        '04'  'L',
 ' ' 'OBJNR'     'OBJNR'        '14'  'L',
 ' ' 'F_OBJNR'   'F_OBJNR'      '17'  'L',
 ' ' 'VARMN'     'Variance'     '10'  'R',
 ' ' 'VSPVB'     'Def.Suppl'    '10'  'L',
* ' ' 'RP'        'RP'           '02'  'L',
 ' ' 'XMPER'     'Scrap'        '10'  'R',
 ' ' 'XMSUM'     'ScrapSum'     '10'  'R',
 ' ' 'AUFNR'     'Order'        '12'  'L'.

**  loop at gt_fcat into gs_fcat.
**    case gs_fcat-fieldname.
**      when 'A_SCRAP' or 'AUSCH' or 'AVOAU'
**        or 'R_SCRAP'.
**        gs_fcat-just = 'R'.
***        GS_FCAT-NO_ZERO = 'X'.
**      when 'NETAU'.
**        gs_fcat-checkbox = 'X'.
**    endcase.
**
**    gs_fcat-ref_table = 'ZTCOU135'.
**    gs_fcat-ref_field = gs_fieldcat-fieldname.
**
**    modify gt_fcat from gs_fcat.
**  endloop.

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
            'WERKS'     '1' 0,
            'FEVOR'     '1' 0,
            'TXT'       '1' 0,
            'A_SCRAP'   '2' 0,
            'AUSCH'     '3' 0,
            'AVOAU'     '3' 0,
            'NETAU'     '3' 0,
            'R_SCRAP'   '2' 0.

  gt_out-tabcolor[] = gt_specialcol[].
  MODIFY gt_out TRANSPORTING tabcolor WHERE tabcolor IS initial.

ENDFORM.                    " SET_COLOR
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

  CLEAR lt_celltab.
  REFRESH lt_celltab.

  CLEAR gs_fcat.

  LOOP AT gt_fcat INTO gs_fcat.
    ls_celltab-fieldname = gs_fcat-fieldname.
    IF   ls_celltab-fieldname = 'A_SCRAP'
      OR ls_celltab-fieldname = 'AUSCH'
      OR ls_celltab-fieldname = 'AVOAU'
      OR ls_celltab-fieldname = 'NETAU'
      OR ls_celltab-fieldname = 'R_SCRAP'.
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

*  data: lt_celltab type lvc_t_styl,
*        ls_celltab type lvc_s_styl.
*
*  clear lt_celltab.
*  refresh lt_celltab.
*
*  __cls gt_out-celltab.
*  modify gt_out transporting celltab where fevor = space.
*
*  clear gs_fcat.
*
*  loop at gt_fcat into gs_fcat.
*    ls_celltab-fieldname = gs_fcat1-fieldname.
*    ls_celltab-style = cl_gui_alv_grid=>mc_style_disabled.
*    insert ls_celltab into table lt_celltab.
*  endloop.
*
*  insert lines of lt_celltab into table gt_out-celltab.
*  modify gt_out transporting celltab where fevor = space.


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
*&      Form  view_result
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM view_result.

ENDFORM.                    " view_result

*&---------------------------------------------------------------------*
*&      Form  SWITCH_EDIT_MODE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM switch_edit_mode.

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

  DATA $ix LIKE sy-tabix.

  PERFORM show_progress USING 'Gather Data...' '5'.

  u_break.

  PERFORM get_data_normal.

  CHECK g_error EQ false.
  u_break.

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

ENDFORM.                    " APPLY_ICON
*&---------------------------------------------------------------------*
*&      Form  save_to_cpzp
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM save_to_cpzp.

  __cls $gt_out.

  DATA: lt_row   TYPE lvc_t_row,
        ls_row   TYPE lvc_s_row,
        lt_roid  TYPE lvc_t_roid,
        lv_cnt(5),
        lv_dcnt(5),
        lv_msg(200).                 " Message

  DATA  lt_messages      LIKE messages       OCCURS 0 WITH HEADER LINE.
  DATA  $messages      LIKE messages       OCCURS 0 WITH HEADER LINE.
  DATA  $ix LIKE sy-tabix.

* Save seleted data to table ZTCOU135
  CLEAR: lv_cnt, lt_row[], lt_roid[].

  PERFORM get_selected_rows TABLES $gt_out.

  CHECK NOT $gt_out[] IS INITIAL.

  AUTHORITY-CHECK OBJECT 'S_DEVELOP'
    ID 'DEVCLASS' DUMMY
    ID 'OBJTYPE'  FIELD 'DEBUG'
    ID 'OBJNAME'  DUMMY
    ID 'P_GROUP'  DUMMY
    ID 'ACTVT'    FIELD '03'.
  IF  sy-subrc <> 0.
    MESSAGE s895(m7) WITH 'Sorry, no authorization'.
    EXIT.
  ENDIF.

  DATA: lt_cpzp LIKE cpzp OCCURS 0 WITH HEADER LINE.
  DATA: i_ztcou_cpzp_log LIKE ztcou_cpzp_log OCCURS 0 WITH HEADER LINE.

  u_break.
  __cls: lt_cpzp, i_ztcou_cpzp_log.
  CLEAR g_error.
  LOOP AT $gt_out.
    $ix = sy-tabix.
    CLEAR ls_cpzp.

    MOVE-CORRESPONDING $gt_out TO ls_cpzp.
    ls_cpzp-gjper = t_gjper.
    ls_cpzp-istmn = $gt_out-wip.
    ls_cpzp-gmsum = $gt_out-wip.
    clear ls_cpzp-gmper.

    SELECT SINGLE * FROM cpzp
    WHERE objnr EQ  $gt_out-objnr
    AND f_objnr EQ $gt_out-f_objnr
    AND gjper   EQ t_gjper.

    IF sy-subrc EQ 0.
      MESSAGE s001 WITH 'Data already exist for ' t_gjper.
      g_error = true.
      EXIT.
    ENDIF.

    IF p_test IS INITIAL.

      INSERT cpzp FROM ls_cpzp.

      IF sy-subrc NE 0.
        MESSAGE s001 WITH 'Data already exist for ' t_gjper.
        g_error = true.
        EXIT.
      ENDIF.

      MOVE-CORRESPONDING ls_cpzp TO i_ztcou_cpzp_log.
      i_ztcou_cpzp_log-zedat = sy-datum.
      i_ztcou_cpzp_log-zetim = sy-uzeit.
      i_ztcou_cpzp_log-erdat = sy-datum.
      i_ztcou_cpzp_log-ernam = sy-uname.
      APPEND i_ztcou_cpzp_log.

    ELSE.

    ENDIF.

  ENDLOOP.

  IF g_error EQ true.
    ROLLBACK WORK.
    EXIT.
  ENDIF.
  IF p_test IS INITIAL.
    MODIFY ztcou_cpzp_log FROM TABLE i_ztcou_cpzp_log.
    COMMIT WORK.
  ENDIF.

ENDFORM.                    " save_to_ztable
*&---------------------------------------------------------------------*
*&      Form  get_data_normal
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data_normal.

  PERFORM get_open_order.
  IF l_problem = 'A'.
    WRITE: /1
       'ABORT! Please finish first the open confirmations.(Tr.PPCGO)'.
    EXIT.
  ENDIF.

  RANGES: r_fobjn FOR cpzp-f_objnr.

  DESCRIBE TABLE s_matnr LINES sy-tabix.
  IF sy-tabix > 0.
    r_fobjn-option = 'EQ'.
    r_fobjn-sign   = 'I'.
    SELECT * FROM mbew WHERE matnr IN s_matnr
                         AND bwkey = p_bwkey.
      r_fobjn-low(2)    = 'VS'.
      r_fobjn-low+4(12) = mbew-kaln1.
      APPEND r_fobjn.
    ENDSELECT.
  ENDIF.

  SELECT cpzp~objnr
         cpzp~f_objnr cpzp~gjper
         cpzp~istmn cpzp~gmper cpzp~gmsum
         cpzp~varmn
         cpzp~xmper cpzp~xmsum
         cpzp~meinh
         aufk~aufnr
         ckmlmv013~pmatn ckmlmv013~verid
  INTO it_itab
  FROM ( cpzp
         INNER JOIN aufk
            ON aufk~objnr = cpzp~objnr
         INNER JOIN ckmlmv013
            ON ckmlmv013~aufnr = aufk~aufnr )
         WHERE cpzp~objnr      IN s_objnr
           AND cpzp~f_objnr    IN s_fobjn
           AND cpzp~f_objnr    IN r_fobjn
           AND cpzp~gjper      IN s_gjper
           AND ckmlmv013~aufnr IN s_aufnr
           AND ckmlmv013~pmatn IN s_pmatn
           AND ckmlmv013~verid IN s_verid.

    it_itab-wip = it_itab-istmn - it_itab-gmsum.
    APPEND it_itab.

  ENDSELECT.

  LOOP AT it_itab.
    IF it_itab-f_objnr(2) = 'VS'.
      it_kal-kaln1   = it_itab-f_objnr+4(12).
      it_kal-f_objnr = it_itab-f_objnr.
      APPEND it_kal.
    ENDIF.
  ENDLOOP.

  SORT it_kal BY kaln1.
  DELETE ADJACENT DUPLICATES FROM it_kal.
  SELECT a~kaln1 a~matnr c~vspvb
    INTO TABLE it_mbew
    FROM mbew AS a
    INNER JOIN marc AS c
       ON c~matnr = a~matnr
      AND c~werks = a~bwkey
    FOR ALL ENTRIES IN it_kal
    WHERE a~kaln1 = it_kal-kaln1.

  DATA: l_idx LIKE sy-tabix.
  LOOP AT it_mbew.
    l_idx = sy-tabix.
    READ TABLE it_kal WITH KEY kaln1 = it_mbew-kaln1 BINARY SEARCH.
    it_mbew-f_objnr = it_kal-f_objnr.
    MODIFY it_mbew INDEX l_idx TRANSPORTING f_objnr.
  ENDLOOP.
  SORT it_mbew BY f_objnr.

  LOOP AT it_itab.
    l_idx = sy-tabix.
    it_itab-typps = it_itab-f_objnr(2).

    IF it_itab-typps = 'VS'.
    READ TABLE it_mbew WITH KEY f_objnr = it_itab-f_objnr BINARY SEARCH.
      it_itab-compn = it_mbew-matnr.
      it_itab-vspvb = it_mbew-vspvb.
      MODIFY it_itab INDEX l_idx TRANSPORTING typps compn vspvb.
    ELSE.
      it_itab-compn = it_itab-f_objnr+6(16).
      MODIFY it_itab INDEX l_idx TRANSPORTING typps compn.
    ENDIF.
  ENDLOOP.

  __cls  it_itab_tmp.

  LOOP AT it_itab.
    CHECK it_itab-compn IN s_matnr.
    MOVE-CORRESPONDING it_itab TO it_itab_tmp.
    APPEND it_itab_tmp.
  ENDLOOP.

  SORT it_itab_tmp BY pmatn verid typps compn ASCENDING
                    gjper DESCENDING.

  __cls it_itab.

  DATA $flag.

  LOOP AT it_itab_tmp.
    AT NEW compn.
      $flag = true.
    ENDAT.
    CHECK $flag EQ true.
    CLEAR $flag.
    CHECK it_itab_tmp-wip <> 0.
    MOVE-CORRESPONDING it_itab_tmp TO it_itab.
    APPEND it_itab.
    CLEAR it_itab.
  ENDLOOP.

  SORT it_itab BY pmatn verid typps compn.

  __cls it_row_tab.


  LOOP AT it_itab.
    MOVE-CORRESPONDING it_itab TO it_row_tab.
    APPEND it_row_tab.
  ENDLOOP.

ENDFORM.                    " get_data_normal
*&---------------------------------------------------------------------*
*&      Form  view_from_memory
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM view_from_memory.

  PERFORM : move_out ,
            set_output .

ENDFORM.                    " view_from_memory
*&---------------------------------------------------------------------*
*&      Form  filter_itab
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_open_order.

  CLEAR : lt_hdr[],ls_hdr,
          lt_ord[],ls_ord,
          ls_cpzp,ls_cpzp_tmp,
          lt_cpzp[],
          lt_cpzp_tmp[],
          l_accassobj,
          lf_aufnr,lf_objnr,lf_pkosa_error,
          l_d,l_mng1,l_mng2,l_problem.

  CLEAR l_problem.

  SELECT
        a~orderid
        a~accassobj
        a~rmprofile
        a~materialnr
        a~plant
        a~version
        a~line_header
        a~line_version
        a~ordernr
        a~dummy_order
        b~aufnr
  INTO CORRESPONDING FIELDS OF TABLE lt_ord
  FROM ppc_ord_inf AS a
  INNER JOIN ckmlmv013 AS b
  ON  b~pmatn EQ a~materialnr
  AND b~prwrk EQ a~plant
  AND b~verid EQ a~version
  WHERE a~materialnr IN s_pmatn
    AND b~aufnr IN s_aufnr
    AND b~pmatn IN s_pmatn
    AND b~verid IN s_verid.

  CHECK NOT lt_ord[] IS INITIAL.

  SELECT * FROM ppc_head INTO TABLE lt_hdr
  FOR ALL ENTRIES IN lt_ord
  WHERE orderid = lt_ord-orderid
  AND flg_info_dest NE '3'.  "exclude PPCVAR

  PERFORM filter_lt_hdr TABLES lt_hdr lt_ord.

  SORT lt_hdr BY orderid  ASCENDING
                 conftime DESCENDING .

  DATA $ix LIKE sy-tabix.

  LOOP AT lt_ord INTO ls_ord.

    $ix = sy-tabix.

    READ TABLE lt_hdr INTO ls_hdr WITH KEY orderid = ls_ord-orderid
         BINARY SEARCH.

    IF sy-subrc EQ 0.
      IF ls_hdr-flg_gr_head IS INITIAL.
*        write: /1 'Is this order final confirmed?',ls_ord-ordernr.
        ls_ord-problem = 'X'.
      ENDIF.
    ELSE.
*write: /1 'Confirmation does not exist for order', ls_ord-ordernr.
      ls_ord-problem = 'X'.
    ENDIF.

* Checking open processes
    LOOP AT lt_hdr INTO ls_hdr WHERE orderid = ls_ord-orderid.
*  is there any open process
      IF ls_hdr-flg_synch EQ ' ' OR ls_hdr-flg_synch = 'B' OR
         ls_hdr-flg_asynch EQ ' ' OR ls_hdr-flg_asynch = 'B' OR
         ls_hdr-flg_asynch_a EQ ' ' OR ls_hdr-flg_asynch_a = 'B'.
        WRITE: /1 'Order has still open process', ls_ord-ordernr,
                                                  ls_hdr-headid.
        l_problem = 'A'.
      ENDIF.
      DELETE lt_hdr.
    ENDLOOP.
    MODIFY lt_ord FROM ls_ord INDEX $ix TRANSPORTING problem.
  ENDLOOP.

  SORT lt_ord BY accassobj.
  DELETE ADJACENT DUPLICATES FROM lt_ord COMPARING accassobj.


ENDFORM.                    " filter_itab
*&---------------------------------------------------------------------*
*&      Form  filter_lt_hdr
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_HDR  text
*      -->P_LT_ORD  text
*----------------------------------------------------------------------*
FORM filter_lt_hdr TABLES p_lt_hdr STRUCTURE ppc_head
                          p_lt_ord STRUCTURE ppc_ord_inf.

  DATA : BEGIN OF lt_order_sum OCCURS 0,
            orderid	TYPE ppc_orderid,
            confquant TYPE ppc_headconfquant,
            flg_reversal  TYPE ppc_flg_rev,
         END OF lt_order_sum.
  DATA $ix LIKE sy-tabix.

  LOOP AT p_lt_hdr INTO ls_hdr.
    MOVE-CORRESPONDING ls_hdr TO lt_order_sum.

    IF lt_order_sum-flg_reversal EQ 'X'.
      lt_order_sum-confquant = -1 * lt_order_sum-confquant.
    ENDIF.
    CLEAR lt_order_sum-flg_reversal.
    COLLECT lt_order_sum.
    CLEAR lt_order_sum.
  ENDLOOP.
  DELETE lt_order_sum WHERE confquant <= 0.
  SORT lt_order_sum BY orderid.

  LOOP AT p_lt_ord INTO ls_ord.
    $ix = sy-tabix.
    READ TABLE lt_order_sum WITH KEY orderid = ls_ord-orderid
                            BINARY SEARCH.
    IF sy-subrc NE 0.
      DELETE p_lt_ord INDEX $ix.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " filter_lt_hdr
*&---------------------------------------------------------------------*
*&      Form  check_date
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_date.

  DATA: h_dontpanic LIKE sy-datlo.

** On 04/08/14
*  GET PARAMETER ID 'DONTPANIC' FIELD h_dontpanic.
*  IF h_dontpanic = sy-datlo.
*  ELSE.
*    p_test = true.
*  ENDIF.
** End on 04/08/14

  DATA : $year(4) TYPE n,
         $mon(3) TYPE n,
         yvper TYPE co_gjper.

  SELECT SINGLE * FROM marv WHERE bukrs EQ p_bukrs.
  IF sy-subrc EQ 0.
    $year = marv-lfgja.
    $mon = marv-lfmon.
    CONCATENATE $year $mon INTO yvper.

    IF t_gjper <> yvper.

      $year = marv-vmgja.
      $mon = marv-vmmon.
      CONCATENATE $year $mon INTO yvper.
      IF t_gjper <> yvper.
 MESSAGE e000 WITH 'The date must be greater or equal to current Date!'.
        STOP.
      ENDIF.
    ELSE.
      EXIT.
    ENDIF.

  ELSE.
    MESSAGE e000 WITH 'Invalid Company!'.
    STOP.
  ENDIF.

ENDFORM.                    " check_date
