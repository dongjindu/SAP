*----------------------------------------------------------------------
* Program ID        : ZACOU144
* Title             : NAFTA Material Cost Report
* Created on        : 4/15/2008
* Created by        : I.G.MOON
* Specifications By : Andy Choi
* Description       : Analyze NAFTA Data
*----------------------------------------------------------------------
REPORT zacou144 MESSAGE-ID zmco..

TABLES : ztco_ck11, ztco_abispost, ztco_nafta, zsco_nafta_ck11,
sscrfields.
INCLUDE : z_moon_alv_top,
          z_moon_alv_fnc.

INCLUDE <icon>.                        " icon

************************************************************************
*////////////////      Dynamic Internal Table //////////////////////////
************************************************************************

DATA: new_table TYPE REF TO data,
      new_line  TYPE REF TO data,
      is_lvc_cat TYPE lvc_s_fcat,
      it_lvc_cat TYPE lvc_t_fcat,
      is_fieldcat TYPE slis_fieldcat_alv.

FIELD-SYMBOLS: <l_table> TYPE table,
               <l_line>  TYPE ANY,
               <l_field> TYPE ANY.

* //////////////////////////////////////////////////// *
DEFINE __cls.                          " clear & refresh
  clear &1.refresh &1.
END-OF-DEFINITION.

DEFINE u_break.
  if p_debug eq true.
    break-point.
  endif.
END-OF-DEFINITION.
DEFINE __define_not_important.
* { not important
* Total Doc. Count to be created.
  data  : total_doc_cnt type i,
          current_doc_cnt type i.
  data : percentage type p,$mod type i,
         $current_cnt(10),$total_cnt(10),$text(100) .
  clear : total_doc_cnt,current_doc_cnt.
* }
END-OF-DEFINITION.

CONSTANTS:  false VALUE ' ',
            true  VALUE 'X'.
* //////////////////////////////////////////////////// *
DATA cursor_f(30).

DATA BEGIN OF g_t_ztco_nafta_ck11 OCCURS 0.
        INCLUDE STRUCTURE zsco_nafta_ck11.
DATA END OF g_t_ztco_nafta_ck11.

DATA  : it_row_tab TYPE TABLE OF zsco_nafta_ck11 WITH HEADER LINE,
        gt_out     TYPE TABLE OF zsco_nafta_ck11 WITH HEADER LINE.

TYPES: BEGIN OF ty_plant,
         bwkey TYPE bwkey,
       END OF ty_plant.

DATA : BEGIN OF gt_fsc OCCURS 0,
         artnr LIKE mara-matnr,
         verid LIKE ztco_nafta-verid,
       END OF  gt_fsc.

DATA : BEGIN OF gt_mat_compn OCCURS 0,
         compn LIKE mara-matnr,
       END OF  gt_mat_compn.

DATA : BEGIN OF gt_mat_text OCCURS 0,
         compn LIKE mara-matnr,
         maktg LIKE makt-maktg,
       END OF  gt_mat_text.

DATA gt_plant      TYPE TABLE OF ty_plant    WITH HEADER LINE.
RANGES : gr_bwkey FOR t001w-bwkey.
DATA   : g_bukrs LIKE bsis-bukrs.

DATA: g_error(1),
      g_repid  LIKE sy-repid.
* //////////////////////////////////////////////////// *
SELECTION-SCREEN BEGIN OF BLOCK b0 WITH FRAME TITLE text-001.
PARAMETERS       p_kokrs LIKE ztco_ck11-kokrs DEFAULT 'H201'.
PARAMETERS       p_bdatj LIKE ztco_ck11-bdatj.
SELECT-OPTIONS   s_poper FOR ztco_ck11-poper.
SELECT-OPTIONS   s_artnr FOR ztco_ck11-artnr.
PARAMETERS       p_klvar LIKE ztco_ck11-klvar DEFAULT 'ZUNF' NO-DISPLAY.
PARAMETERS       p_verid LIKE ztco_ck11-verid.
SELECTION-SCREEN END OF BLOCK b0.


SELECTION-SCREEN BEGIN OF BLOCK b0t WITH FRAME TITLE text-01t.
PARAMETERS p_2   RADIOBUTTON GROUP radl USER-COMMAND ucom.
PARAMETERS p_1   RADIOBUTTON GROUP radl.
SELECTION-SCREEN END OF BLOCK b0t.

SELECTION-SCREEN BEGIN OF BLOCK b0s WITH FRAME TITLE text-002.
PARAMETERS       p_amt AS CHECKBOX DEFAULT 'X'.
PARAMETERS       p_osd AS CHECKBOX DEFAULT 'X'.
PARAMETERS       p_qty AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK b0s.

SELECTION-SCREEN BEGIN OF BLOCK b5 WITH FRAME TITLE text-004.
SELECT-OPTIONS s_compn FOR ztco_ck11-compn.
SELECT-OPTIONS s_kstar FOR ztco_ck11-kstar.
SELECTION-SCREEN END OF BLOCK b5.

* Layout
SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME TITLE text-010.

PARAMETER  p_vari TYPE slis_vari.
SELECTION-SCREEN END OF BLOCK b4.
*----------------------------------------------------------------------*
* INITIALIZATION
*----------------------------------------------------------------------*
INITIALIZATION.
  sy-title = '[CO] NAFTA Material Cost Report'.
  p_2 = true.
  PERFORM modify_screen.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_vari.
  PERFORM alv_variant_f4 CHANGING p_vari.

AT SELECTION-SCREEN OUTPUT.
  PERFORM modify_screen.
  IMPORT cursor_f FROM MEMORY ID '144_cus'.
  IF NOT cursor_f IS INITIAL.
    SET CURSOR FIELD cursor_f.
  ENDIF.

START-OF-SELECTION.
  CLEAR : g_error, cursor_f.

  IF p_bdatj IS INITIAL.
    MESSAGE s000 WITH 'Please enter the year!'.
    g_error = true.
    cursor_f = 'P_BDATJ'.
    STOP.
  ENDIF.

  CASE true.
    WHEN p_1.
      IF s_artnr[] IS INITIAL.
        MESSAGE s000 WITH 'Please enter the FSC!'.
        g_error = true.
        cursor_f = 'S_ARTNR-LOW'.
        STOP.
      ENDIF.
      IF p_verid IS INITIAL.
        MESSAGE s000 WITH 'Please enter the version!'.
        g_error = true.
        cursor_f = 'P_VERID'.
        STOP.
      ENDIF.
    WHEN p_2.
      IF s_poper[] IS INITIAL.
        MESSAGE s000 WITH 'Please enter the period!'.
        g_error = true.
        cursor_f = 'S_POPER-LOW'.
        STOP.
      ENDIF.
  ENDCASE.

  IF p_amt EQ false AND p_qty EQ false AND p_osd EQ false.
    MESSAGE s000 WITH 'Please select at least one display field.' .
    EXIT.
  ENDIF.

  PERFORM show_progress     USING 'Initializing...' '5'.
  PERFORM initialize.

  CASE true.
    WHEN p_1.
      PERFORM : get_row_data,
                group_uniq_fsc.
    WHEN p_2.
      PERFORM : get_row_data_one_p,
                group_uniq_fsc.
  ENDCASE.

  CHECK g_error EQ false.

  PERFORM set_output.

  CASE true.
    WHEN p_1.
      PERFORM : fieldcat_init     USING gt_fieldcat[],
                sort_build        USING gt_sort[],
                catego_by_poper.
    WHEN p_2.
      PERFORM : fieldcat_init_2   USING gt_fieldcat[],
                sort_build_2      USING gt_sort[],
                catego_by_fsc.
  ENDCASE.

  PERFORM show_progress     USING 'Filling the description...' '90'.

  PERFORM : get_txt, fill_text.

  PERFORM show_progress     USING 'Preparing screen...' '95'.

END-OF-SELECTION.
  EXPORT cursor_f TO MEMORY ID '144_cus'.

  CHECK g_error EQ false.
  PERFORM display_alv.

*&---------------------------------------------------------------------*
*&      Form  write_ztco_ck11
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_ZTCO_CK11  text
*      -->P_0304   text
*----------------------------------------------------------------------*
FORM write_ztco_ck11 TABLES   t_ztco_ck11 STRUCTURE ztco_ck11
                      USING   value(p_level)        .

  CLEAR g_t_ztco_nafta_ck11.
  MOVE-CORRESPONDING t_ztco_ck11 TO g_t_ztco_nafta_ck11.
  g_t_ztco_nafta_ck11-zlevel = p_level.

ENDFORM.                    " WRITE_ZTCO_CK11

*&---------------------------------------------------------------------*
*&      Form  get_plant
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_plant.
  __cls  : gt_plant, gr_bwkey.

* Get plant
  SELECT bwkey INTO TABLE gt_plant
    FROM t001k
   WHERE bukrs = g_bukrs.

  LOOP AT gt_plant.
    gr_bwkey-sign = 'I'.
    gr_bwkey-option = 'EQ'.
    gr_bwkey-low = gt_plant-bwkey.

    APPEND gr_bwkey.
    CLEAR gr_bwkey.
  ENDLOOP.

ENDFORM.                    " GET_PLANT
*&---------------------------------------------------------------------*
*&      Form  get_bukrs
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_bukrs.

  SELECT SINGLE bukrs INTO g_bukrs FROM tka02
            WHERE kokrs EQ p_kokrs .

ENDFORM.                    " get_bukrs
*&---------------------------------------------------------------------*
*&      Form  set_output
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_output.

  CHECK : g_error IS INITIAL.

  PERFORM show_progress     USING 'Preparing screen...' '95'.

  PERFORM init_alv_parm.

  PERFORM alv_events_get    USING:  'P', 'T'.

ENDFORM.                    " set_output

*---------------------------------------------------------------------*
*       FORM top_of_page                                              *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM top_of_page.
  DATA l_text(60).
  REFRESH gt_listheader.

  l_text = 'NAFTA Material Cost Report'.
  PERFORM set_header_line  USING:
          'P' 'H' ''       l_text       ''.

  IF p_2 EQ true.
    PERFORM set_header_line  USING:
            'S' 'S' 'FSC'    s_artnr-low  s_artnr-high,
            'P' 'S' 'Year'   p_bdatj      '',
            'P' 'S' 'Period' s_poper-low  ''.
  ELSE.
    PERFORM set_header_line  USING:
            'S' 'S' 'FSC'    s_artnr-low  '',
            'P' 'S' 'Year'   p_bdatj      '',
            'D' 'S' 'Period' s_poper-low  s_poper-high,
            'P' 'S' 'Version' p_verid ''.
  ENDIF.
  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
       EXPORTING
            it_list_commentary = gt_listheader.

ENDFORM.                    "top_of_page

*---------------------------------------------------------------------*
*       FORM PF_STATUS_SET
*---------------------------------------------------------------------*
FORM pf_status_set USING  ft_extab TYPE slis_t_extab.
  SET PF-STATUS '100'." excluding 'SAVE'.
ENDFORM.                    "PF_STATUS_SET
*---------------------------------------------------------------------*
*       FORM USER_COMMAND                                             *
*---------------------------------------------------------------------*
FORM user_command USING fp_ucomm LIKE sy-ucomm
                        fs       TYPE slis_selfield.
  CLEAR : g_error.

  CASE fp_ucomm.
    WHEN 'SAVE'.
      CHECK g_error NE true.

  ENDCASE.

ENDFORM.                    "USER_COMMAND

*&---------------------------------------------------------------------*
*&      Form  show_progress
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1894   text
*      -->P_1895   text
*----------------------------------------------------------------------*
FORM show_progress USING    pf_text
                            value(pf_val).

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
       EXPORTING
            percentage = pf_val
            text       = pf_text.

ENDFORM.                    " SHOW_PROGRESS
*&---------------------------------------------------------------------*
*&      Form  init_alv_parm
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM init_alv_parm.

  __cls   :  gt_fieldcat, gt_sort, gt_events, gt_listheader,
             gt_sp_group.

  CLEAR   :  gs_layout.

  gs_layout-colwidth_optimize = 'X'.

*   Set variant
  gv_repid = gs_variant-report = sy-repid.
  gs_variant-variant = p_vari.

ENDFORM.                    " INIT_ALV_PARM
*&---------------------------------------------------------------------*
*&      Form  fieldcat_init
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_FIELDCAT[]  text
*----------------------------------------------------------------------*
FORM fieldcat_init_2 USING ft_fieldcat TYPE slis_t_fieldcat_alv .

  DATA: l_pos TYPE i.

  __cls : ft_fieldcat, it_lvc_cat.

  DEFINE __catalog.
    l_pos = l_pos + 1.
    clear gs_fieldcat.
    gs_fieldcat-col_pos       = l_pos.
    gs_fieldcat-key           = &1.
    gs_fieldcat-fieldname     = &2.
    gs_fieldcat-seltext_m     = &3.        " Column heading
    gs_fieldcat-outputlen     = &4.        " Column width
    gs_fieldcat-datatype      = &5.        " Data type
    gs_fieldcat-emphasize     = &6.
    gs_fieldcat-cfieldname    = &7.
    gs_fieldcat-no_zero       = &8.
    if &9 eq space.
      gs_fieldcat-seltext_l     = &3.
    else.
      gs_fieldcat-seltext_l     = &9.
    endif.
    append gs_fieldcat to  ft_fieldcat.

  END-OF-DEFINITION.

  DATA : $ix(3) TYPE n,
         $mtxt(20),
         $mtxt_long(20),
  $mtxt_long_long(40).

  __catalog :
    'X'  'COMPN'    'Component'         18  'CHAR' '' '' '' '',
    'X'  'MAKTG'    'Description'       40  'CHAR' '' '' '' '',
    'X'  'KSTAR'    'Cst.Elem'          10  'CHAR' '' '' '' '',
    'X'  'MTART'    'M.Grp'              5  'CHAR' '' '' '' '',
    'X'  'POPER'    'Mon'                3  'NUMC' '' '' '' '',
    ' '  'LIFNR'    'Vendor'             4  'CHAR' '' '' '' '',
    ' '  'MEEHT'    'UoM'                3  'CHAR' '' '' '' '',
    ' '  'PEINH'    'PrU'                5  'DEC'  '' '' '' '',
    ' '  'GPREIS'   'U/P'               15  'CURR'  '' '' '' ''.

  LOOP AT gt_fsc.
    $ix = sy-tabix.
    IF gt_fsc-artnr+13(1) EQ space.

      CONCATENATE : 'F_TOTAL' $ix INTO $mtxt,
                  gt_fsc-artnr+6 '-' gt_fsc-verid '($)' INTO $mtxt_long,
                     gt_fsc-artnr '-' gt_fsc-verid '(ck11 $)'
                                                   INTO $mtxt_long_long.
      __catalog ' '  $mtxt $mtxt_long  30  'CURR'
                       'C40' '' '' $mtxt_long_long.
      CONCATENATE : 'F_ODAMT' $ix INTO $mtxt,
                  gt_fsc-artnr+6 '-' gt_fsc-verid '(o)' INTO $mtxt_long,
                     gt_fsc-artnr '-' gt_fsc-verid '(os and d)'
                            INTO $mtxt_long_long.

      __catalog ' '  $mtxt $mtxt_long  30  'CURR'
                        'C50' '' '' $mtxt_long_long.
      CONCATENATE : 'F_REQQT' $ix INTO $mtxt,
                  gt_fsc-artnr+6 '-' gt_fsc-verid '(Q)' INTO $mtxt_long,
                  gt_fsc-artnr '-' gt_fsc-verid '(ck11 Qty)'
                 INTO $mtxt_long_long.

      __catalog ' '  $mtxt $mtxt_long  30  'QUAN'
                        'C30' '' '' $mtxt_long_long.
    ELSE.
      CONCATENATE : 'F_TOTAL' $ix INTO $mtxt,
                  gt_fsc-artnr+5 '-' gt_fsc-verid '($)' INTO $mtxt_long,
                  gt_fsc-artnr '-' gt_fsc-verid '(ck11 $)'
                 INTO $mtxt_long_long.
      __catalog ' '  $mtxt $mtxt_long  30  'CURR'
                        'C40' '' '' $mtxt_long_long.

      CONCATENATE : 'F_REQQT' $ix INTO $mtxt,
                  gt_fsc-artnr+5 '-' gt_fsc-verid '(Q)' INTO $mtxt_long,
                  gt_fsc-artnr '-' gt_fsc-verid '(ck11 Qty)'
                 INTO $mtxt_long_long.
      __catalog ' '  $mtxt $mtxt_long  30  'QUAN'
                        'C30' '' '' $mtxt_long_long.
      CONCATENATE : 'F_ODAMT' $ix INTO $mtxt,
                  gt_fsc-artnr+5 '-' gt_fsc-verid '(o)' INTO $mtxt_long,
                  gt_fsc-artnr '-' gt_fsc-verid '(os and d)'
                INTO $mtxt_long_long.
      __catalog ' '  $mtxt $mtxt_long  30  'CURR'
                        'C50' '' '' $mtxt_long_long.
    ENDIF.
  ENDLOOP.

  PERFORM change_fieldcat USING ft_fieldcat[] .

  LOOP AT ft_fieldcat INTO gs_fieldcat.
    is_lvc_cat-fieldname = gs_fieldcat-fieldname.
    is_lvc_cat-ref_field = gs_fieldcat-ref_fieldname.
    is_lvc_cat-ref_table = gs_fieldcat-ref_tabname.
    is_lvc_cat-scrtext_s = gs_fieldcat-seltext_m.
    is_lvc_cat-scrtext_l = gs_fieldcat-seltext_l.
    APPEND is_lvc_cat TO it_lvc_cat.
  ENDLOOP.

* Create a new Table
  CALL METHOD cl_alv_table_create=>create_dynamic_table
    EXPORTING
      it_fieldcatalog = it_lvc_cat
    IMPORTING
      ep_table        = new_table.

* Create a new Line with the same structure of the table.
  ASSIGN new_table->* TO <l_table>.
  CREATE DATA new_line LIKE LINE OF <l_table>.
  ASSIGN new_line->* TO <l_line>.

ENDFORM.                    " fieldcat_init
*&---------------------------------------------------------------------*
*&      Form  sort_build
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_SORT[]  text
*----------------------------------------------------------------------*
FORM sort_build_2 USING    ft_sort TYPE slis_t_sortinfo_alv.

  DEFINE sort_tab.
    clear gs_sort.
    gs_sort-fieldname = &1.
    gs_sort-spos      = &2.
    gs_sort-up        = &3.
    gs_sort-group     = &4.
    gs_sort-comp      = &5.
    append gs_sort to ft_sort.
  END-OF-DEFINITION.

  sort_tab :
     'COMPN'        ' ' 'X' 'X' 'X',
     'MAKTG'        ' ' 'X' 'X' 'X',
     'KSTAR'        ' ' 'X' 'X' 'X',
     'MTART'        ' ' 'X' 'X' 'X',
     'LIFNR'        ' ' 'X' 'X' 'X'.

ENDFORM.                    " SORT_BUILD

*---------------------------------------------------------------------*
*       FORM sort_build                                               *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  FT_SORT                                                       *
*---------------------------------------------------------------------*
FORM sort_build USING    ft_sort TYPE slis_t_sortinfo_alv.

  DEFINE sort_tab.
    clear gs_sort.
    gs_sort-fieldname = &1.
    gs_sort-spos      = &2.
    gs_sort-up        = &3.
    gs_sort-group     = &4.
    gs_sort-comp      = &5.
    append gs_sort to ft_sort.
  END-OF-DEFINITION.

  sort_tab :
   'ARTNR'        ' ' 'X' 'X' 'X',
   'COMPN'        ' ' 'X' 'X' 'X',
   'MAKTG'        ' ' 'X' 'X' 'X',
   'KSTAR'        ' ' 'X' 'X' 'X',
   'MTART'        ' ' 'X' 'X' 'X',
   'LIFNR'        ' ' 'X' 'X' 'X'.

ENDFORM.                    " SORT_BUILD

*&---------------------------------------------------------------------*
*&      Form  change_fieldcat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_FT_FIELDCAT[]  text
*----------------------------------------------------------------------*
FORM change_fieldcat USING    pt_fieldcat TYPE slis_t_fieldcat_alv.

  LOOP AT pt_fieldcat INTO gs_fieldcat.

    IF gs_fieldcat-fieldname NP 'F_*'.
      gs_fieldcat-ref_tabname = 'ZSCO_NAFTA_CK11'.
      gs_fieldcat-ref_fieldname = gs_fieldcat-fieldname.
    ENDIF.
    IF gs_fieldcat-fieldname EQ 'POPER' AND p_2 EQ true.
      gs_fieldcat-no_out = 'X'.
    ENDIF.
    IF gs_fieldcat-fieldname CP 'F_REQQT*' AND p_qty NE true.
      gs_fieldcat-no_out = 'X'.
    ENDIF.

    IF gs_fieldcat-fieldname CP 'F_ODAMT*' AND p_osd NE true.
      gs_fieldcat-no_out = 'X'.
    ENDIF.

    IF gs_fieldcat-fieldname CP 'F_TOTAL*' AND p_amt NE true.
      gs_fieldcat-no_out = 'X'.
    ENDIF.

    IF p_1 EQ true.
      IF gs_fieldcat-fieldname CP 'ARTNR'.
        READ TABLE gt_fsc INDEX 2.
        IF sy-subrc NE 0.
          gs_fieldcat-no_out = 'X'.
        ENDIF.
      ENDIF.
    ENDIF.
    MODIFY pt_fieldcat FROM gs_fieldcat.
  ENDLOOP.

ENDFORM.                    " CHANGE_FIELDCAT

*&---------------------------------------------------------------------*
*&      Form  initialize
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM initialize.
  PERFORM : get_bukrs, get_plant.
ENDFORM.                    " initialize
*&---------------------------------------------------------------------*
*&      Form  get_row_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_row_data_one_p.

  __cls gt_out.

  SELECT *
  INTO CORRESPONDING FIELDS OF TABLE it_row_tab
  FROM ztco_nafta
  WHERE kokrs = p_kokrs
    AND bdatj EQ p_bdatj
    AND poper EQ s_poper-low
    AND artnr IN s_artnr
    AND werks IN gr_bwkey
    AND compn IN s_compn
    AND kstar IN s_kstar.

  IF sy-subrc NE 0.
    MESSAGE s000 WITH 'No data has been found!'.
    g_error = true.
  ENDIF.

ENDFORM.                    " get_row_data
*&---------------------------------------------------------------------*
*&      Form  catego_by_fsc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM catego_by_fsc.

  DATA : $ix(3) TYPE n,
         $mtxt1(20),
         $mtxt2(20),
         $mtxt3(20),
         $f_ix TYPE i,
         ff1(30),
         ff2(30).

  __define_not_important.
  DESCRIBE TABLE gt_fsc LINES total_doc_cnt.
  $total_cnt = total_doc_cnt.

  LOOP AT gt_fsc.

    ADD 1 TO current_doc_cnt.
    $current_cnt = current_doc_cnt.
    CONCATENATE gt_fsc-artnr ':' $current_cnt '/' $total_cnt
    INTO $text.
    CONDENSE $text.
    percentage = current_doc_cnt / total_doc_cnt * 100.
    PERFORM show_progress USING $text percentage.

    $ix = sy-tabix.
    READ TABLE it_row_tab WITH KEY artnr = gt_fsc-artnr
                                   verid = gt_fsc-verid
                                   BINARY SEARCH.
    CHECK sy-subrc EQ 0.

    $f_ix = sy-tabix.
    LOOP AT it_row_tab FROM $f_ix.
      IF it_row_tab-artnr NE gt_fsc-artnr
              OR it_row_tab-verid NE gt_fsc-verid.
        EXIT.
      ENDIF.

      ff1 = 'COMPN'.
      ff2 = 'POPER'.

      CLEAR <l_line> .
      READ TABLE <l_table> INTO <l_line>
            WITH KEY (ff1) = it_row_tab-compn
                     (ff2) = it_row_tab-poper.

      IF sy-subrc EQ 0.

        PERFORM move_field_value USING : 'KSTAR'   '' it_row_tab-kstar,
                                         'MTART'   '' it_row_tab-mtart,
                                         'LIFNR'   '' it_row_tab-lifnr,
                                         'MEEHT'   '' it_row_tab-meeht,
                                         'PEINH'   '' it_row_tab-peinh,
                                         'GPREIS'  '' it_row_tab-gpreis.
        IF p_qty EQ true.
          PERFORM move_field_value USING
                            'F_REQQT' $ix it_row_tab-reqqt.
        ENDIF.
        IF p_amt EQ true.
          PERFORM move_field_value USING
                            'F_TOTAL' $ix it_row_tab-total.
        ENDIF.
        IF p_osd EQ true.
          PERFORM move_field_value USING
                            'F_ODAMT' $ix it_row_tab-osndamt.
        ENDIF.

        CONCATENATE : 'F_TOTAL' $ix INTO $mtxt1,
                      'F_ODAMT' $ix INTO $mtxt2,
                      'F_REQQT' $ix INTO $mtxt3.

        MODIFY <l_table>  FROM <l_line> INDEX sy-tabix
                  TRANSPORTING ($mtxt1) ($mtxt2) ($mtxt3).

      ELSE.

        PERFORM move_field_value USING : ff1      '' it_row_tab-compn,
                                         ff2      '' it_row_tab-poper,
                                         'KSTAR'  '' it_row_tab-kstar,
                                         'MTART'  '' it_row_tab-mtart,
                                         'LIFNR'  '' it_row_tab-lifnr,
                                         'MEEHT'  '' it_row_tab-meeht,
                                         'PEINH'  '' it_row_tab-peinh,
                                         'GPREIS' '' it_row_tab-gpreis.
        IF p_qty EQ true.
          PERFORM move_field_value USING
                  'F_REQQT' $ix it_row_tab-reqqt.
        ENDIF.
        IF p_amt EQ true.
          PERFORM move_field_value USING
                  'F_TOTAL' $ix it_row_tab-total.
        ENDIF.
        IF p_osd EQ true.
          PERFORM move_field_value USING
                  'F_ODAMT' $ix it_row_tab-osndamt.
        ENDIF.

        APPEND <l_line> TO <l_table>.

      ENDIF.
    ENDLOOP.

  ENDLOOP.

ENDFORM.                    " catego_by_fsc
*&---------------------------------------------------------------------*
*&      Form  group_uniq_fsc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM group_uniq_fsc.
  __cls gt_fsc.

  SORT it_row_tab BY artnr verid kokrs bdatj poper .

  LOOP AT it_row_tab.
    AT NEW verid.
      gt_fsc-artnr = it_row_tab-artnr.
      gt_fsc-verid = it_row_tab-verid.
      COLLECT gt_fsc.
    ENDAT.
  ENDLOOP.

  SORT gt_fsc.

ENDFORM.                    " group_uniq_fsc
*&---------------------------------------------------------------------*
*&      Form  display_alv
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_alv.

  g_program = sy-repid.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
       EXPORTING
            i_callback_program       = g_program
            i_callback_pf_status_set = 'PF_STATUS_SET'
            i_callback_user_command  = 'USER_COMMAND'
            is_layout                = gs_layout
            it_fieldcat              = gt_fieldcat
            it_special_groups        = gt_sp_group
            it_sort                  = gt_sort
            i_save                   = g_save
            is_variant               = gs_variant
            it_events                = gt_events
       TABLES
            t_outtab                 = <l_table>
       EXCEPTIONS
            program_error            = 1
            OTHERS                   = 2.

** refresh
  IF gs_exit_caused_by_user CS 'X'.
    SET SCREEN 0.
  ENDIF.

  .
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " display_alv
*&---------------------------------------------------------------------*
*&      Form  move_field_value
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1515   text
*      -->P_$IX  text
*      -->P_IT_ROW_TAB_LIFNR  text
*----------------------------------------------------------------------*
FORM move_field_value USING    p_f_name
                               p_ix
                               p_value.
  DATA $mtxt1(20).

  CONCATENATE p_f_name p_ix INTO $mtxt1.
  ASSIGN COMPONENT $mtxt1 OF STRUCTURE <l_line> TO <l_field>.
  <l_field> = p_value.

ENDFORM.                    " move_field_value
*&---------------------------------------------------------------------*
*&      Form  get_row_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_row_data.
  __cls gt_out.

  IF s_poper[] IS INITIAL.
    s_poper = 'IBT'.
    s_poper-low = '001'.
    s_poper-high = '012'.
    APPEND s_poper.
  ENDIF.

  SELECT *
  INTO CORRESPONDING FIELDS OF TABLE it_row_tab
  FROM ztco_nafta
  WHERE kokrs = p_kokrs
    AND bdatj EQ p_bdatj
    AND poper IN s_poper
    AND artnr IN s_artnr
    AND verid EQ p_verid
    AND werks IN gr_bwkey
    AND compn IN s_compn
    AND kstar IN s_kstar.
  IF sy-subrc NE 0.
    MESSAGE s000 WITH 'No data has been found!'.
    g_error = true.
  ENDIF.

ENDFORM.                    " get_row_data
*&---------------------------------------------------------------------*
*&      Form  fieldcat_init
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_FIELDCAT[]  text
*----------------------------------------------------------------------*
FORM fieldcat_init USING ft_fieldcat TYPE slis_t_fieldcat_alv .

  DATA: l_pos TYPE i.

  __cls : ft_fieldcat, it_lvc_cat.

  DEFINE __catalog.
    l_pos = l_pos + 1.
    clear gs_fieldcat.
    gs_fieldcat-col_pos       = l_pos.
    gs_fieldcat-key           = &1.
    gs_fieldcat-fieldname     = &2.
    gs_fieldcat-seltext_m     = &3.        " Column heading
    gs_fieldcat-outputlen     = &4.        " Column width
    gs_fieldcat-datatype      = &5.        " Data type
    gs_fieldcat-emphasize     = &6.
    gs_fieldcat-cfieldname    = &7.
    gs_fieldcat-no_zero       = &8.
    if &9 eq space.
      gs_fieldcat-seltext_l     = &3.
    else.
      gs_fieldcat-seltext_l     = &9.
    endif.
    append gs_fieldcat to  ft_fieldcat.

  END-OF-DEFINITION.

  DATA : $ix(3) TYPE n,
         $mtxt(20),
         $mtxt_long(20),
  $mtxt_long_long(40).

  __catalog :
  'X'  'ARTNR'    'FSC'         18  'CHAR' '' '' '' '',
  'X'  'COMPN'    'Component'         18  'CHAR' '' '' '' '',
  'X'  'MAKTG'    'Description'       40  'CHAR' '' '' '' '',
  'X'  'KSTAR'    'Cst.Elem'          10  'CHAR' '' '' '' '',
  'X'  'MTART'    'M.Grp'              5  'CHAR' '' '' '' '',
  ' '  'LIFNR'    'Vendor'             4  'CHAR' '' '' '' '',
  ' '  'MEEHT'    'UoM'                3  'CHAR' '' '' '' '',
  ' '  'PEINH'    'PrU'                5  'DEC'  '' '' '' ''.

  DO 12 TIMES.
    $ix = sy-index.
    CHECK $ix IN s_poper.

    CONCATENATE : 'F_TOTAL' $ix INTO $mtxt,
                  $ix+1 '($)' INTO $mtxt_long,
                  $ix+1 '(ck11 $)' INTO $mtxt_long_long.
    __catalog ' '  $mtxt $mtxt_long  30  'CURR'
                      'C40' '' '' $mtxt_long_long.
    CONCATENATE : 'F_ODAMT' $ix INTO $mtxt,
                  $ix+1 '(o)' INTO $mtxt_long,
                  $ix+1 '(os and d)' INTO $mtxt_long_long.
    __catalog ' '  $mtxt $mtxt_long  30  'CURR'
                      'C50' '' '' $mtxt_long_long.
    CONCATENATE : 'F_REQQT' $ix INTO $mtxt,
                  $ix+1 '(Q)' INTO $mtxt_long,
                  $ix+1 '(ck11 Qty)' INTO $mtxt_long_long.
    __catalog ' '  $mtxt $mtxt_long  30  'QUAN'
                      'C30' '' '' $mtxt_long_long.
  ENDDO.

  PERFORM change_fieldcat USING ft_fieldcat[] .

  LOOP AT ft_fieldcat INTO gs_fieldcat.
    is_lvc_cat-fieldname = gs_fieldcat-fieldname.
    is_lvc_cat-ref_field = gs_fieldcat-ref_fieldname.
    is_lvc_cat-ref_table = gs_fieldcat-ref_tabname.
    is_lvc_cat-scrtext_s = gs_fieldcat-seltext_m.
    is_lvc_cat-scrtext_l = gs_fieldcat-seltext_l.
    APPEND is_lvc_cat TO it_lvc_cat.
  ENDLOOP.

* Create a new Table
  CALL METHOD cl_alv_table_create=>create_dynamic_table
    EXPORTING
      it_fieldcatalog = it_lvc_cat
    IMPORTING
      ep_table        = new_table.

* Create a new Line with the same structure of the table.
  ASSIGN new_table->* TO <l_table>.
  CREATE DATA new_line LIKE LINE OF <l_table>.
  ASSIGN new_line->* TO <l_line>.

ENDFORM.                    " fieldcat_init
*&---------------------------------------------------------------------*
*&      Form  modify_screen
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM modify_screen.

  LOOP AT SCREEN.
    CASE 'X'.
      WHEN p_1.
        IF screen-name = 'S_POPER-HIGH'
        OR screen-name = '%_S_POPER_%_APP_%-VALU_PUSH'.
          screen-input = 1.
          screen-invisible = 0.
        ENDIF.
        IF screen-name = 'S_ARTNR-HIGH'
        OR screen-name = '%_S_ARTNR_%_APP_%-VALU_PUSH'.
          screen-input = 0.
          screen-invisible = 1.
          CLEAR s_artnr-high.
        ENDIF.
        IF screen-name = 'P_VERID'.
          screen-input = 1.
          screen-invisible = 0.
        ENDIF.
        IF screen-name = '%_P_VERID_%_APP_%-TEXT'.
          screen-invisible = 0.
        ENDIF.
      WHEN p_2.
        IF screen-name = 'S_POPER-HIGH'
        OR screen-name = '%_S_POPER_%_APP_%-VALU_PUSH'.
          screen-input = 0.
          screen-invisible = 1.
          CLEAR s_poper-high.
        ENDIF.
        IF screen-name = 'S_ARTNR-HIGH'
        OR screen-name = '%_S_ARTNR_%_APP_%-VALU_PUSH'.
          screen-input = 1.
          screen-invisible = 0.
        ENDIF.
        IF screen-name = 'P_VERID'.
          screen-input = 0.
          screen-invisible = 1.
        ENDIF.
        IF screen-name = '%_P_VERID_%_APP_%-TEXT'.
          screen-invisible = 1.
        ENDIF.
    ENDCASE.
    MODIFY SCREEN.
  ENDLOOP.

ENDFORM.                    " modify_screen
*&---------------------------------------------------------------------*
*&      Form  catego_by_poper
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM catego_by_poper.

  DATA : $ix(3) TYPE n,
         $mtxt1(20),
         $mtxt2(20),
         $mtxt3(20),
         $f_ix TYPE i,
         ff1(30),
         ff2(30).

  SORT it_row_tab BY artnr poper verid.

  __define_not_important.
  total_doc_cnt = 12.
  $total_cnt = total_doc_cnt.

  LOOP AT gt_fsc.
    DO 12 TIMES.
      $ix = sy-index.
      CHECK $ix IN s_poper.

      ADD 1 TO current_doc_cnt.
      $current_cnt = current_doc_cnt.
      CONCATENATE gt_fsc-artnr ':' $current_cnt '/' $total_cnt
      INTO $text.
      CONDENSE $text.
      percentage = current_doc_cnt / total_doc_cnt * 100.
      PERFORM show_progress USING $text percentage.

      READ TABLE it_row_tab WITH KEY artnr = gt_fsc-artnr
                                     poper = $ix
                                     BINARY SEARCH.
      CHECK sy-subrc EQ 0.

      $f_ix = sy-tabix.
      LOOP AT it_row_tab FROM $f_ix.

        IF it_row_tab-artnr NE gt_fsc-artnr OR it_row_tab-poper NE $ix.
          EXIT.
        ENDIF.

        ff1 = 'ARTNR'.
        ff2 = 'COMPN'.
        CLEAR <l_line> .
        READ TABLE <l_table> INTO <l_line>
              WITH KEY (ff1) = it_row_tab-artnr
                       (ff2) = it_row_tab-compn.

        IF sy-subrc EQ 0.

         PERFORM move_field_value USING :  ff1      '' it_row_tab-artnr,
                                          'KSTAR'   '' it_row_tab-kstar,
                                          'MTART'   '' it_row_tab-mtart,
                                          'LIFNR'   '' it_row_tab-lifnr,
                                          'MEEHT'   '' it_row_tab-meeht,
                                          'PEINH'   '' it_row_tab-peinh.
          IF p_qty EQ true.
        PERFORM move_field_value USING   'F_REQQT' $ix it_row_tab-reqqt.
          ENDIF.
          IF p_amt EQ true.
        PERFORM move_field_value USING   'F_TOTAL' $ix it_row_tab-total.
          ENDIF.
          IF p_osd EQ true.
        PERFORM move_field_value USING 'F_ODAMT' $ix it_row_tab-osndamt.
          ENDIF.

          CONCATENATE : 'F_TOTAL' $ix INTO $mtxt1,
                        'F_ODAMT' $ix INTO $mtxt2,
                        'F_REQQT' $ix INTO $mtxt3.

          MODIFY <l_table>  FROM <l_line> INDEX sy-tabix
                    TRANSPORTING ($mtxt1) ($mtxt2) ($mtxt3).

        ELSE.

         PERFORM move_field_value USING :  ff1      '' it_row_tab-artnr,
                                          ff2       '' it_row_tab-compn,
                                          'KSTAR'   '' it_row_tab-kstar,
                                          'MTART'   '' it_row_tab-mtart,
                                          'LIFNR'   '' it_row_tab-lifnr,
                                          'MEEHT'   '' it_row_tab-meeht,
                                          'PEINH'   '' it_row_tab-peinh.
          IF p_qty EQ true.
        PERFORM move_field_value USING   'F_REQQT' $ix it_row_tab-reqqt.
          ENDIF.
          IF p_amt EQ true.
        PERFORM move_field_value USING   'F_TOTAL' $ix it_row_tab-total.
          ENDIF.
          IF p_osd EQ true.
        PERFORM move_field_value USING 'F_ODAMT' $ix it_row_tab-osndamt.
          ENDIF.

          APPEND <l_line> TO <l_table>.

        ENDIF.
      ENDLOOP.

    ENDDO.
  ENDLOOP.

ENDFORM.                    " catego_by_poper
*&---------------------------------------------------------------------*
*&      Form  get_txt
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_txt.

  __cls : gt_mat_compn, gt_mat_text.

  ASSIGN COMPONENT 'COMPN' OF STRUCTURE <l_line> TO <l_field>.
  LOOP AT <l_table> INTO <l_line>.
    gt_mat_compn = <l_field>.
    APPEND gt_mat_compn.
  ENDLOOP.

  SORT gt_mat_compn.
  DELETE ADJACENT DUPLICATES FROM gt_mat_compn.

  SELECT matnr maktg INTO TABLE gt_mat_text
        FROM makt
        FOR ALL ENTRIES IN gt_mat_compn
                  WHERE matnr EQ gt_mat_compn-compn
                    AND spras EQ sy-langu.

  SORT gt_mat_text BY compn.

ENDFORM.                    " get_txt
*&---------------------------------------------------------------------*
*&      Form  fill_text
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_text.
  DATA $ix LIKE sy-tabix.
  DATA $matnr(18).

  FIELD-SYMBOLS: <l_field_t> TYPE ANY.

  LOOP AT <l_table> INTO <l_line>.
    gt_mat_compn = <l_field>.
    APPEND gt_mat_compn.
  ENDLOOP.
  DATA ff(20) VALUE 'MAKTG'.

  ASSIGN COMPONENT : 'COMPN' OF STRUCTURE <l_line> TO <l_field>,
                     ff OF STRUCTURE <l_line> TO <l_field_t>.

  LOOP AT <l_table> INTO <l_line>.
    $ix = sy-tabix.
    READ TABLE gt_mat_text  WITH KEY compn = <l_field>
                            BINARY SEARCH.
    IF sy-subrc EQ 0.
      <l_field_t> = gt_mat_text-maktg.
      MODIFY <l_table> FROM <l_line> INDEX $ix TRANSPORTING (ff).
    ELSE.
      IF <l_field>(2) EQ 'EM'.
        CONCATENATE <l_field> '%' INTO $matnr.
        SELECT SINGLE maktg INTO <l_field_t>
              FROM makt
                        WHERE matnr LIKE $matnr
                          AND spras EQ sy-langu.
        MODIFY <l_table> FROM <l_line> INDEX $ix TRANSPORTING (ff).
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " fill_text
