*----------------------------------------------------------------------
* Program ID        : ZACOU109
* Title             : [CO] Cost Detail Report
* Created on        : 10/05/2006
* Created by        : Michelle Jeong
* Specifications By : Andy Choi
* Description       : Display Cost Detail
*----------------------------------------------------------------------
* Date        Developer  Request    Description
* 10/11/2010  Valerian   UD1K949919 Add 'Material Level II', 'Material
*                                   Level I', Description and Category
*                                   to Rep.Layout
* 14/06/2013  T00303   UD1K957369   U1: Apply archiving
*----------------------------------------------------------------------
REPORT zacou109 NO STANDARD PAGE HEADING MESSAGE-ID zmco.

INCLUDE zacoui00.
INCLUDE zacou109_top.

* UD1K941189 - by IG.MOON 8/1/2007 {
TYPE-POOLS:   ccs00,  ccs01,  ckmv0,  ckmv3,  vrm,  ckmd,  ckru0.
TYPES:
   t_ckmllacr_type     TYPE STANDARD TABLE OF ckmllacr
                            WITH KEY kalnr poper bdatj untper curtp.
DATA : gt_sort_moon            TYPE slis_t_sortinfo_alv,
       gs_sort_moon            TYPE slis_sortinfo_alv.

DATA: it_prkeko       LIKE ckmlprkeko OCCURS 0 WITH HEADER LINE.
DATA: it_prkeph       LIKE ckmlprkeph OCCURS 0 WITH HEADER LINE.

DATA : BEGIN OF gt_cc_amt OCCURS 0 ,
        elemt    TYPE kstar,
        dmbtr    TYPE dmbtr,
        dmbtr_f  TYPE dmbtr,
       END OF gt_cc_amt.
* }

*ANDY
DATA: p_year  LIKE keko-bdatj.

*----------------------------------------------------------------------
* Selection-Screen
*----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK b0 WITH FRAME TITLE text-000.
PARAMETERS: p_kokrs LIKE keko-kokrs OBLIGATORY
                                    MEMORY ID cac
                                    MATCHCODE OBJECT fc_kokrs.

* UD1K941566 - by IG.MOON 9/11/2007 {
*            P_KALKA LIKE KEKO-KALKA OBLIGATORY MEMORY ID KKA.
SELECT-OPTIONS :
             s_kalka FOR keko-kalka OBLIGATORY
              MEMORY ID kka.
*             NO-EXTENSION
* }

*           P_YEAR  LIKE KEKO-BDATJ OBLIGATORY MEMORY ID BDTJ.
SELECT-OPTIONS :
            s_poper FOR keko-poper
                        MEMORY ID popr,
*NO-EXTENSION
            s_year  FOR keko-bdatj
                        OBLIGATORY MEMORY ID bdtj.
*NO-EXTENSION


SELECTION-SCREEN SKIP 1.
PARAMETERS:
            p_ver   LIKE ztcou103-ver.

SELECTION-SCREEN BEGIN OF BLOCK b0s WITH FRAME TITLE text-011.
PARAMETERS:
            " Costing Type Compare
            p_ctp  AS CHECKBOX USER-COMMAND ucoc,
            " Period compare - FSC 1EA
            p_mon  AS CHECKBOX USER-COMMAND ucom.
SELECTION-SCREEN END OF BLOCK b0s.

PARAMETERS: p_qty  AS CHECKBOX,   " Quantity Comparison
            p_dlv  AS CHECKBOX,   " Exclude Delivery cost
            p_unit AS CHECKBOX DEFAULT 'X'.
PARAMETERS  p_max  TYPE i OBLIGATORY DEFAULT 12.
SELECTION-SCREEN END OF BLOCK b0.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECT-OPTIONS: s_mtart FOR mara-mtart,
                s_matnr FOR ztcou103-artnr,
                s_upg   FOR ztcou103-upgvc,
                s_compn FOR ztcou103-compn,
                s_kstar FOR ztcou103-kstar.

SELECTION-SCREEN END OF BLOCK b1.

PARAMETERS p_call(1) NO-DISPLAY.
* Layout
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-003.
PARAMETERS p_vari TYPE slis_vari.
SELECTION-SCREEN END OF BLOCK b2.

*- U1 Start
INCLUDE ziarch_comm01.
*- U1 End

*----------------------------------------------------------------------*
* At Selection-Screen
*----------------------------------------------------------------------*
* UD1K941189 - by IG.MOON {
AT SELECTION-SCREEN .
  CASE sscrfields-ucomm.
    WHEN 'UCOM'.
      p_ctp = ''.
      PERFORM modify_screen.
    WHEN 'UCOC'.
      p_mon = ''.
      PERFORM modify_screen.
  ENDCASE.

AT SELECTION-SCREEN OUTPUT.
  IF p_call EQ space.
    PERFORM modify_screen.
  ENDIF.

* }

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_vari.
  PERFORM alv_variant_f4 CHANGING p_vari.

*----------------------------------------------------------------------*
* Start of selection
*----------------------------------------------------------------------*
START-OF-SELECTION.
  READ TABLE s_poper INDEX 1.
  IF s_poper-high IS INITIAL.
    s_poper-high = s_poper-low.
  ENDIF.

  READ TABLE s_year  INDEX 1.
  IF s_year-high IS INITIAL.
    s_year-high = s_year-low.
  ENDIF.
  p_year = s_year-low.

  READ TABLE s_kalka INDEX 1.
  IF s_kalka-high IS INITIAL.
    s_kalka-high = s_kalka-low.
  ENDIF.

*-allow single material comparison
  IF p_ctp = 'X'.
    p_max = 2.
  ENDIF.
  IF p_max > 30.  "maximum
    p_max = 30.
  ENDIF.

  PERFORM get_data.

*----------------------------------------------------------------------*
* End of selection
*----------------------------------------------------------------------*
END-OF-SELECTION.
  PERFORM disp_result.

*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       Get cost detail information
*----------------------------------------------------------------------*
FORM get_data.

  CLEAR g_error.
* Get Material Ranges
  PERFORM get_r_matnr.
  CHECK g_error EQ space.

* Get cost detail information from table ZTCOU103
  PERFORM get_103.

  PERFORM get_vc_component.

* Create Internal Table GT_VC for UPG Desc.
  PERFORM get_upgtxt.

* Create Internal Table GT_102 for Reason
  PERFORM get_gt_102.

* Create Internal Table GT_STD for MAP, STD
  PERFORM get_stprs.

* UD1K941194 - by IG.MOON 8/1/2007 {
  PERFORM get_103_manipulation.
* }

* Create Internal Table GT_OUT for display
  PERFORM get_gt_out.

  SORT gt_out BY upgvc kstar compn.
  DELETE ADJACENT DUPLICATES FROM gt_out.

ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  GET_GT_OUT
*&---------------------------------------------------------------------*
FORM get_gt_out.
  LOOP AT gt_vcit.  "GT_COST.
*    MOVE-CORRESPONDING GT_COST TO GT_OUT.
    MOVE-CORRESPONDING gt_vcit TO gt_out.

    gv_idx = gv_idx + 1.
    gt_out-idx = gv_idx.     " Index

*   Get FSC Usage
    PERFORM get_fsc_usage.

    PERFORM get_mat_level.                                  "UD1K949919

    APPEND gt_out.
    CLEAR gt_out.
  ENDLOOP.

* Apply FSC Usage by component & UPG
  PERFORM apply_fsc_usge.

* Get InfoDetail
  PERFORM get_info_detail.

* Get UPG Desc.
  LOOP AT gt_vc.
    gt_out-upgtxt = gt_vc-upgtxt.
    MODIFY gt_out TRANSPORTING upgtxt
        WHERE upgvc = gt_vc-upgvc.
*       WHERE UPGVC+0(7) = GT_VC-UPGVC+0(7).
  ENDLOOP.

ENDFORM.                    " GET_GT_OUT
*&---------------------------------------------------------------------*
*&      Form  GET_UPGTXT
*&---------------------------------------------------------------------*
*       Get UPG Desc. & Material group, Src
*----------------------------------------------------------------------*
FORM get_upgtxt.
  SORT gt_vc BY upgvc.

  DELETE ADJACENT DUPLICATES FROM gt_vc.

  LOOP AT gt_vc.
    SELECT SINGLE maktg INTO gt_vc-upgtxt
      FROM makt
     WHERE matnr LIKE gt_vc-upgvc.
    IF sy-subrc = 0.
      MODIFY gt_vc.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " GET_UPGTXT
*&---------------------------------------------------------------------*
*&      Form  GET_REASON
*&---------------------------------------------------------------------*
*       Get info detail
*----------------------------------------------------------------------*
*       Get the reason from Table ZTCOU102,
*       if it has no data, retrieve from info-record: Table A018, KONH
*----------------------------------------------------------------------*
FORM get_info_detail.
  DATA: l_idx TYPE sy-index.

  SORT gt_std BY matnr kalnr.
*  SORT GT_STD BY KALNR.

  LOOP AT gt_out.
    l_idx = sy-tabix.

    READ TABLE gt_102 WITH KEY matnr = gt_out-compn BINARY SEARCH.
    IF sy-subrc = 0.
      gt_out-lifnr = gt_102-lifnr.          " Vendor
      gt_out-kzust = gt_102-kzust1.         " Reason

      gt_out-wertn = gt_102-wertn.         " Price
      gt_out-peinh = gt_102-peinh.         " Price Unit
      gt_out-pmeht = gt_102-pmeht.         " Price Qty Unit

    ENDIF.

    READ TABLE gt_std WITH KEY matnr = gt_out-compn
                               kalnr = gt_out-kalnr  BINARY SEARCH.

*    READ TABLE GT_STD WITH KEY KALNR = GT_OUT-KALNR BINARY SEARCH.

    IF sy-subrc = 0.
      gt_out-verpr = gt_std-pvprs.      " Moving avg.price
      gt_out-stprs = gt_std-stprs.      " Standard price
    ENDIF.

    MODIFY gt_out INDEX l_idx TRANSPORTING
        lifnr kzust wertn peinh pmeht verpr stprs.
  ENDLOOP.

ENDFORM.                    " GET_INFO_DETAIL
*&---------------------------------------------------------------------*
*&      Form  GET_FSC_USAGE
*&---------------------------------------------------------------------*
*       Get FSC Usage & Price
*----------------------------------------------------------------------*
FORM get_fsc_usage.
  DATA: l_menge(15)  TYPE p DECIMALS 3,
        l_wertn      TYPE zwertn.

* Get price information from table ZTCOU103
  READ TABLE gt_103 WITH KEY compn = gt_out-compn
                             upgvc = gt_out-upgvc
                      BINARY SEARCH.
  IF sy-subrc <> 0.
    EXIT.
  ENDIF.

  CLEAR: gt_fsc, gt_mat, l_menge, l_wertn.

  LOOP AT gt_103 FROM sy-tabix.
    IF gt_103-compn = gt_out-compn AND
       gt_103-upgvc = gt_out-upgvc.

*   when choose [Exclude Delivery cost]
*   : Info-price ... total price - ( duty  + freight + other)
      IF p_dlv = 'X'.
        gt_103-wertn = gt_103-wertn -
                       ( gt_103-duty + gt_103-frg + gt_103-oth ).
      ENDIF.

      l_menge = l_menge + gt_103-menge.
      l_wertn = l_wertn + gt_103-wertn.

*    Create Internal Table GT_MAT for usage of FSC
      gt_mat-artnr = gt_103-artnr.      " Product
      APPEND gt_mat.

*    Create Internal Table GT_FSC for FSC
      gt_fsc-idx   = gv_idx.            " Index (UPG + COMPN)
      gt_fsc-artnr = gt_103-artnr.      " Product
      gt_fsc-menge = gt_103-menge.      " Qty
      gt_fsc-wertn = gt_103-wertn.      " Price
      COLLECT gt_fsc.

*     MOVE to MAIN TABLE
      MOVE-CORRESPONDING gt_103 TO gt_out.
* UD1K941189 - by IG.MOON 8/1/2007 {
      IF p_mon = 'X'
*           UD1K941566
            OR p_ctp = 'X'.
        gt_out-artnr = gt_103-$artnr.
      ENDIF.
* }
    ELSE.
      EXIT.
    ENDIF.
  ENDLOOP.

  SORT gt_fsc BY idx artnr.

  gt_out-wertt = l_wertn.                " Total Amt
  gt_out-mengt = l_menge.                " Total Qty.

ENDFORM.                    " GET_FSC_USAGE
*&---------------------------------------------------------------------*
*&      Form  GET_GT_102
*&---------------------------------------------------------------------*
*       Create Internal Table GT_102 for Reason
*----------------------------------------------------------------------*
FORM get_gt_102.
  IF NOT gt_vcit[] IS INITIAL.
    CLEAR gt_102.
    REFRESH gt_102.

    SELECT matnr werks lifnr kzust1
           wertn peinh pmeht kalka
           bdatj poper
      INTO TABLE gt_102
      FROM ztcou102
       FOR ALL ENTRIES IN gt_vcit
     WHERE kokrs = p_kokrs
*       AND bdatj = p_year
*       AND poper = s_poper-high
*       AND kalka = s_kalka-low
       AND bdatj IN s_year
       AND poper IN s_poper
       AND kalka IN s_kalka
       AND matnr = gt_vcit-compn.
  ENDIF.

  SORT gt_102 BY matnr kalka bdatj poper.

ENDFORM.                    " GET_GT_102
*&---------------------------------------------------------------------*
*&      Form  GET_STPRS
*&---------------------------------------------------------------------*
*       Get MAP, STD
*----------------------------------------------------------------------*
FORM get_stprs.
* Get Price unit, MAP, STD
  IF NOT gt_vcit[] IS INITIAL.
    CLEAR gt_std.
    REFRESH gt_std.

    SELECT matnr bwkey stprs pvprs b~kalnr
      INTO TABLE gt_std
      FROM ckmlcr AS a
      JOIN ckmlhd AS b
        ON b~kalnr = a~kalnr
       FOR ALL ENTRIES IN gt_vcit
     WHERE a~bdatj = p_year
       AND a~poper = s_poper-high
       AND a~untper = '000'
       AND a~curtp = '10'
       AND b~matnr = gt_vcit-compn.
*       AND B~BWKEY = GT_COST-WERKS.
  ENDIF.

  SORT gt_std BY matnr bwkey.

ENDFORM.                    " GET_STPRS
*&---------------------------------------------------------------------*
*&      Form  DISP_RESULT
*&---------------------------------------------------------------------*
*       Display Cost Detail
*----------------------------------------------------------------------*
FORM disp_result.
  CLEAR: gt_fieldcat, gs_layout, gt_events, gs_variant,
         gt_fieldcat[], gt_events[].

* UD1K941189 - by IG.MOON 8/1/2007 {
  CLEAR gt_sort_moon[].
  IF p_mon EQ 'X'.
    READ TABLE r_matnr INDEX 2.
    IF sy-subrc EQ 0.
      PERFORM build_field_category USING:
      'ARTNR'   'X'  'Product'          18  'CHAR'.
    ENDIF.
  ENDIF.

  IF p_ctp EQ 'X'.
    READ TABLE r_matnr INDEX 2.
    IF sy-subrc EQ 0.
      PERFORM build_field_category USING:
      'ARTNR'   'X'  'Product'          18  'CHAR'.
    ENDIF.
  ENDIF.

* }
  PERFORM build_field_category USING:
    'UPGVC'   'X'  'UPG'          18  'CHAR',
    'UPGTXT'  'X'  'Description'  50  'CHAR',
    'KSTAR'   'X'  'Cost E'       10  'CHAR',
    'COMPN'   'X'  'Component'    18  'CHAR',
    'MAKTG'   'X'  'Descrption'   50  'CHAR',
    'PROFL'   ' '  'Src'          1   'CHAR',
    'EKGRP'   ' '  'Pur.Grp'      3   'CHAR',
    'LIFNR'   ' '  'Vendor'       10  'CHAR',
    'WERTN'   ' '  'Info-Price'   15  'CURR',
    'PEINH'   ' '  'Price unit'   5   'DEC',
    'PMEHT'   ' '  'UoM-PO'       5   'UNIT',
    'KZUST'   ' '  'RSN'          3   'CHAR',
    'VERPR'   ' '  'MAP'          11  'CURR',
    'STPRS'   ' '  'STD'          11  'CURR',
    'MEEHT'   ' '  'UoM'          5   'UNIT',
    'MATNR2'  ' '  'Level II'     18  'CHAR',               "UD1K949919
    'MATNR1'  ' '  'Level I'      18  'CHAR',               "UD1K949919
    'MAKTX'   ' '  'Mat.Description' 40 'CHAR',             "UD1K949919
    'ZCATX'   ' '  'Categ.for MCVS'   8 'CHAR'.             "UD1K949919

  PERFORM modify_gt_fieldcat.

  PERFORM sort_build        USING gt_sort_moon[].

  CLEAR gs_layout.
  gs_layout-zebra             = 'X'.
  gs_layout-colwidth_optimize = 'X'.
  gs_layout-coltab_fieldname  = 'TABCOLOR'.

  PERFORM set_color.
  PERFORM set_comment USING gt_list_top_of_page.
  PERFORM set_events CHANGING gt_events.

  gv_repid = gs_variant-report = sy-repid.

* Set variant
  gs_variant-variant = p_vari.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program      = gv_repid
      i_callback_user_command = gc_user_command
      is_layout               = gs_layout
      it_fieldcat             = gt_fieldcat[]
      it_events               = gt_events[]
      it_sort                 = gt_sort_moon[]
      i_save                  = 'A'
      is_variant              = gs_variant
    TABLES
      t_outtab                = gt_out.

ENDFORM.                    " DISP_RESULT
*&---------------------------------------------------------------------*
*&      Form  MODIFY_GT_FIELDCAT
*&---------------------------------------------------------------------*
FORM modify_gt_fieldcat.
  DATA: n(2), l_field(15).

  CLEAR: n, l_field.

  PERFORM build_field_category USING:
      'WAVG'   'X'  'Avg.Cost'    15  'CURR'.

  IF p_qty = 'X'.
    PERFORM build_field_category USING:
      'MAVG'   'X'  'Avg.Qty'     15  'QUAN'.
  ENDIF.

  LOOP AT gt_fieldcat INTO gs_fieldcat.
    CASE gs_fieldcat-fieldname.
      WHEN 'UPGVC'.
        gs_fieldcat-key = 'X'.
        MODIFY gt_fieldcat FROM gs_fieldcat
               TRANSPORTING key WHERE fieldname = 'UPGVC'.

      WHEN 'PROFL' OR 'KZUST' OR 'EKGRP'.
        gs_fieldcat-just = 'C'.
        MODIFY gt_fieldcat FROM gs_fieldcat TRANSPORTING just
         WHERE fieldname = gs_fieldcat-fieldname.

      WHEN 'STPRS' OR 'VERPR'.
        gs_fieldcat-ref_fieldname = gs_fieldcat-fieldname. "'STPRS'.
        gs_fieldcat-ref_tabname = 'CKMLCR'.

        MODIFY gt_fieldcat FROM gs_fieldcat
               TRANSPORTING ref_fieldname ref_tabname
         WHERE fieldname = gs_fieldcat-fieldname.

      WHEN 'PEINH' OR 'PMEHT'.
        gs_fieldcat-just = 'C'.
        gs_fieldcat-ref_fieldname = gs_fieldcat-fieldname.
        gs_fieldcat-ref_tabname = 'ZTCOU102'.

        MODIFY gt_fieldcat FROM gs_fieldcat
               TRANSPORTING just ref_fieldname ref_tabname
         WHERE fieldname = gs_fieldcat-fieldname.
    ENDCASE.

    IF gs_fieldcat-datatype = 'CURR'.
      gs_fieldcat-just = 'R'.
      MODIFY gt_fieldcat FROM gs_fieldcat TRANSPORTING just
         WHERE datatype = 'CURR'.
    ENDIF.
  ENDLOOP.

  LOOP AT gt_mat." TO gv_fsc_count. "  WHERE cnt < '31'.
    CLEAR l_field.

    n = n + 1.

    IF p_qty = 'X'.
      CONCATENATE 'MENGE' n INTO l_field.
      gs_fieldcat-ref_fieldname = 'MENGE'.
    ELSE.
      CONCATENATE 'WERTN' n INTO l_field.
      gs_fieldcat-ref_fieldname = 'WERTN'.
    ENDIF.

    gs_fieldcat-fieldname    = l_field.
    gs_fieldcat-seltext_l    = gt_mat-artnr.
    gs_fieldcat-reptext_ddic = gt_mat-artnr.
    gs_fieldcat-ddictxt      = 'L'.
    gs_fieldcat-just         = 'R'.
    gs_fieldcat-do_sum       = 'X'.
    gs_fieldcat-outputlen    = 18.
    APPEND gs_fieldcat TO gt_fieldcat.
  ENDLOOP.

ENDFORM.                    " MODIFY_GT_FIELDCAT
*&---------------------------------------------------------------------*
*&      Form  SET_COMMENT
*&---------------------------------------------------------------------*
FORM set_comment USING lt_top_of_page TYPE slis_t_listheader.
  DATA ls_line TYPE slis_listheader.

  CLEAR ls_line.
  ls_line-typ  = 'S'.

  ls_line-key  = 'Controlling Area:'.
  ls_line-info = p_kokrs.
  APPEND ls_line TO lt_top_of_page.

  ls_line-key  = 'Costing Type:'.
  IF p_ctp EQ 'X'.
    CONCATENATE s_kalka-low ' & ' s_kalka-high INTO ls_line-info.
  ELSE.
    ls_line-info = s_kalka-low.
  ENDIF.
  APPEND ls_line TO lt_top_of_page.

  ls_line-key  = 'Year:'.
  ls_line-info = p_year.
  APPEND ls_line TO lt_top_of_page.

*  LS_LINE-KEY  = 'Period:'.
*  LS_LINE-INFO = S_POPER-LOW.
*  APPEND LS_LINE TO LT_TOP_OF_PAGE.

  CLEAR: ls_line-key, ls_line-info.
  APPEND ls_line TO lt_top_of_page.

  IF p_dlv = 'X'.
    ls_line-key = '[Exclude Dlv.cost]'.
    APPEND ls_line TO lt_top_of_page.
  ENDIF.

  IF p_qty = 'X'.
    ls_line-key  = '[Quantity Base]'.
    APPEND ls_line TO lt_top_of_page.
  ENDIF.

* UD1K941189 - by IG.MOON 8/1/2007 {
  IF p_mon = 'X' OR p_ctp = 'X'.
    READ TABLE r_matnr INDEX 2.
    IF sy-subrc NE 0.
      ls_line-key  = 'Product:'.
      READ TABLE r_matnr INDEX 1.
      ls_line-info = r_matnr-low.
      APPEND ls_line TO lt_top_of_page.
    ENDIF.
  ENDIF.
*  }

  CLEAR: ls_line-key, ls_line-info.
  APPEND ls_line TO lt_top_of_page.


*  DATA L_TEXT(60).
*  DATA S_TEXT(60).
*
*  REFRESH GT_LIST_TOP_OF_PAGE.
*
*  L_TEXT = 'Cost Comparison Report.'.
*
*  PERFORM SET_HEADER_LINE USING:
*          'P' 'H' ''                 L_TEXT       '',
*          'P' 'S' 'Controlling Area' P_KOKRS      '',
*          'P' 'S' 'Costing Type'     P_KALKA      '',
*          'P' 'S' 'Year'             P_YEAR      '',
*          'S' 'S' 'Period'           S_POPER      '',
*          'P' 'S' 'Costing Type'     P_KALKA      ''.
*  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
*       EXPORTING
*            IT_LIST_COMMENTARY = GT_LIST_TOP_OF_PAGE.
*
*
ENDFORM.                    " SET_COMMENT
*&---------------------------------------------------------------------*
*&      Form  APPLY_FSC_USGE
*&---------------------------------------------------------------------*
FORM apply_fsc_usge.
  DATA: l_cnt TYPE i,
        l_idx TYPE i,
        n(2),
        l_char(15),
        l_mcnt TYPE i,
        l_wcnt TYPE i.

  FIELD-SYMBOLS: <fs1>,
                 <fs2>.

  CLEAR: gv_mcnt, l_cnt, n, l_char, gv_fsc_count, l_idx, n.

* Adjust FSC count
  SORT gt_mat BY artnr.
  DELETE ADJACENT DUPLICATES FROM gt_mat.
  LOOP AT gt_mat.
    IF sy-tabix > p_max.
      DELETE gt_mat INDEX sy-tabix.
      MESSAGE s000 WITH 'More columns selected'.
    ELSE.
      gt_mat-cnt = sy-tabix.
      MODIFY gt_mat TRANSPORTING cnt.
    ENDIF.
  ENDLOOP.
  DESCRIBE TABLE gt_mat LINES gv_fsc_count.

* Apply FSC, Qty, price by Component & UPG
  LOOP AT gt_out.
    CLEAR: l_mcnt, l_wcnt.
    l_idx = l_idx + 1.

    LOOP AT gt_mat.   " WHERE cnt <= p_max.
      READ TABLE gt_fsc WITH KEY idx = gt_out-idx
                                 artnr = gt_mat-artnr BINARY SEARCH.

      IF sy-subrc = 0.
        n = gt_mat-cnt.

*       Usage
        CONCATENATE 'GT_OUT-MENGE' n INTO l_char.
        ASSIGN (l_char) TO <fs1>.
        <fs1> = gt_fsc-menge.

        IF gt_fsc-menge <> 0.
          l_mcnt = l_mcnt + 1.
        ENDIF.

*       Price
        CONCATENATE 'GT_OUT-WERTN' n INTO l_char.
        ASSIGN (l_char) TO <fs2>.
        <fs2> = gt_fsc-wertn.

        IF gt_fsc-wertn <> 0.
          l_wcnt = l_wcnt + 1.
        ENDIF.
      ENDIF.

      gt_out-mcnt = l_mcnt.
      gt_out-wcnt = l_wcnt.

    ENDLOOP.

*     Avg.Cost : FSC Usage * Info.Price / Total count of FSC ???
    IF gv_fsc_count <> 0.
      gt_out-mavg = gt_out-mengt / gv_fsc_count.
      gt_out-wavg = gt_out-wertt / gv_fsc_count.
    ENDIF.
*FIXME>>>>
*    if l_mcnt <> 0.
*      gt_out-mavg = gt_out-mengt / l_mcnt.
*    endif.
*    if l_wcnt <> 0.
*      gt_out-wavg = gt_out-wertt / l_wcnt.
*    endif.

    MODIFY gt_out INDEX l_idx.

  ENDLOOP.

ENDFORM.                    " APPLY_FSC_USGE
*&---------------------------------------------------------------------*
*&      Form  SET_COLOR
*&---------------------------------------------------------------------*
*       Set Color
*----------------------------------------------------------------------*
FORM set_color.
  DATA: n(2),
        l_fnam(7).

  CLEAR: n, gs_specialcol, gt_specialcol[], gt_out-tabcolor[].

  DO gv_fsc_count TIMES.
    n = n + 1.

    CLEAR l_fnam.
    IF p_qty = 'X'.
      CONCATENATE 'MENGE' n INTO l_fnam.
    ELSE.
      CONCATENATE 'WERTN' n INTO l_fnam.
    ENDIF.

    gs_specialcol-fieldname = l_fnam.
    gs_specialcol-color-col = cl_gui_resources=>list_col_positive.
    gs_specialcol-color-int = 0.
    APPEND gs_specialcol TO gt_specialcol.
  ENDDO.

  gt_out-tabcolor[] = gt_specialcol[].
  MODIFY gt_out TRANSPORTING tabcolor WHERE tabcolor IS INITIAL.

ENDFORM.                    " SET_COLOR
*&---------------------------------------------------------------------*
*&      Form  GET_R_MATNR
*&---------------------------------------------------------------------*
*       Get Material Range
*----------------------------------------------------------------------*
FORM get_r_matnr.
  CLEAR r_matnr.
  REFRESH r_matnr.

* UD1K941189 - commented by IG.MOON 8/1/2007 {
*  IF P_MON = 'X'.
*    p_max = 1.
*  ENDIF.
* }

  IF p_unit = 'X'.
    TYPES: BEGIN OF ty_mat,
             matnr TYPE matnr,
           END OF ty_mat.

    DATA lt_mat TYPE TABLE OF ty_mat WITH HEADER LINE.

    CLEAR lt_mat.
    REFRESH lt_mat.

    SELECT DISTINCT matnr INTO TABLE lt_mat
      FROM ztcou100 UP TO p_max ROWS
      WHERE kokrs = p_kokrs
        AND bdatj = p_year
        AND poper IN s_poper
        AND kalka = s_kalka-low
        AND matnr IN s_matnr.

    IF sy-subrc NE 0.
      MESSAGE s000 WITH 'No registered product was found in ZTCOU100'.
      g_error = 'X'.
      EXIT.
    ENDIF.

    LOOP AT lt_mat.
      r_matnr-low = lt_mat-matnr.
      APPEND r_matnr.
      CLEAR r_matnr.
    ENDLOOP.

    r_matnr-sign   = 'I'.
    r_matnr-option = 'EQ'.

    MODIFY r_matnr TRANSPORTING sign option WHERE sign IS INITIAL.

  ELSE.

    LOOP AT s_matnr.
      MOVE s_matnr TO r_matnr.
      APPEND r_matnr.
      CLEAR r_matnr.
      IF sy-tabix > p_max.
        EXIT.
      ENDIF.
    ENDLOOP.

  ENDIF.

ENDFORM.                    " GET_R_MATNR
*&---------------------------------------------------------------------*
*&      Form  get_103
*&---------------------------------------------------------------------*
FORM get_103.
  CLEAR:   gt_103.
  REFRESH: gt_103.

  IF p_ctp = ' ' AND p_mon = ' '.

    PERFORM get_103_fr_db  USING s_year-low
                                 s_poper-low
                                 s_kalka-low.

*compare...
  ELSEIF p_ctp = 'X'.
    RANGES: w_kalka FOR ztcou103-kalka,
            w_bdatj FOR ztcou103-bdatj,
            w_poper FOR ztcou103-poper.

    READ TABLE s_kalka INDEX 2 INTO w_kalka.
    IF sy-subrc <> 0.
      READ TABLE s_kalka INDEX 1 INTO w_kalka.
    ENDIF.
    IF w_kalka-high IS INITIAL.
      w_kalka-high = w_kalka-low.
    ENDIF.

    READ TABLE s_year  INDEX 2 INTO w_bdatj.
    IF sy-subrc <> 0.
      READ TABLE s_year  INDEX 1 INTO w_bdatj.
    ENDIF.
    IF w_bdatj-high IS INITIAL.
      w_bdatj-high = w_bdatj-low.
    ENDIF.

    READ TABLE s_poper INDEX 2 INTO w_poper.
    IF sy-subrc <> 0.
      READ TABLE s_poper INDEX 1 INTO w_poper.
    ENDIF.
    IF w_poper-high IS INITIAL.
      w_poper-high = w_poper-low.
    ENDIF.

    PERFORM get_103_fr_db  USING s_year-low
                                 s_poper-low
                                 s_kalka-low.
    PERFORM get_103_fr_db  USING w_bdatj-high
                                 w_poper-high
                                 w_kalka-high.

  ELSEIF p_mon = 'X'.

    PERFORM get_103_fr_db_range.

  ENDIF.

ENDFORM.                                                    " get_103
*&---------------------------------------------------------------------*
*&      Form  get_vc_component
*&---------------------------------------------------------------------*
FORM get_vc_component.
  DATA l_idx TYPE sy-tabix.

* Get Cost Roll-Up
  CLEAR:   gt_vc, gt_fsc, gt_mat, gt_out.
  REFRESH: gt_vc, gt_fsc, gt_mat, gt_out.


  LOOP AT gt_103.
    l_idx = sy-tabix.

    IF p_mon = 'X'.
* UD1K941189 - by IG.MOON 8/1/2007 {
      gt_103-$artnr = gt_103-artnr.
* }
      CONCATENATE gt_103-bdatj '.' gt_103-poper INTO gt_103-artnr.
      MODIFY gt_103 INDEX l_idx TRANSPORTING artnr $artnr.
    ENDIF.

* UD1K941566 - by IG.MOON 9/11/2007 {
    IF p_ctp = 'X'.
* }
      gt_103-$artnr = gt_103-artnr.

      CONCATENATE gt_103-kalka '-'  gt_103-bdatj '.' gt_103-poper
             INTO gt_103-artnr.
      MODIFY gt_103 INDEX l_idx TRANSPORTING artnr $artnr.
    ENDIF.

*  FIXME
*   will be BOM as 7 digit
*   CONCATENATE GT_103-UPGVC+0(7) '%' INTO GT_VC-UPGVC.
    gt_vc-upgvc = gt_103-upgvc.

    READ TABLE gt_vc WITH KEY upgvc = gt_vc-upgvc.
    IF sy-subrc <> 0 AND gt_vc-upgvc NE space.
      APPEND gt_vc.
      CLEAR gt_vc.
    ENDIF.

    gt_vcit-upgvc = gt_103-upgvc.
    gt_vcit-compn = gt_103-compn.
    APPEND gt_vcit.
  ENDLOOP.

  SORT gt_vcit BY upgvc compn.
  DELETE ADJACENT DUPLICATES FROM gt_vcit.

  SORT gt_103  BY compn upgvc artnr. "by werks compn upgvc artnr,

ENDFORM.                    " get_vc_component
*&---------------------------------------------------------------------*
*&      Form  MODIFY_SCREEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM modify_screen.

  LOOP AT SCREEN.
    IF p_ctp = 'X'.
      IF screen-name = 'S_KALKA-HIGH'
      OR screen-name = '%_S_KALKA_%_APP_%-VALU_PUSH'.
        screen-input = 1.
        screen-invisible = 0.
      ENDIF.
*-------ANDY
      IF screen-name = 'S_POPER-HIGH' OR screen-name = 'S_YEAR-HIGH'
      OR screen-name = '%_S_POPER_%_APP_%-VALU_PUSH'
      OR screen-name = '%_S_YEAR_%_APP_%-VALU_PUSH'.

        screen-input = 1.
        screen-invisible = 0.
      ENDIF.

    ELSEIF p_mon = 'X'.
      IF screen-name = 'S_YEAR-HIGH'
      OR screen-name = 'S_POPER-HIGH'
      OR screen-name = '%_S_YEAR_%_APP_%-VALU_PUSH'
      OR screen-name = '%_S_POPER_%_APP_%-VALU_PUSH'.
        REFRESH s_kalka.
        CLEAR s_kalka.
        screen-input = 1.
        screen-invisible = 0.
      ENDIF.

      IF screen-name = 'S_KALKA-HIGH'
      OR screen-name = '%_S_KALKA_%_APP_%-VALU_PUSH'.
        REFRESH: s_kalka.
        CLEAR: s_kalka.
        screen-input = 0.
        screen-invisible = 1.
      ENDIF.

    ELSE.
      IF screen-name = 'S_KALKA-HIGH'
      OR screen-name = 'S_YEAR-HIGH'
      OR screen-name = 'S_POPER-HIGH'
      OR screen-name = '%_S_KALKA_%_APP_%-VALU_PUSH'
      OR screen-name = '%_S_POPER_%_APP_%-VALU_PUSH'
      OR screen-name = '%_S_YEAR_%_APP_%-VALU_PUSH'.
        REFRESH: s_poper, s_year, s_poper.
        CLEAR: s_poper, s_year, s_poper.
        screen-input = 0.
        screen-invisible = 1.
      ENDIF.
    ENDIF.


    MODIFY SCREEN.
  ENDLOOP.
ENDFORM.                    " MODIFY_SCREEN
*&---------------------------------------------------------------------*
*&      Form  SORT_BUILD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_SORT[]  text
*----------------------------------------------------------------------*
FORM sort_build USING    ft_sort TYPE slis_t_sortinfo_alv.

  DEFINE sort_tab.
    clear gs_sort_moon.
    gs_sort_moon-fieldname = &1.
    gs_sort_moon-spos      = &2.
    gs_sort_moon-up        = &3.
    gs_sort_moon-group     = &4.
    gs_sort_moon-comp      = &5.
    append gs_sort_moon to ft_sort.
  END-OF-DEFINITION.

  IF p_mon EQ 'X'.
    READ TABLE r_matnr INDEX 2.
    IF sy-subrc EQ 0.
      sort_tab :
           'ARTNR'        ' ' 'X' 'X' 'X'.
    ENDIF.
  ENDIF.

  sort_tab :
             'UPGVC'        ' ' 'X' 'X' 'X',
             'UPGTXT'       ' ' 'X' 'X' 'X',
             'COMPN'        ' ' 'X' 'X' 'X',
             'MAKTG'        ' ' 'X' 'X' 'X'.

ENDFORM.                    " SORT_BUILD
*&---------------------------------------------------------------------*
*&      Form  GET_103_MANIPULATION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_103_manipulation.

  DATA : lt_kalnr TYPE ckmv0_laobj_tbl WITH HEADER LINE,
         it_prkeko_temp      TYPE mlccs_t_prkeko,
         it_prkeph_temp      TYPE mlccs_t_prkeph.

  RANGES :ir_prtyp FOR mlprkeph-prtyp,
          ir_curtp FOR tkel-curtp.

* STORE MIP KALNR & MATNR
  __cls it_kal_mat.

  LOOP AT gt_103 WHERE stkkz EQ 'X'.
    lt_kalnr-kalnr  = gt_103-kalnr .
    COLLECT lt_kalnr.  CLEAR lt_kalnr.

    it_kal_mat-kalnr = gt_103-kalnr.
    it_kal_mat-matnr = gt_103-compn.
    COLLECT it_kal_mat.
  ENDLOOP.

  READ TABLE lt_kalnr INDEX 1.
  CHECK sy-subrc EQ 0.

  SORT lt_kalnr.
  DELETE ADJACENT DUPLICATES FROM lt_kalnr.

  ir_prtyp = 'IEQS'.
  APPEND ir_prtyp.

  ir_prtyp = 'IEQV'.
  APPEND ir_prtyp.

  ir_curtp = 'IEQ10'.
  APPEND ir_curtp.

  DATA : $str(10),
         $code(2),
         $proper(3) TYPE n.

  __cls : it_prkeko,it_prkeph.

  DO 12 TIMES.
    $proper = sy-index.
    CHECK $proper IN s_poper.

    __cls : it_prkeko_temp, it_prkeph_temp.

    CALL FUNCTION 'MLCCS_READ_PR'
      EXPORTING
        i_use_buffer            = space
        i_bdatj_1               = p_year
        i_poper_1               = $proper
      IMPORTING
        et_prkeko               = it_prkeko_temp
        et_prkeph               = it_prkeph_temp
      TABLES
        it_kalnr                = lt_kalnr
        ir_prtyp                = ir_prtyp
        ir_curtp                = ir_curtp
      EXCEPTIONS
        no_data_found           = 1
        input_data_inconsistent = 2
        OTHERS                  = 3.

    APPEND LINES OF :
            it_prkeko_temp TO it_prkeko,
            it_prkeph_temp TO it_prkeph.

  ENDDO.

  DATA : it_tckh3      LIKE tckh3         OCCURS 0 WITH HEADER LINE.

  SELECT * INTO TABLE it_tckh3
      FROM tckh3
        WHERE elehk = 'H1'.

  PERFORM get_mlccs_pr_total TABLES it_tckh3.


ENDFORM.                    " GET_103_MANIPULATION
*&---------------------------------------------------------------------*
*&      Form  get_mlccs_pr_total
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_mlccs_pr_total TABLES it_tckh3 STRUCTURE tckh3.

  DATA : s_amt TYPE dmbtr,
         v_amt TYPE dmbtr.

  __cls gt_cc_amt.

  SORT it_kal_mat BY kalnr.

  LOOP AT it_prkeph WHERE prtyp EQ 'S'
                      AND kkzst EQ ' '.
* for debug
    __cls gt_cc_amt.
    CLEAR s_amt.
    PERFORM do_sum TABLES it_tckh3
                          gt_cc_amt
                 CHANGING s_amt.

    READ TABLE it_kal_mat WITH KEY kalnr = it_prkeph-kalnr
                                                       BINARY SEARCH.
    IF sy-subrc EQ 0.
      DELETE gt_std WHERE matnr EQ it_kal_mat-matnr.
      gt_std-matnr = it_kal_mat-matnr.
      gt_std-kalnr = it_kal_mat-kalnr.
      gt_std-stprs = s_amt.
      APPEND gt_std.
    ENDIF.
  ENDLOOP.

  SORT gt_std BY matnr.

  LOOP AT it_prkeph WHERE prtyp EQ 'V'
                      AND kkzst EQ ' '.
* for debug
    __cls gt_cc_amt.
    CLEAR v_amt.
    PERFORM do_sum TABLES it_tckh3
                          gt_cc_amt
                 CHANGING v_amt.

    READ TABLE it_kal_mat WITH KEY kalnr = it_prkeph-kalnr
                                                       BINARY SEARCH.
    IF sy-subrc EQ 0.
      READ TABLE gt_std WITH KEY matnr = it_kal_mat-matnr
                                                       BINARY SEARCH.
      gt_std-pvprs = v_amt.
      MODIFY gt_std INDEX sy-tabix TRANSPORTING pvprs.
    ENDIF.

  ENDLOOP.


ENDFORM.                    " get_mlccs_pr_total
*&---------------------------------------------------------------------*
*&      Form  do_sum
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM do_sum TABLES  it_tckh3   STRUCTURE tckh3
                    lt_cc_amt  STRUCTURE gt_cc_amt
            CHANGING t_amt.

  FIELD-SYMBOLS: <f_field> .

  DATA : l_cnt(3) TYPE n,
         l_field(25),
         l_amt TYPE p DECIMALS 6.


  DO 40 TIMES.
    l_cnt = l_cnt + 1.

    CONCATENATE 'IT_PRKEPH-KST' l_cnt INTO l_field.
    ASSIGN  (l_field)    TO   <f_field> .
    CLEAR l_amt.
    l_amt = <f_field>.
    CHECK NOT l_amt IS INITIAL.

* Overall value
    CLEAR it_tckh3.
    READ TABLE it_tckh3 WITH KEY el_hv = l_cnt.
    IF sy-subrc   =  0.
      lt_cc_amt-dmbtr   = l_amt.
      IF it_tckh3-elemt BETWEEN '010' AND '050'.
        t_amt = t_amt + lt_cc_amt-dmbtr.
      ENDIF.
*    ELSE.
** Fixed value
*      READ TABLE IT_TCKH3 WITH KEY EL_HF = L_CNT.
*      IF SY-SUBRC   = 0 .
*        LT_CC_AMT-DMBTR_F = L_AMT.
*      ENDIF.
    ENDIF.

    lt_cc_amt-elemt = it_tckh3-elemt.

    COLLECT lt_cc_amt. CLEAR lt_cc_amt.

  ENDDO.

ENDFORM.                    " do_sum

*---------------------------------------------------------------------*
*       FORM USER_COMMAND                                             *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  R_UCOMM                                                       *
*  -->  RS_SELFIELD                                                   *
*---------------------------------------------------------------------*
FORM user_command USING r_ucomm LIKE sy-ucomm
                        rs_selfield TYPE slis_selfield.


  IF r_ucomm = '&IC1'.
    READ TABLE gt_out INDEX rs_selfield-tabindex.
    CHECK sy-subrc EQ 0.
    RANGES lr_matnr FOR ztcou100-matnr.
    lr_matnr-sign = 'I'.
    lr_matnr-option = 'EQ'.
    lr_matnr-low = gt_out-compn.
    APPEND lr_matnr.
    CLEAR lr_matnr.

    RANGES lr_kalka FOR ztcou100-kalka.
    lr_kalka-sign = 'I'.
    lr_kalka-option = 'EQ'.

    IF gt_out-stkkz EQ 'X'.
      lr_kalka-low = 'U1'.
    ELSE.
      PERFORM get_kalka_type USING gt_out-compn
                          CHANGING lr_kalka-low.
    ENDIF.

    CHECK lr_kalka-low NE space.

    APPEND lr_kalka.
    CLEAR lr_kalka.

    SUBMIT zacou109 WITH p_kokrs = p_kokrs
                    WITH s_kalka IN lr_kalka
                    WITH p_ver   = p_ver
*                    WITH p_year  = p_year
                    WITH s_year IN s_year
                    WITH s_poper IN s_poper

                    WITH p_dlv = space
                    WITH p_qty = p_qty
                    WITH p_mon = space
                    WITH s_matnr IN lr_matnr
                    WITH p_unit = p_unit
                    WITH p_call = 'X'
                AND RETURN.

  ENDIF.

ENDFORM.                    "user_command
*&---------------------------------------------------------------------*
*&      Form  get_kalka_type
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_OUT_COMPN  text
*      <--P_LR_KALKA_LOW  text
*----------------------------------------------------------------------*
FORM get_kalka_type USING    p_compn
                    CHANGING p_low.


  SELECT SINGLE * INTO *mast FROM mast
    WHERE matnr EQ p_compn
      AND stlan EQ '2'.

  IF sy-subrc EQ 0.
    p_low = 'M1'. " Module
  ENDIF.
ENDFORM.                    " get_kalka_type
*&---------------------------------------------------------------------*
*&      Form  get_103_fr_db
*&---------------------------------------------------------------------*
FORM get_103_fr_db USING    p_s_year
                            p_s_poper
                            p_s_kalka.

  SELECT a~werks artnr upgvc kstar compn menge
         gpreis wertn duty frg oth a~peinh a~meeht
         splnt bwdat
         b~maktg c~matkl c~profl c~mtart d~ekgrp
         a~poper a~bdatj
         a~stkkz e~kaln1 AS kalnr
         a~kalka a~stkkz
    APPENDING CORRESPONDING FIELDS OF TABLE gt_103
    FROM ztcou103 AS a
    JOIN makt AS b
      ON b~matnr = a~compn
     AND b~spras = sy-langu
    JOIN mara AS c
      ON c~matnr = a~compn
    JOIN marc AS d
      ON d~matnr = a~compn
     AND d~werks = a~werks
    JOIN mbew AS e
      ON e~matnr = a~compn
     AND e~bwkey = a~werks
   WHERE kokrs = p_kokrs
     AND bdatj = p_s_year    "P_YEAR
     AND poper = p_s_poper   "IN S_POPER
     AND kalka = p_s_kalka
     AND ver   = p_ver
     AND artnr IN r_matnr
     AND upgvc IN s_upg
     AND compn IN s_compn
     AND c~mtart IN s_mtart
     AND a~kstar IN s_kstar.

*- U1 Start
  IF p_arch EQ 'X'.
    PERFORM archive_read_ztcou103 USING 'S'.
  ENDIF.
*- U1 End

ENDFORM.                    " get_103_fr_db
*&---------------------------------------------------------------------*
*&      Form  get_103_fr_db
*&---------------------------------------------------------------------*
FORM get_103_fr_db_range.

  SELECT a~werks artnr upgvc kstar compn menge
         gpreis wertn duty frg oth a~peinh a~meeht
         splnt bwdat
         b~maktg c~matkl c~profl c~mtart d~ekgrp
         a~poper a~bdatj
         a~stkkz e~kaln1 AS kalnr
         a~kalka a~stkkz
    APPENDING CORRESPONDING FIELDS OF TABLE gt_103
    FROM ztcou103 AS a
    JOIN makt AS b
      ON b~matnr = a~compn
     AND b~spras = sy-langu
    JOIN mara AS c
      ON c~matnr = a~compn
    JOIN marc AS d
      ON d~matnr = a~compn
     AND d~werks = a~werks
    JOIN mbew AS e
      ON e~matnr = a~compn
     AND e~bwkey = a~werks
   WHERE kokrs = p_kokrs
     AND bdatj IN s_year    "P_YEAR
     AND poper IN s_poper   "IN S_POPER
     AND kalka = s_kalka-low
     AND ver   = p_ver
     AND artnr IN r_matnr
     AND upgvc IN s_upg
     AND compn IN s_compn
     AND c~mtart IN s_mtart
     AND a~kstar IN s_kstar.

*- U1 Start
  IF p_arch EQ 'X'.
    PERFORM archive_read_ztcou103 USING 'R'.
  ENDIF.
*- U1 End

ENDFORM.                    " get_103_fr_db

*&---------------------------------------------------------------------*
*&      Form  get_mat_level
*&---------------------------------------------------------------------*
*       Get material level
*----------------------------------------------------------------------*
FORM get_mat_level.
  SELECT SINGLE ztco_upg~matnr ztco_upg~wrkts
                ztco_upg~maktx ztco_upg~zcatx
    INTO (gt_out-matnr2, gt_out-matnr1, gt_out-maktx, gt_out-zcatx)
    FROM mara JOIN ztco_upg
                ON mara~wrkst = ztco_upg~matnr
   WHERE mara~matnr = gt_out-compn.
ENDFORM.                    " get_mat_level
*&---------------------------------------------------------------------*
*&      Form  ARCHIVE_READ_ZTCOU103
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM archive_read_ztcou103 USING p_x.

  TYPES: BEGIN OF ty_ztcou103,
         kokrs TYPE kokrs,
         bdatj TYPE bdatj,
         kalka TYPE ck_kalka,
         poper TYPE poper,
         artnr TYPE artnr,
         ver   TYPE zver1,
           archivekey TYPE arkey,
           archiveofs TYPE admi_offst.
  TYPES: END OF ty_ztcou103.

  DATA: l_handle    TYPE sytabix,
        lt_ztcou103 TYPE TABLE OF ztcou103 WITH HEADER LINE,
        l_archindex LIKE aind_str2-archindex,
        l_gentab    LIKE aind_str2-gentab.

  DATA: lt_inx_ztcou103 TYPE TABLE OF ty_ztcou103,
        ls_inx_ztcou103 TYPE ty_ztcou103.

  DATA: lt_makt TYPE TABLE OF makt WITH HEADER LINE,
        lt_mara TYPE TABLE OF mara WITH HEADER LINE,
        lt_marc TYPE TABLE OF marc WITH HEADER LINE,
        lt_mbew TYPE TABLE OF mbew WITH HEADER LINE,
        lt_ztcou103_tmp TYPE TABLE OF ty_cost WITH HEADER LINE.

* 1. Input the archive infostructure name
  CLEAR l_archindex.
  l_archindex = 'ZTCOU103_001'.

* 2. Get the structure table using infostructure
  CLEAR l_gentab.
  SELECT SINGLE gentab INTO l_gentab FROM aind_str2
   WHERE archindex = l_archindex.

  CHECK sy-subrc = 0 AND NOT l_gentab IS INITIAL.

* 3. Get the archived data from structure table
  IF p_x = 'S'.    "S: Single parameter
    CLEAR lt_inx_ztcou103[].
    SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_inx_ztcou103
      FROM (l_gentab)
     WHERE kokrs = p_kokrs
       AND bdatj = s_year-low
       AND kalka = s_kalka-low
       AND poper = s_poper-low
       AND artnr IN r_matnr
       AND ver   = p_ver.
  ELSEIF p_x = 'R'.  "R: Range parameter
    CLEAR lt_inx_ztcou103[].
    SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_inx_ztcou103
      FROM (l_gentab)
     WHERE kokrs = p_kokrs
       AND bdatj IN s_year
       AND poper IN s_poper
       AND kalka = s_kalka-low
       AND ver   = p_ver
       AND artnr IN r_matnr.
  ENDIF.

  CHECK NOT lt_inx_ztcou103[] IS INITIAL.

* 4. Get more archived data looping structure table
  CLEAR: gt_ztcou103, gt_ztcou103.
  LOOP AT lt_inx_ztcou103 INTO ls_inx_ztcou103.
*  4.1 Read information from archivekey & offset
    CLEAR l_handle.
    CALL FUNCTION 'ARCHIVE_READ_OBJECT'
      EXPORTING
        object                    = 'ZTCOU103'
        archivkey                 = ls_inx_ztcou103-archivekey
        offset                    = ls_inx_ztcou103-archiveofs
      IMPORTING
        archive_handle            = l_handle
      EXCEPTIONS
        no_record_found           = 1
        file_io_error             = 2
        internal_error            = 3
        open_error                = 4
        cancelled_by_user         = 5
        archivelink_error         = 6
        object_not_found          = 7
        filename_creation_failure = 8
        file_already_open         = 9
        not_authorized            = 10
        file_not_found            = 11
        error_message             = 12
        OTHERS                    = 13.

    CHECK sy-subrc = 0.

*  4.2 Read table from information
    CLEAR: lt_ztcou103, lt_ztcou103[].
    CALL FUNCTION 'ARCHIVE_GET_TABLE'
      EXPORTING
        archive_handle          = l_handle
        record_structure        = 'ZTCOU103'
        all_records_of_object   = 'X'
      TABLES
        table                   = lt_ztcou103
      EXCEPTIONS
        end_of_object           = 1
        internal_error          = 2
        wrong_access_to_archive = 3
        OTHERS                  = 4.

    CHECK sy-subrc = 0 AND NOT lt_ztcou103[] IS INITIAL.

    DELETE lt_ztcou103 WHERE upgvc NOT IN s_upg
                          OR compn NOT IN s_compn
                          OR kstar NOT IN s_kstar.

* 5. Append archived data table to finally interal table
    LOOP AT lt_ztcou103.
      MOVE-CORRESPONDING lt_ztcou103 TO gt_ztcou103.
      APPEND gt_ztcou103.  CLEAR gt_ztcou103.
    ENDLOOP.
  ENDLOOP.

  SORT gt_ztcou103.
  DELETE ADJACENT DUPLICATES FROM gt_ztcou103 COMPARING ALL FIELDS.

* makt
  CLEAR: lt_ztcou103_tmp, lt_ztcou103_tmp[].
  lt_ztcou103_tmp[] = gt_ztcou103[].
  SORT lt_ztcou103_tmp BY compn.
  DELETE ADJACENT DUPLICATES FROM lt_ztcou103_tmp COMPARING compn.

  CLEAR: lt_makt, lt_makt[].
  SELECT *
    FROM makt
    INTO CORRESPONDING FIELDS OF TABLE lt_makt
    FOR ALL ENTRIES IN lt_ztcou103_tmp
   WHERE matnr = lt_ztcou103_tmp-compn
     AND spras = sy-langu.

* mara
  CLEAR: lt_mara, lt_mara[].
  SELECT *
    FROM mara
    INTO CORRESPONDING FIELDS OF TABLE lt_mara
    FOR ALL ENTRIES IN lt_ztcou103_tmp
   WHERE matnr = lt_ztcou103_tmp-compn.

* marc
  CLEAR: lt_ztcou103_tmp, lt_ztcou103_tmp[].
  lt_ztcou103_tmp[] = gt_ztcou103[].
  SORT lt_ztcou103_tmp BY compn werks.
  DELETE ADJACENT DUPLICATES FROM lt_ztcou103_tmp COMPARING compn werks.

  CLEAR: lt_marc, lt_marc[].
  SELECT *
    FROM marc
    INTO CORRESPONDING FIELDS OF TABLE lt_mara
    FOR ALL ENTRIES IN lt_ztcou103_tmp
   WHERE matnr = lt_ztcou103_tmp-compn
     AND werks = lt_ztcou103_tmp-werks.

* mbew
  CLEAR: lt_mbew, lt_mbew[].
  SELECT *
    FROM mbew
    INTO CORRESPONDING FIELDS OF TABLE lt_mbew
    FOR ALL ENTRIES IN lt_ztcou103_tmp
   WHERE matnr = lt_ztcou103_tmp-compn
     AND bwkey = lt_ztcou103_tmp-werks.

  LOOP AT gt_ztcou103.
    "makt
    CLEAR lt_makt.
    READ TABLE lt_makt WITH KEY matnr = gt_ztcou103-compn
                                spras = sy-langu.
    IF sy-subrc = 0.
      gt_ztcou103-maktg = lt_makt-maktg.
    ELSE.
      DELETE gt_ztcou103.  CONTINUE.
    ENDIF.

    "mara
    CLEAR lt_mara.
    READ TABLE lt_mara WITH KEY matnr = gt_ztcou103-compn.
    IF sy-subrc = 0.
      gt_ztcou103-matkl = lt_mara-matkl.
      gt_ztcou103-profl = lt_mara-profl.
      gt_ztcou103-mtart = lt_mara-mtart.
    ELSE.
      DELETE gt_ztcou103.  CONTINUE.
    ENDIF.

    "marc
    CLEAR lt_marc.
    READ TABLE lt_marc WITH KEY matnr = gt_ztcou103-compn
                                werks = gt_ztcou103-werks.
    IF sy-subrc = 0.
      gt_ztcou103-ekgrp = lt_marc-ekgrp.
    ELSE.
      DELETE gt_ztcou103.  CONTINUE.
    ENDIF.

    "mbew
    CLEAR lt_mbew.
    READ TABLE lt_mbew WITH KEY matnr = gt_ztcou103-compn
                                bwkey = gt_ztcou103-werks.
    IF sy-subrc = 0.
      gt_ztcou103-kalnr = lt_mbew-kaln1.
    ELSE.
      DELETE gt_ztcou103.  CONTINUE.
    ENDIF.

    MODIFY gt_ztcou103.
  ENDLOOP.

* 5.1 Append archived data table to finally interal table
  INSERT LINES OF gt_ztcou103 INTO TABLE gt_103.

ENDFORM.                    " ARCHIVE_READ_ZTCOU103
