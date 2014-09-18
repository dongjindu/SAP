*----------------------------------------------------------------------
* Program ID        : ZACOU108
* Title             : [CO] Cost Summary
* Created on        : 10/04/2006
* Created by        : Michelle Jeong
* Specifications By : Andy Choi
* Description       : Display Cost Review
*                     Get data from table ZTCOU103, ZTCOU100 &
*                     Info-record: A004, A005
* Date        Developer  Request    Description
*----------------------------------------------------------------------
* 14/06/2013  T00303   UD1K957373   U1: Apply archiving
*----------------------------------------------------------------------
REPORT zacou108 NO STANDARD PAGE HEADING MESSAGE-ID zmco.

INCLUDE zacoui00.

TABLES: ztcou103,                     " [CO] Cost Roll-Up
        keko,
        mara.                         " General Material Data

TYPES: BEGIN OF ty_comp,
         kalka TYPE ck_kalka,
         bdatj TYPE bdatj,
         poper TYPE poper,
         bwdat TYPE ck_bwdat,         " Valuation date
         artnr TYPE artnr,            " Product
         kstar TYPE kstar,            " Cost Element
         bklas TYPE bklas,            " Valuation class
         maktx TYPE maktx,            " Product Desc.
         wertn TYPE zwertn,           " Overall value
         duty  TYPE zduty1,           " Duty
         frg   TYPE zfrg1,            " Freight
         oth   TYPE zoth1,            " Others
       END OF ty_comp.

TYPES: BEGIN OF ty_ce,
         matnr TYPE artnr,             " Material
         kstar TYPE kstar,             " Cost Element
         wertn TYPE zwertn,            " Price
       END OF ty_ce.

TYPES: BEGIN OF ty_100,
         kalka TYPE ck_kalka,
         bdatj TYPE bdatj,
         poper TYPE poper,
         matnr TYPE matnr,             " Material
         verid TYPE verid,             " Version
         crint TYPE zcrint,            " In-Color
         crext TYPE zcrext,            " Out-Color
         bwdat TYPE ck_bwdat,
       END OF ty_100.

TYPES: BEGIN OF ty_kstar,
         kstar TYPE kstar,             " Cost Element
       END OF ty_kstar.

TYPES: BEGIN OF ty_a004,
         kschl TYPE kscha,             " Condition type
         matnr TYPE matnr,             " Material
         datbi TYPE kodatbi,           " End date
         datab TYPE kodatab,           " Start date
         knumh TYPE knumh,             " Condition record number
       END OF ty_a004.

TYPES: BEGIN OF ty_cost,
         kalka   TYPE ck_kalka,
         bdatj   TYPE bdatj,
         poper   TYPE poper,
         matnr   TYPE matnr,           " Material
         maktx   TYPE maktx,           " Desc.
         bklas   TYPE bklas,           " VC
         bwdat   TYPE ck_bwdat,        " Valuation date
         verid   TYPE verid,           " Product Version
         crint   TYPE zcrint,          " In-Color
         crext   TYPE zcrext,          " Out-Color
         kstar1  TYPE kstar,           " Cost Element
         kstar2  TYPE kstar,           " Cost Element
         kstar3  TYPE kstar,           " Cost Element
         kstar4  TYPE kstar,           " Cost Element
         kstar5  TYPE kstar,           " Cost Element
         kstar6  TYPE kstar,           " Cost Element
         kstar7  TYPE kstar,           " Cost Element
         kstar8  TYPE kstar,           " Cost Element
         kstar9  TYPE kstar,           " Cost Element
         kstar10 TYPE kstar,           " Cost Element
         amt1    TYPE zwertn,          " Price by cost element
         amt2    TYPE zwertn,          " Price by cost element
         amt3    TYPE zwertn,          " Price by cost element
         amt4    TYPE zwertn,          " Price by cost element
         amt5    TYPE zwertn,          " Price by cost element
         amt6    TYPE zwertn,          " Price by cost element
         amt7    TYPE zwertn,          " Price by cost element
         amt8    TYPE zwertn,          " Price by cost element
         amt9    TYPE zwertn,          " Price by cost element
         amt10   TYPE zwertn,          " Price by cost element
         netamt  TYPE zwertn,          " Net Amount
         duty    TYPE zduty1,          " Duty
         frg     TYPE zfrg1,           " Freight
         oth     TYPE zoth1,           " Others
         dsum    TYPE zwertn,          " Deliver Cost
         wertn   TYPE zwertn,          " Material Cost SUM
         sales   TYPE zwertn,          " Sales Amount
         amt     TYPE zwertn,          " Material Cost / Sales


         prodh  LIKE mvke-prodh,
         mvgr3  LIKE mvke-mvgr3,
         mvgr4  LIKE mvke-mvgr4,
         mvgr5  LIKE mvke-mvgr5,
         regst  TYPE char01,
       END OF ty_cost.

TYPES: BEGIN OF ty_out.
        INCLUDE TYPE ty_cost.
TYPES:   chk,
         tabcolor TYPE slis_t_specialcol_alv,
       END OF ty_out.

RANGES r_vkorg FOR tvko-vkorg.

DATA: gt_comp  TYPE TABLE OF ty_comp  WITH HEADER LINE,
      gt_kstar TYPE TABLE OF ty_kstar WITH HEADER LINE,
      gt_100   TYPE TABLE OF ty_100   WITH HEADER LINE,
      gt_a004  TYPE TABLE OF ty_a004  WITH HEADER LINE,
      gt_a005  TYPE TABLE OF ty_a004  WITH HEADER LINE,
      gt_cost  TYPE TABLE OF ty_cost  WITH HEADER LINE,
      gt_out   TYPE TABLE OF ty_out   WITH HEADER LINE,
      gv_bwdat LIKE sy-datum.

*- U1 Start
DATA: gt_ztcou103_a TYPE TABLE OF ztcou103 WITH HEADER LINE,
      gt_konp_a     TYPE TABLE OF konp WITH HEADER LINE.
*- U1 End

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

RANGES: r_matnr FOR ztcou103-artnr.

*----------------------------------------------------------------------
* Selection-Screen
*----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK b0 WITH FRAME.
PARAMETERS: p_kokrs LIKE keko-kokrs OBLIGATORY
                                    MEMORY ID cac
                                    MATCHCODE OBJECT fc_kokrs.

*           P_KALKA LIKE KEKO-KALKA OBLIGATORY MEMORY ID KKA,
SELECT-OPTIONS :
             s_kalka FOR keko-kalka OBLIGATORY
              NO-EXTENSION MEMORY ID kka.

PARAMETERS: p_zver1 TYPE zver1.
*           P_POPER LIKE KEKO-POPER OBLIGATORY MEMORY ID POPR.
SELECT-OPTIONS: s_bdatj FOR keko-bdatj OBLIGATORY MEMORY ID bdtj,
                s_poper FOR ztcou103-poper  MEMORY ID popr,
                s_mtart FOR mara-mtart      MEMORY ID mta,
                s_matnr FOR ztcou103-artnr  MEMORY ID mat.
SELECTION-SCREEN END OF BLOCK b0.
PARAMETERS: p_100 AS CHECKBOX DEFAULT 'X'.

* Layout
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
PARAMETERS: p_vari TYPE slis_vari.
SELECTION-SCREEN END OF BLOCK b1.

*- U1 Start
INCLUDE ziarch_comm01.
*- U1 End

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_vari.
  PERFORM alv_variant_f4 CHANGING p_vari.

*----------------------------------------------------------------------*
* Start of selection
*----------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM get_data.

*----------------------------------------------------------------------*
* End of selection
*----------------------------------------------------------------------*
END-OF-SELECTION.
  IF gt_out[] IS INITIAL.
    MESSAGE s000 WITH 'No data found.'.
    EXIT.
  ENDIF.

  PERFORM get_nav_attr.
  PERFORM disp_result.

*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       Get Costing Data
*----------------------------------------------------------------------*
FORM get_data.
* Get Material range
  PERFORM get_rmatnr.

* Get Costing Data
  CLEAR gt_comp.
  REFRESH gt_comp.

  SELECT a~kalka a~bdatj a~poper a~bwdat
         a~artnr a~kstar  d~bklas c~maktx
         SUM( a~wertn ) SUM( a~duty )
         SUM( a~frg )   SUM( a~oth )
    INTO TABLE gt_comp
    FROM ztcou103 AS a
    JOIN mara AS b
      ON b~matnr = a~artnr
    JOIN makt AS c
      ON c~matnr = a~artnr
     AND c~spras = sy-langu
    JOIN mbew AS d
      ON d~matnr = a~artnr
     AND d~bwkey = a~werks
   WHERE a~kokrs = p_kokrs
     AND a~bdatj IN s_bdatj
*     AND A~POPER = P_POPER
     AND a~poper IN s_poper
*     AND A~KALKA = P_KALKA
     AND a~kalka IN s_kalka
     AND a~artnr IN r_matnr
     AND mtart IN s_mtart
   GROUP BY a~kalka a~bdatj a~poper a~bwdat
            a~artnr a~kstar d~bklas c~maktx.

*- U1 Start
  IF p_arch EQ 'X'.
    PERFORM archive_read_ztcou103.
  ENDIF.
*- U1 End

  IF sy-subrc = 0.
*   Create Internal Tables for Product version, Color & Val.Class
    PERFORM get_others.

*   Create Internal Tables for Costiong review
    PERFORM get_info_record.
    PERFORM get_gt_cost.

    CLEAR gt_out.
    REFRESH gt_out.

    LOOP AT gt_cost.
      MOVE-CORRESPONDING gt_cost TO gt_out.
      APPEND gt_out.
      CLEAR gt_out.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  GET_GT_COST
*&---------------------------------------------------------------------*
*       Get Cost Info
*----------------------------------------------------------------------*
FORM get_gt_cost.
  SORT gt_comp BY artnr.

  CLEAR  gt_cost.
  REFRESH  gt_cost.

  LOOP AT gt_comp.
    gt_cost-kalka = gt_comp-kalka.               " Period
    gt_cost-bdatj = gt_comp-bdatj.               " Period
    gt_cost-poper = gt_comp-poper.               " Period
    gt_cost-matnr = gt_comp-artnr.               " Product
    gt_cost-maktx = gt_comp-maktx.               " Desc.
    gt_cost-bklas = gt_comp-bklas.               " Val.Class
    gv_bwdat = gt_cost-bwdat = gt_comp-bwdat.    " Valuation date
    gt_cost-duty = gt_comp-duty.                 " Duty
    gt_cost-frg = gt_comp-frg.                   " Freight
    gt_cost-oth = gt_comp-oth.                   " Others
    gt_cost-wertn = gt_comp-wertn.               " Material Cost

*   Deliver Cost Sum
    gt_cost-dsum = gt_cost-duty + gt_cost-frg + gt_cost-oth.

    COLLECT gt_cost.
    CLEAR gt_cost.
  ENDLOOP.

* UD1K941680 - by IG.MOON 9/25/07 {
  SORT gt_comp BY poper artnr kstar.
* }
  LOOP AT gt_cost.
*   Price by cost element
    PERFORM get_amt_by_coste.

*   Net Amount
    gt_cost-netamt = gt_cost-wertn - gt_cost-dsum.

*   Material Cost / Sales
    PERFORM calc_mat_cost_per_sales.

    MODIFY gt_cost.
  ENDLOOP.

  DELETE gt_cost WHERE wertn = 0.

  LOOP AT gt_100.
    gt_cost-verid = gt_100-verid.         " Product Version
    gt_cost-crint = gt_100-crint.         " In-color
    gt_cost-crext = gt_100-crext.         " Out-color

    MODIFY gt_cost TRANSPORTING verid crint crext
                  WHERE kalka = gt_100-kalka
                    AND poper = gt_100-poper
                    AND matnr = gt_100-matnr.
  ENDLOOP.

ENDFORM.                    " GET_GT_COST
*&---------------------------------------------------------------------*
*&      Form  DISP_RESULT
*&---------------------------------------------------------------------*
*       Display Cost Review
*----------------------------------------------------------------------*
FORM disp_result.
  CLEAR: gt_fieldcat, gs_layout, gt_events, gs_variant,
         gt_fieldcat[], gt_events[].

* Set field category
  PERFORM set_field_category.

* Set layout
  CLEAR gs_layout.
  gs_layout-zebra             = 'X'.
  gs_layout-colwidth_optimize = 'X'.
  gs_layout-box_fieldname     = 'CHK'.
  gs_layout-coltab_fieldname  = 'TABCOLOR'.

* Set color
  PERFORM set_color.

* Set list header
  PERFORM comment USING gt_list_top_of_page.

* Set events
  PERFORM set_events CHANGING gt_events.

* Set callback program
  gv_repid = gs_variant-report = sy-repid.

* Set variant
  gs_variant-variant = p_vari.

* Display alv grid
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program       = gv_repid
      i_callback_pf_status_set = gc_pf_status_set
      i_callback_user_command  = gc_user_command
      is_layout                = gs_layout
      it_fieldcat              = gt_fieldcat[]
      it_events                = gt_events[]
      i_save                   = 'A'
      is_variant               = gs_variant
    TABLES
      t_outtab                 = gt_out.

ENDFORM.                    " DISP_RESULT
*&---------------------------------------------------------------------*
*&      Form  COMMENT
*&---------------------------------------------------------------------*
FORM comment USING lt_top_of_page TYPE slis_t_listheader.
  DATA ls_line TYPE slis_listheader.

  CLEAR ls_line.
  ls_line-typ  = 'S'.

  ls_line-key  = 'Controlling Area:'.
  ls_line-info = p_kokrs.
  APPEND ls_line TO lt_top_of_page.

*  ls_line-key  = 'Costing Type:'.
*  ls_line-info = s_kalka-low.
*  APPEND ls_line TO lt_top_of_page.
*
*  ls_line-key  = 'Year:'.
*  ls_line-info = s_bdatj-low.
*  APPEND ls_line TO lt_top_of_page.
*
*  ls_line-key  = 'Period:'.
*  CONCATENATE s_poper-low '-' s_poper-high INTO ls_line-info.
*  APPEND ls_line TO lt_top_of_page.

  CLEAR: ls_line-key, ls_line-info.
  APPEND ls_line TO lt_top_of_page.

ENDFORM.                    " COMMENT
*&---------------------------------------------------------------------*
*&      Form  GET_OTHERS
*&---------------------------------------------------------------------*
*       Create Internal Tables for Product version, Color & Val.Class
*----------------------------------------------------------------------*
FORM get_others.
  CLEAR: gt_kstar, gt_100.
  REFRESH: gt_kstar, gt_100.

  LOOP AT gt_comp.
    IF NOT gt_comp-kstar IS INITIAL.
      gt_kstar-kstar = gt_comp-kstar.
      APPEND gt_kstar.
      CLEAR gt_kstar.
    ENDIF.
  ENDLOOP.

  SORT: gt_kstar BY kstar.

  DELETE ADJACENT DUPLICATES FROM gt_kstar.

  IF gt_100[] IS INITIAL.
    PERFORM get_gt_100.
  ENDIF.

ENDFORM.                    " GET_OTHERS
*&---------------------------------------------------------------------*
*&      Form  calc_mat_cost_per_sales
*&---------------------------------------------------------------------*
*       Get Price
*----------------------------------------------------------------------*
FORM calc_mat_cost_per_sales.
  DATA: l_kbetr TYPE kbetr,         " Rate
        l_sales TYPE kbetr.         " Sales Price

  CLEAR: l_sales, l_kbetr.

* Sales Price
* ZV00: Price(A004-Material)
  IF gt_cost-bklas = '7920'.
    PERFORM search_a004 CHANGING l_kbetr.

* ZP00: price(A005-Customer/Material)
* ZP07: margin(A307-Customer with Release Status)
  ELSE.
    PERFORM search_a005 CHANGING l_kbetr.
  ENDIF.

  l_sales = l_kbetr.      " Sales price

  IF gt_cost-wertn <> 0 AND l_sales <> 0.
    gt_cost-amt = 100 * gt_cost-wertn / l_sales.
  ENDIF.

ENDFORM.                    " calc_mat_cost_per_sales
*&---------------------------------------------------------------------*
*&      Form  get_amt_BY_coste
*&---------------------------------------------------------------------*
*       Price by cost element
*----------------------------------------------------------------------*
FORM get_amt_by_coste.
  DATA: l_tot  TYPE zwertn,
        l_fname(15),
        n(1).

  FIELD-SYMBOLS: <fs1> TYPE kstar,
                 <fs2> TYPE zwertn.

  CLEAR: n, l_tot.

  LOOP AT gt_kstar.
    n = n + 1.
    CONCATENATE 'GT_COST-KSTAR' n INTO l_fname.
    ASSIGN (l_fname) TO <fs1>.
    <fs1> = gt_kstar-kstar.

    CONCATENATE 'GT_COST-AMT' n INTO l_fname.
    ASSIGN (l_fname) TO <fs2>.

    READ TABLE gt_comp WITH KEY
* UD1K941680 - by IG.MOON 9/25/07 {

                                poper = gt_cost-poper
* }
                                artnr = gt_cost-matnr
                                kstar = gt_kstar-kstar
                     BINARY SEARCH.
    IF sy-subrc = 0.
      <fs2> = gt_comp-wertn - gt_comp-duty - gt_comp-frg - gt_comp-oth.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " get_amt_BY_coste
*&---------------------------------------------------------------------*
*&      Form  SET_FIELD_CATEGORY
*&---------------------------------------------------------------------*
*       Set Field category
*----------------------------------------------------------------------*
FORM set_field_category.
  DATA: n,
        l_fname TYPE slis_fieldname,
        l_text  TYPE scrtext_l.

  PERFORM build_field_category USING:
    'KALKA'   'X'   'CstT'        02  'CHAR',
    'BDATJ'   'X'   'Year'        04  'CHAR',
    'POPER'   'X'   'Period'      03  'CHAR',

    'PRODH'   'X'   'Prod.Hir'    10  'CHAR',
    'MVGR3'   'X'   'T/M'         01  'CHAR',
    'MVGR4'   'X'   'Engine'      01  'CHAR',
    'MVGR5'   'X'   'Grade'       03  'CHAR',

    'REGST'   'X'   'Reg'         01  'CHAR',
    'MATNR'   'X'   'Product'     18  'CHAR',
    'MAKTX'   'X'   'Desc.'       30  'CHAR',
    'VERID'   'X'   'Version'     4   'CHAR',
    'CRINT'   ' '   'Internal'    2   'CHAR',
    'CREXT'   ' '   'External'    2   'CHAR',
    'WERTN'   ' '   'Total Amt'   15  'CURR',
    'NETAMT'  ' '   'Net Total'   15  'CURR'.

  CLEAR: n, l_fname.

  LOOP AT gt_kstar.
    n = n + 1.
    CONCATENATE 'AMT' n INTO l_fname.
    l_text = gt_kstar-kstar.
    PACK  l_text TO l_text.
    SHIFT l_text LEFT DELETING LEADING space.

    PERFORM build_field_category USING:
      l_fname  ' '  l_text     15  'CURR'.
  ENDLOOP.

  PERFORM build_field_category USING:
    'DSUM'   ' '  'Import Cost'        15  'CURR',
    'DUTY'   ' '  'Duty'               15  'CURR',
    'FRG'    ' '  'Freight'            15  'CURR',
    'OTH'    ' '  'Other'              15  'CURR',
    'AMT'    ' '  '% Material/Sales'   10  'CURR'.

  gs_fieldcat-decimals_out = 3.
  gs_fieldcat-just = 'R'.
  MODIFY gt_fieldcat FROM gs_fieldcat TRANSPORTING decimals_out just
     WHERE datatype = 'CURR'.

  gs_fieldcat-just = 'C'.
  MODIFY gt_fieldcat FROM gs_fieldcat TRANSPORTING just
     WHERE fieldname = 'VERID'
        OR fieldname = 'CRINT'
        OR fieldname = 'CREXT'.

ENDFORM.                    " SET_FIELD_CATEGORY
*&---------------------------------------------------------------------*
*&      Form  GET_INFO_RECORD
*&---------------------------------------------------------------------*
*       Get Material Info record: Vehicle
*----------------------------------------------------------------------*
FORM get_info_record.
  IF NOT gt_100[] IS INITIAL.
    PERFORM get_vkorg.

    CLEAR: gt_a004, gt_a005.
    REFRESH: gt_a004, gt_a005.

    SELECT kschl matnr datbi datab knumh
      INTO TABLE gt_a004
      FROM a004
      FOR ALL ENTRIES IN gt_100
     WHERE kappl = 'V'
       AND kschl = 'ZV00'
       AND matnr = gt_100-matnr
       AND datab =< gt_100-bwdat
       AND datbi >= gt_100-bwdat.

    SELECT kschl matnr datbi datab knumh
      INTO TABLE gt_a005
      FROM a005
      FOR ALL ENTRIES IN gt_100
     WHERE kappl = 'V'
       AND kschl = 'ZP00'
       AND matnr = gt_100-matnr
       AND vkorg IN r_vkorg
       AND datab =< gt_100-bwdat
       AND datbi >= gt_100-bwdat.

    SORT: gt_a004 BY matnr datab DESCENDING,
          gt_a005 BY matnr datab DESCENDING.
  ENDIF.

ENDFORM.                    " GET_INFO_RECORD
*&---------------------------------------------------------------------*
*&      Form  GET_GT_100
*&---------------------------------------------------------------------*
*       Get color information
*----------------------------------------------------------------------*
FORM get_gt_100.
  CLEAR gt_100.
  REFRESH gt_100.

  SELECT kalka bdatj poper matnr verid crint crext bwdat
    INTO TABLE gt_100
    FROM ztcou100
     FOR ALL ENTRIES IN gt_comp
   WHERE kokrs = p_kokrs
*     AND KALKA = P_KALKA
     AND kalka IN s_kalka
     AND bdatj IN s_bdatj
     AND poper IN s_poper
     AND matnr = gt_comp-artnr.

  SORT gt_100 BY kalka bdatj poper matnr.

ENDFORM.                    " GET_GT_100
*&---------------------------------------------------------------------*
*&      Form  GET_VKORG
*&---------------------------------------------------------------------*
*       Get Sales organization
*----------------------------------------------------------------------*
FORM get_vkorg.
  TYPES: BEGIN OF ty_tka02,
           vkorg TYPE vkorg,
         END OF ty_tka02.

  DATA lt_tka02 TYPE TABLE OF ty_tka02 WITH HEADER LINE.

  CLEAR: lt_tka02, r_vkorg.
  REFRESH: lt_tka02, r_vkorg.

  SELECT vkorg INTO TABLE lt_tka02
    FROM tka02 AS a
    JOIN tvko AS b
      ON b~bukrs = a~bukrs
   WHERE a~kokrs = p_kokrs.

  SORT lt_tka02 BY vkorg.
  r_vkorg-sign = 'I'.
  r_vkorg-option = 'EQ'.
  MODIFY r_vkorg TRANSPORTING sign option WHERE sign IS INITIAL.

ENDFORM.                    " GET_VKORG
*&---------------------------------------------------------------------*
*&      Form  SEARCH_A004
*&---------------------------------------------------------------------*
*       Get Sales price: HMMA Vech.Price
*----------------------------------------------------------------------*
FORM search_a004 CHANGING p_kbetr TYPE kbetr_kond.
  TYPES: BEGIN OF ty_a004,
          matnr TYPE matnr,     " Material
          knumh TYPE knumh,     " Condition record number
        END OF ty_a004.

  DATA: lt_a004 TYPE TABLE OF ty_a004 WITH HEADER LINE,
        l_knumh TYPE knumh,
        l_matnr TYPE matnr.

  CLEAR l_knumh.

  READ TABLE gt_a004 WITH KEY matnr = gt_cost-matnr BINARY SEARCH.

  IF sy-subrc = 0.
    l_knumh =  gt_a004-knumh.

  ELSE.
    CLEAR: l_matnr, lt_a004.
    REFRESH lt_a004.

    CONCATENATE gt_cost-matnr(13) '%' INTO l_matnr.

    SELECT matnr knumh INTO TABLE lt_a004
      FROM a004
     WHERE kappl = 'V'
       AND kschl = 'ZV00'
       AND matnr LIKE l_matnr
       AND datab <= gt_cost-bwdat
       AND datbi >= gt_cost-bwdat.

    SORT lt_a004 BY matnr DESCENDING.

    READ TABLE lt_a004 INDEX 1.
    IF sy-subrc = 0.
      l_knumh = lt_a004-knumh.
    ENDIF.
  ENDIF.

  SELECT SINGLE kbetr INTO p_kbetr
    FROM konp
   WHERE knumh = l_knumh.

*- U1 Start
  IF p_arch EQ 'X' AND sy-subrc <> 0.
    PERFORM archive_read_konp USING l_knumh CHANGING p_kbetr.
  ENDIF.
*- U1 End

ENDFORM.                    " SEARCH_A004
*&---------------------------------------------------------------------*
*&      Form  SEARCH_A005
*&---------------------------------------------------------------------*
*       Get Sales price: Part sales price * ( 1 + A/S Margin / 1000)
*----------------------------------------------------------------------*
FORM search_a005 CHANGING p_kbetr TYPE kbetr_kond.
  TYPES: BEGIN OF ty_a005,
           matnr TYPE matnr,
           knumh TYPE knumh,
           kunnr TYPE kunnr,
         END OF ty_a005.

  DATA: lt_a005  TYPE TABLE OF ty_a005 WITH HEADER LINE,
        l_knumh  TYPE knumh,
        l_matnr  TYPE matnr,
        l_kunnr  TYPE kunnr,
        l_kbetr1 TYPE kbetr_kond,
        l_kbetr2 TYPE kbetr_kond.

  READ TABLE gt_a005 WITH KEY matnr = gt_cost-matnr BINARY SEARCH.
  IF sy-subrc = 0.
    l_knumh =  gt_a005-knumh.

  ELSE.
    REFRESH lt_a005.
    CLEAR: lt_a005, l_matnr, l_knumh, l_kunnr.

    CONCATENATE gt_cost-matnr(13) '%' INTO l_matnr.

    SELECT matnr knumh kunnr INTO TABLE lt_a005
           FROM a005
           WHERE kappl = 'V'
           AND   kschl = 'ZP00'
           AND   matnr LIKE l_matnr
           AND   vkorg IN r_vkorg
           AND   datab <= gt_cost-bwdat
           AND   datbi >= gt_cost-bwdat.

    SORT lt_a005 BY matnr DESCENDING.

    READ TABLE lt_a005 INDEX 1.

    IF sy-subrc = 0.
      l_knumh = lt_a005-knumh.
      l_kunnr = lt_a005-kunnr.
    ENDIF.
  ENDIF.

* A/S Margin
  IF NOT l_knumh IS INITIAL.
    CLEAR: l_kbetr1, l_kbetr2.

    SELECT SINGLE kbetr INTO l_kbetr1
      FROM konp
     WHERE knumh = l_knumh.

*- U1 Start
  IF p_arch EQ 'X' AND sy-subrc <> 0.
    PERFORM archive_read_konp USING l_knumh CHANGING l_kbetr1.
  ENDIF.
*- U1 End

    SELECT SINGLE knumh INTO l_knumh
      FROM a307
     WHERE kappl = 'V'
       AND kschl = 'ZP07'
       AND vkorg IN r_vkorg
       AND kunnr = l_kunnr
       AND datab <= gt_cost-bwdat
       AND datbi >= gt_cost-bwdat.

    IF sy-subrc = 0.
      SELECT SINGLE kbetr INTO l_kbetr2
        FROM konp WHERE knumh = l_knumh.
*- U1 Start
      IF p_arch EQ 'X' AND sy-subrc <> 0.
        PERFORM archive_read_konp USING l_knumh CHANGING l_kbetr2.
      ENDIF.
*- U1 End
    ENDIF.

    p_kbetr = l_kbetr1 * ( 1  + l_kbetr2 / 1000 ).

  ENDIF.

ENDFORM.                    " SEARCH_A005
*&---------------------------------------------------------------------*
*&      Form  GET_RMATNR
*&---------------------------------------------------------------------*
*       Get Material range
*----------------------------------------------------------------------*
FORM get_rmatnr.
  CLEAR r_matnr.
  REFRESH r_matnr.

  CLEAR gt_100.
  REFRESH gt_100.

  IF p_100 = 'X'.
    SELECT kalka bdatj poper matnr verid crint crext
      INTO TABLE gt_100
      FROM ztcou100
     WHERE kokrs = p_kokrs
*       AND KALKA = P_KALKA
       AND kalka IN s_kalka
       AND bdatj IN s_bdatj
       AND poper IN s_poper
       AND matnr IN s_matnr.

    IF sy-subrc <> 0.
      MESSAGE e000 WITH 'No entry found'.
    ENDIF.

    SORT gt_100 BY kalka bdatj poper matnr.

    LOOP AT gt_100.
      r_matnr-low = gt_100-matnr.
      APPEND r_matnr.
    ENDLOOP.

    r_matnr-sign   = 'I'.
    r_matnr-option = 'EQ'.
    MODIFY r_matnr TRANSPORTING sign option WHERE sign IS INITIAL.

  ELSE.
    r_matnr[] = s_matnr[].
  ENDIF.

ENDFORM.                    " GET_RMATNR
*&---------------------------------------------------------------------*
*&      Form  SET_COLOR
*&---------------------------------------------------------------------*
*       Setting color
*----------------------------------------------------------------------*
FORM set_color.
  CLEAR: gs_specialcol, gt_specialcol[], gt_out-tabcolor[].

  gs_specialcol-fieldname = 'TOT'.
  gs_specialcol-color-col = cl_gui_resources=>list_col_negative.
  gs_specialcol-color-int = 0.
  APPEND gs_specialcol TO gt_specialcol.

  gt_out-tabcolor[] = gt_specialcol[].
  MODIFY gt_out TRANSPORTING tabcolor WHERE tabcolor IS INITIAL.

ENDFORM.                    " SET_COLOR
*&---------------------------------------------------------------------*
*&      Form  PF_STATUS_SET
*&---------------------------------------------------------------------*
FORM pf_status_set USING rt_extab TYPE slis_t_extab.
  SET PF-STATUS 'STANDARD'.

ENDFORM.                    " PF_STATUS_SET
*&-------------------------------------------------------------------*
*&      USER_COMMAND
*&-------------------------------------------------------------------*
FORM user_command USING r_ucomm LIKE sy-ucomm
                        rs_selfield TYPE slis_selfield.
  RANGES: lr_matnr FOR ztcou100-matnr,
          lr_poper FOR ztcou100-poper,
          lr_bdatj FOR ztcou100-bdatj,
          lr_kalka FOR ztcou100-kalka.

* Submit Cost Detail Report ZACOU109 when Choose [Cost Detail]
  IF r_ucomm = '&IC1'.
    READ TABLE gt_out INDEX rs_selfield-tabindex.
    CHECK sy-subrc EQ 0.
    lr_matnr-sign = 'I'. lr_matnr-option = 'EQ'.
    lr_matnr-low = gt_out-matnr. APPEND lr_matnr.

    REFRESH: lr_poper, lr_bdatj, lr_kalka.
    lr_poper-sign = 'I'. lr_poper-option = 'EQ'.
    lr_poper-low = gt_out-poper. APPEND lr_poper.
    lr_bdatj-sign = 'I'. lr_bdatj-option = 'EQ'.
    lr_bdatj-low = gt_out-bdatj. APPEND lr_bdatj.
    lr_kalka-sign = 'I'. lr_kalka-option = 'EQ'.
    lr_kalka-low = gt_out-kalka. APPEND lr_kalka.

    SUBMIT zacou109 WITH p_kokrs = p_kokrs
                    WITH s_kalka IN s_kalka
                    WITH p_ver   = p_zver1
                    WITH s_year  IN lr_bdatj
                    WITH s_poper IN lr_poper
                    WITH p_dlv = space
                    WITH p_qty = space
                    WITH p_mon = space
                    WITH s_matnr IN lr_matnr
                    WITH p_call = 'X'
                AND RETURN.

  ENDIF.

  IF r_ucomm = 'DET'.
    REFRESH: lr_poper, lr_matnr, lr_bdatj, lr_kalka.

    lr_matnr-sign = 'I'. lr_matnr-option = 'EQ'.
    lr_poper-sign = 'I'. lr_poper-option = 'EQ'.
    lr_bdatj-sign = 'I'. lr_bdatj-option = 'EQ'.
    lr_kalka-sign = 'I'. lr_kalka-option = 'EQ'.

    LOOP AT gt_out WHERE chk = 'X'.
      lr_matnr-low = gt_out-matnr. APPEND lr_matnr.
      lr_poper-low = gt_out-poper. APPEND lr_poper.
      lr_bdatj-low = gt_out-bdatj. APPEND lr_bdatj.
      lr_kalka-low = gt_out-kalka. APPEND lr_kalka.
    ENDLOOP.

    DELETE ADJACENT DUPLICATES FROM lr_bdatj COMPARING low.
    DELETE ADJACENT DUPLICATES FROM lr_poper COMPARING low.
    DELETE ADJACENT DUPLICATES FROM lr_kalka COMPARING low.

    SUBMIT zacou109 WITH p_kokrs = p_kokrs
                    WITH s_kalka IN lr_kalka
                    WITH p_ver   = p_zver1
                    WITH s_year  IN lr_bdatj
                    WITH s_poper IN lr_poper
                    WITH p_dlv = space
                    WITH p_qty = space
                    WITH p_mon = space
                    WITH s_matnr IN lr_matnr
                    WITH p_call = 'X'
                AND RETURN.

  ENDIF.

ENDFORM.                    "user_command
*&---------------------------------------------------------------------*
*&      Form  get_nav_attr
*&---------------------------------------------------------------------*
FORM get_nav_attr.
  DATA $ix LIKE sy-tabix.

  SELECT a~matnr a~prodh a~mvgr3 a~mvgr4 a~mvgr5 b~werks b~fevor
          c~mtart c~matkl
    INTO TABLE lt_mvke
    FROM mvke AS a
     INNER JOIN marc AS b
        ON b~matnr EQ a~matnr
     INNER JOIN mara AS c
        ON c~matnr EQ b~matnr
    FOR ALL ENTRIES IN gt_out
    WHERE a~matnr EQ gt_out-matnr.
  SORT : lt_mvke BY matnr.

  LOOP AT gt_out.
    $ix = sy-tabix.
    READ TABLE lt_mvke WITH KEY matnr = gt_out-matnr BINARY SEARCH.
    IF sy-subrc EQ 0.
      gt_out-prodh = lt_mvke-prodh.
      gt_out-mvgr3 = lt_mvke-mvgr3.
      gt_out-mvgr4 = lt_mvke-mvgr4.
      gt_out-mvgr5 = lt_mvke-mvgr5.

    ENDIF.

    READ TABLE gt_100 WITH KEY kalka = gt_out-kalka

                               poper = gt_out-poper
                               matnr = gt_out-matnr.
    IF sy-subrc = 0.
      gt_out-regst = 'X'.
    ENDIF.

    MODIFY gt_out INDEX $ix.
  ENDLOOP.

ENDFORM.                    " get_nav_attr
*&---------------------------------------------------------------------*
*&      Form  ARCHIVE_READ_ZTCOU103
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM archive_read_ztcou103 .

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

  DATA: lt_comp TYPE TABLE OF ty_comp WITH HEADER LINE.

  DATA: lt_makt TYPE TABLE OF makt WITH HEADER LINE,
        lt_mara TYPE TABLE OF mara WITH HEADER LINE,
        lt_marc TYPE TABLE OF marc WITH HEADER LINE,
        lt_mbew TYPE TABLE OF mbew WITH HEADER LINE,
        lt_ztcou103_tmp TYPE TABLE OF ztcou103 WITH HEADER LINE.

* 1. Input the archive infostructure name
  CLEAR l_archindex.
  l_archindex = 'ZTCOU103_001'.

* 2. Get the structure table using infostructure
  CLEAR l_gentab.
  SELECT SINGLE gentab INTO l_gentab FROM aind_str2
   WHERE archindex = l_archindex.

  CHECK sy-subrc = 0 AND NOT l_gentab IS INITIAL.

* 3. Get the archived data from structure table
  CLEAR lt_inx_ztcou103[].
  SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_inx_ztcou103
    FROM (l_gentab)
   WHERE kokrs = p_kokrs
     AND bdatj IN s_bdatj
     AND poper IN s_poper
     AND kalka IN s_kalka
     AND artnr IN r_matnr.

  CHECK NOT lt_inx_ztcou103[] IS INITIAL.

* 4. Get more archived data looping structure table
  CLEAR: gt_ztcou103_a, gt_ztcou103_a[].
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

* 5. Append archived data table to finally interal table
    INSERT LINES OF lt_ztcou103 INTO TABLE gt_ztcou103_a.
  ENDLOOP.

  SORT gt_ztcou103_a.
  DELETE ADJACENT DUPLICATES FROM gt_ztcou103_a COMPARING ALL FIELDS.

  CLEAR: lt_ztcou103_tmp, lt_ztcou103_tmp[].
  lt_ztcou103_tmp[] = gt_ztcou103_a[].
  SORT lt_ztcou103_tmp BY artnr.
  DELETE ADJACENT DUPLICATES FROM lt_ztcou103_tmp COMPARING artnr.

* mara
  CLEAR: lt_mara, lt_mara[].
  SELECT *
    FROM mara
    INTO CORRESPONDING FIELDS OF TABLE lt_mara
    FOR ALL ENTRIES IN lt_ztcou103_tmp
   WHERE matnr = lt_ztcou103_tmp-artnr
     AND mtart IN s_mtart.

* makt
  CLEAR: lt_makt, lt_makt[].
  SELECT *
    FROM makt
    INTO CORRESPONDING FIELDS OF TABLE lt_makt
    FOR ALL ENTRIES IN lt_ztcou103_tmp
   WHERE matnr = lt_ztcou103_tmp-artnr
     AND spras = sy-langu.

  CLEAR: lt_ztcou103_tmp, lt_ztcou103_tmp[].
  lt_ztcou103_tmp[] = gt_ztcou103_a[].
  SORT lt_ztcou103_tmp BY artnr werks.
  DELETE ADJACENT DUPLICATES FROM lt_ztcou103_tmp COMPARING artnr werks.

* mbew
  CLEAR: lt_mbew, lt_mbew[].
  SELECT *
    FROM mbew
    INTO CORRESPONDING FIELDS OF TABLE lt_mbew
    FOR ALL ENTRIES IN lt_ztcou103_tmp
   WHERE matnr = lt_ztcou103_tmp-artnr
     AND bwkey = lt_ztcou103_tmp-werks.

  CLEAR: lt_comp, lt_comp[].
  LOOP AT gt_ztcou103_a.
    MOVE-CORRESPONDING gt_ztcou103_a TO lt_comp.

    "mara
    CLEAR lt_mara.
    READ TABLE lt_mara WITH KEY matnr = gt_ztcou103_a-artnr.
    IF sy-subrc = 0.
      "Do Nothing
    ELSE.
      CONTINUE.
    ENDIF.

    "makt
    CLEAR lt_makt.
    READ TABLE lt_makt WITH KEY matnr = gt_ztcou103_a-artnr
                                spras = sy-langu.
    IF sy-subrc = 0.
      lt_comp-maktx = lt_makt-maktx.
    ELSE.
      CONTINUE.
    ENDIF.

    "mbew
    CLEAR lt_mbew.
    READ TABLE lt_mbew WITH KEY matnr = gt_ztcou103_a-compn
                                bwkey = gt_ztcou103_a-werks.
    IF sy-subrc = 0.
      lt_comp-bklas = lt_mbew-bklas.
    ELSE.
      CONTINUE.
    ENDIF.

    APPEND lt_comp.  CLEAR lt_comp.
  ENDLOOP.

* 5.1 Append archived data table to finally interal table
  LOOP AT lt_comp.
    MOVE-CORRESPONDING lt_comp TO gt_comp.
    COLLECT gt_comp.  CLEAR gt_comp.
  ENDLOOP.

ENDFORM.                    " ARCHIVE_READ_ZTCOU103
*&---------------------------------------------------------------------*
*&      Form  ARCHIVE_READ_KONP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_P_KBETR  text
*----------------------------------------------------------------------*
FORM archive_read_konp USING p_knumh CHANGING p_kbetr.

  TYPES: BEGIN OF ty_konp,
         knumh    TYPE knumh,
         kopos    TYPE kopos,
         kappl    TYPE kappl,
         kschl    TYPE kscha,
         loevm_ko TYPE loevm_ko,
           archivekey TYPE arkey,
           archiveofs TYPE admi_offst.
  TYPES: END OF ty_konp.

  DATA: l_handle    TYPE sytabix,
        lt_konp     TYPE TABLE OF konp WITH HEADER LINE,
        l_archindex LIKE aind_str2-archindex,
        l_gentab    LIKE aind_str2-gentab.

  DATA: lt_inx_konp TYPE TABLE OF ty_konp,
        ls_inx_konp TYPE ty_konp.

* 1. Input the archive infostructure name
  CLEAR l_archindex.
  l_archindex = 'ZKONP_002'.

* 2. Get the structure table using infostructure
  CLEAR l_gentab.
  SELECT SINGLE gentab INTO l_gentab FROM aind_str2
   WHERE archindex = l_archindex.

  CHECK sy-subrc = 0 AND NOT l_gentab IS INITIAL.

* 3. Get the archived data from structure table
  CLEAR lt_inx_konp[].
  SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_inx_konp
    FROM (l_gentab)
   WHERE knumh = p_knumh.

  CHECK NOT lt_inx_konp[] IS INITIAL.

* 4. Get more archived data looping structure table
  CLEAR: gt_konp_a, gt_konp_a[].
  LOOP AT lt_inx_konp INTO ls_inx_konp.
*  4.1 Read information from archivekey & offset
    CLEAR l_handle.
    CALL FUNCTION 'ARCHIVE_READ_OBJECT'
      EXPORTING
        object                    = 'MM_EKKO'
        archivkey                 = ls_inx_konp-archivekey
        offset                    = ls_inx_konp-archiveofs
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
    CLEAR: lt_konp, lt_konp[].
    CALL FUNCTION 'ARCHIVE_GET_TABLE'
      EXPORTING
        archive_handle          = l_handle
        record_structure        = 'KONP'
        all_records_of_object   = 'X'
      TABLES
        table                   = lt_konp
      EXCEPTIONS
        end_of_object           = 1
        internal_error          = 2
        wrong_access_to_archive = 3
        OTHERS                  = 4.

    CHECK sy-subrc = 0 AND NOT lt_konp[] IS INITIAL.

    READ TABLE lt_konp INDEX 1.
    CHECK sy-subrc = 0.
    CLEAR p_kbetr.
    p_kbetr = lt_konp-kbetr.
    EXIT.
  ENDLOOP.

ENDFORM.                    " ARCHIVE_READ_KONP
