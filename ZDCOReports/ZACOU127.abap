*----------------------------------------------------------------------
* Program ID        : ZACOU127
* Title             : [CO] Purchasing Cost Reduction Analysis
* Created on        : 09/5/2007
* Created by        : I.G.MOON
* Specifications By : Andy Choi
* Description       : Purchasing Cost Reduction Analysis
*----------------------------------------------------------------------
REPORT zacou127 MESSAGE-ID zmco.

INCLUDE : z_moon_alv_top,
          z_moon_alv_fnc,
          zacou127_top.
*--------------------------------------------------------------------*
* SELECTION-SCREEN
*--------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK bl1 WITH FRAME TITLE text-001.

PARAMETERS     : p_kokrs LIKE ztco_shop_sum-kokrs OBLIGATORY
                 MEMORY ID cac,
                 p_bdatj LIKE ztco_shop_sum-bdatj OBLIGATORY
                 MEMORY ID bdtj,
                 p_poper LIKE ztco_shop_sum-poper OBLIGATORY
                 MEMORY ID popr.
SELECTION-SCREEN END OF BLOCK bl1.

SELECTION-SCREEN BEGIN OF BLOCK bl3 WITH FRAME TITLE text-003.
SELECT-OPTIONS : s_artnr FOR ztcou103-artnr MEMORY ID  mat,
                 s_compn FOR ztcou103-compn.
SELECTION-SCREEN END OF BLOCK bl3.

SELECTION-SCREEN BEGIN OF BLOCK bl4 WITH FRAME TITLE text-004.
PARAMETER: p_upd AS CHECKBOX DEFAULT false,
           p_dsp AS CHECKBOX DEFAULT true,
           p_fwng AS CHECKBOX DEFAULT true,
           p_colo AS CHECKBOX DEFAULT true.

SELECTION-SCREEN END OF BLOCK bl4.

SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE text-008.
PARAMETER: p_rev  AS CHECKBOX.                 "include revaluation

SELECT-OPTIONS : s_bklas  FOR ckmlmv011-bklas,
                 s_hkont  FOR bsis-hkont.
SELECTION-SCREEN END OF BLOCK b3.

* Layout
SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME TITLE text-010.
PARAMETER p_vari TYPE slis_vari.
SELECTION-SCREEN END OF BLOCK b4.

SELECTION-SCREEN BEGIN OF BLOCK view-result WITH FRAME TITLE text-t03.
SELECTION-SCREEN PUSHBUTTON  1(24) vslt USER-COMMAND vslt.
PARAMETERS: p_unit AS CHECKBOX DEFAULT false.
SELECTION-SCREEN END OF BLOCK view-result.

*----------------------------------------------------------------------*
* INITIALIZATION
*----------------------------------------------------------------------*
INITIALIZATION.
*  sy-title = '[CO] Purchasing Cost Reduction Analysis'.
  PERFORM default_.
*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_vari.
  PERFORM alv_variant_f4 CHANGING p_vari.

*----------------------------------------------------------------------*
START-OF-SELECTION.
*----------------------------------------------------------------------*

  PERFORM :
* Preparation for analyzing
            initialize            ,
            validate              ,
            read_103_table        ,
            read_106_table        ,
            get_product_qty       ,
            revaluation.

  PERFORM :
* Analyzing
            calc_total_unit_cost  ,
            revaluation_with_103  .
  PERFORM :
* Etc
            refine_row_itab       ,
            fill_text             .

  IF  p_upd   EQ true .
    PERFORM save_z_table  . " Save Anlysis data into ZTCOU127
    " & ZTCOU127_ERROR for Error
  ENDIF.

  PERFORM   move_out              .

*----------------------------------------------------------------------*

AT SELECTION-SCREEN.
  CASE sscrfields-ucomm.
* screen
    WHEN 'VSLT'.
      PERFORM view_.
  ENDCASE.

END-OF-SELECTION.
  CHECK g_error EQ space .
  PERFORM set_output .
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

  CHECK : p_dsp EQ true,
          g_error IS INITIAL.

  PERFORM init_alv_parm.

***   Initialization fieldcatalog   ***
  PERFORM fieldcat_init     USING gt_fieldcat[].
  PERFORM sort_build        USING gt_sort[].

  PERFORM alv_events_get    USING:  'P', 'T'.
  PERFORM alv_grid_display  TABLES  gt_out USING ''.

ENDFORM.                    " SET_OUTPUT
*&---------------------------------------------------------------------*
*&      Form  INIT_ALV_PARM
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
*&      Form  FIELDCAT_INIT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_FIELDCAT[]  text
*----------------------------------------------------------------------*
FORM fieldcat_init USING ft_fieldcat TYPE slis_t_fieldcat_alv .

  DATA: l_pos       TYPE i.

  __cls ft_fieldcat.

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
    append gs_fieldcat to  ft_fieldcat.
  END-OF-DEFINITION.


  __catalog :

    'X'  'ARTNR'    'FSC'              18  'CHAR' '' '',
    'X'  'UPGVC'    'UPG-VC'           18  'CHAR' '' '',
    'X'  'UPGVC_T'  'UPG-VC(T)'        20  'CHAR' '' '',
    'X'  'COMPN'    'Component'        18  'CHAR' '' '',
    'X'  'COMPN_T'  'Component(T)'     20  'CHAR' '' '',
    'X'  'KSTAR'    'Elem.'            10  'CHAR' '' '',
    ' '  'LIFNR'    'Vendor'           10  'CHAR' '' '',
    ' '  'EKGRP'    'PuG.'              3  'CHAR' '' '',
    ' '  'KZUST'    'Reason'            2  'CHAR' '' '',
    ' '  'SIGN'    '+/-'                1  'CHAR' '' '',
    ' '  'RSN_T'    'Reason(T)'        30  'CHAR' '' '',
    ' '  'ANPOPER'  'Period'            3  'CHAR' '' '',
    ' '  'WERTN'    'Amt'              13  'CURR' '' 'WAERS',
    ' '  'MENGE'    'Menge'            13  'QUAN' '' '',
    ' '  'MEEHT'    'Unit'              3  'CHAR' '' '',
    ' '  '$PRD_QTY'   'PRD.Qty'          13  'CHAR' '' '',
    ' '  'SOURCE'   'S'                1   'CHAR' '' ''.

  PERFORM change_fieldcat USING ft_fieldcat[] .

ENDFORM.                    " fieldcat_init
*&---------------------------------------------------------------------*
*&      Form  SORT_BUILD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_SORT[]  text
*----------------------------------------------------------------------*
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
           'ANPOPER'        ' ' 'X' 'X' 'X',
             'UPGVC'        ' ' 'X' 'X' 'X',
             'UPGVC_T'      ' ' 'X' 'X' 'X',
             'COMPN'        ' ' 'X' 'X' 'X',
             'COMPN_T'      ' ' 'X' 'X' 'X',
             'KSTAR'        ' ' 'X' 'X' 'X',
             'KZUST'        ' ' 'X' 'X' 'X',
           'ANPOPER'        ' ' 'X' 'X' 'X'.

ENDFORM.                    " SORT_BUILD

*&---------------------------------------------------------------------*
*&      Form  TOP_OF_PAGE
*&---------------------------------------------------------------------*
FORM top_of_page.
  DATA l_text(60).
  DATA s_text(60).

  REFRESH gt_listheader.

  l_text = 'Purchasing Cost Reduction Analysis.'.
  CONCATENATE p_bdatj '/' p_poper INTO s_text.
  PERFORM set_header_line USING:
          'P' 'H' ''                 l_text       '',
          'P' 'S' 'Controlling Area' p_kokrs      '',
          'P' 'S' 'Fisical Year/Preiod'     s_text      ''.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
       EXPORTING
            it_list_commentary = gt_listheader.

ENDFORM.                    "top_of_page
*---------------------------------------------------------------------*
*       FORM PF_STATUS_SET
*---------------------------------------------------------------------*
FORM pf_status_set USING  ft_extab TYPE slis_t_extab.
  IF p_upd EQ true.
    SET PF-STATUS '100' EXCLUDING 'SAVE'.
  ELSE.
    SET PF-STATUS '100' EXCLUDING ft_extab.
  ENDIF.
ENDFORM.                    "PF_STATUS_SET
*---------------------------------------------------------------------*
*       FORM USER_COMMAND                                             *
*---------------------------------------------------------------------*
FORM user_command USING fp_ucomm LIKE sy-ucomm
                        fs       TYPE slis_selfield.
  CLEAR : g_error.

  CASE fp_ucomm.
    WHEN 'SAVE'.
      PERFORM data_delete_confirm.
      CHECK g_error NE true.
      PERFORM save_z_table.
  ENDCASE.

ENDFORM.                    "USER_COMMAND
*&---------------------------------------------------------------------*
*&      Form  CHANGE_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_FT_FIELDCAT[]  text
*      -->P_ENDFORM  text
*----------------------------------------------------------------------*
FORM change_fieldcat USING    pt_fieldcat TYPE slis_t_fieldcat_alv.

  LOOP AT pt_fieldcat INTO gs_fieldcat.
    CASE gs_fieldcat-fieldname.
      WHEN 'LIFNR' OR 'EKGRP'  OR 'KZUST'.
        gs_fieldcat-ref_tabname = 'ZTCOU127'.
        gs_fieldcat-ref_fieldname = gs_fieldcat-fieldname.
      WHEN '$PRD_QTY'.
        gs_fieldcat-just = 'R'.
    ENDCASE.
    MODIFY pt_fieldcat FROM gs_fieldcat.
  ENDLOOP.

ENDFORM.                    " CHANGE_FIELDCAT
*&---------------------------------------------------------------------*
*&      Form  INITIALIZE_
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM initialize.
  CLEAR g_error.
  PERFORM get_date.
  PERFORM get_bukrs.
  PERFORM get_reason_info.
ENDFORM.                    " INITIALIZE_
*&---------------------------------------------------------------------*
*&      Form  VALIDATE_
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM validate.
  CLEAR tka01.

  SELECT SINGLE * FROM tka01
                 WHERE kokrs = p_kokrs.
  IF sy-subrc <> 0.
    MESSAGE s038 WITH p_kokrs.
    g_error = true.
  ENDIF.

  CHECK p_upd EQ true.

  PERFORM data_delete_confirm.

ENDFORM.                    " VALIDATE_
*&---------------------------------------------------------------------*
*&      Form  Read_table_
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_103_table.
  CHECK g_error EQ space.
  __process 'Read 103 Table...' '10'.

  __cls it_ztcou103.

  SELECT
              kalka  artnr  upgvc  compn
              werks  kstar
              typps  menge  meeht
              stkkz  gpreis wertn
              duty   frg    oth
              peinh kalnr

  INTO CORRESPONDING FIELDS OF TABLE it_ztcou103
  FROM ztcou103
  WHERE kokrs EQ p_kokrs
    AND bdatj EQ p_bdatj
    AND ( kalka EQ 'U1' OR kalka EQ 'UA' )
    AND poper EQ p_poper
    AND compn IN s_compn
    %_HINTS oracle 'FIRST_ROWS(10)'.

  IF sy-subrc NE 0.
*    MESSAGE S000 WITH 'Could not find at 103.'.
*    G_ERROR = TRUE.
    EXIT.
  ENDIF.

  __cls it_103.

  LOOP AT it_ztcou103.
    MOVE-CORRESPONDING it_ztcou103 TO it_103.
    APPEND it_103.
  ENDLOOP.

ENDFORM.                    " Read_table_
*&---------------------------------------------------------------------*
*&      Form  READ_106_TABLE_
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_106_table.

  CHECK g_error EQ space.
  __process 'Read 106 Table...' '20'.

  __cls it_ztcou106.

  SELECT
            id  AS artnr
            poper  AS anpoper
            upgvc  compn
            seq    kstar lifnr ekgrp
            kzust1 wertn1
            kzust2 wertn2
            kzust3 wertn3
            kpein  meeht dmenge AS menge

  INTO CORRESPONDING FIELDS OF TABLE it_ztcou106
  FROM ztcou106
  WHERE kokrs EQ p_kokrs
    AND bdatj EQ p_bdatj
    AND poper <= p_poper
    AND id IN s_artnr
    AND compn IN s_compn
    %_HINTS oracle 'FIRST_ROWS(10)'.

  IF sy-subrc NE 0.
    EXIT.
  ENDIF.

  LOOP AT it_ztcou106.
    MOVE-CORRESPONDING it_ztcou106 TO it_103.
    it_103-artnr = it_ztcou106-artnr.
    APPEND it_103.
  ENDLOOP.

  SORT it_103 BY artnr upgvc compn werks.
  DELETE ADJACENT DUPLICATES FROM it_103
            COMPARING artnr upgvc compn werks.


ENDFORM.                    " READ_106_TABLE_
*&---------------------------------------------------------------------*
*&      Form  GET_PRODUCT_QTY_
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_product_qty.
  CHECK p_unit EQ false.

  __process 'Read product quantity...' '30'.

  DATA : BEGIN OF it_proc_gr OCCURS 0,
          werks LIKE ztco_shop_sum-bwkey,
          artnr     LIKE ztco_shop_sum-artnr,
        END OF it_proc_gr.

  DATA : it_ckmlmv003_temp LIKE it_ckmlmv003 OCCURS 0 WITH HEADER LINE.

  LOOP AT it_103.
    MOVE-CORRESPONDING it_103 TO it_proc_gr.
    APPEND it_proc_gr.
    CLEAR it_proc_gr.
  ENDLOOP.

  SORT it_proc_gr.
  DELETE ADJACENT DUPLICATES FROM it_proc_gr.

  CHECK NOT it_proc_gr[] IS INITIAL.

  SELECT  b~bwkey b~matnr b~verid_nd
          c~aufnr
          a~out_menge
          a~meinh
    INTO CORRESPONDING FIELDS OF TABLE it_ckmlmv003_temp
    FROM
    ( ( ckmlmv003 AS a
    INNER JOIN ckmlmv001 AS b
       ON b~kalnr = a~kalnr_bal )
    INNER JOIN ckmlmv013 AS c
       ON c~kalnr_proc = a~kalnr_in )
     FOR ALL ENTRIES IN it_proc_gr
   WHERE a~mgtyp EQ '00001'
     AND a~gjahr EQ p_bdatj
     AND a~perio EQ p_poper
     AND a~werks EQ it_proc_gr-werks
     AND a~matnr EQ it_proc_gr-artnr
     AND b~btyp  =  'BF'
     AND b~bwkey EQ it_proc_gr-werks
     AND c~flg_wbwg = 'X'
     AND c~autyp = '05' .

  LOOP AT it_ckmlmv003_temp.
    MOVE-CORRESPONDING it_ckmlmv003_temp TO it_ckmlmv003.
    CLEAR: it_ckmlmv003-verid_nd,
           it_ckmlmv003-aufnr.
    COLLECT it_ckmlmv003. CLEAR it_ckmlmv003.
  ENDLOOP.

  SORT it_ckmlmv003 BY matnr.


ENDFORM.                    " GET_PRODUCT_QTY_
*&---------------------------------------------------------------------*
*&      Form  CALC_TOTAL_UNIT_COST_
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM calc_total_unit_cost.

  CHECK g_error EQ space.
  __process 'Calc total unit cost...' '60'.
  __cls it_row_tab.

* start calc

  DATA : $kzust(20),
         $wertn(20).

  FIELD-SYMBOLS : <_kzust>,<_wertn>.

  DATA : $seq(1) TYPE n.

  DATA : $flag(1),
         $prd_qty TYPE ckml_outmenge .

  LOOP AT it_ztcou106.

    AT NEW artnr.
      $flag = true.
    ENDAT.

    IF $flag EQ true.
      PERFORM get_$prd_qty USING it_ztcou106-artnr
                      CHANGING $prd_qty.
      CLEAR $flag.
    ENDIF.

    MOVE-CORRESPONDING it_ztcou106 TO it_row_tab.
    it_row_tab-artnr = it_ztcou106-artnr.

    DO 3 TIMES.
      $seq = sy-index.

      CONCATENATE:

              'IT_ZTCOU106-KZUST' $seq INTO $kzust,
              'IT_ZTCOU106-WERTN' $seq INTO $wertn.

      ASSIGN:
              ($kzust) TO <_kzust>,
              ($wertn) TO <_wertn>.

      CHECK NOT <_kzust> IS INITIAL.

      it_row_tab-kzust = <_kzust>.
      it_row_tab-wertn = <_wertn> * $prd_qty.

      IF it_row_tab-wertn <> 0.
        WRITE $prd_qty TO it_row_tab-$prd_qty UNIT it_ckmlmv003-meinh
              RIGHT-JUSTIFIED.
        it_row_tab-source = '6'.
        COLLECT it_row_tab .
      ENDIF.
    ENDDO.

  ENDLOOP.

  DATA $it_row_tab LIKE it_row_tab OCCURS 0 WITH HEADER LINE.

  DATA : $gpreis_ua LIKE ztcou103-gpreis,
         $wertn_ua  LIKE ztcou103-wertn,
         $duty_ua   LIKE ztcou103-duty,
         $frg_ua    LIKE ztcou103-frg,
         $oth_ua    LIKE ztcou103-oth,
         $peinh_ua  LIKE ztcou103-peinh,
         $n_prc_ua  LIKE ztcou103-wertn, " Net Price for UA
         $menge     LIKE ztcou103-menge.

  DATA : $gpreis_u1 LIKE ztcou103-gpreis,
         $wertn_u1  LIKE ztcou103-wertn,
         $duty_u1   LIKE ztcou103-duty,
         $frg_u1    LIKE ztcou103-frg,
         $oth_u1    LIKE ztcou103-oth,
         $peinh_u1  LIKE ztcou103-peinh,
         $n_prc_u1  LIKE ztcou103-wertn, " Net Price for U1
         $meeht     LIKE ztcou103-meeht,
         $kstar     LIKE ztcou103-kstar.

  SORT it_ztcou103 BY kalka artnr upgvc compn werks.

  LOOP AT it_103.

    CLEAR : $gpreis_ua,
            $wertn_ua,
            $duty_ua,
            $frg_ua,
            $oth_ua,
            $peinh_ua,
            $n_prc_ua,

            $gpreis_u1,
            $wertn_u1,
            $duty_u1,
            $frg_u1,
            $oth_u1,
            $peinh_u1,
            $n_prc_u1,
            $meeht ,$kstar .

    AT NEW artnr.
      $flag = true.
    ENDAT.

    IF $flag EQ true.
      PERFORM get_$prd_qty USING it_103-artnr
                      CHANGING $prd_qty.
      CLEAR $flag.
    ENDIF.

    READ TABLE it_ztcou103 WITH KEY  kalka = 'UA'
                                     artnr = it_103-artnr
                                     upgvc = it_103-upgvc
                                     compn = it_103-compn
                                     werks = it_103-werks
                                     BINARY SEARCH.
    IF sy-subrc EQ 0.
      $gpreis_ua = it_ztcou103-gpreis / it_ztcou103-peinh.
      $wertn_ua  = it_ztcou103-wertn.
      $duty_ua   = it_ztcou103-duty.
      $frg_ua    = it_ztcou103-frg.
      $oth_ua    = it_ztcou103-oth.
      $peinh_ua  = it_ztcou103-peinh.
      $n_prc_ua  = $wertn_ua - $frg_ua - $duty_ua - $oth_ua .
      $meeht     = it_ztcou103-meeht.
    ENDIF.

    READ TABLE it_ztcou103 WITH KEY  kalka = 'U1'
                                     artnr = it_103-artnr
                                     upgvc = it_103-upgvc
                                     compn = it_103-compn
                                     werks = it_103-werks
                                     BINARY SEARCH.

    IF sy-subrc EQ 0.
      $gpreis_u1 = it_ztcou103-gpreis / it_ztcou103-peinh.
      $wertn_u1  = it_ztcou103-wertn.
      $duty_u1   = it_ztcou103-duty.
      $frg_u1    = it_ztcou103-frg.
      $oth_u1    = it_ztcou103-oth.
      $peinh_u1  = it_ztcou103-peinh.
      $n_prc_u1  = $wertn_u1 - $frg_u1 - $duty_u1 - $oth_u1 .
      $menge     = it_ztcou103-menge.
      $kstar =     it_ztcou103-kstar.
    ENDIF.

    it_row_tab-artnr = it_103-artnr.
    it_row_tab-upgvc = it_103-upgvc.
    it_row_tab-compn = it_103-compn.
    it_row_tab-kzust = 'Z1'.
    it_row_tab-wertn = $n_prc_ua - $n_prc_u1 .
    it_row_tab-wertn = it_row_tab-wertn * $prd_qty.
    it_row_tab-menge = $menge.
    it_row_tab-meeht = $meeht.
    it_row_tab-kstar = $kstar.
    it_row_tab-anpoper = p_poper.

    IF it_row_tab-wertn <> 0.
      WRITE $prd_qty TO it_row_tab-$prd_qty UNIT it_ckmlmv003-meinh
            RIGHT-JUSTIFIED.
      it_row_tab-source = '3'.
      COLLECT it_row_tab.
    ENDIF.

    it_row_tab-kzust = 'Z5'.
    it_row_tab-wertn =  ( $frg_ua + $duty_ua + $oth_ua )
                      - ( $frg_u1 + $duty_u1 + $oth_u1 ) .
    it_row_tab-wertn = it_row_tab-wertn * $prd_qty.

    IF it_row_tab-wertn <> 0.
      WRITE $prd_qty TO it_row_tab-$prd_qty UNIT it_ckmlmv003-meinh
            RIGHT-JUSTIFIED.
      it_row_tab-source = '3'.
      COLLECT it_row_tab.
    ENDIF.

  ENDLOOP.

  DATA : BEGIN OF $matnr OCCURS 0,
       matnr TYPE matnr,
       poper TYPE poper,
       END OF $matnr.

  DATA : BEGIN OF $102 OCCURS 0,
       matnr TYPE matnr,
       poper TYPE poper,
       lifnr TYPE lifnr,
       ekgrp TYPE ekgrp,
       END OF $102.

  SORT it_ztcou106 BY artnr compn lifnr .
  LOOP AT it_row_tab WHERE lifnr EQ space OR ekgrp EQ space.
    g_ix = sy-tabix.
    READ TABLE it_ztcou106 WITH KEY artnr = it_row_tab-artnr
                                 compn = it_row_tab-compn
                                 BINARY SEARCH.
    IF sy-subrc EQ 0 AND
        it_ztcou106-lifnr NE space AND
        it_ztcou106-ekgrp NE space.
      it_row_tab-lifnr = it_ztcou106-lifnr.
      it_row_tab-ekgrp = it_ztcou106-ekgrp.
      MODIFY  it_row_tab INDEX g_ix TRANSPORTING lifnr ekgrp .
    ELSE.
      $matnr-matnr = it_row_tab-compn.
      IF it_row_tab-anpoper EQ '000'.
        $matnr-poper = p_poper.
      ELSE.
        $matnr-poper = it_row_tab-anpoper.
      ENDIF.
      COLLECT $matnr.
    ENDIF.

  ENDLOOP.

  CLEAR it_row_tab.

  CHECK NOT $matnr[] IS INITIAL.

  SELECT matnr poper lifnr ekgrp INTO TABLE $102
  FROM ztcou102
  FOR ALL ENTRIES IN $matnr
  WHERE  kokrs EQ p_kokrs
    AND  bdatj EQ p_bdatj
    AND  poper EQ $matnr-poper
    AND  kalka EQ 'U1'
    AND  ver EQ '000'
    AND  matnr EQ $matnr-matnr.
  SORT $102 BY matnr poper.

  DATA $poper TYPE poper.
  __cls $it_row_tab.

  LOOP AT it_row_tab WHERE lifnr EQ space OR ekgrp EQ space.
    g_ix = sy-tabix.
    IF it_row_tab-anpoper EQ '000'.
      $poper = p_poper.
    ELSE.
      $poper = it_row_tab-anpoper.
    ENDIF.

    IF it_row_tab-lifnr EQ space.
      READ TABLE $102 WITH KEY matnr = it_row_tab-compn
                               poper = $poper
                                   BINARY SEARCH.
      IF sy-subrc EQ 0.
        it_row_tab-lifnr = $102-lifnr.
        MODIFY  it_row_tab INDEX g_ix TRANSPORTING lifnr.
        IF it_row_tab-lifnr NE space.
          $it_row_tab = it_row_tab.
          COLLECT $it_row_tab.
        ENDIF.
      ENDIF.
    ENDIF.

    IF it_row_tab-ekgrp EQ space.
      READ TABLE $102 WITH KEY matnr = it_row_tab-compn
                               poper = $poper
                                   BINARY SEARCH.
      IF sy-subrc EQ 0.
        it_row_tab-ekgrp = $102-ekgrp.
        MODIFY  it_row_tab INDEX g_ix TRANSPORTING ekgrp.
        IF it_row_tab-ekgrp NE space.
          $it_row_tab = it_row_tab.
          COLLECT $it_row_tab.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDLOOP.

  SORT $it_row_tab BY artnr upgvc compn.

  LOOP AT it_row_tab WHERE lifnr EQ space OR ekgrp EQ space.
    g_ix = sy-tabix.
    IF it_row_tab-lifnr EQ space.
      READ TABLE $it_row_tab WITH KEY artnr = it_row_tab-artnr
                                     upgvc = it_row_tab-upgvc
                                     compn = it_row_tab-compn
                                   BINARY SEARCH.
      IF sy-subrc EQ 0.
        it_row_tab-lifnr = $it_row_tab-lifnr.
        MODIFY  it_row_tab INDEX g_ix TRANSPORTING lifnr.
      ENDIF.
    ENDIF.

    IF it_row_tab-ekgrp EQ space.
      READ TABLE $it_row_tab WITH KEY artnr = it_row_tab-artnr
                                     upgvc = it_row_tab-upgvc
                                     compn = it_row_tab-compn
                                   BINARY SEARCH.
      IF sy-subrc EQ 0.
        it_row_tab-ekgrp = $it_row_tab-ekgrp.
        MODIFY  it_row_tab INDEX g_ix TRANSPORTING ekgrp.
      ENDIF.
    ENDIF.

  ENDLOOP.

  CLEAR it_row_tab.

ENDFORM.                    " CALC_TOTAL_UNIT_COST_
*&---------------------------------------------------------------------*
*&      Form  get_$PRD_QTY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_ZTCOU106_ID  text
*      <--P_$PRD_QTY  text
*----------------------------------------------------------------------*
FORM get_$prd_qty USING    p_matnr
                CHANGING p_$prd_qty.

  p_$prd_qty = 0.

  IF p_unit EQ true.
    p_$prd_qty = 1.
    EXIT.
  ENDIF.

  p_$prd_qty = 0.
  READ TABLE it_ckmlmv003 WITH KEY matnr = p_matnr
                                          BINARY SEARCH.
  IF sy-subrc EQ 0.
    p_$prd_qty = it_ckmlmv003-out_menge.
  ENDIF.

ENDFORM.                    " get_$PRD_QTY
*&---------------------------------------------------------------------*
*&      Form  default_
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM default_.

  s_hkont-sign = 'I'.
  s_hkont-option = 'EQ'.
  s_hkont-low = '0000530180'.
  APPEND s_hkont.

  s_hkont-sign = 'I'.
  s_hkont-option = 'EQ'.
  s_hkont-low = '0000532100'.
  APPEND s_hkont.

  s_bklas-sign = 'I'.
  s_bklas-option = 'BT'.
  s_bklas-low  = '3000'.
  s_bklas-high = '3005'.
  APPEND s_bklas.

  WRITE:
          icon_biw_report_view AS ICON TO vslt,
         'View saved data' TO vslt+4(21).

ENDFORM.                    " default_
*&---------------------------------------------------------------------*
*&      Form  REVALUATION_
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM revaluation.
  CHECK g_error EQ space.
  CHECK p_rev = 'X'.

  __process 'Revaluation. step 1' '40'.

  __cls gt_mat_reval.

  PERFORM get_r_bwkey.
  PERFORM get_gt_mat_reval.

  __process 'Revaluation. step 2' '50'.

  PERFORM get_rv_bsis_bseg.

  " // PERFORM COLLECT_IT_RV_SUM.

ENDFORM.                    " REVALUATION_
*&---------------------------------------------------------------------*
*&      Form  get_r_bwkey
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_r_bwkey.

  DATA  lt_plant TYPE TABLE OF ty_t001k     WITH HEADER LINE.

  __cls : r_bwkey.

  LOOP AT it_103.
    lt_plant-bwkey = it_103-werks.
    COLLECT lt_plant.
  ENDLOOP.

  r_bwkey-sign   = 'I'.
  r_bwkey-option = 'EQ'.

  LOOP AT lt_plant.
    r_bwkey-low = lt_plant-bwkey.
    APPEND r_bwkey.
  ENDLOOP.

ENDFORM.                    " get_r_bwkey
*&---------------------------------------------------------------------*
*&      Form  GET_GT_MAT_REVAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_gt_mat_reval.

  __cls gt_mat_reval.

  DATA l_abrechdat LIKE ckmlhd-abrechdat.

  DATA: l_date LIKE sy-datum.
  SELECT SINGLE last_day INTO l_date FROM ckmlrunperiod
      INNER JOIN ckmlmv011
         ON  ckmlmv011~laufid = ckmlrunperiod~run_id
      WHERE gjahr = p_bdatj
        AND poper = p_poper
        AND bwkey IN r_bwkey.

  CHECK sy-subrc = 0.

  SELECT c~kalnr
         a~matnr a~werks
         a~mtart a~matkl a~kalst
         a~bklas
         e~stprs
         e~pvprs AS verpr
         e~peinh
         d~meins
         d~zukumo
         d~ekkumo
     INTO TABLE gt_mat_reval
           FROM ( ( ckmlmv011 AS a
                  INNER JOIN ckmlrunperiod AS b
                    ON  a~laufid = b~run_id )
                  INNER JOIN ckmlhd AS c
                    ON  c~kalnr = a~kalnr )
                  INNER JOIN ckmlpp AS d
                    ON  d~kalnr  = c~kalnr
                   AND  d~bdatj  = b~gjahr
                   AND  d~poper  = b~poper
                   AND  d~untper = space
                  INNER JOIN ckmlcr AS e
                    ON  e~kalnr  = d~kalnr
                   AND  e~bdatj  = d~bdatj
                   AND  e~poper  = d~poper
                   AND  e~curtp  = '10'
                   AND  e~untper = space
           WHERE b~gjahr EQ p_bdatj
             AND b~poper EQ p_poper
             AND a~bwkey IN r_bwkey
             AND a~bklas IN s_bklas
             AND a~matnr IN s_compn
             AND ( d~ekkumo <> 0
                OR d~zukumo <> 0
                OR d~pbpopo <> 0 )
%_HINTS ORACLE 'FIRST_ROWS(10)'
                .  " GR/IV

  SORT gt_mat_reval BY matnr.

ENDFORM.                    " GET_GT_MAT_REVAL
*&---------------------------------------------------------------------*
*&      Form  get_rv_bsis_bseg
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_rv_bsis_bseg.

  DATA l_buzei TYPE buzei.

  DATA: BEGIN OF $bseg OCCURS 0,
           bukrs LIKE bseg-bukrs,
           belnr LIKE bseg-belnr,
           zuonr LIKE bseg-zuonr,
           gjahr LIKE bseg-gjahr,
           buzei LIKE bseg-buzei,
           bwkey LIKE bseg-bwkey,
           ebeln LIKE bseg-ebeln,
           ebelp LIKE bseg-ebelp,
           menge LIKE bseg-menge,
           meins LIKE bseg-meins,
           peinh LIKE bseg-peinh,
  END OF $bseg.

  DATA: BEGIN OF $bsik OCCURS 0,
           bukrs LIKE bsik-bukrs,
           belnr LIKE bsik-belnr,
           gjahr LIKE bsik-gjahr,
           lifnr LIKE bsik-lifnr,
  END OF $bsik.

  DATA: BEGIN OF $bsak OCCURS 0,
           bukrs LIKE bsak-bukrs,
           belnr LIKE bsak-belnr,
           gjahr LIKE bsak-gjahr,
           lifnr LIKE bsak-lifnr,
  END OF $bsak.

  DATA : $bsis LIKE $bseg OCCURS 0 WITH HEADER LINE,
         $flag(1),
         $lifnr LIKE bsak-lifnr.

  DATA : BEGIN  OF $gt_bsis OCCURS 0,
           belnr TYPE belnr_d,
         END OF $gt_bsis.

  __cls : gt_bsis, gt_rv.

  CHECK NOT gt_mat_reval[] IS INITIAL.

  SELECT bukrs gjahr monat zuonr belnr buzei dmbtr bschl
    INTO CORRESPONDING FIELDS OF TABLE gt_bsis
    FROM bsis
   WHERE bukrs = p_bukrs
     AND hkont IN s_hkont
     AND budat BETWEEN gv_date_f AND gv_date_t
     AND zuonr IN s_compn.

  CHECK sy-subrc EQ 0.

  SORT gt_bsis BY belnr gjahr.

  LOOP AT gt_bsis.
    g_ix = sy-tabix.

* GR/GI exist... revaluation is relevant for allocation..
    READ TABLE gt_mat_reval WITH KEY matnr = gt_bsis-zuonr
        BINARY SEARCH.
    IF sy-subrc <> 0.
      gt_bsis-$matnr = gt_bsis-zuonr.
    ENDIF.

    CONCATENATE gt_bsis-belnr gt_bsis-gjahr INTO gt_bsis-$key.
    MODIFY gt_bsis INDEX g_ix TRANSPORTING $key $matnr.

    $gt_bsis-belnr = gt_bsis-belnr.
    APPEND $gt_bsis.
  ENDLOOP.

  SORT $gt_bsis.

  DELETE ADJACENT DUPLICATES FROM $gt_bsis.

  IF  NOT $gt_bsis[] IS INITIAL.
    SELECT bukrs
           belnr augdt augbl zuonr gjahr
           buzei rebzj rebzg rebzz
           bwkey ebeln ebelp menge meins peinh umsks
      INTO CORRESPONDING FIELDS OF TABLE $bseg
      FROM bseg
     FOR ALL ENTRIES IN $gt_bsis
     WHERE bukrs = p_bukrs
       AND belnr = $gt_bsis-belnr
       AND gjahr = p_bdatj
       AND matnr IN s_compn.

    SELECT bukrs belnr gjahr lifnr
      INTO CORRESPONDING FIELDS OF TABLE $bsak
      FROM bsak
     FOR ALL ENTRIES IN $gt_bsis
     WHERE bukrs = p_bukrs
       AND gjahr = p_bdatj
       AND belnr = $gt_bsis-belnr
       AND lifnr NE space.

    SELECT bukrs belnr gjahr lifnr
      INTO CORRESPONDING FIELDS OF TABLE $bsik
      FROM bsik
     FOR ALL ENTRIES IN $gt_bsis
     WHERE bukrs = p_bukrs
       AND gjahr = p_bdatj
       AND belnr = $gt_bsis-belnr
       AND lifnr NE space.
  ENDIF.

  SORT : $bseg BY bukrs belnr gjahr buzei,
         $bsak BY bukrs belnr gjahr,
         $bsik BY bukrs belnr gjahr,
         gt_bsis BY $key.

  LOOP AT gt_bsis.

    AT NEW $key.
      $flag = true.
    ENDAT.

    IF gt_bsis-bschl = 'H'.
      gt_bsis-dmbtr = - gt_bsis-dmbtr.
    ENDIF.

    MOVE-CORRESPONDING gt_bsis TO gt_rv.

    READ TABLE $bseg WITH KEY bukrs = gt_bsis-bukrs
                              belnr = gt_bsis-belnr
                              gjahr = gt_bsis-gjahr
                              buzei = gt_bsis-buzei
                              BINARY SEARCH.

    IF sy-subrc EQ 0.....

      MOVE-CORRESPONDING $bseg TO gt_rv.

      IF $flag EQ true.
        CLEAR $lifnr.
        READ TABLE $bsik WITH KEY bukrs = gt_bsis-bukrs
                                  belnr = gt_bsis-belnr
                                  gjahr = gt_bsis-gjahr
                                  BINARY SEARCH.
        IF sy-subrc EQ 0.
          $lifnr = $bsik-lifnr.
        ELSE.

          READ TABLE $bsak WITH KEY bukrs = gt_bsis-bukrs
                                    belnr = gt_bsis-belnr
                                    gjahr = gt_bsis-gjahr
                                    BINARY SEARCH.
          IF sy-subrc EQ 0.
            $lifnr = $bsak-lifnr.
          ENDIF.

        ENDIF.
        CLEAR $flag.
      ENDIF.
      gt_rv-lifnr = $lifnr.
      APPEND gt_rv.
      CLEAR gt_rv.
    ENDIF.....

  ENDLOOP.

  __cls it_rv_sum.

  DATA : BEGIN OF $ekgrp OCCURS 0,
             matnr TYPE matnr,
             ekgrp TYPE ekgrp,
         END OF $ekgrp.

  DATA : BEGIN OF $matnr OCCURS 0,
             matnr TYPE matnr,
         END OF $matnr.

  LOOP AT gt_rv.
    $matnr-matnr = gt_rv-zuonr.
    COLLECT $matnr.
  ENDLOOP.
  SORT $matnr.
  DELETE ADJACENT DUPLICATES FROM $matnr.

  SELECT matnr ekgrp
      INTO TABLE $ekgrp
      FROM marc
      FOR ALL ENTRIES IN $matnr
  WHERE matnr EQ $matnr-matnr.

  SORT $ekgrp BY matnr.

  LOOP AT gt_rv.
    it_rv_sum-kokrs = p_kokrs.                     " Controlling area
    it_rv_sum-gjahr = gt_rv-gjahr.                 " Fiscal year
    it_rv_sum-poper = gt_rv-monat.                 " Period

    IF gt_rv-zuonr <> space.
      IF p_colo = true.
        PERFORM matnr_expt_color  USING gt_rv-zuonr
                               CHANGING it_rv_sum-matnr.
        PERFORM matnr_expt_color  USING gt_rv-$matnr
                               CHANGING it_rv_sum-$matnr.
      ELSE.
        it_rv_sum-matnr = gt_rv-zuonr.                 " Material
        it_rv_sum-$matnr = gt_rv-$matnr.
      ENDIF.
    ENDIF.

    it_rv_sum-lifnr = gt_rv-lifnr.                 " Vendor
    it_rv_sum-menge = gt_rv-menge.                 " Qty.
    it_rv_sum-kzust = 'Z6'.                        " Reason code
    it_rv_sum-chnge = gt_rv-dmbtr.                 " Price
    it_rv_sum-meins = gt_rv-meins.

    READ TABLE $ekgrp WITH KEY matnr = gt_rv-zuonr BINARY SEARCH.
    IF sy-subrc EQ 0 AND $ekgrp-ekgrp NE space.
      it_rv_sum-ekgrp = $ekgrp-ekgrp.
    ENDIF.

    COLLECT it_rv_sum.
    CLEAR it_rv_sum.
  ENDLOOP.

ENDFORM.                    " get_rv_bsis_bseg
*&---------------------------------------------------------------------*
*&      Form  get_date
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_date.
  DATA: l_date(8).

  CLEAR: gv_info_f, gv_date_f, gv_date_t, gv_date3.

  CLEAR l_date.
  CONCATENATE '0101' p_bdatj INTO l_date.

  CALL FUNCTION 'CONVERT_DATE_INPUT'
       EXPORTING
            input  = l_date
       IMPORTING
            output = gv_info_f.

  CLEAR l_date.
  CONCATENATE p_poper+1(2) '01' p_bdatj  INTO l_date.

  CALL FUNCTION 'CONVERT_DATE_INPUT'
       EXPORTING
            input  = l_date
       IMPORTING
            output = gv_date_f.

  CALL FUNCTION 'LAST_DAY_IN_PERIOD_GET'
       EXPORTING
            i_gjahr = p_bdatj
            i_periv = 'K0'
            i_poper = p_poper
       IMPORTING
            e_date  = gv_date_t.

ENDFORM.                    " get_date
*&---------------------------------------------------------------------*
*&      Form  get_bukrs
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_bukrs.

  SELECT SINGLE bukrs INTO p_bukrs FROM tka02
            WHERE kokrs EQ p_kokrs .

ENDFORM.                    " get_bukrs
*&---------------------------------------------------------------------*
*&      Form  revaluation_with_103
*&---------------------------------------------------------------------*
*  1: allocate based on 103
*  2: others, allocated to all
*  issue: color part...
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM revaluation_with_103.
  CHECK g_error EQ space.
  CHECK p_rev = 'X'.
  __process 'Revaluation. step 3' '60'.

  DATA : m_idx LIKE sy-tabix,
         $chnge_u LIKE it_rv_sum-chnge,
         $ix LIKE sy-tabix.

  DATA : to_matnr LIKE it_row_tab-compn. " for Following Parts.

* colorless compare
  IF p_colo = true.
    LOOP AT it_ztcou103.
      PERFORM matnr_expt_color USING it_ztcou103-compn
                            CHANGING it_ztcou103-compn.
      MODIFY it_ztcou103 INDEX sy-tabix TRANSPORTING compn.
    ENDLOOP.
  ENDIF.

  SORT : it_ztcou103 BY compn artnr,
         it_rv_sum BY matnr .

  LOOP AT it_rv_sum.
    $ix = sy-tabix.
    CLEAR $chnge_u.
    IF it_rv_sum-menge <> 0.
      $chnge_u = -1 * it_rv_sum-chnge / it_rv_sum-menge.
    ENDIF.

    READ TABLE it_ztcou103 WITH KEY compn = it_rv_sum-$matnr
                              BINARY SEARCH.

    IF sy-subrc EQ 0.
      m_idx = sy-tabix.
      PERFORM add_revaluation_recode USING m_idx
                                           it_rv_sum-$matnr
                                           $chnge_u
                                           it_rv_sum-lifnr
                                           it_rv_sum-menge
                                           it_rv_sum-ekgrp.
      CONTINUE.
    ENDIF.

    CLEAR to_matnr.
    it_rv_sum-$flag = true. " no data

*    IF p_colo EQ true.
*
*      PERFORM matnr_expt_color  USING it_rv_sum-matnr
*                             CHANGING to_matnr.
*
*      READ TABLE it_ztcou103 WITH KEY compn = to_matnr
*                                BINARY SEARCH.
*      IF sy-subrc EQ 0.
*        CLEAR it_rv_sum-$flag.
*        PERFORM add_revaluation_recode USING m_idx
*                                             to_matnr
*                                             $chnge_u
*                                             it_rv_sum-lifnr
*                                             it_rv_sum-menge
*                                             it_rv_sum-ekgrp.
*
*      ENDIF.
*    ENDIF.

*FIXME...following part....
    IF p_fwng EQ true.
      IF to_matnr NE space.
        it_rv_sum-matnr = to_matnr.
      ENDIF.

      PERFORM get_following_part USING it_rv_sum-matnr
                              CHANGING to_matnr.
      IF to_matnr NE space.
        READ TABLE it_ztcou103 WITH KEY compn = to_matnr
                                  BINARY SEARCH.
        IF sy-subrc EQ 0.
          CLEAR it_rv_sum-$flag.
          PERFORM add_revaluation_recode USING m_idx
                                               to_matnr
                                               $chnge_u
                                           it_rv_sum-lifnr
                                           it_rv_sum-menge
                                           it_rv_sum-ekgrp.
        ENDIF.
      ENDIF.
    ENDIF.
    MODIFY it_rv_sum INDEX $ix TRANSPORTING $flag.
  ENDLOOP.

  PERFORM  add_not_assigned_rv_sum.

ENDFORM.                    " revaluation_with_103
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

* ( Deleting record by user selection criterion of artnr. )
  DELETE it_row_tab WHERE NOT artnr IN s_artnr.

  SORT it_row_tab BY artnr upgvc compn kzust.
  CLEAR it_row_tab.

  CHECK p_dsp EQ true.

  PERFORM get_mat_for_infor.

ENDFORM.                    " refine_row_itab
*&---------------------------------------------------------------------*
*&      Form  add_revaluation_recode
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_M_IDX  text
*----------------------------------------------------------------------*
FORM add_revaluation_recode USING    p_m_idx
                                     p_matnr
                                     p_chnge_u
                                     p_lifnr
                                     p_menge
                                     p_ekgrp.

  DATA : $flag(1),
         $prd_qty LIKE ckmlmv003-out_menge ,
         $tot_use LIKE it_ztcou103-menge.

  LOOP AT it_ztcou103 FROM p_m_idx.
    g_ix = sy-tabix.
    AT NEW artnr.
      $flag = true.
    ENDAT.
    IF $flag EQ true.
      CLEAR $flag.
      PERFORM get_$prd_qty USING it_ztcou103-artnr
                      CHANGING $prd_qty.
    ENDIF.
    IF it_ztcou103-compn NE p_matnr.
      EXIT.
    ENDIF.

    $tot_use = $tot_use + ( $prd_qty * it_ztcou103-menge ).

  ENDLOOP.

  DATA $factor TYPE p DECIMALS 4.

  LOOP AT it_ztcou103 FROM p_m_idx.
    g_ix = sy-tabix.

    AT NEW artnr.
      $flag = true.
    ENDAT.

    IF it_ztcou103-compn NE p_matnr.
      EXIT.
    ENDIF.

    IF $flag EQ true.
      CLEAR $flag.
      PERFORM get_$prd_qty USING it_ztcou103-artnr
                      CHANGING $prd_qty.
    ENDIF.

    it_row_tab-artnr   = it_ztcou103-artnr.
    it_row_tab-anpoper = p_poper.
    it_row_tab-kstar = it_ztcou103-kstar.
    it_row_tab-upgvc = it_ztcou103-upgvc.
    it_row_tab-compn = it_ztcou103-compn.
    it_row_tab-meeht = it_ztcou103-meeht.
    it_row_tab-kzust = 'Z6'.
    it_row_tab-lifnr = p_lifnr.
    it_row_tab-ekgrp = p_ekgrp.

*    IT_ROW_TAB-WERTN = IT_ZTCOU103-MENGE * P_CHNGE_U * $PRD_QTY.
    IF $tot_use  <> 0.
      $factor = $prd_qty / $tot_use .
      it_row_tab-wertn = p_chnge_u * ( p_menge * $factor ).
    ENDIF.

    it_row_tab-menge = it_ztcou103-menge.

    IF it_row_tab-wertn <> 0.
      WRITE $prd_qty TO it_row_tab-$prd_qty UNIT it_ckmlmv003-meinh
            RIGHT-JUSTIFIED.
      it_row_tab-source = 'R'.
      COLLECT it_row_tab.
    ENDIF.

  ENDLOOP.

ENDFORM.                    " add_revaluation_recode
*&---------------------------------------------------------------------*
*&      Form  GET_FOLLOWING_PART
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_RV_SUM_MATNR  text
*      <--P_TO_MATNR  text
*----------------------------------------------------------------------*
FORM get_following_part USING    p_from_matnr
                        CHANGING p_to_matnr.
  CHECK p_fwng EQ true.

  SELECT SINGLE tmatnr INTO p_to_matnr
    FROM ztcou105
   WHERE kokrs = p_kokrs
     AND fmatnr = p_from_matnr.

ENDFORM.                    " GET_FOLLOWING_PART
*&---------------------------------------------------------------------*
*&      Form  COLLECT_IT_RV_SUM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM collect_it_rv_sum.

  DATA : to_matnr LIKE it_row_tab-compn, " for Following Parts.
         $strlen TYPE i,
         $it_rv_sum LIKE it_rv_sum OCCURS 0 WITH HEADER LINE.

  LOOP AT it_rv_sum.
    g_ix = sy-tabix.

    IF p_colo EQ true.  "change to colorless
      PERFORM matnr_expt_color USING it_rv_sum-matnr
                            CHANGING it_rv_sum-matnr.
    ENDIF.

    CLEAR to_matnr.

    PERFORM get_following_part USING it_rv_sum-matnr
                            CHANGING to_matnr.

    IF to_matnr NE space.
      it_rv_sum-matnr = to_matnr.
    ENDIF.

    IF p_colo EQ true OR p_fwng EQ true.
      MODIFY it_rv_sum INDEX g_ix TRANSPORTING matnr.
    ENDIF.

  ENDLOOP.

  LOOP AT it_rv_sum.
    $it_rv_sum = it_rv_sum.
    COLLECT $it_rv_sum.
  ENDLOOP.

  __cls it_rv_sum.

  it_rv_sum[] = $it_rv_sum[].


ENDFORM.                    " COLLECT_IT_RV_SUM
*&---------------------------------------------------------------------*
*&      Form  GET_MATL_INFO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_MAT_TXT  text
*      -->P_IT_MATERIAL  text
*      -->P_2503   text
*----------------------------------------------------------------------*
FORM get_matl_info TABLES   p_it_mat_txt STRUCTURE it_mat_txt
                            p_it_material STRUCTURE it_material
                   USING    p_append.
  CHECK NOT p_it_material[] IS INITIAL.

  IF p_append EQ true.
    SELECT a~matnr b~werks
           a~raube b~fevor
           a~mtart a~matkl a~bismt
           b~vspvb b~prctr c~maktg
      APPENDING TABLE p_it_mat_txt
      FROM mara AS a
      INNER JOIN marc AS b
         ON a~matnr = b~matnr
      INNER JOIN makt AS c
         ON c~matnr = a~matnr
        AND c~spras = sy-langu
      FOR ALL ENTRIES  IN p_it_material
     WHERE b~matnr    = p_it_material-matnr .
  ELSE.
    SELECT a~matnr b~werks
           a~raube b~fevor
           a~mtart a~matkl a~bismt
           b~vspvb b~prctr c~maktg
      INTO TABLE p_it_mat_txt
      FROM mara AS a
      INNER JOIN marc AS b
         ON a~matnr = b~matnr
      INNER JOIN makt AS c
         ON c~matnr = a~matnr
        AND c~spras = sy-langu
      FOR ALL ENTRIES  IN p_it_material
     WHERE b~matnr    = p_it_material-matnr .
  ENDIF.

ENDFORM.                    " GET_MATL_INFO
*&---------------------------------------------------------------------*
*&      Form  FILL_TEXT_
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_text.
  CHECK g_error EQ space.
  __process 'Filling descriptions...' '80'.

  DATA : $ix LIKE sy-tabix,
         $rgrp2 LIKE it_reason_txt-rgrp2,
         $strlen TYPE i.

  SORT : it_mat_txt BY matnr,
         it_reason_txt BY rgrp2 .

  LOOP AT it_row_tab.
    $ix = sy-tabix.

    $strlen = strlen( it_row_tab-kzust ).
    IF $strlen >= 3.
      CONCATENATE it_row_tab-kzust(1) it_row_tab-kzust+2(1) INTO $rgrp2.

      IF it_row_tab-kzust+1(1) EQ 'D'.
        it_row_tab-sign = '-'.
      ELSEIF  it_row_tab-kzust+1(1) EQ 'U'.
        it_row_tab-sign = '+'.
      ELSEIF  it_row_tab-kzust+1(1) EQ 'E'.
        it_row_tab-sign = '='.
      ENDIF.
      it_row_tab-kzust = $rgrp2.

    ELSE.
      MOVE it_row_tab-kzust TO $rgrp2.
    ENDIF.

    READ TABLE it_reason_txt WITH KEY rgrp2 = $rgrp2
                             BINARY SEARCH.
    IF sy-subrc EQ 0.
      it_row_tab-rsn_t = it_reason_txt-text.
    ENDIF.

    MODIFY it_row_tab INDEX $ix TRANSPORTING sign kzust rsn_t.

  ENDLOOP.

  CHECK p_dsp EQ true.

  LOOP AT it_row_tab.
    $ix = sy-tabix.

    READ TABLE it_mat_txt WITH KEY matnr = it_row_tab-upgvc
                               BINARY SEARCH.
    IF sy-subrc EQ 0.
      it_row_tab-upgvc_t = it_mat_txt-maktg.
    ENDIF.

    READ TABLE it_mat_txt WITH KEY matnr = it_row_tab-compn
                               BINARY SEARCH.

    IF sy-subrc EQ 0.
      it_row_tab-compn_t = it_mat_txt-maktg.
    ENDIF.

    MODIFY it_row_tab INDEX $ix TRANSPORTING upgvc_t compn_t.

  ENDLOOP.

ENDFORM.                    " FILL_TEXT_
*&---------------------------------------------------------------------*
*&      Form  MOVE_OUT_
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM move_out.
  CHECK p_dsp EQ true.
  __process 'Preparing output...' '95'.

  __cls gt_out.

  LOOP AT it_row_tab.
    MOVE-CORRESPONDING it_row_tab TO gt_out.
    gt_out-waers = 'USD'.
    IF p_colo = 'X'.
      PERFORM matnr_expt_color USING gt_out-compn
                            CHANGING gt_out-compn.
    ENDIF.

    APPEND gt_out.
  ENDLOOP.

ENDFORM.                    " MOVE_OUT_
*&---------------------------------------------------------------------*
*&      Form  get_reason_info
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_reason_info.
  __cls it_reason_txt.

  SELECT rgrp2 text INTO TABLE it_reason_txt
    FROM ztcoum02 .

ENDFORM.                    " get_reason_info
*&---------------------------------------------------------------------*
*&      Form  SAVE_z_TABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM save_z_table.

  CHECK : g_error EQ false.

  __process 'Saving data...' '90'.

  DATA $ztcou127 LIKE ztcou127 OCCURS 0 WITH HEADER LINE.

  DELETE FROM ztcou127
    WHERE     kokrs        EQ    p_kokrs
      AND     bdatj        EQ    p_bdatj
      AND     poper        EQ    p_poper
      AND     artnr        IN    s_artnr
      AND     compn        IN    s_compn.

  LOOP AT it_row_tab.
    MOVE-CORRESPONDING it_row_tab TO $ztcou127.
    $ztcou127-kokrs = p_kokrs.
    $ztcou127-poper = p_poper.
    $ztcou127-bdatj = p_bdatj.
    $ztcou127-prd_qty = it_row_tab-$prd_qty.
    $ztcou127-aedat = sy-datum.
    $ztcou127-aenam = sy-uname.

    IF p_colo = 'X'.
      PERFORM matnr_expt_color USING $ztcou127-compn
                            CHANGING $ztcou127-compn.
    ENDIF.

    APPEND $ztcou127.
  ENDLOOP.

  MODIFY ztcou127 FROM TABLE $ztcou127.

* for dup. test {
*  LOOP AT $ZTCOU127.
*    INSERT ZTCOU127 FROM $ZTCOU127.
*    IF SY-SUBRC NE 0.
*      BREAK-POINT.
*    ENDIF.
*  ENDLOOP.
* }

  IF sy-subrc EQ 0.
    MESSAGE s000 WITH 'Data was saved successfully.' .
  ENDIF.

  COMMIT WORK.

  DATA $ztcou127_error LIKE ztcou127_error OCCURS 0 WITH HEADER LINE.

  DELETE FROM   ztcou127_error
  WHERE     kokrs        EQ    p_kokrs
    AND     bdatj        EQ    p_bdatj
    AND     poper        EQ    p_poper
    AND     matnr        IN    s_compn.

  LOOP AT it_rv_sum WHERE $flag EQ true.
    MOVE-CORRESPONDING it_rv_sum TO $ztcou127_error.
    $ztcou127_error-kokrs = p_kokrs.
    $ztcou127_error-bdatj = p_bdatj.
    $ztcou127_error-aedat = sy-datum.
    $ztcou127_error-aenam = sy-uname.
    $ztcou127_error-eflag = it_rv_sum-$flag.
    APPEND $ztcou127_error.
  ENDLOOP.

  MODIFY ztcou127_error FROM TABLE $ztcou127_error.

  COMMIT WORK.

ENDFORM.                    " SAVE_z_TABLE
*&---------------------------------------------------------------------*
*&      Form  VIEW_
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM view_.

  __cls it_row_tab.

  SELECT kokrs bdatj anpoper
         artnr upgvc compn
         lifnr kzust sign
         kstar wertn menge
         meeht prd_qty AS $prd_qty source prd_qty
  INTO CORRESPONDING FIELDS OF TABLE it_row_tab
  FROM ztcou127
    WHERE     kokrs        EQ    p_kokrs
      AND     bdatj        EQ    p_bdatj
      AND     poper        EQ    p_poper
      AND     artnr        IN    s_artnr
      AND     compn        IN    s_compn.

  IF sy-subrc <> 0.
    MESSAGE s000 WITH 'No analysis data was found.'.
    g_error = true.
    EXIT.
  ENDIF.

  DATA: lv_qty TYPE menge_d.
  IF p_unit EQ true.
    DATA $ix LIKE sy-tabix.
    LOOP AT it_row_tab.
      PERFORM check_num CHANGING it_row_tab-prd_qty.

      lv_qty = it_row_tab-prd_qty.
      CHECK lv_qty NE 0.
      $ix = sy-tabix.
      it_row_tab-wertn = it_row_tab-wertn / it_row_tab-prd_qty.
      MODIFY it_row_tab INDEX $ix TRANSPORTING wertn.
    ENDLOOP.
  ENDIF.

  PERFORM :            initialize            ,
                       get_mat_for_infor     ,
                       fill_text             ,
                       move_out              .

  CHECK g_error EQ space .

  DATA : $p_upd LIKE p_upd,
         $p_dsp LIKE p_dsp.

  $p_upd = p_upd.
  $p_dsp = p_dsp.

  p_upd = true.
  p_dsp = true.


  PERFORM set_output .

  p_upd = $p_upd.
  p_dsp = $p_dsp.

ENDFORM.                    " VIEW_
*&---------------------------------------------------------------------*
*&      Form  get_mat_for_infor
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_mat_for_infor.

  __cls : it_material,
          it_mat_txt.

  LOOP AT it_row_tab.
    AT NEW artnr.
      it_material-matnr = it_row_tab-artnr.
      it_material-mtype = 'P'.
      APPEND it_material.
    ENDAT.

    AT NEW upgvc.
      it_material-matnr = it_row_tab-upgvc.
      it_material-mtype = 'U'.
      APPEND it_material.
    ENDAT.

    AT NEW compn.
      it_material-matnr = it_row_tab-compn.
      it_material-mtype = 'M'.
      APPEND it_material.
    ENDAT.

    CLEAR it_material.
  ENDLOOP.

  SORT it_material BY matnr.
  DELETE ADJACENT DUPLICATES FROM it_material COMPARING matnr.

  PERFORM get_matl_info TABLES it_mat_txt
                               it_material
                         USING ' '.


ENDFORM.                    " get_mat_for_infor
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
FORM data_delete_confirm.
  DATA $exists(1).

  SELECT SINGLE * INTO *ztcou127
  FROM ztcou127
    WHERE     kokrs        EQ    p_kokrs
      AND     bdatj        EQ    p_bdatj
      AND     poper        EQ    p_poper
      AND     artnr        IN    s_artnr
      AND     compn        IN    s_compn.

  IF sy-subrc EQ 0.

    IF sy-subrc EQ 0.
      $exists = true.
    ENDIF.

  ENDIF.

  CHECK : $exists = true,
          sy-batch NE true.

  DATA l_answer(1).

  PERFORM pop_up USING
      'The Analysis data already exists.'
      'Do you want to refresh it?' ' '
                 CHANGING l_answer.

  IF l_answer NE 'J'.
    g_error = true.
    MESSAGE s000 WITH 'Processing was canceled by user.'.
  ENDIF.


ENDFORM.                    " DATA_DELETE_CONFIRM
*&---------------------------------------------------------------------*
*&      Form  ADD_NOT_ASSIGNED_RV_SUM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM add_not_assigned_rv_sum.

  DATA $it_rv_sum LIKE it_rv_sum OCCURS 0 WITH HEADER LINE.

  DATA: BEGIN OF lt_marc OCCURS 0,
          matnr LIKE marc-matnr,
          fevor LIKE marc-fevor,
        END OF lt_marc.

* MIP
*        {
  SELECT matnr fevor INTO TABLE lt_marc
     FROM marc
     FOR ALL ENTRIES IN it_rv_sum
     WHERE matnr = it_rv_sum-matnr
       AND fevor <> space.
  SORT lt_marc BY matnr.
*        }

  __cls $it_row_tab.

  LOOP AT it_row_tab WHERE source EQ '3'.
    MOVE-CORRESPONDING it_row_tab TO $it_row_tab.
    COLLECT $it_row_tab.
  ENDLOOP.

  LOOP AT it_rv_sum WHERE $flag = 'X'.
    READ TABLE lt_marc WITH KEY matnr =  it_rv_sum-matnr BINARY SEARCH.
    CHECK sy-subrc NE 0. " not MIP
    $it_rv_sum =  it_rv_sum.
    CLEAR : $it_rv_sum-matnr.
    COLLECT $it_rv_sum.
  ENDLOOP.


  DATA : $flag(1),
         $prd_qty LIKE ckmlmv003-out_menge ,
         $total LIKE it_row_tab-menge,
         $chnge_u LIKE it_rv_sum-chnge,
         $total_prd LIKE ckmlmv003-out_menge ,
         $vendor_rate TYPE p DECIMALS 5.

  SORT : $it_row_tab BY artnr lifnr,
         $it_rv_sum BY lifnr.

  CLEAR it_row_tab.

  LOOP AT $it_row_tab.
    AT NEW lifnr.
      $flag = true.
    ENDAT.

    CHECK $flag EQ true.
    CLEAR $flag.


* Distribute by vendor + pur.group

    READ TABLE $it_rv_sum WITH KEY lifnr = $it_row_tab-lifnr
        BINARY SEARCH.

    CHECK sy-subrc EQ 0.
    g_ix = sy-tabix.

    $chnge_u = 0.
    IF $it_rv_sum-menge <> 0.
      $chnge_u = $it_rv_sum-chnge. " / $IT_RV_SUM-MENGE.
    ENDIF.

    PERFORM get_prd_total CHANGING $total_prd.
    CHECK $total_prd <> 0.

    PERFORM get_$prd_qty USING $it_row_tab-artnr
                    CHANGING $prd_qty.

    PERFORM get_pr_total TABLES   $it_row_tab
                         USING    $it_row_tab-artnr
                         CHANGING $total.
    CHECK $total <> 0.
    $vendor_rate = $it_row_tab-menge / $total.

    it_row_tab-artnr   = $it_row_tab-artnr.
    it_row_tab-anpoper = p_poper.
    it_row_tab-meeht = $it_rv_sum-meins.
    it_row_tab-kzust = 'Z6'.
    it_row_tab-lifnr = $it_rv_sum-lifnr.
    it_row_tab-ekgrp = $it_rv_sum-ekgrp.

*    IT_ROW_TAB-WERTN = $CHNGE_U * ( $IT_ROW_TAB-MENGE / $TOTAL )
*                        * $PRD_QTY.

    it_row_tab-wertn = $chnge_u
                        * ( $prd_qty / $total_prd ).

    it_row_tab-menge = 1.

    IF it_row_tab-wertn <> 0.
      WRITE $prd_qty TO it_row_tab-$prd_qty UNIT it_ckmlmv003-meinh
            RIGHT-JUSTIFIED.
      it_row_tab-source = 'R'.
      COLLECT it_row_tab.
    ENDIF.

  ENDLOOP.

ENDFORM.                    " ADD_NOT_ASSIGNED_RV_SUM
*&---------------------------------------------------------------------*
*&      Form  GET_PR_TOTAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_$IT_ROW_TAB  text
*      -->P_$IT_ROW_TAB_ID  text
*      -->P_CHANGE  text
*      -->P_$TOTAL  text
*----------------------------------------------------------------------*
FORM get_pr_total         TABLES    p_it_row_tab STRUCTURE $it_row_tab
                           USING    p_id
                        CHANGING    p_total.
  p_total = 0.
  LOOP AT p_it_row_tab WHERE artnr EQ p_id.
    p_total = p_total + p_it_row_tab-menge.
  ENDLOOP.

ENDFORM.                    " GET_PR_TOTAL
*&---------------------------------------------------------------------*
*&      Form  get_prd_total
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_$TOTAL_PRD  text
*----------------------------------------------------------------------*
FORM get_prd_total CHANGING p_prd.

  p_prd = 0.

  LOOP AT it_ckmlmv003.
    ADD it_ckmlmv003-out_menge TO p_prd.
  ENDLOOP.

ENDFORM.                    " get_prd_total
*&---------------------------------------------------------------------*
*&      Form  matnr_expt_color
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_$MATNR_MATNR  text
*----------------------------------------------------------------------*
FORM matnr_expt_color USING p_f_matnr
                      CHANGING p_t_matnr.

  DATA $strlen TYPE i.
  $strlen = strlen( p_f_matnr ).

  IF p_f_matnr+5(2) EQ 'M1'.
    p_t_matnr = p_f_matnr(12).
  ELSE.
    IF $strlen > 11.
      CASE $strlen.
        WHEN 12 OR 13.
          p_t_matnr = p_f_matnr(10).
*      when 14 or 15.
*        p_t_matnr = p_f_matnr(12).
      ENDCASE.
    ELSE.
      p_t_matnr = p_f_matnr.
    ENDIF.
  ENDIF.

ENDFORM.                    " matnr_expt_color
*&---------------------------------------------------------------------*
*&      Form  CHECK_NUM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_IT_ROW_TAB_PRD_QTY  text
*----------------------------------------------------------------------*
FORM check_num CHANGING n_value.

  REPLACE : ',' WITH '' INTO n_value,
            ',' WITH '' INTO n_value,
            ',' WITH '' INTO n_value,
            ',' WITH '' INTO n_value.
  CONDENSE n_value NO-GAPS.
  IF n_value CN num. n_value = 0. ENDIF.

ENDFORM.
