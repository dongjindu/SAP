*----------------------------------------------------------------------
* Program ID        : ZACOU110
* Title             : [CO] Variance Analysis
* Created on        : 10/13/2006
* Created by        : Michelle Jeong
* Specifications By : Andy Choi
* Description       : Report for Variance Analysis
*----------------------------------------------------------------------
REPORT zacou110 NO STANDARD PAGE HEADING MESSAGE-ID zmco.

INCLUDE zacoui00.

CONSTANTS:  false VALUE ' ',
            true  VALUE 'X'.

TABLES: ztcou103,ztcou104,
        ztcou106,              " [CO] Calculate Variances
        ztcou102.              " [CO] Costing Result

TYPES: BEGIN OF ty_103,
         artnr  TYPE artnr,
         kstar  TYPE kstar,
         upgvc  TYPE zupgvc,  "summarization level
         compn  TYPE idnrk,   "summarization level

         wertn  TYPE zdwertn,
         duty   TYPE zdwertn,
         frg    TYPE zdwertn,
         oth    TYPE zdwertn,

         keyid  TYPE artnr,
         keytp(4),
         idtext TYPE zidtext,
         lifnr  TYPE lifnr,
         ekgrp  TYPE ekgrp,
       END OF ty_103.

TYPES: BEGIN OF ty_103_all,
         artnr  TYPE artnr,
         kstar  TYPE kstar,
         upgvc  TYPE zupgvc,  "summarization level
         compn  TYPE idnrk,   "summarization level
         poper  TYPE poper,

         wertn  TYPE zdwertn,
         duty   TYPE zdwertn,
         frg    TYPE zdwertn,
         oth    TYPE zdwertn,

         keyid  TYPE artnr,
         keytp(4),
         idtext TYPE zidtext,
         lifnr  TYPE lifnr,
         ekgrp  TYPE ekgrp,
       END OF ty_103_all.

TYPES: BEGIN OF ty_104_map,
         id  TYPE artnr,
         keytp(4),
         bdatj  TYPE bdatj,
         poper  TYPE poper,
         keyid  TYPE artnr,
       END OF ty_104_map.

TYPES: BEGIN OF ty_106,
         kokrs  TYPE kokrs,
         kalka  TYPE ck_kalka,
         bdatj  TYPE bdatj,
         id     TYPE zid1,

         kstar  TYPE kstar,
         upgvc  TYPE zupgvc,  "summarization level
         compn  TYPE idnrk,   "summarization level
         lifnr  TYPE lifnr,

         poper  TYPE poper,
         kzust1 TYPE kzust,
         wertn1 TYPE zdwertn,
         kzust2 TYPE zkzust2,
         wertn2 TYPE zwertn2_1,
         kzust3 TYPE zkzust3,
         wertn3 TYPE zwertn3,
         idtext TYPE zidtext,
* UD1K941594 by IG.MOON 9/18/07
* {
         zrclss TYPE zrclss,
         infrsn TYPE zkzust1,
         ekgrp  TYPE ekgrp,
* }

       END OF ty_106.

TYPES: BEGIN OF ty_rgrp,
         rgrp2   TYPE zrgrp2,
         grp1	 TYPE zrgrp1,
         alu	 TYPE zcorsc,
         ald	 TYPE zcorsc,
         ale	 TYPE zcorsc,
         alx	 TYPE zcorsc,
       END OF ty_rgrp.

TYPES: BEGIN OF ty_itab,
         kokrs     TYPE kokrs,       " Cotrolling Area
         bdatj     TYPE bdatj,       " Fiscal Year
         poper     TYPE poper,
         id        TYPE zid1,        " ID

         kstar     TYPE kstar,       " Cost element
         upgvc  TYPE zupgvc,  "summarization level
         compn  TYPE idnrk,   "summarization level
         lifnr  TYPE lifnr,

         unitf     TYPE zwertn3,     "Fr amount
         unitt     TYPE zwertn3,     "To amount

         start     TYPE zwertn3,     "Fr amount
         end       TYPE zwertn3,     "To amount
         sttmat    TYPE matnr,
         endmat    TYPE matnr,

         amt1      TYPE zwertn3,     " Amount
         amt2      TYPE zwertn3,
         amt3      TYPE zwertn3,
         amt4      TYPE zwertn3,
         amt5      TYPE zwertn3,
         amt6      TYPE zwertn3,
         amt7      TYPE zwertn3,
         amt8      TYPE zwertn3,
         amt9      TYPE zwertn3,
         amt10     TYPE zwertn3,
         amt11     TYPE zwertn3,
         amt12     TYPE zwertn3,
         amt13     TYPE zwertn3,
         amt14     TYPE zwertn3,
         amt15     TYPE zwertn3,
         amt16     TYPE zwertn3,
         amt17     TYPE zwertn3,
         amt18     TYPE zwertn3,
         amt19     TYPE zwertn3,
         amt20     TYPE zwertn3,
         sum1      TYPE zwertn3,     " Purchasing Sum
         sum2      TYPE zwertn3,     " R&D Sum
         sum3      TYPE zwertn3,     " Etc
         sum4      TYPE zwertn3,     " Others
         tot       TYPE zwertn3,     " Total
         idtext    TYPE zidtext,     " ID Desc.
         amt99     TYPE zwertn3,     " Not assigned
* UD1K941594 by IG.MOON 9/18/07
* {
         zrclss TYPE zrclss,
         infrsn TYPE zkzust1,
         upgvc_t   TYPE maktg,
         compn_t   TYPE maktg,
         ekgrp    TYPE ekgrp,
* }
       END OF ty_itab.

TYPES: BEGIN OF ty_mon,
         id        TYPE zid1,        " ID
         bdatj     TYPE bdatj,       " Fiscal Year
         poper     TYPE poper,
       END OF ty_mon.

TYPES: BEGIN OF ty_prd,
         id        TYPE zid1,        " ID
         bdatj     TYPE bdatj,       " Fiscal Year
         poper     TYPE poper,
         prd       TYPE zid1,
       END OF ty_prd.

TYPES: BEGIN OF ty_out.
INCLUDE TYPE ty_itab.
TYPES:   chk,
         tabcolor  TYPE slis_t_specialcol_alv,
       END OF ty_out.

TYPES: BEGIN OF ty_ztcoum01.
INCLUDE TYPE ztcoum01.
TYPES:   idx TYPE sytabix,
       END OF ty_ztcoum01.

TYPES: BEGIN OF ty_resp,
         zresp TYPE zcorsp,
       END OF ty_resp.

DATA: gt_103f     TYPE TABLE OF ty_103      WITH HEADER LINE,
      gt_103t     TYPE TABLE OF ty_103      WITH HEADER LINE,
      gt_103p     TYPE TABLE OF ty_103      WITH HEADER LINE,
      gt_103_all  TYPE TABLE OF ty_103_all      WITH HEADER LINE,
      gt_106      TYPE TABLE OF ty_106      WITH HEADER LINE,
      gt_rgrp     TYPE TABLE OF ty_rgrp     WITH HEADER LINE,
      gt_resp     TYPE TABLE OF ty_resp     WITH HEADER LINE,
      gt_ztcoum01 TYPE TABLE OF ty_ztcoum01 WITH HEADER LINE,
      itab        TYPE TABLE OF ty_itab     WITH HEADER LINE,
      gt_out      TYPE TABLE OF ty_out      WITH HEADER LINE,
      gt_103b     TYPE TABLE OF ty_103_all  WITH HEADER LINE,
      gt_104      LIKE ztcou104 OCCURS 0 WITH HEADER LINE,
      gt_104_map  TYPE TABLE OF ty_104_map  WITH HEADER LINE,
      gt_mon_min  TYPE TABLE OF ty_mon  WITH HEADER LINE,
      gt_mon_max  TYPE TABLE OF ty_mon  WITH HEADER LINE,
      gt_from_prd TYPE TABLE OF ty_prd  WITH HEADER LINE,
      gt_to_prd   TYPE TABLE OF ty_prd  WITH HEADER LINE.


DATA  gv_abp.

FIELD-SYMBOLS <fs>.

*----------------------------------------------------------------------
* Selection-Screen
*----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK b0 WITH FRAME.
PARAMETERS: p_kokrs LIKE ztcou106-kokrs OBLIGATORY
                                        MEMORY ID cac
                                        MATCHCODE OBJECT fc_kokrs,
            p_year  LIKE ztcou106-bdatj OBLIGATORY MEMORY ID bdtj,
            p_kalka LIKE ztcou106-kalka MEMORY ID kka.
SELECT-OPTIONS: s_id    FOR ztcou106-id MATCHCODE OBJECT zid,
                s_poper FOR ztcou102-poper,
* UD1K941594 by IG.MOON 9/18/07
* {
                s_zrclss FOR ztcou106-zrclss,
                s_ekgrp FOR ztcou106-ekgrp.
* }
SELECTION-SCREEN END OF BLOCK b0.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.
SELECT-OPTIONS: s_upgvc FOR ztcou106-upgvc,
                s_compn FOR ztcou106-compn,
                s_lifnr for ztcou106-lifnr.
PARAMETERS p_dbg LIKE mara-matnr.
SELECTION-SCREEN END OF BLOCK b2.

* Layout
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
PARAMETERS: p_all AS CHECKBOX,
            p_upg AS CHECKBOX,
            p_com AS CHECKBOX,
            p_vnd AS CHECKBOX,
            p_cst AS CHECKBOX,
            p_ekg AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK b1.

PARAMETERS: p_vari TYPE slis_vari.

DEFINE __cls.                          " clear & refresh
  clear &1.refresh &1.
END-OF-DEFINITION.

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
  PERFORM disp_result.

*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       Get Costing Data
*----------------------------------------------------------------------*
FORM get_data.

* Get Calculate Variances Data from table ZTCOU106
  CLEAR gt_106.
  REFRESH gt_106.

  SELECT a~kokrs a~kalka a~bdatj a~id a~kstar a~upgvc a~compn a~lifnr
         a~poper a~kzust1 a~wertn1
         a~kzust2 a~wertn2 a~kzust3 a~wertn3 b~idtext
* UD1K941594 by IG.MOON 9/18/07
* {
           a~zrclss
           d~kzust1 AS infrsn
           a~ekgrp
* }
          INTO CORRESPONDING FIELDS OF TABLE gt_106
    FROM ztcou106 AS a
    JOIN ztcou104 AS b
      ON b~kokrs = a~kokrs
    AND b~bdatj = a~bdatj
    AND b~kalka = a~kalka
    AND b~id = a~id
* UD1K941594 by IG.MOON 9/18/07
* {
      LEFT OUTER JOIN ztcou102 AS d
        ON  d~kokrs EQ a~kokrs
        AND d~bdatj EQ a~bdatj
        AND d~poper EQ a~poper
        AND d~kalka EQ a~kalka
        AND d~ver   EQ '0'
        AND d~matnr EQ a~compn
* }
   WHERE a~kokrs = p_kokrs
     AND a~bdatj = p_year
     AND a~kalka = p_kalka
     AND a~id IN s_id
     AND a~poper IN s_poper
     AND a~zrclss IN s_zrclss
     AND a~ekgrp IN s_ekgrp
     AND a~upgvc IN s_upgvc
     AND a~compn IN s_compn
     and a~lifnr in s_lifnr.

* ABP-BASE: period = 0
  PERFORM check_abp_102.

* Create Reason Codes Internal Table GT_RGRP from table ZTCOUM02
  PERFORM get_gt_rgrp.

* Create Internal Table GT_ZTCOUM01 for Responsible Analysis Codes
  PERFORM get_gt_ztcoum01.

* Create Internal Table for Reason
  PERFORM get_gt_103.

  PERFORM collect_by_level.

  PERFORM get_gt_rsn.

* Create Internal Tables for Variance Analysis
  PERFORM get_gt_out.

ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  DISP_RESULT
*&---------------------------------------------------------------------*
*       Display Cost Review
*----------------------------------------------------------------------*
FORM disp_result.
  CLEAR: gt_fieldcat, gs_layout, gt_events, gs_variant,
         gt_fieldcat[], gt_events[].

  PERFORM build_field_category USING:
    'ID'         'X'  'ID'               10   'CHAR',
    'IDTEXT'     'X'  'Description'      25   'CHAR',
    'POPER'      'X'  'Period'           03   'CHAR',
    'KSTAR'      'X'  'Cost element'     10   'CHAR',
    'UPGVC'      'X'  'UPG-VC'           16   'CHAR',
    'UPGVC_T'    'X'  'UPG-VC.T'         30   'CHAR',
    'COMPN'      'X'  'Component'        18   'CHAR',
    'COMPN_T'    'X'  'Comp.T'           30   'CHAR',
    'LIFNR'      'X'  'Vendor'           10   'CHAR',
    'EKGRP'      'X'  'PurG'              3   'CHAR',
    'INFRSN'     'X'  'Info.'             3   'CHAR'.

  IF p_all = 'X'.
    PERFORM build_field_category USING:
      'UNITF'      ' '  'Start U.Cost'    15   'CURR',
      'START'      ' '  'From(M)'            15   'CURR',
      'END'        ' '  'To(M)'              15   'CURR',
      'STTMAT'     ' '  'From Prod'          18   'CURR',
      'ENDMAT'     ' '  'To Prod'            18   'CURR',
      'UNITT'      ' '  'End U.Cost'    15   'CURR'.
  ENDIF.

  PERFORM build_field_category USING:
    'TOT'        ' '  'Total'            15   'CURR'.

  PERFORM modify_fieldcat.

  PERFORM build_field_category USING:
    'AMT99'      ' '  'NotAssigned'      15   'CURR'.


  LOOP AT gt_fieldcat INTO gs_fieldcat.

    IF gs_fieldcat-fieldname EQ 'KZUST' OR
        gs_fieldcat-fieldname EQ 'INFRSN'.
      gs_fieldcat-ref_tabname   = 'ZTCOUM02'.
      gs_fieldcat-ref_fieldname   = 'RGRP2'.
      MODIFY gt_fieldcat FROM gs_fieldcat TRANSPORTING ref_tabname
                                               ref_fieldname
                     WHERE fieldname = gs_fieldcat-fieldname.
    ENDIF.

    IF gs_fieldcat-fieldname EQ 'EKGRP'.
      gs_fieldcat-ref_tabname   = 'ZTCOU106'.
      gs_fieldcat-ref_fieldname   = 'EKGRP'.
      MODIFY gt_fieldcat FROM gs_fieldcat TRANSPORTING ref_tabname
                                               ref_fieldname
                     WHERE fieldname = gs_fieldcat-fieldname.
    ENDIF.

    IF gs_fieldcat-datatype   EQ 'CURR'.
      gs_fieldcat-no_zero = 'X'.
      MODIFY gt_fieldcat FROM gs_fieldcat TRANSPORTING no_zero
                     WHERE fieldname = gs_fieldcat-fieldname.
    ENDIF.


  ENDLOOP.

  CLEAR gs_layout.
  gs_layout-zebra             = 'X'.
  gs_layout-colwidth_optimize = 'X'.
  gs_layout-box_fieldname     = 'CHK'.
  gs_layout-coltab_fieldname  = 'TABCOLOR'.

  PERFORM set_color.
  PERFORM comment1 USING gt_list_top_of_page.
  PERFORM set_events CHANGING gt_events.

  gv_repid = gs_variant-report = sy-repid.

* Set variant
  gs_variant-variant = p_vari.

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
FORM comment1 USING lt_top_of_page TYPE slis_t_listheader.
  DATA ls_line TYPE slis_listheader.

  CLEAR ls_line.
  ls_line-typ  = 'S'.

  ls_line-key  = 'Controlling Area:'.
  ls_line-info = p_kokrs.
  APPEND ls_line TO lt_top_of_page.

  ls_line-key  = 'Fiscal Year:'.
  ls_line-info = p_year.
  APPEND ls_line TO lt_top_of_page.

  IF NOT s_poper IS INITIAL.
    ls_line-key  = 'Period:'.
    IF s_poper-high IS INITIAL.
      ls_line-info = s_poper-low.
    ELSE.
      CONCATENATE s_poper-low '~' s_poper-high INTO ls_line-info.
    ENDIF.
    APPEND ls_line TO lt_top_of_page.
  ENDIF.

  CLEAR: ls_line-key, ls_line-info.
  APPEND ls_line TO lt_top_of_page.

ENDFORM.                                                    " COMMENT1
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
  RANGES: r_id    FOR ztcou106-id,
          r_poper FOR ztcou106-poper.

  CLEAR:   r_id, r_poper.
  REFRESH: r_id, r_poper.

  IF r_ucomm = 'DET'.
    r_id-sign = 'I'.
    r_id-option = 'EQ'.

    LOOP AT gt_out WHERE chk = 'X'.
      r_id-low = gt_out-id.
      APPEND r_id.
    ENDLOOP.
    CLEAR r_id.

    r_poper-sign = 'I'.
    r_poper-option = 'BT'.
    r_poper-low = '001'.

    IF gv_abp = 'X'.
      r_poper-high = '013'.     " Exist ABP
    ELSE.
      r_poper-high = '012'.
    ENDIF.

    APPEND r_poper.
    CLEAR r_poper.

*   Call Variance List
    SUBMIT zacou114 WITH p_kokrs = p_kokrs
                    WITH p_year  = p_year
                    WITH p_kalka = p_kalka
                    WITH s_id IN r_id
                    WITH s_poper IN s_poper AND RETURN.

    rs_selfield-refresh = 'X'.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SET_COLOR
*&---------------------------------------------------------------------*
FORM set_color.
  CLEAR: gs_specialcol, gt_specialcol[], gt_out-tabcolor[].

  gs_specialcol-fieldname = 'TOT'.
  gs_specialcol-color-col = cl_gui_resources=>list_col_negative.
  gs_specialcol-color-int = 0.
  APPEND gs_specialcol TO gt_specialcol.

  gs_specialcol-fieldname = 'SUM1'.
  gs_specialcol-color-col = cl_gui_resources=>list_col_total.
  gs_specialcol-color-int = 0.
  APPEND gs_specialcol TO gt_specialcol.

  gs_specialcol-fieldname = 'SUM2'.
  gs_specialcol-color-col = cl_gui_resources=>list_col_total.
  APPEND gs_specialcol TO gt_specialcol.

  gs_specialcol-fieldname = 'SUM3'.
  gs_specialcol-color-col = cl_gui_resources=>list_col_total.
  APPEND gs_specialcol TO gt_specialcol.

  gs_specialcol-fieldname = 'SUM4'.
  gs_specialcol-color-col = cl_gui_resources=>list_col_total.
  APPEND gs_specialcol TO gt_specialcol.

  gt_out-tabcolor[] = gt_specialcol[].
  MODIFY gt_out TRANSPORTING tabcolor WHERE tabcolor IS initial.

ENDFORM.                    " SET_COLOR
*&---------------------------------------------------------------------*
*&      Form  GET_GT_RSN
*&---------------------------------------------------------------------*
FORM get_gt_rsn.
* Collect data to Internal Table ITAB

  LOOP AT gt_106.

    IF gt_106-compn EQ p_dbg AND p_dbg NE space.
      BREAK-POINT.
    ENDIF.

    IF gt_106-poper = '000'.         " ABP
      gv_abp = 'X'.
    ENDIF.

    CLEAR itab.
    itab-kokrs  = gt_106-kokrs.      " Cotrolling Area
    itab-bdatj  = gt_106-bdatj.      " Fiscal Year
    itab-poper  = gt_106-poper.      " Fiscal Year
    itab-id     = gt_106-id.         " ID
    itab-idtext = gt_106-idtext.     " ID Desc.

    PERFORM get_amt_by_reason USING gt_106-kzust1
                                    gt_106-wertn1.

    PERFORM get_amt_by_reason USING gt_106-kzust2
                                    gt_106-wertn2.

    PERFORM get_amt_by_reason USING gt_106-kzust3
                                    gt_106-wertn3.

    COLLECT itab.
    CLEAR itab.

  ENDLOOP.

  CLEAR itab.

*  SORT itab BY bdatj poper id kstar upgvc compn .

  LOOP AT gt_103b.

    IF gt_103b-keytp EQ 'ABP'.
      READ TABLE itab WITH KEY   bdatj = p_year
                                 poper = '000'
                                 id    = gt_103b-keyid
                                 kstar = gt_103b-kstar
                                 upgvc = gt_103b-upgvc
                                 compn = gt_103b-compn
                                 lifnr = gt_103b-lifnr
                                 ekgrp = gt_103b-ekgrp.

      IF sy-subrc NE 0.
        CLEAR itab.
        MOVE-CORRESPONDING gt_103b TO itab.

        IF itab-compn EQ p_dbg AND p_dbg NE space.
          sy-calld = space.
        ENDIF.

        itab-id = gt_103b-keyid.
        itab-kokrs = p_kokrs.
        itab-bdatj = p_year.
        itab-poper = '000'.

        APPEND itab.
        CLEAR itab.
      ENDIF.
    ELSE.
      READ TABLE itab WITH KEY   bdatj = p_year
                                 poper = gt_103b-poper
                                 id    = gt_103b-keyid
                                 kstar = gt_103b-kstar
                                 upgvc = gt_103b-upgvc
                                 compn = gt_103b-compn
                                 lifnr = gt_103b-lifnr
                                 ekgrp = gt_103b-ekgrp
                                 .
      IF sy-subrc NE 0.
        IF NOT gt_103b-poper IN s_poper.
          CONTINUE.
        ENDIF.

        READ TABLE itab WITH KEY   bdatj = p_year
                                   poper = gt_103b-poper
                                   id    = gt_103b-keyid.
        IF sy-subrc NE 0.
          CONTINUE.
        ENDIF.

        CLEAR itab.
        MOVE-CORRESPONDING gt_103b TO itab.

        IF itab-compn EQ p_dbg AND p_dbg NE space.
          sy-calld = space.
        ENDIF.

        itab-id = gt_103b-keyid.
        itab-kokrs = p_kokrs.
        itab-bdatj = p_year.

        APPEND itab.
        CLEAR itab.
      ENDIF.
    ENDIF.

  ENDLOOP.

  CLEAR itab.

  SORT itab BY bdatj poper id kstar upgvc compn.
  SORT gt_104_map BY id keytp bdatj poper.
  SORT gt_103b BY keyid keytp poper kstar upgvc compn lifnr ekgrp.
  SORT gt_103f BY artnr kstar upgvc compn lifnr ekgrp.

  DATA $ix TYPE i.
  DATA $poper TYPE poper.
  DATA $year TYPE bdatj.
  DATA $flag.

  __cls : gt_mon_min,gt_mon_max.

  LOOP AT itab.
    AT NEW id.
      $flag = true.
    ENDAT.
    CHECK $flag EQ true.
    CLEAR $flag.
    IF itab-poper NE 0.
      MOVE-CORRESPONDING itab TO gt_mon_min.
      APPEND gt_mon_min.
    ENDIF.
  ENDLOOP.
  gt_mon_max[] = gt_mon_min[].

  SORT gt_mon_min BY  id  bdatj poper.
  SORT gt_mon_max BY  id  bdatj poper DESCENDING.

  LOOP AT gt_mon_min.
    AT NEW bdatj.
      $flag = true.
    ENDAT.
    CHECK $flag EQ true.
    DELETE gt_mon_min WHERE poper NE  gt_mon_min-poper.
  ENDLOOP.
  SORT gt_mon_min BY  id  bdatj poper.

  LOOP AT gt_mon_max.
    AT NEW bdatj.
      $flag = true.
    ENDAT.
    CHECK $flag EQ true.
    DELETE gt_mon_max WHERE poper NE  gt_mon_max-poper.
  ENDLOOP.
  SORT gt_mon_max BY  id  bdatj poper.

  LOOP AT itab.

    $ix = sy-tabix.
    IF itab-compn EQ p_dbg AND p_dbg NE space.
      BREAK-POINT.
    ENDIF.

    IF itab-poper EQ s_poper-low.

      IF itab-poper EQ '000'.
        READ TABLE gt_103b WITH KEY keyid = itab-id
                                    keytp = 'ABP'
                                    poper = '001'
                                    kstar = itab-kstar
                                    upgvc = itab-upgvc
                                    compn = itab-compn
                                    lifnr = itab-lifnr
                                    ekgrp = itab-ekgrp
                                    BINARY SEARCH.
        IF sy-subrc EQ 0.
          itab-unitf  = gt_103b-wertn.
          itab-start  = gt_103b-wertn.
          itab-sttmat = gt_103b-artnr.
        ENDIF.

        READ TABLE gt_104_map WITH KEY id     = itab-id
                                       keytp  = 'BASE'
                                       BINARY SEARCH.

        READ TABLE gt_103b WITH KEY keyid = itab-id
                                    keytp = 'BASE'
                                    poper = gt_104_map-poper
                                    kstar = itab-kstar
                                    upgvc = itab-upgvc
                                    compn = itab-compn
                                    lifnr = itab-lifnr
                                    ekgrp = itab-ekgrp
                                    BINARY SEARCH.

        IF sy-subrc EQ 0.
          itab-end    = gt_103b-wertn.
          itab-endmat = gt_103b-artnr.
        ENDIF.

      ELSE.

        READ TABLE gt_103f WITH KEY artnr = itab-id
                                    kstar = itab-kstar
                                    upgvc = itab-upgvc
                                    compn = itab-compn
                                    lifnr = itab-lifnr
                                    ekgrp = itab-ekgrp
                                    BINARY SEARCH.
        IF sy-subrc EQ 0.
          itab-unitf  = gt_103f-wertn.
          itab-start  = gt_103f-wertn.
          itab-sttmat = gt_103f-artnr.
        ELSE.

          $year =  itab-bdatj.
          $poper = itab-poper - 1.
          IF $poper EQ 0.
            $year = $year - 1.
            $poper = 12.
          ENDIF.

          READ TABLE gt_103b WITH KEY keyid = itab-id
                                      poper = $poper
                                      kstar = itab-kstar
                                      upgvc = itab-upgvc
                                      compn = itab-compn
                                      lifnr = itab-lifnr
                                      ekgrp = itab-ekgrp.
          IF sy-subrc EQ 0.
            itab-unitf  = gt_103b-wertn.
            itab-start  = gt_103b-wertn.
            itab-sttmat = gt_103b-artnr.
          ENDIF.

        ENDIF.

      ENDIF.
    ENDIF.

    READ TABLE gt_mon_max WITH KEY id    =  itab-id
                               bdatj =  itab-bdatj
                               poper =  itab-poper
                               BINARY SEARCH.
    IF sy-subrc EQ 0.
      READ TABLE gt_103b WITH KEY keyid = itab-id
                                  keytp = 'FSC'
                                  poper = itab-poper
                                  kstar = itab-kstar
                                  upgvc = itab-upgvc
                                  compn = itab-compn
                                  lifnr = itab-lifnr
                                  ekgrp = itab-ekgrp
                                  BINARY SEARCH.

      IF sy-subrc EQ 0.
        itab-unitt  = gt_103b-wertn.
        itab-end    = gt_103b-wertn.
        itab-endmat = gt_103b-artnr.
      ENDIF.
    ENDIF.


    IF itab-poper NE '000'.
      IF itab-poper EQ s_poper-low.

*        itab-start  = itab-unitf.
*        itab-sttmat = itab-id.

        READ TABLE gt_103b WITH KEY keyid = itab-id
                                    keytp = 'FSC'
                                    poper = itab-poper
                                    kstar = itab-kstar
                                    upgvc = itab-upgvc
                                    compn = itab-compn
                                    lifnr = itab-lifnr
                                    ekgrp = itab-ekgrp
                                    BINARY SEARCH.


        IF sy-subrc EQ 0.
          itab-end    = gt_103b-wertn.
          itab-endmat = gt_103b-artnr.
        ENDIF.

      ELSE.

        READ TABLE gt_mon_min WITH KEY id    =  itab-id
                                   bdatj =  itab-bdatj
                                   poper =  itab-poper
                                   BINARY SEARCH.
        IF sy-subrc EQ 0.

          READ TABLE gt_104_map WITH KEY id     = itab-id
                                         keytp  = 'BASE'
                                         BINARY SEARCH.

          IF sy-subrc EQ 0.
            READ TABLE gt_103b WITH KEY keyid = itab-id
                                        keytp = 'BASE'
                                        poper = gt_104_map-poper
                                        kstar = itab-kstar
                                        upgvc = itab-upgvc
                                        compn = itab-compn
                                        lifnr = itab-lifnr
                                        ekgrp = itab-ekgrp
                                        BINARY SEARCH.

            IF sy-subrc EQ 0.
              itab-start = gt_103b-wertn.
              itab-sttmat = gt_103b-artnr.
            ENDIF.
          ELSE.
            sy-calld = space.
          ENDIF.

        ELSE.

          $year =  itab-bdatj.
          $poper = itab-poper - 1.
          IF $poper EQ 0.
            $year = $year - 1.
            $poper = 12.
          ENDIF.

          IF itab-poper EQ '001'.
            READ TABLE gt_103p WITH KEY keyid = itab-id
                                        keytp = 'FSC'
                                        kstar = itab-kstar
                                        upgvc = itab-upgvc
                                        compn = itab-compn
                                        lifnr = itab-lifnr
                                        ekgrp = itab-ekgrp
                                        BINARY SEARCH.
            IF sy-subrc EQ 0.
              itab-start = gt_103p-wertn.
              itab-sttmat = gt_103p-artnr.
            ENDIF.
          ELSE.
            READ TABLE gt_103b WITH KEY keyid = itab-id
                                        keytp = 'FSC'
                                        poper = $poper
                                        kstar = itab-kstar
                                        upgvc = itab-upgvc
                                        compn = itab-compn
                                        lifnr = itab-lifnr
                                        ekgrp = itab-ekgrp
                                        BINARY SEARCH.
            IF sy-subrc EQ 0.
              itab-start  = gt_103b-wertn.
              itab-sttmat = gt_103b-artnr.
            ENDIF.
          ENDIF.

        ENDIF.

        READ TABLE gt_103b WITH KEY keyid = itab-id
                                    keytp = 'FSC'
                                    poper = itab-poper
                                    kstar = itab-kstar
                                    upgvc = itab-upgvc
                                    compn = itab-compn
                                    lifnr = itab-lifnr
                                    ekgrp = itab-ekgrp
                                    BINARY SEARCH.
        IF sy-subrc EQ 0.
          itab-end = gt_103b-wertn.
          itab-endmat = gt_103b-artnr.
        ELSE.
          sy-calld = space.
        ENDIF.
      ENDIF.
    ENDIF.

    MODIFY itab INDEX $ix
       TRANSPORTING unitf unitt start end sttmat endmat.

  ENDLOOP.

  SORT itab BY bdatj poper id kstar upgvc compn.
  __cls : gt_from_prd,gt_to_prd.

  CLEAR $flag.

  LOOP AT itab.
    AT NEW id.
      $flag = true.
    ENDAT.
    CHECK $flag EQ true.
    IF itab-sttmat NE space.
      gt_from_prd-id = itab-id.
      gt_from_prd-bdatj = itab-bdatj.
      gt_from_prd-poper = itab-poper.
      gt_from_prd-prd   = itab-sttmat.
      APPEND gt_from_prd.
      CLEAR gt_from_prd.
      CLEAR $flag.
    ENDIF.
  ENDLOOP.

  LOOP AT itab.
    AT NEW id.
      $flag = true.
    ENDAT.
    CHECK $flag EQ true.
    IF itab-endmat NE space.
      gt_to_prd-id = itab-id.
      gt_to_prd-bdatj = itab-bdatj.
      gt_to_prd-poper = itab-poper.
      gt_to_prd-prd   = itab-endmat.
      APPEND gt_to_prd.
      CLEAR gt_to_prd.
      CLEAR $flag.
    ENDIF.
  ENDLOOP.

  SORT : gt_from_prd, gt_to_prd .

  LOOP AT itab.
    $ix = sy-tabix.
    IF itab-sttmat EQ space.
      READ TABLE gt_from_prd WITH KEY    id = itab-id
                                      bdatj = itab-bdatj
                                      poper = itab-poper
                                      BINARY SEARCH.
      IF sy-subrc EQ 0.
        itab-sttmat = gt_from_prd-prd.
      ENDIF.
    ENDIF.
    IF itab-endmat EQ space.
      READ TABLE gt_to_prd WITH KEY    id = itab-id
                                      bdatj = itab-bdatj
                                      poper = itab-poper
                                      BINARY SEARCH.
      IF sy-subrc EQ 0.
        itab-endmat = gt_to_prd-prd.
      ENDIF.
    ENDIF.
    MODIFY itab INDEX $ix TRANSPORTING sttmat endmat.
  ENDLOOP.

ENDFORM.                    " GET_GT_RSN
*&---------------------------------------------------------------------*
*&      Form  GET_GT_OUT1
*&---------------------------------------------------------------------*
FORM get_gt_out.
  DATA: l_idx(2),
        l_sum  TYPE zwertn3,
        l_sum1 TYPE zwertn3,
        l_sum2 TYPE zwertn3,
        l_sum3 TYPE zwertn3,
        l_sum4 TYPE zwertn3,
        i TYPE i,
        n(2),
        l_name(20).

  FIELD-SYMBOLS: <fs>  TYPE ANY,
                 <fs1> TYPE ANY.

  CLEAR: gt_out, i.
  REFRESH gt_out.

  DESCRIBE TABLE gt_resp LINES i.
  SORT itab BY kokrs bdatj id kstar.

* UD1K941594 - by IG.MOON 9/18/2007 {
  DATA : BEGIN OF $upgvc OCCURS 0,
           upgvc     LIKE makt-matnr,
         END   OF  $upgvc.
  DATA : BEGIN OF $upgvc_t OCCURS 0,
           upgvc     LIKE makt-matnr,
           maktg     LIKE makt-maktg,
         END   OF  $upgvc_t.

  LOOP AT itab.
    $upgvc-upgvc = itab-upgvc.
    COLLECT $upgvc.
    $upgvc-upgvc = itab-compn.
    COLLECT $upgvc.
  ENDLOOP.

  SELECT matnr maktg INTO TABLE $upgvc_t
  FROM makt
   FOR ALL ENTRIES IN $upgvc
   WHERE matnr = $upgvc-upgvc.

  SORT $upgvc_t BY upgvc.

* }

*  DATA $itab LIKE itab OCCURS 0 WITH HEADER LINE.
*
*  LOOP AT itab.
*    $itab = itab.
*    IF p_upg NE 'X'.
*      CLEAR $itab-upgvc.
*    ENDIF.
*    IF p_com NE 'X'.
*      CLEAR : $itab-compn, $itab-infrsn.
*    ENDIF.
*    IF p_vnd NE 'X'.
*      CLEAR $itab-lifnr.
*    ENDIF.
*    IF p_ekg NE 'X'.
*      CLEAR $itab-ekgrp.
*    ENDIF.
*    IF p_cst NE 'X'.
*      CLEAR $itab-kstar.
*    ENDIF.
*    COLLECT $itab.
*  ENDLOOP.
*  __cls itab.
*  itab[] = $itab[].
*

  LOOP AT itab.
    MOVE-CORRESPONDING itab TO gt_out.

    CLEAR n.
    DO i TIMES.
      n = n + 1.

      CLEAR l_sum.
      LOOP AT gt_ztcoum01 WHERE zresp = n.
        CLEAR l_idx.
        l_idx = gt_ztcoum01-idx.
        CONCATENATE 'GT_OUT-AMT' l_idx INTO l_name.
        ASSIGN (l_name) TO <fs>.
        l_sum = l_sum + <fs>.
      ENDLOOP.

      CLEAR l_name.
      CONCATENATE 'L_SUM' n INTO l_name.
      ASSIGN (l_name) TO <fs1>.
      <fs1> = l_sum.
    ENDDO.

    gt_out-sum1 = l_sum1.              " Purchasing Sum
    gt_out-sum2 = l_sum2.              " R&D Sum
    gt_out-sum3 = l_sum3.              " Etc
    gt_out-sum4 = l_sum4.              " Others

*   Total
    gt_out-tot = l_sum1 + l_sum2 + l_sum3 + l_sum4 + gt_out-amt99.

    READ TABLE $upgvc_t WITH KEY upgvc = gt_out-upgvc BINARY SEARCH.
    IF sy-subrc EQ 0.
      gt_out-upgvc_t = $upgvc_t-maktg.
    ENDIF.

    READ TABLE $upgvc_t WITH KEY upgvc = gt_out-compn BINARY SEARCH.
    IF sy-subrc EQ 0.
      gt_out-compn_t = $upgvc_t-maktg.
    ENDIF.

    APPEND gt_out.
    CLEAR gt_out.
  ENDLOOP.

ENDFORM.                    " GET_GT_OUT
*&---------------------------------------------------------------------*
*&      Form  GET_GT_RGRP
*&---------------------------------------------------------------------*
*  Create Reason Codes Internal Table GT_RGRP from table ZTCOUM02
*----------------------------------------------------------------------*
FORM get_gt_rgrp.
  CLEAR gt_rgrp.
  REFRESH gt_rgrp.

  SELECT rgrp2 grp1 alu ald ale alx
    INTO TABLE gt_rgrp
    FROM ztcoum02
   WHERE grp1 <> 'Z'.

ENDFORM.                    " get_GT_RGRP
*&---------------------------------------------------------------------*
*&      Form  get_amt_by_reason
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM get_amt_by_reason USING p_kzust TYPE kzust
                             p_wertn TYPE zwertn1_1.

*  CLEAR itab.
*  itab-kokrs  = gt_106-kokrs.      " Cotrolling Area
*  itab-bdatj  = gt_106-bdatj.      " Fiscal Year
*  itab-poper  = gt_106-poper.      " Fiscal Year
*  itab-id     = gt_106-id.         " ID
*  itab-idtext = gt_106-idtext.     " ID Desc.

*  IF itab-poper EQ s_poper-low.
*    READ TABLE gt_103f WITH KEY artnr = itab-id
*                                kstar = gt_106-kstar
*                                upgvc = gt_106-upgvc
*                                compn = gt_106-compn
*                                BINARY SEARCH.
*    IF sy-subrc EQ 0.
*      itab-unitf = gt_103f-wertn.
*    ENDIF.
*  ENDIF.
*  IF itab-poper EQ s_poper-high .
*    READ TABLE gt_103t WITH KEY artnr = itab-id
*                                kstar = gt_106-kstar
*                                upgvc = gt_106-upgvc
*                                compn = gt_106-compn
*                                BINARY SEARCH.
*    IF sy-subrc EQ 0.
*      itab-unitt = gt_103t-wertn.
*    ENDIF.
*  ENDIF.


*  IF p_upg = 'X'.
  itab-upgvc = gt_106-upgvc.
*  ENDIF.

*  IF p_com = 'X'.
  itab-compn  = gt_106-compn.
  itab-infrsn = gt_106-infrsn.
*  ENDIF.

*  IF p_vnd = 'X'.
  itab-lifnr = gt_106-lifnr.
*  ENDIF.

*  IF p_ekg = 'X'.
  itab-ekgrp = gt_106-ekgrp.
*  ENDIF.

*  IF p_cst = 'X'.
  itab-kstar  = gt_106-kstar.      " Cost Element
*  ENDIF.

  DATA: l_kzust TYPE kzust,
        l_char(2),
        l_name(20),
        l_indx(2).

  FIELD-SYMBOLS <fs> TYPE ANY.

  CHECK " NOT p_kzust IS INITIAL AND
        NOT p_wertn IS INITIAL.

  CLEAR l_kzust.

  IF p_kzust+0(1) = 'X'.
    l_kzust = gt_106-kzust1+1(2).
  ELSE.
    CONCATENATE p_kzust+0(1) p_kzust+2(1) INTO l_kzust.
  ENDIF.

  READ TABLE gt_rgrp WITH KEY rgrp2 = l_kzust.
  IF sy-subrc = 0.
    CLEAR: l_char, l_name.

    IF p_kzust+0(1) = 'X'.
      l_char = gt_rgrp-alx.
    ELSE.
      CASE p_kzust+1(1).
        WHEN 'U'.
          l_char = gt_rgrp-alu.
        WHEN 'D'.
          l_char = gt_rgrp-ald.
        WHEN 'E'.
          l_char = gt_rgrp-ale.
      ENDCASE.
    ENDIF.
  ENDIF.

  READ TABLE gt_ztcoum01 WITH KEY zresc = l_char.
  IF sy-subrc = 0.
    CLEAR l_indx.
    l_indx = gt_ztcoum01-idx.
    CONCATENATE 'ITAB-AMT' l_indx INTO l_name.
    ASSIGN (l_name) TO <fs>.
    <fs> = p_wertn.

  ELSE.
    itab-amt99 = itab-amt99 + p_wertn.
  ENDIF.

*  COLLECT itab.

ENDFORM.                    " get_amt_by_reason
*&---------------------------------------------------------------------*
*&      Form  GET_GT_ZTCOUM01
*&---------------------------------------------------------------------*
*  Create Internal Table GT_ZTCOUM01 for Responsible Analysis Codes
*----------------------------------------------------------------------*
FORM get_gt_ztcoum01.
  DATA l_cnt TYPE i.

  CLEAR: gt_ztcoum01, gt_resp, l_cnt.
  REFRESH: gt_ztcoum01, gt_resp.

  SELECT * INTO TABLE gt_ztcoum01
    FROM ztcoum01.

  SORT gt_ztcoum01 BY zresp zresc.

  LOOP AT gt_ztcoum01.
    l_cnt = l_cnt + 1.
    gt_ztcoum01-idx = l_cnt.
    MODIFY gt_ztcoum01.

    gt_resp-zresp = gt_ztcoum01-zresp.
    APPEND gt_resp.
    CLEAR gt_resp.
  ENDLOOP.

  DELETE ADJACENT DUPLICATES FROM gt_resp.

ENDFORM.                    " GET_GT_ZTCOUM01
*&---------------------------------------------------------------------*
*&      Form  MODIFY_FIELDCAT
*&---------------------------------------------------------------------*
FORM modify_fieldcat.
  DATA l_indx(2).
  DATA f_indx(1) TYPE n.

  LOOP AT gt_resp.
    gs_fieldcat-outputlen = 15.
    gs_fieldcat-datatype  = 'CURR'.

    CONCATENATE 'SUM' gt_resp-zresp
           INTO gs_fieldcat-fieldname.

    CASE gt_resp-zresp.
**S> 08/04/11 Paul
      WHEN 1.
        gs_fieldcat-seltext_l = '10.purchasing'.
      WHEN 2.
        gs_fieldcat-seltext_l = '20.r&d'.
      WHEN 3.
        gs_fieldcat-seltext_l = '30.etc'.
      WHEN OTHERS.
        gs_fieldcat-seltext_l = '40.others'.
**E<
    ENDCASE.

    APPEND gs_fieldcat TO gt_fieldcat.
    f_indx = 0.
    LOOP AT gt_ztcoum01 WHERE zresp = gt_resp-zresp.
      CLEAR l_indx.
      l_indx = gt_ztcoum01-idx.
      CONCATENATE 'AMT' l_indx INTO gs_fieldcat-fieldname.
*      gs_fieldcat-seltext_l = gt_ztcoum01-text.
      ADD 1 TO f_indx.
      CONCATENATE  gt_resp-zresp f_indx  '.'
                                             gt_ztcoum01-text INTO
gs_fieldcat-seltext_l.
      APPEND gs_fieldcat TO gt_fieldcat.
    ENDLOOP.

  ENDLOOP.

ENDFORM.                    " MODIFY_FIELDCAT
*&---------------------------------------------------------------------*
*&      Form  get_gt_103
*&---------------------------------------------------------------------*
FORM get_gt_103.
*  CHECK p_all = 'X'.

  LOOP AT s_poper.
    IF s_poper-high IS INITIAL.
      s_poper-high = s_poper-low.
      MODIFY s_poper INDEX sy-tabix.
    ENDIF.
  ENDLOOP.

  DATA: l_frp LIKE ztcou103-poper,
        l_fry LIKE ztcou103-bdatj.

  __cls : gt_103_all, gt_103f, gt_103t, gt_103p.

  DATA $kalka LIKE p_kalka .

  DATA: prev_p TYPE poper,
        prev_y LIKE p_year.

  l_fry = p_year.

  PERFORM get_104_103.

ENDFORM.                    " get_gt_103
*&---------------------------------------------------------------------*
*&      Form  combine_to_out
*&---------------------------------------------------------------------*
FORM combine_to_out.

* data: lt_itab like itab occurs 0 with header line.
*
* sort itab by id upgvc compn.
*
* loop at GT_103F.
*   read table itab with key id    = gt_103f-artnr
*                            upgvc = gt_103f-upgvc
*                            compn = gt_103f-compn
*                   binary search.
*   if sy-subrc = 0.
*
*   else.
*     append lt_itab.
*   endif.
*
* endloop.

ENDFORM.                    " combine_to_out
*&---------------------------------------------------------------------*
*&      Form  check_abp_102
*&---------------------------------------------------------------------*
FORM check_abp_102.
  LOOP AT gt_106 WHERE poper = 0.
    SELECT SINGLE kzust1 INTO gt_106-infrsn
     FROM ztcou102
     WHERE kokrs EQ gt_106-kokrs
       AND bdatj EQ gt_106-bdatj
       AND poper EQ gt_106-poper
       AND kalka EQ gt_106-kalka
       AND ver   EQ '0'
       AND matnr EQ gt_106-compn.
    MODIFY gt_106 INDEX sy-tabix TRANSPORTING infrsn.
  ENDLOOP.
ENDFORM.                    " check_abp_102
*&---------------------------------------------------------------------*
*&      Form  collect_by_level
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM collect_by_level.

  DATA : $gt_106  LIKE gt_106 OCCURS 0 WITH HEADER LINE,
         $gt_103b LIKE gt_103b OCCURS 0 WITH HEADER LINE,
         $gt_103p LIKE gt_103p OCCURS 0 WITH HEADER LINE,
         $gt_103f LIKE gt_103f OCCURS 0 WITH HEADER LINE.

  LOOP AT gt_106.
    $gt_106 = gt_106.
    IF p_upg NE 'X'.
      CLEAR $gt_106-upgvc.
    ENDIF.
    IF p_com NE 'X'.
      CLEAR : $gt_106-compn, $gt_106-infrsn.
    ENDIF.
    IF p_vnd NE 'X'.
      CLEAR $gt_106-lifnr.
    ENDIF.
    IF p_ekg NE 'X'.
      CLEAR $gt_106-ekgrp.
    ENDIF.
    IF p_cst NE 'X'.
      CLEAR $gt_106-kstar.
    ENDIF.
    COLLECT $gt_106.
  ENDLOOP.

  __cls gt_106.  gt_106[] = $gt_106[].

  LOOP AT gt_103b.
    $gt_103b = gt_103b.
    IF p_upg NE 'X'.
      CLEAR $gt_103b-upgvc.
    ENDIF.
    IF p_com NE 'X'.
      CLEAR : $gt_103b-compn.
    ENDIF.
    IF p_cst NE 'X'.
      CLEAR $gt_103b-kstar.
    ENDIF.
    IF p_vnd NE 'X'.
      CLEAR $gt_103b-lifnr.
    ENDIF.
    IF p_ekg NE 'X'.
      CLEAR $gt_103b-ekgrp.
    ENDIF.
    COLLECT $gt_103b.
  ENDLOOP.
  __cls gt_103b.  gt_103b[] = $gt_103b[].


  LOOP AT gt_103p.
    $gt_103p = gt_103p.
    IF p_upg NE 'X'.
      CLEAR $gt_103p-upgvc.
    ENDIF.
    IF p_com NE 'X'.
      CLEAR : $gt_103p-compn.
    ENDIF.
    IF p_cst NE 'X'.
      CLEAR $gt_103p-kstar.
    ENDIF.
    IF p_vnd NE 'X'.
      CLEAR $gt_103p-lifnr.
    ENDIF.
    IF p_ekg NE 'X'.
      CLEAR $gt_103p-ekgrp.
    ENDIF.
    COLLECT $gt_103p.
  ENDLOOP.
  __cls gt_103p.  gt_103p[] = $gt_103p[].

  LOOP AT gt_103f.
    $gt_103f = gt_103f.
    IF p_upg NE 'X'.
      CLEAR $gt_103f-upgvc.
    ENDIF.
    IF p_com NE 'X'.
      CLEAR : $gt_103f-compn.
    ENDIF.
    IF p_cst NE 'X'.
      CLEAR $gt_103f-kstar.
    ENDIF.
    IF p_vnd NE 'X'.
      CLEAR $gt_103f-lifnr.
    ENDIF.
    IF p_ekg NE 'X'.
      CLEAR $gt_103f-ekgrp.
    ENDIF.
    COLLECT $gt_103f.
  ENDLOOP.
  __cls gt_103f.  gt_103f[] = $gt_103f[].

ENDFORM.                    " collect_by_level
*&---------------------------------------------------------------------*
*&      Form  get_base_000
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_104_103.

  DATA wfscxx TYPE zjan_fsc.
  DATA $idx(3) TYPE n.
  DATA: prev_p TYPE poper,
        prev_y LIKE p_year.

  DATA fname(30).
  DATA l_frp(3) TYPE n.

  l_frp = s_poper-low - 1.

  __cls : gt_103b, gt_104, gt_104_map, gt_103p.

  SELECT * INTO TABLE gt_104
                    FROM ztcou104
                    WHERE kokrs EQ p_kokrs
                      AND bdatj EQ p_year
                      AND kalka EQ p_kalka
                      AND id IN s_id.

  LOOP AT gt_104.

*////// Base FSC
    SELECT a~artnr a~kstar a~upgvc a~compn a~poper
           a~wertn a~duty  a~frg a~oth b~lifnr b~ekgrp
        APPENDING CORRESPONDING FIELDS OF TABLE gt_103b
        FROM ztcou103 AS a
        LEFT OUTER JOIN ztcou102 AS b
        ON  b~kokrs EQ a~kokrs
        AND b~bdatj EQ a~bdatj
        AND b~poper EQ a~poper
        AND b~kalka EQ a~kalka
        AND b~ver   EQ a~ver
        AND b~matnr EQ a~compn
       WHERE a~kokrs = p_kokrs
         AND a~bdatj = gt_104-base_year
         AND a~kalka = p_kalka
         AND a~artnr = gt_104-zbase_fsc
         AND a~poper = gt_104-base_poper
         AND a~upgvc IN s_upgvc
         AND a~compn IN s_compn.

    gt_103b-keyid = gt_104-id.
    gt_103b-keytp = 'BASE'.
    gt_103b-idtext = gt_104-idtext.
    MODIFY gt_103b TRANSPORTING keyid keytp idtext
      WHERE artnr = gt_104-zbase_fsc
              AND keyid = space
              AND keytp = space.

    PERFORM keep_map USING gt_104-id
                           'BASE'
                           gt_104-base_year
                           gt_104-base_poper
                           gt_104-zbase_fsc .

    IF '000' IN s_poper.
*////// ABP
      SELECT a~artnr a~kstar a~upgvc a~compn a~poper
             a~wertn a~duty  a~frg a~oth b~lifnr  b~ekgrp
          APPENDING CORRESPONDING FIELDS OF TABLE gt_103b
          FROM ztcou103 AS a
          LEFT OUTER JOIN ztcou102 AS b
          ON  b~kokrs EQ a~kokrs
          AND b~bdatj EQ a~bdatj
          AND b~poper EQ a~poper
          AND b~kalka EQ a~kalka
          AND b~ver   EQ a~ver
          AND b~matnr EQ a~compn
         WHERE a~kokrs = p_kokrs
           AND a~bdatj = p_year
           AND a~kalka = 'BP'
           AND a~artnr = gt_104-zabp_fsc
           AND a~poper = '001'
           AND a~upgvc IN s_upgvc
           AND a~compn IN s_compn.

      gt_103b-keyid = gt_104-id.
      gt_103b-keytp = 'ABP'.
      gt_103b-idtext = gt_104-idtext.

      MODIFY gt_103b TRANSPORTING keyid keytp idtext
        WHERE artnr = gt_104-zabp_fsc
                AND keyid = space
                AND keytp = space.

      PERFORM keep_map USING gt_104-id
                             'ABP'
                             p_year
                             '001'
                             gt_104-zabp_fsc .
    ENDIF.
*////// Each of FSC
    DO 12 TIMES VARYING wfscxx FROM
          gt_104-fsc01 NEXT gt_104-fsc02.

      $idx = sy-index.
      CHECK $idx IN s_poper.
      IF wfscxx EQ space.
        wfscxx = gt_104-id.
      ENDIF.

      READ TABLE gt_103b WITH KEY keyid = wfscxx
                                  keytp = 'FSC'
                                  poper = $idx.
      CHECK sy-subrc NE 0.

      SELECT a~artnr a~kstar a~upgvc a~compn a~poper
             a~wertn a~duty  a~frg a~oth b~lifnr  b~ekgrp
          APPENDING CORRESPONDING FIELDS OF TABLE gt_103b
          FROM ztcou103 AS a
        LEFT OUTER JOIN ztcou102 AS b
        ON  b~kokrs EQ a~kokrs
        AND b~bdatj EQ a~bdatj
        AND b~poper EQ a~poper
        AND b~kalka EQ a~kalka
        AND b~ver   EQ a~ver
        AND b~matnr EQ a~compn
         WHERE a~kokrs = p_kokrs
           AND a~bdatj = p_year
           AND a~kalka = p_kalka
           AND a~artnr = wfscxx
           AND a~poper = $idx
           AND a~upgvc IN s_upgvc
           AND a~compn IN s_compn.

      IF sy-subrc EQ 0.
        gt_103b-keyid = gt_104-id.
        gt_103b-keytp = 'FSC'.
        gt_103b-idtext = gt_104-idtext.

        MODIFY gt_103b TRANSPORTING keyid keytp idtext
          WHERE artnr = wfscxx
                  AND keyid = space
                  AND keytp = space.

        PERFORM keep_map USING gt_104-id
                               'FSC'
                               p_year
                               $idx
                               wfscxx .

      ENDIF.
    ENDDO.

    IF l_frp > 0.
      CONCATENATE 'GT_104-FSC' l_frp+1(2) INTO fname.
      ASSIGN (fname) TO <fs>.

      SELECT a~artnr a~kstar a~upgvc a~compn
             a~wertn a~duty  a~frg a~oth b~lifnr  b~ekgrp
        APPENDING CORRESPONDING FIELDS OF TABLE gt_103f
        FROM ztcou103 AS a
        LEFT OUTER JOIN ztcou102 AS b
        ON  b~kokrs EQ a~kokrs
        AND b~bdatj EQ a~bdatj
        AND b~poper EQ a~poper
        AND b~kalka EQ a~kalka
        AND b~ver   EQ a~ver
        AND b~matnr EQ a~compn
       WHERE a~kokrs = p_kokrs
         AND a~bdatj = p_year
         AND a~kalka = p_kalka
         AND a~artnr = <fs>
         AND a~poper = l_frp
         AND a~upgvc IN s_upgvc
         AND a~compn IN s_compn.

      gt_103f-keyid = gt_104-id.
      gt_103f-keytp = 'FSC'.
      gt_103f-idtext = gt_104-idtext.

      MODIFY gt_103f TRANSPORTING keyid keytp idtext
        WHERE artnr = <fs>
                AND keyid = space
                AND keytp = space.

      PERFORM keep_map USING gt_104-id
                             'FSC'
                             p_year
                             l_frp
                             <fs> .

    ENDIF.

  ENDLOOP.

  IF '001' IN s_poper.

    __cls : gt_103p.
    prev_p = 12.
    prev_y = p_year - 1.

    __cls : gt_104.
    SELECT * INTO TABLE gt_104
                      FROM ztcou104
                      WHERE kokrs EQ p_kokrs
                        AND bdatj EQ prev_y
                        AND kalka EQ p_kalka
                        AND id IN s_id.

    LOOP AT gt_104.

      IF gt_104-fsc12 EQ space.
        gt_104-fsc12 = gt_104-id.
      ENDIF.

      SELECT a~artnr a~kstar a~upgvc a~compn a~poper
             a~wertn a~duty  a~frg a~oth b~lifnr  b~ekgrp
          INTO CORRESPONDING FIELDS OF TABLE gt_103p
          FROM ztcou103 AS a
        LEFT OUTER JOIN ztcou102 AS b
        ON  b~kokrs EQ a~kokrs
        AND b~bdatj EQ a~bdatj
        AND b~poper EQ a~poper
        AND b~kalka EQ a~kalka
        AND b~ver   EQ a~ver
        AND b~matnr EQ a~compn
         WHERE a~kokrs = p_kokrs
           AND a~bdatj = prev_y
           AND a~kalka = p_kalka
           AND a~artnr = gt_104-fsc12
           AND a~poper = $idx
           AND a~upgvc IN s_upgvc
           AND a~compn IN s_compn.

      IF sy-subrc EQ 0.
        gt_103p-keyid = gt_104-id.
        gt_103p-keytp = 'FSC'.
        gt_103p-idtext = gt_104-idtext.
        MODIFY gt_103p TRANSPORTING keyid keytp idtext
          WHERE artnr = gt_104-fsc12
                  AND keyid = space
                  AND keytp = space.

        PERFORM keep_map USING gt_104-id
                               'FSC'
                               prev_y
                               '012'
                               gt_104-fsc12 .
      ENDIF.

    ENDLOOP.
  ENDIF.

ENDFORM.                    " get_base_000
*&---------------------------------------------------------------------*
*&      Form  keep_map
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_104_MAP  text
*      -->P_GT_104_ID  text
*      -->P_3141   text
*      -->P_GT_104_BASE_YEAR  text
*      -->P_GT_104_BASE_POPER  text
*      -->P_GT_104_ZBASE_FSC  text
*----------------------------------------------------------------------*
FORM keep_map USING    p_gt_104_id
                       value(p_tp)
                       p_gt_104_year
                       p_gt_104_poper
                       p_gt_104_fsc.

  gt_104_map-id    = p_gt_104_id.
  gt_104_map-keytp = p_tp.
  gt_104_map-bdatj = p_gt_104_year.
  gt_104_map-poper = p_gt_104_poper.
  gt_104_map-keyid = p_gt_104_fsc.
  APPEND gt_104_map.
  CLEAR gt_104_map.

ENDFORM.                    " keep_map
