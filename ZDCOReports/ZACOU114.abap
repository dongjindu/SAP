*----------------------------------------------------------------------
* Program ID        : ZACOU114
* Title             : [CO] Variance List
* Created on        : 10/13/2006
* Created by        : Michelle Jeong
* Specifications By : Andy Choi
* Description       : Report for Variance List
*----------------------------------------------------------------------
REPORT zacou114 NO STANDARD PAGE HEADING MESSAGE-ID zmco.

INCLUDE zacoui00.

TABLES ztcou106.     " [CO] Calculate Variances

TYPES: BEGIN OF ty_106,
         kokrs  TYPE kokrs,          " Cotrolling Area
         bdatj  TYPE bdatj,          " Fiscal Year
         poper  TYPE poper,
         id     TYPE zid1,           " ID
         kzust  TYPE kzust,          " ID Desc.
         dwertn TYPE zdwertn,        " Price
         kstar  TYPE kstar,
         lifnr  TYPE lifnr,
         upgvc  TYPE zupgvc,
         compn  TYPE idnrk,
         idtext TYPE zidtext,        " ID Desc.
         chk,
* UD1K941594 by IG.MOON 9/18/07
* {
         zrclss TYPE zrclss,
         infrsn TYPE zkzust1,
         ekgrp TYPE ekgrp,
* }
       END OF ty_106.

TYPES: BEGIN OF ty_itab,
         kokrs     TYPE kokrs,       " Cotrolling Area
         bdatj     TYPE bdatj,       " Fiscal Year
         poper  TYPE poper,
         id        TYPE zid1,        " ID
         idtext    TYPE zidtext,     " ID Desc.
         zresc	 TYPE zcorsc,      " Responsible Category
         zresp	 TYPE zcorsp,      " Responsible Area
         text	 TYPE zrtext,      " Description
         kzust     TYPE kzust,       " Reason
         rtext     TYPE zrtext,      " Reason Description
         kstar     TYPE kstar,
         lifnr     TYPE lifnr,
         upgvc     TYPE zupgvc,
         compn     TYPE idnrk,
         incr      TYPE zwertn,      " Increase
         decr      TYPE zwertn,      " Decrease
         tot       TYPE zwertn,      " Total
* UD1K941594 by IG.MOON 9/18/07
* {
         zrclss TYPE zrclss,
         infrsn TYPE zkzust1,
         upgvc_t   TYPE maktg,
         compn_t   TYPE maktg,
         ekgrp TYPE ekgrp,
* }

       END OF ty_itab.

TYPES: BEGIN OF ty_out.
INCLUDE  TYPE ty_itab.
TYPES:   tabcolor TYPE slis_t_specialcol_alv,
       END OF ty_out.

TYPES: BEGIN OF ty_kzust,
         kzust TYPE kzust,
       END OF ty_kzust.

DATA: gt_106   TYPE TABLE OF ty_106   WITH HEADER LINE,
      gt_itab  TYPE TABLE OF ty_itab  WITH HEADER LINE,
      gt_out   TYPE TABLE OF ty_out   WITH HEADER LINE,
      gt_resp  TYPE TABLE OF ztcoum01 WITH HEADER LINE,
      gt_rgrp  TYPE TABLE OF ztcoum02 WITH HEADER LINE,
      gt_kzust TYPE TABLE OF ty_kzust WITH HEADER LINE.

*----------------------------------------------------------------------
* Selection-Screen
*----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK b0 WITH FRAME TITLE text-011.
PARAMETERS: p_kokrs LIKE ztcou106-kokrs OBLIGATORY
                                        MEMORY ID cac
                                        MATCHCODE OBJECT fc_kokrs,
            p_year  LIKE ztcou106-bdatj OBLIGATORY MEMORY ID bdtj,
            p_kalka LIKE ztcou106-kalka OBLIGATORY MEMORY ID kka.
SELECT-OPTIONS: s_poper FOR ztcou106-poper. "OBLIGATORY.
SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN BEGIN OF BLOCK b0s WITH FRAME TITLE text-021.
SELECT-OPTIONS: s_id    FOR ztcou106-id MATCHCODE OBJECT zid,
                s_upgvc FOR ztcou106-upgvc,
                s_compn FOR ztcou106-compn,
                s_kzust FOR ztcou106-kzust,
* UD1K941594 by IG.MOON 9/18/07
* {
                s_zrclss FOR ztcou106-zrclss,
                s_ekgrp  FOR ztcou106-ekgrp.
* }
SELECTION-SCREEN END OF BLOCK b0s.
SELECTION-SCREEN END OF BLOCK b0.

* Layout
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
PARAMETERS: p_upg AS CHECKBOX,
            p_com AS CHECKBOX,
            p_vnd AS CHECKBOX,
            p_cst AS CHECKBOX,
            p_ekg AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK b1.

PARAMETER p_vari TYPE slis_vari.
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
* Get Calculate Variances Data from Table ZTCOU106
  PERFORM get_gt_106.

* Get Reason Info.
  PERFORM get_reason.

* Remain selected reason only when entered reason codes
  PERFORM choose_reason.

* Get Resp.
  PERFORM get_resp.

* Create Internal Tables for Variance List
  PERFORM get_gt_out.

ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  GET_RGRP
*&---------------------------------------------------------------------*
*       Create Internal Table for Reason
*----------------------------------------------------------------------*
FORM get_reason.
  CLEAR gt_rgrp.
  REFRESH gt_rgrp.

  SELECT * INTO TABLE gt_rgrp
    FROM ztcoum02
   WHERE grp1 <> 'Z'.

  SORT gt_rgrp BY rgrp2.

ENDFORM.                    " GET_GT_RSN
*&---------------------------------------------------------------------*
*&      Form  GET_RESP
*&---------------------------------------------------------------------*
*       Get Responsible Area
*----------------------------------------------------------------------*
FORM get_resp.
  CLEAR gt_resp.
  REFRESH gt_resp.

  SELECT * INTO TABLE gt_resp
    FROM ztcoum01.

  SORT gt_resp BY zresc.

ENDFORM.                    " GET_RESP
*&---------------------------------------------------------------------*
*&      Form  GET_GT_OUT
*&---------------------------------------------------------------------*
FORM get_gt_out.
  CLEAR: gt_itab, gt_out.
  REFRESH: gt_itab, gt_out.

  LOOP AT gt_106.
    gt_itab-kokrs = gt_106-kokrs.        " Cotrolling Area
    gt_itab-bdatj = gt_106-bdatj.        " Piscal Year
    gt_itab-poper = gt_106-poper.        " Period
    gt_itab-id = gt_106-id.              " ID
    gt_itab-idtext = gt_106-idtext.      " ID Desc.

    gt_itab-kstar = gt_106-kstar.        " ID
    gt_itab-lifnr = gt_106-lifnr.        " ID Desc.

    IF p_upg = 'X'.
      gt_itab-upgvc = gt_106-upgvc.
    ENDIF.
    IF p_com = 'X'.
      gt_itab-compn = gt_106-compn.
    ENDIF.
    IF p_vnd = 'X'.
      gt_itab-lifnr = gt_106-lifnr.
    ENDIF.
    IF p_cst = 'X'.
      gt_itab-kstar  = gt_106-kstar.      " Cost Element
    ENDIF.

    IF p_ekg = 'X'.
      gt_itab-ekgrp = gt_106-ekgrp.
    ENDIF.

* UD1K941594 by IG.MOON 9/18/07
* {
    CONCATENATE gt_106-infrsn+0(1) gt_106-infrsn+2(1)
    INTO gt_itab-infrsn.

    gt_itab-zrclss = gt_106-zrclss.
* }
    CONCATENATE gt_106-kzust+0(1) gt_106-kzust+2(1) INTO gt_itab-kzust.

    IF gt_106-kzust+1(1) = 'U'.
      gt_itab-incr = gt_106-dwertn.      " Increase
    ELSEIF gt_106-kzust+1(1) = 'D'.
      gt_itab-decr = gt_106-dwertn.      " Decrease
    ELSE.
      IF gt_106-dwertn < 0.
        gt_itab-decr = abs( gt_106-dwertn ).      " Decrease
      ELSE.
        gt_itab-incr = abs( gt_106-dwertn ).      " Increase
      ENDIF.
    ENDIF.

    gt_itab-tot = gt_itab-incr + gt_itab-decr.        " Total

*   [CO] Reason Codes
    READ TABLE gt_rgrp WITH KEY rgrp2 = gt_itab-kzust BINARY SEARCH.
    IF sy-subrc = 0.
      gt_itab-rtext = gt_rgrp-text.       " Reason Desc.
    ENDIF.

*   Resp.
    READ TABLE gt_resp WITH KEY zresc = gt_rgrp-alu.

    IF sy-subrc = 0.
      PERFORM move_resp.
      CONTINUE.
    ELSE.
      READ TABLE gt_resp WITH KEY zresc = gt_rgrp-ald.
      IF sy-subrc = 0.
        PERFORM move_resp.
        CONTINUE.
      ELSE.
        READ TABLE gt_resp WITH KEY zresc = gt_rgrp-ale.
        IF sy-subrc = 0.
          PERFORM move_resp.
*-------ANDY FIX
        ELSE.
          COLLECT gt_itab.
          CLEAR gt_itab.

        ENDIF.
      ENDIF.

    ENDIF.
  ENDLOOP.

* UD1K941594 - by IG.MOON 9/18/2007 {
  DATA : BEGIN OF $upgvc OCCURS 0,
           upgvc     LIKE makt-matnr,
         END   OF  $upgvc.
  DATA : BEGIN OF $upgvc_t OCCURS 0,
           upgvc     LIKE makt-matnr,
           maktg     LIKE makt-maktg,
         END   OF  $upgvc_t.

  LOOP AT gt_itab.
    $upgvc-upgvc = gt_itab-upgvc.
    COLLECT $upgvc.
    $upgvc-upgvc = gt_itab-compn.
    COLLECT $upgvc.
  ENDLOOP.

  SELECT matnr maktg INTO TABLE $upgvc_t
  FROM makt
   FOR ALL ENTRIES IN $upgvc
   WHERE matnr = $upgvc-upgvc.

  SORT $upgvc_t BY upgvc.

* }

  LOOP AT gt_itab.
    MOVE-CORRESPONDING gt_itab TO gt_out.

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
*&      Form  DISP_RESULT
*&---------------------------------------------------------------------*
*       Display Cost Review
*----------------------------------------------------------------------*
FORM disp_result.
  CLEAR: gt_fieldcat, gs_layout, gt_events, gs_variant,
         gt_fieldcat[], gt_events[].

  PERFORM build_field_category USING:
    'ID'         'X'  'ID'               10    'CHAR',
    'IDTEXT'     'X'  'ID.Desc.'         25    'CHAR',
    'POPER'      ' '  'Period'           03    'CHAR',
    'KSTAR'      ' '  'CostEl'           10    'CHAR',
    'LIFNR'      ' '  'Vendor'           10    'CHAR',
    'EKGRP'      ' '  'PurG'              3    'CHAR',
    'INFRSN'     ' '  'Info.'            2    'CHAR',
    'ZRCLSS'     ' '  'C'                1    'CHAR',
    'ZRESC'      ' '  'Resp.Category'    2     'CHAR',
    'ZRESP'      ' '  'Resp.Area'        1     'CHAR',
    'TEXT'       ' '  'Rsn.Category'     30    'CHAR',
    'KZUST'      ' '  'Reason'           10    'CHAR',
    'RTEXT'      ' '  'Reason.Desc.'     25    'CHAR',
    'TOT'        ' '  'Total'            15    'CURR',
    'INCR'       ' '  'Increase'         15    'CURR',
    'DECR'       ' '  'Decrease'         15    'CURR',
    'UPGVC'      ' '  'UPGVC'            12    'CHAR',
    'UPGVC_T'    ' '  'UPGVC_T'          30    'CHAR',
    'COMPN'      ' '  'Component'        18    'CHAR',
    'COMPN_T'    ' '  'COMPN_T'          30    'CHAR'.


  LOOP AT gt_fieldcat INTO gs_fieldcat.

    IF gs_fieldcat-fieldname EQ 'ZRCLSS' OR
        gs_fieldcat-fieldname EQ 'EKGRP'.
      gs_fieldcat-ref_tabname   = 'ZTCOU106'.
      gs_fieldcat-ref_fieldname   = gs_fcat-fieldname.
      MODIFY gt_fieldcat FROM gs_fieldcat TRANSPORTING ref_tabname
                                               ref_fieldname
                     WHERE fieldname = gs_fieldcat-fieldname.
    ENDIF.

    IF gs_fieldcat-fieldname EQ 'KZUST' OR
        gs_fieldcat-fieldname EQ 'INFRSN'.
      gs_fieldcat-ref_tabname   = 'ZTCOUM02'.
      gs_fieldcat-ref_fieldname   = 'RGRP2'.
      MODIFY gt_fieldcat FROM gs_fieldcat TRANSPORTING ref_tabname
                                               ref_fieldname
                     WHERE fieldname = gs_fieldcat-fieldname.
    ENDIF.

    IF gs_fieldcat-fieldname EQ 'ZRESC'.
      gs_fieldcat-ref_tabname   = 'ZTCOUM02'.
      gs_fieldcat-ref_fieldname   = 'ALU'.
      MODIFY gt_fieldcat FROM gs_fieldcat TRANSPORTING ref_tabname
                                               ref_fieldname
                     WHERE fieldname = gs_fieldcat-fieldname.
    ENDIF.

    IF gs_fieldcat-fieldname EQ 'ZRESP'.
      gs_fieldcat-ref_tabname   = 'ZTCOUM01'.
      gs_fieldcat-ref_fieldname   = 'ZRESP'.
      MODIFY gt_fieldcat FROM gs_fieldcat TRANSPORTING ref_tabname
                                               ref_fieldname
                     WHERE fieldname = gs_fieldcat-fieldname.
    ENDIF.

  ENDLOOP.


  CLEAR gs_layout.
  gs_layout-zebra             = 'X'.
  gs_layout-colwidth_optimize = 'X'.
  gs_layout-coltab_fieldname  = 'TABCOLOR'.

  PERFORM set_color.
  PERFORM set_list_header USING gt_list_top_of_page.
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
*&      Form  SET_LIST_HEADER
*&---------------------------------------------------------------------*
FORM set_list_header USING lt_top_of_page TYPE slis_t_listheader.
  DATA: ls_line TYPE slis_listheader,
        l_cnt TYPE i.

  CLEAR ls_line.
  ls_line-typ  = 'S'.

  ls_line-key  = 'Controlling Area'.
  ls_line-info = p_kokrs.
  APPEND ls_line TO lt_top_of_page.

  ls_line-key  = 'Fiscal Year'.
  ls_line-info = p_year.
  APPEND ls_line TO lt_top_of_page.

  ls_line-key  = 'Period'.
  DESCRIBE TABLE s_poper LINES l_cnt.
  IF l_cnt = 1.
    IF s_poper-high IS INITIAL.
      ls_line-info = s_poper-low.
    ELSE.
      CONCATENATE s_poper-low '~' s_poper-high INTO ls_line-info.
    ENDIF.
    APPEND ls_line TO lt_top_of_page.
  ELSEIF l_cnt > 1.
    CONCATENATE s_poper-low '...' INTO ls_line-info.
    APPEND ls_line TO lt_top_of_page.
  ENDIF.

  IF NOT s_id IS INITIAL.
    ls_line-key  = 'ID'.
    DESCRIBE TABLE s_id LINES l_cnt.
    IF l_cnt = 1.
      IF s_id-high IS INITIAL.
        ls_line-info = s_id-low.
      ELSE.
        CONCATENATE s_id-low '~' s_id-high INTO ls_line-info.
      ENDIF.
      APPEND ls_line TO lt_top_of_page.
    ELSEIF l_cnt > 1.
      CONCATENATE s_id-low '...' INTO ls_line-info.
      APPEND ls_line TO lt_top_of_page.
    ENDIF.
  ENDIF.

  IF NOT s_kzust IS INITIAL.
    ls_line-key  = 'Reason'.
    DESCRIBE TABLE s_kzust LINES l_cnt.
    IF l_cnt = 1.
      IF s_kzust-high IS INITIAL.
        ls_line-info = s_kzust-low.
      ELSE.
        CONCATENATE s_kzust-low '~' s_kzust-high INTO ls_line-info.
      ENDIF.
      APPEND ls_line TO lt_top_of_page.
    ELSEIF l_cnt > 1.
      CONCATENATE s_kzust-low '...' INTO ls_line-info.
      APPEND ls_line TO lt_top_of_page.
    ENDIF.
  ENDIF.

  CLEAR: ls_line-key, ls_line-info.
  APPEND ls_line TO lt_top_of_page.
*
ENDFORM.                    " SET_LIST_HEADER
*&---------------------------------------------------------------------*
*&      Form  SET_COLOR
*&---------------------------------------------------------------------*
FORM set_color.
  CLEAR: gs_specialcol, gt_specialcol[], gt_out-tabcolor[].

  gs_specialcol-fieldname = 'TOT'.
  gs_specialcol-color-col = cl_gui_resources=>list_col_total.
  gs_specialcol-color-int = 0.
  APPEND gs_specialcol TO gt_specialcol.

  gs_specialcol-fieldname = 'INCR'.
  gs_specialcol-color-col = cl_gui_resources=>list_col_positive.
  APPEND gs_specialcol TO gt_specialcol.

  gs_specialcol-fieldname = 'DECR'.
  gs_specialcol-color-col = cl_gui_resources=>list_col_negative.
  APPEND gs_specialcol TO gt_specialcol.

  gt_out-tabcolor[] = gt_specialcol[].
  MODIFY gt_out TRANSPORTING tabcolor WHERE tabcolor IS initial.

ENDFORM.                    " SET_COLOR
*&---------------------------------------------------------------------*
*&      Form  USER_COMMAND
*&---------------------------------------------------------------------*
FORM user_command USING r_ucomm LIKE sy-ucomm
                        rs_selfield TYPE slis_selfield.

  CASE r_ucomm.
    WHEN 'BACK' OR 'CANC' OR 'EXIT'.
      LEAVE SCREEN.

    WHEN 'DET'.
      READ TABLE gt_out INDEX rs_selfield-tabindex.
      IF sy-subrc = 0.
        SUBMIT zacou115 WITH p_kokrs = p_kokrs
                        WITH p_year = p_year
                        WITH p_id = gt_out-id
                        WITH s_poper IN s_poper.
      ENDIF.
  ENDCASE.

ENDFORM.                    " USER_COMMAND
*&---------------------------------------------------------------------*
*&      Form  CHOOSE_REASON
*&---------------------------------------------------------------------*
*       Remain selected reason only when entered reason codes
*----------------------------------------------------------------------*
FORM choose_reason.
  DATA l_cnt TYPE i.

  IF NOT s_kzust[] IS INITIAL.
    READ TABLE s_kzust INDEX 1.

    IF s_kzust-option = 'BT'.
      CLEAR l_cnt.
      DESCRIBE TABLE s_kzust LINES l_cnt.

      IF l_cnt = 1.
        LOOP AT gt_rgrp
           WHERE rgrp2 BETWEEN s_kzust-low AND s_kzust-high.
          gt_106-chk = 'X'.
          MODIFY gt_106 TRANSPORTING chk
            WHERE kzust = gt_rgrp-rgrp2.
        ENDLOOP.
      ENDIF.

    ELSE.
      LOOP AT s_kzust.
        gt_106-chk = 'X'.

        MODIFY gt_106 TRANSPORTING chk
          WHERE kzust+0(1) = s_kzust-low+0(1)
            AND kzust+2(1) = s_kzust-low+1(1).

        IF NOT s_kzust-high IS INITIAL.
          MODIFY gt_106 TRANSPORTING chk
            WHERE kzust+0(1) = s_kzust-high+0(1)
              AND kzust+2(1) = s_kzust-high+1(1).
        ENDIF.
      ENDLOOP.

    ENDIF.

    DELETE gt_106 WHERE chk IS initial.

  ENDIF.

ENDFORM.                    " CHOOSE_REASON
*&---------------------------------------------------------------------*
*&      Form  PF_STATUS_SET
*&---------------------------------------------------------------------*
FORM pf_status_set USING rt_extab TYPE slis_t_extab.
  SET PF-STATUS 'STANDARD'.

ENDFORM.                    " PF_STATUS_SET
*&---------------------------------------------------------------------*
*&      Form  MOVE_RESP
*&---------------------------------------------------------------------*
FORM move_resp.
  gt_itab-zresc = gt_resp-zresc.     " Responsible Category
  gt_itab-zresp = gt_resp-zresp.     " Responsible Area
  gt_itab-text  = gt_resp-text.      " Description

  COLLECT gt_itab.
  CLEAR gt_itab.

ENDFORM.                    " MOVE_RESP
*&---------------------------------------------------------------------*
*&      Form  get_gt_106
*&---------------------------------------------------------------------*
FORM get_gt_106.

  CLEAR   gt_106.
  REFRESH gt_106.

  SELECT a~kokrs a~bdatj a~poper a~id a~kzust a~dwertn
         a~kstar a~lifnr a~upgvc a~compn
         b~idtext
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
        AND d~ver   EQ space
        AND d~matnr EQ a~compn
* }
   WHERE a~kokrs = p_kokrs
     AND a~bdatj = p_year
     AND a~kalka = p_kalka
     AND a~id IN s_id
     AND a~poper IN s_poper
     AND a~upgvc IN s_upgvc
     AND a~compn IN s_compn
     AND a~zrclss IN s_zrclss
     AND a~ekgrp IN s_ekgrp.


  SORT gt_106 BY kokrs bdatj id kzust.

ENDFORM.                    " get_gt_106
