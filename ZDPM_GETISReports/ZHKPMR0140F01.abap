*&---------------------------------------------------------------------*
*&  Include           ZHKPMR0140F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  INITIALIZATION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM initialization .

  s_aedat = 'IBT'.
  s_aedat-low = sy-datum - 1 .
  s_aedat-high = sy-datum - 1 .
  APPEND s_aedat .

ENDFORM.                    " INITIALIZATION
*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data .

  DATA : lt_t001k LIKE TABLE OF t001k WITH HEADER LINE ,
         lt_t001  LIKE TABLE OF t001  WITH HEADER LINE .

  DATA : lt_equz  LIKE TABLE OF equz  WITH HEADER LINE ,
         lt_iloa  LIKE TABLE OF iloa  WITH HEADER LINE ,
         lt_equi  LIKE TABLE OF equi  WITH HEADER LINE ,
         lt_anla  LIKE TABLE OF anla  WITH HEADER LINE ,
         lt_eqkt  LIKE TABLE OF eqkt  WITH HEADER LINE .

  DATA : lt_anlc  LIKE TABLE OF anlc  WITH HEADER LINE .

  DATA : lt_t357  LIKE TABLE OF t357  WITH HEADER LINE ,
         lt_t370k_t LIKE TABLE OF t370k_t WITH HEADER LINE ,
         lt_iflotx  LIKE TABLE OF iflotx  WITH HEADER LINE .

  DATA : lt_cskt  LIKE TABLE OF cskt  WITH HEADER LINE ,
         lt_t024i LIKE TABLE OF t024i WITH HEADER LINE .

  DATA : BEGIN OF lt_text OCCURS 0 ,
          objnr LIKE equi-objnr ,
          sttxt LIKE rihafvr-sttxt ,
         END OF lt_text .

  DATA : l_sttxt LIKE rihafvr-sttxt ,
         l_flag .

*. Get Company Code
  SELECT * INTO TABLE lt_t001k
    FROM t001k
   WHERE bwkey IN s_werks .

  SELECT * INTO TABLE lt_t001
    FROM t001
   WHERE bukrs EQ p_bukrs .


  CLEAR : gt_data[], gt_data .

*. Get row data : Equipment master data
  SELECT *
    INTO TABLE lt_equi
    FROM equi
   WHERE ( aedat IN s_aedat OR
           erdat IN s_aedat )
     AND eqtyp IN s_eqtyp .

*.. Check Object ( all Select )
  IF s_aedat[] IS INITIAL .
    LOOP AT lt_equi .

      CLEAR : l_sttxt ,
              l_flag .

      CALL FUNCTION 'STATUS_TEXT_EDIT'
        EXPORTING
          flg_user_stat    = 'X'
          objnr            = lt_equi-objnr
          only_active      = ' '
          spras            = sy-langu
        IMPORTING
          line             = l_sttxt
        EXCEPTIONS
          object_not_found = 1
          OTHERS           = 2.

      PERFORM read_status USING : l_sttxt
                                  'DLFL'
                                  l_flag ,
                                  l_sttxt
                                  'INAC'
                                  l_flag .
      IF l_flag = c_x .
        DELETE lt_equi .
      ENDIF .

      lt_text-objnr = lt_equi-objnr .
      lt_text-sttxt = l_sttxt .
      APPEND lt_text .
    ENDLOOP .
  ENDIF .
*.
  IF lt_equi[] IS NOT INITIAL .

*.. Equipment Short Texts
    SELECT * INTO TABLE lt_t370k_t
     FROM t370k_t
     FOR ALL ENTRIES IN lt_equi
    WHERE eqart EQ lt_equi-eqart
      AND spras EQ sy-langu .

*.. Equipment time segment
    SELECT * INTO TABLE lt_equz
      FROM equz
      FOR ALL ENTRIES IN lt_equi
     WHERE equnr EQ lt_equi-equnr
       AND datbi EQ '99991231' .

*.. Equipment Short Texts
    SELECT * INTO TABLE lt_eqkt
     FROM eqkt
     FOR ALL ENTRIES IN lt_equi
    WHERE equnr EQ lt_equi-equnr
      AND spras EQ sy-langu .

    IF lt_equz[] IS NOT INITIAL .

*.. Maintenance planner groups
      SELECT * INTO TABLE lt_t024i
        FROM t024i
        FOR ALL ENTRIES IN lt_equz
       WHERE ingrp EQ lt_equz-ingrp .

*.. PM Object Location and Account Assignment
      SELECT * INTO TABLE lt_iloa
        FROM iloa
        FOR ALL ENTRIES IN lt_equz
       WHERE iloan EQ lt_equz-iloan .

      DELETE lt_iloa WHERE swerk NOT IN s_werks[] .

      IF lt_iloa[] IS NOT INITIAL .

*.. Asset Master Record Segment
        SELECT * INTO TABLE lt_anla
          FROM anla
          FOR ALL ENTRIES IN lt_iloa
         WHERE  bukrs EQ p_bukrs
            AND anln1 EQ lt_iloa-anlnr .

*.. Asset Value Fields
        SELECT * INTO TABLE lt_anlc
          FROM anlc
          FOR ALL ENTRIES IN lt_iloa
         WHERE  bukrs EQ p_bukrs
            AND anln1 EQ lt_iloa-anlnr .

*.. Plant Section
        SELECT * INTO TABLE lt_t357
          FROM t357
          FOR ALL ENTRIES IN lt_iloa
         WHERE werks EQ lt_iloa-swerk
           AND beber EQ lt_iloa-beber .

*.. Functional Location: Short Texts
        SELECT * INTO TABLE lt_iflotx
          FROM iflotx
          FOR ALL ENTRIES IN lt_iloa
         WHERE tplnr EQ lt_iloa-tplnr
           AND spras EQ sy-langu .

*.. Cost Center Texts
        SELECT * INTO TABLE lt_cskt
          FROM cskt
          FOR ALL ENTRIES IN lt_iloa
         WHERE kostl EQ lt_iloa-kostl
           AND spras EQ sy-langu .

      ENDIF .

    ENDIF .


  ENDIF .

*.
  SORT lt_text BY objnr .
  SORT lt_t357 BY beber .
  SORT lt_iflotx BY tplnr .
  SORT lt_t370k_t BY eqart .
  SORT lt_cskt BY kostl .
  SORT lt_t024i BY ingrp .

  SORT lt_equz  BY equnr .
  SORT lt_eqkt  BY equnr .
  SORT lt_iloa  BY iloan .
  SORT lt_anla  BY anln1 .

  SORT lt_anlc  BY anln1 gjahr DESCENDING afabe DESCENDING
                   knafa DESCENDING .

  LOOP AT lt_equi .

    CLEAR  gt_data .
    CLEAR : lt_equz , lt_iloa , lt_anla , lt_eqkt ,
            lt_anlc .

    READ TABLE lt_equz  WITH KEY equnr = lt_equi-equnr
                                 BINARY SEARCH .

    READ TABLE lt_eqkt  WITH KEY equnr = lt_equi-equnr
                                 BINARY SEARCH .

    READ TABLE lt_iloa WITH KEY iloan = lt_equz-iloan
                                BINARY SEARCH .
    IF sy-subrc <> 0 .
      CONTINUE .
    ENDIF .

    READ TABLE lt_anla  WITH KEY anln1 = lt_iloa-anlnr
                                 BINARY SEARCH .

    READ TABLE lt_anlc  WITH KEY anln1 = lt_iloa-anlnr
                                 BINARY SEARCH .

    READ TABLE lt_t001k WITH KEY bwkey = lt_iloa-swerk .
    IF sy-subrc = 0 .
      gt_data-bukrs  = lt_t001k-bukrs .
    ENDIF .

    gt_data-swerk = lt_iloa-swerk .
    gt_data-equnr = lt_equi-equnr .

    gt_data-iloan = lt_iloa-iloan .
    gt_data-tplnr = lt_iloa-tplnr .
    gt_data-beber = lt_iloa-beber .
    gt_data-stort = lt_iloa-stort .

    gt_data-eqktu = lt_eqkt-eqktu .

    gt_data-eqtyp = lt_equi-eqtyp .
    gt_data-eqart = lt_equi-eqart .
    gt_data-groes = lt_equi-groes .
    gt_data-brgew = lt_equi-brgew .
    gt_data-gewei = lt_equi-gewei .

    gt_data-anlnr = lt_iloa-anlnr .

    gt_data-herst = lt_equi-herst .
    gt_data-herld = lt_equi-herld .
    gt_data-typbz = lt_equi-typbz .
    gt_data-ansdt = lt_equi-ansdt .

    gt_data-zujhr = lt_anla-zujhr .

    gt_data-zdamt  = lt_anlc-knafa .

    gt_data-menge = lt_anla-menge .
    gt_data-meins = lt_anla-meins .

    gt_data-answt = lt_equi-answt .
    gt_data-waers = lt_equi-waers .

    gt_data-kostl = lt_iloa-kostl .

    gt_data-ingrp = lt_equz-ingrp .
    gt_data-erdat = lt_equi-erdat .

    gt_data-proid = lt_iloa-proid .
    gt_data-inbdt = lt_equi-inbdt .

    IF s_aedat[] IS NOT INITIAL .
      CLEAR : l_sttxt ,
              l_flag .

      CALL FUNCTION 'STATUS_TEXT_EDIT'
        EXPORTING
          flg_user_stat    = 'X'
          objnr            = lt_equi-objnr
          only_active      = ' '
          spras            = sy-langu
        IMPORTING
          line             = l_sttxt
        EXCEPTIONS
          object_not_found = 1
          OTHERS           = 2.

      PERFORM read_status USING : l_sttxt
                                  'DLFL'
                                  l_flag ,
                                  l_sttxt
                                  'INAC'
                                  l_flag .

      gt_data-zdefl = l_flag .

      gt_data-sttxt = l_sttxt .
    ENDIF .

    READ TABLE lt_text WITH KEY objnr = lt_equi-objnr
                                BINARY SEARCH .
    IF sy-subrc = 0 .
      gt_data-sttxt = lt_text-sttxt .
    ENDIF .

    READ TABLE lt_t357 WITH KEY beber = lt_iloa-beber
                                BINARY SEARCH .
    IF sy-subrc = 0 .
      gt_data-fing = lt_t357-fing .
    ENDIF .

    READ TABLE lt_iflotx WITH KEY tplnr = lt_iloa-tplnr
                                  BINARY SEARCH .
    IF sy-subrc = 0 .
      gt_data-pltxt = lt_iflotx-pltxt .
    ENDIF .

    READ TABLE lt_t370k_t WITH KEY eqart = lt_equi-eqart
                                   BINARY SEARCH .
    IF sy-subrc = 0 .
      gt_data-eartx = lt_t370k_t-eartx .
    ENDIF .

    READ TABLE lt_cskt WITH KEY kostl = lt_iloa-kostl
                                BINARY SEARCH .
    IF sy-subrc = 0 .
      gt_data-ktext = lt_cskt-ktext .
    ENDIF .

    READ TABLE lt_t024i WITH KEY ingrp = lt_equz-ingrp
                                 BINARY SEARCH .
    IF sy-subrc = 0 .
      gt_data-innam = lt_t024i-innam .
    ENDIF .

    gt_data-icon = '@5D@'."  yellow
    APPEND gt_data .

  ENDLOOP .

ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  MODIFY_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM modify_data .
  DATA : lt_pmt0014 LIKE TABLE OF zhkpmt0014 WITH HEADER LINE .

  CHECK gt_data[] IS NOT INITIAL .

  SELECT * INTO TABLE lt_pmt0014
    FROM zhkpmt0014
    FOR ALL ENTRIES IN gt_data
   WHERE bukrs EQ gt_data-bukrs
     AND swerk EQ gt_data-swerk
     AND equnr EQ gt_data-equnr .


  SORT gt_data BY bukrs swerk equnr.

  LOOP AT lt_pmt0014 .
    READ TABLE gt_data WITH KEY bukrs = lt_pmt0014-bukrs
                                swerk = lt_pmt0014-swerk
                                equnr = lt_pmt0014-equnr
                                BINARY SEARCH .
    IF sy-subrc = 0 .
      gt_data-zifflag   = lt_pmt0014-zifflag .
      gt_data-zifresult = lt_pmt0014-zifresult .
      MODIFY gt_data INDEX sy-tabix .
    ENDIF .
  ENDLOOP .

ENDFORM.                    " MODIFY_DATA
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_ALV_SCREEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_alv_screen .

  PERFORM make_layout.

  PERFORM make_fieldcat.

  PERFORM make_sortcat.

  PERFORM call_alv_grid_function.

ENDFORM.                    " DISPLAY_ALV_SCREEN
*&---------------------------------------------------------------------*
*&      Form  MAKE_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM make_layout .

** Declare Init .
  MOVE : sy-repid TO g_repid,
         c_a      TO g_save.

  CLEAR : gs_layout, gt_event[] .

  MOVE : c_x       TO gs_layout-colwidth_optimize,
         'MARK'    TO gs_layout-box_fieldname,
         "C_X       TO GS_LAYOUT-TOTALS_BEFORE_ITEMS,
         c_x       TO gs_layout-zebra,
         c_x       TO gs_layout-cell_merge.


** Declare Event
  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
    EXPORTING
      i_list_type = 0
    IMPORTING
      et_events   = gt_event.

  MOVE: slis_ev_user_command TO gs_event-name,
        slis_ev_user_command TO gs_event-form.
  APPEND gs_event TO gt_event.

  MOVE: slis_ev_pf_status_set TO gs_event-name,
        slis_ev_pf_status_set TO gs_event-form.
  APPEND gs_event TO gt_event.

**  MOVE: SLIS_EV_TOP_OF_PAGE TO GS_EVENT-NAME,
**        SLIS_EV_TOP_OF_PAGE TO GS_EVENT-FORM.
**  APPEND GS_EVENT TO GT_EVENT.



ENDFORM.                    " MAKE_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  MAKE_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM make_fieldcat .

  DATA l_tabnam(10)   TYPE c.

  CLEAR : gt_fieldcat, gt_fieldcat[], gs_fieldcat.
  l_tabnam = 'GT_DATA'.

  PERFORM make_fieldcat_att USING:
   'X'  l_tabnam   'ICON'   'Status'    '04' ' ' ' ' ' ' ' ' 'C' ,
   'X'  l_tabnam   'BUKRS'  'CO Code'   '04' ' ' ' ' ' ' ' ' ' ' ,
   'X'  l_tabnam   'SWERK'  'Plant'     '04' ' ' ' ' ' ' ' ' ' ' ,
   'X'  l_tabnam   'EQUNR'  'Equipment' '18' ' ' ' ' ' ' ' ' ' ' ,

   ' '  l_tabnam   'ILOAN'  'Loc/ACC'   '12' ' ' ' ' ' ' ' ' ' ' ,
   ' '  l_tabnam   'BEBER'  'SHOP'      '03' ' ' ' ' ' ' ' ' ' ' ,
   ' '  l_tabnam   'FING'   'SHOP Desc'    '14' ' ' ' ' ' ' ' ' ' ' ,
   ' '  l_tabnam   'TPLNR'  'FLoc'      '10' ' ' ' ' ' ' ' ' ' ' ,
   ' '  l_tabnam   'PLTXT'  'FLoc Desc' '30' ' ' ' ' ' ' ' ' ' ' ,
   ' '  l_tabnam   'STORT'  'Location'  '10' ' ' ' ' ' ' ' ' ' ' ,

   ' '  l_tabnam   'EQKTU'  'Desc'      '30' ' ' ' ' ' ' ' ' ' ' ,

   ' '  l_tabnam   'EQTYP'  'EqptCat'   '05' ' ' ' ' ' ' ' ' 'C' ,

   ' '  l_tabnam   'EQART'  'Type'      '04' ' ' ' ' ' ' ' ' ' ' ,
   ' '  l_tabnam   'EARTX'  'Type Desc' '20' ' ' ' ' ' ' ' ' ' ' ,
   ' '  l_tabnam   'STTXT'  'System status' '30' ' ' ' ' ' ' ' ' ' ' ,

   ' '  l_tabnam   'GROES'  'Size'      '18' ' ' ' ' ' ' ' ' ' ' ,

   ' '  l_tabnam   'BRGEW'  'Weight'    '10' '1' ' ' ' ' ' ' ' ' ,
   ' '  l_tabnam   'GEWEI'  'Wunit'     '04' ' ' ' ' ' ' ' ' ' ' ,
   ' '  l_tabnam   'ANLNR'  'Asset'     '12' ' ' ' ' ' ' ' ' ' ' ,
   ' '  l_tabnam   'HERST'  'Manufacturer' '20' ' ' ' ' ' ' ' ' ' ' ,

   ' '  l_tabnam   'HERLD'  'Country'    '20' ' ' ' ' ' ' ' ' ' ' ,

   ' '  l_tabnam   'TYPBZ'  'Model'      '03' ' ' ' ' ' ' ' ' ' ' ,
   ' '  l_tabnam   'ANSDT'  'Acq Date'   '10' ' ' ' ' ' ' ' ' ' ' ,
   ' '  l_tabnam   'ZUJHR'  'Acq Yr'     '04' ' ' ' ' ' ' ' ' ' ' ,

   ' '  l_tabnam   'ZDAMT'  'Depr Amt'   '10' '3' ' ' ' ' ' ' ' ' ,
   ' '  l_tabnam   'MENGE'  'Qty'        '10' '5' ' ' ' ' ' ' ' ' ,
   ' '  l_tabnam   'MEINS'  'UNIT'       '04' ' ' ' ' ' ' ' ' ' ' ,
   ' '  l_tabnam   'ANSWT'  'Acq Val'    '10' '3' ' ' ' ' ' ' ' ' ,
   ' '  l_tabnam   'WAERS'  'Curr'       '04' ' ' ' ' ' ' ' ' ' ' ,

   ' '  l_tabnam   'KOSTL'  'CCtr'       '10' ' ' ' ' ' ' ' ' ' ' ,
   ' '  l_tabnam   'KTEXT'  'CCtr Name'  '20' ' ' ' ' ' ' ' ' ' ' ,

   ' '  l_tabnam   'INGRP'  'PlanGR'     '03' ' ' ' ' ' ' ' ' ' ' ,
   ' '  l_tabnam   'INNAM'  'PlanGR Desc' '18' ' ' ' ' ' ' ' ' ' ' ,

   ' '  l_tabnam   'ERDAT'  'Crt date'   '10' ' ' ' ' ' ' ' ' ' ' ,
   ' '  l_tabnam   'PROID'  'WBS'        '08' ' ' ' ' ' ' ' ' ' ' ,
   ' '  l_tabnam   'INBDT'  'Start Date' '10' ' ' ' ' ' ' ' ' ' ' ,
   ' '  l_tabnam   'ZDEFL'  'DLT'        '03' ' ' ' ' ' ' ' ' 'C' ,

   ' '  l_tabnam   'ZIFDATE'  'I/F Date' '10' ' ' ' ' ' ' ' ' ' ' ,
   ' '  l_tabnam   'ZIFEMP'  'I/F emp'   '10' ' ' ' ' ' ' ' ' ' ' ,
   ' '  l_tabnam   'ZIFFLAG'  'I/F Flag' '04' ' ' ' ' ' ' ' ' ' ' ,
   ' '  l_tabnam   'ZIFRESULT'  'I/F Result' '40' ' ' ' ' ' ' ' ' ' ' .

ENDFORM.                    " MAKE_FIELDCAT
*&---------------------------------------------------------------------*
*&      Form  MAKE_FIELDCAT_ATT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM make_fieldcat_att  USING p_key
                              p_tabname
                              p_fieldname
                              p_reptext_ddic
                              p_outputlen
                              p_no_out
                              p_no_zero
                              p_emphasize
                              p_hotspot
                              p_just .

  DATA : ls_fieldcat  TYPE slis_fieldcat_alv.

  CLEAR ls_fieldcat.

  ls_fieldcat-key          = p_key.
  ls_fieldcat-tabname      = p_tabname.
  ls_fieldcat-fieldname    = p_fieldname.
  ls_fieldcat-reptext_ddic = p_reptext_ddic.
  ls_fieldcat-outputlen    = p_outputlen.
  ls_fieldcat-no_zero      = p_no_zero.
  ls_fieldcat-emphasize    = p_emphasize.
  ls_fieldcat-hotspot      = p_hotspot.
  ls_fieldcat-just         = p_just .

  IF p_no_out = 'X'.
    ls_fieldcat-no_out       = p_no_out.
  ENDIF.
  IF p_no_out = '1'.
    ls_fieldcat-qfieldname   = 'GEWEI'.
*    ls_fieldcat-do_sum       = 'X'.
  ENDIF.
  IF p_no_out = '2'.
    ls_fieldcat-hotspot = 'X'.
  ENDIF.

  IF p_no_out = '3'.
    ls_fieldcat-cfieldname   = 'WAERS'.
  ENDIF.

  IF p_no_out = '5'.
    ls_fieldcat-qfieldname   = 'MEINS'.
  ENDIF.

  IF p_no_out = '7'.
    ls_fieldcat-cfieldname   = 'P_WAERS'.
  ENDIF.

  IF p_fieldname = 'MATNR_SORT'.
    ls_fieldcat-no_out  = 'X'.
  ENDIF.


  IF p_fieldname = 'EBELN' .
    ls_fieldcat-ref_fieldname  = 'EBELN' .
    ls_fieldcat-ref_tabname    = 'EKPO' .
  ENDIF .

  IF p_fieldname = 'EQUNR' .
    ls_fieldcat-ref_fieldname  = 'EQUNR' .
    ls_fieldcat-ref_tabname    = 'EQUI' .
  ENDIF .

  IF p_fieldname = 'MBLNR' .
    ls_fieldcat-ref_fieldname  = 'MBLNR' .
    ls_fieldcat-ref_tabname    = 'MKPF' .
  ENDIF .

  IF p_fieldname = 'AUFNR' .
    ls_fieldcat-ref_fieldname  = 'AUFNR' .
    ls_fieldcat-ref_tabname    = 'AUFM' .
  ENDIF .


  IF p_fieldname = 'WARPL' .
    ls_fieldcat-ref_fieldname  = 'WARPL' .
    ls_fieldcat-ref_tabname    = 'MHIS' .
  ENDIF .

  IF p_fieldname = 'MDOCM' .
    ls_fieldcat-ref_fieldname  = 'MDOCM' .
    ls_fieldcat-ref_tabname    = 'IMRG' .
  ENDIF .

  IF p_fieldname = 'POINT' .
    ls_fieldcat-ref_fieldname  = 'POINT' .
    ls_fieldcat-ref_tabname    = 'IMRG' .
  ENDIF .


  APPEND ls_fieldcat TO gt_fieldcat.


ENDFORM.                    " MAKE_FIELDCAT_ATT
*&---------------------------------------------------------------------*
*&      Form  MAKE_SORTCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM make_sortcat .

  CLEAR : gt_sortcat, gt_sortcat[], gs_sortcat .

*  MOVE : 1          TO gs_sortcat-spos ,
*       'BUKRS'      TO gs_sortcat-fieldname ,
*       'X'          TO gs_sortcat-up .
*  APPEND gs_sortcat TO gt_sortcat .
*  CLEAR  gs_sortcat .
*
*  MOVE : 2          TO gs_sortcat-spos ,
*       'WERKS'      TO gs_sortcat-fieldname ,
*       'X'          TO gs_sortcat-up .
**               'X'        TO GS_sortcat-subtot .
*  APPEND gs_sortcat TO gt_sortcat .
*  CLEAR  gs_sortcat .

ENDFORM.                    " MAKE_SORTCAT
*&---------------------------------------------------------------------*
*&      Form  CALL_ALV_GRID_FUNCTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM call_alv_grid_function .

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = g_repid
*      i_grid_title       = l_title
      i_save             = g_save
      is_layout          = gs_layout
      it_fieldcat        = gt_fieldcat[]
      it_sort            = gt_sortcat[]
      it_events          = gt_event[]
      is_print           = g_slis_print
      i_html_height_top  = 0
    TABLES
      t_outtab           = gt_data[]
    EXCEPTIONS
      program_error      = 1
      OTHERS             = 2.

ENDFORM.                    " CALL_ALV_GRID_FUNCTION
*&---------------------------------------------------------------------*
*&      Form  PF_STATUS_SET
*&---------------------------------------------------------------------*
*       PF-STATUS
*----------------------------------------------------------------------*
FORM pf_status_set USING rt_extab TYPE slis_t_extab.

  DATA: ls_extab TYPE slis_extab ,
        lt_extab TYPE slis_extab OCCURS 0 .


  SET PF-STATUS 'STANDARD' EXCLUDING lt_extab.

*  SET TITLEBAR 'T1100' WITH l_title.

ENDFORM.                    "PF_STATUS_SET
*---------------------------------------------------------------------*
*       FORM USER_COMMAND                                             *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  RF_UCOMM                                                      *
*  -->  RS_SELFIELD                                                   *
*---------------------------------------------------------------------*
FORM user_command USING rf_ucomm LIKE sy-ucomm
                        rs_selfield TYPE slis_selfield.
  DATA : seltab LIKE rsparams OCCURS 0 WITH HEADER LINE.
*-->
  DATA: BEGIN OF lt_mast OCCURS 0,
        matnr TYPE mast-matnr,
        werks TYPE mast-werks,
        stlan TYPE mast-stlan,
      END OF lt_mast.
*-->
  rs_selfield-col_stable  = 'X' .
  rs_selfield-row_stable  = 'X' .


  CASE rf_ucomm .
    WHEN '&IC1' .
      READ TABLE gt_data INDEX rs_selfield-tabindex .
      CHECK sy-subrc = 0 .

*      CASE rs_selfield-fieldname.
*        WHEN 'MATNR' .
*          SET PARAMETER ID 'MAT' FIELD gt_data-matnr .
*          CALL TRANSACTION 'MM03' AND SKIP FIRST SCREEN.
*      ENDCASE .

    WHEN 'ZRFC' . "Call RFC
      PERFORM get_line_rfc .
      rs_selfield-refresh = c_x .

  ENDCASE .

ENDFORM.                    "USER_COMMAND
*&---------------------------------------------------------------------*
*&      Form  GET_LINE_RFC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_line_rfc .

  READ TABLE gt_data WITH KEY mark = c_x .
  IF sy-subrc <> 0 .
    MESSAGE s000 WITH 'There is no selected line' .
    EXIT .
  ENDIF .

  SELECT SINGLE * FROM usr01 WHERE bname EQ sy-uname.

  DATA : lt_data LIKE zhkpmt0014 OCCURS 0 WITH HEADER LINE ,
         lt_send LIKE zhkpmt0014 OCCURS 0 WITH HEADER LINE .

  DATA : lt_pmt0014 LIKE TABLE OF zhkpmt0014 WITH HEADER LINE .

  LOOP AT gt_data  .

    IF gt_data-mark IS INITIAL .
      CONTINUE .
    ENDIF .

    gt_data-zifflag = c_i .
    gt_data-zifdate = sy-datum .
    CLEAR gt_data-zifresult .
    MOVE-CORRESPONDING gt_data TO lt_data .
    APPEND lt_data .
    MODIFY gt_data .

    MOVE-CORRESPONDING gt_data TO lt_pmt0014 .
    APPEND lt_pmt0014 .

  ENDLOOP .

*. Update CBO table
  MODIFY zhkpmt0014 FROM TABLE lt_pmt0014 .
*  SORT lt_pmt0014 BY werks .

  DATA : l_at     TYPE i ,
         l_total  TYPE i ,
         l_start  TYPE i ,
         l_end    TYPE i .
  DATA : l_answt(20) ,
         l_zdamt(20) .



  CLEAR :  g_success , g_error .

  DESCRIBE TABLE lt_data LINES l_total .

  l_at = 1000 .

  DO .

    IF l_start IS INITIAL .
      l_start = 1 .
      l_end   = l_at .
    ELSE .
      l_start = l_end  + 1 .
      l_end   = l_end + l_at .
    ENDIF .

    IF l_start > l_total .
      EXIT .
    ENDIF .

    IF l_end > l_total .
      l_end = l_total .
    ENDIF .

    CLEAR lt_send[] .
    APPEND LINES OF lt_data  FROM l_start TO l_end
                 TO lt_send .

*. Conversion Price
    LOOP AT lt_send .

      IF lt_send-waers IS NOT INITIAL .
        WRITE lt_send-answt TO l_answt CURRENCY lt_send-waers .
        PERFORM set_write_no_mask USING  l_answt .
        lt_send-answt = l_answt .

        WRITE lt_send-zdamt TO l_zdamt CURRENCY lt_send-waers .
        PERFORM set_write_no_mask USING  l_zdamt .
        lt_send-zdamt = l_zdamt .
      ENDIF .

      MODIFY lt_send .

      UPDATE zhkpmt0014
         SET zifflag = c_p
       WHERE bukrs = lt_send-bukrs
         AND swerk = lt_send-swerk
         AND equnr = lt_send-equnr.

    ENDLOOP .

    COMMIT WORK .

*. Call RFC
    CALL FUNCTION 'ZPM014_EQPT_GETIS'
      DESTINATION p_dest
      TABLES
        t_data                = lt_send
      EXCEPTIONS
        system_failure        = 1
        communication_failure = 2.
* ICON
* '@5B@'."  green
* '@5C@'."  red
* '@5D@'."  yellow

    IF sy-subrc <> 0 .
      LOOP AT lt_send .
        READ TABLE gt_data WITH KEY swerk = lt_send-swerk
                                    equnr = lt_send-equnr .
        .

        IF sy-subrc = 0 .
          gt_data-icon = '@5C@'."  red
          MODIFY gt_data INDEX sy-tabix .

          UPDATE zhkpmt0014
             SET zifflag = c_e
                 zifresult = 'system_failure '
           WHERE bukrs = lt_send-bukrs
             AND swerk = lt_send-swerk
             AND equnr = lt_send-equnr .

        ENDIF .

        g_error = g_error + 1.
      ENDLOOP .

    ELSE .

      LOOP AT lt_send .
        READ TABLE gt_data WITH KEY swerk = lt_send-swerk
                                    equnr = lt_send-equnr .
        IF sy-subrc = 0 .
          IF lt_send-zifflag = 'S' OR
             lt_send-zifflag = 'Z' .
            gt_data-icon = '@5B@'."  green
          ELSE .
            gt_data-icon = '@5C@'."  red
          ENDIF .
          gt_data-zifflag   = lt_send-zifflag .
          gt_data-zifresult = lt_send-zifresult .
          MODIFY gt_data INDEX sy-tabix .
        ENDIF .

        UPDATE zhkpmt0014
       SET zifflag   = lt_send-zifflag
           zifresult = lt_send-zifresult
         WHERE bukrs = lt_send-bukrs
           AND swerk = lt_send-swerk
           AND equnr = lt_send-equnr .

        g_success = g_success + 1 .
      ENDLOOP .

    ENDIF .

  ENDDO .

*.
  IF g_error IS NOT INITIAL .
    MESSAGE s001 WITH 'Error : Transfer.' .
  ELSE .
    MESSAGE s001 WITH 'Has been completed : Transfer.' .
  ENDIF .

ENDFORM.                    " GET_LINE_RFC
*&---------------------------------------------------------------------*
*&      Form  PROCESS_BATCH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM process_batch .

  gt_data-mark = c_x .
  MODIFY gt_data FROM gt_data TRANSPORTING mark WHERE mark = space .

  PERFORM get_line_rfc .

ENDFORM.                    " PROCESS_BATCH
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_LOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_log .

**  ------------------------------------------------------
**  PGM. Name .TEXTXXXXXXXXXXXXXXXX (T-CODE, Program Discription)
**  JOB-Start Time: YYYY.MM.DD HH:MM:SS
**  JOB-End  Time: YYYY.MM.DD HH:MM:SS
**  Data Process count: xxxx EA
**  ------------------------------------------------------
  DATA: l_tabix_a  TYPE sytabix,
        l_tabix_b  TYPE sytabix,
        l_start(22),
        l_end(22).

  WRITE g_job_start_date TO l_start+0(10).
  WRITE g_job_start_time TO l_start+11(10).
  WRITE g_job_end_date   TO l_end+0(10).
  WRITE g_job_end_time   TO l_end+11(10).

  WRITE:/2 sy-uline(109).
  WRITE:/2(29) 'PGM Namr          :', (10) sy-cprog, (70) sy-title.
  WRITE:/2(29) 'JOB-Start Time    :', (20) l_start.
  WRITE:/2(29) 'JOB-End Time      :', (20) l_end.

  WRITE:/2 sy-uline(109).

ENDFORM.                    " DISPLAY_LOG
*&---------------------------------------------------------------------*
*&      Form  SELECTION_SCREEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM selection_screen .

  IF sy-ucomm = 'ONLI' .
  ENDIF .

ENDFORM.                    " SELECTION_SCREEN
*&---------------------------------------------------------------------*
*&      Form  READ_STATUS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM read_status  USING    p_sttxt
                           p_at
                           p_flag.

  SEARCH p_sttxt FOR p_at .
  IF sy-subrc = 0 .
    p_flag = c_x .
  ENDIF .

ENDFORM.                    " READ_STATUS
