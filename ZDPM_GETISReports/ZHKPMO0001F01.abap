*&---------------------------------------------------------------------*
*&  Include           ZHKPMR0001F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data .

  CLEAR : gt_data[] .
  CASE c_x .
    WHEN p_rda . "display
      PERFORM get_data_display .
    WHEN p_rdb . "New
      PERFORM get_data_new .
  ENDCASE .

ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  GET_DATA_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data_display .

  CLEAR : gt_data[], gt_data .
  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE gt_data
    FROM zhkpmt0008
   WHERE werks IN s_werks
     AND lgort IN s_lgort
     AND matnr IN s_matnr
     AND zdate IN s_zdate .

  LOOP AT gt_data .
    gt_data-z2giq = gt_data-zmonq01 + gt_data-zmonq02 +
                    gt_data-zmonq03 + gt_data-zmonq04 +
                    gt_data-zmonq05 + gt_data-zmonq06 +
                    gt_data-zmonq07 + gt_data-zmonq08 +
                    gt_data-zmonq09 + gt_data-zmonq10 +
                    gt_data-zmonq11 + gt_data-zmonq12 +
                    gt_data-zmonq13 + gt_data-zmonq14 +
                    gt_data-zmonq15 + gt_data-zmonq16 +
                    gt_data-zmonq17 + gt_data-zmonq18 +
                    gt_data-zmonq19 + gt_data-zmonq20 +
                    gt_data-zmonq21 + gt_data-zmonq22 +
                    gt_data-zmonq23 + gt_data-zmonq24 .
    MODIFY gt_data .
  ENDLOOP .

ENDFORM.                    " GET_DATA_DISPLAY
*&---------------------------------------------------------------------*
*&      Form  GET_DATA_NEW
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data_new .
  DATA : lt_mard LIKE TABLE OF mard WITH HEADER LINE ,
         lt_marc LIKE TABLE OF marc WITH HEADER LINE ,
         lt_eban LIKE TABLE OF eban WITH HEADER LINE ,
         lt_ekpo LIKE TABLE OF ekpo WITH HEADER LINE ,
         lt_mseg LIKE TABLE OF zvpm_mksg WITH HEADER LINE .

*.. Storage Location Data for Material
  SELECT * INTO TABLE lt_mard
    FROM mard
   WHERE werks IN s_werks
     AND lgort IN s_lgort
     AND matnr IN s_matnr
     AND lvorm EQ space .

  CHECK lt_mard[] IS NOT INITIAL .

*.. Plant Data for Material
  SELECT * INTO TABLE lt_marc
    FROM marc
    FOR ALL ENTRIES IN lt_mard
   WHERE werks EQ lt_mard-werks
     AND matnr EQ lt_mard-matnr .

*.. Purchase Requisition
  SELECT * INTO TABLE lt_eban
    FROM eban
    FOR ALL ENTRIES IN lt_mard
   WHERE werks EQ lt_mard-werks
     AND matnr EQ lt_mard-matnr
     AND loekz EQ space .

  DELETE lt_eban WHERE ebeln <> space .
  DELETE lt_eban WHERE lgort NOT IN s_lgort .

*.. Purchasing Document Item
  SELECT * INTO TABLE lt_ekpo
    FROM ekpo
    FOR ALL ENTRIES IN lt_mard
   WHERE werks EQ lt_mard-werks
     AND matnr EQ lt_mard-matnr .


  DELETE lt_ekpo WHERE loekz <> space OR
                       elikz <> space .
  DELETE lt_ekpo WHERE lgort NOT IN s_lgort .

*.. MKPF + MSEG
  IF lt_ekpo[] IS NOT INITIAL .
    SELECT * INTO TABLE lt_mseg
      FROM zvpm_mksg
       FOR ALL ENTRIES IN lt_ekpo
     WHERE ebeln EQ lt_ekpo-ebeln
       AND ebelp EQ lt_ekpo-ebelp .
  ENDIF .

  SORT lt_marc BY werks matnr .
  SORT lt_eban BY werks lgort matnr .
  SORT lt_ekpo BY werks lgort matnr .
  SORT lt_mseg BY ebeln ebelp .

  DATA : l_prqty LIKE eban-menge ,
         l_poqty LIKE ekpo-menge ,
         l_menge LIKE mseg-menge ,
         l_totqty LIKE zhkpmt0008-zprpqq .

  LOOP AT lt_mard .

    CLEAR : gt_data .
    CLEAR : l_prqty , l_poqty .

    CLEAR : lt_marc .
    READ TABLE lt_marc WITH KEY werks = lt_mard-werks
                                matnr = lt_mard-matnr
                                BINARY SEARCH .

    " PR qty
    READ TABLE lt_eban WITH KEY werks = lt_mard-werks
                                lgort = lt_mard-lgort
                                matnr = lt_mard-matnr
                                BINARY SEARCH .
    IF sy-subrc = 0 .
      LOOP AT lt_eban FROM sy-tabix .
        IF lt_eban-werks <> lt_mard-werks OR
           lt_eban-lgort <> lt_mard-lgort OR
           lt_eban-matnr <> lt_mard-matnr .
          EXIT .
        ENDIF .
        l_prqty = l_prqty + lt_eban-menge .
      ENDLOOP .
    ENDIF .


    " PO qty
    READ TABLE lt_ekpo WITH KEY werks = lt_mard-werks
                                lgort = lt_mard-lgort
                                matnr = lt_mard-matnr
                                BINARY SEARCH .
    IF sy-subrc = 0 .
      LOOP AT lt_ekpo FROM sy-tabix .
        IF lt_ekpo-werks <> lt_mard-werks OR
           lt_ekpo-lgort <> lt_mard-lgort OR
           lt_ekpo-matnr <> lt_mard-matnr .
          EXIT .
        ENDIF .
        l_prqty = l_prqty + lt_ekpo-menge .

        CLEAR : l_menge .

        READ TABLE lt_mseg WITH KEY ebeln = lt_ekpo-ebeln
                                    ebelp = lt_ekpo-ebelp
                                    BINARY SEARCH .
        IF sy-subrc = 0 .
          LOOP AT lt_mseg FROM sy-tabix .
            IF lt_mseg-ebeln <> lt_ekpo-ebeln OR
               lt_mseg-ebelp <> lt_ekpo-ebelp .
              EXIT .
            ENDIF .
            IF lt_mseg-shkzg = 'H' .
              lt_mseg-menge = lt_mseg-menge * -1 .
            ENDIF .

            l_menge = l_menge + lt_mseg-menge .

          ENDLOOP .
        ENDIF .

        l_prqty = l_prqty - l_menge .
      ENDLOOP .
    ENDIF .

    gt_data-labst   = lt_mard-labst .
    gt_data-zprpqq  = l_prqty + l_poqty .
    gt_data-lminb   = lt_mard-lminb .

    l_totqty = gt_data-labst + gt_data-zprpqq .

    IF gt_data-lminb <= l_totqty .
      CONTINUE .
    ENDIF .

    gt_data-werks = lt_mard-werks .
    gt_data-lgort = lt_mard-lgort .
    gt_data-matnr = lt_mard-matnr .
    gt_data-zdate = sy-datum .

    gt_data-maabc = lt_marc-maabc .
    gt_data-lbstf = lt_mard-lbstf .

    APPEND gt_data .

  ENDLOOP .


ENDFORM.                    " GET_DATA_NEW
*&---------------------------------------------------------------------*
*&      Form  MODIFY_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM modify_data .
  DATA : lt_mara LIKE TABLE OF mara WITH HEADER LINE ,
         lt_makt LIKE TABLE OF makt WITH HEADER LINE .

  DATA : lt_aufm LIKE TABLE OF aufm WITH HEADER LINE .

  DATA : lt_pmv0008 LIKE TABLE OF zhkpmv0008 WITH HEADER LINE .

  DATA : lt_zvpm_mksg   LIKE TABLE OF zvpm_mksg  WITH HEADER LINE .

  RANGES : r_bwart FOR zhkpmv0008-bwart .

  DATA : BEGIN OF lt_mqty OCCURS 0 ,
          werks LIKE aufm-werks ,
          lgort LIKE aufm-lgort ,
          matnr LIKE aufm-matnr ,
          zmonq01 TYPE zqpmmonq ,
          zmonq02 TYPE zqpmmonq ,
          zmonq03 TYPE zqpmmonq ,
          zmonq04 TYPE zqpmmonq ,
          zmonq05 TYPE zqpmmonq ,
          zmonq06 TYPE zqpmmonq ,
          zmonq07 TYPE zqpmmonq ,
          zmonq08 TYPE zqpmmonq ,
          zmonq09 TYPE zqpmmonq ,
          zmonq10 TYPE zqpmmonq ,
          zmonq11 TYPE zqpmmonq ,
          zmonq12 TYPE zqpmmonq ,
          zmonq13 TYPE zqpmmonq ,
          zmonq14 TYPE zqpmmonq ,
          zmonq15 TYPE zqpmmonq ,
          zmonq16 TYPE zqpmmonq ,
          zmonq17 TYPE zqpmmonq ,
          zmonq18 TYPE zqpmmonq ,
          zmonq19 TYPE zqpmmonq ,
          zmonq20 TYPE zqpmmonq ,
          zmonq21 TYPE zqpmmonq ,
          zmonq22 TYPE zqpmmonq ,
          zmonq23 TYPE zqpmmonq ,
          zmonq24 TYPE zqpmmonq ,
         END OF lt_mqty .

  DATA :  l_field(30)  TYPE c,
          l_count(2)   TYPE n.
  FIELD-SYMBOLS: <fs_001> .


  CHECK p_rdb = c_x .

  CHECK gt_data[] IS NOT INITIAL .

*.. General Material Data
  SELECT * INTO TABLE lt_mara
    FROM mara
    FOR ALL ENTRIES IN gt_data
   WHERE matnr EQ gt_data-matnr .

*.. Material Descriptions
  SELECT * INTO TABLE lt_makt
    FROM makt
    FOR ALL ENTRIES IN gt_data
   WHERE matnr EQ gt_data-matnr
     AND spras EQ sy-langu .

*.. Get BUDAT
  PERFORM get_range_budat .

*.. PM G/I Movement Type
  SELECT * INTO TABLE lt_pmv0008
   FROM zhkpmv0008 .

  LOOP AT lt_pmv0008 .
    r_bwart = 'IEQ' .
    r_bwart-low = lt_pmv0008-bwart .
    APPEND r_bwart .
  ENDLOOP .

*.. Goods movements for order
  SELECT * INTO TABLE lt_aufm
    FROM aufm
    FOR ALL ENTRIES IN gt_data
   WHERE werks EQ gt_data-werks
     AND lgort EQ gt_data-lgort
     AND matnr EQ gt_data-matnr
     AND budat IN r_budat .
*     AND bwart IN r_bwart .

*. Get row data : GI
  IF r_bwart[] IS NOT INITIAL .
    SELECT *
      INTO TABLE lt_zvpm_mksg
      FROM zvpm_mksg
      FOR ALL ENTRIES IN gt_data
     WHERE werks EQ gt_data-werks
       AND lgort EQ gt_data-lgort
       AND matnr EQ gt_data-matnr .
*       AND bwart IN r_bwart
*       AND budat IN r_budat .

    DELETE lt_zvpm_mksg WHERE bwart NOT IN r_bwart .
    DELETE lt_zvpm_mksg WHERE budat NOT IN r_budat .
  ENDIF .

  SORT gt_yymm BY yymm .

  LOOP AT lt_aufm .

    IF lt_aufm-shkzg = 'H' .
      lt_aufm-erfmg = lt_aufm-erfmg * -1 .
    ENDIF .

    READ TABLE gt_yymm WITH KEY yymm = lt_aufm-budat+0(6)
                                       BINARY SEARCH .
    IF sy-subrc = 0 .
      l_count = sy-tabix .
      CLEAR lt_mqty .
      lt_mqty-werks = lt_aufm-werks .
      lt_mqty-lgort = lt_aufm-lgort .
      lt_mqty-matnr = lt_aufm-matnr .

      CONCATENATE 'LT_MQTY-ZMONQ' l_count  INTO l_field.
      ASSIGN (l_field)                     TO   <fs_001>.
      IF sy-subrc = 0 .
        <fs_001> = lt_aufm-erfmg .
      ENDIF .

      COLLECT lt_mqty .
    ENDIF .

  ENDLOOP .


  LOOP AT lt_zvpm_mksg .

    IF lt_zvpm_mksg-shkzg = 'H' .
      lt_zvpm_mksg-menge = lt_zvpm_mksg-menge * -1 .
    ENDIF .

    READ TABLE gt_yymm WITH KEY yymm = lt_zvpm_mksg-budat+0(6)
                                       BINARY SEARCH .
    IF sy-subrc = 0 .
      l_count = sy-tabix .
      CLEAR lt_mqty .
      lt_mqty-werks = lt_zvpm_mksg-werks .
      lt_mqty-lgort = lt_zvpm_mksg-lgort .
      lt_mqty-matnr = lt_zvpm_mksg-matnr .

      CONCATENATE 'LT_MQTY-ZMONQ' l_count  INTO l_field.
      ASSIGN (l_field)                     TO   <fs_001>.
      IF sy-subrc = 0 .
        <fs_001> = lt_zvpm_mksg-menge .
      ENDIF .

      COLLECT lt_mqty .
    ENDIF .

  ENDLOOP .

  SORT lt_mara BY matnr .
  SORT lt_makt BY matnr .
  SORT lt_mqty BY werks lgort matnr .

  LOOP AT gt_data .

    CLEAR : lt_mara , lt_makt , lt_mqty .

    READ TABLE lt_mara WITH KEY matnr = gt_data-matnr
                                BINARY SEARCH .
    READ TABLE lt_makt WITH KEY matnr = gt_data-matnr
                              BINARY SEARCH .
    READ TABLE lt_mqty WITH KEY werks = gt_data-werks
                                lgort = gt_data-lgort
                                matnr = gt_data-matnr
                                BINARY SEARCH .

    gt_data-mtart = lt_mara-mtart .
    gt_data-maktx = lt_makt-maktx .
    gt_data-wrkst = lt_mara-wrkst .
    gt_data-matkl = lt_mara-matkl .
    gt_data-meins = lt_mara-meins .

    IF lt_mqty IS NOT INITIAL .
      MOVE-CORRESPONDING lt_mqty TO gt_data .
    ENDIF .

    gt_data-z2giq = gt_data-zmonq01 + gt_data-zmonq02 +
                    gt_data-zmonq03 + gt_data-zmonq04 +
                    gt_data-zmonq05 + gt_data-zmonq06 +
                    gt_data-zmonq07 + gt_data-zmonq08 +
                    gt_data-zmonq09 + gt_data-zmonq10 +
                    gt_data-zmonq11 + gt_data-zmonq12 +
                    gt_data-zmonq13 + gt_data-zmonq14 +
                    gt_data-zmonq15 + gt_data-zmonq16 +
                    gt_data-zmonq17 + gt_data-zmonq18 +
                    gt_data-zmonq19 + gt_data-zmonq20 +
                    gt_data-zmonq21 + gt_data-zmonq22 +
                    gt_data-zmonq23 + gt_data-zmonq24 .

    IF gt_data-lbstf <> 0 .
      gt_data-menge = gt_data-lbstf - ( gt_data-zprpqq + gt_data-labst ) .
    ENDIF .

    MODIFY gt_data .

  ENDLOOP .

ENDFORM.                    " MODIFY_DATA
*&---------------------------------------------------------------------*
*&      Form  GET_RANGE_BUDAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_range_budat .
  DATA : l_date LIKE sy-datum ,
         l_yyyy(4) TYPE n ,
         l_mm(2)   TYPE n .
  DATA : l_yymm(6) TYPE n .

  CLEAR : r_budat , gt_yymm, gt_yymm[] .

  l_date = sy-datum .
  l_date+6(2) = '01' .
  l_date = l_date - 1 .

  r_budat = 'IBT' .

  IF sy-datum+4(2) = '01' .
    l_yyyy = l_date+0(4) .
    r_budat-high+0(4) = l_yyyy .
    r_budat-high+4(2) = '12' .
    r_budat-high+6(2) = '31' .
    l_yyyy = l_yyyy - 1.
    r_budat-low+0(4) = l_yyyy .
    r_budat-low+4(2) = '12' .
    r_budat-low+6(2) = '01' .
    APPEND r_budat .
  ELSE .
    l_yyyy = l_date+0(4) .
    l_mm   = l_date+4(2) .
    r_budat-high+0(4) = l_yyyy .
    r_budat-high+4(2) = l_mm  .
    r_budat-high+6(2) = '31' .
    l_yyyy = l_yyyy - 2.
    r_budat-low+0(4) = l_yyyy .
    r_budat-low+4(2) = 12 - l_mm .
    r_budat-low+6(2) = '01' .
    APPEND r_budat .
  ENDIF .

  l_yymm = r_budat-low+0(6) .

  DO 24 TIMES .

    gt_yymm-yymm = l_yymm .
    APPEND gt_yymm .

    IF l_yymm+4(2) = '12' .
      l_yymm+0(4) = l_yymm+0(4) + 1 .
      l_yymm+4(2) = '01' .
    ELSE .
      l_yymm+4(2) = l_yymm+4(2) + 1 .
    ENDIF .

  ENDDO .

ENDFORM.                    " GET_RANGE_BUDAT
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

         "C_X       TO GS_LAYOUT-TOTALS_BEFORE_ITEMS,
         c_x       TO gs_layout-zebra,
         c_x       TO gs_layout-cell_merge.

  IF p_rdb = c_x .
    MOVE : 'MARK'    TO gs_layout-box_fieldname .
  ENDIF .

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

  CASE c_x .
    WHEN p_rda .
      PERFORM make_fieldcat_rda .
    WHEN p_rdb .
      PERFORM make_fieldcat_rdb .
  ENDCASE .

ENDFORM.                    " MAKE_FIELDCAT
*&---------------------------------------------------------------------*
*&      Form  MAKE_FIELDCAT_RDA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM make_fieldcat_rda .

  DATA l_tabnam(10)   TYPE c.

  CLEAR : gt_fieldcat, gt_fieldcat[], gs_fieldcat.
  l_tabnam = 'GT_DATA'.

  PERFORM make_fieldcat_att USING:
   'X'  l_tabnam   'WERKS'  'Plant'    '05' ' ' ' ' ' ' ' ' ' ' ,
   'X'  l_tabnam   'LGORT'  'Sloc'     '10' ' ' ' ' ' ' ' ' ' ' ,

   'X'  l_tabnam   'MATNR'  'Material'    '18' ' ' ' ' ' ' ' ' ' ' ,
   ' '  l_tabnam   'MTART'  'MType'       '05' ' ' ' ' ' ' ' ' ' ' ,
   ' '  l_tabnam   'MAKTX'  'Description' '30' ' ' ' ' ' ' ' ' ' ' ,
   ' '  l_tabnam   'WRKST'  'SPEC'        '20' ' ' ' ' ' ' ' ' ' ' ,

   ' '  l_tabnam   'MATKL'  'Matl Gr'     '05' ' ' ' ' ' ' ' ' ' ' ,
   ' '  l_tabnam   'MAABC'  'ABC'         '03' ' ' ' ' ' ' ' ' 'C' ,
   ' '  l_tabnam   'MEINS'  'UMO'         '04' ' ' ' ' ' ' ' ' ' ' ,

   ' '  l_tabnam   'LABST'  'Current Stock' '10' '1' ' ' ' ' ' ' ' ' ,
   ' '  l_tabnam   'LMINB'  'Reorder point(MIN)'  '10' '1' ' ' ' ' ' ' ' ' ,
   ' '  l_tabnam   'LBSTF'  'Repl Qty(MAX)'      '10' '1' ' ' ' ' ' ' ' ' ,
   ' '  l_tabnam   'ZPRPQQ'  'PRPO Qty'     '10' '1' ' ' ' ' ' ' ' ' ,

   ' '  l_tabnam   'Z2GIQ'  'G/I Qty(2Yr)'  '10' '1' ' ' ' ' ' ' ' ' ,
   ' '  l_tabnam   'MENGE'  'PR Qty'        '10' '1' ' ' ' ' ' ' ' ' ,

   ' '  l_tabnam   'PREIS'  'Price'      '08' '3' ' ' ' ' ' ' ' ' ,

   ' '  l_tabnam   'WAERS'  'CrCy'       '06' ' ' ' ' ' ' ' ' ' ' ,
   ' '  l_tabnam   'NETWR'  'Amount'     '08' '3' ' ' ' ' ' ' ' ' ,
   ' '  l_tabnam   'ZDATE'  'Req date'   '10' ' ' ' ' ' ' ' ' ' ' ,
   ' '  l_tabnam   'BADAT'  'Deliv.Date' '10' ' ' ' ' ' ' ' ' ' ' ,
*   ' '  l_tabnam   'DISUB_PSPNR'  'WBS'  '05' ' ' ' ' ' ' ' ' ' ' ,
   ' '  l_tabnam   'BANFN'  'PR'         '10' ' ' ' ' ' ' ' ' ' ' ,
   ' '  l_tabnam   'BNFPO'  'Item'       '05' ' ' ' ' ' ' ' ' ' ' ,
   ' '  l_tabnam   'BSART'  'DocType'    '04' ' ' ' ' ' ' ' ' ' '  .


ENDFORM.                    " MAKE_FIELDCAT_RDA
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

  IF p_rda = c_x .
    APPEND 'ZEXEC' TO lt_extab .
  ENDIF .

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
*    WHEN '&IC1' .
*      READ TABLE gt_data INDEX rs_selfield-tabindex .
*      CHECK sy-subrc = 0 .
*
*      CASE rs_selfield-fieldname.
*        WHEN 'MATNR' .
*          SET PARAMETER ID 'MAT' FIELD gt_data-matnr .
*          CALL TRANSACTION 'MM03' AND SKIP FIRST SCREEN.
*      ENDCASE .

    WHEN 'ZEXEC' . "Call screen 0100
      PERFORM call_screen_0100 .
      rs_selfield-refresh = c_x .

  ENDCASE .

ENDFORM.                    "USER_COMMAND
*&---------------------------------------------------------------------*
*&      Form  MAKE_FIELDCAT_ATT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM make_fieldcat_att USING  p_key
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
    ls_fieldcat-qfieldname   = 'MEINS'.
*    ls_fieldcat-do_sum       = 'X'.
  ENDIF.
  IF p_no_out = '2'.
    ls_fieldcat-hotspot = 'X'.
  ENDIF.

  IF p_no_out = '3'.
    ls_fieldcat-cfieldname   = 'WAERS'.
  ENDIF.

  IF p_fieldname = 'MATNR_SORT'.
    ls_fieldcat-no_out  = 'X'.
  ENDIF.


  IF p_fieldname = 'EQUNR' .
    ls_fieldcat-ref_fieldname  = 'EQUNR' .
    ls_fieldcat-ref_tabname    = 'EQUI' .
  ENDIF .

  APPEND ls_fieldcat TO gt_fieldcat.


ENDFORM.                    " MAKE_FIELDCAT_ATT
*&---------------------------------------------------------------------*
*&      Form  MAKE_FIELDCAT_RDB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM make_fieldcat_rdb .

  DATA l_tabnam(10)   TYPE c.

  CLEAR : gt_fieldcat, gt_fieldcat[], gs_fieldcat.
  l_tabnam = 'GT_DATA'.

  PERFORM make_fieldcat_att USING:
   'X'  l_tabnam   'WERKS'  'Plant'    '05' ' ' ' ' ' ' ' ' ' ' ,
   'X'  l_tabnam   'LGORT'  'Sloc'     '10' ' ' ' ' ' ' ' ' ' ' ,

   'X'  l_tabnam   'MATNR'  'Material'    '18' ' ' ' ' ' ' ' ' ' ' ,
   ' '  l_tabnam   'MTART'  'MType'       '05' ' ' ' ' ' ' ' ' ' ' ,
   ' '  l_tabnam   'MAKTX'  'Description' '30' ' ' ' ' ' ' ' ' ' ' ,
   ' '  l_tabnam   'WRKST'  'SPEC'        '20' ' ' ' ' ' ' ' ' ' ' ,

   ' '  l_tabnam   'MATKL'  'Matl Gr'     '05' ' ' ' ' ' ' ' ' ' ' ,
   ' '  l_tabnam   'MAABC'  'ABC'         '03' ' ' ' ' ' ' ' ' 'C' ,
   ' '  l_tabnam   'MEINS'  'UMO'         '04' ' ' ' ' ' ' ' ' ' ' ,

   ' '  l_tabnam   'LABST'  'Current Stock' '10' '1' ' ' ' ' ' ' ' ' ,
   ' '  l_tabnam   'LMINB'  'Safety Stock(MIN)'  '10' '1' ' ' ' ' ' ' ' ' ,
   ' '  l_tabnam   'LBSTF'  'Repl Qty(MAX)'      '10' '1' ' ' ' ' ' ' ' ' ,
   ' '  l_tabnam   'ZPRPQQ'  'PRPO Qty'     '10' '1' ' ' ' ' ' ' ' ' ,
   ' '  l_tabnam   'Z2GIQ'  'G/I Qty(2Yr)'  '10' '1' ' ' ' ' ' ' ' ' ,
   ' '  l_tabnam   'MENGE'  'PR Qty'        '10' '1' ' ' ' ' ' ' ' ' ,

   ' '  l_tabnam   'PREIS'  'Price'      '08' '3' ' ' ' ' ' ' ' ' ,

   ' '  l_tabnam   'WAERS'  'CrCy'       '06' ' ' ' ' ' ' ' ' ' ' ,
   ' '  l_tabnam   'NETWR'  'Amount'     '08' '3' ' ' ' ' ' ' ' ' ,
   ' '  l_tabnam   'ZDATE'  'Req date'   '10' ' ' ' ' ' ' ' ' ' ' ,
   ' '  l_tabnam   'BADAT'  'Deliv.Date' '10' ' ' ' ' ' ' ' ' ' ' ,
   ' '  l_tabnam   'DISUB_PSPNR'  'WBS'  '05' ' ' ' ' ' ' ' ' ' ' ,
   ' '  l_tabnam   'BANFN'  'PR'         '10' ' ' ' ' ' ' ' ' ' ' ,
   ' '  l_tabnam   'BNFPO'  'Item'       '05' ' ' ' ' ' ' ' ' ' ' ,
   ' '  l_tabnam   'BSART'  'DocType'    '04' ' ' ' ' ' ' ' ' ' '  .

  DATA : l_tabix LIKE sy-tabix ,
         l_cnt(02) TYPE n ,
         l_field(20) .

  DO 24 TIMES .
    l_tabix = l_tabix + 1 .
    l_cnt   = l_cnt + 1 .
    READ TABLE gt_yymm INDEX l_tabix .
    CONCATENATE 'ZMONQ' l_cnt INTO l_field .
    PERFORM make_fieldcat_att USING:
     ' '  l_tabnam   l_field  gt_yymm-yymm    '06' '1' ' ' ' ' ' ' ' ' .

  ENDDO .

ENDFORM.                    " MAKE_FIELDCAT_RDB
*&---------------------------------------------------------------------*
*&      Form  CREATE_CONTAINER_OBJECT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SY_DYNNR  text
*----------------------------------------------------------------------*
FORM create_container_object  USING    p_sy_dynnr.
  CREATE OBJECT g_alv_doc_mdat
    EXPORTING
      repid     = sy-repid
      dynnr     = sy-dynnr
      side      = g_alv_doc_mdat->dock_at_left
      extension = 2700. "1500.

  CREATE OBJECT g_grid_mdat
    EXPORTING
      i_parent = g_alv_doc_mdat.

  CREATE OBJECT g_event_mdat.
  CREATE OBJECT g_event_item.

ENDFORM.                    " CREATE_CONTAINER_OBJECT
*&---------------------------------------------------------------------*
*&      Form  SET_ATTRIBUTES_ALV_GRID
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM set_attributes_alv_grid  USING    p_dynnr.

*celltab
  st_lay-stylefname = 'CELLTAB'.
*  st_lay-ctab_fname = 'ALV_COLOR'.
*columns
*  st_lay-edit = 'X'.
*  ST_LAY-CWIDTH_OPT = 'X'.
  st_lay-zebra      = 'X'.

*  st_lay-box_fname  = 'MARK'.
*      ST_LAY-SEL_MODE = 'A'.
*      st_lay-grid_title  = g_title.

*      ST_LAY-GRID_TITLE  = G_TITLE.

ENDFORM.                    " SET_ATTRIBUTES_ALV_GRID
*&---------------------------------------------------------------------*
*&      Form  BUILD_FIELD_CATALOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM build_field_catalog  USING    p_dynnr.

  DATA: l_tabname  TYPE tabname ,
        l_tabix    TYPE sytabix .

  l_tabname = 'ZHKPMS0030' .

  CLEAR: gt_field[].
  SET PARAMETER ID 'ALVBUFFER' FIELD sy-datum.
  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_bypassing_buffer = 'X'
      i_buffer_active    = 'X'
      i_structure_name   = l_tabname
    CHANGING
      ct_fieldcat        = gt_field.

  LOOP AT gt_field INTO st_field.
    l_tabix = sy-tabix .
    CASE st_field-fieldname.
      WHEN 'ICON'.
        st_field-reptext   = 'Status' .
        st_field-scrtext_m = 'Status' .
        st_field-coltext   = 'Status' .
        st_field-key       = 'X'.
        st_field-outputlen = 4 .
        MODIFY gt_field FROM st_field.
      WHEN 'WERKS'.
        st_field-reptext   = 'Plant' .
        st_field-scrtext_m = 'Plant'.
        st_field-coltext   = 'Plant'.
        st_field-key       = 'Plant'.
        st_field-edit      = c_x .
*        st_field-outputlen = 2 .
*        st_field-just      = c_c .
*        st_field-no_out    = c_x .

        MODIFY gt_field FROM st_field.

      WHEN 'MATNR'.
        st_field-reptext   = 'Material' .
        st_field-scrtext_m = 'Material' .
        st_field-coltext   = 'Material' .
        st_field-key       = 'X'.
        st_field-edit      = c_x .
        st_field-outputlen = 18 .
        MODIFY gt_field FROM st_field.

      WHEN 'MAKTX'.
        st_field-reptext   = 'Description' .
        st_field-scrtext_m = 'Description' .
        st_field-coltext   = 'Description' .
        st_field-key       = ' '.
*        st_field-edit      = c_x .
        st_field-outputlen = 20 .
        MODIFY gt_field FROM st_field.

      WHEN 'LGORT' .
        st_field-reptext   = 'Sloc' .
        st_field-scrtext_m = 'Sloc' .
        st_field-coltext   = 'Sloc' .
        st_field-key       = 'X'.
        st_field-edit      = c_x .
        MODIFY gt_field FROM st_field.

      WHEN 'MEINS'.
        st_field-reptext   = 'UOM' .
        st_field-scrtext_m = 'UOM' .
        st_field-coltext   = 'UOM' .
        st_field-key       = ' '.
        MODIFY gt_field FROM st_field.

      WHEN 'MENGE' .
        st_field-reptext   = 'PR Qty' .
        st_field-scrtext_m = 'PR Qty' .
        st_field-coltext   = 'PR Qty' .
        st_field-key       = ' '.
        st_field-edit      = c_x .
*        st_field-outputlen = 10 .
        MODIFY gt_field FROM st_field.
      WHEN 'PREIS'.
        st_field-reptext   = 'Price' .
        st_field-scrtext_m = 'Price' .
        st_field-coltext   = 'Price' .
        st_field-key       = ' '.
        st_field-edit      = c_x .
        MODIFY gt_field FROM st_field.
      WHEN 'WAERS'.
        st_field-reptext   = 'CrCy' .
        st_field-scrtext_m = 'CrCy' .
        st_field-coltext   = 'CrCy' .
        st_field-key       = ' '.
        st_field-edit      = ' '.
        MODIFY gt_field FROM st_field.
      WHEN 'NETWR'.
        st_field-reptext   = 'Amount' .
        st_field-scrtext_m = 'Amount' ..
        st_field-coltext   = 'Amount' .
        st_field-key       = ' '.
        st_field-edit      = ' ' .
        MODIFY gt_field FROM st_field.

      WHEN 'BADAT'.
        st_field-reptext   = 'GR Date' .
        st_field-scrtext_m = 'GR Date' .
        st_field-coltext   = 'GR Date' .
        st_field-key       = ' '.
        st_field-edit      = c_x .
        MODIFY gt_field FROM st_field.

*      WHEN 'DISUB_PSPNR'.
*        st_field-reptext   = 'WBS'.
*        st_field-scrtext_m = 'WBS'.
*        st_field-coltext   = 'WBS'.
*        st_field-key       = ' '.
*        st_field-edit      = c_x .
*        MODIFY gt_field FROM st_field.

**      WHEN 'BSART'.
**        st_field-reptext   = 'Doc. Type' .
**        st_field-scrtext_m = 'Doc. Type' .
**        st_field-coltext   = 'Doc. Type' .
**        st_field-key       = ' '.
**        st_field-edit      = c_x .
**        MODIFY gt_field FROM st_field.

      WHEN 'FICTR'.
        st_field-reptext   = 'Funds Ctr' .
        st_field-scrtext_m = 'Funds Ctr' .
        st_field-coltext   = 'Funds Ctr' .
        st_field-key       = ' '.
        st_field-edit      = c_x .
        MODIFY gt_field FROM st_field.

      WHEN 'FIPOS'.
        st_field-reptext   = 'Commit Item' .
        st_field-scrtext_m = 'Commit Item' .
        st_field-coltext   = 'Commit Item' .
        st_field-key       = ' '.
        st_field-edit      = c_x .
        MODIFY gt_field FROM st_field.

**      WHEN 'BANFN'.
**        st_field-reptext   = 'Purch.Req.NO' .
**        st_field-scrtext_m = 'Purch.Req.NO' .
**        st_field-coltext   = 'Purch.Req.NO' .
**        st_field-key       = ' '.
**        st_field-edit      = c_x .
**        MODIFY gt_field FROM st_field.

      WHEN 'MESG' .
        st_field-reptext   = 'Message' .
        st_field-scrtext_m = 'Message' .
        st_field-coltext   = 'Message' .
        st_field-key       = ' '.
        st_field-outputlen = 30 .
        MODIFY gt_field FROM st_field.
      WHEN OTHERS .
        DELETE gt_field .
    ENDCASE .
  ENDLOOP.


ENDFORM.                    " BUILD_FIELD_CATALOG
*&---------------------------------------------------------------------*
*&      Form  EXCLUDING_FUNCTIONS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM excluding_functions  USING pa_dynnr.
  DATA ls_exclude TYPE ui_func.


  CLEAR: gt_exclude, gt_exclude[].
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_insert_row.
  APPEND ls_exclude TO gt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_undo.
  APPEND ls_exclude TO gt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_copy_row.
  APPEND ls_exclude TO gt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_append_row.
  APPEND ls_exclude TO gt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_copy.
  APPEND ls_exclude TO gt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
  APPEND ls_exclude TO gt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_undo.
  APPEND ls_exclude TO gt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_paste.
  APPEND ls_exclude TO gt_exclude.

  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_cut.
  APPEND ls_exclude TO gt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_delete_row.
  APPEND ls_exclude TO gt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_insert_row.
  APPEND ls_exclude TO gt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_append_row.
  APPEND ls_exclude TO gt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_refresh.
  APPEND ls_exclude TO gt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_check.
  APPEND ls_exclude TO gt_exclude.
*  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_SORT.
*  APPEND LS_EXCLUDE TO GT_EXCLUDE.
*  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_SORT_ASC.
*  APPEND LS_EXCLUDE TO GT_EXCLUDE.
*  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_SORT_DSC.
*  APPEND LS_EXCLUDE TO GT_EXCLUDE.
**  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_SUBTOT.
**  APPEND LS_EXCLUDE TO GT_EXCLUDE.
**  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_SUM.
**  APPEND LS_EXCLUDE TO GT_EXCLUDE.
  ls_exclude = cl_gui_alv_grid=>mc_fc_info.
  APPEND ls_exclude TO gt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_graph.
  APPEND ls_exclude TO gt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_refresh.
  APPEND ls_exclude TO gt_exclude.
**  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FULL.
**  APPEND LS_EXCLUDE TO GT_EXCLUDE.
**  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_SOFT.
**  APPEND LS_EXCLUDE TO GT_EXCLUDE.

**  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_FILTER .
**  APPEND LS_EXCLUDE TO IT_EXCLUDE.
**  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_FIND .
**  APPEND LS_EXCLUDE TO IT_EXCLUDE.

**  IF PA_DYNNR <> '0400'.
**    LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_SUBTOT .
**    APPEND LS_EXCLUDE TO IT_EXCLUDE.
**    LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_SUM .
**    APPEND LS_EXCLUDE TO IT_EXCLUDE.
**  ENDIF.

**  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_MAXIMUM .
**  APPEND LS_EXCLUDE TO IT_EXCLUDE.
**  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_MINIMUM .
**  APPEND LS_EXCLUDE TO IT_EXCLUDE.
**  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_AVERAGE .
**  APPEND LS_EXCLUDE TO IT_EXCLUDE.

ENDFORM.                    " EXCLUDING_FUNCTIONS
*&---------------------------------------------------------------------*
*&      Form  ASSIGN_ITAB_TO_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SY_DYNNR  text
*----------------------------------------------------------------------*
FORM assign_itab_to_alv  USING    p_dynnr.

  CALL METHOD g_grid_mdat->set_table_for_first_display
    EXPORTING      "
      it_toolbar_excluding = gt_exclude
      is_layout            = st_lay
      i_save               = 'x'
    CHANGING
      it_fieldcatalog      = gt_field[]
      it_outtab            = gt_item[].

* Set editable cells to ready for input initially
  CALL METHOD g_grid_mdat->set_ready_for_input
    EXPORTING
      i_ready_for_input = 1.

ENDFORM.                    " ASSIGN_ITAB_TO_ALV
*&---------------------------------------------------------------------*
*&      Form  ASSIGN_EVENT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM assign_event  USING    p_dynnr.

  SET HANDLER g_event_item->hotspot_click_item   FOR g_grid_mdat.
*  SET HANDLER G_EVENT_ITEM->DOUBLE_CLICK         FOR G_GRID_MDAT.
  SET HANDLER g_event_item->toolbar_item         FOR g_grid_mdat.
  SET HANDLER g_event_item->user_command_item    FOR g_grid_mdat .
*  SET HANDLER G_EVENT_ITEM->DATA_CHANGED         FOR G_GRID_MDAT .

  CALL METHOD g_grid_mdat->set_toolbar_interactive.

  CALL METHOD g_grid_mdat->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_modified.

ENDFORM.                    " ASSIGN_EVENT
*&---------------------------------------------------------------------*
*&      Form  REFRESH_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM refresh_data  USING p_dynnr.

  DATA: ls_stable TYPE lvc_s_stbl.

  ls_stable-row = 'x'.
  ls_stable-col = 'x'.

  CALL METHOD g_grid_mdat->refresh_table_display
    EXPORTING
      is_stable = ls_stable.

ENDFORM.                    " REFRESH_DATA
*&---------------------------------------------------------------------*
*&      Form  PROCESS_SAVE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM process_save .

  DATA : l_answer .
  DATA : l_valid ,
         l_error .
  DATA : l_check_error .


  CALL METHOD g_grid_mdat->check_changed_data
    IMPORTING
      e_valid = l_valid.

  REFRESH g_rows_t.
  CALL METHOD g_grid_mdat->get_selected_rows
    IMPORTING
      et_index_rows = g_rows_t.
  IF g_rows_t[] IS INITIAL.
    MESSAGE s000 WITH 'not selection line.'.
    EXIT.
  ENDIF.

*.
  CLEAR: v_return, v_return[].

  CLEAR : gs_prheader, gs_prheaderx,
          gt_return,   gt_return[],
          gt_pritem,   gt_pritem[],
          gt_pritemx,  gt_pritemx[],
          gt_pritemexp, gt_pritemexp[],
          gt_pracct,   gt_pracct[],
          gt_pracctx,  gt_pracctx[].

  CLEAR : it_exten, it_exten[].

  CLEAR : l_error .


  LOOP AT g_rows_t INTO g_rows_s.

    READ TABLE gt_item INDEX g_rows_s-index.
    IF sy-subrc = 0 .

      IF gt_item-matnr IS INITIAL .
        CONTINUE .
      ENDIF .

      PERFORM check_data USING l_error .

      IF l_error IS NOT INITIAL .
        MODIFY gt_item INDEX g_rows_s-index.
        CONTINUE .
      ENDIF .

      PERFORM get_others_data .

      MODIFY gt_item INDEX g_rows_s-index.
    ENDIF .

  ENDLOOP .
*.
  CHECK l_error IS INITIAL .

  CLEAR : g_enter , zhkpms0030 .
  CALL SCREEN 0200 STARTING AT 30 08 .

  CHECK g_enter = c_x .

  DATA : l_item(4) TYPE n .

  LOOP AT g_rows_t INTO g_rows_s.

    READ TABLE gt_item INDEX g_rows_s-index.
    IF sy-subrc = 0 .

      IF gt_item-matnr IS INITIAL .
        CONTINUE .
      ENDIF .

      l_item = l_item + 1 .
      PERFORM get_bapi_pr USING l_item .

    ENDIF .

  ENDLOOP .

*.
  CHECK gt_pritem[] IS NOT INITIAL .

  PERFORM call_bapi_pr .


ENDFORM.                    " PROCESS_SAVE
*&---------------------------------------------------------------------*
*&      Form  CHECK_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM check_data  USING    p_error.
  DATA : ls_t001w  LIKE t001w ,
         ls_t001l  LIKE t001l ,
         ls_t161   LIKE t161  .

  DATA : l_message(72) .
*  '@08@'."  Green light; positive
*  '@09@'."  Yellow light; neutral
*  '@0A@'."  Red light; negative

*.
  IF gt_item-icon = '@08@' .
    CONCATENATE 'Purchase requisition number' gt_item-mesg 'created'
           INTO l_message SEPARATED BY space .
    MESSAGE s000 WITH l_message .
    p_error = c_x .
    EXIT .
  ENDIF .
*.

  CLEAR : gt_item-icon , gt_item-mesg .

  SELECT SINGLE mandt INTO sy-mandt
    FROM mara
   WHERE matnr EQ gt_item-matnr .
  IF sy-subrc <> 0 .
    gt_item-icon = '@0A@'."  Red light; negative
    gt_item-mesg = 'Please. Check out Material.' .
    p_error = c_x .
    EXIT .
  ENDIF .

  SELECT SINGLE * INTO ls_t001w
    FROM t001w
   WHERE werks EQ gt_item-werks .
  IF sy-subrc <> 0 .
    gt_item-icon = '@0A@'."  Red light; negative
    gt_item-mesg = 'Please. Check out Plant.' .
    p_error = c_x .
    EXIT .
  ENDIF .

  SELECT SINGLE * INTO ls_t001l
    FROM t001l
   WHERE werks EQ gt_item-werks
     AND lgort EQ gt_item-lgort .
  IF sy-subrc <> 0 .
    gt_item-icon = '@0A@'."  Red light; negative
    gt_item-mesg = 'Please. Check out Storage Location.' .
    p_error = c_x .
    EXIT .
  ENDIF .

  IF gt_item-menge IS INITIAL .
    gt_item-icon = '@0A@'."  Red light; negative
    gt_item-mesg = 'Please. Check out Repl Qty.' .
    p_error = c_x .
    EXIT .
  ENDIF .

  IF gt_item-preis IS INITIAL .
    gt_item-icon = '@0A@'."  Red light; negative
    gt_item-mesg = 'Please. Check out Price.' .
    p_error = c_x .
    EXIT .
  ENDIF .

  IF gt_item-badat IS INITIAL .
    gt_item-icon = '@0A@'."  Red light; negative
    gt_item-mesg = 'Please. Check out GR Date.' .
    p_error = c_x .
    EXIT .
  ENDIF .

*  IF gt_item-disub_pspnr IS INITIAL .
*    gt_item-icon = '@0A@'."  Red light; negative
*    gt_item-mesg = 'Please. Check out WBS.' .
*    p_error = c_x .
*    EXIT .
*  ENDIF .

**  SELECT SINGLE * INTO ls_t161
**    FROM t161
**   WHERE bsart EQ gt_item-bsart .
**  IF sy-subrc <> 0 .
**    gt_item-icon = '@0A@'."  Red light; negative
**    gt_item-mesg = 'Please. Check out Doc. Type.' .
**    p_error = c_x .
**    EXIT .
**  ENDIF .
**
**  IF ls_t161-numke IS NOT INITIAL AND
**     gt_item-banfn IS INITIAL .
**    gt_item-icon = '@0A@'."  Red light; negative
**    gt_item-mesg = 'Enter Purchase Requisition number' .
**    p_error = c_x .
**    EXIT .
**  ENDIF .
**
**  IF ls_t161-numke IS INITIAL .
**    CLEAR gt_item-banfn .
**  ENDIF .

  IF gt_item-fictr IS NOT INITIAL .
    SELECT SINGLE mandt INTO sy-mandt
      FROM fmfctr
     WHERE fictr EQ gt_item-fictr .
    IF sy-subrc <> 0 .
      gt_item-icon = '@0A@'."  Red light; negative
      gt_item-mesg = 'Please. Check out Funds Ctr.' .
      p_error = c_x .
      EXIT .
    ENDIF .
  ENDIF .

  IF gt_item-fipos IS NOT INITIAL .
    SELECT SINGLE mandt INTO sy-mandt
      FROM fmfxpo
     WHERE fipos EQ gt_item-fipos .
    IF sy-subrc <> 0 .
      gt_item-icon = '@0A@'."  Red light; negative
      gt_item-mesg = 'Please. Check out Commit Item.' .
      p_error = c_x .
      EXIT .
    ENDIF .
  ENDIF .

  PERFORM get_others_data .

ENDFORM.                    " CHECK_DATA
*&---------------------------------------------------------------------*
*&      Form  GET_OTHERS_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_others_data .
  DATA : ls_t001k LIKE t001k ,
         ls_t001  LIKE t001 .

  SELECT SINGLE maktx INTO gt_item-maktx
    FROM makt
   WHERE matnr EQ gt_item-matnr
     AND spras EQ sy-langu .

  SELECT SINGLE meins INTO gt_item-meins
    FROM mara
   WHERE matnr EQ gt_item-matnr .

  SELECT SINGLE * INTO ls_t001k
    FROM t001k
   WHERE bwkey EQ gt_item-werks .
  IF sy-subrc = 0 .
    SELECT SINGLE waers INTO gt_item-waers
      FROM t001
     WHERE bukrs EQ ls_t001k-bukrs .
    gt_item-bukrs = ls_t001k-bukrs .
  ENDIF .

  gt_item-netwr = gt_item-menge * gt_item-preis .

ENDFORM.                    " GET_OTHERS_DATA
*&---------------------------------------------------------------------*
*&      Form  CALL_BAPI_PR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM call_bapi_pr .


  CLEAR g_banfn .
  CALL FUNCTION 'BAPI_PR_CREATE'
    EXPORTING
      prheader    = gs_prheader
      prheaderx   = gs_prheaderx
    IMPORTING
      number      = g_banfn
    TABLES
      return      = gt_return
      pritem      = gt_pritem
      pritemx     = gt_pritemx
      praccount   = gt_pracct
      praccountx  = gt_pracctx
      extensionin = it_exten.

*  '@08@'."  Green light; positive
*  '@09@'."  Yellow light; neutral
*  '@0A@'."  Red light; negative

  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
    EXPORTING
      wait = 'X'.

  DATA : l_save .
  SELECT SINGLE mandt INTO sy-mandt
    FROM eban
   WHERE banfn EQ g_banfn .
  IF sy-subrc = 0 .
    l_save = 'X' .
  ENDIF .

*
  DATA : l_item(4) TYPE n .

  LOOP AT g_rows_t INTO g_rows_s.

    READ TABLE gt_item INDEX g_rows_s-index.
    IF sy-subrc = 0 .

      IF gt_item-matnr IS INITIAL .
        CONTINUE .
      ENDIF .

      IF  l_save = 'X' .
        gt_item-icon = '@08@' .
        gt_item-mesg = g_banfn .

        l_item = l_item + 1 .
        PERFORM save_cbo_table USING l_item .
      ELSE.
        READ TABLE gt_return WITH KEY type = 'E'.
        IF sy-subrc = 0.
          gt_item-icon = '@0A@' .
          gt_item-mesg = gt_return-message.
        ENDIF.
      ENDIF.

      MODIFY gt_item INDEX g_rows_s-index.
    ENDIF .
  ENDLOOP .

ENDFORM.                    " CALL_BAPI_PR
*&---------------------------------------------------------------------*
*&      Form  SAVE_CBO_TABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM save_cbo_table USING p_item .
  DATA : ls_pmt0008 LIKE zhkpmt0008 .

  READ TABLE gt_data WITH KEY werks = gt_item-werks
                              lgort = gt_item-lgort
                              matnr = gt_item-matnr .
  IF sy-subrc = 0 .
    MOVE-CORRESPONDING gt_data TO ls_pmt0008 .
  ENDIF .

  MOVE-CORRESPONDING gt_item TO ls_pmt0008 .

  ls_pmt0008-banfn = g_banfn .
  ls_pmt0008-bnfpo = p_item .
  ls_pmt0008-bsart = zhkpms0030-bsart.
  ls_pmt0008-zdate = sy-datum .
  ls_pmt0008-erdat = sy-datum .
  ls_pmt0008-erzet = sy-uzeit .
  ls_pmt0008-ernam = sy-uname .
  ls_pmt0008-aedat = sy-datum .
  ls_pmt0008-aezet = sy-uzeit .
  ls_pmt0008-aenam = sy-uname .

  MODIFY zhkpmt0008 FROM ls_pmt0008 .


ENDFORM.                    " SAVE_CBO_TABLE
*&---------------------------------------------------------------------*
*&      Form  CALL_SCREEN_0100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM call_screen_0100 .

  READ TABLE gt_data WITH KEY mark = c_x .
  IF sy-subrc <> 0 .
    MESSAGE s000 WITH 'not selection line.'.
    EXIT.
  ENDIF .

  CLEAR : gt_item, gt_item[] .

  LOOP AT gt_data WHERE mark = c_x .
    CLEAR gt_item .
    MOVE-CORRESPONDING gt_data TO gt_item .
    PERFORM get_others_data .
    PERFORM set_field .
    APPEND gt_item .
  ENDLOOP .

  DO 30 TIMES .
    CLEAR : gt_item .
    gt_item-new = c_x .
    PERFORM set_field .
    APPEND gt_item .
  ENDDO .

  CALL SCREEN 0100. " STARTING AT 5 5  .

ENDFORM.                    " CALL_SCREEN_0100
*&---------------------------------------------------------------------*
*&      Form  GET_OTHERS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_others .

  DATA : l_valid .

  CALL METHOD g_grid_mdat->check_changed_data
    IMPORTING
      e_valid = l_valid.

  LOOP AT gt_item .
    PERFORM get_others_data .
    MODIFY gt_item .
  ENDLOOP .

ENDFORM.                    " GET_OTHERS
*&---------------------------------------------------------------------*
*&      Form  SET_FIELD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_field .

  CLEAR : gt_celltab[] .

  IF gt_item-new IS INITIAL .
    PERFORM set_field_enabled USING : 'MENGE' ,
                                      'PREIS' ,
                                      'BADAT' ,
                                      'DISUB_PSPNR' ,
                                      'BSART' ,
                                      'FICTR' ,
                                      'FIPOS' .

    PERFORM set_field_disabled USING :  'MATNR' ,
                                        'LGORT' ,
                                        'WERKS' .
  ELSE .
    PERFORM set_field_enabled USING : 'MENGE' ,
                                      'PREIS' ,
                                      'BADAT' ,
                                      'DISUB_PSPNR' ,
                                      'BSART' ,
                                      'MATNR' ,
                                      'LGORT' ,
                                      'WERKS' ,
                                      'FICTR' ,
                                      'FIPOS' .


  ENDIF .

  gt_item-celltab[] =  gt_celltab[] .

ENDFORM.                    " SET_FIELD
*&---------------------------------------------------------------------*
*&      Form  SET_FIELD_ENABLED
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM set_field_enabled  USING p_field.

  CLEAR st_celltab.
  st_celltab-fieldname =  p_field.
  st_celltab-style = cl_gui_alv_grid=>mc_style_enabled.
  INSERT st_celltab INTO TABLE gt_celltab.

ENDFORM.                    " SET_FIELD_ENABLED
*&---------------------------------------------------------------------*
*&      Form  SET_FIELD_DISABLED
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM set_field_disabled  USING p_field.

  CLEAR st_celltab.
  st_celltab-fieldname =  p_field.
  st_celltab-style = cl_gui_alv_grid=>mc_style_disabled.
  INSERT st_celltab INTO TABLE gt_celltab.

ENDFORM.                    " SET_FIELD_DISABLED
*&---------------------------------------------------------------------*
*&      Form  EVENT_TOOLBAR_ITEM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_OBJECT  text
*      -->P_E_INTERACTIVE  text
*----------------------------------------------------------------------*
FORM event_toolbar_item  USING p_e_object TYPE REF TO cl_alv_event_toolbar_set
                          p_e_interactive TYPE char1.

  DATA: ls_toolbar TYPE stb_button.


  CLEAR ls_toolbar.
  ls_toolbar-butn_type = '3'.
  APPEND ls_toolbar TO p_e_object->mt_toolbar.
*
  CLEAR ls_toolbar.
  ls_toolbar-function  = 'L_INSERT'.
  ls_toolbar-icon      = icon_insert_row.
  ls_toolbar-disabled  = space.
*  LS_TOOLBAR-TEXT      = ''.
*  LS_TOOLBAR-QUICKINFO = ' '.
*  LS_TOOLBAR-CHECKED   = SPACE.
  APPEND ls_toolbar TO p_e_object->mt_toolbar.


  CLEAR ls_toolbar.
  ls_toolbar-butn_type = '3'.
  APPEND ls_toolbar TO p_e_object->mt_toolbar.
*
  CLEAR ls_toolbar.
  ls_toolbar-function  = 'L_DELETE'.
  ls_toolbar-icon      = icon_delete_row .
  ls_toolbar-disabled  = space .
*  LS_TOOLBAR-TEXT      = ' ' .
*  LS_TOOLBAR-QUICKINFO = ' '.
*  LS_TOOLBAR-CHECKED   = SPACE.
  APPEND ls_toolbar TO p_e_object->mt_toolbar.

ENDFORM.                    " EVENT_TOOLBAR_ITEM
*&---------------------------------------------------------------------*
*&      Form  EVENT_UCOMM_ITEM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_UCOMM  text
*----------------------------------------------------------------------*
FORM event_ucomm_item  USING p_e_ucomm.
*
  CASE p_e_ucomm.
    WHEN 'L_INSERT'.
      PERFORM  line_insert .
    WHEN 'L_DELETE' .
      PERFORM  line_delete .
    WHEN OTHERS.
  ENDCASE.

ENDFORM.                    " EVENT_UCOMM_ITEM
*&---------------------------------------------------------------------*
*&      Form  LINE_INSERT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM line_insert .

  DATA : lt_item LIKE gt_item OCCURS 0 WITH HEADER LINE .

  REFRESH g_rows_t.
  CALL METHOD g_grid_mdat->get_selected_rows
    IMPORTING
      et_index_rows = g_rows_t.
  IF g_rows_t[] IS INITIAL.
    MESSAGE s000 WITH 'not selection line.'.
    EXIT.
  ENDIF.

  LOOP AT gt_item .

    READ TABLE g_rows_t INTO g_rows_s WITH KEY INDEX = sy-tabix .

    IF sy-subrc = 0 .
      CLEAR lt_item .
      lt_item-new = c_x .

      APPEND lt_item .
    ENDIF .

    APPEND gt_item TO lt_item .

  ENDLOOP .

  gt_item[] = lt_item[] .

  LOOP AT gt_item .
    PERFORM set_field .
    MODIFY gt_item .
  ENDLOOP .

ENDFORM.                    " LINE_INSERT
*&---------------------------------------------------------------------*
*&      Form  LINE_DELETE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM line_delete .

  DATA : lt_item LIKE gt_item OCCURS 0 WITH HEADER LINE .

  REFRESH g_rows_t.
  CALL METHOD g_grid_mdat->get_selected_rows
    IMPORTING
      et_index_rows = g_rows_t.
  IF g_rows_t[] IS INITIAL.
    MESSAGE s000 WITH 'not selection line.'.
    EXIT.
  ENDIF.

  LOOP AT gt_item .

    READ TABLE g_rows_t INTO g_rows_s WITH KEY INDEX = sy-tabix .

    IF sy-subrc <> 0 .
      APPEND gt_item TO lt_item .
    ENDIF .

  ENDLOOP .

  gt_item[] = lt_item[] .

ENDFORM.                    " LINE_DELETE
*&---------------------------------------------------------------------*
*&      Form  GET_BAPI_PR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_bapi_pr USING p_item .

  MOVE :  zhkpms0030-bsart TO gs_prheader-pr_type ,
          'X'              TO gs_prheaderx-pr_type .

  IF zhkpms0030-banfn IS NOT INITIAL .
    MOVE : zhkpms0030-banfn TO gs_prheader-preq_no ,
           'X'              TO gs_prheaderx-preq_no .
  ENDIF .

  MOVE : p_item        TO gt_pritem-preq_item,
         ' '           TO gt_pritem-acctasscat,
         gt_item-matnr TO gt_pritem-material,
         gt_item-menge TO gt_pritem-quantity,
         gt_item-meins TO gt_pritem-unit,

         sy-datum TO gt_pritem-preq_date,

         gt_item-badat TO gt_pritem-deliv_date,
*           g_alv2_s-ekgrp TO gt_pritem-pur_group,
         sy-uname       TO gt_pritem-preq_name,
         gt_item-werks  TO gt_pritem-plant,
         gt_item-preis  TO gt_pritem-preq_price,
         gt_item-waers  TO gt_pritem-currency,
         '1'            TO gt_pritem-price_unit,
         '2'            TO gt_pritem-po_price ,
         gt_item-lgort  TO gt_pritem-store_loc.


  IF gt_item-fictr IS NOT INITIAL .
    MOVE : gt_item-fictr TO gt_pritem-funds_ctr,
           gt_item-fipos TO gt_pritem-cmmt_item.
  ENDIF .

  APPEND gt_pritem.  CLEAR gt_pritem.

  MOVE : 'X' TO gs_prheaderx-pr_type.

  MOVE : p_item TO gt_pritemx-preq_item,
         'X' TO gt_pritemx-preq_itemx,
         'X' TO gt_pritemx-acctasscat,
*           '' TO gt_pritemx-acctasscat,
         'X' TO gt_pritemx-material,
         'X' TO gt_pritemx-quantity,
         'X' TO gt_pritemx-unit,
         'X' TO gt_pritemx-deliv_date,
*           'X' TO gt_pritemx-pur_group,
         'X' TO gt_pritemx-preq_name,
         'X' TO gt_pritemx-plant,
         'X' TO gt_pritemx-preq_price,
*           '' TO gt_pritemx-preq_price,
         'X' TO gt_pritemx-currency,
         'X' TO gt_pritemx-price_unit,
         'X' TO gt_pritemx-po_price  ,
         'X' TO gt_pritemx-store_loc.

  IF gt_item-fictr IS NOT INITIAL .
    MOVE : 'X' TO gt_pritemx-funds_ctr,
           'X' TO gt_pritemx-cmmt_item.
  ENDIF .

  APPEND gt_pritemx.  CLEAR gt_pritemx.

*  MOVE : '1'      TO gt_pracct-preq_item,
*         '1'      TO gt_pracctx-preq_item,
*
*         'X' TO gt_pracctx-preq_itemx,
*
*         '01' TO gt_pracct-serial_no,
*         '01' TO gt_pracctx-serial_no.
*           'X'  TO gt_pracctx-serial_nox.
*           g_alv2_s-zreqty TO gt_pracct-quantity,
*           'X' TO gt_pracctx-quantity,
*           g_alv2_s-verpr TO gt_pracct-net_value,
*           'X' TO gt_pracctx-net_value.


*  APPEND gt_pracct.  CLEAR gt_pracct.
*  APPEND gt_pracctx.  CLEAR gt_pracctx.

ENDFORM.                    " GET_BAPI_PR
*&---------------------------------------------------------------------*
*&      Form  CHECK_DATA_0200
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_data_0200 .

  DATA : ls_t161 LIKE t161 .

  CLEAR : g_err .
  SELECT SINGLE * INTO ls_t161
    FROM t161
   WHERE bsart EQ zhkpms0030-bsart .
  IF sy-subrc <> 0 .
    MESSAGE s001 WITH 'Please. Check out Doc. Type.' .
    g_err = c_x .
    EXIT .
  ENDIF .

  IF ls_t161-numke IS NOT INITIAL AND
     zhkpms0030-banfn IS INITIAL .
    gt_item-icon = '@0A@'."  Red light; negative
    MESSAGE s001 WITH 'Enter Purchase Requisition number' .
    g_err = c_x .
    EXIT .
  ELSE .
    IF ls_t161-numke IS INITIAL .
      CLEAR zhkpms0030-banfn .
    ENDIF .
  ENDIF .


  IF g_err IS INITIAL .
    g_enter = c_x .
  ENDIF .

ENDFORM.                    " CHECK_DATA_02
