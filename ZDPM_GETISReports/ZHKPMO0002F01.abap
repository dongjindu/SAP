*&---------------------------------------------------------------------*
*&  Include           ZHKPMO0002F01
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

  s_budat = 'IEQ' .
  s_budat-low = sy-datum .
  s_budat-high = '00000000' .
  APPEND s_budat .

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

  DATA : lt_ekpo LIKE TABLE OF ekpo WITH HEADER LINE ,
         lt_ekko LIKE TABLE OF ekko WITH HEADER LINE ,
         lt_ekkn LIKE TABLE OF ekkn WITH HEADER LINE ,
         lt_mseg LIKE TABLE OF zvpm_mksg WITH HEADER LINE ,
         lt_dele LIKE TABLE OF mseg WITH HEADER LINE .

  CLEAR : gt_data[] .

*.. Get Purchasing Document Item
  SELECT * INTO TABLE lt_ekpo
    FROM ekpo
   WHERE ebeln IN s_ebeln
     AND werks IN s_werks
     AND knttp EQ c_a .

  CHECK lt_ekpo[] IS NOT INITIAL .

  SELECT * INTO TABLE lt_ekko
    FROM ekko
    FOR ALL ENTRIES IN lt_ekpo
   WHERE ebeln EQ lt_ekpo-ebeln .

  SELECT * INTO TABLE lt_ekkn
    FROM ekkn
    FOR ALL ENTRIES IN lt_ekpo
   WHERE ebeln EQ lt_ekpo-ebeln
     AND ebelp EQ lt_ekpo-ebelp .

*.. Get MKPF + MSEG
  SELECT * INTO TABLE lt_mseg
    FROM zvpm_mksg
    FOR ALL ENTRIES IN lt_ekpo
   WHERE ebeln EQ lt_ekpo-ebeln
     AND ebelp EQ lt_ekpo-ebelp
     AND budat IN s_budat .

  IF lt_mseg[] IS NOT INITIAL .
    SELECT mjahr mblnr zeile  sjahr smbln smblp
      INTO CORRESPONDING FIELDS OF TABLE lt_dele
      FROM mseg
      FOR ALL ENTRIES IN lt_mseg
     WHERE sjahr EQ lt_mseg-mjahr
       AND smbln EQ lt_mseg-mblnr
       AND smblp EQ lt_mseg-zeile .
  ENDIF .

*.. Get Delete data
  SORT lt_dele BY sjahr smbln smblp  .

  LOOP AT lt_mseg .
    READ TABLE lt_dele WITH KEY sjahr = lt_mseg-mjahr
                                smbln = lt_mseg-mblnr
                                smblp = lt_mseg-zeile
                                BINARY SEARCH .
    IF sy-subrc = 0 .
      DELETE lt_mseg .
    ENDIF .
  ENDLOOP .

  SORT lt_dele BY mjahr mblnr zeile .
  LOOP AT lt_mseg .
    READ TABLE lt_dele WITH KEY mjahr = lt_mseg-mjahr
                                mblnr = lt_mseg-mblnr
                                zeile = lt_mseg-zeile
                                BINARY SEARCH .
    IF sy-subrc = 0 .
      DELETE lt_mseg .
    ENDIF .
  ENDLOOP .

*..
  SORT lt_mseg BY ebeln ebelp .
  SORT lt_ekkn BY ebeln ebelp .
  SORT lt_ekko BY ebeln .

  LOOP AT lt_ekpo  .

    READ TABLE lt_mseg WITH KEY ebeln = lt_ekpo-ebeln
                                ebelp = lt_ekpo-ebelp
                                BINARY SEARCH .
    IF sy-subrc <> 0 .
      CONTINUE .
    ENDIF .

    CLEAR : lt_ekko , lt_ekkn .
    READ TABLE lt_ekko WITH KEY ebeln = lt_ekpo-ebeln
                                BINARY SEARCH .
    READ TABLE lt_ekkn WITH KEY ebeln = lt_ekpo-ebeln
                                ebelp = lt_ekpo-ebelp
                                BINARY SEARCH .


    CLEAR : gt_data .

    gt_data-mblnr = lt_mseg-mblnr .
    gt_data-mjahr = lt_mseg-mjahr .
    gt_data-zeile = lt_mseg-zeile .
    gt_data-ebeln = lt_mseg-ebeln .
    gt_data-ebelp = lt_mseg-ebelp .
    gt_data-bukrs = lt_ekpo-bukrs .
    gt_data-lifnr = lt_ekko-lifnr .
*  gt_data-BUDAT =
    gt_data-bedat = lt_mseg-budat .
    gt_data-knttp = lt_ekpo-knttp .
    gt_data-matnr = lt_ekpo-matnr .
    gt_data-ematn = lt_ekpo-ematn .
    gt_data-werks = lt_ekpo-werks .
    gt_data-lgort = lt_ekpo-lgort .
    gt_data-anln1 = lt_ekkn-anln1 .
    gt_data-anln2 = lt_ekkn-anln2 .
    gt_data-aufnr = lt_ekkn-aufnr .

    APPEND gt_data .
  ENDLOOP .

ENDFORM.                    " GET_DATA
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

  MOVE : 'MARK'    TO gs_layout-box_fieldname .

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
   'X'  l_tabnam   'ICON'   'Status'      '04' ' ' ' ' ' ' ' ' 'C' ,
   'X'  l_tabnam   'MBLNR'  'Mat. Doc'    '10' ' ' ' ' ' ' ' ' ' ' ,
   'X'  l_tabnam   'MJAHR'  'MatYr'       '04' ' ' ' ' ' ' ' ' ' ' ,

   'X'  l_tabnam   'ZEILE'  'Item'        '04' ' ' ' ' ' ' ' ' ' ' ,
   ' '  l_tabnam   'EBELN'  'Purch.Doc.'  '10' ' ' ' ' ' ' ' ' ' ' ,
   ' '  l_tabnam   'EBELP'  'Item'        '04' ' ' ' ' ' ' ' ' ' ' ,
   ' '  l_tabnam   'BUKRS'  'CoCd'        '04' ' ' ' ' ' ' ' ' ' ' ,

   ' '  l_tabnam   'LIFNR'  'Vendor'      '10' ' ' ' ' ' ' ' ' ' ' ,
   ' '  l_tabnam   'BEDAT'  'Pstng Date'  '10' ' ' ' ' ' ' ' ' ' ' ,
   ' '  l_tabnam   'KNTTP'  'A'           '01' ' ' ' ' ' ' ' ' ' ' ,

   ' '  l_tabnam   'MATNR'  'Material'    '18' ' ' ' ' ' ' ' ' ' ' ,
   ' '  l_tabnam   'EMATN'  'MPN Materia' '18' ' ' ' ' ' ' ' ' ' ' ,
   ' '  l_tabnam   'WERKS'  'Plnt'        '04' ' ' ' ' ' ' ' ' ' ' ,
   ' '  l_tabnam   'LGORT'  'SLoc'        '04' ' ' ' ' ' ' ' ' ' ' ,

   ' '  l_tabnam   'ANLN1'  'Asset'       '12' ' ' ' ' ' ' ' ' ' ' ,
   ' '  l_tabnam   'ANLN2'  'SNo.'        '04' ' ' ' ' ' ' ' ' ' ' ,

   ' '  l_tabnam   'AUFNR'  'Order'       '12' ' ' ' ' ' ' ' ' ' '  .


ENDFORM.                    " MAKE_FIELDCAT
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

  IF p_fieldname = 'MBLNR' .
    ls_fieldcat-ref_fieldname  = 'MBLNR' .
    ls_fieldcat-ref_tabname    = 'MSEG' .
  ENDIF .

  IF p_fieldname = 'LIFNR' .
    ls_fieldcat-ref_fieldname  = 'LIFNR' .
    ls_fieldcat-ref_tabname    = 'EKKO' .
  ENDIF .

  IF p_fieldname = 'AUFNR' .
    ls_fieldcat-ref_fieldname  = 'AUFNR' .
    ls_fieldcat-ref_tabname    = 'EKKN' .
  ENDIF .

  IF p_fieldname = 'ANLN1' .
    ls_fieldcat-ref_fieldname  = 'ANLN1' .
    ls_fieldcat-ref_tabname    = 'EKKN' .
  ENDIF .

  APPEND ls_fieldcat TO gt_fieldcat.


ENDFORM.                    " MAKE_FIELDCAT_ATT
*&---------------------------------------------------------------------*
*&      Form  CALL_SCREEN_0100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM call_screen_0100 .

  DATA : ls_t0009 LIKE zhkpmt0009 .

  READ TABLE gt_data WITH KEY mark = c_x .
  IF sy-subrc <> 0 .
    MESSAGE s000 WITH 'not selection line.'.
    EXIT.
  ENDIF .


  LOOP AT gt_data WHERE mark = c_x .

    CLEAR zhkpms0001 .
    SELECT SINGLE * INTO ls_t0009
      FROM zhkpmt0009
     WHERE mblnr EQ gt_data-mblnr
       AND mjahr EQ gt_data-mjahr
       AND zeile EQ gt_data-zeile .
    IF sy-subrc = 0 .
      MOVE-CORRESPONDING ls_t0009 TO zhkpms0001 .
    ENDIF .

    CLEAR : gt_item[] .
    SELECT equnr
      INTO CORRESPONDING FIELDS OF TABLE gt_item
      FROM zhkpmt0009
     WHERE mblnr EQ gt_data-mblnr
       AND mjahr EQ gt_data-mjahr
       AND zeile EQ gt_data-zeile .

    DO 300 TIMES .
      CLEAR gt_item .
      APPEND gt_item .
    ENDDO .

    CALL SCREEN 0100 STARTING AT 40 08.

    MODIFY gt_data .
  ENDLOOP .

ENDFORM.                    " CALL_SCREEN_0100
*&---------------------------------------------------------------------*
*&      Form  SAVE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM save_data .
  DATA : lt_t0009 LIKE TABLE OF zhkpmt0009 WITH HEADER LINE .
  DATA : lt_item  LIKE gt_item OCCURS 0 WITH HEADER LINE .
  DATA : l_error .

  CLEAR : lt_t0009 .
  MOVE-CORRESPONDING gt_data TO lt_t0009 .
  lt_t0009-BUDAT   = sy-datum .
  lt_t0009-idate   = zhkpms0001-idate  .
  lt_t0009-itype   = zhkpms0001-itype  .
  lt_t0009-ipart1  = zhkpms0001-ipart1 .
  lt_t0009-ipart2  = zhkpms0001-ipart2 .
  lt_t0009-ipart3  = zhkpms0001-ipart3 .
  lt_t0009-irslt1  = zhkpms0001-irslt1 .

  lt_item[] = gt_item[] .

  DELETE lt_item WHERE equnr = space .

* Check out : Equipment
  LOOP AT lt_item .
    SELECT SINGLE mandt INTO sy-mandt
      FROM equi
     WHERE equnr EQ lt_item-equnr .
    IF sy-subrc <> 0 .
      MESSAGE s000 WITH 'Check out : Equipment ' .
      l_error = c_x . EXIT .
    ENDIF .
  ENDLOOP .

  CHECK l_error IS INITIAL .

  IF lt_item[] IS INITIAL .
    IF lt_t0009-ernam IS INITIAL .
      lt_t0009-erdat = sy-datum .
      lt_t0009-erzet = sy-uzeit .
      lt_t0009-ernam = sy-uname .
    ENDIF .
    lt_t0009-aedat = sy-datum .
    lt_t0009-aezet = sy-uzeit .
    lt_t0009-aenam = sy-uname .
    APPEND lt_t0009 .
  ELSE .
    LOOP AT lt_item .
      lt_t0009-equnr = lt_item-equnr .
      IF lt_t0009-ernam IS INITIAL .
        lt_t0009-erdat = sy-datum .
        lt_t0009-erzet = sy-uzeit .
        lt_t0009-ernam = sy-uname .
      ENDIF .
      lt_t0009-aedat = sy-datum .
      lt_t0009-aezet = sy-uzeit .
      lt_t0009-aenam = sy-uname .
      APPEND lt_t0009 .
    ENDLOOP .
  ENDIF .

  DELETE FROM zhkpmt0009 WHERE mblnr EQ gt_data-mblnr
                           AND mjahr EQ gt_data-mjahr
                           AND zeile EQ gt_data-zeile .

  MODIFY zhkpmt0009 FROM TABLE lt_t0009 .

  gt_data-icon = '@5B@'."  green

ENDFORM.                    " SAVE_DA
*&---------------------------------------------------------------------*
*&      Form  MODIFY_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM modify_data .

  DATA : lt_t0009 LIKE TABLE OF  zhkpmt0009 WITH HEADER LINE .

  CHECK gt_data[] IS NOT INITIAL .

  SELECT * INTO TABLE lt_t0009
    FROM zhkpmt0009
    FOR ALL ENTRIES IN gt_data
   WHERE mblnr EQ gt_data-mblnr
     AND mjahr EQ gt_data-mjahr
     AND zeile EQ gt_data-zeile .

  SORT lt_t0009 BY mblnr mjahr zeile .

  LOOP AT gt_data .

* ICON
* '@5B@'."  green
* '@5C@'."  red
* '@5D@'."  yellow

    READ TABLE lt_t0009 WITH KEY mblnr = gt_data-mblnr
                                 mjahr = gt_data-mjahr
                                 zeile = gt_data-zeile
                                 BINARY SEARCH .
    IF sy-subrc = 0 .
      gt_data-icon = '@5B@'."  green
      gt_data-ERDAT = lt_t0009-erdat .
      gt_data-ERZET = lt_t0009-erzet .
      gt_data-ERNAM = lt_t0009-ernam .
    ELSE .
      gt_data-icon = '@5D@'."  yellow
    ENDIF .

    MODIFY gt_data .
  ENDLOOP .

ENDFORM.                    " MODIFY_DATA
