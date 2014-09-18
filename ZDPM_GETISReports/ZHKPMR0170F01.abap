*&---------------------------------------------------------------------*
*&  Include           ZHKPMR0170F01
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

*  s_aedat = 'IGE' .
*  s_aedat-low = sy-datum - 1 .
*  APPEND s_aedat .

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
         lt_t001  LIKE TABLE OF t001  WITH HEADER LINE ,
         lt_plko  LIKE TABLE OF plko  WITH HEADER LINE ,
         lt_plpo  LIKE TABLE OF plpo  WITH HEADER LINE ,
         lt_plas  LIKE TABLE OF plas  WITH HEADER LINE ,
         lt_plmz  LIKE TABLE OF plmz  WITH HEADER LINE ,
         lt_stpo  LIKE TABLE OF stpo  WITH HEADER LINE .

  DATA : lt_eapl  LIKE TABLE OF eapl  WITH HEADER LINE ,
         lt_tapl  LIKE TABLE OF tapl  WITH HEADER LINE .

*. Get Company Code
  SELECT * INTO TABLE lt_t001k
    FROM t001k
   WHERE bwkey IN s_werks .

  SELECT * INTO TABLE lt_t001
    FROM t001
   WHERE bukrs EQ p_bukrs .


  CLEAR : gt_data[], gt_data .

*. Get row data : Task list - header
  IF s_aedat[] IS NOT INITIAL .

    SELECT *
      INTO TABLE lt_plko
      FROM plko
     WHERE werks IN s_werks
       AND ( plnty EQ 'E' OR
             plnty EQ 'T' ) .

    if lt_plko[] is not initial .
      PERFORM get_group_name tables lt_plko .
    endif .

    IF gt_group[] IS NOT INITIAL .
      SELECT *
        INTO TABLE lt_plko
        FROM plko
        FOR ALL ENTRIES IN gt_group
       WHERE plnnr EQ gt_group-plnnr
         AND werks IN s_werks
         AND ( plnty EQ 'E' OR
               plnty EQ 'T' ) .
    else .
      exit .
    ENDIF .
  ELSE .

    SELECT *
      INTO TABLE lt_plko
      FROM plko
     WHERE aedat IN s_aedat
       AND werks IN s_werks
       AND ( plnty EQ 'E' OR
             plnty EQ 'T' ) .
  ENDIF .

  DELETE lt_plko WHERE loekz = c_x .
*.
  IF lt_plko[] IS NOT INITIAL .

*.. Task list - selection of operations/activities
    SELECT * INTO TABLE lt_plas
      FROM plas
      FOR ALL ENTRIES IN lt_plko
     WHERE plnty EQ lt_plko-plnty
       AND plnnr EQ lt_plko-plnnr
       AND plnal EQ lt_plko-plnal .

    DELETE lt_plas WHERE loekz <> space .

*.. Task list - operation/activity
    IF lt_plas[] IS NOT INITIAL .
      SELECT * INTO TABLE lt_plpo
        FROM plpo
        FOR ALL ENTRIES IN lt_plas
       WHERE plnty EQ lt_plas-plnty
         AND plnnr EQ lt_plas-plnnr
         AND plnkn EQ lt_plas-plnkn  .
    ENDIF .

*.. Allocation of bill of material items to operations
    IF lt_plas[] IS NOT INITIAL .
      SELECT * INTO TABLE lt_plmz
        FROM plmz
        FOR ALL ENTRIES IN lt_plas
       WHERE plnty EQ lt_plas-plnty
         AND plnnr EQ lt_plas-plnnr
         AND plnal EQ lt_plas-plnal
         AND plnkn EQ lt_plas-plnkn  .
    ENDIF .

**.. BOM item
    IF lt_plmz[] IS NOT INITIAL .
      SELECT * INTO TABLE lt_stpo
        FROM stpo
        FOR ALL ENTRIES IN lt_plmz
       WHERE stlty EQ lt_plmz-stlty
         AND stlnr EQ lt_plmz-stlnr
         AND stlkn EQ lt_plmz-stlkn  .
    ENDIF .

*.. Allocation of task lists to pieces of equipment
    SELECT * INTO TABLE lt_eapl
      FROM eapl
      FOR ALL ENTRIES IN lt_plko
     WHERE plnty EQ lt_plko-plnty
       AND plnnr EQ lt_plko-plnnr
       AND plnal EQ lt_plko-plnal
       AND zaehl EQ lt_plko-zaehl .

**.. Allocation of task lists to functional locations
    SELECT * INTO TABLE lt_tapl
      FROM tapl
      FOR ALL ENTRIES IN lt_plko
     WHERE plnty EQ lt_plko-plnty
       AND plnnr EQ lt_plko-plnnr
       AND plnal EQ lt_plko-plnal
       AND zaehl EQ lt_plko-zaehl .

  ENDIF .

*.
  SORT lt_eapl BY plnty plnnr plnal zaehl .
  SORT lt_tapl BY plnty plnnr plnal zaehl .
  SORT lt_plas BY plnty plnnr plnal .
  SORT lt_plpo BY plnty plnnr plnkn .
  SORT lt_plmz BY plnty plnnr plnal plnkn .
  SORT lt_stpo BY stlty stlnr stlkn .

  LOOP AT lt_plko .

    CLEAR   gt_data .

    READ TABLE lt_t001k WITH KEY bwkey = lt_plko-werks .
    IF sy-subrc <> 0 .
      CONTINUE .
    ENDIF .

    gt_data-bukrs = lt_t001k-bukrs .
    gt_data-werks = lt_plko-werks .

    CASE lt_plko-plnty .
      WHEN 'E' .
        READ TABLE lt_eapl WITH KEY plnty = lt_plko-plnty
                                    plnnr = lt_plko-plnnr
                                    plnal = lt_plko-plnal
                                    zaehl = lt_plko-zaehl
                                    BINARY SEARCH .
        IF sy-subrc = 0 .
          gt_data-zfleq = lt_eapl-equnr .
        ENDIF .
      WHEN 'T' .
        READ TABLE lt_tapl WITH KEY plnty = lt_plko-plnty
                                    plnnr = lt_plko-plnnr
                                    plnal = lt_plko-plnal
                                    zaehl = lt_plko-zaehl
                                    BINARY SEARCH .
        IF sy-subrc = 0 .
          gt_data-zfleq = lt_tapl-tplnr .
        ENDIF .

    ENDCASE .

    gt_data-plnty = lt_plko-plnty .
    gt_data-plnnr = lt_plko-plnnr .
    gt_data-plnal = lt_plko-plnal .

    gt_data-datuv = lt_plko-datuv .
    gt_data-andat = lt_plko-andat .
    gt_data-aedat = lt_plko-aedat .
    gt_data-verwe = lt_plko-verwe .
    gt_data-ktext = lt_plko-ktext .

    READ TABLE lt_plas WITH KEY plnty = lt_plko-plnty
                                plnnr = lt_plko-plnnr
                                plnal = lt_plko-plnal
                                BINARY SEARCH .
**+ 1
    IF sy-subrc = 0 . "+ 3
      LOOP AT lt_plas FROM sy-tabix . "+ 2
        IF lt_plas-plnty <> lt_plko-plnty OR
           lt_plas-plnnr <> lt_plko-plnnr OR
           lt_plas-plnal <> lt_plko-plnal .
          EXIT.
        ENDIF .

        gt_data-plnkn = lt_plas-plnkn .

        READ TABLE lt_plpo WITH KEY plnty = lt_plas-plnty
                                    plnnr = lt_plas-plnnr
                                    plnkn = lt_plas-plnkn
                                    BINARY SEARCH .
        IF sy-subrc = 0 .
          gt_data-vornr = lt_plpo-vornr .
          gt_data-ltxa1 = lt_plpo-ltxa1 .
          gt_data-sortl = lt_plpo-sortl .
          gt_data-odatuv = lt_plpo-datuv .
          gt_data-oandat = lt_plpo-andat .
          gt_data-oaedat = lt_plpo-aedat .

        ELSE .
          CLEAR :
          gt_data-vornr ,
          gt_data-ltxa1 ,
          gt_data-sortl ,
          gt_data-odatuv ,
          gt_data-oandat ,
          gt_data-oaedat .

        ENDIF .

        CLEAR :
        gt_data-idnrk1 ,
        gt_data-menge1 ,
        gt_data-meins1 ,
        gt_data-idnrk2 ,
        gt_data-menge2 ,
        gt_data-meins2 ,
        gt_data-idnrk3 ,
        gt_data-menge3 ,
        gt_data-meins3 ,
        gt_data-idnrk4 ,
        gt_data-menge4 ,
        gt_data-meins4 ,
        gt_data-idnrk5 ,
        gt_data-menge5 ,
        gt_data-meins5 .

        READ TABLE lt_plmz WITH KEY plnty = lt_plas-plnty
                                    plnnr = lt_plas-plnnr
                                    plnal = lt_plas-plnal
                                    plnkn = lt_plas-plnkn
                                    BINARY SEARCH .
        IF sy-subrc = 0 . "+ 1
          LOOP AT lt_plmz FROM sy-tabix .
            IF lt_plmz-plnty <> lt_plas-plnty OR
               lt_plmz-plnnr <> lt_plas-plnnr OR
               lt_plmz-plnal <> lt_plas-plnal OR
               lt_plmz-plnkn <> lt_plas-plnkn .
              EXIT .
            ENDIF .

            READ TABLE lt_stpo WITH KEY stlty = lt_plmz-stlty
                                        stlnr = lt_plmz-stlnr
                                        stlkn = lt_plmz-stlkn
                                        BINARY SEARCH .
            IF sy-subrc = 0 .
              LOOP AT lt_stpo FROM sy-tabix .

                IF lt_stpo-stlty <> lt_plmz-stlty OR
                   lt_stpo-stlnr <> lt_plmz-stlnr OR
                   lt_stpo-stlkn <> lt_plmz-stlkn .
                  EXIT .
                ENDIF .

                IF gt_data-idnrk1 IS INITIAL .
                  gt_data-idnrk1 = lt_stpo-idnrk .
                  gt_data-menge1 = lt_stpo-menge .
                  gt_data-meins1 = lt_stpo-meins .
                ELSEIF gt_data-idnrk2 IS INITIAL .
                  gt_data-idnrk2 = lt_stpo-idnrk .
                  gt_data-menge2 = lt_stpo-menge .
                  gt_data-meins2 = lt_stpo-meins .
                ELSEIF gt_data-idnrk3 IS INITIAL .
                  gt_data-idnrk3 = lt_stpo-idnrk .
                  gt_data-menge3 = lt_stpo-menge .
                  gt_data-meins3 = lt_stpo-meins .
                ELSEIF gt_data-idnrk4 IS INITIAL .
                  gt_data-idnrk4 = lt_stpo-idnrk .
                  gt_data-menge4 = lt_stpo-menge .
                  gt_data-meins4 = lt_stpo-meins .
                ELSEIF gt_data-idnrk5 IS INITIAL .
                  gt_data-idnrk5 = lt_stpo-idnrk .
                  gt_data-menge5 = lt_stpo-menge .
                  gt_data-meins5 = lt_stpo-meins .
                ENDIF .

              ENDLOOP .
            ENDIF .

          ENDLOOP .
        ENDIF .  "+ 1

        gt_data-icon = '@5D@'."  yellow

        APPEND gt_data .

      ENDLOOP. "+ 2

    ENDIF . "+ 3

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
  DATA : lt_pmt0017 LIKE TABLE OF zhkpmt0017 WITH HEADER LINE .

  CHECK gt_data[] IS NOT INITIAL .

  SELECT * INTO TABLE lt_pmt0017
    FROM zhkpmt0017
    FOR ALL ENTRIES IN gt_data
   WHERE bukrs EQ gt_data-bukrs
     AND werks EQ gt_data-werks
     AND zfleq EQ gt_data-zfleq
     AND plnty EQ gt_data-plnty
     AND plnnr EQ gt_data-plnnr
     AND plnal EQ gt_data-plnal
     AND plnkn EQ gt_data-plnkn .

  SORT gt_data BY bukrs werks zfleq plnty plnnr plnal plnkn.

  LOOP AT lt_pmt0017 .
    READ TABLE gt_data WITH KEY bukrs = lt_pmt0017-bukrs
                                werks = lt_pmt0017-werks
                                zfleq = lt_pmt0017-zfleq
                                plnty = lt_pmt0017-plnty
                                plnnr = lt_pmt0017-plnnr
                                plnal = lt_pmt0017-plnal
                                plnkn = lt_pmt0017-plnkn
                                BINARY SEARCH .
    IF sy-subrc = 0 .
      gt_data-zifflag   = lt_pmt0017-zifflag .
      gt_data-zifresult = lt_pmt0017-zifresult .
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
   'X'  l_tabnam   'WERKS'  'Plant'     '04' ' ' ' ' ' ' ' ' ' ' ,
   'X'  l_tabnam   'ZFLEQ'  'Equipment' '30' ' ' ' ' ' ' ' ' ' ' ,

   'X'  l_tabnam   'PLNTY'  'Type'      '04' ' ' ' ' ' ' ' ' 'C' ,
   'X'  l_tabnam   'PLNNR'  'Group'     '08' ' ' ' ' ' ' ' ' ' ' ,
   'X'  l_tabnam   'PLNAL'  'Grcounter' '05' ' ' ' ' ' ' ' ' 'C' ,
   'X'  l_tabnam   'PLNKN'  'Node'      '08' ' ' ' ' ' ' ' ' ' ' ,

   ' '  l_tabnam   'DATUV'  'Vld Date'  '10' ' ' ' ' ' ' ' ' ' ' ,
   ' '  l_tabnam   'ANDAT'  'Hcr date'  '10' ' ' ' ' ' ' ' ' ' ' ,
   ' '  l_tabnam   'AEDAT'  'HCh Date'  '10' ' ' ' ' ' ' ' ' ' ' ,
   ' '  l_tabnam   'VERWE'  'Usage'     '03' ' ' ' ' ' ' ' ' ' ' ,
   ' '  l_tabnam   'KTEXT'  'Desc'      '30' ' ' ' ' ' ' ' ' ' ' ,

   ' '  l_tabnam   'ODATUV'  'VldFrom.' '10' ' ' ' ' ' ' ' ' ' ' ,
   ' '  l_tabnam   'OANDAT'  'Icr date' '10' ' ' ' ' ' ' ' ' ' ' ,
   ' '  l_tabnam   'OAEDAT'  'Ich Date' '10' ' ' ' ' ' ' ' ' ' ' ,

   ' '  l_tabnam   'VORNR'  'Op'        '040' ' ' ' ' ' ' ' ' ' ' ,
   ' '  l_tabnam   'LTXA1'  'Op Text'   '30' ' ' ' ' ' ' ' ' ' ' ,
   ' '  l_tabnam   'SORTL'  'PM Type'   '10' ' ' ' ' ' ' ' ' ' ' ,

   ' '  l_tabnam   'IDNRK1'  'Comp1'     '18' ' ' ' ' ' ' ' ' ' ' ,
   ' '  l_tabnam   'MENGE1'  'Qty1'      '10' 'A' ' ' ' ' ' ' ' ' ,
   ' '  l_tabnam   'MEINS1'  'Unit1'     '04' ' ' ' ' ' ' ' ' ' ' ,
   ' '  l_tabnam   'IDNRK2'  'Comp2'     '18' ' ' ' ' ' ' ' ' ' ' ,
   ' '  l_tabnam   'MENGE2'  'Qty2'      '10' 'B' ' ' ' ' ' ' ' ' ,
   ' '  l_tabnam   'MEINS2'  'Unit2'     '04' ' ' ' ' ' ' ' ' ' ' ,
   ' '  l_tabnam   'IDNRK3'  'Comp3'     '18' ' ' ' ' ' ' ' ' ' ' ,
   ' '  l_tabnam   'MENGE3'  'Qty3'      '10' 'C' ' ' ' ' ' ' ' ' ,
   ' '  l_tabnam   'MEINS3'  'Unit3'     '04' ' ' ' ' ' ' ' ' ' ' ,
   ' '  l_tabnam   'IDNRK4'  'Comp4'     '18' ' ' ' ' ' ' ' ' ' ' ,
   ' '  l_tabnam   'MENGE4'  'Qty4'      '10' 'D' ' ' ' ' ' ' ' ' ,
   ' '  l_tabnam   'MEINS4'  'Unit4'     '04' ' ' ' ' ' ' ' ' ' ' ,
   ' '  l_tabnam   'IDNRK5'  'Comp5'     '18' ' ' ' ' ' ' ' ' ' ' ,
   ' '  l_tabnam   'MENGE5'  'Qty5'      '10' 'E' ' ' ' ' ' ' ' ' ,
   ' '  l_tabnam   'MEINS5'  'Unit5'     '04' ' ' ' ' ' ' ' ' ' ' ,

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
    ls_fieldcat-qfieldname   = 'MEINS'.
*    ls_fieldcat-do_sum       = 'X'.
  ENDIF.
  IF p_no_out = '2'.
    ls_fieldcat-hotspot = 'X'.
  ENDIF.

  IF p_no_out = '3'.
    ls_fieldcat-cfieldname   = 'WAERS'.
  ENDIF.

  IF p_no_out = '5'.
    ls_fieldcat-qfieldname   = 'P_MEINS'.
  ENDIF.

  IF p_no_out = '7'.
    ls_fieldcat-cfieldname   = 'P_WAERS'.
  ENDIF.

  IF p_fieldname = 'MATNR_SORT'.
    ls_fieldcat-no_out  = 'X'.
  ENDIF.

  IF p_no_out = 'A'.
    ls_fieldcat-qfieldname   = 'MEINS1'.
*    ls_fieldcat-do_sum       = 'X'.
  ENDIF.
  IF p_no_out = 'B'.
    ls_fieldcat-qfieldname   = 'MEINS2'.
*    ls_fieldcat-do_sum       = 'X'.
  ENDIF.
  IF p_no_out = 'C'.
    ls_fieldcat-qfieldname   = 'MEINS3'.
*    ls_fieldcat-do_sum       = 'X'.
  ENDIF.
  IF p_no_out = 'D'.
    ls_fieldcat-qfieldname   = 'MEINS4'.
*    ls_fieldcat-do_sum       = 'X'.
  ENDIF.
  IF p_no_out = 'E'.
    ls_fieldcat-qfieldname   = 'MEINS5'.
*    ls_fieldcat-do_sum       = 'X'.
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

  DATA : lt_data LIKE zhkpmt0017 OCCURS 0 WITH HEADER LINE ,
         lt_send LIKE zhkpmt0017 OCCURS 0 WITH HEADER LINE .

  DATA : lt_pmt0017 LIKE TABLE OF zhkpmt0017 WITH HEADER LINE .

  LOOP AT gt_data  .

    IF gt_data-mark IS INITIAL .
      CONTINUE .
    ENDIF .

    gt_data-zifflag = c_i .
    gt_data-zifdate = sy-datum .
    clear gt_data-zifresult .
    MOVE-CORRESPONDING gt_data TO lt_data .
    APPEND lt_data .
    MODIFY gt_data .

    MOVE-CORRESPONDING gt_data TO lt_pmt0017 .
    APPEND lt_pmt0017 .

  ENDLOOP .

*. Update CBO table
  MODIFY zhkpmt0017 FROM TABLE lt_pmt0017 .
*  SORT lt_pmt0017 BY werks .

  DATA : l_at     TYPE i ,
         l_total  TYPE i ,
         l_start  TYPE i ,
         l_end    TYPE i .
  DATA : l_dmbtr(15) .


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

      UPDATE zhkpmt0017
         SET zifflag = c_p
       WHERE bukrs = lt_send-bukrs
         AND werks = lt_send-werks
         AND zfleq = lt_send-zfleq
         AND plnty = lt_send-plnty
         AND plnnr = lt_send-plnnr
         AND plnal = lt_send-plnal
         AND plnkn = lt_send-plnkn .

    ENDLOOP .

    COMMIT WORK .

*. Call RFC
    CALL FUNCTION 'ZPM017_TASK_LIST_GETIS'
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
        READ TABLE gt_data WITH KEY werks = lt_send-werks
                                    zfleq = lt_send-zfleq
                                    plnty = lt_send-plnty
                                    plnnr = lt_send-plnnr
                                    plnal = lt_send-plnal
                                    plnkn = lt_send-plnkn .

        IF sy-subrc = 0 .
          gt_data-icon = '@5C@'."  red
          MODIFY gt_data INDEX sy-tabix .

          UPDATE zhkpmt0017
             SET zifflag = c_e
                 zifresult = 'system_failure '
           WHERE bukrs = lt_send-bukrs
             AND werks = lt_send-werks
             AND zfleq = lt_send-zfleq
             AND plnty = lt_send-plnty
             AND plnnr = lt_send-plnnr
             AND plnal = lt_send-plnal
             AND plnkn = lt_send-plnkn .

        ENDIF .

        g_error = g_error + 1.
      ENDLOOP .

    ELSE .

      LOOP AT lt_send .
        READ TABLE gt_data WITH KEY werks = lt_send-werks
                                    zfleq = lt_send-zfleq
                                    plnty = lt_send-plnty
                                    plnnr = lt_send-plnnr
                                    plnal = lt_send-plnal
                                    plnkn = lt_send-plnkn .
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

        UPDATE zhkpmt0017
       SET zifflag   = lt_send-zifflag
           zifresult = lt_send-zifresult
         WHERE bukrs = lt_send-bukrs
           AND werks = lt_send-werks
           AND zfleq = lt_send-zfleq
           AND plnty = lt_send-plnty
           AND plnnr = lt_send-plnnr
           AND plnal = lt_send-plnal
           AND plnkn = lt_send-plnkn .

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
*&      Form  GET_GROUP_NAME
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_group_name tables pt_plko STRUCTURE  plko .

  CLEAR : gt_group, gt_group[] .

  SELECT plnnr aedat
    INTO CORRESPONDING FIELDS OF TABLE gt_group
    FROM plko
     FOR ALL ENTRIES IN pt_plko
   WHERE plnty EQ pt_plko-plnty
     AND plnnr EQ pt_plko-plnnr .

*   WHERE aedat IN s_aedat .

  SELECT plnnr aedat
    APPENDING CORRESPONDING FIELDS OF TABLE gt_group
    FROM plpo
     FOR ALL ENTRIES IN pt_plko
   WHERE plnty EQ pt_plko-plnty
     AND plnnr EQ pt_plko-plnnr .

*   WHERE aedat IN s_aedat .

  SELECT plnnr aedat
    APPENDING CORRESPONDING FIELDS OF TABLE gt_group
    FROM plas
     FOR ALL ENTRIES IN pt_plko
   WHERE plnty EQ pt_plko-plnty
     AND plnnr EQ pt_plko-plnnr .

*   WHERE aedat IN s_aedat .

  SELECT plnnr aedat
    APPENDING CORRESPONDING FIELDS OF TABLE gt_group
    FROM plmz
     FOR ALL ENTRIES IN pt_plko
   WHERE plnty EQ pt_plko-plnty
     AND plnnr EQ pt_plko-plnnr .

*   WHERE aedat IN s_aedat .

  delete gt_group where aedat not in s_aedat .

  SORT gt_group BY plnnr .
  DELETE ADJACENT DUPLICATES FROM gt_group COMPARING plnnr .


ENDFORM.                    " GET_GROUP_NAME
