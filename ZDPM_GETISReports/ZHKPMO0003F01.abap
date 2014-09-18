*&---------------------------------------------------------------------*
*&  Include           ZHKPMO0003F01
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

  PERFORM get_data_row .

  PERFORM get_data_zhkpmt0010 .

ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  GET_DATA_ROW
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data_row .

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

    CLEAR : gt_row .
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

    gt_row-labst   = lt_mard-labst .
    gt_row-zprpqq  = l_prqty + l_poqty .
    gt_row-lminb   = lt_mard-lminb .

    l_totqty = gt_row-labst + gt_row-zprpqq .

    IF gt_row-lminb > l_totqty .
      gt_row-check = c_x .
    ENDIF .

    gt_row-werks = lt_mard-werks .
    gt_row-lgort = lt_mard-lgort .
    gt_row-matnr = lt_mard-matnr .


    gt_row-maabc = lt_marc-maabc .
    gt_row-lbstf = lt_mard-lbstf .

    APPEND gt_row .

  ENDLOOP .

ENDFORM.                    " GET_DATA_ROW
*&---------------------------------------------------------------------*
*&      Form  GET_DATA_ZHKPMT0010
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data_zhkpmt0010 .

  CLEAR : gt_pmt0010[] .

  SELECT * INTO TABLE gt_pmt0010
    FROM zhkpmt0010
   WHERE werks IN s_werks
     AND lgort IN s_lgort
     AND matnr IN s_matnr .

ENDFORM.                    " GET_DATA_ZHKPMT0010
*&---------------------------------------------------------------------*
*&      Form  MODIFY_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM modify_data .

  DATA : lt_t001k LIKE TABLE OF t001k WITH HEADER LINE ,
         lt_t001  LIKE TABLE OF t001  WITH HEADER LINE .

  DATA : lt_mbew LIKE TABLE OF mbew WITH HEADER LINE .

  DATA : lt_mara LIKE TABLE OF mara WITH HEADER LINE ,
         lt_makt LIKE TABLE OF makt WITH HEADER LINE .

  DATA: l_shop_today TYPE sydatum,
        l_shop_date  TYPE sydatum,
        l_time_f     TYPE t,
        l_time_t     TYPE t,
        l_n1me_ordur TYPE n1me_ordur.

  CLEAR : gt_data[], gt_data .

  CHECK gt_row[] IS NOT INITIAL .

*.. Company Codes
  SELECT * INTO TABLE lt_t001k
    FROM t001k
   WHERE bwkey IN s_werks .
  IF sy-subrc = 0 .
    SELECT * INTO TABLE lt_t001
      FROM t001
      FOR ALL ENTRIES IN lt_t001k
    WHERE bukrs EQ lt_t001k-bukrs .
  ENDIF .


*.. General Material Data
  SELECT * INTO TABLE lt_mara
    FROM mara
    FOR ALL ENTRIES IN gt_row
   WHERE matnr EQ gt_row-matnr .

*.. Material Descriptions
  SELECT * INTO TABLE lt_makt
    FROM makt
    FOR ALL ENTRIES IN gt_row
   WHERE matnr EQ gt_row-matnr
     AND spras EQ sy-langu .


*.. Get moving average Price/Periodic Unit Price
  SELECT * INTO TABLE lt_mbew
    FROM mbew
    FOR ALL ENTRIES IN gt_row
  WHERE matnr EQ gt_row-matnr
    AND bwkey EQ gt_row-werks .


  SORT lt_mbew BY matnr bwkey .
  SORT lt_t001k BY bwkey .
  SORT lt_t001 BY bukrs .
  SORT lt_mara BY matnr .
  SORT lt_makt BY matnr .


  SORT gt_pmt0010 BY werks lgort matnr .

  LOOP AT gt_row .
    CLEAR : gt_data .

    MOVE-CORRESPONDING gt_row TO gt_data .
    gt_data-menge = gt_row-zprpqq .

    IF gt_row-check = 'X'  .

      CLEAR : l_shop_date , l_shop_today , l_n1me_ordur .
      READ TABLE gt_pmt0010 WITH KEY werks = gt_row-werks
                                     lgort = gt_row-lgort
                                     matnr = gt_row-matnr
                                     BINARY SEARCH .
      IF sy-subrc = 0 .
        l_shop_date   = gt_pmt0010-zdate .
        gt_data-aedat = sy-datum .
        gt_data-aezet = sy-uzeit .
        gt_data-aenam = sy-uname .
        gt_data-zdate = gt_pmt0010-zdate .
      ELSE .
*        gt_data-zdate = sy-datum .
        gt_data-erdat = sy-datum .
        gt_data-erzet = sy-uzeit .
        gt_data-ernam = sy-uname .
        gt_data-aedat = sy-datum .
        gt_data-aezet = sy-uzeit .
        gt_data-aenam = sy-uname .
      ENDIF .

      IF l_shop_date IS INITIAL or
         l_shop_date = '00000000' .
        l_shop_date   = sy-datum .
        gt_data-zdate = sy-datum .
      ENDIF .

      l_shop_today = sy-datum .

      CALL FUNCTION 'ISHMED_CALC_DATE_TIME_DIFF'
        EXPORTING
          i_begdt    = l_shop_date
          i_begtm    = l_time_f
          i_enddt    = l_shop_today
          i_endtm    = l_time_t
        IMPORTING
          e_diff_day = l_n1me_ordur.

      gt_data-zdays  =  l_n1me_ordur + 1.

    ENDIF .

    IF gt_data-zdays = 0 .
      gt_data-zstat  = 'N' .
      gt_data-zsttxt = 'Normal' .

    ELSEIF gt_data-zdays >= 1 AND
           gt_data-zdays <= 7 .
      gt_data-zstat  = 'B' .
      gt_data-zsttxt = 'Below Stock' .

    ELSEIF gt_data-zdays >= 8 AND
           gt_data-zdays <= 14 .
      gt_data-zstat  = 'S' .
      gt_data-zsttxt = 'Short Shortage' .

    ELSEIF gt_data-zdays > 14  .
      gt_data-zstat  = 'L' .
      gt_data-zsttxt = 'Long Shortage' .

    ENDIF .

    READ TABLE lt_mbew WITH KEY matnr = gt_data-matnr
                                bwkey = gt_data-werks
                                BINARY SEARCH .
    IF sy-subrc = 0 .
      gt_data-verpr = lt_mbew-verpr .
      gt_data-peinh = lt_mbew-peinh .

      IF gt_data-peinh IS NOT INITIAL .
        gt_data-lbkum = ( gt_data-labst * gt_data-verpr ) / gt_data-peinh .
      ENDIF .
    ENDIF .

    READ TABLE lt_t001k WITH KEY bwkey = gt_data-werks
                                 BINARY SEARCH .
    IF sy-subrc = 0 .
      READ TABLE lt_t001 WITH KEY bukrs = lt_t001k-bukrs
                                  BINARY SEARCH .
      IF sy-subrc = 0 .
        gt_data-waers = lt_t001-waers .
        gt_data-bukrs = lt_t001-bukrs .
      ENDIF .
    ENDIF .

    READ TABLE lt_mara WITH KEY matnr = gt_data-matnr
                                BINARY SEARCH .
    IF sy-subrc = 0 .
      gt_data-meins = lt_mara-meins .
      gt_data-wrkst = lt_mara-wrkst .
    ENDIF .

    READ TABLE lt_makt WITH KEY matnr = gt_data-matnr
                                BINARY SEARCH .
    IF sy-subrc = 0 .
      gt_data-maktx = lt_makt-maktx .
    ENDIF .

    gt_data-zsdate = sy-datum .

    APPEND gt_data .

  ENDLOOP .

*.
  DATA : lt_pmt0010 LIKE TABLE OF zhkpmt0010 WITH HEADER LINE .

  LOOP AT gt_data .
    CLEAR lt_pmt0010 .
    MOVE-CORRESPONDING gt_data TO lt_pmt0010 .
    APPEND lt_pmt0010 .
  ENDLOOP .

  MODIFY zhkpmt0010 FROM TABLE lt_pmt0010 .


ENDFORM.                    " MODIFY_DATA
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

  PERFORM process_sned_mail .


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
   'X'  l_tabnam   'BUKRS'  'CO Code'     '04' ' ' ' ' ' ' ' ' ' ' ,
   'X'  l_tabnam   'WERKS'  'Plant'    '05' ' ' ' ' ' ' ' ' ' ' ,
   'X'  l_tabnam   'LGORT'  'Sloc'     '10' ' ' ' ' ' ' ' ' ' ' ,

   'X'  l_tabnam   'MATNR'  'Material'   '18' ' ' ' ' ' ' ' ' ' ' ,

   ' '  l_tabnam   'ZSDATE' 'Date'       '10' ' ' ' ' ' ' ' ' ' ' ,
   ' '  l_tabnam   'MAKTX'  'Desc'       '30' ' ' ' ' ' ' ' ' ' ' ,
   ' '  l_tabnam   'MEINS'  'Unit'       '04' ' ' ' ' ' ' ' ' 'C' ,
   ' '  l_tabnam   'WRKST'  'Spec'       '30' ' ' ' ' ' ' ' ' ' ' ,
   ' '  l_tabnam   'LMINB'  'Reorder point(MIN)'  '10' '1' ' ' ' ' ' ' ' ' ,
   ' '  l_tabnam   'LBSTF'  'Re Qty(MAX)'        '10' '1' ' ' ' ' ' ' ' ' ,

   ' '  l_tabnam   'MAABC'  'ABC'          '03' ' ' ' ' ' ' ' ' 'C' ,
   ' '  l_tabnam   'LABST'  'Current Stock' '10' '1' ' ' ' ' ' ' ' ' ,
   ' '  l_tabnam   'LBKUM'  'Amount'       '10' '3' ' ' ' ' ' ' ' ' ,

   ' '  l_tabnam   'WAERS'  'Currency'     '05' ' ' ' ' ' ' ' ' ' ' ,
   ' '  l_tabnam   'VERPR'  'Price'        '10' '3' ' ' ' ' ' ' ' ' ,

   ' '  l_tabnam   'PEINH'  'Punit'        '05' ' ' ' ' ' ' ' ' 'C' ,
   ' '  l_tabnam   'MENGE'  'Pur Qty'      '10' '1' ' ' ' ' ' ' ' ' ,
   ' '  l_tabnam   'ZDATE'  'Shortage date' '10' ' ' ' ' ' ' ' ' ' ' ,
   ' '  l_tabnam   'ZDAYS'  'Shortage days' '04' ' ' ' ' ' ' ' ' ' ' ,
   ' '  l_tabnam   'ZSTAT'  'Status'        '05' ' ' ' ' ' ' ' ' ' ' ,
   ' '  l_tabnam   'ZSTTXT'  'S text'       '20' ' ' ' ' ' ' ' ' ' '  .

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

  IF p_fieldname = 'MATNR_SORT'.
    ls_fieldcat-no_out  = 'X'.
  ENDIF.


  IF p_fieldname = 'EQUNR' .
    ls_fieldcat-ref_fieldname  = 'EQUNR' .
    ls_fieldcat-ref_tabname    = 'EQUI' .
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

      CASE rs_selfield-fieldname.
        WHEN 'MATNR' .
          SET PARAMETER ID 'MAT' FIELD gt_data-matnr .
          CALL TRANSACTION 'MM03' AND SKIP FIRST SCREEN.
      ENDCASE .

    WHEN 'ZEMAIL' . "Send mail
      PERFORM process_sned_mail .
      rs_selfield-refresh = c_x .

  ENDCASE .

ENDFORM.                    "USER_COMMAND
*&---------------------------------------------------------------------*
*&      Form  PROCESS_SNED_MAIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM process_sned_mail .

  READ TABLE gt_data WITH KEY mark = c_x .
  IF sy-subrc <> 0 .
    MESSAGE s000 WITH 'There is no selected line' .
    EXIT .
  ENDIF .

  CLEAR : gt_send[] .

  LOOP AT gt_data WHERE mark = c_x .

    IF gt_data-zstat = 'N' .
      CONTINUE .
    ENDIF .

    IF gt_data-maabc = 'A' OR
       gt_data-maabc = 'B' .
    ELSE .
      CONTINUE .
    ENDIF .

    IF gt_data-zstat = 'B' AND "Below Stock
       p_chk_a IS INITIAL .
      CONTINUE .
    ENDIF .

    IF gt_data-zstat = 'S' AND "Short Shortage
       p_chk_b IS INITIAL .
      CONTINUE .
    ENDIF .

    IF gt_data-zstat = 'L' AND "Long Shortage
       p_chk_c IS INITIAL .
      CONTINUE .
    ENDIF .

    APPEND gt_data TO gt_send .
  ENDLOOP .

  IF gt_send[] IS INITIAL .
    MESSAGE s000 WITH 'Not found data(ABC:A,B)'.
    EXIT .
  ENDIF .

  DATA : lt_pmt0021 LIKE TABLE OF zhkpmt0021 WITH HEADER LINE ,
         lt_sort    LIKE TABLE OF zhkpmt0021 WITH HEADER LINE .
  DATA : l_count TYPE i .

  SELECT * INTO TABLE lt_pmt0021
    FROM zhkpmt0021 .

  lt_sort[] = lt_pmt0021[] .
  SORT lt_pmt0021 BY lgort .
  SORT lt_sort BY lgort .

  DELETE ADJACENT DUPLICATES FROM lt_sort COMPARING lgort .


  LOOP AT lt_sort .

    CLEAR l_count  .

    DO 3 TIMES .

      l_count = l_count + 1 .

      CASE l_count .
        WHEN 1 .
          PERFORM send_mail USING 'B'  "Below safety Stock
                                  lt_sort-lgort .
        WHEN 2 .
          PERFORM send_mail USING 'S'   "Short Term Shortage
                                  lt_sort-lgort .
        WHEN 3 .
          PERFORM send_mail USING 'L'   "Long term Shortage
                                  lt_sort-lgort .
      ENDCASE .

      CHECK gt_mail[] IS NOT INITIAL .

      READ TABLE lt_pmt0021 WITH KEY lgort = lt_sort-lgort
                                     BINARY SEARCH .
      IF sy-subrc = 0 .
        LOOP AT lt_pmt0021 FROM sy-tabix .
          IF lt_pmt0021-lgort <> lt_sort-lgort .
            EXIT .
          ENDIF .

          CASE l_count .
            WHEN 1 .
              IF lt_pmt0021-zfrst IS INITIAL .
                CONTINUE .
              ENDIF .
            WHEN 2 .
              IF lt_pmt0021-zsecd IS INITIAL .
                CONTINUE .
              ENDIF .
            WHEN 3 .
              IF lt_pmt0021-zthrd IS INITIAL .
                CONTINUE .
              ENDIF .

          ENDCASE .

          PERFORM make_receiver_new USING lt_pmt0021-zmail .
          PERFORM send_mail_function .
        ENDLOOP .
      ENDIF .

    ENDDO .

  ENDLOOP .

ENDFORM.                    " PROCESS_SNED_MAIL
*&---------------------------------------------------------------------*
*&      Form  SEND_MAIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM send_mail  USING    p_status
                          p_lgort .

  MOVE gt_send[] TO gt_mail[] .

  LOOP AT gt_mail .

    IF gt_mail-lgort <> p_lgort .
      DELETE gt_mail . CONTINUE .
    ENDIF .

    CASE p_status .
      WHEN 'B' .
        IF gt_mail-zdays <= '7' .
        ELSE .
          DELETE gt_mail .
        ENDIF .
      WHEN 'S' .
        IF gt_mail-zdays > '7'  AND
           gt_mail-zdays <= '14' .
        ELSE .
          DELETE gt_mail .
        ENDIF .
      WHEN 'L' .
        IF gt_mail-zdays > '14' .
        ELSE .
          DELETE gt_mail .
        ENDIF .
    ENDCASE .

  ENDLOOP .

  CHECK gt_mail[] IS NOT INITIAL .

  PERFORM get_send_data USING p_status .


ENDFORM.                    " SEND_MAIL
*&---------------------------------------------------------------------*
*&      Form  GET_SEND_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_STATUS  text
*----------------------------------------------------------------------*
FORM get_send_data  USING    p_status .



  CLEAR : gt_cont_bin, gt_cont_bin[].
  CLEAR : gt_cont_txt, gt_cont_txt[] .
  PERFORM input_title.
  PERFORM input_item.
  PERFORM make_header USING    p_status.
  PERFORM make_text   USING    p_status.
  PERFORM make_pack   USING    p_status.


ENDFORM.                    " GET_SEND_DATA
*&---------------------------------------------------------------------*
*&      Form  INPUT_TITLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM input_title .

  DATA : l_output(1000) TYPE c.
  CLEAR : l_output.

  CONCATENATE 'CO Code'     'Plant'            'Sloc'
              'Material'     'Date' 'Desc'
              'Unit'    'Spec'     'Safety Stock'
              'Re Qty' 'ABC'  'Current Stock'
              'Amount'         'Currency'  'Price'
              'Punit' 'Pur Qty'
              'Shortage date'    'Shortage days'
              'Status' 'S text'
         INTO l_output SEPARATED BY con_tab.


  WHILE l_output <> space.
    CALL FUNCTION 'TEXT_SPLIT'
      EXPORTING
        length = 255
        text   = l_output
      IMPORTING
        line   = gt_cont_bin
        rest   = l_output.

    IF l_output = space.
      CONCATENATE con_cret gt_cont_bin con_cret INTO gt_cont_bin.
    ENDIF.
    APPEND gt_cont_bin.  CLEAR gt_cont_bin.
  ENDWHILE.


ENDFORM.                    " INPUT_TITLE
*&---------------------------------------------------------------------*
*&      Form  INPUT_ITEM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM input_item .
  DATA : l_output(1000) TYPE c.
  CLEAR : l_output.
  DATA : l_lminb(10) ,
         l_lbstf(10) ,
         l_lbkum(10) ,
         l_labst(10) ,
         l_verpr(10) ,
         l_menge(10) ,
         l_peinh .

  DATA : l_zdate(10) .

  LOOP AT gt_mail.

    WRITE gt_mail-lbkum TO l_lbkum CURRENCY gt_mail-waers .
    WRITE gt_mail-verpr TO l_verpr CURRENCY gt_mail-waers .

    WRITE gt_mail-lminb TO l_lminb UNIT gt_mail-meins .
    WRITE gt_mail-lbstf TO l_lbstf UNIT gt_mail-meins .
    WRITE gt_mail-labst TO l_labst UNIT gt_mail-meins .
    WRITE gt_mail-menge TO l_menge UNIT gt_mail-meins .

    WRITE gt_mail-peinh TO l_peinh .

    WRITE gt_mail-zdate TO l_zdate .

    CONCATENATE  gt_mail-bukrs  gt_mail-werks  gt_mail-lgort
                 gt_mail-matnr  gt_mail-zsdate  gt_mail-maktx
                 gt_mail-meins  gt_mail-wrkst   l_lminb
                 l_lbstf  gt_mail-maabc l_labst  l_lbkum
                 gt_mail-waers  l_verpr  l_peinh
                 l_menge  l_zdate  gt_mail-zdays
                 gt_mail-zstat  gt_mail-zsttxt
           INTO  l_output SEPARATED BY con_tab.

    WHILE l_output <> space.
      CALL FUNCTION 'TEXT_SPLIT'
        EXPORTING
          length = 255
          text   = l_output
        IMPORTING
          line   = gt_cont_bin
          rest   = l_output.

      IF l_output = space.
        CONCATENATE con_cret gt_cont_bin con_cret INTO gt_cont_bin.
      ENDIF.
      APPEND gt_cont_bin.  CLEAR gt_cont_bin.
    ENDWHILE.

  ENDLOOP.

ENDFORM.                    " INPUT_ITEM
*&---------------------------------------------------------------------*
*&      Form  MAKE_HEADER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM make_header USING    p_status.

  CLEAR: st_doc_header.
  DATA : l_cnt TYPE i.
  MOVE: 'Message'  TO st_doc_header-obj_name,
*        text-e01   TO st_doc_header-obj_descr,
        sy-langu   TO st_doc_header-obj_langu,
        'F'        TO st_doc_header-sensitivty,
        '1'        TO st_doc_header-obj_prio,
        '1'        TO st_doc_header-priority.

  CASE p_status. .
    WHEN 'B' .
      st_doc_header-obj_descr = text-e01 .
    WHEN 'S' .
      st_doc_header-obj_descr = text-e02 .
    WHEN 'L' .
      st_doc_header-obj_descr = text-e03 .
  ENDCASE .

ENDFORM.                    " MAKE_HEADER
*&---------------------------------------------------------------------*
*&      Form  MAKE_TEXT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM make_text  USING    p_status.

  DATA : ls_t001l LIKE t001l ,
         ls_t001w LIKE t001w .

  DATA : l_cnt   TYPE i,
         l_count(5) TYPE c .

  CASE p_status .
    WHEN 'B' .
      CLEAR : gt_cont_txt.
      CONCATENATE 'This is the 1st notice email for PR required Spare Parts. (Date/Time:'
                  sy-datum '/' sy-uzeit ')' INTO gt_cont_txt-line .
      APPEND gt_cont_txt.

    WHEN 'S' .
      CLEAR : gt_cont_txt.
      CONCATENATE 'This is the 2nd notice email for PR delayed Spare Parts. (Date/Time:'
                  sy-datum '/' sy-uzeit ')' INTO gt_cont_txt-line .
      APPEND gt_cont_txt.

    WHEN 'L' .
      CLEAR : gt_cont_txt.
      CONCATENATE 'This is the 3rd notice email for PR delayed Spare Parts. (Date/Time:'
                  sy-datum '/' sy-uzeit ')' INTO gt_cont_txt-line .
      APPEND gt_cont_txt.

  ENDCASE .

  CLEAR : gt_cont_txt. APPEND gt_cont_txt.
*.
  READ TABLE gt_mail INDEX 1 .

  SELECT SINGLE * INTO ls_t001w
    FROM t001w
   WHERE werks EQ gt_mail-werks .

  SELECT SINGLE * INTO ls_t001l
    FROM t001l
   WHERE werks EQ gt_mail-werks
     AND lgort EQ gt_mail-lgort .

  CLEAR : gt_cont_txt.
  CONCATENATE '1. Plant :' ls_t001w-name1
         INTO gt_cont_txt-line
              SEPARATED BY space .
  APPEND gt_cont_txt.
  CLEAR : gt_cont_txt. APPEND gt_cont_txt.

  CLEAR : gt_cont_txt.
  CONCATENATE '2. Inventory: ' gt_mail-lgort '(' ls_t001l-lgobe ')'
         INTO gt_cont_txt-line
              SEPARATED BY space .
  APPEND gt_cont_txt.
  CLEAR : gt_cont_txt. APPEND gt_cont_txt.

  CLEAR : gt_cont_txt.
  CASE p_status .
    WHEN 'B' .
      gt_cont_txt-line = text-e31 .
      APPEND gt_cont_txt.
    WHEN 'S' .
      gt_cont_txt-line = text-e32 .
      APPEND gt_cont_txt.
    WHEN 'L' .
      gt_cont_txt-line = text-e33 .
      APPEND gt_cont_txt.
  ENDCASE .

  CLEAR : gt_cont_txt. APPEND gt_cont_txt.

  CLEAR : gt_cont_txt.
  DESCRIBE TABLE gt_mail LINES l_cnt .
  WRITE l_cnt TO l_count .
  CONCATENATE '4.  Part Items :' l_count  'items'
         INTO gt_cont_txt-line
              SEPARATED BY space .
  APPEND gt_cont_txt.
  CLEAR : gt_cont_txt. APPEND gt_cont_txt.

  CLEAR : gt_cont_txt.
  gt_cont_txt-line = text-e11 .
  APPEND gt_cont_txt.
  CLEAR : gt_cont_txt.
  gt_cont_txt-line = text-e12 .
  APPEND gt_cont_txt.
  CLEAR : gt_cont_txt.
  gt_cont_txt-line = text-e13 .
  APPEND gt_cont_txt.
  CLEAR : gt_cont_txt.
  gt_cont_txt-line = text-e14 .
  APPEND gt_cont_txt.
  CLEAR : gt_cont_txt.
**  gt_cont_txt-line = text-e15 .
**  APPEND gt_cont_txt.
**  CLEAR : gt_cont_txt.
**  gt_cont_txt-line = text-e16 .
**  APPEND gt_cont_txt.
**  CLEAR : gt_cont_txt.
**  gt_cont_txt-line = text-e17 .
**  APPEND gt_cont_txt.
**  CLEAR : gt_cont_txt.
**  gt_cont_txt-line = text-e18 .
**  APPEND gt_cont_txt.
**  CLEAR : gt_cont_txt. APPEND gt_cont_txt.
**
**  CLEAR : gt_cont_txt.
**  gt_cont_txt-line = text-e19 .
**  APPEND gt_cont_txt.
**  CLEAR : gt_cont_txt.
**  gt_cont_txt-line = text-e20 .
**  APPEND gt_cont_txt.
**  CLEAR : gt_cont_txt. APPEND gt_cont_txt.
**
**  CLEAR : gt_cont_txt.
**  gt_cont_txt-line = text-e21 .
**  APPEND gt_cont_txt.
**  CLEAR : gt_cont_txt.
**  gt_cont_txt-line = text-e22 .
**  APPEND gt_cont_txt.
**  CLEAR : gt_cont_txt.
**  gt_cont_txt-line = text-e23 .
**  APPEND gt_cont_txt.

ENDFORM.                    " MAKE_TEXT
*&---------------------------------------------------------------------*
*&      Form  MAKE_PACK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_STATUS  text
*----------------------------------------------------------------------*
FORM make_pack  USING    p_status.
  DATA: lv_index TYPE i.

  CLEAR : lv_index.
** Describe the body of the message
  CLEAR gt_packing.
  REFRESH gt_packing.

  DESCRIBE TABLE gt_cont_txt LINES lv_index.
  READ TABLE gt_cont_txt INDEX lv_index.
*  gt_packing-doc_size = ( lv_index - 1 ) * 255 + STRLEN( gt_cont_txt ).
*  gt_packing-transf_bin = space.
  gt_packing-head_start = 1.
  gt_packing-head_num   = 0.
  gt_packing-body_start = 1.
  gt_packing-body_num   = lv_index.
*  DESCRIBE TABLE gt_cont_txt LINES gt_packing-body_num.
  gt_packing-doc_type = 'RAW'.
  CLEAR gt_packing-transf_bin.
  APPEND gt_packing.  CLEAR gt_packing.

* Create attachment notification
  DESCRIBE TABLE gt_cont_bin LINES lv_index.
  READ TABLE gt_cont_bin INDEX lv_index.

  gt_packing-transf_bin = 'X'.
  gt_packing-head_start = 1.
  gt_packing-head_num   = 0.
  gt_packing-body_start = 1.

*  DESCRIBE TABLE gt_cont_bin LINES gt_packing-body_num.
*  gt_packing-doc_size = ( lv_index - 1 ) * 255 + STRLEN( gt_cont_bin ).
  gt_packing-body_num   =  lv_index.
  gt_packing-doc_type   =  'XLS'.
  gt_packing-obj_descr  =  text-e01.
  CONCATENATE 'PR Required Part List'  ' '
       INTO gt_packing-obj_descr .
  gt_packing-obj_name   =  'ATTACHMENT'.
*  gt_packing-doc_size   =  gt_packing-body_num * 255.
  APPEND gt_packing.
*
*  st_doc_header-doc_size = st_doc_header-doc_size + gt_packing-doc_size.

ENDFORM.                    " MAKE_PACK
*&---------------------------------------------------------------------*
*&      Form  MAKE_RECEIVER_NEW
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM make_receiver_new  USING    p_zmail.

  CLEAR gt_receivers.
  REFRESH gt_receivers.
  gt_receivers-rec_type = 'U'.  " internet email
*  gt_receivers-rec_type = 'C'.
  gt_receivers-receiver = p_zmail.

  gt_receivers-com_type = 'INT'.
*  GT_RECEIVERS-notif_del = 'X'.
*  GT_RECEIVERS-notif_ndel = 'X'.
  APPEND gt_receivers.

ENDFORM.                    " MAKE_RECEIVER_NEW
*&---------------------------------------------------------------------*
*&      Form  SEND_MAIL_FUNCTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM send_mail_function .

  CALL FUNCTION 'SO_NEW_DOCUMENT_ATT_SEND_API1'
    EXPORTING
      document_data              = st_doc_header
      put_in_outbox              = 'X'
      commit_work                = 'X'
    TABLES
      packing_list               = gt_packing
      contents_bin               = gt_cont_bin
      contents_txt               = gt_cont_txt
      receivers                  = gt_receivers
    EXCEPTIONS
      too_many_receivers         = 1
      document_not_sent          = 2
      document_type_not_exist    = 3
      operation_no_authorization = 4
      parameter_error            = 5
      x_error                    = 6
      enqueue_error              = 7
      OTHERS                     = 8.

  IF sy-subrc = 0.
    SUBMIT rsconn01           "Start the sending process
          WITH mode   = 'INT'
          WITH output = ' '
          AND RETURN.

  ENDIF.

ENDFORM.                    " SEND_MAIL_FUNCTION
