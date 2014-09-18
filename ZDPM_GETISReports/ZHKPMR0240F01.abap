*&---------------------------------------------------------------------*
*&  Include           ZHKPMR0240F01
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

  s_idate = 'IBT'.
  s_idate-low = sy-datum - 1 .
  s_idate-high = sy-datum - 1 .
  APPEND s_idate .

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
         lt_imrg  LIKE TABLE OF imrg  WITH HEADER LINE .

  DATA : lt_equz  LIKE TABLE OF equz  WITH HEADER LINE ,
         lt_iloa  LIKE TABLE OF iloa  WITH HEADER LINE ,
         lt_equi  LIKE TABLE OF equi  WITH HEADER LINE ,
         lt_imptt LIKE TABLE OF imptt WITH HEADER LINE ,
         lt_qpct  LIKE TABLE OF qpct  WITH HEADER LINE ,
         lt_aufk  LIKE TABLE OF aufk  WITH HEADER LINE ,
         lt_cabn  LIKE TABLE OF cabn  WITH HEADER LINE .


*. Get Company Code
  SELECT * INTO TABLE lt_t001k
    FROM t001k
   WHERE bwkey IN s_werks .

  SELECT * INTO TABLE lt_t001
    FROM t001
   WHERE bukrs EQ p_bukrs .


  CLEAR : gt_data[], gt_data .

*. Get row data : Measurement Document
  SELECT *
    INTO TABLE lt_imrg
    FROM imrg
   WHERE idate IN s_idate .

  DELETE lt_imrg WHERE cancl = c_x .
*.
  IF lt_imrg[] IS NOT INITIAL .

*.. Measuring Point (Table)
    SELECT * INTO TABLE lt_imptt
      FROM imptt
      FOR ALL ENTRIES IN lt_imrg
     WHERE point EQ lt_imrg-point .


    IF lt_imptt[] IS NOT INITIAL .

*.. Reservation/dependent requirements
      SELECT * INTO TABLE lt_equi
        FROM equi
        FOR ALL ENTRIES IN lt_imptt
       WHERE objnr EQ lt_imptt-mpobj .

*.. Characteristic
      SELECT * INTO TABLE lt_cabn
        FROM cabn
        FOR ALL ENTRIES IN lt_imptt
       WHERE atinn = lt_imptt-atinn .

    ENDIF .


*.. Equipment time segment
    IF lt_equi[] IS NOT INITIAL .
      SELECT * INTO TABLE lt_equz
        FROM equz
        FOR ALL ENTRIES IN lt_equi
       WHERE equnr EQ lt_equi-equnr
         AND datbi EQ '99991231' .
    ENDIF .

*.. PM Object Location and Account Assignment
    IF lt_equz[] IS NOT INITIAL .
      SELECT * INTO TABLE lt_iloa
        FROM iloa
        FOR ALL ENTRIES IN lt_equz
       WHERE iloan EQ lt_equz-iloan .

      DELETE lt_iloa WHERE swerk NOT IN s_werks[] .
    ENDIF .

*.. Code texts
    SELECT * INTO TABLE lt_qpct
      FROM qpct
      FOR ALL ENTRIES IN lt_imrg
     WHERE katalogart = lt_imrg-codct
       AND codegruppe = lt_imrg-codgr
       AND code       = lt_imrg-vlcod
       AND sprache    = sy-langu
       AND version    = lt_imrg-cvers.

*.. Order master data
    SELECT * INTO TABLE lt_aufk
      FROM aufk
      FOR ALL ENTRIES IN lt_imrg
     WHERE objnr = lt_imrg-woobj .


  ENDIF .

*.

*.
  SORT lt_imptt BY point.
  SORT lt_equi  BY objnr .
  SORT lt_equz  BY equnr .
  SORT lt_iloa  BY iloan .
  SORT lt_aufk  BY objnr .
  SORT lt_qpct  BY katalogart codegruppe code version .
  SORT lt_cabn  BY atinn .

  LOOP AT lt_imrg .

    CLEAR  gt_data .
    CLEAR : lt_imptt , lt_equi , lt_equz , lt_iloa  ,
            lt_cabn  , lt_qpct , lt_aufk .

    READ TABLE lt_imptt WITH KEY point = lt_imrg-point
                                 BINARY SEARCH .

    READ TABLE lt_equi WITH KEY objnr = lt_imptt-mpobj
                                BINARY SEARCH .

    READ TABLE lt_equz WITH KEY equnr = lt_equi-equnr
                                BINARY SEARCH .

    READ TABLE lt_iloa WITH KEY iloan = lt_equz-iloan
                                BINARY SEARCH .
    IF sy-subrc <> 0 .
      CONTINUE .
    ENDIF .

    READ TABLE lt_cabn WITH KEY atinn = lt_imptt-atinn
                                BINARY SEARCH .

    READ TABLE lt_qpct WITH KEY katalogart = lt_imrg-codct
                                codegruppe = lt_imrg-codgr
                                code       = lt_imrg-vlcod
                                version    = lt_imrg-cvers
                                BINARY SEARCH .

    READ TABLE lt_aufk WITH KEY objnr = lt_imrg-woobj
                                BINARY SEARCH .

    READ TABLE lt_t001k WITH KEY bwkey = lt_iloa-swerk .
    IF sy-subrc = 0 .
      gt_data-bukrs  = lt_t001k-bukrs .
    ENDIF .

    gt_data-werks = lt_iloa-swerk .
    gt_data-mdocm = lt_imrg-mdocm .
    gt_data-point = lt_imrg-point .
    gt_data-idate = lt_imrg-idate .

    gt_data-equnr = lt_equi-equnr .

    gt_data-tplnr = lt_iloa-tplnr .
    gt_data-beber = lt_iloa-beber .

    gt_data-pttxt = lt_imptt-pttxt .

    gt_data-atnam = lt_cabn-atnam .
    gt_data-msehi = lt_cabn-msehi .

    gt_data-desir = lt_imptt-desir .
    gt_data-dstxt = lt_imptt-dstxt .
    gt_data-mrmin = lt_imptt-mrmin .
    gt_data-mrmax = lt_imptt-mrmax .
    gt_data-mrngu = lt_imptt-mrngu .
    gt_data-codgr = lt_imptt-codgr .

    gt_data-readr = lt_imrg-readr .
    gt_data-recdv = lt_imrg-recdv .
    gt_data-recdvi = lt_imrg-recdvi .
    gt_data-recdu = lt_imrg-recdu .
    gt_data-vlcod = lt_imrg-vlcod .

    gt_data-kurztext = lt_qpct-kurztext .

    gt_data-aufnr = lt_aufk-aufnr .

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
  DATA : lt_pmt0024 LIKE TABLE OF zhkpmt0024 WITH HEADER LINE .

  CHECK gt_data[] IS NOT INITIAL .

  SELECT * INTO TABLE lt_pmt0024
    FROM zhkpmt0024
    FOR ALL ENTRIES IN gt_data
   WHERE bukrs EQ gt_data-bukrs
     AND werks EQ gt_data-werks
     AND mdocm EQ gt_data-mdocm .


  SORT gt_data BY bukrs werks mdocm.

  LOOP AT lt_pmt0024 .
    READ TABLE gt_data WITH KEY bukrs = lt_pmt0024-bukrs
                                werks = lt_pmt0024-werks
                                mdocm = lt_pmt0024-mdocm
                                BINARY SEARCH .
    IF sy-subrc = 0 .
      gt_data-zifflag   = lt_pmt0024-zifflag .
      gt_data-zifresult = lt_pmt0024-zifresult .
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
   'X'  l_tabnam   'MDOCM'  'MeaDoc'    '20' ' ' ' ' ' ' ' ' ' ' ,

   ' '  l_tabnam   'POINT'  'MeaPoint'  '12' ' ' ' ' ' ' ' ' ' ' ,

   ' '  l_tabnam   'IDATE'  'Date'      '10' ' ' ' ' ' ' ' ' ' ' ,
   ' '  l_tabnam   'EQUNR'  'Equipment' '18' ' ' ' ' ' ' ' ' ' ' ,

   ' '  l_tabnam   'TPLNR'  'FLoc'      '10' ' ' ' ' ' ' ' ' ' ' ,
   ' '  l_tabnam   'BEBER'  'SHOP'      '03' ' ' ' ' ' ' ' ' ' ' ,

   ' '  l_tabnam   'PTTXT'  'Desc'      '30' ' ' ' ' ' ' ' ' ' ' ,

   ' '  l_tabnam   'ATNAM'  'Char'      '30' ' ' ' ' ' ' ' ' ' ' ,
   ' '  l_tabnam   'MSEHI'  'Unit'      '03' ' ' ' ' ' ' ' ' ' ' ,

   ' '  l_tabnam   'DESIR' 'TargVal'    '22' '1' ' ' ' ' ' ' ' ' ,
   ' '  l_tabnam   'DSTXT'  'Desc'      '30' ' ' ' ' ' ' ' ' ' ' ,
   ' '  l_tabnam   'MRMIN'  'Low Val'   '22' '5' ' ' ' ' ' ' ' ' ,
   ' '  l_tabnam   'MRMAX'  'Up Val'    '22' '5' ' ' ' ' ' ' ' ' ,

   ' '  l_tabnam   'MRNGU'  'Unit'      '03' ' ' ' ' ' ' ' ' ' ' ,

   ' '  l_tabnam   'CODGR'  'CdGr'       '08' ' ' ' ' ' ' ' ' ' ' ,
   ' '  l_tabnam   'READR'  'Person'     '12' ' ' ' ' ' ' ' ' ' ' ,
   ' '  l_tabnam   'RECDV'  'MeaVal'     '22' '5' ' ' ' ' ' ' ' ' ,

   ' '  l_tabnam   'RECDVI'    'ID'      '03' ' ' ' ' ' ' ' ' 'C' ,
   ' '  l_tabnam   'RECDU'     'Unit'    '03' ' ' ' ' ' ' ' ' ' ' ,
   ' '  l_tabnam   'VLCOD'     'RsltCd'  '06' ' ' ' ' ' ' ' ' ' ' ,
   ' '  l_tabnam   'KURZTEXT'  'RsltDes' '40' ' ' ' ' ' ' ' ' ' ' ,
   ' '  l_tabnam   'AUFNR'     'Order'   '12' ' ' ' ' ' ' ' ' ' ' ,

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
    ls_fieldcat-qfieldname   = 'MSEHI'.
*    ls_fieldcat-do_sum       = 'X'.
  ENDIF.
  IF p_no_out = '2'.
    ls_fieldcat-hotspot = 'X'.
  ENDIF.

  IF p_no_out = '3'.
    ls_fieldcat-cfieldname   = 'WAERS'.
  ENDIF.

  IF p_no_out = '5'.
    ls_fieldcat-qfieldname   = 'MRNGU'.
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

  DATA : lt_data LIKE zhkpmt0024 OCCURS 0 WITH HEADER LINE ,
         lt_send LIKE zhkpmt0024 OCCURS 0 WITH HEADER LINE .

  DATA : lt_pmt0024 LIKE TABLE OF zhkpmt0024 WITH HEADER LINE .

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

    MOVE-CORRESPONDING gt_data TO lt_pmt0024 .
    APPEND lt_pmt0024 .

  ENDLOOP .

*. Update CBO table
  MODIFY zhkpmt0024 FROM TABLE lt_pmt0024 .
*  SORT lt_pmt0024 BY werks .

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

      UPDATE zhkpmt0024
         SET zifflag = c_p
       WHERE bukrs = lt_send-bukrs
         AND werks = lt_send-werks
         AND mdocm = lt_send-mdocm.

    ENDLOOP .

    COMMIT WORK .

*. Call RFC
    CALL FUNCTION 'ZPM024_CBM_ACTUAL_GETIS'
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
                                    mdocm = lt_send-mdocm .
        .

        IF sy-subrc = 0 .
          gt_data-icon = '@5C@'."  red
          MODIFY gt_data INDEX sy-tabix .

          UPDATE zhkpmt0024
             SET zifflag = c_e
                 zifresult = 'system_failure '
           WHERE bukrs = lt_send-bukrs
             AND werks = lt_send-werks
             AND mdocm = lt_send-mdocm .

        ENDIF .

        g_error = g_error + 1.
      ENDLOOP .

    ELSE .

      LOOP AT lt_send .
        READ TABLE gt_data WITH KEY werks = lt_send-werks
                                    mdocm = lt_send-mdocm .
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

          UPDATE zhkpmt0024
         SET zifflag   = lt_send-zifflag
             zifresult = lt_send-zifresult
           WHERE bukrs = lt_send-bukrs
             AND werks = lt_send-werks
             AND mdocm = lt_send-mdocm .

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
