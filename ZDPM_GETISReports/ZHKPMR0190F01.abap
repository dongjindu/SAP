*&---------------------------------------------------------------------*
*&  Include           ZHKPMR0190F01
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

  s_nplda = 'IBT'.
  s_nplda-low = sy-datum - 1 .
  s_nplda-high = sy-datum - 1 .
  APPEND s_nplda .

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
         lt_mhis  LIKE TABLE OF mhis  WITH HEADER LINE ,
         lt_mpla  LIKE TABLE OF mpla  WITH HEADER LINE ,
         lt_iloa  LIKE TABLE OF iloa  WITH HEADER LINE ,
         lt_mhio  LIKE TABLE OF mhio  WITH HEADER LINE .

  DATA : lt_qmih  LIKE TABLE OF qmih  WITH HEADER LINE ,
         lt_qmel  LIKE TABLE OF qmel  WITH HEADER LINE ,
         lt_qmma  LIKE TABLE OF qmma  WITH HEADER LINE ,
         lt_qmsm  LIKE TABLE OF qmsm  WITH HEADER LINE ,
         lt_qmur  LIKE TABLE OF qmur  WITH HEADER LINE .

  data : lt_QPCT  like table of QPCT with header line .


*. Get Company Code
  SELECT * INTO TABLE lt_t001k
    FROM t001k
   WHERE bwkey IN s_werks .

  SELECT * INTO TABLE lt_t001
    FROM t001
   WHERE bukrs EQ p_bukrs .


  CLEAR : gt_data[], gt_data .

*. Get row data : Maintenance plan history
  SELECT *
    INTO TABLE lt_mhis
    FROM mhis
   WHERE nplda IN s_nplda .

  DELETE lt_mhis WHERE tsabr <> c_x .
  DELETE lt_mhis WHERE tsvbt = c_x OR
                       tsenq = c_x OR
                       tsenm = c_x OR
                       tstat = c_x .
*.
  IF lt_mhis[] IS NOT INITIAL .

*.. Call Object from Maintenance Order
    SELECT * INTO TABLE lt_mhio
      FROM mhio
      FOR ALL ENTRIES IN lt_mhis
     WHERE warpl EQ lt_mhis-warpl
       AND abnum EQ lt_mhis-abnum .

    IF lt_mhio[] IS NOT INITIAL .

*.. Quality message - maintenance data excerpt
      SELECT * INTO TABLE lt_qmih
        FROM qmih
        FOR ALL ENTRIES IN lt_mhio
       WHERE qmnum EQ lt_mhio-qmnum
         AND iwerk IN s_werks .

*.. Quality Notification
      SELECT * INTO TABLE lt_qmel
        FROM qmel
        FOR ALL ENTRIES IN lt_mhio
       WHERE qmnum EQ lt_mhio-qmnum .

*.. Quality notification - activities
      SELECT * INTO TABLE lt_qmma
        FROM qmma
        FOR ALL ENTRIES IN lt_mhio
       WHERE qmnum EQ lt_mhio-qmnum .

*.. Quality notification - tasks
      SELECT * INTO TABLE lt_qmsm
        FROM qmsm
        FOR ALL ENTRIES IN lt_mhio
       WHERE qmnum EQ lt_mhio-qmnum .

*.. Quality notification - causes
      SELECT * INTO TABLE lt_qmur
        FROM qmur
        FOR ALL ENTRIES IN lt_mhio
       WHERE qmnum EQ lt_mhio-qmnum .

    ENDIF .

*.. PM Object Location and Account Assignment
    IF lt_qmih[] IS NOT INITIAL .
      SELECT * INTO TABLE lt_iloa
        FROM iloa
        FOR ALL ENTRIES IN lt_qmih
       WHERE iloan EQ lt_qmih-iloan .
    ENDIF .


  ENDIF .

*..
   select * into table lt_QPCT
     from QPCT .

   sort lt_QPCT by KATALOGART CODEGRUPPE CODE .

*.
  SORT lt_mpla BY warpl .
  SORT lt_iloa BY iloan .
  SORT lt_mhio BY warpl .

  SORT lt_qmih BY qmnum .
  SORT lt_qmel BY qmnum .
  SORT lt_qmma BY qmnum .
  SORT lt_qmsm BY qmnum .
  SORT lt_qmur BY qmnum .

  LOOP AT lt_mhio .

    CLEAR   gt_data .
    CLEAR : lt_qmel , lt_iloa, lt_qmma , lt_qmsm, lt_qmur , lt_QPCT .

    READ TABLE lt_qmih WITH KEY qmnum = lt_mhio-qmnum
                                BINARY SEARCH .
    IF sy-subrc <> 0 .
      CONTINUE .
    ENDIF .

    READ TABLE lt_qmel WITH KEY qmnum = lt_mhio-qmnum
                                BINARY SEARCH .

    READ TABLE lt_iloa WITH KEY iloan = lt_qmih-iloan
                                BINARY SEARCH .

    READ TABLE lt_qmma WITH KEY qmnum = lt_mhio-qmnum
                                BINARY SEARCH .

    read table lt_qpct with key KATALOGART = lt_qmma-MNKAT
                                CODEGRUPPE = lt_qmma-MNGRP
                                CODE       = lt_qmma-MNCOD
                                binary search .

    READ TABLE lt_qmsm WITH KEY qmnum = lt_mhio-qmnum
                               BINARY SEARCH .

    READ TABLE lt_qmur WITH KEY qmnum = lt_mhio-qmnum
                               BINARY SEARCH .

    READ TABLE lt_t001k WITH KEY bwkey = lt_qmih-iwerk .
    IF sy-subrc = 0 .
      gt_data-bukrs  = lt_t001k-bukrs .
    ENDIF .

    gt_data-iwerk = lt_qmih-iwerk .
    gt_data-warpl = lt_mhio-warpl .
    gt_data-abnum = lt_mhio-abnum .
    gt_data-wapos = lt_mhio-wppos .
    gt_data-qmnum = lt_mhio-qmnum .

    gt_data-qmart = lt_qmel-qmart .
    gt_data-qmtxt = lt_qmel-qmtxt .
    gt_data-equnr = lt_qmih-equnr .

    gt_data-tplnr = lt_iloa-tplnr .
    gt_data-beber = lt_iloa-beber .
    .
    gt_data-qmnam = lt_qmel-qmnam .
    gt_data-ltrmn = lt_qmel-ltrmn .
    gt_data-qmdab = lt_qmel-qmdab .

**    gt_data-matxt = lt_qmma-matxt .
    if lt_qpct-code is not initial .
    CONCATENATE lt_qpct-code '#' lt_qpct-KURZTEXT
           into gt_data-matxt .
    endif .

    gt_data-mttxt = lt_qmsm-matxt .
    gt_data-urtxt = lt_qmur-urtxt .

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
  DATA : lt_pmt0019 LIKE TABLE OF zhkpmt0019 WITH HEADER LINE .

  CHECK gt_data[] IS NOT INITIAL .

  SELECT * INTO TABLE lt_pmt0019
    FROM zhkpmt0019
    FOR ALL ENTRIES IN gt_data
   WHERE bukrs EQ gt_data-bukrs
     AND iwerk EQ gt_data-iwerk
     AND warpl EQ gt_data-warpl
     AND abnum EQ gt_data-abnum
     AND wapos EQ gt_data-wapos
     AND qmnum EQ gt_data-qmnum .

  SORT gt_data BY bukrs iwerk warpl abnum wapos qmnum .

  LOOP AT lt_pmt0019 .
    READ TABLE gt_data WITH KEY bukrs = lt_pmt0019-bukrs
                                iwerk = lt_pmt0019-iwerk
                                warpl = lt_pmt0019-warpl
                                abnum = lt_pmt0019-abnum
                                wapos = lt_pmt0019-wapos
                                qmnum = lt_pmt0019-qmnum
                                BINARY SEARCH .
    IF sy-subrc = 0 .
      gt_data-zifflag   = lt_pmt0019-zifflag .
      gt_data-zifresult = lt_pmt0019-zifresult .
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
   'X'  l_tabnam   'IWERK'  'Plant'     '04' ' ' ' ' ' ' ' ' ' ' ,
   'X'  l_tabnam   'WARPL'  'MntPlan'   '12' ' ' ' ' ' ' ' ' ' ' ,

   'X'  l_tabnam   'ABNUM'  'Call No.'  '10' ' ' ' ' ' ' ' ' ' ' ,

   'X'  l_tabnam   'WAPOS'  'MntItem'   '16' ' ' ' ' ' ' ' ' ' ' ,
   'X'  l_tabnam   'QMNUM'  'Notification'  '12' ' ' ' ' ' ' ' ' ' ' ,

   ' '  l_tabnam   'QMART'  'Type'   '04' ' ' ' ' ' ' ' ' 'C' ,
   ' '  l_tabnam   'QMTXT'  'Desc'   '30' ' ' ' ' ' ' ' ' ' ' ,

   ' '  l_tabnam   'EQUNR'  'Equioment'   '18' ' ' ' ' ' ' ' ' ' ' ,

   ' '  l_tabnam   'TPLNR'  'Flocation'   '30' ' ' ' ' ' ' ' ' ' ' ,
   ' '  l_tabnam   'BEBER'  'Shop'        '03' ' ' ' ' ' ' ' ' ' ' ,

   ' '  l_tabnam   'QMNAM'  'User'        '12' ' ' ' ' ' ' ' ' ' ' ,
   ' '  l_tabnam   'LTRMN'  'PlnDate'     '10' ' ' ' ' ' ' ' ' ' ' ,
   ' '  l_tabnam   'QMDAB'  'CompDate'    '10' ' ' ' ' ' ' ' ' ' ' ,

   ' '  l_tabnam   'MATXT'  'Activity'    '30' ' ' ' ' ' ' ' ' ' ' ,
   ' '  l_tabnam   'MTTXT'  'Task'        '30' ' ' ' ' ' ' ' ' ' ' ,
   ' '  l_tabnam   'URTXT'  'Cause'       '30' ' ' ' ' ' ' ' ' ' ' ,

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

  DATA : lt_data LIKE zhkpmt0019 OCCURS 0 WITH HEADER LINE ,
         lt_send LIKE zhkpmt0019 OCCURS 0 WITH HEADER LINE .

  DATA : lt_pmt0019 LIKE TABLE OF zhkpmt0019 WITH HEADER LINE .

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

    MOVE-CORRESPONDING gt_data TO lt_pmt0019 .
    APPEND lt_pmt0019 .

  ENDLOOP .

*. Update CBO table
  MODIFY zhkpmt0019 FROM TABLE lt_pmt0019 .
*  SORT lt_pmt0019 BY werks .

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

      UPDATE zhkpmt0019
         SET zifflag = c_p
       WHERE bukrs = lt_send-bukrs
         AND iwerk = lt_send-iwerk
         AND warpl = lt_send-warpl
         AND abnum = lt_send-abnum
         AND wapos = lt_send-wapos
         AND qmnum = lt_send-qmnum .

    ENDLOOP .

    COMMIT WORK .

*. Call RFC
    CALL FUNCTION 'ZPM019_PLAN_NOTI_GETIS'
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
        READ TABLE gt_data WITH KEY iwerk = lt_send-iwerk
                                    warpl = lt_send-warpl
                                    abnum = lt_send-abnum
                                    wapos = lt_send-wapos
                                    qmnum = lt_send-qmnum .
        .

        IF sy-subrc = 0 .
          gt_data-icon = '@5C@'."  red
          MODIFY gt_data INDEX sy-tabix .

          UPDATE zhkpmt0019
             SET zifflag = c_e
                 zifresult = 'system_failure '
           WHERE bukrs = lt_send-bukrs
             AND iwerk = lt_send-iwerk
             AND warpl = lt_send-warpl
             AND abnum = lt_send-abnum
             AND wapos = lt_send-wapos
             AND qmnum = lt_send-qmnum .

        ENDIF .

        g_error = g_error + 1.
      ENDLOOP .

    ELSE .

      LOOP AT lt_send .
        READ TABLE gt_data WITH KEY iwerk = lt_send-iwerk
                                    warpl = lt_send-warpl
                                    abnum = lt_send-abnum
                                    wapos = lt_send-wapos
                                    qmnum = lt_send-qmnum .
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

          UPDATE zhkpmt0019
         SET zifflag   = lt_send-zifflag
             zifresult = lt_send-zifresult
           WHERE bukrs = lt_send-bukrs
             AND iwerk = lt_send-iwerk
             AND warpl = lt_send-warpl
             AND abnum = lt_send-abnum
             AND wapos = lt_send-wapos
             AND qmnum = lt_send-qmnum .

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
