*&---------------------------------------------------------------------*
*&  Include           ZHKPMR0010F01
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

  DATA : BEGIN OF lt_read OCCURS 0 ,
           matnr LIKE mara-matnr ,
         END OF lt_read .

  RANGES : r_lvorm FOR mard-lvorm .

  CLEAR : gt_data[], gt_data .

  IF s_date[] IS INITIAL .
    r_lvorm = 'IEQ' .
    r_lvorm-low = space .
    APPEND r_lvorm .


*. Get row data
    SELECT rd~werks
           rd~lgort
           rd~matnr
           kt~maktx
           ra~mtart
           ra~matkl
           ra~zeinr AS bismt
           ra~meins
           ra~wrkst
           rd~lvorm
           rd~lminb
           rd~lbstf
           rd~lgpbe
           rc~maabc
           rc~minbe
           rc~eisbe
           ra~ferth
           ra~mfrpn
      INTO CORRESPONDING FIELDS OF TABLE gt_data
      FROM mara AS ra INNER JOIN mard AS rd
                   ON ra~matnr = rd~matnr
                      INNER JOIN marc AS rc
                   ON rd~matnr = rc~matnr
                  AND rd~werks = rc~werks
                      INNER JOIN makt AS kt
                   ON kt~matnr = ra~matnr
                  AND kt~spras = sy-langu
    WHERE ra~matnr IN s_matnr
      AND ra~mtart IN s_mtart
      AND rd~werks IN s_werks
      AND rd~lgort IN s_lgort
      AND rd~lvorm IN r_lvorm .

  ELSE .


    SELECT objectid AS matnr
      INTO CORRESPONDING FIELDS OF TABLE lt_read
      FROM cdhdr
     WHERE OBJECTCLAS eq 'MATERIAL'
       and tcode IN ('MM01','MM02','MM06')
       AND udate IN s_date .

    sort lt_read .
    delete ADJACENT DUPLICATES FROM lt_read  .

*. Get row data
    IF lt_read[] IS NOT INITIAL .
      SELECT rd~werks
             rd~lgort
             rd~matnr
             kt~maktx
             ra~mtart
             ra~matkl
             ra~zeinr AS bismt
             ra~meins
             ra~wrkst
             rd~lvorm
             rd~lminb
             rd~lbstf
             rd~lgpbe
             rc~maabc
             rc~minbe
             rc~eisbe
             ra~ferth
             ra~mfrpn
        INTO CORRESPONDING FIELDS OF TABLE gt_data
        FROM mara AS ra INNER JOIN mard AS rd
                     ON ra~matnr = rd~matnr
                        INNER JOIN marc AS rc
                     ON rd~matnr = rc~matnr
                    AND rd~werks = rc~werks
                        INNER JOIN makt AS kt
                     ON kt~matnr = ra~matnr
                    AND kt~spras = sy-langu
       FOR ALL ENTRIES IN lt_read
      WHERE ra~matnr EQ lt_read-matnr
        AND ra~mtart IN s_mtart
        AND rd~werks IN s_werks
        AND rd~lgort IN s_lgort .
    ENDIF .


      SELECT rd~werks
             rd~lgort
             rd~matnr
             kt~maktx
             ra~mtart
             ra~matkl
             ra~zeinr AS bismt
             ra~meins
             ra~wrkst
             rd~lvorm
             rd~lminb
             rd~lbstf
             rd~lgpbe
             rc~maabc
             rc~minbe
             rc~eisbe
             ra~ferth
             ra~mfrpn
        APPENDING CORRESPONDING FIELDS OF TABLE gt_data
        FROM mara AS ra INNER JOIN mard AS rd
                     ON ra~matnr = rd~matnr
                        INNER JOIN marc AS rc
                     ON rd~matnr = rc~matnr
                    AND rd~werks = rc~werks
                        INNER JOIN makt AS kt
                     ON kt~matnr = ra~matnr
                    AND kt~spras = sy-langu
      WHERE ra~matnr in s_matnr
        AND ra~mtart IN s_mtart
        AND rd~werks IN s_werks
        AND rd~lgort IN s_lgort
        and ra~ersda in s_date .

  sort gt_data by werks lgort matnr .
  delete ADJACENT DUPLICATES FROM gt_data
                  comparing werks lgort matnr .

  ENDIF .

  CHECK gt_data[] IS NOT INITIAL .

  DATA : lt_mbew  LIKE TABLE OF mbew  WITH HEADER LINE ,
         lt_t001k LIKE TABLE OF t001k WITH HEADER LINE ,
         lt_t001  LIKE TABLE OF t001  WITH HEADER LINE .

*. Get Company Code
  SELECT * INTO TABLE lt_t001k
    FROM t001k
   WHERE bwkey IN s_werks .

*. Get Currency Key
  IF lt_t001k[] IS NOT INITIAL .
    SELECT * INTO TABLE lt_t001
      FROM t001
      FOR ALL ENTRIES IN lt_t001k
     WHERE bukrs EQ lt_t001k-bukrs .
  ENDIF .

*. Get moving average Price/Periodic Unit Price
  SELECT * INTO TABLE lt_mbew
    FROM mbew
    FOR ALL ENTRIES IN gt_data
  WHERE matnr EQ gt_data-matnr
    AND bwkey EQ gt_data-werks .

  SORT lt_mbew BY matnr bwkey .
  SORT lt_t001k BY bwkey .
  SORT lt_t001 BY bukrs .

  LOOP AT gt_data .

    READ TABLE lt_t001k WITH KEY bwkey = gt_data-werks
                                 BINARY SEARCH .
    IF sy-subrc = 0 .
      gt_data-bukrs = lt_t001k-bukrs .
    ENDIF .

    READ TABLE lt_mbew WITH KEY matnr = gt_data-matnr
                                bwkey = gt_data-werks
                                BINARY SEARCH .
    IF sy-subrc = 0 .
      gt_data-bklas = lt_mbew-bklas .
      gt_data-verpr = lt_mbew-verpr .
      gt_data-peinh = lt_mbew-peinh .
    ENDIF .

    READ TABLE lt_t001 WITH KEY bukrs = gt_data-bukrs
                                BINARY SEARCH .
    IF sy-subrc = 0 .
      gt_data-waers = lt_t001-waers .
    ENDIF .

*.
    IF gt_data-bukrs = 'HA08' .
      gt_data-bismt = gt_data-ferth .
    ENDIF .

    IF gt_data-bukrs = 'H201'  .
      gt_data-wrkst = gt_data-mfrpn .
    ENDIF .

    gt_data-icon = '@5D@'."  yellow
    MODIFY gt_data .
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
  DATA : lt_pmt0001 LIKE TABLE OF zhkpmt0001 WITH HEADER LINE .

  CHECK gt_data[] IS NOT INITIAL .

  SELECT * INTO TABLE lt_pmt0001
    FROM zhkpmt0001
    FOR ALL ENTRIES IN gt_data
   WHERE bukrs EQ gt_data-bukrs
     AND werks EQ gt_data-werks
     AND lgort EQ gt_data-lgort
     AND matnr EQ gt_data-matnr .

  SORT gt_data BY bukrs werks lgort matnr .

  LOOP AT lt_pmt0001 .
    READ TABLE gt_data WITH KEY bukrs = lt_pmt0001-bukrs
                                werks = lt_pmt0001-werks
                                lgort = lt_pmt0001-lgort
                                matnr = lt_pmt0001-matnr
                                BINARY SEARCH .
    IF sy-subrc = 0 .
      gt_data-zifflag   = lt_pmt0001-zifflag .
      gt_data-zifresult = lt_pmt0001-zifresult .
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
   'X'  l_tabnam   'ICON'   'Status'   '04' ' ' ' ' ' ' ' ' 'C' ,
   'X'  l_tabnam   'BUKRS'  'Code'     '04' ' ' ' ' ' ' ' ' ' ' ,
   'X'  l_tabnam   'WERKS'  'Plant'    '05' ' ' ' ' ' ' ' ' ' ' ,
   'X'  l_tabnam   'LGORT'  'Sloc'     '10' ' ' ' ' ' ' ' ' ' ' ,

   'X'  l_tabnam   'MATNR'  'Material'    '18' ' ' ' ' ' ' ' ' ' ' ,
   ' '  l_tabnam   'MAKTX'  'Mat Description'  '30' ' ' ' ' ' ' ' ' ' ' ,
   ' '  l_tabnam   'MTART'  'MType'       '05' ' ' ' ' ' ' ' ' ' ' ,
   ' '  l_tabnam   'MATKL'  'Matl Group'  '05' ' ' ' ' ' ' ' ' ' ' ,
   ' '  l_tabnam   'BISMT'  'Make'        '18' ' ' ' ' ' ' ' ' ' ' ,
   ' '  l_tabnam   'MEINS'  'unit'        '04' ' ' ' ' ' ' ' ' ' ' ,
   ' '  l_tabnam   'WRKST'  'material spec' '20' ' ' ' ' ' ' ' ' ' ' ,
   ' '  l_tabnam   'LVORM'  'DFlag'       '05' ' ' ' ' ' ' ' ' ' ' ,
   ' '  l_tabnam   'LMINB'  'Safety'      '08' '1' ' ' ' ' ' ' ' ' ,
   ' '  l_tabnam   'LBSTF'  'Repl'        '08' '1' ' ' ' ' ' ' ' ' ,
   ' '  l_tabnam   'LGPBE'  'SBin'        '08' ' ' ' ' ' ' ' ' ' ' ,
   ' '  l_tabnam   'MAABC'  'ABC'         '03' ' ' ' ' ' ' ' ' 'C' ,
   ' '  l_tabnam   'MINBE'  'RPoint'      '08' '1' ' ' ' ' ' ' ' ' ,

   ' '  l_tabnam   'EISBE'  'SStock'      '08' '1' ' ' ' ' ' ' ' ' ,

   ' '  l_tabnam   'BKLAS'  ' VClass'    '06' ' ' ' ' ' ' ' ' ' ' ,
   ' '  l_tabnam   'VERPR'  ' MPrice'    '06' '3' ' ' ' ' ' ' ' ' ,
   ' '  l_tabnam   'PEINH'  'PUint'      '10' ' ' ' ' ' ' ' ' ' ' ,
   ' '  l_tabnam   'WAERS'  'Currency'   '05' ' ' ' ' ' ' ' ' ' ' ,
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

  DATA : lt_data LIKE zhkpmt0001 OCCURS 0 WITH HEADER LINE ,
         lt_send LIKE zhkpmt0001 OCCURS 0 WITH HEADER LINE .

  DATA : lt_pmt0001 LIKE TABLE OF zhkpmt0001 WITH HEADER LINE .

  LOOP AT gt_data  .

    IF gt_data-mark IS INITIAL .
      CONTINUE .
    ENDIF .

    gt_data-zifflag = c_i .
    gt_data-zifdate = sy-datum .
    CLEAR : gt_data-zifresult .
    MOVE-CORRESPONDING gt_data TO lt_data .
    APPEND lt_data .
    MODIFY gt_data .

    MOVE-CORRESPONDING gt_data TO lt_pmt0001 .
    APPEND lt_pmt0001 .

  ENDLOOP .

*. Update CBO table
  MODIFY zhkpmt0001 FROM TABLE lt_pmt0001 .
  SORT lt_pmt0001 BY werks lgort matnr .

  DATA : l_at     TYPE i ,
         l_total  TYPE i ,
         l_start  TYPE i ,
         l_end    TYPE i .
  DATA : l_verpr(20) .

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
        WRITE lt_send-verpr TO l_verpr CURRENCY lt_send-waers .
        PERFORM set_write_no_mask USING l_verpr .
        lt_send-verpr = l_verpr .
      ENDIF .
      MODIFY lt_send .

      UPDATE zhkpmt0001
         SET zifflag = c_p
       WHERE bukrs = lt_send-bukrs
         AND werks = lt_send-werks
         AND lgort = lt_send-lgort
         AND matnr = lt_send-matnr .

    ENDLOOP .

    COMMIT WORK .

*. Call RFC
    CALL FUNCTION 'ZPM001_MATERIAL_GETIS'
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
                                    lgort = lt_send-lgort
                                    matnr = lt_send-matnr .
        IF sy-subrc = 0 .
          gt_data-icon = '@5C@'."  red
          MODIFY gt_data INDEX sy-tabix .

          UPDATE zhkpmt0001
             SET zifflag = c_e
                 zifresult = 'system_failure '
           WHERE bukrs = lt_send-bukrs
             AND werks = lt_send-werks
             AND lgort = lt_send-lgort
             AND matnr = lt_send-matnr .

        ENDIF .

        g_error = g_error + 1.
      ENDLOOP .

    ELSE .

      LOOP AT lt_send .
        READ TABLE gt_data WITH KEY werks = lt_send-werks
                                    lgort = lt_send-lgort
                                    matnr = lt_send-matnr .
        IF sy-subrc = 0 .
          IF lt_send-zifflag = 'S' OR
             lt_send-zifflag = 'Z' .
            gt_data-icon = '@5B@'."  green
          ELSE .
            gt_data-icon = '@5C@'."  red
          ENDIF .
          gt_data-zifflag = lt_send-zifflag .
          gt_data-zifresult = lt_send-zifresult .
          MODIFY gt_data INDEX sy-tabix .
        ENDIF .

        g_success = g_success + 1 .

        UPDATE zhkpmt0001
         SET zifflag   = lt_send-zifflag
             zifresult = lt_send-zifresult
       WHERE bukrs = lt_send-bukrs
         AND werks = lt_send-werks
         AND lgort = lt_send-lgort
         AND matnr = lt_send-matnr .

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
