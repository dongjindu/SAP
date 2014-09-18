*&---------------------------------------------------------------------*
*& Report  ZHBCR00200
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  ZHBCR00200             MESSAGE-ID bt NO STANDARD PAGE HEADING
LINE-SIZE 119. "INE-COUNT 100.

*&---------------------------------------------------------------------*
*& Table & Data Declaration
*&---------------------------------------------------------------------*
*--- Table
TABLES: tstc, trdir, trdirt, ZHBCT002. "ZTBCDPER
*--- Internal Table
DATA: BEGIN OF yytb OCCURS 0,
        yymmdd LIKE sy-datum,
END OF yytb.

DATA: statistic_version_used LIKE sapwlpstrc-version.
DATA: BEGIN OF v2_normal_records OCCURS 100.
        INCLUDE STRUCTURE sapwlpfnrm.
DATA: END OF v2_normal_records.

DATA: BEGIN OF v2_btc_step_records OCCURS 100.
        INCLUDE STRUCTURE sapwlpfbtc.
DATA: END OF v2_btc_step_records.
DATA: stat_file LIKE SORTED TABLE OF v2_normal_records
WITH NON-UNIQUE KEY report account
WITH HEADER LINE.
DATA: i_ztbct LIKE SORTED TABLE OF ZHBCT002
WITH NON-UNIQUE KEY report account server respti
WITH HEADER LINE.
DATA: rc TYPE i VALUE 0.

DATA: BEGIN OF ttab OCCURS 0,
        prog TYPE program,
        r_time TYPE sta_mresti,
END OF ttab.

DATA: tgt_host_chk_has_failed TYPE i VALUE 1,
no_batch_server_found TYPE i VALUE 4.

DATA BEGIN OF btc_sys_tbl OCCURS 10.
        INCLUDE STRUCTURE btctgtitbl.
DATA END OF btc_sys_tbl.
DATA: BEGIN OF sys_tabl OCCURS 50.
        INCLUDE STRUCTURE msxxlist.
DATA: END OF sys_tabl.
DATA: v_startdate LIKE sy-datum,
      v_enddate LIKE sy-datum,
      v_starttime LIKE sy-uzeit,
      v_endtime LIKE sy-uzeit.
DATA: t_time LIKE sy-uzeit,
      gv_client LIKE t000-mandt,
      gv_date LIKE sy-datum,
gv_server LIKE btch1140-execserver,
gv_fname LIKE sapwlpstrc-filename.
DATA: i_cnt LIKE sy-dbcnt.

*&---------------------------------------------------------------------*
*& Event: INITIALIZATION
*&---------------------------------------------------------------------*
INITIALIZATION.
*
  gv_client = sy-mandt.
* ## ## ##
  PERFORM get_btc_systems USING rc.

  IF rc EQ tgt_host_chk_has_failed.
    MESSAGE s505.
    EXIT.
  ELSEIF rc EQ no_batch_server_found.
    MESSAGE s504.
    EXIT.
  ENDIF.
  READ TABLE sys_tabl WITH KEY host = sy-host.
  IF sy-subrc = 0.
    gv_server = sys_tabl-name.
  ENDIF.

*&---------------------------------------------------------------------*
*& Event: START-OF-SELECTION
*&---------------------------------------------------------------------*
START-OF-SELECTION.
* SET PF-STATUS 'ZMENU'. "APPLICATION TOOL BAR ##
* SET TITLEBAR '001'.
  PERFORM calc_startdatetime.
  PERFORM collect_data.
  DESCRIBE TABLE i_ztbct LINES i_cnt.
  IF sy-subrc = 0.
    COMMIT WORK.
    MESSAGE i499(sy) WITH i_cnt '# Insert ##.'.
  ELSE.
    ROLLBACK WORK.
    MESSAGE i499(sy) WITH 'Insert Failure.'.
  ENDIF.
  CHECK sy-batch = ' '.
  PERFORM list_output.
*&---------------------------------------------------------------------*
*& Event: TOP-OF-PAGE
*&---------------------------------------------------------------------*
TOP-OF-PAGE.
  PERFORM page_header.

*&---------------------------------------------------------------------*
*& Form LIST_OUTPUT
*&---------------------------------------------------------------------*
FORM list_output.
* BATCH ### ### ####
  LOOP AT i_ztbct.
    WRITE : / '|' NO-GAP, (10) sy-tabix NO-GAP,
    '|' NO-GAP, gv_date NO-GAP,
    '|' NO-GAP, i_ztbct-report NO-GAP,
    '|' NO-GAP, i_ztbct-account NO-GAP,
    '|' NO-GAP, i_ztbct-server NO-GAP,
    '|' NO-GAP, (20) i_ztbct-respti NO-GAP, '|' NO-GAP.
  ENDLOOP.
  ULINE.
ENDFORM. " LIST_OUTPUT

*&---------------------------------------------------------------------*
*& Form ADJUST_YYMMDD
*&---------------------------------------------------------------------*
* text
*----------------------------------------------------------------------*
FORM adjust_yymmdd.
  gv_date+4(02) = gv_date+4(02) - 1.
  IF gv_date+4(02) = '00'.
    gv_date+4(02) = '12'.
    gv_date(04) = gv_date(04) - 1.
  ENDIF.
  yytb-yymmdd = gv_date.
  APPEND yytb.
ENDFORM. " ADJUST_YYMMDD

*&---------------------------------------------------------------------*
*& Form GET_BTC_SYSTEMS
*&---------------------------------------------------------------------*
* text
*----------------------------------------------------------------------*
* -->P_RC text
*----------------------------------------------------------------------*
FORM get_btc_systems USING rc.
  DATA:
  batch LIKE msxxlist-msgtypes VALUE 255,
  batch_server_found TYPE i,
  lv_smal(26) VALUE 'abcdefghijklmnopqrstuvwxyz',
  lv_char,
  num_lines TYPE i.

  FREE sys_tabl.
*
* Liste aller Batchinstanzen (beim Messageserver) abholen
*
  CALL FUNCTION 'TH_SERVER_LIST'
    EXPORTING
      services = batch
    TABLES
      list     = sys_tabl
    EXCEPTIONS
      OTHERS   = 99.

  IF sy-subrc <> 0.
    rc = tgt_host_chk_has_failed. " Liste kann nicht beschafft werden
    EXIT.
  ENDIF.

  DESCRIBE TABLE sys_tabl LINES num_lines.

  IF num_lines EQ 0.
    rc = no_batch_server_found. " kein Batchserver vorhanden
    EXIT.
  ENDIF.

  SORT sys_tabl BY name ASCENDING.
*//SERVER NAMER ADJUST
  LOOP AT sys_tabl.
    lv_char = sys_tabl-name+0(01).
    IF lv_smal CA lv_char.
      TRANSLATE sys_tabl-host TO LOWER CASE.
    ELSE.
      TRANSLATE sys_tabl-host TO UPPER CASE.
    ENDIF.
    MODIFY sys_tabl.
  ENDLOOP.
*
  FREE btc_sys_tbl.
  LOOP AT sys_tabl.
    btc_sys_tbl-btcsystem = sys_tabl-host.
    btc_sys_tbl-instname = sys_tabl-name.
    APPEND btc_sys_tbl.
  ENDLOOP.

  rc = 0.

ENDFORM. " GET_BTC_SYSTEMS
*&---------------------------------------------------------------------*
*& Form CALC_STARTDATETIME
*&---------------------------------------------------------------------*
* text
*----------------------------------------------------------------------*
* --> p1 text
* <-- p2 text
*----------------------------------------------------------------------*
FORM calc_startdatetime.
*/// #### # ## Setting
  CLEAR: t_time.
  gv_date = sy-datum.
  t_time = sy-uzeit.
  v_startdate = gv_date.
  v_starttime = t_time - 3600.
  v_enddate = gv_date.
  v_endtime = sy-uzeit.
  IF v_starttime > sy-uzeit.
    v_startdate = gv_date - 1.
  ENDIF.
ENDFORM. " CALC_STARTDATETIME
*&---------------------------------------------------------------------*
*& Form COLLECT_DATA
*&---------------------------------------------------------------------*
* text
*----------------------------------------------------------------------*
* --> p1 text
* <-- p2 text
*----------------------------------------------------------------------*
FORM collect_data.
* DATA : lt_all_stats TYPE stad_allstats. "OCCURS 0 WITH HEADER LINE.
  DATA: l_datum LIKE sy-datum,
  l_problems TYPE int4,
  l_total TYPE int4,
  l_loghandle TYPE balloghndl,
  lt_all_stats TYPE stad_allstats. "WITH HEADER LINE.

  DATA: lt_protocol LIKE sapwlminut OCCURS 0 WITH HEADER LINE,
        lt_returns  LIKE sapwlsfrer OCCURS 0 WITH HEADER LINE,
        lt_server   LIKE msxxlist OCCURS 0 WITH HEADER LINE.

  DEFINE add_rec_to_main.
    move-corresponding &1   to ls_main.
  END-OF-DEFINITION. "Macro ADD_REC_TO_MAIN

  LOOP AT sys_tabl.
*    CHECK gv_server = sys_tabl-name.
    APPEND sys_tabl   TO lt_server .
  ENDLOOP.

  CALL FUNCTION 'SWNC_STAD_READ_STATRECS'
  EXPORTING
* read_client = '*'
     read_client = sy-mandt
* read_time = '240000'
     read_time = '010000'
     read_start_date = v_startdate
* read_start_time = v_starttime "'000000'
     read_start_time = v_starttime
     read_username = '*'
     wait_factor = 150
     include_appl_stat = ' '
  IMPORTING
     problems    = l_problems
     total_recs_read = l_total
     loghandle   = l_loghandle
  TABLES
     protocol    = lt_protocol
     rfc_returns = lt_returns
     server_list = lt_server
  CHANGING
     all_stats = lt_all_stats.

  FIELD-SYMBOLS:<lf_record> TYPE stad_statrec.

  DATA: ls_all_stats TYPE stad_statrec,
        ls_main TYPE stad_output,
        ls_record TYPE swncmainrec.

  LOOP AT lt_all_stats ASSIGNING <lf_record>.
    add_rec_to_main <lf_record>.
  ENDLOOP.

*// ## PROGRAM# #### CBO# ####.
*// 5# ### ### #### ####, ms### ####.
  LOOP AT lt_all_stats INTO ls_all_stats    WHERE record-respti >= 3000000.

    MOVE-CORRESPONDING ls_all_stats-main TO ls_main.
    MOVE-CORRESPONDING ls_all_stats-record TO ls_record.
*    CHECK LS_RECORD-REPORT+0(1) EQ 'Z' OR LS_RECORD-REPORT+0(5) EQ 'SAPMZ' OR
*    LS_RECORD-REPORT+0(1) EQ 'Y' OR LS_RECORD-REPORT+0(5) EQ 'SAPMY'.

*    CHECK ls_main-task     NE 'B'.     "BATCH ##
*    CHECK ls_main-task     NE 'S'.     "SPOOL ##
*    CHECK ls_main-task     NE 'R'.     "RFC   ##
*    CHECK ls_record-tcode  NE 'SM51'.  "Exclude remote
*    CHECK ls_record-tcode  NE 'SM59'.  "login trans-
*    CHECK ls_record-tcode  NE 'SM66'.  "actions and
*    CHECK ls_record-tcode  NE 'SH01'.                       "F1-Help
*    CHECK ls_record-report NE 'RSHLP001'.

    MOVE-CORRESPONDING ls_main TO i_ztbct.
    MOVE-CORRESPONDING ls_record TO i_ztbct.
    i_ztbct-server = ls_main-instance.
* i_ztbct-ENDDATE = LS_RECORD-ENDDATE.
* I_ZTBCT-ENDTIME = LS_RECORD-ENDTIME.
    i_ztbct-respti = ls_record-respti / 1000.
    i_ztbct-dbcallti = ls_record-dbreqtime / 1000.
    i_ztbct-procti = ls_record-procti / 1000.
    i_ztbct-cputi = ls_record-cputi / 1000.
    i_ztbct-queueti = ls_record-queueti / 1000.

    INSERT table i_ztbct.
    CLEAR i_ztbct.

  ENDLOOP.
  MODIFY ZHBCT002 FROM TABLE i_ztbct.

ENDFORM. " COLLECT_DATA
*&---------------------------------------------------------------------*
*& Form page_header
*&---------------------------------------------------------------------*
* text
*----------------------------------------------------------------------*
* --> p1 text
* <-- p2 text
*----------------------------------------------------------------------*
FORM page_header.
  FORMAT COLOR 1.
  ULINE.
  WRITE: / sy-vline NO-GAP,
  (10) 'Seq No' CENTERED NO-GAP,sy-vline NO-GAP,
  (10) '##' CENTERED NO-GAP, sy-vline NO-GAP,
  (40) '#####' CENTERED NO-GAP, sy-vline NO-GAP.
  SET LEFT SCROLL-BOUNDARY.
  WRITE: (12) '###' CENTERED NO-GAP, sy-vline NO-GAP,
  (20) '##' CENTERED NO-GAP, sy-vline NO-GAP,
  (20) '####(Total)' NO-GAP, sy-vline NO-GAP.
  ULINE.

ENDFORM. " page_header
