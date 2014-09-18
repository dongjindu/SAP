*----------------------------------------------------------------------*
*   INCLUDE ZACOU101_F01                                               *
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       Get data of entered period
*----------------------------------------------------------------------*
FORM get_data.

  CLEAR gv_klvar.
  SELECT SINGLE klvar INTO gv_klvar
        FROM tck03 WHERE kalka EQ s_kalka-low.

* Get Costing Date
  PERFORM get_date.

* Get BDC Options
  PERFORM get_opt USING 'N'.

* Get data from table ZTCOU100
  PERFORM get_gt_out.

* by ig.moon 5/12/2008 {
* need to be changed
  DATA: l_idx LIKE sy-tabix,
        l_kalaid TYPE ck_kalaid.

** Furong on 10/31/11
  if not gt_out[] is initial.
** End on 10/31/11

  PERFORM get_gt_keko.
  PERFORM get_rollup.
  endif.

  LOOP AT gt_out.
    l_idx = sy-tabix.

    CLEAR l_kalaid.

    IF s_kalka-low <> 'M1'.
      PERFORM check_selected.
      PERFORM check_bom_exp.
    ENDIF.

    PERFORM check_costed.
    PERFORM check_roll_up.

    IF gt_out-lstat = 'X'.
      gv_rcnt = gv_rcnt + 1.
    ENDIF.

    MODIFY gt_out INDEX l_idx.
  ENDLOOP.

* }

ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  CREATE_FIELD_CATEGORY
*&---------------------------------------------------------------------*
*       Create ALV control: Field catalog
*----------------------------------------------------------------------*
FORM create_field_category.
  CLEAR gt_fcat.

  IF s_kalka-low = 'M1'.
    PERFORM fill_field_category USING:
            1  'MATNR'  'Product'         '20'  'CHAR',
            2  'STLAN'  'Usg'             '3'   'CHAR',
            3  'VERID'  'Ver'             '3'   'CHAR',
*            4  'CREXT'  'Out-Color'       '7'   'CHAR',
*            5  'CRINT'  'In-Color'        '7'   'CHAR',
            4  'CNFG'   'Configured'      '7'   'CHAR',
            5  'BWDAT'  'Valuation Date'  '0'   'DATS',
            6  'ALDAT'  'Qty struc.date'  '10'  'DATS',
            7  'CSTAT'  'Costed'          '09'  'ICON',
            8  'RSTAT1' 'Roll-up'         '09'  'ICON',
            9  'LSTAT'  'Lock'            '09'  'CHAR',
           10  'REG'    'Registered'      '9'   'CHAR',
           11  'MSG2' 'Remarks for Cst.Run   >'  '40'  'CHAR'.

  ELSE.
    PERFORM fill_field_category USING:
            1  'MATNR'  'Product'         '20'  'CHAR',
            2  'STLAN'  'Usg'             '3'   'CHAR',
            3  'VERID'  'Ver'             '3'   'CHAR',
            4  'CNFG'   'Configured'      '7'   'CHAR',
            5  'BWDAT'  'Valuation Date'  '0'   'DATS',
            6  'ALDAT'  'Qty struc.date'  '10'  'DATS',
            7  'SSTAT'  'Selected'        '09'  'ICON',
            8  'BSTAT'  'BOM'             '09'  'ICON',
            9  'CSTAT'  'Costed'          '09'  'ICON',
           10  'RSTAT1' 'Roll-up'         '09'  'ICON',
           11  'LSTAT'  'Lock'            '09'  'CHAR',
           12  'REG'    'Registered'      '9'   'CHAR',
           13  'MSG2' 'Remarks for Cst.Run   >'  '40'  'CHAR'.
  ENDIF.

  LOOP AT gt_fcat INTO gs_fcat.
    CASE gs_fcat-fieldname.
      WHEN 'MATNR'.
        gs_fcat-ref_field = gs_fcat-fieldname.
        gs_fcat-ref_table = 'MARA'.
        MODIFY gt_fcat FROM gs_fcat
                TRANSPORTING ref_field ref_table
                WHERE fieldname = gs_fcat-fieldname.

      WHEN 'VERID' OR 'CREXT' OR 'CRINT' OR 'BWDAT' OR 'ALDAT' .
        gs_fcat-just = 'C'.
        MODIFY gt_fcat FROM gs_fcat
                       TRANSPORTING just
                WHERE fieldname = gs_fcat-fieldname.

      WHEN 'CNFG' OR 'LSTAT'  OR 'REG' .
        gs_fcat-checkbox = 'X'.
        gs_fcat-just = 'C'.
        MODIFY gt_fcat FROM gs_fcat
                       TRANSPORTING checkbox just
                WHERE fieldname = gs_fcat-fieldname.

      WHEN 'REG' .
        IF p_rg EQ space.
          gs_fcat-no_out = 'X'.
          MODIFY gt_fcat FROM gs_fcat
                         TRANSPORTING checkbox just
                  WHERE fieldname = gs_fcat-fieldname.
        ENDIF.

      WHEN 'CSTAT'.
        gs_fcat-icon = 'X'.
        gs_fcat-just = 'C'.
        gs_fcat-hotspot = 'X'.

        MODIFY gt_fcat FROM gs_fcat
                       TRANSPORTING icon just hotspot
                WHERE fieldname = gs_fcat-fieldname.

      WHEN 'SSTAT' OR 'BSTAT' OR 'RSTAT1'.
        gs_fcat-icon = 'X'.
        gs_fcat-just = 'C'.

        MODIFY gt_fcat FROM gs_fcat
                       TRANSPORTING icon just
                WHERE fieldname = gs_fcat-fieldname.

    ENDCASE.

  ENDLOOP.

ENDFORM.                    " CREATE_FIELD_CATEGORY
*&---------------------------------------------------------------------*
*&      Form  SET_LVC_LAYOUT
*&---------------------------------------------------------------------*
*       Setting for layout
*----------------------------------------------------------------------*
FORM set_lvc_layout.
  gs_layo-zebra      = 'X'.
  gs_layo-sel_mode   = 'A'.       " Column and row selection
  gs_layo-stylefname = 'CELLTAB'.
  gs_layo-cwidth_opt = 'X'.

ENDFORM.                    " SET_LVC_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  Create_ck40n_costing
*&---------------------------------------------------------------------*
*       Excute Costing Run
*----------------------------------------------------------------------*
FORM create_ck40n_costing.
  PERFORM create_costing_header.

  IF sy-subrc = 0.
    MESSAGE s000 WITH 'Created costing Run.'.
  ENDIF.

* Check updated costing information
  IF NOT gt_out[] IS INITIAL.
    PERFORM get_gt_keko.
  ENDIF.

  DATA l_idx TYPE sytabix.

  CLEAR l_idx.

  LOOP AT gt_out.
    l_idx = sy-tabix.
    PERFORM check_costed.
    MODIFY gt_out INDEX l_idx TRANSPORTING bwdat aldat cstat.
  ENDLOOP.

  PERFORM refresh_disp.

ENDFORM.                    " Create_ck40n_costing
*&---------------------------------------------------------------------*
*&      Form  REFRESH_DISP
*&---------------------------------------------------------------------*
*       Refresh ALV grid
*----------------------------------------------------------------------*
FORM refresh_disp.
  CALL METHOD g_grid->set_frontend_fieldcatalog
       EXPORTING
         it_fieldcatalog = gt_fcat.

  CALL METHOD g_grid->refresh_table_display.

  CALL METHOD cl_gui_cfw=>flush.

ENDFORM.                    " REFRESH_DISP
*&---------------------------------------------------------------------*
*&      Form  BDC_MAT_SELECTION
*&---------------------------------------------------------------------*
*       BDC Process for Material Selection
*----------------------------------------------------------------------*
FORM bdc_mat_selection.
  DATA: l_kalaid TYPE ck_kalaid,   " Costing Run
        l_flg(1) TYPE c,
        l_kalka TYPE ck_kalka.

  REFRESH: gt_sel_f, gt_sel_m.
  CLEAR l_kalka.

* Get selected rows
  CALL METHOD g_grid->get_selected_rows
              IMPORTING et_index_rows = gt_row
                        et_row_no = gt_roid.

  LOOP AT gt_row INTO gs_row.
    READ TABLE gt_out INDEX gs_row-index.

    IF sy-subrc = 0.
      IF gt_out-cnfg = 'X'.
        gt_sel_f-matnr = gt_out-matnr.
        APPEND gt_sel_f.
      ELSE.
        gt_sel_m-matnr = gt_out-matnr.
        APPEND gt_sel_m.
      ENDIF.

      l_kalka = gt_out-kalka.
    ENDIF.
  ENDLOOP.
*
  RANGES r_matnr FOR mara-matnr.

* Configurable
  REFRESH r_matnr.
  CLEAR l_flg.

  r_matnr-option = 'EQ'. r_matnr-sign = 'I'.
  LOOP AT gt_sel_f.
    r_matnr-low = gt_sel_f-matnr. APPEND r_matnr.
    l_flg = 'X'.
  ENDLOOP.

  IF l_flg = 'X'.
    CONCATENATE l_kalka 'F' INTO l_kalaid.
    SUBMIT saprck60
            WITH backgr    = 'X'
            WITH confmat   = 'X'
            WITH ck_matnr  IN r_matnr
            WITH noueb     = p_noueb
            WITH kalaid    = l_kalaid
            WITH kaladat   = gv_cstdt
         AND RETURN.
  ENDIF.

* Others
  REFRESH r_matnr.
  CLEAR l_flg.

  r_matnr-option = 'EQ'. r_matnr-sign = 'I'.
  LOOP AT gt_sel_m.
    r_matnr-low = gt_sel_m-matnr. APPEND r_matnr.
    l_flg = 'X'.
  ENDLOOP.

  IF l_flg = 'X'.
    CONCATENATE l_kalka 'M' INTO l_kalaid.
    SUBMIT saprck60
            WITH backgr    = 'X'
            WITH confmat   = ' '
            WITH ck_matnr  IN r_matnr
            WITH noueb     = p_noueb
            WITH kalaid    = l_kalaid
            WITH kaladat   = gv_cstdt
         AND RETURN.
  ENDIF.

ENDFORM.                         " BDC_MAT_SELECTION
*&---------------------------------------------------------------------*
*&      Form  CK40N
*&---------------------------------------------------------------------*
*       CK40N BDC
*----------------------------------------------------------------------*
FORM ck40n USING p_kalaid TYPE ck_kalaid.    " Costing Run
  DATA: l_cstdt(10),        " Costing Run Date
        l_valdt(10),        " Costing Run Date
        l_aldat(10).        " Costing Run Date

* Get costing version
  PERFORM get_costing_var.

  REFRESH: gt_bdc, gt_msg.
  CLEAR  : gt_bdc, gt_msg, l_cstdt, l_valdt.

  PERFORM convert_date USING: gv_cstdt CHANGING l_cstdt,
                              gv_valdt CHANGING l_valdt,
                              gv_aldat CHANGING l_aldat.

  PERFORM dynpro USING: 'X'  'SAPLCKCC01'      '2000',
                        ' '  'BDC_OKCODE'      '=CREATE',

                        'X'  'SAPLCKCC01'      '2000',
                        ' '  'BDC_OKCODE'      '=ENTR',
                        ' '  'KALA-KALAID'      p_kalaid,
                        ' '  'KALA-KALABEZ'    'ANDY',
                        ' '  'KALA-KALADAT'     l_cstdt,

                        ' '  'KALV-KLVAR'       gv_klvar,
                        ' '  'KALA-TVERS'       '01',
                        ' '  'KALA-KOKRS'       'H201',
                        ' '  'KALA-BUKRS'       'H201',
                        ' '  'KALA-UEBID'       'ZPC1',
                        ' '  'KALA-RFC_GROUP'   'PG_FI',

                        'X'  'SAPLCKCC01'       '2000',
                        ' '  'BDC_OKCODE'       '=BUCA',
                        ' '  'KALA-KADAT'        l_cstdt,
                        ' '  'KALA-BIDAT'        l_cstdt,
                        ' '  'KALA-ALDAT'        l_aldat,
                        ' '  'KALA-BWDAT'        l_valdt.

  CALL TRANSACTION 'CK40N'  USING         gt_bdc
                            OPTIONS FROM  gs_opt
                            MESSAGES INTO gt_msg.

  READ TABLE gt_msg WITH KEY msgtyp = 'S'
                             msgid = 'CKCC'
                             msgnr = '018'.

  IF sy-subrc = 0.
    MESSAGE s000 WITH 'Success Costion Run.'.
  ELSE.
    MESSAGE s000 WITH 'Fail Costion Run.'.
  ENDIF.

ENDFORM.                                                    " CK40N
*&---------------------------------------------------------------------*
*&      Form  INIT
*&---------------------------------------------------------------------*
FORM init.
  DATA lv_answer.

  CLEAR lv_answer.
  CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
       EXPORTING
            textline1      = 'Do you really want to delete '
            textline2      = 'the Roll-Up Data?'
            titel          = 'Check!'
            cancel_display = 'X'
       IMPORTING
            answer         = lv_answer.

  IF lv_answer = 'J'.
* LOCK status -> NO DELETE!!!

    LOOP AT gt_out WHERE lstat = space.
      DELETE FROM ztcou103
        WHERE kokrs = p_kokrs
          AND bdatj = p_year
          AND poper = p_poper
          AND kalka IN s_kalka
          AND artnr = gt_out-matnr.
    ENDLOOP.

    IF sy-subrc = 0.
      MESSAGE s000 WITH 'Delete costing result by the period.'.
    ENDIF.
  ENDIF.


ENDFORM.                    " INIT
*&---------------------------------------------------------------------*
*&      Form  create_costing_header
*&---------------------------------------------------------------------*
FORM create_costing_header.
  DATA: l_kalka TYPE ck_kalka,
        l_kalaid TYPE ck_kalaid.   " Costing Run

  CLEAR l_kalka.
  PERFORM chk_cnfg CHANGING l_kalka.

  IF gv_cnt_f > 0.
    CLEAR l_kalaid.
    CONCATENATE l_kalka 'F' INTO l_kalaid.

    PERFORM ck40n USING l_kalaid.
  ENDIF.

  IF gv_cnt_m > 0.
    CLEAR l_kalaid.
    CONCATENATE l_kalka 'M' INTO l_kalaid.

    PERFORM ck40n USING l_kalaid.
  ENDIF.

ENDFORM.                    " create_costing_header
*&---------------------------------------------------------------------*
*&      Form  GET_DATE
*&---------------------------------------------------------------------*
*  Get Valuation date, Qty structure date for Costing BDC
*&---------------------------------------------------------------------*
FORM get_date.
  CLEAR: gv_cstdt, gv_valdt.

  CONCATENATE p_year p_poper+1(2) '01' INTO gv_cstdt.

  IF p_valdt IS INITIAL.
    IF p_day1 = 'X'.
      CONCATENATE p_year p_poper+1(2) '01' INTO gv_valdt.
    ELSEIF p_day15 = 'X'.
      CONCATENATE p_year p_poper+1(2) '15' INTO gv_valdt.
    ELSEIF p_lday = 'X'.
      CALL FUNCTION 'LAST_DAY_IN_PERIOD_GET'
           EXPORTING
                i_gjahr = p_year
                i_periv = 'K0'
                i_poper = p_poper
           IMPORTING
                e_date  = gv_valdt.
    ENDIF.
  ELSE.
    gv_valdt = p_valdt.      " Valuation date
  ENDIF.

  IF p_aldat IS INITIAL.
    gv_aldat = gv_valdt.     " Same with val.date
  ELSE.
    gv_aldat = p_aldat.      " Qty structure date
  ENDIF.

ENDFORM.                    " GET_DATE
*&---------------------------------------------------------------------*
*&      Form  UPDATE_STAT
*&---------------------------------------------------------------------*
FORM update_stat.
  DATA: l_idx LIKE sy-tabix,
        l_kalaid TYPE ck_kalaid.

  PERFORM get_gt_keko.
  PERFORM get_rollup.

  LOOP AT gt_out.
    l_idx = sy-tabix.

    CLEAR l_kalaid.

    IF s_kalka-low <> 'M1'.
      PERFORM check_selected.
      PERFORM check_bom_exp.
    ENDIF.

    PERFORM check_costed.
    PERFORM check_roll_up.

    IF gt_out-lstat = 'X'.
      gv_rcnt = gv_rcnt + 1.
    ENDIF.

    MODIFY gt_out INDEX l_idx.
  ENDLOOP.

  PERFORM check_reg TABLES gt_out.

* Refresh ALV grid
  PERFORM refresh_disp.

ENDFORM.                    " UPDATE_STAT
*&---------------------------------------------------------------------*
*&      Form  CHECK_COSTED
*&---------------------------------------------------------------------*
*       Check Costed
*----------------------------------------------------------------------*
FORM check_costed.
  PERFORM check_warning.

  READ TABLE gt_keko WITH KEY matnr = gt_out-matnr BINARY SEARCH.

  IF sy-subrc = 0.
    IF gt_keko-feh_sta = 'KF'.
      gt_out-cstat = icon_led_red.
    ELSEIF gt_keko-feh_sta = 'KA' AND gt_keko-maxmsg = 'W'.
      gt_out-cstat = icon_led_yellow.
    ELSE.
      gt_out-cstat = icon_led_green.
    ENDIF.

    gt_out-bwdat = gt_keko-bwdat.
    gt_out-aldat = gt_keko-aldat.

  ELSE.

    gt_out-cstat = icon_dummy.

    SELECT SINGLE
    bwdat aldat INTO (gt_out-bwdat,gt_out-aldat)
    FROM kala
       WHERE kalaid  = gt_out-kalaid
         AND kaladat = gv_cstdt.

  ENDIF.

*    CLEAR L_KALAID.
*    CONCATENATE L_KALKA 'F' INTO L_KALAID.
*
*    SELECT COUNT( * ) INTO SY-DBCNT FROM KALA
*       WHERE KALAID  = L_KALAID
*         AND KALADAT = GV_CSTDT.
*
*
*    SELECT COUNT( * ) INTO SY-DBCNT FROM KALA
*       WHERE KALAID  = L_KALAID
*         AND KALADAT = GV_CSTDT.

ENDFORM.                    " CHECK_COSTED
*&---------------------------------------------------------------------*
*&      Form  CHECK_ROLL_UP
*&---------------------------------------------------------------------*
*       Check Roll-up
*----------------------------------------------------------------------*
FORM check_roll_up.
  READ TABLE gt_103 WITH KEY artnr = gt_out-matnr BINARY SEARCH.
  IF sy-subrc = 0.
    gt_out-rstat = 'X'.      " Roll-up
    gv_rcnt = gv_rcnt + 1.   " Count of Roll-up
  ENDIF.

ENDFORM.                    " CHECK_ROLL_UP
*&---------------------------------------------------------------------*
*&      Form  TOGGLE_LOCK
*&---------------------------------------------------------------------*
*       Change Lock Status
*----------------------------------------------------------------------*
FORM toggle_lock.
* Get selected rows
  CLEAR: gt_row[], gt_roid[].

  CALL METHOD g_grid->get_selected_rows
              IMPORTING et_index_rows = gt_row
                        et_row_no = gt_roid.

  LOOP AT gt_row INTO gs_row.
    READ TABLE gt_out INDEX gs_row-index.

    IF sy-subrc = 0.
      IF gt_out-lstat = space.
        IF gt_out-rstat = 'X'.
          gt_out-lstat = 'X'.
        ENDIF.
      ELSE.
        gt_out-lstat = ' '.
      ENDIF.

      MODIFY gt_out INDEX gs_row-index.
    ENDIF.
  ENDLOOP.

* Refresh ALV grid
  PERFORM refresh_disp.

ENDFORM.                    " TOGGLE_LOCK
*&---------------------------------------------------------------------*
*&      Form  GET_OTHERS
*&---------------------------------------------------------------------*
FORM get_others.
  IF NOT gt_out[] IS INITIAL.
*   Get Rollup Information
    PERFORM get_rollup.

*   Get Costing Run ID
    CLEAR gt_kala.
    REFRESH gt_kala.

    SELECT kalka kalaid INTO TABLE gt_kala
      FROM kala
     WHERE kaladat = gv_valdt
       AND spras = sy-langu
       AND kalka IN s_kalka.
  ENDIF.

ENDFORM.                    " GET_OTHERS
*&---------------------------------------------------------------------*
*&      Form  CONVERT_DATE
*&---------------------------------------------------------------------*
*       Convert date to external
*----------------------------------------------------------------------*
FORM convert_date  USING    f_date  LIKE sy-datum
                   CHANGING f_dtout TYPE char10.
  CALL FUNCTION 'CONVERT_DATE_TO_EXTERNAL'
       EXPORTING
            date_internal            = f_date
       IMPORTING
            date_external            = f_dtout
       EXCEPTIONS
            date_internal_is_invalid = 1
            OTHERS                   = 2.

ENDFORM.                    " CONVERT_DATE
*&---------------------------------------------------------------------*
*&      Form  GET_COSTING_VAR
*&---------------------------------------------------------------------*
*       Get costing version
*----------------------------------------------------------------------*
FORM get_costing_var.
*  CLEAR gv_klvar.
*
*  CASE gt_out-kalka+0(1).
*    WHEN 'U'.
*      gv_klvar = 'ZU01'.
*    WHEN 'M'.
*      gv_klvar = 'ZM01'.
*    WHEN 'B'.
*      gv_klvar = 'ZUBP'.
*  ENDCASE.

ENDFORM.                    " GET_COSTING_VAR
*&---------------------------------------------------------------------*
*&      Form  REORG
*&---------------------------------------------------------------------*
*       Reorganization
*----------------------------------------------------------------------*
FORM reorg.
  RANGES r_klvar FOR keko-klvar.     " Costing variant

  DATA: l_kalaid  TYPE ck_kalaid,    " Costing Run
        l_test    TYPE ck_test,      " Test
        l_listau  TYPE ck_protoco,   " with List
        l_kalka   TYPE ck_kalka.
  DATA: wa_jobcount LIKE  tbtcjob-jobcount,
        wa_jobname  LIKE  tbtcjob-jobname,
        wa_p_jobname LIKE  tbtcjob-jobname,
        $jobname LIKE  tbtcjob-jobname.

  CLEAR: r_klvar, l_kalaid, l_test, l_listau, l_kalka.

* Costing variant
  REFRESH r_klvar.

  r_klvar-sign = 'I'.
  r_klvar-option = 'EQ'.
  r_klvar-low = gv_klvar.
  APPEND r_klvar.
  CLEAR r_klvar.

* Costing Run
  CLEAR l_kalka.
  PERFORM chk_cnfg CHANGING l_kalka.

  IF gv_cnt_f > 0.
    CLEAR l_kalaid.
    CONCATENATE l_kalka 'F' INTO l_kalaid.

    CONCATENATE l_kalaid '_' gv_cstdt '_' 'REORG' INTO wa_jobname.
    PERFORM call_job_open USING wa_jobname wa_jobcount.

* by IG.MOON 10/11/2007 {
    SUBMIT saprckr1
       VIA JOB wa_jobname NUMBER wa_jobcount AND RETURN
                      WITH kalaid   = l_kalaid
                      WITH kaladat  = gv_cstdt
                      WITH maschin  = 'X'
                      WITH manuell  = 'X'
                      WITH mimeger  = 'X'
                      WITH ohmeger  = 'X'
                      WITH kaohref  = 'X'
                      WITH test     = ' '
                      WITH p_listau = 'X'.
    IF sy-subrc = 0.
      PERFORM call_job_close USING wa_jobname wa_jobcount wa_p_jobname.
      CLEAR wa_p_jobname.

      MESSAGE s000 WITH l_kalaid 'Reorganization job is scheduled.'.
    ENDIF.
* }

*    SELECT COUNT( * ) INTO SY-DBCNT FROM KALA
*       WHERE KALAID  = L_KALAID
*         AND KALADAT = GV_CSTDT.
*
*    IF SY-SUBRC = 0.
*      SUBMIT SAPRCK44 WITH KALAID  = L_KALAID
*                      WITH KALADAT = GV_CSTDT
*                      WITH BACKGR  = 'X'
*                  AND RETURN.
*    ENDIF.

    $jobname = wa_jobname.

  ENDIF.

  IF gv_cnt_m > 0.
    CLEAR l_kalaid.
    CONCATENATE l_kalka 'M' INTO l_kalaid.

    CONCATENATE l_kalaid '_' gv_cstdt '_' 'REORG' INTO wa_jobname.
    PERFORM call_job_open USING wa_jobname wa_jobcount.

    SUBMIT saprckr1
       VIA JOB wa_jobname NUMBER wa_jobcount AND RETURN
                    WITH kalaid   = l_kalaid
                    WITH kaladat  = gv_cstdt
                    WITH maschin  = 'X'
                    WITH manuell  = 'X'
                    WITH mimeger  = 'X'
                    WITH ohmeger  = 'X'
                    WITH kaohref  = 'X'
                    WITH test     = ' '
                    WITH p_listau = 'X'.
    IF sy-subrc = 0.
      IF gv_cnt_f > 0.
        wa_p_jobname = $jobname.
      ENDIF.
      PERFORM call_job_close USING wa_jobname wa_jobcount wa_p_jobname.
      CLEAR wa_p_jobname.
      MESSAGE s000 WITH 'Reorganization job is scheduled.'.
    ENDIF.

* by IG.MOON 10/11/2007 {
*    SELECT COUNT( * ) INTO SY-DBCNT FROM KALA
*       WHERE KALAID  = L_KALAID
*         AND KALADAT = GV_CSTDT.
*    IF SY-SUBRC = 0.
*      SUBMIT SAPRCK44 WITH KALAID  = L_KALAID
*                      WITH KALADAT = GV_CSTDT
*                      WITH BACKGR  = 'X'
*                  AND RETURN.
*    ENDIF.
* }
  ENDIF.

* reorganize other costing
*  TABLES TCK03.
*  RANGES LR_KLVAR FOR TCK03-KLVAR.
*  LR_KLVAR-OPTION = 'EQ'.
*  LR_KLVAR-SIGN   = 'I'.
*  SELECT * FROM TCK03 WHERE KALKA IN S_KALKA.
*    LR_KLVAR-LOW = TCK03-KLVAR. APPEND LR_KLVAR.
*  ENDSELECT.
*
*  CONCATENATE L_KALKA '_' GV_CSTDT '_' 'REORG' INTO WA_JOBNAME.
*  PERFORM CALL_JOB_OPEN USING WA_JOBNAME WA_JOBCOUNT.
*
*  SUBMIT SAPRCKR1
*     VIA JOB WA_JOBNAME NUMBER WA_JOBCOUNT AND RETURN
*                  WITH BUKRS    = P_KOKRS
*                  WITH P_KLVAR  IN LR_KLVAR
*                  WITH P_KADAT  = GV_CSTDT
*                  WITH MASCHIN  = 'X'
*                  WITH MANUELL  = 'X'
*                  WITH MIMEGER  = 'X'
*                  WITH OHMEGER  = 'X'
*                  WITH KAOHREF  = 'X'
*                  WITH TEST     = ' '
*                  WITH P_LISTAU = 'X'.
*  IF SY-SUBRC = 0.
*    PERFORM CALL_JOB_CLOSE USING WA_JOBNAME WA_JOBCOUNT WA_P_JOBNAME.
*    CLEAR WA_P_JOBNAME.
*    MESSAGE S000 WITH 'Reorganization job is scheduled.'.
*  ENDIF.

ENDFORM.                    " REORG
*&---------------------------------------------------------------------*
*&      Form  CHK_CNFG
*&---------------------------------------------------------------------*
*       Check Configurable count
*----------------------------------------------------------------------*
FORM chk_cnfg CHANGING p_kalka TYPE ck_kalka.
  CLEAR: gv_cnt_f, gv_cnt_m, gt_row[], gt_roid[], gs_row.

* Get seleted rows
  CALL METHOD g_grid->get_selected_rows
              IMPORTING et_index_rows = gt_row
                        et_row_no = gt_roid.

  LOOP AT gt_row INTO gs_row.
    READ TABLE gt_out INDEX gs_row-index.
    IF sy-subrc = 0.
      IF gt_out-cnfg = 'X'.
        gv_cnt_f = gv_cnt_f + 1.
      ELSE.
        gv_cnt_m = gv_cnt_m + 1.
      ENDIF.

      p_kalka = gt_out-kalka.

    ENDIF.
  ENDLOOP.

ENDFORM.                    " CHK_CNFG
*&---------------------------------------------------------------------*
*&      Form  BOM_EXPL
*&---------------------------------------------------------------------*
*       BOM explosion
*      : Costing Run for Material: Breakdown of Preselection Criteria
*----------------------------------------------------------------------*
FORM bom_expl.
  DATA: l_kalaid TYPE ck_kalaid,
        l_kalka  TYPE ck_kalka.

  CLEAR: l_kalaid, l_kalka.

* for get costing run ID
  PERFORM chk_cnfg CHANGING l_kalka.

  IF gv_cnt_f > 0.
    CONCATENATE l_kalka 'F' INTO l_kalaid.

    SUBMIT saprck62 WITH kalaid   = l_kalaid
                    WITH kaladat  = gv_cstdt
                    WITH backgr   = 'X' AND RETURN.

  ENDIF.

  IF gv_cnt_m > 0.
    CONCATENATE l_kalka 'M' INTO l_kalaid.

    SUBMIT saprck62 WITH kalaid   = l_kalaid
                    WITH kaladat  = gv_cstdt
                    WITH backgr   = 'X' AND RETURN.

  ENDIF.

ENDFORM.                    " BOM_EXPL
*&---------------------------------------------------------------------*
*&      Form  COSTING
*&---------------------------------------------------------------------*
*       Costing Run for Material: Costing by Costing
*----------------------------------------------------------------------*
FORM costing.
  DATA: l_kalaid  TYPE ck_kalaid,
        l_servnum TYPE ck_servnum,
        l_kalka   TYPE ck_kalka.

  CLEAR: l_servnum, l_kalka.

* for get costing run ID
  PERFORM chk_cnfg CHANGING l_kalka.

*MIP first
  IF gv_cnt_m > 0.
    CLEAR l_kalaid.
    CONCATENATE l_kalka 'M' INTO l_kalaid.

    SUBMIT saprck10 WITH kalaid   = l_kalaid
                    WITH kaladat  = gv_cstdt
                    WITH onlykf   = onlykf
                    WITH llc_log  = 'X'
                    WITH parallel = 'X'
                    WITH servnum  = p_servn
                    WITH protdr   = 'X'
                    WITH backgr   = 'X'
                    AND RETURN.
  ENDIF.

*Vehicle next
  IF gv_cnt_f > 0.
    CLEAR l_kalaid.
    CONCATENATE l_kalka 'F' INTO l_kalaid.

    SUBMIT saprck10 WITH kalaid   = l_kalaid
                    WITH kaladat  = gv_cstdt
                    WITH onlykf   = onlykf
                    WITH llc_log  = 'X'
                    WITH parallel = 'X'
                    WITH servnum  = p_servn
                    WITH protdr   = 'X'
                    WITH backgr   = 'X'
                    AND RETURN.

  ENDIF.

ENDFORM.                    " COSTING
*&---------------------------------------------------------------------*
*&      Form  SAVE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM save_data.
  exit.

*  DATA l_cnt TYPE i.
*
*  CLEAR l_cnt.
*
*  LOOP AT gt_out.
*    UPDATE ztcou100 SET rstat = gt_out-rstat
*                        lstat = gt_out-lstat
*                        bwdat = gv_valdt
*                        aldat = gt_out-aldat
*                        aedat = sy-datum
*                        aenam = sy-uname
*      WHERE kokrs = p_kokrs
*        AND kalka = gt_out-kalka
*        AND bdatj = gt_out-bdatj
*        AND poper = gt_out-poper
*        AND matnr = gt_out-matnr
*        AND verid = gt_out-verid.
*
*    IF sy-subrc = 0.
*      l_cnt = l_cnt + 1.
*    ENDIF.
*  ENDLOOP.
*
*  MESSAGE s000 WITH 'Saved' l_cnt 'records.'.

ENDFORM.                    " SAVE_DATA
*&---------------------------------------------------------------------*
*&      Form  save_component
*&---------------------------------------------------------------------*
*  Execute Costing Result program ZACOU102
*----------------------------------------------------------------------*
FORM save_component.
  DATA: wa_jobcount LIKE  tbtcjob-jobcount,
        wa_jobname  LIKE  tbtcjob-jobname.

  READ TABLE s_kalka INDEX 1.
  CONCATENATE s_kalka-low '_' gv_cstdt '_' 'Component' INTO wa_jobname.
  PERFORM call_job_open USING wa_jobname wa_jobcount.

  SUBMIT zacou102
       VIA JOB wa_jobname NUMBER wa_jobcount AND RETURN
                  WITH p_kokrs = p_kokrs
                  WITH p_year  = p_year
                  WITH p_poper = p_poper
                  WITH s_kalka IN s_kalka
                  WITH p_abs = space
                  WITH p_expt = space
                  WITH p_batch = 'X'.

  IF sy-subrc = 0.
    PERFORM call_job_close USING wa_jobname wa_jobcount ''.
    MESSAGE s000 WITH 'Job is scheduled.'.
  ENDIF.

ENDFORM.                    " save_component
*&---------------------------------------------------------------------*
*&      Form  ROLL_UP
*&---------------------------------------------------------------------*
FORM roll_up.
  DATA: wa_jobcount LIKE  tbtcjob-jobcount,
        wa_jobname  LIKE  tbtcjob-jobname.

  DATA l_cnt TYPE i.

* PERFORM GET_MATNR_RANGES.
* Submit Rollup program ZACOU103
  READ TABLE s_kalka INDEX 1.
  CONCATENATE s_kalka-low '_' gv_cstdt '_' 'Rollup' INTO wa_jobname.
  PERFORM call_job_open USING wa_jobname wa_jobcount.

  SUBMIT zacou103
*       VIA JOB WA_JOBNAME NUMBER WA_JOBCOUNT AND RETURN
                  WITH p_kokrs = p_kokrs
                  WITH p_year  = p_year
                  WITH p_poper = p_poper
*                 WITH S_MATNR IN R_MATNR
                  WITH p_kalka = s_kalka-low.
*                  WITH P_SMALL = 'X'.

  IF sy-subrc = 0.
    PERFORM call_job_close USING wa_jobname wa_jobcount ' '.
    MESSAGE s000 WITH 'Job is scheduled.'.

    CLEAR: gt_row[], gt_roid[], l_cnt.

* Update table ZTCOU100
    CALL METHOD g_grid->get_selected_rows
                IMPORTING et_index_rows = gt_row
                          et_row_no = gt_roid.

    LOOP AT gt_row INTO gs_row.
      READ TABLE gt_out INDEX gs_row-index.

      IF sy-subrc = 0.
        UPDATE ztcou100 SET lstat = gt_out-lstat
                            bwdat = gv_valdt
                            aldat = gt_out-aldat
                            aedat = sy-datum
                            aenam = sy-uname
          WHERE kokrs = p_kokrs
            AND kalka = gt_out-kalka
            AND bdatj = gt_out-bdatj
            AND poper = gt_out-poper
            AND matnr = gt_out-matnr
            AND verid = gt_out-verid.

        IF sy-subrc = 0.
          l_cnt = l_cnt + 1.
        ENDIF.

      ENDIF.

    ENDLOOP.

    MESSAGE s000 WITH 'Date is updated ' l_cnt 'records.'.

* Refresh
    PERFORM get_gt_out.
    PERFORM refresh_disp.

  ENDIF.

ENDFORM.                    " ROLL_UP
*&---------------------------------------------------------------------*
*&      Form  GET_MATNR_RANGES
*&---------------------------------------------------------------------*
*       Get Material Ranges
*----------------------------------------------------------------------*
FORM get_matnr_ranges.
  CLEAR: gt_row[], gt_roid[], r_matnr[].

* Get selected rows
  CALL METHOD g_grid->get_selected_rows
              IMPORTING et_index_rows = gt_row
                        et_row_no = gt_roid.

  r_matnr-sign = 'I'.
  r_matnr-option = 'EQ'.

  LOOP AT gt_row INTO gs_row.
    READ TABLE gt_out INDEX gs_row-index.

    IF sy-subrc = 0.
      r_matnr-low = gt_out-matnr.
      APPEND r_matnr.
    ENDIF.
  ENDLOOP.

  CLEAR r_matnr.

ENDFORM.                    " GET_MATNR_RANGES
*&---------------------------------------------------------------------*
*&      Form  CHECK_SELECTED
*&---------------------------------------------------------------------*
*       Check Seleted
*----------------------------------------------------------------------*
FORM check_selected.
  DATA: l_kalaid TYPE ck_kalaid,
        l_stat   TYPE ck_feh_sta.

  IF gt_out-cnfg = 'X'.
    CONCATENATE gv_klvar 'F' INTO l_kalaid.
  ELSE.
    CONCATENATE gv_klvar 'M' INTO l_kalaid.
  ENDIF.

  SELECT SINGLE feh_status INTO l_stat
    FROM kalm
   WHERE kalaid  = l_kalaid
     AND kaladat = gv_cstdt
     AND matnr   = gt_out-matnr
     AND ( feh_status LIKE 'S%' OR feh_status LIKE 'K%' ).

  IF sy-subrc = 0.
    IF l_stat = 'SF'.
      gt_out-sstat = icon_led_red.
    ELSE.
      gt_out-sstat = icon_led_green.
    ENDIF.
  ELSE.
    gt_out-sstat = icon_dummy.
  ENDIF.

ENDFORM.                    " CHECK_SELECTED
*&---------------------------------------------------------------------*
*&      Form  check_bom_exp
*&---------------------------------------------------------------------*
FORM check_bom_exp.
  DATA: l_kalaid     TYPE ck_kalaid,
        w_kals       LIKE kals,
        l_cmf_nr     TYPE cmf_nr.


  IF gt_out-cnfg = 'X'.
    CONCATENATE gv_klvar 'F' INTO l_kalaid.
  ELSE.
    CONCATENATE gv_klvar 'M' INTO l_kalaid.
  ENDIF.

  SELECT SINGLE * INTO w_kals FROM kals
   WHERE kalaid  = l_kalaid
     AND kaladat = gv_cstdt.

  IF sy-subrc = 0 AND w_kals-aufgeloest = 'X'.

    SELECT MAX( cmf_nr ) INTO l_cmf_nr
      FROM kalf
     WHERE kalaid  = l_kalaid
       AND kaladat = gv_cstdt
       AND histtxt LIKE '%EXP%'.

    SELECT COUNT( * ) INTO sy-index
      FROM cmfp
     WHERE aplid = 'CK'
       AND nr    = l_cmf_nr
       AND msgty = 'W'
       AND msgv1 = gt_out-matnr.

    IF sy-subrc = 0.
      gt_out-bstat = icon_led_yellow.
    ELSE.
      gt_out-bstat = icon_led_green.
    ENDIF.
  ELSE.
    gt_out-bstat = icon_dummy.
  ENDIF.

ENDFORM.                    " check_bom_exp
*&---------------------------------------------------------------------*
*&      Form  HOTSPOT_CLICK
*&---------------------------------------------------------------------*
*       Setting of Hotspot click
*        : Display Material(MM03) when click material
*----------------------------------------------------------------------*
*      -->P_ROW_ID     Row ID
*      -->P_COLUMN_ID  Column ID
*----------------------------------------------------------------------*
FORM hotspot_click USING  p_row_id
                          p_column_id.
  READ TABLE gt_out INDEX p_row_id.

  IF sy-subrc = 0.
    CASE p_column_id.
      WHEN 'MATNR'.
        SET PARAMETER ID 'MAT'  FIELD gt_out-matnr.
        CALL TRANSACTION 'MM03' AND SKIP FIRST SCREEN.

      WHEN 'CSTAT'.
        IF gt_out-cstat = icon_led_yellow. "ICON_BW_DATA_MARTS.
          PERFORM dosp_log.
        ENDIF.
    ENDCASE.
  ENDIF.

ENDFORM.                    " HOTSPOT_CLICK
*&---------------------------------------------------------------------*
*&      Form  DOSP_LOG
*&---------------------------------------------------------------------*
*       Display Costing Run Log: warning only
*----------------------------------------------------------------------*
FORM dosp_log.
*  PERFORM CHECK_WARNING.

  CALL FUNCTION 'CK_PROCESS_DISPLAY_LOG'
       EXPORTING
            i_kalaid   = gt_out-kalaid
            i_kaladat  = gv_cstdt
            i_kalastep = 'KA'
       EXCEPTIONS
            not_found  = 1
            OTHERS     = 2.

ENDFORM.                    " DOSP_LOG
*&---------------------------------------------------------------------*
*&      Form  CHECK_WARNING
*&---------------------------------------------------------------------*
*       Check Costing Warning
*----------------------------------------------------------------------*
FORM check_warning.
  DATA: l_cmf_nr TYPE cmf_nr,
        l_msgv1  TYPE symsgv.

  CLEAR l_cmf_nr.
  SELECT MAX( cmf_nr ) INTO l_cmf_nr
    FROM kalf
   WHERE kalaid  = gt_out-kalaid
     AND kaladat = gv_cstdt.

  SELECT SINGLE msgv1 INTO l_msgv1
    FROM cmfp
   WHERE nr = l_cmf_nr
     AND msgty = 'W'.

  IF sy-subrc = 0.
  ENDIF.

ENDFORM.                    " CHECK_WARNING
*&---------------------------------------------------------------------*
*&      Form  GET_ROLLUP
*&---------------------------------------------------------------------*
*       Get Rolluped Data from table ZTCOU103
*----------------------------------------------------------------------*
FORM get_rollup.
  CLEAR gt_103.
  REFRESH gt_103.

*** : KDM on 11/01/2011 for performance
  IF gt_out[] IS NOT INITIAL.
    SELECT DISTINCT artnr INTO TABLE gt_103
      FROM ztcou103
      FOR ALL ENTRIES IN gt_out
     WHERE kokrs = gt_out-kokrs
       AND bdatj = gt_out-bdatj
       AND poper = gt_out-poper
       AND kalka = gt_out-kalka
       AND ver   = '00'
       AND artnr = gt_out-matnr.

    SORT gt_103 BY artnr.
  ENDIF.

ENDFORM.                    " GET_ROLLUP
*&---------------------------------------------------------------------*
*&      Form  GET_GT_OUT
*&---------------------------------------------------------------------*
FORM get_gt_out.
  DATA l_index TYPE sytabix.
  DATA $subrc LIKE sy-subrc.
  DATA $ix LIKE sy-tabix.

  REFRESH gt_out.
  CLEAR: gt_out, gv_tcnt, gv_ccnt, gv_rcnt, gv_lcnt, l_index.

  IF p_rg EQ true.
    IF confmat = 'X'.
      SELECT kokrs kalka bdatj poper matnr
             verid werks cnfg verid stlan crint crext lstat
        INTO CORRESPONDING FIELDS OF TABLE gt_out
        FROM ztcou100
       WHERE kokrs = p_kokrs
         AND kalka IN s_kalka
         AND bdatj = p_year
         AND poper = p_poper
         AND cnfg  = 'X'
         AND matnr IN s_matnr
         and ( intrn = space or intrn is null ).
    ELSEIF excconf = 'X'.
      SELECT kokrs kalka bdatj poper matnr
             verid werks cnfg verid stlan crint crext lstat
        INTO CORRESPONDING FIELDS OF TABLE gt_out
        FROM ztcou100
       WHERE kokrs = p_kokrs
         AND kalka IN s_kalka
         AND bdatj = p_year
         AND poper = p_poper
         AND cnfg  = ' '
         AND matnr IN s_matnr
         and ( intrn = space or intrn is null ).
    ELSE.
      SELECT kokrs kalka bdatj poper matnr
             verid werks cnfg verid stlan crint crext lstat
        INTO CORRESPONDING FIELDS OF TABLE gt_out
        FROM ztcou100
       WHERE kokrs = p_kokrs
         AND kalka IN s_kalka
         AND bdatj = p_year
         AND poper = p_poper
         AND matnr IN s_matnr
         and ( intrn = space or intrn is null ).
    ENDIF.

    $subrc = sy-subrc.
    gt_out-reg = 'X'.
    MODIFY  gt_out TRANSPORTING reg WHERE reg EQ space.
  ELSE.

    SELECT kokrs kalka matnr
           verid werks verid stlan kadky
      INTO CORRESPONDING FIELDS OF TABLE gt_out
      FROM keko
     WHERE kokrs = p_kokrs
       AND kalka IN s_kalka
       AND kadky = gv_cstdt
       AND stlan NE space
       AND matnr IN s_matnr.

    $subrc = sy-subrc.
    PERFORM check_reg TABLES gt_out.

  ENDIF.


  IF $subrc = 0.

    IF confall EQ space.
      LOOP AT gt_out.
        $ix = sy-tabix.
        IF confmat EQ 'X' AND gt_out-cnfg NE 'X'.
          DELETE gt_out INDEX $ix.
          CONTINUE.
        ENDIF.
        IF excconf EQ 'X' AND gt_out-cnfg = 'X'.
          DELETE gt_out INDEX $ix.
          CONTINUE.
        ENDIF.
      ENDLOOP.
    ENDIF.

    PERFORM get_others.

    LOOP AT gt_out.
      l_index = sy-tabix.
      gv_tcnt = gv_tcnt + 1.

      IF gt_out-cnfg = 'X'.
        CONCATENATE gt_out-kalka 'F' INTO gt_out-kalaid.
      ELSE.
        CONCATENATE gt_out-kalka 'M' INTO gt_out-kalaid.
      ENDIF.


*     Check Seleted
      PERFORM check_selected.

*     Check BOM exp.
      PERFORM check_bom_exp.

*     Check Costed
*      PERFORM GET_COST_STATUS.
      PERFORM check_costed.


*     Check Roll-up
      PERFORM check_roll_up.

*     Check Lock
      IF gt_out-lstat = 'X'.
        gv_lcnt = gv_lcnt + 1.
      ENDIF.

      IF gt_out-rstat = 'X'.
        gt_out-rstat1 = icon_led_green.
      ELSE.
        gt_out-rstat1 = icon_dummy.
      ENDIF.

      MODIFY gt_out INDEX l_index
         TRANSPORTING sstat bstat cstat rstat rstat1 kalaid bwdat aldat.
    ENDLOOP.

  ENDIF.

  IF gt_out[] IS INITIAL.
    MESSAGE s000 WITH 'Data not found.'.
  ENDIF.

ENDFORM.                    " GET_GT_OUT
*&---------------------------------------------------------------------*
*&      Form  GET_GT_KEKO
*&---------------------------------------------------------------------*
*       Get Costing Information
*----------------------------------------------------------------------*
FORM get_gt_keko.
  CLEAR gt_keko.
  REFRESH gt_keko.

  SELECT keko~kalnr keko~matnr verid kokrs kalka tvers
         kaladat feh_sta maxmsg kalaid bwdat aldat
    INTO TABLE gt_keko
    FROM ckmlhd
     INNER JOIN keko
        ON keko~bzobj = '0'
       AND keko~kalnr = ckmlhd~kalnr
       AND keko~kalka = gt_out-kalka
     FOR ALL entries IN gt_out
   WHERE ckmlhd~matnr = gt_out-matnr
     AND ckmlhd~bwkey = gt_out-werks
     AND kokrs = gt_out-kokrs
     AND tvers = gc_tvers
     AND bdatj = p_year
     AND poper = p_poper.

  SORT gt_keko BY matnr.

ENDFORM.                    " GET_GT_KEKO
*&---------------------------------------------------------------------*
*&      Form  CALL_JOB_OPEN
*&---------------------------------------------------------------------*
FORM call_job_open USING p_jobname p_jobcount.
  CALL FUNCTION 'JOB_OPEN'
    EXPORTING
*      DELANFREP              = ' '
*      JOBGROUP               = ' '
      jobname                = p_jobname
*      SDLSTRTDT              = NO_DATE
*      SDLSTRTTM              = NO_TIME
    IMPORTING
      jobcount               = p_jobcount
    EXCEPTIONS
      cant_create_job        = 1
      invalid_job_data       = 2
      jobname_missing        = 3
      OTHERS                 = 4.
*  IF SY-SUBRC <> 0.
*    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*  ENDIF.
ENDFORM.                    " CALL_JOB_OPEN
*&---------------------------------------------------------------------*
*&      Form  CALL_JOB_SUBMIT
*&---------------------------------------------------------------------*
FORM call_job_submit USING p_jobname
                           p_report
                           p_jobcount.

  CALL FUNCTION 'JOB_SUBMIT'
    EXPORTING
*      ARCPARAMS                         =
      authcknam                         = sy-uname
*      COMMANDNAME                       = ' '
*      OPERATINGSYSTEM                   = ' '
*      EXTPGM_NAME                       = ' '
*      EXTPGM_PARAM                      = ' '
*      EXTPGM_SET_TRACE_ON               = ' '
*      EXTPGM_STDERR_IN_JOBLOG           = 'X'
*      EXTPGM_STDOUT_IN_JOBLOG           = 'X'
*      EXTPGM_SYSTEM                     = ' '
*      EXTPGM_RFCDEST                    = ' '
*      EXTPGM_WAIT_FOR_TERMINATION       = 'X'
      jobcount                          = p_jobcount
      jobname                           = p_jobname
*      LANGUAGE                          = SY-LANGU
*      PRIPARAMS                         = ' '
      report                            = p_report
*      VARIANT                           = ' '
*    IMPORTING
*      STEP_NUMBER                       =
      EXCEPTIONS
      bad_priparams                     = 1
      bad_xpgflags                      = 2
      invalid_jobdata                   = 3
      jobname_missing                   = 4
      job_notex                         = 5
      job_submit_failed                 = 6
      lock_failed                       = 7
      program_missing                   = 8
      prog_abap_and_extpg_set           = 9
      OTHERS                            = 10.

*  IF SY-SUBRC <> 0.
*    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*  ENDIF.
ENDFORM.                    " CALL_JOB_SUBMIT
*&---------------------------------------------------------------------*
*&      Form  CALL_JOB_CLOSE
*&---------------------------------------------------------------------*
FORM call_job_close USING p_jobname p_jobcount p_p_job.

  DATA : $flag(1),
         $flag_st(1).

  IF p_p_job NE space.
    $flag = space.
    $flag_st = 'X'.
  ELSE.
    $flag = 'X'.
    $flag_st = ' '.
  ENDIF.

  CALL FUNCTION 'JOB_CLOSE'
    EXPORTING
*   AT_OPMODE                         = ' '
*   AT_OPMODE_PERIODIC                = ' '
*   CALENDAR_ID                       = ' '
*   EVENT_ID                          = ' '
*   EVENT_PARAM                       = ' '
*   EVENT_PERIODIC                    = ' '
      jobcount                          = p_jobcount
      jobname                           = p_jobname
*   LASTSTRTDT                        = NO_DATE
*   LASTSTRTTM                        = NO_TIME
*   PRDDAYS                           = 0
*   PRDHOURS                          = 0
*   PRDMINS                           = 0
*   PRDMONTHS                         = 0
*   PRDWEEKS                          = 0
    predjob_checkstat                 = $flag_st
*   PRED_JOBCOUNT                     = ' '
    pred_jobname                      = p_p_job
*   SDLSTRTDT                         = NO_DATE
*   SDLSTRTTM                         = NO_TIME
*   STARTDATE_RESTRICTION             = BTC_PROCESS_ALWAYS
     strtimmed                         = $flag "'X'  "IMMEDIATE
*   TARGETSYSTEM                      = ' '
*   START_ON_WORKDAY_NOT_BEFORE       = SY-DATUM
*   START_ON_WORKDAY_NR               = 0
*   WORKDAY_COUNT_DIRECTION           = 0
*   RECIPIENT_OBJ                     =
*   TARGETSERVER                      = ' '
*   DONT_RELEASE                      = ' '
* IMPORTING
*   JOB_WAS_RELEASED                  =
   EXCEPTIONS
     cant_start_immediate              = 1
     invalid_startdate                 = 2
     jobname_missing                   = 3
     job_close_failed                  = 4
     job_nosteps                       = 5
     job_notex                         = 6
     lock_failed                       = 7
     OTHERS                            = 8.

  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " CALL_JOB_CLOSE
*&---------------------------------------------------------------------*
*&      Form  GET_COST_STATUS
*&---------------------------------------------------------------------*
*       Get Cost Staus
*----------------------------------------------------------------------*
FORM get_cost_status.
  CLEAR gt_keko.

  SELECT SINGLE feh_sta maxmsg bwdat aldat
    INTO (gt_keko-feh_sta, gt_keko-maxmsg, gt_out-bwdat, gt_out-aldat)
    FROM ckmlhd
     INNER JOIN keko
        ON keko~bzobj = '0'
       AND keko~kalnr = ckmlhd~kalnr
       AND keko~kalka = gt_out-kalka
   WHERE ckmlhd~matnr = gt_out-matnr
     AND ckmlhd~bwkey = gt_out-werks
     AND kokrs = gt_out-kokrs
     AND tvers = gc_tvers
     AND bdatj = p_year
     AND poper = p_poper.

* UD1K941202 - by IG.MOON 8/2/2007 {
  IF sy-subrc NE 0.
    gt_out-cstat = icon_dummy.
  ELSE.
* }
    IF gt_keko-feh_sta = 'KF'.
      gt_out-cstat = icon_led_red.
    ELSEIF gt_keko-feh_sta = 'KA' AND gt_keko-maxmsg = 'W'.
      gt_out-cstat = icon_led_yellow.
    ELSE.
      gt_out-cstat = icon_led_green.
    ENDIF.
  ENDIF.
ENDFORM.                    " GET_COST_STATUS
