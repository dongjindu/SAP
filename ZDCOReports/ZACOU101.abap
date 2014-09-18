*----------------------------------------------------------------------
* Program ID        : ZACOU101
* Title             : [CO] Check Costing Status
* Created on        : 08/04/2006
* Created by        : Michelle Jeong
* Specifications By : Andy Choi
* Description       : Check costing status &
*                     excute BDC for Costing run &
*                     excute Material selection &
*                     excute Material master update.
*----------------------------------------------------------------------
REPORT zacou101_test NO STANDARD PAGE HEADING MESSAGE-ID zmco.

INCLUDE zacoui00.
INCLUDE zacou101_top.

* by ig.moon 5/12/2008 {
CONSTANTS:  false VALUE ' ',
            true  VALUE 'X'.
DEFINE __cls.                          " clear & refresh
  clear &1.refresh &1.
END-OF-DEFINITION.

DATA  $gt_out LIKE gt_out OCCURS 0 WITH HEADER LINE.
DATA $$gt_out LIKE gt_out OCCURS 0 WITH HEADER LINE.
DATA: BEGIN OF gt_rsparams OCCURS 0.
        INCLUDE STRUCTURE rsparams.
DATA: END OF gt_rsparams.

DEFINE __add_sel_tab .
  gt_rsparams-selname = &1.
  gt_rsparams-kind    = &2.
  gt_rsparams-sign    = ' '.
  gt_rsparams-option  = ' '.
  gt_rsparams-low     = &3.
  clear gt_rsparams-high.
  append gt_rsparams.
END-OF-DEFINITION.

DEFINE __add_sel_tab_mat .
  gt_rsparams-selname = &1.
  gt_rsparams-kind    = &2.
  gt_rsparams-sign    = &3.
  gt_rsparams-option  = &4.
  gt_rsparams-low     = &5.
  gt_rsparams-high    = ' '.
  append gt_rsparams.
END-OF-DEFINITION.


* }

*----------------------------------------------------------------------
* Selection-Screen
*----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK b0 WITH FRAME TITLE text-001.

PARAMETERS: p_rg AS CHECKBOX DEFAULT 'X'.

PARAMETER p_kokrs LIKE keko-kokrs OBLIGATORY MEMORY ID cac.
SELECT-OPTIONS s_kalka FOR ztcou100-kalka NO INTERVALS NO-EXTENSION
                      OBLIGATORY MEMORY ID kka.
PARAMETERS: p_year  LIKE keko-bdatj OBLIGATORY MEMORY ID bdtj,
            p_poper LIKE keko-poper OBLIGATORY MEMORY ID popr.
SELECTION-SCREEN END OF BLOCK b0.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-002.
PARAMETERS: p_day1  RADIOBUTTON GROUP r1,
            p_day15 RADIOBUTTON GROUP r1,
            p_lday  RADIOBUTTON GROUP r1 DEFAULT 'X'.
PARAMETERS: p_valdt LIKE keko-bwdat,
            p_aldat LIKE keko-aldat.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-004.
*PARAMETERS: confmat TYPE ck_select_confmat_only,
*            excconf AS CHECKBOX.  "exclude conf.material

PARAMETERS: confall RADIOBUTTON GROUP r2,
            confmat RADIOBUTTON GROUP r2,
            excconf RADIOBUTTON GROUP r2.
SELECT-OPTIONS: s_matnr FOR ztcou100-matnr.
*S_CNFG  FOR ZTCOU100-CNFG default 'X',

SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME TITLE text-005.
PARAMETERS: p_noueb AS CHECKBOX DEFAULT 'X'.
PARAMETERS: onlykf LIKE kala-onlykf DEFAULT 'X',
            p_servn TYPE ck_servnum DEFAULT '7'.
SELECTION-SCREEN END OF BLOCK b4.
PARAMETERS: p_m1bat AS CHECKBOX.

INCLUDE zacou101_f01.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_kalka-low.
  PERFORM popup_kalka USING s_kalka-low
                            'S_KALKA-LOW'.

*----------------------------------------------------------------------*
* Start of selection
*----------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM get_data.

  READ TABLE s_kalka INDEX 1.
  IF p_m1bat = 'X' AND s_kalka-low = 'M1'.
    LOOP AT gt_out.
      MESSAGE s000 WITH 'Costing...' gt_out-matnr.
      PERFORM bdc_ck11 CHANGING sy-subrc.
    ENDLOOP.

  ELSE.
    CALL SCREEN 100.
  ENDIF.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS '100'.
  SET TITLEBAR '100'.

ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  CREATE_ALV_CONTROL  OUTPUT
*&---------------------------------------------------------------------*
MODULE create_alv_control OUTPUT.
  IF g_custom_container IS INITIAL.
*   Create object
    PERFORM create_object.

*   Exclude toolbar
    PERFORM exclude_functions IN PROGRAM zacou100
                        USING 'GT_EXCLUDE'.

*   Create field category
    PERFORM create_field_category.

*   Setting for layout
    PERFORM set_lvc_layout.

*  Setting for event
    CREATE OBJECT g_event_receiver.
    SET HANDLER g_event_receiver->handle_hotspot_click FOR g_grid.

*   Define variant
    gs_variant-report = sy-repid.

*   Display alv grid
    CALL METHOD g_grid->set_table_for_first_display
         EXPORTING is_layout            = gs_layo
                   it_toolbar_excluding = gt_exclude
                   i_save               = gc_var_save
                   is_variant           = gs_variant
         CHANGING  it_outtab            = gt_out[]
                   it_fieldcatalog      = gt_fcat[].

  ENDIF.

ENDMODULE.                 " CREATE_ALV_CONTROL  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
*allow below function if not locked...
  ok_code = sy-ucomm.
  CLEAR sy-ucomm.

  CASE ok_code.
    WHEN 'BACK' OR 'CANC'.
      CLEAR: gv_lock, gv_but.
      LEAVE TO SCREEN 0.

    WHEN 'EXIT'.
      CLEAR: gv_lock, gv_but.
      LEAVE PROGRAM.

*   Create costing run: Excute Costing Run BDC
    WHEN 'CRUN'.
      IF s_kalka-low <> 'M1'.
        PERFORM create_ck40n_costing.
      ENDIF.

*   Fill Material Selection
    WHEN 'MSEL'.
      IF s_kalka-low <> 'M1'.
        PERFORM bdc_mat_selection.
      ENDIF.

*   BOM explosion
*   : Costing Run for Material: Breakdown of Preselection Criteria
    WHEN 'BOM'.
      IF s_kalka-low <> 'M1'.
        PERFORM bom_expl.
      ENDIF.

*   Costing
*   : Costing Run for Material: Costing by Costing
    WHEN 'COST'.
*     module bom -> ck11
      IF s_kalka-low = 'M1'.
        PERFORM ck11_costing.
      ELSE.
        PERFORM costing.
      ENDIF.

* CK11 costing...
    WHEN 'RCST'.
      PERFORM ck11_costing.

*   Update Status: Read current status
    WHEN 'UPST'.
      PERFORM update_stat.

*   LOCK / Unlock: Change Lock Status
    WHEN 'LOCK'.
      PERFORM toggle_lock.

*   Reorganization
    WHEN 'REORG'.
      IF s_kalka-low <> 'M1'.
        PERFORM reorg.
      ELSE.
        PERFORM reorg_module.
      ENDIF.

*   Initialization New
    WHEN 'INIT'.
      IF s_kalka-low EQ 'M1'.
      ELSE.
        PERFORM init_ck40.
      ENDIF.

*   refresh, delete 103
    WHEN 'REF'.
      PERFORM init.

*   Save??? (why need?)
    WHEN 'SAVE'.
      PERFORM save_data.

*   Execute Costing result: program ZACOU102
    WHEN 'S102'.
      PERFORM save_component.

*   Execute Rollup: program ZACOU103
    WHEN 'S103'.
      PERFORM roll_up.

    WHEN 'CALL'.

      __cls $$gt_out.

      CLEAR : gv_cnt_f, gv_cnt_m.

      PERFORM get_selected_rows TABLES $gt_out.
      LOOP AT $gt_out.
        IF $gt_out-cnfg = 'X'.
          gv_cnt_f = gv_cnt_f + 1.
        ELSE.
          gv_cnt_m = gv_cnt_m + 1.
        ENDIF.
      ENDLOOP.

      $$gt_out[] = $gt_out[].

      IF gv_cnt_f > 0.

        __cls $gt_out.
        LOOP AT $$gt_out.
          IF $$gt_out-cnfg = 'X'.
            $gt_out = $$gt_out.
            APPEND $gt_out.
          ENDIF.
        ENDLOOP.

        PERFORM pre_crun USING 'F'.
      ENDIF.

      IF gv_cnt_m > 0.

        __cls $gt_out.
        LOOP AT $$gt_out.
          IF $$gt_out-cnfg = ' '.
            $gt_out = $$gt_out.
            APPEND $gt_out.
          ENDIF.
        ENDLOOP.

        PERFORM pre_crun USING 'M'.
      ENDIF.

      PERFORM refresh_disp.

    WHEN 'ININ'.
      PERFORM init_ck40_new.

    WHEN 'GCK4'.


      DATA: l_cstdt(10).
      DATA $var_text type CK_KALAID.

      CONCATENATE gv_klvar 'M' INTO $var_text.

      PERFORM convert_date USING: gv_cstdt CHANGING l_cstdt.

      SET PARAMETER ID 'KKF'  FIELD $var_text. "gv_klvar.
      SET PARAMETER ID 'KK1'  FIELD l_cstdt.

      CALL TRANSACTION 'CK40N' AND SKIP FIRST SCREEN..

  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Form  ck11_costing
*&---------------------------------------------------------------------*
FORM ck11_costing.
  DATA: l_rc   TYPE sysubrc.

* Get selected rows
  CALL METHOD g_grid->get_selected_rows
              IMPORTING et_index_rows = gt_row
                        et_row_no = gt_roid.

  LOOP AT gt_row INTO gs_row.
    READ TABLE gt_out INDEX gs_row-index.

    IF sy-subrc = 0.
      PERFORM bdc_ck11 CHANGING l_rc.
      IF l_rc <> 0.
        MESSAGE s000 WITH 'Costing error'.
      ENDIF.
    ENDIF.

  ENDLOOP.
ENDFORM.                    " ck11_costing
*&---------------------------------------------------------------------*
*&      Form  BDC_CK11
*&---------------------------------------------------------------------*
*       Execute BDC Transaction: Create Cost Estimates for Material
*----------------------------------------------------------------------*
FORM bdc_ck11 CHANGING p_rc TYPE sysubrc.
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

* Get BDC Option
  PERFORM get_opt USING 'N'.


* Create Cost Estimates for Material
  PERFORM dynpro USING:
     'X'  'SAPLCKDI'        '0111',
     ' '  'CKI64A-KLVAR'    gv_klvar,         " Costing Version
     ' '  'CKI64A-MATNR'    gt_out-matnr,     " Material
     ' '  'CKI64A-WERKS'    gt_out-werks.     " Plant

  IF gt_out-verid <> space.
    PERFORM dynpro USING:
     ' '  'CKI64A-FVIDK'    gt_out-verid.

  ELSE.
    PERFORM dynpro USING:
     ' '  'CKI64A-STLAN'    gt_out-stlan.                   "Module=2
  ENDIF.


  PERFORM dynpro USING:
     ' '  'BDC_OKCODE'      '/00',            " [Enter]
* Date
     'X'  'SAPLCKDI'        '0400',
     ' '  'CKI64A-KADAT'    l_cstdt,          " Costing date from
     ' '  'CKI64A-BIDAT'    l_cstdt,          " Costing date to
     ' '  'CKI64A-ALDAT'    l_aldat,          " Qty structure date
     ' '  'CKI64A-BWDAT'    l_valdt,          " Valuation date
     ' '  'BDC_OKCODE'	     '=ENTR',         " [Enter]

     'X'  'SAPLCKDI'        '2100',
     ' '  'BDC_OKCODE'	     '=BUCA'.         " [Save]

  CALL TRANSACTION 'CK11'   USING         gt_bdc
                            OPTIONS FROM  gs_opt
                            MESSAGES INTO gt_msg.

  READ TABLE gt_msg WITH KEY msgtyp = 'S'
                             msgid = 'CK'
                             msgnr = '039'.

  p_rc = sy-subrc.

ENDFORM.                                                    " BDC_CK11
*&---------------------------------------------------------------------*
*&      Form  reorg_module
*&---------------------------------------------------------------------*
FORM reorg_module.
  DATA: wa_jobcount LIKE  tbtcjob-jobcount,
        wa_jobname  LIKE  tbtcjob-jobname.

  wa_jobname = 'REORG_MODULE_COSTING'.
  PERFORM call_job_open USING wa_jobname wa_jobcount.

  SUBMIT saprckr1
     VIA JOB wa_jobname NUMBER wa_jobcount AND RETURN
                  WITH p_kadat  = gv_cstdt
                  WITH p_klvar  = 'ZM01'
                  WITH maschin  = 'X'
                  WITH manuell  = 'X'
                  WITH mimeger  = 'X'
                  WITH ohmeger  = 'X'
                  WITH kaohref  = 'X'
                  WITH test     = ' '
                  WITH p_listau = 'X'.

  IF sy-subrc = 0.
    PERFORM call_job_close USING wa_jobname wa_jobcount ' '.
    MESSAGE s000 WITH 'Reorganization job is scheduled.'.
  ENDIF.

ENDFORM.                    " reorg_module
*&---------------------------------------------------------------------*
*&      Form  INIT_CK40
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM init_ck40.

  DATA lv_answer.

  CLEAR lv_answer.
  CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
       EXPORTING
            textline1      = 'Do you really want to reorg '
            textline2      = 'the Costing Data(CK40)?'
            titel          = 'Check!'
            cancel_display = 'X'
       IMPORTING
            answer         = lv_answer.

  CHECK lv_answer = 'J'.

  RANGES r_klvar FOR keko-klvar.     " Costing variant

  DATA: l_kalaid  TYPE ck_kalaid,    " Costing Run
        l_test    TYPE ck_test,      " Test
        l_listau  TYPE ck_protoco,   " with List
        l_kalka   TYPE ck_kalka.
  DATA: wa_jobcount LIKE  tbtcjob-jobcount,
        wa_jobname  LIKE  tbtcjob-jobname.

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

    SELECT COUNT( * ) INTO sy-dbcnt FROM kala
       WHERE kalaid  = l_kalaid
         AND kaladat = gv_cstdt.

    IF sy-subrc = 0.
      SUBMIT saprck44 WITH kalaid  = l_kalaid
                      WITH kaladat = gv_cstdt
                      WITH backgr  = 'X'
                  AND RETURN.
    ENDIF.
  ENDIF.

  IF gv_cnt_m > 0.
    CLEAR l_kalaid.
    CONCATENATE l_kalka 'M' INTO l_kalaid.

    SELECT COUNT( * ) INTO sy-dbcnt FROM kala
       WHERE kalaid  = l_kalaid
         AND kaladat = gv_cstdt.
    IF sy-subrc = 0.
      SUBMIT saprck44 WITH kalaid  = l_kalaid
                      WITH kaladat = gv_cstdt
                      WITH backgr  = 'X'
                  AND RETURN.
    ENDIF.

  ENDIF.

ENDFORM.                                                    " INIT_CK40
*&---------------------------------------------------------------------*
*&      Form  pre_crun
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM pre_crun USING c_txt.

  DATA: lv_cnt(5),
        lv_dcnt(5),
        lv_msg(200).                 " Message

  DATA: l_cstdt(10),        " Costing Run Date
        l_valdt(10),        " Costing Run Date
        l_aldat(10).        " Costing Run Date

  DATA $var_text(10).
  CONCATENATE gv_klvar c_txt INTO $var_text.

  __cls : gt_bdc, gt_msg.

* Save seleted data to table ztcou100
  CLEAR: lv_cnt.

  DATA  : i_ztcou100 LIKE ztcou100 OCCURS 0 WITH HEADER LINE,
          ls_ztcou100 LIKE ztcou100.

  DATA: msg LIKE cfgnl-msglin.

  CLEAR : l_cstdt, l_valdt .

  PERFORM convert_date USING: gv_cstdt CHANGING l_cstdt,
                              gv_valdt CHANGING l_valdt,
                              gv_aldat CHANGING l_aldat.


* Create Cost Estimates for Material

  PERFORM dynpro USING: 'X'  'SAPLCKCC01'      '2000',
                        ' '  'BDC_OKCODE'      '=CREATE',

                        'X'  'SAPLCKCC01'      '2000',
                        ' '  'BDC_OKCODE'      '=ENTR',
                        ' '  'KALA-KALAID'      $var_text,
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

  DATA $ix LIKE sy-tabix.
  LOOP AT $gt_out.

    IF $gt_out-idx IS INITIAL.
      $ix = sy-tabix.
    ELSE.
      $ix = $gt_out-idx.
    ENDIF.

    READ TABLE gt_msg WITH KEY msgtyp = 'E'.
    IF sy-subrc NE 0.
      concatenate 'Created:' $var_text '-' l_cstdt into msg.
      CLEAR gt_out-err2.
      gt_out-err2 = 'N'.
    ELSE.
      PERFORM make_msg_string USING msg.
      gt_out-err2 = true.
    ENDIF.
    gt_out-msg2 = msg.
    MODIFY gt_out INDEX $ix TRANSPORTING err2 msg2.

    lv_cnt = lv_cnt + 1.

  ENDLOOP.

  IF lv_dcnt > 0 OR lv_cnt > 0.
    CONCATENATE 'Data''s been processed;'
                 lv_cnt  'rec(s).'
            INTO lv_msg SEPARATED BY space.
    MESSAGE s000 WITH lv_msg.
  ENDIF.

  IF gt_out-err2 EQ 'N'.

* create variants
    DATA r_c TYPE i.
    PERFORM create_run_variant TABLES $gt_out
                               USING gv_cstdt c_txt
                               CHANGING r_c.

  ENDIF.

ENDFORM.                    " pre_crun
*&---------------------------------------------------------------------*
*&      Form  get_selected_rows
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_$GT_OUT  text
*----------------------------------------------------------------------*
FORM get_selected_rows TABLES $gt_out STRUCTURE gt_out.

  PERFORM clear_chk.

  DATA: lt_rows TYPE lvc_t_row WITH HEADER LINE,
        lt_row_no TYPE lvc_t_roid. "Numeric IDs of Selected Rows

  CALL METHOD g_grid->get_selected_rows
           IMPORTING et_index_rows = lt_rows[]
                     et_row_no     = lt_row_no.

  CALL METHOD cl_gui_cfw=>flush.

  IF sy-subrc NE 0.
    MESSAGE e000
    WITH 'Error founded during flushing of ALV Grid Control'.
    EXIT.
  ENDIF.

  __cls $gt_out.

  READ TABLE lt_rows INDEX 1.
  IF sy-subrc NE 0.
    $gt_out[] = gt_out[].
    gt_out-chk = true .
    MODIFY gt_out TRANSPORTING chk WHERE chk EQ false.
  ELSE.
    LOOP AT lt_rows WHERE rowtype IS initial.
      READ TABLE gt_out INDEX lt_rows-index.
      gt_out-chk = true .
      MODIFY gt_out INDEX lt_rows-index .
    ENDLOOP.
    LOOP AT gt_out.
      CHECK gt_out-chk EQ true.
      $gt_out = gt_out.
      $gt_out-idx = sy-tabix.
      APPEND $gt_out.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " get_selected_rows
*&---------------------------------------------------------------------*
*&      Form  clear_chk
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM clear_chk.

  CLEAR gt_out-chk.
  MODIFY gt_out TRANSPORTING chk WHERE chk EQ true.

ENDFORM.                    " clear_chk
*&---------------------------------------------------------------------*
*&      Form  create_run_variant
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_$GT_OUT  text
*      -->P_GV_CSTDT  text
*      <--P_R_C  text
*----------------------------------------------------------------------*
FORM create_run_variant   TABLES $gt_out STRUCTURE gt_out
                          USING  p_date p_txt
                          CHANGING p_r_c.

  DATA:      x_date       TYPE sy-datum,
             x_intdate    TYPE i,
             x_variant    TYPE variant.
  DATA g_report TYPE rsvar-report.
  DATA       p_kaladat   TYPE kala-kaladat.

  DATA $confmat LIKE confmat.

  DATA: BEGIN OF lt_mat OCCURS 0,
          matnr TYPE matnr,
        END   OF lt_mat.

  IF p_txt EQ 'F'.
    $confmat = 'X'.
  ENDIF.

  CHECK NOT $gt_out[] IS INITIAL.
  CLEAR p_r_c.

  DATA $var_text(10).
  CONCATENATE gv_klvar p_txt INTO $var_text.

  p_kaladat = p_date.
  x_date = p_kaladat.
*  Convert Date of Costingrun to a internal number

  x_intdate         = x_date.
  x_variant+8(6)    = x_intdate.
  x_variant(5)      = $var_text.

  SELECT  * FROM  kala
         WHERE  kalaid    = $var_text
         AND    kaladat   = p_kaladat.
  ENDSELECT.

  IF sy-subrc EQ 0.
  ELSE.
    MESSAGE s000 WITH 'No Name of costing run!' .
    p_r_c = 4.
    EXIT.
  ENDIF.

  __cls gt_rsparams.
  __add_sel_tab 'BACKGR'   'P' 'X'.

  LOOP AT $gt_out.
    lt_mat-matnr = $gt_out-matnr.
    APPEND lt_mat.
  ENDLOOP.

  SORT lt_mat.
  DELETE ADJACENT DUPLICATES FROM lt_mat
      COMPARING matnr .


  LOOP AT lt_mat.
    __add_sel_tab_mat 'CK_MATNR' 'S' 'I' 'EQ' lt_mat-matnr.
  ENDLOOP.

  __add_sel_tab 'CONFMAT'  'P' $confmat.
  __add_sel_tab 'NOUEB'    'P' p_noueb.
  __add_sel_tab 'KALADAT'  'P' p_kaladat.
  __add_sel_tab 'KALAID'   'P' $var_text.
  __add_sel_tab 'PROTDR'   'P' 'X'.
  __add_sel_tab 'P_COCKP'  'P' 'X'.

  PERFORM create_var_by_ig USING 'SAPRCK60' x_variant
                           CHANGING p_r_c.

  __cls gt_rsparams.
  __add_sel_tab 'BACKGR'   'P' 'X'.
  __add_sel_tab 'KALADAT'  'P' p_kaladat.
  __add_sel_tab 'KALAID'   'P' $var_text.
  __add_sel_tab 'PROTDR'   'P' 'X'.
  __add_sel_tab 'P_COCKP'  'P' 'X'.

  PERFORM create_var_by_ig USING 'SAPRCK62' x_variant
                           CHANGING p_r_c.

  __cls gt_rsparams.
  __add_sel_tab 'BACKGR'   'P' 'X'.
  __add_sel_tab 'KALADAT'  'P' p_kaladat.
  __add_sel_tab 'KALAID'   'P' $var_text.
  __add_sel_tab 'LLC_LOG'  'P' 'X'.
  __add_sel_tab 'ONLYKF'   'P' 'X'.
  __add_sel_tab 'PARALLEL' 'P' 'X'.
  __add_sel_tab 'PROTDR'   'P' 'X'.
  __add_sel_tab 'P_COCKP'  'P' 'X'.
  __add_sel_tab 'SERVNUM'  'P' '07'.

  PERFORM create_var_by_ig USING 'SAPRCK10' x_variant
                           CHANGING p_r_c.

ENDFORM.                    " create_run_variant
*&---------------------------------------------------------------------*
*&      Form  create_var_by_ig
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1527   text
*      -->P_X_VARIANT  text
*      <--P_P_R_C  text
*----------------------------------------------------------------------*
FORM create_var_by_ig USING    p_report
                               p_variant
                      CHANGING p_r_c .

  DATA p_rc LIKE sy-subrc.

  CALL FUNCTION 'RS_VARIANT_EXISTS'
       EXPORTING
            report              = p_report
            variant             = p_variant
       IMPORTING
            r_c                 = p_rc
       EXCEPTIONS
            not_authorized      = 1
            no_report           = 2
            report_not_existent = 3
            report_not_supplied = 4
            OTHERS              = 5.

  IF p_rc EQ 0.

    CALL FUNCTION 'RS_VARIANT_DELETE'
         EXPORTING
              report               = p_report
              variant              = p_variant
              flag_confirmscreen   = 'N'
              flag_delallclient    = 'X'
         EXCEPTIONS
              not_authorized       = 1
              not_executed         = 2
              no_report            = 3
              report_not_existent  = 4
              report_not_supplied  = 5
              variant_locked       = 6
              variant_not_existent = 7
              no_corr_insert       = 8
              variant_protected    = 9
              OTHERS               = 10.
    IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ELSE.
*      MESSAGE s000 WITH p_report p_variant 'was deleted'.
    ENDIF.

  ENDIF.

  DATA: BEGIN OF lt_varit OCCURS 0.
          INCLUDE STRUCTURE varit.
  DATA: END OF lt_varit.
  DATA: ld_variant LIKE rsvar-variant.
  DATA: ld_repid LIKE sy-repid.
  DATA: ls_varid LIKE varid.

  ld_repid = p_report.
  ld_variant = p_variant.

*...fill table with variant texts......................................
*
  lt_varit-langu   = sy-langu.
  lt_varit-report  = p_report .
  lt_varit-variant = p_variant.
  lt_varit-vtext   = 'Generated variant (do not change manually!)'.
  APPEND lt_varit.

*...fill structure ls_varid (variant attributes).......................
*
  ls_varid-report    = p_report.
  ls_varid-variant   = p_variant.
  ls_varid-protected = space.
  ls_varid-ename     = sy-uname.
  ls_varid-edat      = sy-datum.
  ls_varid-etime     = sy-uzeit.

*...store variant......................................................
*
  CALL FUNCTION 'RS_CREATE_VARIANT'
       EXPORTING
            curr_report   = p_report
            curr_variant  = p_variant
            vari_desc     = ls_varid
       TABLES
            vari_contents = gt_rsparams
            vari_text     = lt_varit
       EXCEPTIONS
            OTHERS        = 1.

  IF sy-subrc <> 0.
    p_r_c = 4.
  ENDIF.

  CALL FUNCTION 'RS_VARIANT_EXISTS'
       EXPORTING
            report              = p_report
            variant             = p_variant
       IMPORTING
            r_c                 = p_rc
       EXCEPTIONS
            not_authorized      = 1
            no_report           = 2
            report_not_existent = 3
            report_not_supplied = 4
            OTHERS              = 5.

  IF p_rc EQ 0.
    MESSAGE s000 WITH p_report p_variant 'was created successfully.'.
  ELSE.
    MESSAGE s000 WITH 'Error was occurred when create variant.'.
    p_r_c = 4.
  ENDIF.

ENDFORM.                    " create_var_by_ig
*&---------------------------------------------------------------------*
*&      Form  make_msg_string
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_MSG  text
*----------------------------------------------------------------------*
FORM make_msg_string USING    p_msg.

  CALL FUNCTION 'RKC_MSG_STRING'
       EXPORTING
            id      = sy-msgid
            mtype   = sy-msgty
            number  = sy-msgno
            par1    = sy-msgv1
            par2    = sy-msgv2
            par3    = sy-msgv3
            par4    = sy-msgv4
       IMPORTING
            msg_lin = p_msg.

ENDFORM.                    " MAKE_MSG_STRING
*&---------------------------------------------------------------------*
*&      Form  init_ck40_new
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM init_ck40_new.
  DATA lv_answer.

  DATA $var_text(10).

  CLEAR lv_answer.
  CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
       EXPORTING
            textline1      = 'Do you really want to delete '
            textline2      = 'the Costing Data(CK40)?'
            titel          = 'Check!'
            cancel_display = 'X'
       IMPORTING
            answer         = lv_answer.

  CHECK lv_answer = 'J'.


  CLEAR : gv_cnt_f, gv_cnt_m.

  PERFORM get_selected_rows TABLES $gt_out.
  LOOP AT $gt_out.
    IF $gt_out-cnfg = 'X'.
      gv_cnt_f = gv_cnt_f + 1.
    ELSE.
      gv_cnt_m = gv_cnt_m + 1.
    ENDIF.
  ENDLOOP.

  IF gv_cnt_f > 0.
    CONCATENATE gv_klvar 'F' INTO $var_text.

    SELECT COUNT( * ) INTO sy-dbcnt FROM kala
       WHERE kalaid  = $var_text
         AND kaladat = gv_cstdt.

    IF sy-subrc = 0.
      SUBMIT saprck44 WITH kalaid  = $var_text
                      WITH kaladat = gv_cstdt
                      WITH backgr  = 'X'
                  AND RETURN.
*  ELSE.
*    MESSAGE s000 WITH 'Could not find Costing Run data.'.
    ENDIF.

  ENDIF.

  IF gv_cnt_m > 0.

    CONCATENATE gv_klvar 'M' INTO $var_text.

    SELECT COUNT( * ) INTO sy-dbcnt FROM kala
       WHERE kalaid  = $var_text
         AND kaladat = gv_cstdt.

    IF sy-subrc = 0.
      SUBMIT saprck44 WITH kalaid  = $var_text
                      WITH kaladat = gv_cstdt
                      WITH backgr  = 'X'
                  AND RETURN.
*  ELSE.
*    MESSAGE s000 WITH 'Could not find Costing Run data.'.
    ENDIF.
  ENDIF.

ENDFORM.                    " init_ck40_new
*&---------------------------------------------------------------------*
*&      Form  check_reg
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_OUT  text
*----------------------------------------------------------------------*
FORM check_reg TABLES   p_gt_out STRUCTURE gt_out.

  DATA $ix LIKE sy-tabix.

  LOOP AT p_gt_out.
    $ix = sy-tabix.
    p_gt_out-bdatj = p_gt_out-kadky(4).
    p_gt_out-poper = p_gt_out-kadky+4(2).

    SELECT SINGLE cnfg crint crext lstat INTO
      (p_gt_out-cnfg,p_gt_out-crint,p_gt_out-crext,p_gt_out-lstat)
      FROM ztcou100
     WHERE kokrs = p_kokrs
       AND kalka IN s_kalka
       AND bdatj = p_gt_out-bdatj
       AND poper = p_gt_out-poper
       AND matnr = p_gt_out-matnr
       and ( intrn = space or intrn is null ).

    IF sy-subrc EQ 0..
      p_gt_out-reg = 'X'.
    ENDIF.

    MODIFY p_gt_out INDEX $ix TRANSPORTING bdatj poper cnfg crint
                                         crext lstat reg .
  ENDLOOP.



ENDFORM.                    " check_reg
