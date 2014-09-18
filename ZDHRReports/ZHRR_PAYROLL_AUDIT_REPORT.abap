*&---------------------------------------------------------------------*
*& Report  ZAHRU001_NEW
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT zhrr_payroll_audit_report MESSAGE-ID zmfi.

INCLUDE : zhrr_payroll_audit_report_1,
"Data Declaration
          zhrr99990t.                  "ALV common routine


*----------------------------------------------------------------------*
* INITIALIZATION
*----------------------------------------------------------------------*
INITIALIZATION.
  sy-title = '[HR] Payroll Audit Report'.
  old_abkrs = p_abkrs.
  PERFORM re549a USING old_abkrs.
  w_permo = t549a-permo.

*  PERFORM get_payroll_period USING old_abkrs
*                          CHANGING w_permo s_begda-low s_begda-high
*                                   w_abkrt.
*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.
  CASE 'X'.
    WHEN p_op0.
      CLEAR s_begda. REFRESH s_begda.
      IF old_abkrs <> p_abkrs OR p_date IS INITIAL.
        old_abkrs = p_abkrs.
        PERFORM re549a USING old_abkrs.
        w_permo = t549a-permo.
*        PERFORM get_payroll_period USING old_abkrs
*CHANGING w_permo s_begda-low s_begda-high
*                                         w_abkrt.
      ENDIF.
    WHEN p_op1.
      IF old_abkrs <> p_abkrs.
        old_abkrs = p_abkrs.
      ENDIF.
      CLEAR p_date.

  ENDCASE.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_vari.
  PERFORM alv_variant_f4 CHANGING p_vari.

*----------------------------------------------------------------------*
*START-OF-SELECTION.
*----------------------------------------------------------------------*
START-OF-SELECTION.

  DATA: i549s LIKE t549s OCCURS 0 WITH HEADER LINE.
  DATA: i549q LIKE t549q OCCURS 0 WITH HEADER LINE.
  DATA: v_begda TYPE sy-datum.
  DATA: lt_pc261 LIKE TABLE OF pc261 WITH HEADER LINE.

  PERFORM re549a USING p_abkrs.
  w_permo = t549a-permo.

  l_relid = 'RU'.

  CLEAR g_error.
  __cls r_begda .
*Sriram cvhanges 08/10/2010
  CASE 'X'.
    WHEN p_op0.

      s_begda-low = s_begda-high = p_date.
      s_begda-sign   = 'I'.
      s_begda-option = 'EQ'.  APPEND s_begda.

    WHEN p_op1.
      v_begda = s_begda-low.
      PERFORM get_payroll_period_sel USING old_abkrs
                          CHANGING w_permo v_begda v_begda
                                   w_abkrt.
      lv_begda = v_begda.
  ENDCASE.

  PERFORM get_it_pernr USING lv_begda.


  REFRESH r_datum.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE i549s
           FROM t549s WHERE molga = '10'
                        AND datmo = '01'
                        AND permo = w_permo
                        AND datid = '1'          "paydate
                        AND pdate IN s_begda.
  IF sy-subrc = 0.
    LOOP AT i549s.
      AT FIRST.
        READ TABLE i549s INDEX sy-tabix.
        SELECT SINGLE * INTO CORRESPONDING FIELDS OF i549q
                 FROM t549q WHERE permo = w_permo
                              AND pabrj = i549s-pabrj
                              AND pabrp = i549s-pabrp.

        r_datum-low = i549q-begda.
        CASE 'X'.
          WHEN p_op0.
            lv_begda = i549q-begda.

          WHEN p_op1.
            "do nothing
        ENDCASE.
      ENDAT.
      AT LAST.
        READ TABLE i549s INDEX sy-tabix.
        SELECT SINGLE * INTO CORRESPONDING FIELDS OF i549q
                 FROM t549q WHERE permo = w_permo
                              AND pabrj = i549s-pabrj
                              AND pabrp = i549s-pabrp.
        r_datum-high = i549q-endda.
      ENDAT.
    ENDLOOP.
  ELSE.
** off-cycle pay date
    SORT it_pernr BY pernr.
    READ TABLE it_pernr INDEX 1.
    REFRESH: lt_pc261.
    CALL FUNCTION 'HR_NO_EVALU_PERIODS_OFFCYCLE'
      EXPORTING
        rep_pernr       = it_pernr-pernr
        inper_modif     = '00'
        rep_begda       = p_date
        rep_endda       = p_date
      TABLES
        evpdir          = lt_pc261
*       IABKRS          =
      EXCEPTIONS
        no_record_found = 1
        OTHERS          = 2.
    IF sy-subrc = 0.
      READ TABLE  lt_pc261 INDEX 1.
      r_datum-low = lt_pc261-paydt.
      r_datum-high = lt_pc261-paydt.
    ELSE.

      MESSAGE e016(pg) WITH 'Payroll period cannot be determined'.
    ENDIF.
  ENDIF.

  r_datum-sign   = 'I'.
  r_datum-option = 'BT'.
  APPEND r_datum.

  r_begda[] = r_datum[].

  READ TABLE r_begda INDEX 1.
  PERFORM get_status USING r_begda-high.

*----------------------------------------------------------------------*
* END-OF-SELECTION.
*----------------------------------------------------------------------*
END-OF-SELECTION.

  CHECK g_error NE true.

* Display and read the setted wage type
  PERFORM get_wage TABLES gt_temp  gt_temp2  USING p_lgafm.

* Assign sequence number to display the fields
  PERFORM set_display_no TABLES gt_0029.
  PERFORM set_output.

* Collect the payroll data
  PERFORM fill_out.

  IF gt_result[] IS INITIAL.
    MESSAGE i020.  EXIT.  "Thers is no data.
  ENDIF.
  CHECK gt_result[] IS NOT INITIAL.

  SORT gt_result STABLE BY cpid paydt fpper ocrsn.

* Output of ALV lists
  PERFORM build_eventcat USING '1'.
  DELETE gt_events_lvc WHERE name = g_top_of_page.

  DESCRIBE TABLE gt_result LINES g_cnt.
  PERFORM fieidcat_gathering.
  PERFORM call_function TABLES gt_result
                         USING 'ALV_USER_COMMAND' 'ALV_PF_STATUS_SET'.

  CLEAR: p_date.
*&---------------------------------------------------------------------*
*&      Form  set_output
*&---------------------------------------------------------------------*
FORM set_output.

  CHECK g_error IS INITIAL.

  PERFORM show_progress     USING 'Preparing screen...' '95'.

ENDFORM.                    " set_output

*&---------------------------------------------------------------------*
*&      Form  show_progress
*&---------------------------------------------------------------------*
FORM show_progress USING    pf_text
                            value(pf_val).

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = pf_val
      text       = pf_text.

ENDFORM.                    " SHOW_PROGRESS
*&---------------------------------------------------------------------*
*&      Form  get_status
*&---------------------------------------------------------------------*
FORM get_status USING check_date .
  __cls it_status.

  SELECT pernr  begda massn massg stat2 INTO TABLE it_status
  FROM pa0000
    FOR ALL ENTRIES IN it_pernr
  WHERE  pernr = it_pernr-pernr
    AND  begda LE check_date
    AND  endda GE check_date.

  SORT it_status BY pernr ASCENDING
                    begda DESCENDING .

  DELETE ADJACENT DUPLICATES FROM it_status
      COMPARING pernr.

  SORT it_status BY pernr.
*Begin of Sriram Changes 08/30/2010.
  LOOP AT it_pernr INTO ws_pernr.
    lv_tabix = sy-tabix.
    CLEAR ws_status.
    READ TABLE it_status INTO ws_status WITH KEY pernr = ws_pernr-pernr.
    IF sy-subrc EQ 0.
      MOVE ws_status-stat2 TO ws_pernr-stat2.
      MODIFY it_pernr FROM ws_pernr INDEX lv_tabix TRANSPORTING stat2.
    ENDIF.
    CLEAR ws_pernr.
  ENDLOOP.
*End of Sriram Changes 08/30/2010.

ENDFORM.                     " get_status
*&---------------------------------------------------------------------*
*&      Form  fill_out
*&---------------------------------------------------------------------*
FORM fill_out.

  __define_not_important.
  DESCRIBE TABLE it_pernr LINES total_doc_cnt.
  $total_cnt = total_doc_cnt.

  DATA : $fr(3) TYPE n,
         $to(3) TYPE n.
  DATA : $total1 TYPE maxbt,
         $total2 TYPE maxbt,
         $total3 TYPE maxbt,
         $total4 TYPE maxbt.
  DATA : $557 TYPE maxbt,
         $559 TYPE maxbt,
         $net TYPE maxbt.

  DATA : l_lgart TYPE lgart.
  DATA : l_tabix LIKE sy-tabix.

  LOOP AT it_pernr.

    ADD 1 TO current_doc_cnt.
    $current_cnt = current_doc_cnt.
    CONCATENATE 'Calculating...'
                                 $current_cnt '/' $total_cnt
    INTO $text.
    CONDENSE $text.
    percentage = current_doc_cnt MOD 10.
    PERFORM show_progress USING $text 0.

* Get the payroll data
    PERFORM read_payroll_data USING it_pernr-pernr.

    IF in_rgdir[] IS INITIAL AND it_pernr-stat2 NE '0'.
      LOOP AT i549s.
        gt_tab-pernr = it_pernr-pernr.  "Pers. No.
*        gt_tab-vorna = it_pernr-vorna.  "First Name
*        gt_tab-nachn = it_pernr-nachn.  "Last Name
*        gt_tab-perid = it_pernr-perid.  "SSN ID
        CONCATENATE i549s-pabrj i549s-pabrp INTO fpper.

        CASE 'X'.
* Each period
          WHEN p_each.
            MOVE-CORRESPONDING gt_wpbp TO gt_tab.
            gt_tab-paydt = i549s-pdate.        "Pay date
            gt_tab-fpper = fpper.              "Payroll Period
            PERFORM fill_text2 USING  gt_wpbp-begda gt_wpbp-endda
                            CHANGING  gt_tab.
* Total periods
          WHEN p_total.
            MOVE-CORRESPONDING lt_wpbp TO gt_tab.
            PERFORM fill_text2 USING  lt_wpbp-begda lt_wpbp-endda
                            CHANGING  gt_tab.
        ENDCASE.

* Clear the values of displaying sequence for each fields
        MOVE-CORRESPONDING gt_tab TO gt_result.
        PERFORM clear_value TABLES gt_0029
                             USING gt_result.
        MOVE gt_tab-vorna TO gt_result-vorna.
         COLLECT gt_result.
        CLEAR : gt_tab, gt_result.
      ENDLOOP.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " fill_out
*&---------------------------------------------------------------------*
*&      Form  read_payroll_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_DIS_PERNR  text
*----------------------------------------------------------------------*
FORM read_payroll_data USING p_pernr.

  DATA: wa_rt LIKE pc207 OCCURS 0 WITH HEADER LINE,
        seqnr LIKE pc261-seqnr,
        result TYPE pay99_result.
  DATA: ls_inper_directory_entry TYPE ts_inper_directory_entry,
        sign_flag(1) .

  DATA : t2009 TYPE betrg.        "06102010PYS: 401K Loan A-tax
  DATA : t3000 TYPE betrg,
         t3001 TYPE betrg.
  DATA : t4000 TYPE betrg,
         t4001 TYPE betrg.
  DATA : gip_p  TYPE betrg,
         dlif_a TYPE betrg,
         olfe_a TYPE betrg,
         olsp_a TYPE betrg,
         olch_a TYPE betrg,
         can_a  TYPE betrg,
         pai_a  TYPE betrg,
         sphe_a TYPE betrg.

  DATA  retro TYPE c.
  DATA: BEGIN OF rgdir OCCURS 100.
          INCLUDE STRUCTURE pc261.
  DATA: END OF rgdir.
  DATA:  index   LIKE sy-index.

  DATA: xevp    LIKE STANDARD TABLE OF pc261,
        tmp_evp LIKE STANDARD TABLE OF pc261.
*
  DATA  sel_evp LIKE pc261.

  DATA:  BEGIN OF tr_evp_sort,           "WLIAHRK001342
           sortn LIKE sy-index,
           inevp LIKE pc261,                              " WLIK035912
           evp   LIKE pc261,
         END OF tr_evp_sort.

  DATA:  $xdata LIKE STANDARD TABLE OF tr_evp_sort WITH HEADER LINE.

  DATA: BEGIN OF lt_cpid OCCURS 0,
        objid LIKE hrp1001-objid ,
        sobid LIKE hrp1001-sobid,
        END OF lt_cpid.
  DATA: l_index LIKE sy-tabix.

  REFRESH $xdata.

  __cls in_rgdir.
* Read Payroll Control Records
  CLEAR lv_molga.
  CALL FUNCTION 'CU_READ_RGDIR'
    EXPORTING
      persnr          = p_pernr
    IMPORTING
      molga           = lv_molga
    TABLES
      in_rgdir        = in_rgdir
    EXCEPTIONS
      no_record_found = 1
      OTHERS          = 2.

  CLEAR: lt_inper_directory[], lt_inper_directory.
  CLEAR: lt_eval_tab[], lt_eval_tab.
  CLEAR: lt_tweaked_evp[], lt_tweaked_evp.
  CLEAR: lt_evp_related_records[], lt_evp_related_records.

  CLEAR: ypernodt, lv_evp_lines_number.

  REFRESH rg_abkrs.  CLEAR rg_abkrs.
  rg_abkrs-sign   = 'I'.
  rg_abkrs-option = 'EQ'.
  rg_abkrs-low    = p_abkrs.
  APPEND rg_abkrs.

  APPEND LINES OF in_rgdir TO xevp.

  CASE 'X'.
    WHEN p_op0.
*      ypernodt = true.
      g_begda = r_begda-low.
      g_endda = r_begda-high.
      DELETE xevp WHERE paydt <> p_date.

    WHEN p_op1.
      g_begda = s_begda-low.
      g_endda = s_begda-high.
      DELETE xevp WHERE NOT ( paydt BETWEEN g_begda AND g_endda ).
  ENDCASE.

  LOOP AT xevp INTO sel_evp.
    CLEAR retro.
    CALL FUNCTION 'CD_RETROCALC_PERIOD'
      EXPORTING
        entry = sel_evp
      IMPORTING
        calcd = retro.
    IF retro EQ true.
      DELETE xevp.
    ENDIF.
  ENDLOOP.
*      DESCRIBE TABLE tp_rgdir LINES lv_evp_lines_number.
  DESCRIBE TABLE xevp LINES lv_evp_lines_number.
*
*End of  Sriram Changes 10/08/2010.
*      IF lv_evp_lines_number = 1.
  ypernodt = true.
*      ENDIF.
*  ENDCASE.

*(+)2011.08.01 added by jeongsu.youn -new logic-
  DATA:  repid TYPE syrepid.         " WLIK063555
  repid = 'RPCLJNU0'.

  PERFORM change_original_periods_natio
          IN PROGRAM (repid)
          IF FOUND TABLES xevp rgdir.
  PERFORM change_original_periods_mod
          IN PROGRAM (repid)
          IF FOUND TABLES xevp rgdir.

  SORT xevp BY inper iperm inpid inpty bondt.
  DELETE ADJACENT DUPLICATES FROM xevp
         COMPARING inper iperm inpid inpty bondt.
  SORT xevp BY seqnr.
  LOOP AT xevp INTO sel_evp.
    index = sy-tabix.
    CLEAR tmp_evp.
    PERFORM fill_evp TABLES in_rgdir tmp_evp USING sel_evp.
    LOOP AT tmp_evp INTO $xdata-evp.
      $xdata-sortn = index.
      $xdata-inevp = sel_evp.                               "WLIK035912
      APPEND $xdata.
    ENDLOOP.
  ENDLOOP.

  DATA  ls_xdata LIKE $xdata.

  CLEAR in_rgdir. REFRESH in_rgdir.
  LOOP AT $xdata INTO ls_xdata.
    in_rgdir = ls_xdata-evp.

    APPEND in_rgdir. CLEAR in_rgdir.
  ENDLOOP.

  IF in_rgdir[] IS NOT INITIAL.
    LOOP AT in_rgdir INTO ws_rgdir.

*    check ws_rgdir-fper = inper.

      IF  ws_rgdir-srtza EQ 'A'.
        sign_flag = 'A'.
      ELSE.
        sign_flag = 'P'.
      ENDIF.

      REFRESH gt_payroll.
      CALL METHOD zcl_hr_comclass=>get_payroll_result
        EXPORTING
          i_pernr   = it_pernr-pernr
          i_seqnr   = ws_rgdir-seqnr
        IMPORTING
          e_payroll = gt_payroll.



      CLEAR : t2009, t3000, t3001, t4000, t4001, gip_p.
      CLEAR : dlif_a, olfe_a, olsp_a, olch_a, can_a, pai_a, sphe_a.


      READ TABLE gt_payroll INTO lt_payroll INDEX 1.
      REFRESH : gt_wpbp, gt_rt, gt_crt, gt_bt, gt_ddntk, gt_accr.
      CLEAR : gt_wpbp, gt_rt, gt_crt, gt_bt, gt_ddntk, gt_accr.

      gt_wpbp[]   = lt_payroll-inter-wpbp[].
      gt_rt[]     = lt_payroll-inter-rt[].
      gt_crt[]    = lt_payroll-inter-crt[].
      gt_bt[]     = lt_payroll-inter-bt[].
      gt_ddntk[]  = lt_payroll-inter-ddntk[].
      gt_accr[]   = lt_payroll-inter-accr[].

      IF  gt_temp[] IS NOT INITIAL.
        SELECT objid sobid INTO TABLE lt_cpid
          FROM hrp1001
*   FOR ALL ENTRIES IN gt_result
       WHERE "objid = gt_result-pernr
         sclas = 'CP'
       AND plvar = '01'
*and LANGU = 'E'
       AND endda = '99991231'
       AND otype = 'P'.
        SORT lt_cpid BY objid.

*  LOOP AT gt_result.
*    l_index = sy-tabix.
*    READ TABLE lt_cpid WITH KEY objid  = gt_result-pernr.
*    IF sy-subrc = 0.
*      gt_result-cpid = lt_cpid-sobid.
*      MODIFY gt_result INDEX l_index TRANSPORTING cpid.
*    ENDIF.
*  ENDLOOP.
      ENDIF.

      LOOP AT gt_temp.
        CLEAR gt_temp2.
        READ TABLE gt_temp2 WITH KEY lgagp = gt_temp-lgagp.
        IF sy-subrc = 0.
          g_tabix = sy-tabix.
        ENDIF.

        CLEAR : g_info1.
        CONCATENATE 'GT_TAB-BET' g_tabix INTO g_info1.
        ASSIGN (g_info1) TO <fs_02>.
        CHECK sy-subrc = 0.

* Amount or Number or Rate
        CASE gt_temp-cltab.
          WHEN '2'.  "RT
            PERFORM read_rt TABLES gt_rt
                             USING gt_temp-lgart gt_temp-evfld
                                   gt_temp-sign sign_flag
                          CHANGING <fs_02>.
          WHEN '3'.  "CRT
            PERFORM read_crt TABLES gt_crt
                              USING gt_temp-lgart gt_temp-evfld
                                    gt_temp-sign  gt_temp-cumty
                          CHANGING <fs_02>.
          WHEN '4'.  "BT
            PERFORM read_bt TABLES gt_bt
                             USING gt_temp-lgart gt_temp-evfld
                                   gt_temp-sign
                          CHANGING <fs_02>.
          WHEN '5'.  "DDNTK
            PERFORM read_ddntk TABLES gt_ddntk
                                USING gt_temp-lgart gt_temp-evfld
                                      gt_temp-sign
                             CHANGING <fs_02>.
          WHEN '6'.  "ACCR
            PERFORM read_accr TABLES gt_accr
                               USING gt_temp-lgart gt_temp-evfld
                                     gt_temp-sign
                            CHANGING <fs_02>.
          WHEN OTHERS.  CONTINUE.
        ENDCASE.
      ENDLOOP.

* Basic Info.
      SORT gt_wpbp STABLE BY endda DESCENDING.
      IF p_each = 'X'.
        READ TABLE gt_wpbp INDEX 1.
      ELSE.
        IF lt_wpbp IS INITIAL.
          READ TABLE gt_wpbp INTO lt_wpbp INDEX 1.
        ENDIF.
      ENDIF.
*Collect the selected items on selection-screen and read the text of
*codes
      gt_tab-pernr = it_pernr-pernr.  "Pers. No.
*      gt_tab-vorna = it_pernr-vorna.  "First Name
*      gt_tab-nachn = it_pernr-nachn.  "Last Name
*      gt_tab-perid = lt_payroll-nat-perm-perid.  "SSN ID

      CASE 'X'.
* Each period
        WHEN p_each.
          MOVE-CORRESPONDING gt_wpbp TO gt_tab.
          gt_tab-paydt = lt_payroll-inter-versc-paydt.  "Pay date
          gt_tab-ocrsn = lt_payroll-inter-versc-ocrsn.  "Payroll Reason
          gt_tab-abkrs = lt_payroll-inter-versc-abkrs.  "Payroll Area
          gt_tab-fpper = ws_rgdir-fpper.                "Payroll Period

          PERFORM fill_text2 USING  gt_wpbp-begda gt_wpbp-endda
                          CHANGING  gt_tab.
* Total periods
        WHEN p_total.
          MOVE-CORRESPONDING lt_wpbp TO gt_tab.
          PERFORM fill_text2 USING  lt_wpbp-begda lt_wpbp-endda
                          CHANGING  gt_tab.
      ENDCASE.

* Clear the values of displaying sequence for each fields
      MOVE-CORRESPONDING gt_tab TO gt_result.
*      gt_result-inper = ls_xdata-inevp-inper.
      gt_result-inper = ws_rgdir-inper.
      PERFORM clear_value TABLES gt_0029
                           USING gt_result.
      MOVE gt_tab-vorna TO gt_result-vorna.


      READ TABLE lt_cpid WITH KEY objid  = gt_result-pernr.
      IF sy-subrc = 0.
        gt_result-cpid = lt_cpid-sobid.
      ENDIF.
* PYS changes 08/26/2010
      IF p_each = 'X'.
        APPEND gt_result.
*        COLLECT gt_result.
      ELSE.
        CLEAR: gt_result-kostl, gt_result-inper.
        COLLECT gt_result.
      ENDIF.
* End of PYS changes 08/26/2010
      CLEAR : gt_tab, gt_result.

    ENDLOOP.
  ENDIF.

  CHECK gt_result[] IS NOT INITIAL.
  SELECT objid sobid INTO TABLE lt_cpid
    FROM hrp1001
    FOR ALL ENTRIES IN gt_result
 WHERE objid = gt_result-pernr
 AND   sclas = 'CP'
 AND plvar = '01'
*and LANGU = 'E'
 AND endda = '99991231'
 AND otype = 'P'.
  SORT lt_cpid BY objid.

  LOOP AT gt_result.
    l_index = sy-tabix.
    READ TABLE lt_cpid WITH KEY objid  = gt_result-pernr.
    IF sy-subrc = 0.
      gt_result-cpid = lt_cpid-sobid.
      MODIFY gt_result INDEX l_index TRANSPORTING cpid.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " read_payroll_data
*---------------------------------------------------------------------*
FORM fill_evp TABLES in_tab  STRUCTURE pc261
                     out_tab STRUCTURE pc261
              USING value($seldt) LIKE pc261.

  REFRESH out_tab.
*----------------------------------------------------------------------*
* fills internal table EVP for respective personnel number             *
*----------------------------------------------------------------------*
  CALL FUNCTION 'CD_EVALUATION_PERIODS'
    EXPORTING
      bonus_date         = $seldt-ipend  "                WLIK021185
*     bonus_date         = $seldt-bondt  "$bondt          WLIK021185
      inper              = $seldt-inper                  "period
      inper_modif        = $seldt-iperm  "pn-permo
      pay_ident          = $seldt-inpid  "$payid
      pay_type           = $seldt-inpty  "$payty
      all_results_of_run = false      "include 'S-runs'?    "VKIK032705
    TABLES
      evpdir             = out_tab
      rgdir              = in_tab
    EXCEPTIONS
      no_record_found    = 01.
ENDFORM.                               "(FILL_EVP)
*&---------------------------------------------------------------------*
*&      Form  get_it_pernr
*&---------------------------------------------------------------------*
FORM get_it_pernr USING check_date.
  DATA: BEGIN OF lt_cpid OCCURS 0,
          objid LIKE hrp1001-objid ,
          sobid LIKE hrp1001-sobid,
          pernr LIKE pa0001-pernr,
          END OF lt_cpid.

  SELECT objid sobid INTO CORRESPONDING FIELDS OF TABLE lt_cpid
      FROM hrp1001
*   FOR ALL ENTRIES IN gt_result
   WHERE otype = 'CP'
    AND objid IN s_objid
    AND plvar = '01'
   AND endda = '99991231'
   AND sclas = 'P'.

  LOOP AT lt_cpid.
    lt_cpid-pernr = lt_cpid-sobid.
    MODIFY lt_cpid.
  ENDLOOP.

  CHECK lt_cpid[] IS NOT INITIAL.
  SELECT pernr
           INTO CORRESPONDING FIELDS OF TABLE it_pernr
           FROM pa0001
    FOR ALL ENTRIES IN lt_cpid
             WHERE pernr = lt_cpid-pernr
             AND endda EQ '99991231'
             AND abkrs EQ p_abkrs.
ENDFORM.                    " get_it_pernr
*---------------------------------------------------------------------*
*       FORM get_payroll_period                                       *
*---------------------------------------------------------------------*
*FORM get_payroll_period USING v_abkrs
*                     CHANGING v_permo v_begda v_endda
*                              v_abkrt.
*
*
*  SELECT SINGLE * INTO *t549q
*               FROM  t549q
*               WHERE  permo  EQ v_permo
*               AND    begda  LE sy-datum
*               AND    endda  GE sy-datum.
*
*  CHECK sy-subrc EQ 0.
*  v_pabrj = *t549q-pabrj.
*  v_pabrp = *t549q-pabrp - 1.
*
*  CALL FUNCTION 'PA03_PERIODDATES_GET'
*    EXPORTING
*      f_abkrs               = v_abkrs
*    IMPORTING
*      f_permo               = v_permo
*      f_current_begda       = v_begda
*      f_current_endda       = v_endda
*      f_abkrs_text          = v_abkrt
*    CHANGING
*      f_current_period      = v_pabrp
*      f_current_year        = v_pabrj
*    EXCEPTIONS
*      pcr_does_not_exist    = 1
*      abkrs_does_not_exist  = 2
*      period_does_not_exist = 3
*      OTHERS                = 4.
*  IF sy-subrc <> 0.
*
*  ENDIF.
*
*  CALL FUNCTION 'HR_GB_PAY_DATE'
*    EXPORTING
*      molga              = '10'
*      abkrs              = p_abkrs
*      permo              = v_permo
*      pabrj              = v_pabrj
*      pabrp              = v_pabrp
*    IMPORTING
*      pdate              = p_date
*    EXCEPTIONS
*      pay_date_not_found = 1
*      OTHERS             = 2.
*
*  IF sy-subrc <> 0.
*  ENDIF.
*
*ENDFORM.                    " get_payroll_period
*&---------------------------------------------------------------------*
*&      Form  RE549A
*&---------------------------------------------------------------------*
FORM re549a USING value(f_abkrs) LIKE t549a-abkrs.
  CHECK f_abkrs <> t549a-abkrs.
  SELECT SINGLE * FROM t549a WHERE abkrs = f_abkrs.
  IF sy-subrc <> 0.
    MESSAGE e601(pg) WITH f_abkrs
                 RAISING abkrs_does_not_exist.
    CLEAR t549a.
  ENDIF.
ENDFORM.                                                    " RE549A
*&---------------------------------------------------------------------*
*&      Form  GET_WAGE
*&---------------------------------------------------------------------*
FORM get_wage  TABLES  pt_temp  STRUCTURE gt_temp
                       pt_temp2 STRUCTURE gt_temp
                USING  p_group.
  DATA index(3) TYPE n.

  SELECT DISTINCT * INTO CORRESPONDING FIELDS OF TABLE pt_temp
             FROM zthr0029 AS a INNER JOIN zthr0030 AS b
               ON a~lgafm = b~lgafm AND a~lgagp = b~lgagp
            WHERE a~lgafm = p_group.
  SORT pt_temp STABLE BY lgafm lgagp cltab lgart.

* Delete the duplication to distinct by grouping value
  pt_temp2[] = pt_temp[].
  DELETE ADJACENT DUPLICATES FROM pt_temp2 COMPARING lgagp.

ENDFORM.                    " GET_WAGE
*&---------------------------------------------------------------------*
*&      Form  READ_RT
*&---------------------------------------------------------------------*
FORM read_rt  TABLES   pt_rt STRUCTURE pc207
               USING   p_lgart  p_evfld  p_sign p_flg
            CHANGING   ch_betrg.

*- Calculate the amount
  LOOP AT pt_rt WHERE lgart = p_lgart.
* Sign ('+' or '-')
    CASE p_sign.
      WHEN '+'.
        pt_rt-betrg = abs( pt_rt-betrg ). "Sriram 08/09/2010.
        pt_rt-anzhl = abs( pt_rt-anzhl ).
        pt_rt-betpe = abs( pt_rt-betpe ).
      WHEN '-'.
        pt_rt-betrg = pt_rt-betrg * -1. "Sriram 08/09/2010.
        pt_rt-anzhl = pt_rt-anzhl * -1.
        pt_rt-betpe = pt_rt-betpe * -1.
      WHEN OTHERS.
    ENDCASE.
*Sriram Changes 08/09/2010.
    IF p_flg EQ 'P'.
      pt_rt-betrg = pt_rt-betrg * -1.
    ENDIF.

*End of Sriram Changes 08/09/2010.

    CASE p_evfld.
      WHEN '1'.  "Amount
        ch_betrg = ch_betrg + pt_rt-betrg.
      WHEN '2'.  "Number
        ch_betrg = ch_betrg + pt_rt-anzhl.
      WHEN '3'.  "Rate
        ch_betrg = ch_betrg + pt_rt-betpe.
    ENDCASE.
  ENDLOOP.

ENDFORM.                    " READ_RT
*&---------------------------------------------------------------------*
*&      Form  READ_CRT
*&---------------------------------------------------------------------*
FORM read_crt  TABLES   pt_crt STRUCTURE pc208
                USING   p_lgart  p_evfld  p_sign  p_cumty
             CHANGING   ch_betrg.

* Calculate the amount
  LOOP AT pt_crt WHERE lgart = p_lgart
                   AND cumty = p_cumty.

* Sign ('+' or '-')
    CASE p_sign.
      WHEN '+'.
        pt_crt-betrg = abs( pt_crt-betrg ).
        pt_crt-anzhl = abs( pt_crt-anzhl ).
      WHEN '-'.
        pt_crt-betrg = pt_crt-betrg * -1.
        pt_crt-anzhl = pt_crt-anzhl * -1.
      WHEN OTHERS.
    ENDCASE.

    CASE p_evfld.
      WHEN '1'.  "Amount
        ch_betrg = ch_betrg + pt_crt-betrg.
      WHEN '2'.  "Number
        ch_betrg = ch_betrg + pt_crt-anzhl.
      WHEN '3'.  "Rate
        EXIT.
    ENDCASE.
  ENDLOOP.

ENDFORM.                    " READ_CRT
*&---------------------------------------------------------------------*
*&      Form  READ_BT
*&---------------------------------------------------------------------*
FORM read_bt  TABLES   pt_bt STRUCTURE pc209
               USING   p_lgart  p_evfld  p_sign
            CHANGING   ch_betrg.

* Calculate the amount
  LOOP AT pt_bt WHERE lgart = p_lgart.
* Sign ('+' or '-')
    CASE p_sign.
      WHEN '+'.  pt_bt-betrg = abs( pt_bt-betrg ).
      WHEN '-'.  pt_bt-betrg = pt_bt-betrg * -1.
      WHEN OTHERS.
    ENDCASE.

    CASE p_evfld.
      WHEN '1'.  "Amount
        ch_betrg = ch_betrg + pt_bt-betrg.
      WHEN OTHERS.  "Number or Rate
        EXIT.
    ENDCASE.
  ENDLOOP.

ENDFORM.                    " READ_BT
*&---------------------------------------------------------------------*
*&      Form  READ_DDNTK
*&---------------------------------------------------------------------*
FORM read_ddntk  TABLES   pt_ddntk STRUCTURE pc23e
                  USING   p_lgart  p_evfld  p_sign
                CHANGING   ch_betrg.

* Calculate the amount
  LOOP AT pt_ddntk WHERE lgart = p_lgart.

* Sign ('+' or '-')
    CASE p_sign.
      WHEN '+'.  pt_ddntk-betrg = abs( pt_ddntk-betrg ).
      WHEN '-'.  pt_ddntk-betrg = pt_ddntk-betrg * -1.
      WHEN OTHERS.
    ENDCASE.

    CASE p_evfld.
      WHEN '1'.  "Amount
        ch_betrg = ch_betrg + pt_ddntk-betrg.
      WHEN OTHERS.  "Number or Rate
        EXIT.
    ENDCASE.
  ENDLOOP.


ENDFORM.                    " READ_DDNTK
*&---------------------------------------------------------------------*
*&      Form  READ_ACCR
*&---------------------------------------------------------------------*
FORM read_accr  TABLES   pt_accr STRUCTURE pc23g
                 USING   p_lgart  p_evfld  p_sign
              CHANGING   ch_betrg.

* Calculate the amount
  LOOP AT pt_accr WHERE lgart = p_lgart.
* Sign ('+' or '-')
    CASE p_sign.
      WHEN '+'.
        pt_accr-betrg = abs( pt_accr-betrg ).
        pt_accr-anzhl = abs( pt_accr-anzhl ).
      WHEN '-'.
        pt_accr-betrg = pt_accr-betrg * -1.
        pt_accr-anzhl = pt_accr-anzhl * -1.
      WHEN OTHERS.
    ENDCASE.

    CASE p_evfld.
      WHEN '1'.  "Amount
        ch_betrg = ch_betrg + pt_accr-betrg.
      WHEN '2'.  "Number
        ch_betrg = ch_betrg + pt_accr-anzhl.
      WHEN OTHERS.  "Number or Rate
        EXIT.
    ENDCASE.
  ENDLOOP.

ENDFORM.                    " READ_ACCR
*&---------------------------------------------------------------------*
*&      Form  FILL_TEXT2
*&---------------------------------------------------------------------*
FORM fill_text2  USING   p_beg02  p_end02
              CHANGING   pt_tab2 LIKE gt_tab.

  DATA : l_pernr LIKE p0001-pernr.

* Employment Status
  SELECT SINGLE a~stat2 b~text1
           INTO (pt_tab2-stat2, pt_tab2-statx)
           FROM pa0000 AS a INNER JOIN t529u AS b
             ON a~stat2 = b~statv
          WHERE a~pernr  = pt_tab2-pernr
            AND a~endda >= p_end02
            AND a~begda <= p_end02
            AND b~sprsl  = sy-langu
            AND b~statn  = '2'.

ENDFORM.                    " FILL_TEXT2
*&---------------------------------------------------------------------*
*&      Form  SET_DISPLAY_NO
*&---------------------------------------------------------------------*
FORM set_display_no  TABLES   pt_0029 STRUCTURE zthr0029.
*  FIELD-SYMBOLS <fs_10> TYPE ANY.
*  DATA : lt_nametab TYPE pmget_name_tab,
*         ls_nametab LIKE LINE OF lt_nametab,
*         l_str10(30),
*         l_num03(3) TYPE n,
*         lt_0029 LIKE zthr0029 OCCURS 0 WITH HEADER LINE.
*
** Structure to display
*  CALL METHOD zcl_hr_comclass=>get_nametab
*    EXPORTING
*      i_tabname = 'ZSHR0240_EGREP'
*    IMPORTING
*      e_nametab = lt_nametab.
*
**- Adaption of defined fields on selection-screen
*  CLEAR l_num03.
*  LOOP AT lt_nametab INTO ls_nametab.
** leave the fixed fields for each period
*    CLEAR pt_0029-grptx.
*    CONCATENATE 'P_' ls_nametab-fieldname INTO pt_0029-grptx.
*    CASE ls_nametab-fieldname(3).
*      WHEN 'PAY' OR 'FPP' OR 'INP' OR 'OCR' or 'STATX'.
*        IF p_each = 'X'.
*          pt_0029-lgagp = ls_nametab-position.
*          ADD 1 TO l_num03.
*        ENDIF.
*        APPEND pt_0029.  CLEAR pt_0029.
*        CONTINUE.
*      WHEN OTHERS.
*    ENDCASE.
*
*    CLEAR l_str10.
*    CONCATENATE 'P_' ls_nametab-fieldname INTO l_str10.
*    ASSIGN (l_str10) TO <fs_10>.
*    CHECK sy-subrc = 0.
*
*    pt_0029-grptx = l_str10.
*    IF <fs_10> IS INITIAL.
*      pt_0029-lgagp = <fs_10>.
*    ELSE.
*      pt_0029-lgagp = <fs_10> + l_num03.
*    ENDIF.
*    APPEND pt_0029.  CLEAR pt_0029.
*
** Defined name on selection screen is only 1,
**  but Last/First Name and Address is always go with together
*    IF <fs_10> IS NOT INITIAL.
*      pt_0029-lgagp = <fs_10> + l_num03.
*    ELSE.
*      CLEAR pt_0029-lgagp.
*    ENDIF.
*  ENDLOOP.
*  REFRESH lt_0029.
*  lt_0029[] = pt_0029[].
*  SORT lt_0029 STABLE BY lgagp lengh.
*
** Rearrangement to recharge the missed number
*  CLEAR l_num03.
*  LOOP AT lt_0029 WHERE lgagp IS NOT INITIAL.
*    ADD 1 TO l_num03.
*    lt_0029-lgagp = l_num03.
*    MODIFY lt_0029.
*  ENDLOOP.
*
*  CLEAR l_num03.
*  LOOP AT pt_0029 WHERE lgagp IS NOT INITIAL.
*    READ TABLE lt_0029 WITH KEY grptx = pt_0029-grptx.
*    CHECK sy-subrc = 0.
*    pt_0029-lgagp = lt_0029-lgagp.
*    MODIFY pt_0029.
*  ENDLOOP.

ENDFORM.                    " SET_DISPLAY_NO
*&---------------------------------------------------------------------*
*&      Form  CLEAR_VALUE
*&---------------------------------------------------------------------*
FORM clear_value  TABLES   pt_0029 STRUCTURE gt_0029
                   USING   p_result.

  LOOP AT pt_0029 WHERE lgagp IS INITIAL.
    ASSIGN COMPONENT sy-tabix OF STRUCTURE p_result TO <fs_01>.

    CLEAR <fs_01>.
  ENDLOOP.

ENDFORM.                    " CLEAR_VALUE
*&---------------------------------------------------------------------*
*&      Form  ALV_VARIANT_F4
*&---------------------------------------------------------------------*
FORM alv_variant_f4 CHANGING p_vari.
  DATA: rs_variant LIKE disvariant,
        lv_nof4 TYPE c.

  CLEAR lv_nof4.
  LOOP AT SCREEN.
    IF screen-name = 'PA_VARI'.
      IF screen-input = 0.
        lv_nof4 = 'X'.
      ENDIF.
    ENDIF.
  ENDLOOP.

  CLEAR rs_variant.
  rs_variant-report   = sy-repid.
  rs_variant-username = sy-uname.

  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
    EXPORTING
      is_variant = rs_variant
      i_save     = 'A'
    IMPORTING
      es_variant = rs_variant
    EXCEPTIONS
      OTHERS     = 1.

  IF sy-subrc = 0 AND lv_nof4 = space.
    p_vari = rs_variant-variant.
  ENDIF.

ENDFORM.                    " ALV_VARIANT_F4
*&---------------------------------------------------------------------*
*&      Form  CALL_FUNCTION
*&---------------------------------------------------------------------*
FORM call_function TABLES p_gt_result
                    USING p_user_command  p_alv_pf_status_set.
  sy-lsind = sy-lsind - 1.
  g_repid  = sy-repid.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'  "'REUSE_ALV_GRID_DISPLAY'
   EXPORTING
*     i_interface_check                 = ' '
*     i_buffer_active                   = 'X'
     i_callback_program                = g_repid
     i_callback_pf_status_set          = p_alv_pf_status_set
     i_callback_user_command           = p_user_command
     i_callback_top_of_page            = 'ALV_TOP_OF_LIST'
     i_grid_settings                   = gt_gridset
     is_layout_lvc                     = gs_layout
     it_fieldcat_lvc                   = gt_fieldcat_lvc
     it_sort_lvc                       = gt_sort_lvc
*     it_filter                         = gt_filter
     it_events                         = gt_events_lvc[]
     i_save                            = 'A'
     is_variant                        = gs_variant
   TABLES
     t_outtab                          = p_gt_result
   EXCEPTIONS
     program_error                     = 1
     OTHERS                            = 2.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " call_function
*&---------------------------------------------------------------------*
*&      Form  FIEIDCAT_GATHERING
*&---------------------------------------------------------------------*
FORM fieidcat_gathering.

  gs_layout-zebra      = 'X'.
  gs_layout-cwidth_opt = 'X'.
  gs_layout-stylefname = 'CELLTAB'.

* Fieldcat
  REFRESH : gt_fieldcat_lvc.
  CLEAR : gt_fieldcat_lvc, g_tabname.

  g_tabname = 'ZSHR0240_EGREP'.
  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
*     i_buffer_active        = 'X'
      i_structure_name       = g_tabname
*     I_INTERNAL_TABNAME     = 'GT_RESULT'
      i_client_never_display = 'X'
      i_bypassing_buffer     = 'X'
    CHANGING
      ct_fieldcat            = gt_fieldcat_lvc[]
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

* Rearrange the field catalog
  LOOP AT gt_fieldcat_lvc INTO gs_fieldcat_lvc.

    IF gs_fieldcat_lvc-fieldname(3) = 'BET'.
      READ TABLE gt_temp2 INDEX gs_fieldcat_lvc-fieldname+3(3).
      IF sy-subrc = 0.
        CLEAR g_betrg.
        CONCATENATE 'BET' gs_fieldcat_lvc-fieldname+3(3) INTO g_betrg.

        gs_fieldcat_lvc-fieldname = g_betrg.
        gs_fieldcat_lvc-outputlen = gt_temp2-lengh.
        gs_fieldcat_lvc-reptext  = gs_fieldcat_lvc-scrtext_s =
        gs_fieldcat_lvc-scrtext_m = gs_fieldcat_lvc-scrtext_l =
        gt_temp2-grptx.
        gs_fieldcat_lvc-emphasize = 'C200'.
        gs_fieldcat_lvc-no_zero   = 'X'.
        gs_fieldcat_lvc-no_out    = ''.
        gs_fieldcat_lvc-do_sum    = 'X'.
      ELSE.
        gs_fieldcat_lvc-no_out    = 'X'.
      ENDIF.

    ELSE.
      CASE gs_fieldcat_lvc-fieldname.
        WHEN 'PAYDT' OR 'FPPER' OR 'OCRSN'
             OR 'INPER'.
          IF p_each = 'X'.  "Each Period
            gs_fieldcat_lvc-fix_column = 'X'.
            gs_fieldcat_lvc-emphasize  = 'C100'.
          ELSE.
            gs_fieldcat_lvc-no_out     = 'X'.
          ENDIF.
        WHEN 'PERNR' OR 'STATX'.
          gs_fieldcat_lvc-fix_column = 'X'.
          gs_fieldcat_lvc-emphasize  = 'C100'.
        WHEN 'CPID'.
          gs_fieldcat_lvc-fix_column = 'X'.
          gs_fieldcat_lvc-emphasize  = 'C100'.
          gs_fieldcat_lvc-reptext  = gs_fieldcat_lvc-scrtext_s =
       gs_fieldcat_lvc-scrtext_m = gs_fieldcat_lvc-scrtext_l =
          gs_fieldcat_lvc-coltext = 'CP ID'.
       WHEN OTHERS.
          gs_fieldcat_lvc-no_out     = 'X'.
          gs_fieldcat_lvc-emphasize  = 'C300'.
      ENDCASE.
    ENDIF.

    MODIFY gt_fieldcat_lvc FROM gs_fieldcat_lvc.
  ENDLOOP.

ENDFORM.                    " fieidcat_gathering

*&---------------------------------------------------------------------
*&        Form  alv_top_of_list
*&---------------------------------------------------------------------
FORM alv_top_of_list.
  DATA: gt_text      TYPE slis_t_listheader,
        st_textline  TYPE slis_listheader.

  ULINE.

* Title
  st_textline-typ = 'H'.
  WRITE 'Title' TO st_textline-key.
  WRITE g_title TO st_textline-info.
  APPEND st_textline TO gt_text.
  CLEAR st_textline.

* Period
  CLEAR g_info1.
  st_textline-typ = 'S'.
  WRITE : 'Period' TO st_textline-key.
  CALL METHOD zcl_hr_comclass=>convert_date_time
    EXPORTING
      i_clasf = '3'
      i_begda = r_begda-low
      i_endda = r_begda-high
    IMPORTING
      e_datum = g_info1.
  st_textline-info = g_info1.
  APPEND st_textline TO gt_text.
  CLEAR st_textline.

* Print Date & Time
  st_textline-typ = 'S'.
  WRITE : 'Print date' TO st_textline-key.
  write: sy-datum MM/DD/YYYY to st_textline-info.
  CLEAR : g_info1, g_info2.

  APPEND st_textline TO gt_text.
  CLEAR st_textline.

* Prepared by
  st_textline-typ = 'S'.
  WRITE : 'Prepared by' TO st_textline-key.
*  CALL METHOD zcl_hr_common=>get_uname
*    EXPORTING
*      i_usrid = sy-uname
*      i_datum = r_begda-high
*    IMPORTING
*      e_usrnm = g_usrnm.
  st_textline-info = sy-uname.
  APPEND st_textline TO gt_text.
  CLEAR st_textline.

* Count
  st_textline-typ = 'S'.
  WRITE : 'Count' TO st_textline-key.
  MOVE g_cnt TO st_textline-info.
  APPEND st_textline TO gt_text.
  CLEAR st_textline.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = gt_text.
*            i_logo             = 'ICON_IMPORT'.
  ULINE.


ENDFORM.                    " alv_top_of_list
*&---------------------------------------------------------------------
*         Form  ALV_PF_STATUS_SET
*&---------------------------------------------------------------------
FORM alv_pf_status_set USING rt_extab TYPE slis_t_extab.
  DATA: st_extab TYPE slis_extab.                           "0363012

* Button Exception
*  MOVE '&ALL' TO rt_extab_wa-fcode.                         "0363012
*  APPEND rt_extab_wa TO rt_extab.                           "0363012
*  MOVE '&SAL' TO rt_extab_wa-fcode.                         "0363012
*  APPEND rt_extab_wa TO rt_extab.                           "0363012

  SET PF-STATUS '100'." EXCLUDING st_extab.

ENDFORM.                    " ALV_PF_STATUS_SET
*&---------------------------------------------------------------------
*         Form  ALV_USER_COMMAND
*&---------------------------------------------------------------------
FORM alv_user_command USING r_ucomm     LIKE sy-ucomm
                            rs_selfield TYPE slis_selfield.
  DATA: rt_extab TYPE slis_t_extab.

* Event 'ALV_DATA_CHANGED' is happened..
  DATA : l_valid TYPE char01.
  PERFORM alv_exec_data_changed CHANGING l_valid.

  CASE r_ucomm.
    WHEN 'RBACK' OR 'RCANC'.
      CLEAR : r_ucomm.
      SET SCREEN 0.

    WHEN '&IC1'.
      CLEAR gt_result.
      READ TABLE gt_result INDEX rs_selfield-tabindex.

    WHEN 'REXIT'.
      LEAVE  PROGRAM.
      CLEAR : r_ucomm.
  ENDCASE.

  rs_selfield-refresh    = 'X'.
  rs_selfield-col_stable = 'X'.
  rs_selfield-row_stable = 'X'.

ENDFORM.                    " alv_user_command
*&---------------------------------------------------------------------*
*&      Form  GET_PAYROLL_PERIOD_SEL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_OLD_ABKRS  text
*      <--P_W_PERMO  text
*      <--P_S_BEGDA_LOW  text
*      <--P_S_BEGDA_HIGH  text
*      <--P_W_ABKRT  text
*----------------------------------------------------------------------*
FORM get_payroll_period_sel  USING    v_abkrs
                             CHANGING v_permo
                                      v_begda
                                      v_endda
                                      v_abkrt.
  SELECT SINGLE * INTO *t549q
               FROM  t549q
               WHERE  permo  EQ v_permo
               AND    begda  LE v_begda
               AND    endda  GE v_begda.

  CHECK sy-subrc EQ 0.
  v_pabrj = *t549q-pabrj.
  v_pabrp = *t549q-pabrp .

  CALL FUNCTION 'PA03_PERIODDATES_GET'
    EXPORTING
      f_abkrs               = v_abkrs
    IMPORTING
      f_permo               = v_permo
      f_current_begda       = v_begda
      f_current_endda       = v_endda
      f_abkrs_text          = v_abkrt
    CHANGING
      f_current_period      = v_pabrp
      f_current_year        = v_pabrj
    EXCEPTIONS
      pcr_does_not_exist    = 1
      abkrs_does_not_exist  = 2
      period_does_not_exist = 3
      OTHERS                = 4.
  IF sy-subrc <> 0.

  ENDIF.

  CALL FUNCTION 'HR_GB_PAY_DATE'
    EXPORTING
      molga              = '10'
      abkrs              = p_abkrs
      permo              = v_permo
      pabrj              = v_pabrj
      pabrp              = v_pabrp
    IMPORTING
      pdate              = p_date
    EXCEPTIONS
      pay_date_not_found = 1
      OTHERS             = 2.

  IF sy-subrc <> 0.
  ENDIF.

ENDFORM.                    " GET_PAYROLL_PERIOD_SEL
