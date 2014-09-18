*&---------------------------------------------------------------------*
*&  Include           ZCFIT01_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  INIT_SCREEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form INIT_SCREEN .

*// screen initial Setting
  p_month = sy-datum+4(2).
  p_perid = '04'.

  GET PARAMETER ID 'BUK' FIELD P_BUKRS.

  IF P_BUKRS IS INITIAL.
    P_BUKRS = 'H201'.
  ENDIF.

  loop at screen.
    if screen-name = 'P_BUTXT'.
      screen-input  = 0.
      screen-intensified = '0'.
      screen-display_3d  = '0'.
      modify screen.
    endif.
    if screen-name = 'P_BUKRS'.
      screen-input = ' '.
      modify screen.
    endif.
  endloop.


* & find text.
  perform fi_wt_read_t001 using    p_bukrs
                          changing p_butxt.

endform.                    " INIT_SCREEN
*&---------------------------------------------------------------------*
*&      Form  FI_WT_READ_T001
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_BUKRS  text
*      <--P_P_BUTXT  text
*----------------------------------------------------------------------*
form FI_WT_READ_T001  using    pa_bukrs
                      changing pa_butxt.

  data : it_t001 like t001.

  call function 'FI_WT_READ_T001'
    exporting
      i_bukrs   = pa_bukrs
    importing
      t_t001    = it_t001
    exceptions
      not_found = 1.

  case sy-subrc.
    when 0.
      pa_butxt = it_t001-butxt.
    when 1.
      message s101(f5).
    when others.
  endcase.

endform.                    " FI_WT_READ_T001
************************************************************************
* Form  PF_STATUS_SET
************************************************************************
FORM  pf_status_set USING p_rt_extab TYPE slis_t_extab.

  SET PF-STATUS 'STANDARD'.

ENDFORM.
************************************************************************
* AT USER-COMMAND                                                     *
************************************************************************
FORM user_command USING p_ucomm    LIKE sy-ucomm
                        p_selfield TYPE slis_selfield.

  CASE p_ucomm.
    WHEN '&DATA_SAVE'.
      PERFORM delete_cm.
      PERFORM save_cm.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  get_target_fm
*&---------------------------------------------------------------------*
FORM get_target_fm.

  PERFORM setting_period.
  PERFORM get_default_waers.

  CLEAR: imap1[], imap1.

*// == Maintenance view for budget to cash
  IF p_im = 'X'.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE imap1
*  select * APPENDING CORRESPONDING FIELDS OF TABLE  imap1
             FROM ztfi_map1
             WHERE bukrs = p_bukrs
               AND gtype = '2'.       "Budget type
  ELSE.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE imap1
             FROM ztfi_map1
             WHERE bukrs = p_bukrs
               AND gtype = '1'.       "Budget type
  ENDIF.

ENDFORM.                    " get_target_fm
*&---------------------------------------------------------------------*
*&      Form  get_fm_detail
*&---------------------------------------------------------------------*
FORM get_fm_detail.

  CLEAR: it_fm01[], it_fm01, it_fm02[], it_fm02,
         r_gcode1[], r_gcode1, r_gcode2[], r_gcode2.

  r_gcode1-sign = 'I'.    r_gcode1-option = 'EQ'.
  r_gcode2-sign = 'I'.    r_gcode2-option = 'EQ'.

*..get CI & Fund master
  SORT imap1 BY gtype.

  LOOP AT imap1.
    CLEAR: r_fund[], r_fund.
    r_fund-sign = 'I'.  r_fund-option = 'CP'.

    CASE  imap1-gtype.
      WHEN '1'. "commitment item
        SELECT fipos posit INTO (fmfpo-fipos, fmfpo-posit)
               FROM fmfpo
               WHERE fikrs EQ p_bukrs "fm area = company code ?
               AND   fipos >= imap1-frtrm AND fipos <= imap1-totrm.
*              AND   fictr IN rfistl.  "fund center
          IF sy-subrc EQ 0.
            MOVE-CORRESPONDING imap1  TO it_fm01.
            MOVE : fmfpo-fipos        TO it_fm01-gcode1, "commitmnt item
                   fmfpo-posit        TO it_fm01-gcode2. "item code

            r_gcode1-low  = it_fm01-gcode1.  "commitment item
            r_gcode1-high = it_fm01-gcode2.  "===> item code *********
            APPEND r_gcode1.
            r_gcode2-low = it_fm01-gcode2.
            APPEND r_gcode2.
            APPEND it_fm01. CLEAR it_fm01.
          ENDIF.
        ENDSELECT.

* Fund Center
        SELECT * INTO CORRESPONDING FIELDS OF TABLE it_fmfctr
          FROM fmfctr
         WHERE fikrs EQ p_bukrs
           AND fictr IN rfistl.  "fund center

* Fund Center Hiearchy (select end node only)
        LOOP AT it_fmfctr.
          SELECT SINGLE * FROM fmhictr
             WHERE ctr_objnr = it_fmfctr-ctr_objnr.

          IF fmhictr-parent_obj = space.
            DELETE it_fmfctr.
          ELSE.
*-- end node...
*--- check FundCenter Auth.
            SELECT SINGLE * FROM fmfctr
                WHERE fikrs EQ p_bukrs
                  AND fictr EQ it_fmfctr-fictr
                  AND datbis >= sy-datum.

            AUTHORITY-CHECK OBJECT 'Z_FICTR'
                     ID 'FM_FIKRS'   FIELD p_bukrs
                     ID 'FM_FICTR'   FIELD fmfctr-fictr.

            IF sy-subrc <> 0.
              DELETE it_fmfctr.
            ENDIF.

          ENDIF.
        ENDLOOP.

      WHEN '2'.
*       r_fund-low = imap1-frtrm.   APPEND r_fund.
        CONCATENATE '+' imap1-frtrm INTO r_fund-low.
        APPEND r_fund.

        SELECT * FROM ztfi_imfm
         WHERE ayear IN s_gjahr
           AND posid IN r_fund. "fund

          IF sy-subrc EQ 0.
            MOVE-CORRESPONDING imap1      TO it_fm01.
            MOVE : ztfi_imfm-posid+1(10)  TO it_fm01-gcode1.  "fund

            r_gcode1-low = it_fm01-gcode1.
            APPEND r_gcode1.
            APPEND it_fm01. CLEAR it_fm01.

* ==========> fund budget: released amount
            IF ( ztfi_imfm-gubun  EQ '1' OR
                 ztfi_imfm-gubun  EQ '2' OR
                 ztfi_imfm-gubun  EQ '3' OR
                 ztfi_imfm-gubun  EQ '5' )
            AND  ztfi_imfm-status EQ 'A'.  "approval
              MOVE-CORRESPONDING ztfi_imfm TO i_zimfm.
              APPEND i_zimfm. CLEAR i_zimfm.
            ENDIF.
          ENDIF.

        ENDSELECT.


*        SELECT geber INTO bppe-geber
*               FROM bppe
*               WHERE gjahr IN s_gjahr
*               AND   geber IN r_fund. "fund
*          IF sy-subrc EQ 0.
*            MOVE-CORRESPONDING imap1  TO it_fm01.
*            MOVE : bppe-geber         TO it_fm01-gcode1.  "fund
*
*            r_gcode1-low = it_fm01-gcode1.
*            APPEND r_gcode1.
*            APPEND it_fm01. CLEAR it_fm01.
*          ENDIF.
*        ENDSELECT.
    ENDCASE.
  ENDLOOP.

* fill total budget/actuals itab.
  PERFORM fill_amount_tab.

* select-option
  DELETE it_fm01 WHERE gtype EQ '1'
                 AND   NOT gcode1 IN rfipex.  "ci
  DELETE it_fm01 WHERE gtype EQ '2'
                 AND   NOT gcode1 IN rfonds.  "fund
*  LOOP AT it_fm01.
*    CASE it_fm01-gtype.
*      WHEN '1'.
*        IF NOT it_fm01-gcode1 IN rfipex. "ci
*          DELETE it_fm01.  CONTINUE.
*        ENDIF.
*        IF NOT it_fm01-gcode2 IN r_gcode2."item code
*          DELETE it_fm01.
*        ENDIF.
*      WHEN '2'.
*        IF NOT it_fm01-gcode1 IN rfonds.  "fund
*          DELETE it_fm01.
*        ENDIF.
*    ENDCASE.
*  ENDLOOP.

* assign amount
  LOOP AT it_fm01.
    CASE it_fm01-gtype.
      WHEN '1'.  "commitment item
*.......CI budget: released amount
        PERFORM get_released_commitmnt.
*.......CI actuals: commitemnt & invoice
        IF p_yearly EQ space.
          PERFORM get_actual_commitmnt.
        ENDIF.

      WHEN '2'.  "fund
*.......Fudn budget: released amount
        PERFORM get_released_fund.
*.......Fund actuals: commitemnt & invoice
        IF p_yearly EQ space.
          PERFORM get_actual_fund.
        ENDIF.
    ENDCASE.
  ENDLOOP.

  DELETE it_fm02 WHERE rlamt IS initial
                 AND   acamt IS initial.

ENDFORM.                    " get_fm_detail
*&---------------------------------------------------------------------*
*&      Form  setting_period
*&---------------------------------------------------------------------*
FORM setting_period.

  DATA: l_mon(2) TYPE n.

  s_datum-sign = 'I'.    s_datum-option = 'EQ'.
  s_gjahr-sign = 'I'.    s_gjahr-option = 'BT'.

  CONCATENATE p_gjahr p_month '01' INTO s_datum-low.
  APPEND s_datum.
  s_gjahr-low = p_gjahr.

  l_mon = p_perid - 1.
  DO l_mon TIMES.
    CALL FUNCTION 'MONTH_PLUS_DETERMINE'
         EXPORTING
              months  = 1
              olddate = s_datum-low
         IMPORTING
              newdate = s_datum-low.
    APPEND s_datum.
  ENDDO.

  LOOP AT s_datum.
    CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
         EXPORTING
              day_in            = s_datum-low
         IMPORTING
              last_day_of_month = s_datum-high.
    MODIFY s_datum.
  ENDLOOP.

  s_gjahr-high = s_datum-high(4).
  APPEND s_gjahr.

ENDFORM.                    " setting_period
*&---------------------------------------------------------------------*
*&      Form  GET_RELEASED_COMMITMNT
*&---------------------------------------------------------------------*
FORM get_released_commitmnt.

  DATA: l_release_amt LIKE it_fm02-rlamt.

  IF p_yearly EQ 'X'.  "yearly forecasting

    LOOP AT i_bppe WHERE posit EQ it_fm01-gcode2
                   AND   vorga EQ 'KBUD'.  "orgin amount

      LOOP AT s_datum WHERE low(4) EQ i_bppe-gjahr.

        AT NEW low.
          CLEAR: l_amt, fs_wtp.
        ENDAT.

        CONCATENATE 'I_BPPE-WTP' s_datum-low+4(2) INTO fs_wtp.
        ASSIGN (fs_wtp) TO <fs_wtp>.
        l_amt = l_amt + <fs_wtp>.

        MOVE-CORRESPONDING it_fm01 TO it_fm02.
        MOVE : l_amt               TO it_fm02-rlamt.
        CONCATENATE s_datum-low(4) s_datum-low+4(2)
                                   INTO it_fm02-month SEPARATED BY '/'.
        l_release_amt = it_fm02-rlamt.

        COLLECT it_fm02.  CLEAR it_fm02.
      ENDLOOP.

      DELETE i_bppe.  CLEAR i_bppe.
    ENDLOOP.

  ELSE.
*==> 2004/04/29 only current amount calcurate.
*    LOOP AT i_bppe WHERE posit EQ it_fm01-gcode2
*                   AND   vorga EQ 'KBFR'.  "released amount
*      LOOP AT s_datum WHERE low(4) EQ i_bppe-gjahr.
*        AT NEW low. CLEAR: l_amt, fs_wtp. ENDAT.
*
*        CONCATENATE 'I_BPPE-WTP' s_datum-low+4(2) INTO fs_wtp.
*        ASSIGN (fs_wtp) TO <fs_wtp>.
*        l_amt = l_amt + <fs_wtp>.
*
*        MOVE-CORRESPONDING it_fm01 TO it_fm02.
*        MOVE : l_amt               TO it_fm02-rlamt.
*        CONCATENATE s_datum-low(4) s_datum-low+4(2)
*                                   INTO it_fm02-month SEPARATED BY '/'.
*        l_release_amt = it_fm02-rlamt.
*
*        COLLECT it_fm02.  CLEAR it_fm02.
*      ENDLOOP.
*      DELETE i_bppe.  CLEAR i_bppe.
*    ENDLOOP.
** if release amount is initial, calculate 'current amount'.
**  => current amount = orgin + supp. + return + transfer
*    LOOP AT it_fm02 WHERE rlamt IS initial.

    LOOP AT i_bppe WHERE posit EQ it_fm01-gcode2
                   AND ( vorga EQ 'KBUD' OR "orgin amount
                         vorga EQ 'KBNO' OR "supp.
                         vorga EQ 'KBRO' OR "return
                         vorga EQ 'KBUS' OR "transfer
                         vorga EQ 'KBUE' ). "transfer

      LOOP AT s_datum WHERE low(4) EQ i_bppe-gjahr.
*          CHECK s_datum-low+4(2) EQ it_fm02-month+5(2).

        AT NEW low.
          CLEAR: l_amt, fs_wtp.
        ENDAT.

        CONCATENATE 'I_BPPE-WTP' s_datum-low+4(2) INTO fs_wtp.
        ASSIGN (fs_wtp) TO <fs_wtp>.
        l_amt = l_amt + <fs_wtp>.

        MOVE-CORRESPONDING it_fm01 TO it_fm02.
        MOVE : l_amt               TO it_fm02-rlamt.
        CONCATENATE s_datum-low(4) s_datum-low+4(2)
                    INTO it_fm02-month SEPARATED BY '/'.
        COLLECT it_fm02.  CLEAR it_fm02.
      ENDLOOP.

      DELETE i_bppe.  CLEAR i_bppe.
    ENDLOOP.
*    ENDLOOP.
  ENDIF.

*   if it_fm02-rlamt IS INITIAL.
*      LOOP AT i_bppe WHERE posit EQ it_fm01-gcode2
*                     AND ( vorga EQ 'KBUD' OR "orgin amount
*                           vorga EQ 'KBNO' OR "supp.
*                           vorga EQ 'KBRO' OR "return
*                           vorga EQ 'KBUS' OR "transfer
*                           vorga EQ 'KBUE' ). "transfer
*        LOOP AT s_datum WHERE low(4) EQ i_bppe-gjahr.
*          AT NEW low. CLEAR: l_amt, fs_wtp. ENDAT.
*
*          CONCATENATE 'I_BPPE-WTP' s_datum-low+4(2) INTO fs_wtp.
*          ASSIGN (fs_wtp) TO <fs_wtp>.
*          l_amt = l_amt + <fs_wtp>.
*
*          MOVE-CORRESPONDING it_fm01 TO it_fm02.
*          MOVE : l_amt               TO it_fm02-rlamt.
*          CONCATENATE s_datum-low(4) s_datum-low+4(2)
*                                 INTO it_fm02-month SEPARATED BY '/'.
*          COLLECT it_fm02.  CLEAR it_fm02.
*        ENDLOOP.
*        DELETE i_bppe.  CLEAR i_bppe.
*      ENDLOOP.
*    ENDIF.
*  ENDIF.

*  l_prd = p_perid.    l_mon = p_month.
*
*  SELECT * FROM bppe
*           WHERE posit EQ it_fm01-gcode2 "item code
*           AND   gjahr IN s_gjahr
*           AND   vorga EQ 'KBFR'. "released
*    CLEAR: l_amt, fs_wtp.
*    l_gjahr = bppe-gjahr.
*    IF l_idx > 12.
*      IF l_gjahr EQ bppe-gjahr. CONTINUE. ENDIF.
*      l_prd = l_idx - p_month.
*      l_mon = '01'.
*    ENDIF.
*    l_idx = l_mon.
*    DO l_prd TIMES.
*      CHECK l_idx <= 12.
*      CONCATENATE 'BPPE-WTP' l_idx INTO fs_wtp.
*      ASSIGN (fs_wtp) TO <fs_wtp>.
*      l_amt = l_amt + <fs_wtp>.
*
*      MOVE-CORRESPONDING it_fm01 TO it_fm02.
*      MOVE : l_amt               TO it_fm02-rlamt.
*      CONCATENATE s_gjahr-low '/' l_idx INTO it_fm02-month.
*      COLLECT it_fm02.
*
*      l_idx = l_idx + 1.
*    ENDDO.
*  ENDSELECT.

ENDFORM.                    " GET_RELEASED_COMMITMNT
*&---------------------------------------------------------------------*
*&      Form  GET_ACTUAL_COMMITMNT
*&---------------------------------------------------------------------*
FORM get_actual_commitmnt.

  LOOP AT i_fmit01 WHERE rfipex EQ it_fm01-gcode1.

    LOOP AT s_datum WHERE low(4) EQ i_fmit01-ryear.

      AT NEW low.
        CLEAR: l_amt, fs_wtp.
      ENDAT.

      CONCATENATE 'I_FMIT01-TSL' s_datum-low+4(2) INTO fs_wtp.
      ASSIGN (fs_wtp) TO <fs_wtp>.
      l_amt = l_amt + <fs_wtp>.

      MOVE-CORRESPONDING it_fm01 TO it_fm02.
      MOVE : l_amt               TO it_fm02-acamt.
      CONCATENATE s_datum-low(4) s_datum-low+4(2)
                                 INTO it_fm02-month SEPARATED BY '/'.
      COLLECT it_fm02.  CLEAR it_fm02.
    ENDLOOP.

    DELETE i_fmit01.  CLEAR i_fmit01.

  ENDLOOP.

*  l_prd = p_perid.    l_mon = p_month.
*
*  SELECT * FROM fmit
*           WHERE rfipex EQ it_fm01-gcode1  "commitment item
*           AND   ryear  IN s_gjahr
*           AND   rwrttp IN ('50','51','54','60','61','95').
*    IF fmit-rwrttp EQ '54' OR
*       fmit-rwrttp EQ '66' OR
*       fmit-rwrttp EQ '95'.
*      CHECK fmit-rbtart = '0100'.
*    ENDIF.
*
*    CLEAR: l_amt, fs_wtp.
*    l_gjahr = fmit-ryear.
*    IF l_idx > 12.
*      IF l_gjahr EQ fmit-ryear. CONTINUE. ENDIF.
*      l_prd = l_idx - p_month.
*      l_mon = '01'.
*    ENDIF.
*    l_idx = l_mon.
*    DO l_prd TIMES.
*      CHECK l_idx <= 12.
*      CONCATENATE 'FMIT-TSL' l_idx INTO fs_wtp.
*      ASSIGN (fs_wtp) TO <fs_wtp>.
*      l_amt = l_amt + <fs_wtp>.
*
*      MOVE-CORRESPONDING it_fm01 TO it_fm02.
*      MOVE : l_amt               TO it_fm02-acamt.
*      CONCATENATE s_gjahr-low '/' l_idx INTO it_fm02-month.
*      COLLECT it_fm02.
*
*      l_idx = l_idx + 1.
*    ENDDO.
*  ENDSELECT.

ENDFORM.                    " GET_ACTUAL_COMMITMNT
*&---------------------------------------------------------------------*
*&      Form  GET_RELEASED_FUND
*&---------------------------------------------------------------------*
FORM get_released_fund.

  CONCATENATE '+' it_fm01-gcode1 INTO l_posid.

  LOOP AT i_zimfm WHERE posid CP l_posid.

    LOOP AT s_datum WHERE low(4) EQ i_zimfm-ayear
                    AND   low(4) EQ i_zimfm-gjahr.
      AT NEW low.
        CLEAR: l_amt, fs_wtp.
      ENDAT.

      CONCATENATE 'I_ZIMFM-WTP' s_datum-low+4(2) INTO fs_wtp.
      ASSIGN (fs_wtp) TO <fs_wtp>.
      l_amt = l_amt + <fs_wtp>.

      MOVE-CORRESPONDING it_fm01 TO it_fm02.
      MOVE : l_amt               TO it_fm02-rlamt.
      CLEAR it_fm02-acamt.
      CONCATENATE s_datum-low(4) s_datum-low+4(2)
                                 INTO it_fm02-month SEPARATED BY '/'.
      COLLECT it_fm02.  CLEAR it_fm02.
    ENDLOOP.

    DELETE i_zimfm.   CLEAR i_zimfm.

  ENDLOOP.

*  l_prd = p_perid.    l_mon = p_month.
*
*  CONCATENATE '%' it_fm01-gcode1 INTO l_posid.
*  SELECT * FROM ztfi_imfm
*           WHERE posid LIKE l_posid
*           AND   ayear IN s_gjahr
*           AND   gubun IN ('1','2','3','5')
*           AND   status EQ 'A'.
*    CLEAR: l_amt, fs_wtp.
*    l_gjahr = ztfi_imfm-ayear.
*    IF l_idx > 12.
*      IF l_gjahr EQ ztfi_imfm-ayear. CONTINUE. ENDIF.
*      l_prd = l_idx - p_month.
*      l_mon = '01'.
*    ENDIF.
*    DO l_prd TIMES.
*      CHECK l_idx <= 12.
*      CONCATENATE 'ZTFI_IMFM-WTP' l_idx INTO fs_wtp.
*      ASSIGN (fs_wtp) TO <fs_wtp>.
*      l_amt = l_amt + <fs_wtp>.
*
*      MOVE-CORRESPONDING it_fm01 TO it_fm02.
*      MOVE : l_amt               TO it_fm02-rlamt.
*      CONCATENATE s_gjahr-low '/' l_idx INTO it_fm02-month.
*      COLLECT it_fm02.
*
*      l_idx = l_idx + 1.
*    ENDDO.
*  ENDSELECT.

ENDFORM.                    " GET_RELEASED_FUND
*&---------------------------------------------------------------------*
*&      Form  GET_ACTUAL_FUND
*&---------------------------------------------------------------------*
FORM get_actual_fund.

  LOOP AT i_fmit02 WHERE rfonds EQ it_fm01-gcode1.

    LOOP AT s_datum WHERE low(4) EQ i_fmit02-ryear.

      AT NEW low.
        CLEAR: l_amt, fs_wtp.
      ENDAT.

      CONCATENATE 'I_FMIT02-TSL' s_datum-low+4(2) INTO fs_wtp.
      ASSIGN (fs_wtp) TO <fs_wtp>.
      l_amt = l_amt + <fs_wtp>.

      MOVE-CORRESPONDING it_fm01 TO it_fm02.
      MOVE : l_amt               TO it_fm02-acamt.
      CLEAR it_fm02-rlamt.
      CONCATENATE s_datum-low(4) s_datum-low+4(2)
                                 INTO it_fm02-month SEPARATED BY '/'.
      COLLECT it_fm02.  CLEAR it_fm02.
    ENDLOOP.

    DELETE i_fmit02.  CLEAR i_fmit02.

  ENDLOOP.

*  l_prd = p_perid.    l_mon = p_month.
*
*  SELECT * FROM fmit
*           WHERE rfonds EQ it_fm01-gcode1  "fund
*           AND   ryear  IN s_gjahr
*           AND   rwrttp IN ('50','51','54','60','61','95').
*    IF fmit-rwrttp EQ '54' OR
*       fmit-rwrttp EQ '66' OR
*       fmit-rwrttp EQ '95'.
*      CHECK fmit-rbtart = '0100'.
*    ENDIF.
*
*    CLEAR: l_amt, fs_wtp.
*    l_gjahr = fmit-ryear.
*    IF l_idx > 12.
*      IF l_gjahr EQ fmit-ryear. CONTINUE. ENDIF.
*      l_prd = l_idx - p_month.
*      l_mon = '01'.
*    ENDIF.
*    l_idx = l_mon.
*    DO l_prd TIMES.
*      CHECK l_idx <= 12.
*      CONCATENATE 'FMIT-TSL' l_idx INTO fs_wtp.
*      ASSIGN (fs_wtp) TO <fs_wtp>.
*      l_amt = l_amt + <fs_wtp>.
*
*      MOVE-CORRESPONDING it_fm01 TO it_fm02.
*      MOVE : l_amt               TO it_fm02-acamt.
*      CONCATENATE s_gjahr-low '/' l_idx INTO it_fm02-month.
*      COLLECT it_fm02.
*
*      l_idx = l_idx + 1.
*    ENDDO.
*  ENDSELECT.

ENDFORM.                    " GET_ACTUAL_FUND
*&---------------------------------------------------------------------*
*&      Form  convert_to_cm
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM convert_to_cm.
* PERFORM get_month_name.

  SORT it_fm02 BY month gcode1.

  LOOP AT it_fm02.
    MOVE-CORRESPONDING it_fm02 TO it_list.
    CONCATENATE it_list-month(4) it_list-month+5(2) '01'
           INTO it_list-datum.

    it_list-totamt = - it_fm02-rlamt + it_fm02-acamt. "sign?


    IF it_fm02-hrflg NE space.
      PERFORM compute_wage_salary.
    ELSE.
      it_list-amttm1 = it_list-totamt * ( it_fm02-rattm1 / 100 ).
      it_list-amttm2 = it_list-totamt * ( it_fm02-rattm2 / 100 ).
      COLLECT it_list.
      CLEAR it_list.
    ENDIF.

  ENDLOOP.

ENDFORM.                    " convert_to_cm
*&---------------------------------------------------------------------*
*&      Form  alvprn_basic01
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM alvprn_basic01.

  FIELD-SYMBOLS: <ls_event> TYPE slis_alv_event.

  CLEAR   : gt_events, gs_layout.
  REFRESH : gt_events.

  gs_layout-header_text      = 'HEADER'.
  gs_layout-item_text        = 'item_text'.
  gs_layout-default_item     = 'X'.
* gs_layout-box_fieldname    = 'CHKBOX'.
  gs_layout-zebra            = 'X'.

  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
       EXPORTING
            i_list_type = 0
       IMPORTING
            et_events   = gt_events.

  PERFORM   form_setting
   TABLES   gt_events
    USING : slis_ev_pf_status_set  c_status_set,
            slis_ev_user_command   c_user_command,
            slis_ev_end_of_list    c_end_of_list.

  g_repid = sy-repid.

ENDFORM.                    " alvprn_basic01
*&---------------------------------------------------------------------*
*&      Form  alvprn_field01
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0293   text
*----------------------------------------------------------------------*
FORM alvprn_field01 USING p_intab.

  CLEAR   : gt_field, gt_fieldcat.
  REFRESH : gt_field, gt_fieldcat.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
       EXPORTING
            i_program_name     = g_repid
            i_internal_tabname = p_intab
            i_inclname         = g_repid
       CHANGING
            ct_fieldcat        = gt_field.

* FIELD SETTING
  CLEAR g_cnt.
  PERFORM field_setting TABLES gt_fieldcat USING :
                                'S' 'MONTH'       ' ',
                                ' ' 'DDICTXT'     'M',
                                ' ' 'OUTPUTLEN'   '7',
                                ' ' 'JUST'        'C',
                                ' ' 'SELTEXT_M'   'Month',
                                'E' 'KEY'         ' ',

                                'S' 'GCODE1'      ' ',
                                ' ' 'DDICTXT'     'L',
                                ' ' 'JUST'        'L',
                                ' ' 'OUTPUTLEN'   '16',
                                ' ' 'SELTEXT_M'   'Type',
                                'E' 'KEY'         ' ',

                                'S' 'DATUM'       ' ',
                                ' ' 'DDICTXT'     'L',
                                ' ' 'JUST'        'C',
                                ' ' 'OUTPUTLEN'   '10',
                                ' ' 'SELTEXT_M'   'Date',
                                'E' 'KEY'         ' ',

                                'S' 'TOTAMT'      ' ',
                                ' ' 'DDICTXT'     'M',
                                ' ' 'OUTPUTLEN'   '15',
                                ' ' 'FIX_COLUMN'  'X',
                                ' ' 'DO_SUM'      'X',
                                ' ' 'CURRENCY'    sv_waers,
                                'E' 'SELTEXT_M'   'Amount',

                                'S' 'GRPTM1'      ' ',
                                ' ' 'DDICTXT'     'M',
                                ' ' 'OUTPUTLEN'   '10',
                                ' ' 'FIX_COLUMN'  'X',
                                'E' 'SELTEXT_M'   'Pln.Grp1',

                                'S' 'TYPTM1'      ' ',
                                ' ' 'DDICTXT'     'M',
                                ' ' 'OUTPUTLEN'   '02',
                                ' ' 'FIX_COLUMN'  'X',
                                'E' 'SELTEXT_M'   'T1',

                                'S' 'AMTTM1'      ' ',
                                ' ' 'DDICTXT'     'M',
                                ' ' 'OUTPUTLEN'   '15',
                                ' ' 'FIX_COLUMN'  'X',
                                ' ' 'DO_SUM'      'X',
                                ' ' 'CURRENCY'    sv_waers,
                                'E' 'SELTEXT_M'   'Amount1',

                                'S' 'GRPTM2'      ' ',
                                ' ' 'DDICTXT'     'M',
                                ' ' 'OUTPUTLEN'   '10',
                                ' ' 'FIX_COLUMN'  'X',
                                'E' 'SELTEXT_M'   'Pln.Grp2',

                                'S' 'TYPTM2'      ' ',
                                ' ' 'DDICTXT'     'M',
                                ' ' 'OUTPUTLEN'   '02',
                                ' ' 'FIX_COLUMN'  'X',
                                'E' 'SELTEXT_M'   'T2',

                                'S' 'AMTTM2'      ' ',
                                ' ' 'DDICTXT'     'M',
                                ' ' 'OUTPUTLEN'   '15',
                                ' ' 'FIX_COLUMN'  'X',
                                ' ' 'DO_SUM'      'X',
                                ' ' 'CURRENCY'    sv_waers,
                                'E' 'SELTEXT_M'   'Amount2'.

ENDFORM.                    " alvprn_field01
*&-------------------------------------------------------------------
*&      Form  field_setting
*&-------------------------------------------------------------------
FORM field_setting TABLES p_fieldcat_t LIKE gt_fieldcat
                    USING p_gub
                          p_fname
                          p_con.

  DATA: l_col(40).

  IF p_gub = 'S'.
    CLEAR g_fieldcat_s.
    READ TABLE gt_field INTO g_fieldcat_s
                        WITH KEY fieldname  = p_fname.
    EXIT.
  ENDIF.

  FIELD-SYMBOLS <fs>.
  CONCATENATE 'G_FIELDCAT_S-' p_fname  INTO l_col.
  ASSIGN      (l_col)         TO       <fs>.
  MOVE         p_con          TO       <fs>.

* DATA  APPEND
  CHECK  p_gub = 'E'.

  g_cnt = g_cnt + 1.
  g_fieldcat_s-col_pos = g_cnt.

  g_fieldcat_s-seltext_l = g_fieldcat_s-seltext_s
                         = g_fieldcat_s-seltext_m.
  APPEND g_fieldcat_s TO p_fieldcat_t.

ENDFORM.                    " field_setting
*&-------------------------------------------------------------------
*&      Form  form_setting
*&-------------------------------------------------------------------
FORM form_setting TABLES p_events_t LIKE gt_events
                   USING p_com
                         p_form.

  DATA : l_event_s    TYPE  slis_alv_event.

  READ TABLE  p_events_t  WITH KEY  name = p_com
                            INTO l_event_s.
  IF   sy-subrc EQ 0.
    MOVE     p_form      TO   l_event_s-form.
    MODIFY   p_events_t  FROM l_event_s INDEX sy-tabix.
  ENDIF.

ENDFORM.                    " form_setting
*&---------------------------------------------------------------------*
*&      Form  get_default_waers
*&---------------------------------------------------------------------*
FORM get_default_waers.

  SELECT SINGLE waers INTO sv_waers
    FROM t001
   WHERE bukrs EQ p_bukrs.

ENDFORM.                    " get_default_waers
*&---------------------------------------------------------------------*
*&      Form  sort_build
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM sort_build.

  CLEAR ls_sort.
  ls_sort-fieldname = 'MONTH'.
  ls_sort-spos = 1.    "---> KEY
  ls_sort-up = 'X'.    "ascending/desending
  ls_sort-subtot = 'X'.
  APPEND ls_sort TO gt_sort.

  CLEAR ls_sort.
  ls_sort-fieldname = 'GCODE1'.
  ls_sort-spos = 2.    "---> KEY
  ls_sort-up = 'X'.    "ascending/desending
  APPEND ls_sort TO gt_sort.

ENDFORM.                    " sort_build
*&---------------------------------------------------------------------*
*&      Form  get_month_name
*&---------------------------------------------------------------------*
FORM get_month_name.

  CLEAR: month_name[], month_name.

  CALL FUNCTION 'MONTH_NAMES_GET'
       TABLES
            month_names = month_name.

ENDFORM.                    " get_month_name
*&---------------------------------------------------------------------*
*&      Form  save_cm
*&---------------------------------------------------------------------*
FORM save_cm.

  DATA: l_datum LIKE sy-datum.

  LOOP AT it_list.
*   READ TABLE month_name WITH KEY ltx = it_list-month.
*    concatenate it_list-month(4) it_list-month+5(2) '01' into l_datum.
    MOVE : it_list-grptm1 TO wk_fdes-grupp,
           it_list-typtm1 TO wk_fdes-dsart,
           l_datum        TO wk_fdes-datum,
           p_bukrs        TO wk_fdes-bukrs,
           sv_waers       TO wk_fdes-dispw,
           it_list-amttm1 TO wk_fdes-dmshb.
*---text info -> delete info.
    CONCATENATE p_gjahr p_month p_perid INTO wk_fdes-zuonr.
    wk_fdes-refer = c_wkrefer.

    COLLECT wk_fdes. CLEAR wk_fdes.

  ENDLOOP.

  CALL FUNCTION 'TRCM_FDES_IMPORT'
       TABLES
            i_tab_fdes                = wk_fdes
       EXCEPTIONS
            conversion_failed         = 1
            database_operation_failed = 2
            ignore                    = 3
            OTHERS                    = 4.

  IF sy-subrc <> 0.
    MESSAGE w000 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    EXIT.
  ELSE.
    MESSAGE s007.
    COMMIT WORK.
  ENDIF.

ENDFORM.                    " save_cm
*&---------------------------------------------------------------------*
*&      Form  fill_amount_tab
*&---------------------------------------------------------------------*
FORM fill_amount_tab.

  REFRESH: i_bppe, i_fmit01, i_fmit02. "i_zimfm.

**** commitment item
*.......budget: released amount
  SELECT gjahr posit vorga  wtp01 wtp02 wtp03
         wtp04 wtp05 wtp06  wtp07 wtp08 wtp09
         wtp10 wtp11 wtp12
    FROM bppe
    INTO CORRESPONDING FIELDS OF TABLE i_bppe
    FOR ALL ENTRIES IN it_fmfctr
   WHERE objnr EQ it_fmfctr-ctr_objnr
     AND posit IN r_gcode2 "item code
     AND gjahr IN s_gjahr.
*    AND vorga EQ 'KBFR'. "released

*  IF NOT i_bppe[] IS INITIAL.
** filter fund center + item code
*    LOOP AT r_gcode2.
*      READ TABLE i_bppe WITH KEY posit = r_gcode2-low.
*      IF sy-subrc <> 0.
*        DELETE r_gcode2.  "item code
*      ENDIF.
*    ENDLOOP.
** filter fund center + commitment item
*    LOOP AT r_gcode1.
*      READ TABLE r_gcode2 WITH KEY low = r_gcode1-high.
*      IF sy-subrc EQ 0.
*        CLEAR r_gcode1-high.
*        MODIFY r_gcode1.  CLEAR r_gcode1.
*      ELSE.
*        DELETE r_gcode1.  CLEAR r_gcode1.
*      ENDIF.
*    ENDLOOP.
*  ENDIF.

**** commitment item
*.......actuals: commitemnt & invoice
  SELECT rfipex rfonds ryear
         tsl01  tsl02  tsl03
         tsl04  tsl05  tsl06
         tsl07  tsl08  tsl09
         tsl10  tsl11  tsl12
         rwrttp rbtart
         FROM fmit
         INTO (fmit-rfipex, fmit-rfonds, fmit-ryear,
               fmit-tsl01,  fmit-tsl02,  fmit-tsl03,
               fmit-tsl04,  fmit-tsl05,  fmit-tsl06,
               fmit-tsl07,  fmit-tsl08,  fmit-tsl09,
               fmit-tsl10,  fmit-tsl11,  fmit-tsl12,
               fmit-rwrttp, fmit-rbtart)
         WHERE fikrs  EQ p_bukrs
*        AND ( rfipex IN r_gcode1 or
*              rfonds IN r_gcode1 )
         AND   rfistl IN rfistl  "fund center
         AND   ryear  IN s_gjahr
         AND   rwrttp IN ('50','51','54','60','61','95').

    IF fmit-rwrttp EQ '54' OR
       fmit-rwrttp EQ '66' OR
       fmit-rwrttp EQ '95'.
      CHECK fmit-rbtart = '0100'.
    ENDIF.

*.......commitment item actuals: commitemnt & invoice
    IF fmit-rfipex IN r_gcode1.
      MOVE fmit TO i_fmit01.
      APPEND i_fmit01.  CLEAR i_fmit01.
*.......fund actuals: commitemnt & invoice
    ELSEIF fmit-rfonds IN r_gcode1.
      MOVE fmit TO i_fmit02.
      APPEND i_fmit02.  CLEAR i_fmit02.
    ENDIF.

  ENDSELECT.

**** fund
**.......budget: released amount
*  LOOP AT r_gcode1.
*    r_posid-sign = 'I'.   r_posid-option = 'CP'.
*    CONCATENATE '+' r_gcode1-low INTO r_posid-low.
*    APPEND r_posid. CLEAR r_posid.
*  ENDLOOP.
*  SELECT * FROM ztfi_imfm
*           INTO TABLE i_zimfm
*           WHERE posid IN r_posid
*           AND   ayear IN s_gjahr
*           AND   gubun IN ('1','2','3','5')
*           AND   status EQ 'A'.  "approval
*
**.......actuals: commitemnt & invoice
*  SELECT * FROM fmit
*           WHERE rfonds IN r_gcode1  "fund
*           AND   ryear  IN s_gjahr
*           AND   rwrttp IN ('50','51','54','60','61','95').
*    IF fmit-rwrttp EQ '54' OR
*       fmit-rwrttp EQ '66' OR
*       fmit-rwrttp EQ '95'.
*      CHECK fmit-rbtart = '0100'.
*    ENDIF.
*    MOVE fmit TO i_fmit02.
*    APPEND i_fmit02.  CLEAR i_fmit02.
*  ENDSELECT.

ENDFORM.                    " fill_amount_tab
*&---------------------------------------------------------------------*
*&      Form  delete_cm
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM delete_cm.

  DATA: l_zuonr LIKE fdes-zuonr.

* CONCATENATE p_gjahr p_month p_perid INTO l_zuonr.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_dele
           FROM fdes
           WHERE bukrs EQ p_bukrs
           AND   archk EQ space
           AND   datum IN s_datum
*          AND   zuonr EQ l_zuonr
           AND   refer EQ c_wkrefer.

  LOOP AT it_dele.
    MOVE-CORRESPONDING it_dele TO wk_dele.
    APPEND wk_dele. CLEAR:wk_dele.

    it_dele-archk = 'X'.
    it_dele-aenus = sy-uname.
    it_dele-aendt = sy-datum.
    it_dele-avdat = sy-datum.

    MOVE-CORRESPONDING it_dele TO wk_dele.
    APPEND wk_dele. CLEAR wk_dele.
  ENDLOOP.

  CHECK NOT wk_dele[] IS INITIAL.

  CALL FUNCTION 'CASH_FORECAST_MEMO_RECORD_UPD'
       EXPORTING
            aktion   = '2'
       TABLES
            tab_fdes = wk_dele.

  COMMIT WORK.

ENDFORM.                    " delete_cm
*&---------------------------------------------------------------------*
*&      Form  compute_wage_salary
*&---------------------------------------------------------------------*
FORM compute_wage_salary.

  DATA: l_fdate   LIKE sy-datum,
        l_pdate   LIKE sy-datum, "payment date
        l_num1(2) TYPE n,
        l_num2(2) TYPE n.

  l_fdate = it_fm02-month.
  REPLACE  '/'  WITH  space  INTO  l_fdate.
  CONDENSE  l_fdate  NO-GAPS.
  CONCATENATE  l_fdate  '01'  INTO  l_fdate.

  READ TABLE s_datum WITH KEY low = l_fdate.
  l_num2 = s_datum-high - s_datum-low + 1.

  CASE it_fm02-hrflg.
*-- Korea wage/salary rule, monthly => plan type
    WHEN '1'.    "Korea
*     SELECT * FROM t038v
*              WHERE ebene EQ it_fm02-typtm1.
*     ENDSELECT.
      CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
        EXPORTING
          day_in                  = l_fdate
        IMPORTING
          LAST_DAY_OF_MONTH       = it_list-datum.

*-- USA wage/salary rule, by-weekly => payroll period/pay date
    WHEN OTHERS. "USA
      SELECT * FROM t549q
       WHERE permo EQ '04' "by-weekly
         AND pabrj EQ p_gjahr.

        SELECT SINGLE pdate INTO l_pdate
          FROM t549s
         WHERE molga EQ '10' "USA
           AND datmo EQ '01' "pay day
           AND pabrj EQ t549q-pabrj
           AND pabrp EQ t549q-pabrp.

        IF t549q-begda(6) NE s_datum-low(6) AND
           t549q-endda(6) EQ s_datum-low(6).
          l_num1 = t549q-endda - s_datum-low + 1.

        ELSEIF t549q-begda(6) EQ s_datum-low(6) AND
               t549q-endda(6) EQ s_datum-low(6).
          l_num1 = t549q-endda - t549q-begda + 1.

        ELSEIF t549q-begda(6) EQ s_datum-low(6) AND
               t549q-endda(6) NE s_datum-low(6).
          l_num1 = s_datum-high - t549q-begda + 1.
        ENDIF.

        it_list-totamt = ( l_num1 / l_num2 ) * it_list-totamt.
        CONCATENATE l_pdate(4) l_pdate+4(2) INTO it_list-month
                                            SEPARATED BY '/'.

        it_list-datum  = l_pdate.
        it_list-amttm1 = it_list-totamt * ( it_fm02-rattm1 / 100 ).
        it_list-amttm2 = it_list-totamt * ( it_fm02-rattm2 / 100 ).
        COLLECT it_list.

      ENDSELECT.
      CLEAR it_list.

  ENDCASE.

ENDFORM.                    " compute_wage_salary
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form DISPLAY_ALV .

* ALV HEADER & FIELD SETTING
  PERFORM : alvprn_basic01,
            alvprn_field01 USING 'IT_LIST'.

  PERFORM sort_build.

  CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
      EXPORTING
*         I_INTERFACE_CHECK        = ' '
          i_callback_program       =  g_repid
          i_callback_pf_status_set = 'PF_STATUS_SET'
          i_callback_user_command  = 'USER_COMMAND'
          is_layout                =  gs_layout
          it_fieldcat              =  gt_fieldcat[]
*         IT_EXCLUDING             =
*         IT_SPECIAL_GROUPS        =  GT_SLIS_SP_GROUP_ALV
          it_sort                  =  gt_sort[]
*         IT_FILTER                =
*         IS_SEL_HIDE              =
*         I_DEFAULT                = 'X'
          i_save                   =  g_save
          is_variant               =  g_variant
          it_events                =  gt_events[]
       TABLES
          t_outtab                 =  it_list.

endform.                    " DISPLAY_ALV
