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
form init_screen .
*// screen initial Setting
  p_month = sy-datum+4(2).
  p_perid = '04'.

  get parameter id 'BUK' field p_bukrs.

  if p_bukrs is initial.
    p_bukrs = 'H201'.
  endif.

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
form fi_wt_read_t001  using    pa_bukrs
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
form  pf_status_set using p_rt_extab type slis_t_extab.

  set pf-status 'STANDARD'.

endform.
************************************************************************
* AT USER-COMMAND                                                     *
************************************************************************
form user_command using p_ucomm    like sy-ucomm
                        p_selfield type slis_selfield.

  case p_ucomm.
    when '&DATA_SAVE'.
      perform delete_cm.
      perform save_cm.
  endcase.

endform.
*&---------------------------------------------------------------------*
*&      Form  get_target_fm
*&---------------------------------------------------------------------*
form get_target_fm.

  perform setting_period.
  perform get_default_waers.

  clear: imap1[], imap1.

*// == Maintenance view for budget to cash
  if p_im = 'X'.
    select * into corresponding fields of table imap1
*  select * APPENDING CORRESPONDING FIELDS OF TABLE  imap1
             from ztfi_map1
             where bukrs = p_bukrs
               and gtype = '2'.       "Budget type
  else.
    select * into corresponding fields of table imap1
             from ztfi_map1
             where bukrs = p_bukrs
               and gtype = '1'.       "Budget type
  endif.

endform.                    " get_target_fm
*&---------------------------------------------------------------------*
*&      Form  get_fm_detail
*&---------------------------------------------------------------------*
form get_fm_detail.

  clear: it_fm01[], it_fm01, it_fm02[], it_fm02,
         r_gcode1[], r_gcode1, r_gcode2[], r_gcode2.

  r_gcode1-sign = 'I'.    r_gcode1-option = 'EQ'.
  r_gcode2-sign = 'I'.    r_gcode2-option = 'EQ'.

*..get CI & Fund master
  sort imap1 by gtype.

  loop at imap1.
    clear: r_fund[], r_fund.
    r_fund-sign = 'I'.  r_fund-option = 'CP'.

    case  imap1-gtype.
      when '1'. "commitment item
        select fipos posit into (fmci-fipos, fmci-posit)
               from fmci
               where fikrs eq p_bukrs "fm area = company code ?
               and   fipos >= imap1-frtrm and fipos <= imap1-totrm.
*              AND   fictr IN rfistl.  "fund center
          if sy-subrc eq 0.
            move-corresponding imap1  to it_fm01.
            move : fmci-fipos        to it_fm01-gcode1, "commitmnt item
                   fmci-posit        to it_fm01-gcode2. "item code

            r_gcode1-low  = it_fm01-gcode1.  "commitment item
            r_gcode1-high = it_fm01-gcode2.  "===> item code *********
            append r_gcode1.
            r_gcode2-low = it_fm01-gcode2.
            append r_gcode2.
            append it_fm01. clear it_fm01.
          endif.
        endselect.

* Fund Center
        select * into corresponding fields of table it_fmfctr
          from fmfctr
         where fikrs eq p_bukrs
           and fictr in rfistl.  "fund center

* Fund Center Hiearchy (select end node only)
        loop at it_fmfctr.
          select single * from fmhictr
             where ctr_objnr = it_fmfctr-ctr_objnr.

          if fmhictr-parent_obj = space.
            delete it_fmfctr.
          else.
*-- end node...
*--- check FundCenter Auth.
            select single * from fmfctr
                where fikrs eq p_bukrs
                  and fictr eq it_fmfctr-fictr
                  and datbis >= sy-datum.

            authority-check object 'Z_FICTR'
                     id 'FM_FIKRS'   field p_bukrs
                     id 'FM_FICTR'   field fmfctr-fictr.

            if sy-subrc <> 0.
              delete it_fmfctr.
            endif.

          endif.
        endloop.

      when '2'.
*       r_fund-low = imap1-frtrm.   APPEND r_fund.
        concatenate '+' imap1-frtrm into r_fund-low.
        append r_fund.

        select * from ztfi_imfm
         where ayear in s_gjahr
           and posid in r_fund. "fund

          if sy-subrc eq 0.
            move-corresponding imap1      to it_fm01.
            move : ztfi_imfm-posid+1(10)  to it_fm01-gcode1.  "fund

            r_gcode1-low = it_fm01-gcode1.
            append r_gcode1.
            append it_fm01. clear it_fm01.

* ==========> fund budget: released amount
            if ( ztfi_imfm-gubun  eq '1' or
                 ztfi_imfm-gubun  eq '2' or
                 ztfi_imfm-gubun  eq '3' or
                 ztfi_imfm-gubun  eq '5' )
            and  ztfi_imfm-status eq 'A'.  "approval
              move-corresponding ztfi_imfm to i_zimfm.
              append i_zimfm. clear i_zimfm.
            endif.
          endif.

        endselect.


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
    endcase.
  endloop.

* fill total budget/actuals itab.
  perform fill_amount_tab.

* select-option
  delete it_fm01 where gtype eq '1'
                 and   not gcode1 in rfipex.  "ci
  delete it_fm01 where gtype eq '2'
                 and   not gcode1 in rfonds.  "fund
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
  loop at it_fm01.
    case it_fm01-gtype.
      when '1'.  "commitment item
*.......CI budget: released amount
        perform get_released_commitmnt.
*.......CI actuals: commitemnt & invoice
        if p_yearly eq space.
          perform get_actual_commitmnt.
        endif.

      when '2'.  "fund
*.......Fudn budget: released amount
        perform get_released_fund.
*.......Fund actuals: commitemnt & invoice
        if p_yearly eq space.
          perform get_actual_fund.
        endif.
    endcase.
  endloop.

  delete it_fm02 where rlamt is initial
                 and   acamt is initial.

endform.                    " get_fm_detail
*&---------------------------------------------------------------------*
*&      Form  setting_period
*&---------------------------------------------------------------------*
form setting_period.

  data: l_mon(2) type n.

  s_datum-sign = 'I'.    s_datum-option = 'EQ'.
  s_gjahr-sign = 'I'.    s_gjahr-option = 'BT'.

  concatenate p_gjahr p_month '01' into s_datum-low.
  append s_datum.
  s_gjahr-low = p_gjahr.

  l_mon = p_perid - 1.
  do l_mon times.
    call function 'MONTH_PLUS_DETERMINE'
         exporting
              months  = 1
              olddate = s_datum-low
         importing
              newdate = s_datum-low.
    append s_datum.
  enddo.

  loop at s_datum.
    call function 'RP_LAST_DAY_OF_MONTHS'
         exporting
              day_in            = s_datum-low
         importing
              last_day_of_month = s_datum-high.
    modify s_datum.
  endloop.

  s_gjahr-high = s_datum-high(4).
  append s_gjahr.

endform.                    " setting_period
*&---------------------------------------------------------------------*
*&      Form  GET_RELEASED_COMMITMNT
*&---------------------------------------------------------------------*
form get_released_commitmnt.

  data: l_release_amt like it_fm02-rlamt.

  if p_yearly eq 'X'.  "yearly forecasting

    loop at i_bppe where posit eq it_fm01-gcode2
                   and   vorga eq 'KBUD'.  "orgin amount

      loop at s_datum where low(4) eq i_bppe-gjahr.

        at new low.
          clear: l_amt, fs_wtp.
        endat.

        concatenate 'I_BPPE-WTP' s_datum-low+4(2) into fs_wtp.
        assign (fs_wtp) to <fs_wtp>.
        l_amt = l_amt + <fs_wtp>.

        move-corresponding it_fm01 to it_fm02.
        move : l_amt               to it_fm02-rlamt.
        concatenate s_datum-low(4) s_datum-low+4(2)
                                   into it_fm02-month separated by '/'.
        l_release_amt = it_fm02-rlamt.

        collect it_fm02.  clear it_fm02.
      endloop.

      delete i_bppe.  clear i_bppe.
    endloop.

  else.
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

    loop at i_bppe where posit eq it_fm01-gcode2
                   and ( vorga eq 'KBUD' or "orgin amount
                         vorga eq 'KBNO' or "supp.
                         vorga eq 'KBRO' or "return
                         vorga eq 'KBUS' or "transfer
                         vorga eq 'KBUE' ). "transfer

      loop at s_datum where low(4) eq i_bppe-gjahr.
*          CHECK s_datum-low+4(2) EQ it_fm02-month+5(2).

        at new low.
          clear: l_amt, fs_wtp.
        endat.

        concatenate 'I_BPPE-WTP' s_datum-low+4(2) into fs_wtp.
        assign (fs_wtp) to <fs_wtp>.
        l_amt = l_amt + <fs_wtp>.

        move-corresponding it_fm01 to it_fm02.
        move : l_amt               to it_fm02-rlamt.
        concatenate s_datum-low(4) s_datum-low+4(2)
                    into it_fm02-month separated by '/'.
        collect it_fm02.  clear it_fm02.
      endloop.

      delete i_bppe.  clear i_bppe.
    endloop.
*    ENDLOOP.
  endif.

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

endform.                    " GET_RELEASED_COMMITMNT
*&---------------------------------------------------------------------*
*&      Form  GET_ACTUAL_COMMITMNT
*&---------------------------------------------------------------------*
form get_actual_commitmnt.

  loop at i_fmit01 where rfipex eq it_fm01-gcode1.

    loop at s_datum where low(4) eq i_fmit01-ryear.

      at new low.
        clear: l_amt, fs_wtp.
      endat.

      concatenate 'I_FMIT01-TSL' s_datum-low+4(2) into fs_wtp.
      assign (fs_wtp) to <fs_wtp>.
      l_amt = l_amt + <fs_wtp>.

      move-corresponding it_fm01 to it_fm02.
      move : l_amt               to it_fm02-acamt.
      concatenate s_datum-low(4) s_datum-low+4(2)
                                 into it_fm02-month separated by '/'.
      collect it_fm02.  clear it_fm02.
    endloop.

    delete i_fmit01.  clear i_fmit01.

  endloop.

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

endform.                    " GET_ACTUAL_COMMITMNT
*&---------------------------------------------------------------------*
*&      Form  GET_RELEASED_FUND
*&---------------------------------------------------------------------*
form get_released_fund.

  concatenate '+' it_fm01-gcode1 into l_posid.

  loop at i_zimfm where posid cp l_posid.

    loop at s_datum where low(4) eq i_zimfm-ayear
                    and   low(4) eq i_zimfm-gjahr.
      at new low.
        clear: l_amt, fs_wtp.
      endat.

      concatenate 'I_ZIMFM-WTP' s_datum-low+4(2) into fs_wtp.
      assign (fs_wtp) to <fs_wtp>.
      l_amt = l_amt + <fs_wtp>.

      move-corresponding it_fm01 to it_fm02.
      move : l_amt               to it_fm02-rlamt.
      clear it_fm02-acamt.
      concatenate s_datum-low(4) s_datum-low+4(2)
                                 into it_fm02-month separated by '/'.
      collect it_fm02.  clear it_fm02.
    endloop.

    delete i_zimfm.   clear i_zimfm.

  endloop.

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

endform.                    " GET_RELEASED_FUND
*&---------------------------------------------------------------------*
*&      Form  GET_ACTUAL_FUND
*&---------------------------------------------------------------------*
form get_actual_fund.

  loop at i_fmit02 where rfonds eq it_fm01-gcode1.

    loop at s_datum where low(4) eq i_fmit02-ryear.

      at new low.
        clear: l_amt, fs_wtp.
      endat.

      concatenate 'I_FMIT02-TSL' s_datum-low+4(2) into fs_wtp.
      assign (fs_wtp) to <fs_wtp>.
      l_amt = l_amt + <fs_wtp>.

      move-corresponding it_fm01 to it_fm02.
      move : l_amt               to it_fm02-acamt.
      clear it_fm02-rlamt.
      concatenate s_datum-low(4) s_datum-low+4(2)
                                 into it_fm02-month separated by '/'.
      collect it_fm02.  clear it_fm02.
    endloop.

    delete i_fmit02.  clear i_fmit02.

  endloop.

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

endform.                    " GET_ACTUAL_FUND
*&---------------------------------------------------------------------*
*&      Form  convert_to_cm
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form convert_to_cm.
* PERFORM get_month_name.

  sort it_fm02 by month gcode1.

  loop at it_fm02.
    move-corresponding it_fm02 to it_list.
    concatenate it_list-month(4) it_list-month+5(2) '01'
           into it_list-datum.

    it_list-totamt = - it_fm02-rlamt + it_fm02-acamt. "sign?


    if it_fm02-hrflg ne space.
      perform compute_wage_salary.
    else.
      it_list-amttm1 = it_list-totamt * ( it_fm02-rattm1 / 100 ).
      it_list-amttm2 = it_list-totamt * ( it_fm02-rattm2 / 100 ).
      collect it_list.
      clear it_list.
    endif.

  endloop.

endform.                    " convert_to_cm
*&---------------------------------------------------------------------*
*&      Form  alvprn_basic01
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form alvprn_basic01.

  field-symbols: <ls_event> type slis_alv_event.

  clear   : gt_events, gs_layout.
  refresh : gt_events.

  gs_layout-header_text      = 'HEADER'.
  gs_layout-item_text        = 'item_text'.
  gs_layout-default_item     = 'X'.
* gs_layout-box_fieldname    = 'CHKBOX'.
  gs_layout-zebra            = 'X'.

  call function 'REUSE_ALV_EVENTS_GET'
       exporting
            i_list_type = 0
       importing
            et_events   = gt_events.

  perform   form_setting
   tables   gt_events
    using : slis_ev_pf_status_set  c_status_set,
            slis_ev_user_command   c_user_command,
            slis_ev_end_of_list    c_end_of_list.

  g_repid = sy-repid.

endform.                    " alvprn_basic01
*&---------------------------------------------------------------------*
*&      Form  alvprn_field01
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0293   text
*----------------------------------------------------------------------*
form alvprn_field01 using p_intab.

  clear   : gt_field, gt_fieldcat.
  refresh : gt_field, gt_fieldcat.

  call function 'REUSE_ALV_FIELDCATALOG_MERGE'
       exporting
            i_program_name     = g_repid
            i_internal_tabname = p_intab
            i_inclname         = g_repid
       changing
            ct_fieldcat        = gt_field.

* FIELD SETTING
  clear g_cnt.
  perform field_setting tables gt_fieldcat using :
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

endform.                    " alvprn_field01
*&-------------------------------------------------------------------
*&      Form  field_setting
*&-------------------------------------------------------------------
form field_setting tables p_fieldcat_t like gt_fieldcat
                    using p_gub
                          p_fname
                          p_con.

  data: l_col(40).

  if p_gub = 'S'.
    clear g_fieldcat_s.
    read table gt_field into g_fieldcat_s
                        with key fieldname  = p_fname.
    exit.
  endif.

  field-symbols <fs>.
  concatenate 'G_FIELDCAT_S-' p_fname  into l_col.
  assign      (l_col)         to       <fs>.
  move         p_con          to       <fs>.

* DATA  APPEND
  check  p_gub = 'E'.

  g_cnt = g_cnt + 1.
  g_fieldcat_s-col_pos = g_cnt.

  g_fieldcat_s-seltext_l = g_fieldcat_s-seltext_s
                         = g_fieldcat_s-seltext_m.
  append g_fieldcat_s to p_fieldcat_t.

endform.                    " field_setting
*&-------------------------------------------------------------------
*&      Form  form_setting
*&-------------------------------------------------------------------
form form_setting tables p_events_t like gt_events
                   using p_com
                         p_form.

  data : l_event_s    type  slis_alv_event.

  read table  p_events_t  with key  name = p_com
                            into l_event_s.
  if   sy-subrc eq 0.
    move     p_form      to   l_event_s-form.
    modify   p_events_t  from l_event_s index sy-tabix.
  endif.

endform.                    " form_setting
*&---------------------------------------------------------------------*
*&      Form  get_default_waers
*&---------------------------------------------------------------------*
form get_default_waers.

  select single waers into sv_waers
    from t001
   where bukrs eq p_bukrs.

endform.                    " get_default_waers
*&---------------------------------------------------------------------*
*&      Form  sort_build
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form sort_build.

  clear ls_sort.
  ls_sort-fieldname = 'MONTH'.
  ls_sort-spos = 1.    "---> KEY
  ls_sort-up = 'X'.    "ascending/desending
  ls_sort-subtot = 'X'.
  append ls_sort to gt_sort.

  clear ls_sort.
  ls_sort-fieldname = 'GCODE1'.
  ls_sort-spos = 2.    "---> KEY
  ls_sort-up = 'X'.    "ascending/desending
  append ls_sort to gt_sort.

endform.                    " sort_build
*&---------------------------------------------------------------------*
*&      Form  get_month_name
*&---------------------------------------------------------------------*
form get_month_name.

  clear: month_name[], month_name.

  call function 'MONTH_NAMES_GET'
       tables
            month_names = month_name.

endform.                    " get_month_name
*&---------------------------------------------------------------------*
*&      Form  save_cm
*&---------------------------------------------------------------------*
form save_cm.

  data: l_datum like sy-datum.

  loop at it_list.
*   READ TABLE month_name WITH KEY ltx = it_list-month.
*    concatenate it_list-month(4) it_list-month+5(2) '01' into l_datum.
    move : it_list-grptm1 to wk_fdes-grupp,
           it_list-typtm1 to wk_fdes-dsart,
           l_datum        to wk_fdes-datum,
           p_bukrs        to wk_fdes-bukrs,
           sv_waers       to wk_fdes-dispw,
           it_list-amttm1 to wk_fdes-dmshb.
*---text info -> delete info.
    concatenate p_gjahr p_month p_perid into wk_fdes-zuonr.
    wk_fdes-refer = c_wkrefer.

    collect wk_fdes. clear wk_fdes.

  endloop.

  call function 'TRCM_FDES_IMPORT'
       tables
            i_tab_fdes                = wk_fdes
       exceptions
            conversion_failed         = 1
            database_operation_failed = 2
            ignore                    = 3
            others                    = 4.

  if sy-subrc <> 0.
    message w000 with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    exit.
  else.
    message s007.
    commit work.
  endif.

endform.                    " save_cm
*&---------------------------------------------------------------------*
*&      Form  fill_amount_tab
*&---------------------------------------------------------------------*
form fill_amount_tab.

  refresh: i_bppe, i_fmit01, i_fmit02. "i_zimfm.

**** commitment item
*.......budget: released amount
  select gjahr posit vorga  wtp01 wtp02 wtp03
         wtp04 wtp05 wtp06  wtp07 wtp08 wtp09
         wtp10 wtp11 wtp12
    from bppe
    into corresponding fields of table i_bppe
    for all entries in it_fmfctr
   where objnr eq it_fmfctr-ctr_objnr
     and posit in r_gcode2 "item code
     and gjahr in s_gjahr.
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
  select rfipex rfonds ryear
         tsl01  tsl02  tsl03
         tsl04  tsl05  tsl06
         tsl07  tsl08  tsl09
         tsl10  tsl11  tsl12
         rwrttp rbtart
         from fmit
         into (fmit-rfipex, fmit-rfonds, fmit-ryear,
               fmit-tsl01,  fmit-tsl02,  fmit-tsl03,
               fmit-tsl04,  fmit-tsl05,  fmit-tsl06,
               fmit-tsl07,  fmit-tsl08,  fmit-tsl09,
               fmit-tsl10,  fmit-tsl11,  fmit-tsl12,
               fmit-rwrttp, fmit-rbtart)
         where fikrs  eq p_bukrs
*        AND ( rfipex IN r_gcode1 or
*              rfonds IN r_gcode1 )
         and   rfistl in rfistl  "fund center
         and   ryear  in s_gjahr
         and   rwrttp in ('50','51','54','60','61','95').

    if fmit-rwrttp eq '54' or
       fmit-rwrttp eq '66' or
       fmit-rwrttp eq '95'.
      check fmit-rbtart = '0100'.
    endif.

*.......commitment item actuals: commitemnt & invoice
    if fmit-rfipex in r_gcode1.
      move fmit to i_fmit01.
      append i_fmit01.  clear i_fmit01.
*.......fund actuals: commitemnt & invoice
    elseif fmit-rfonds in r_gcode1.
      move fmit to i_fmit02.
      append i_fmit02.  clear i_fmit02.
    endif.

  endselect.

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

endform.                    " fill_amount_tab
*&---------------------------------------------------------------------*
*&      Form  delete_cm
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form delete_cm.

  data: l_zuonr like fdes-zuonr.

* CONCATENATE p_gjahr p_month p_perid INTO l_zuonr.
  select * into corresponding fields of table it_dele
           from fdes
           where bukrs eq p_bukrs
           and   archk eq space
           and   datum in s_datum
*          AND   zuonr EQ l_zuonr
           and   refer eq c_wkrefer.

  loop at it_dele.
    move-corresponding it_dele to wk_dele.
    append wk_dele. clear:wk_dele.

    it_dele-archk = 'X'.
    it_dele-aenus = sy-uname.
    it_dele-aendt = sy-datum.
    it_dele-avdat = sy-datum.

    move-corresponding it_dele to wk_dele.
    append wk_dele. clear wk_dele.
  endloop.

  check not wk_dele[] is initial.

  call function 'CASH_FORECAST_MEMO_RECORD_UPD'
       exporting
            aktion   = '2'
       tables
            tab_fdes = wk_dele.

  commit work.

endform.                    " delete_cm
*&---------------------------------------------------------------------*
*&      Form  compute_wage_salary
*&---------------------------------------------------------------------*
form compute_wage_salary.

  data: l_fdate   like sy-datum,
        l_pdate   like sy-datum, "payment date
        l_num1(2) type n,
        l_num2(2) type n.

  l_fdate = it_fm02-month.
  replace  '/'  with  space  into  l_fdate.
  condense  l_fdate  no-gaps.
  concatenate  l_fdate  '01'  into  l_fdate.

  read table s_datum with key low = l_fdate.
  l_num2 = s_datum-high - s_datum-low + 1.

  case it_fm02-hrflg.
*-- Korea wage/salary rule, monthly => plan type
    when '1'.    "Korea
*     SELECT * FROM t038v
*              WHERE ebene EQ it_fm02-typtm1.
*     ENDSELECT.
      call function 'RP_LAST_DAY_OF_MONTHS'
        exporting
          day_in                  = l_fdate
        importing
          last_day_of_month       = it_list-datum.

*-- USA wage/salary rule, by-weekly => payroll period/pay date
    when others. "USA
      select * from t549q
       where permo eq '04' "by-weekly
         and pabrj eq p_gjahr.

        select single pdate into l_pdate
          from t549s
         where molga eq '10' "USA
           and datmo eq '01' "pay day
           and pabrj eq t549q-pabrj
           and pabrp eq t549q-pabrp.

        if t549q-begda(6) ne s_datum-low(6) and
           t549q-endda(6) eq s_datum-low(6).
          l_num1 = t549q-endda - s_datum-low + 1.

        elseif t549q-begda(6) eq s_datum-low(6) and
               t549q-endda(6) eq s_datum-low(6).
          l_num1 = t549q-endda - t549q-begda + 1.

        elseif t549q-begda(6) eq s_datum-low(6) and
               t549q-endda(6) ne s_datum-low(6).
          l_num1 = s_datum-high - t549q-begda + 1.
        endif.

        it_list-totamt = ( l_num1 / l_num2 ) * it_list-totamt.
        concatenate l_pdate(4) l_pdate+4(2) into it_list-month
                                            separated by '/'.

        it_list-datum  = l_pdate.
        it_list-amttm1 = it_list-totamt * ( it_fm02-rattm1 / 100 ).
        it_list-amttm2 = it_list-totamt * ( it_fm02-rattm2 / 100 ).
        collect it_list.

      endselect.
      clear it_list.

  endcase.

endform.                    " compute_wage_salary
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form display_alv .

* ALV HEADER & FIELD SETTING
  perform : alvprn_basic01,
            alvprn_field01 using 'IT_LIST'.

  perform sort_build.

  call function 'REUSE_ALV_LIST_DISPLAY'
      exporting
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
       tables
          t_outtab                 =  it_list.

endform.                    " DISPLAY_ALV
