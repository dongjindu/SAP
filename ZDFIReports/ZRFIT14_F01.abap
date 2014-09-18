*&---------------------------------------------------------------------*
*&  Include           ZRFIT14_F01
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
*&---------------------------------------------------------------------*
*&      Form  SELECT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form SELECT_DATA .

  clear: it_kun, it_kun[].

  select single * from t001 where bukrs = p_bukrs.

  clear v_last_date.
  call function 'RP_LAST_DAY_OF_MONTHS'
       exporting
            day_in            = p_edatu
       importing
            last_day_of_month = v_last_date.

  select b~kunnr a~vbeln a~posnr a~waerk a~cmpre "a~netwr
    into corresponding fields of it_kun
    from vbap as a join vbpa as b
          on a~vbeln = b~vbeln "AND
*           a~posnr = b~posnr
   where a~pstyv eq 'ZVTQ'        "item category
     and b~parvw eq 'RE'          "bill-to-party
     and b~vbeln in s_vbeln.

*// 2011.09.28 delete.
    check it_kun-cmpre > 0.       "tem credit price
    append it_kun.
    clear: it_kun.

  endselect.

endform.                    " SELECT_DATA
*&---------------------------------------------------------------------*
*&      Form  CREATE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form CREATE_DATA .

  clear: it_vbep, it_vbep[].

  sort it_kun.

  loop at it_kun.
    move : it_kun-kunnr   to it_vbep-kunnr,
           it_kun-vbeln   to it_vbep-vbeln.

*---> planning group : customer master
    select single fdgrv into it_vbep-fdgrv
      from knb1
     where bukrs eq p_bukrs
       and kunnr eq it_kun-kunnr.

*---> scheduled date, order qty,  confirmed qty
    select a~edatu a~wmeng a~bmeng a~vrkme b~zterm
      into (it_vbep-edatu, it_vbep-wmeng, it_vbep-bmeng,
             v_vrkme, it_vbep-zterm )
      from vbep as a join vbkd as b
        on a~vbeln = b~vbeln "AND
*              a~posnr = b~posnr
     where a~vbeln eq it_kun-vbeln
       and a~posnr eq it_kun-posnr
       and a~edatu between p_edatu
       and v_last_date.

      check it_vbep-bmeng > 0.
*.....amount = confiremd qty * net price.
      it_vbep-bamnt = it_vbep-bmeng * it_kun-cmpre.

*.... payment term, currency, local amt
      it_vbep-waerk = it_kun-waerk.

*----> begin of the month
      if p_start eq 'X'.
        concatenate it_vbep-edatu(6) '01' into it_vbep-edatu.

*----> end of the month
      elseif p_end eq 'X'.

        call function 'RP_LAST_DAY_OF_MONTHS'
             exporting
                  day_in            = it_vbep-edatu
             importing
                  last_day_of_month = it_vbep-edatu
             exceptions
                  day_in_no_date    = 1
                  others            = 2.

      endif.

      perform get_paydate.
      check sy-subrc = 0.

      append it_vbep.
    endselect.

    clear it_vbep.

  endloop.

  sort it_vbep by edatu vbeln.

  if p_test eq space.
    loop at it_vbep where fdgrv = space.
      write :/ 'Customer',
               it_vbep-kunnr,
               ' has no plan group'.
    endloop.

    if sy-subrc eq 0.
      exit.
    endif.

    perform dele_data.
    perform save_data.

  endif.

endform.                    " CREATE_DATA

************************************************************************
* Form  PF_STATUS_SET
************************************************************************
form  pf_status_set using p_rt_extab type slis_t_extab.

  if p_test eq 'X'.
    set pf-status 'MENU1'.
  else.
    set pf-status 'MENU2'.
  endif.

endform.

************************************************************************
* AT USER-COMMAND                                                     *
************************************************************************
form user_command using p_ucomm    like sy-ucomm
                        p_selfield type slis_selfield.
  case p_ucomm.
    when '&DATA_SAVE'.
      perform dele_data.
      perform save_data.
  endcase.
endform.

*&---------------------------------------------------------------------*
*&      Form  alvprn_list
*&---------------------------------------------------------------------*
form alvprn_list tables p_tab.
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
*         IT_SORT                  =  IT_SORT[]
*         IT_FILTER                =
*         IS_SEL_HIDE              =
*         I_DEFAULT                = 'X'
          i_save                   =  g_save
          is_variant               =  g_variant
          it_events                =  gt_events[]
       tables
          t_outtab                 =  p_tab.
endform.                    " alvprn_list
*&---------------------------------------------------------------------*
*&      Form  alvprn_basic01
*&---------------------------------------------------------------------*
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

  delete gt_events where name ne 'END_OF_PAGE'
                     and name ne 'TOP_OF_PAGE'
                     and name ne 'TOP_OF_LIST'
                     and name ne 'END_OF_LIST'.
  loop at gt_events assigning <ls_event>.
    concatenate 'ALV_EVENT_'
                <ls_event>-name
                into <ls_event>-form.
  endloop.

endform.                    " alvprn_basic01
*&-------------------------------------------------------------------
*&      Form  alvprn_field01
*&-------------------------------------------------------------------
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
                                'S' 'DATUM'       ' ',
                                ' ' 'DDICTXT'     'M',
                                ' ' 'OUTPUTLEN'   '10',
                                ' ' 'JUST'        'C',
                                'E' 'SELTEXT_M'   'Pln.Dat',

                                'S' 'EDATU'       ' ',
                                ' ' 'DDICTXT'     'M',
                                ' ' 'OUTPUTLEN'   '10',
                                ' ' 'JUST'        'C',
                                'E' 'SELTEXT_M'   'Cf.Dat',

                                'S' 'VBELN'       ' ',
                                ' ' 'DDICTXT'     'M',
                                ' ' 'OUTPUTLEN'   '10',
                                ' ' 'KEY'         ' ',
                                'E' 'SELTEXT_M'   'SalesOrder',

*                                'S' 'WMENG'       ' ',
*                                ' ' 'DDICTXT'     'M',
*                                ' ' 'OUTPUTLEN'   '13',
*                                ' ' 'JUST'        'C',
*                                ' ' 'DO_SUM'      'X',
*                                ' ' 'QUANTITY'    v_vrkme,
*                                'E' 'SELTEXT_M'   'Order Qty.',

                                'S' 'BMENG'       ' ',
                                ' ' 'DDICTXT'     'M',
                                ' ' 'OUTPUTLEN'   '13',
                                ' ' 'JUST'        'C',
                                ' ' 'DO_SUM'      'X',
                                ' ' 'QUANTITY'    v_vrkme,

                                'E' 'SELTEXT_M'   'Confirmed Qty',
                                'S' 'WAERK'       ' ',
                                ' ' 'DDICTXT'     'M',
                                ' ' 'OUTPUTLEN'   '5',
                                ' ' 'DO_SUM'      'X',
                                'E' 'SELTEXT_M'   'Curr.',

                                'S' 'BAMNT'       ' ',
                                ' ' 'DDICTXT'     'M',
                                ' ' 'OUTPUTLEN'   '22',
                                ' ' 'DO_SUM'      'X',
                                'E' 'SELTEXT_M'   'Confirmed Amt.',

                                'S' 'DMSHB'       ' ',
                                ' ' 'DDICTXT'     'M',
                                ' ' 'OUTPUTLEN'   '22',
                                ' ' 'DO_SUM'      'X',
                                'E' 'SELTEXT_M'   'Local Amt.',

                                'S' 'KUNNR'       ' ',
                                ' ' 'DDICTXT'     'M',
                                ' ' 'OUTPUTLEN'   '10',
                                ' ' 'KEY'         ' ',
                                'E' 'SELTEXT_M'   'Customer',

                                'S' 'FDGRV'       ' ',
                                ' ' 'DDICTXT'     'M',
                                ' ' 'OUTPUTLEN'   '10',
                                'E' 'SELTEXT_M'   'Pln.Grp'.

endform.
*&-------------------------------------------------------------------
*&      Form  field_setting
*&-------------------------------------------------------------------
form field_setting tables p_fieldcat_t like gt_fieldcat
                    using p_gub p_fname p_con.

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

*  g_fieldcat_s-seltext_l = g_fieldcat_s-seltext_s
*                         = g_fieldcat_s-seltext_m.
  append g_fieldcat_s to p_fieldcat_t.

endform.                    " field_setting
*&---------------------------------------------------------------------*
*&      Form  dele_data
*&---------------------------------------------------------------------*
form dele_data.

  clear: wk_dele[], wk_dele.

  select * from fdes
    into corresponding fields of wk_dele
   where bukrs eq p_bukrs
     and archk eq space
     and dsart eq c_sptype.

* original
    append wk_dele.

* copied
    wk_dele-archk = 'X'.
    wk_dele-aenus = sy-uname.
    wk_dele-aendt = sy-datum.
    wk_dele-avdat = sy-datum.
    append wk_dele.
    clear wk_dele.
  endselect.

  if sy-subrc eq 0.
    call function 'CASH_FORECAST_MEMO_RECORD_UPD'
         exporting
              aktion   = '2'
         tables
              tab_fdes = wk_dele.
    commit work.
  endif.

endform.                    " dele_data
*&---------------------------------------------------------------------*
*&      Form  save_data
*&---------------------------------------------------------------------*
form save_data.

  clear: wk_fdes[], wk_fdes.

  loop at it_vbep.
*   CONCATENATE it_vbep-datum(6) '01' INTO wk_fdes-datum.
    move : p_bukrs       to  wk_fdes-bukrs,
           it_vbep-fdgrv to  wk_fdes-grupp,
*          it_vbep-edatu TO  wk_fdes-datum,   "cf.dat
           it_vbep-datum to  wk_fdes-datum,   "pln.dat(<=payment term)
           it_vbep-waerk to  wk_fdes-dispw,
           it_vbep-bamnt to  wk_fdes-wrshb,
           it_vbep-dmshb to  wk_fdes-dmshb,
           c_sptype      to  wk_fdes-dsart,
           it_vbep-vbeln to  wk_fdes-merkm.   "S/O -> Characteristics

* order # --> CM reference.
*   wk_fdes-REFER = it_vbep-vbeln.
    append wk_fdes.
    clear wk_fdes.
*   COLLECT wk_fdes.  CLEAR wk_fdes.
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
*   MESSAGE w000 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    message w000 with 'Failed to save'.
    exit.
  else.
    message s007.     "Saved successfully
    commit work.
  endif.

endform.                    " save_data
*&---------------------------------------------------------------------*
*&      Form  get_paydate
*&---------------------------------------------------------------------*
form get_paydate.

  data: l_zfbdt like fdes-datum,
        l_rf40d like rf40d.

* LFDSDF00 - form zaverh_det.
* it_vbep-edatu
* currency...
  select single * from  t052
           where zterm eq it_vbep-zterm.

  if sy-subrc = 0.
    l_rf40d-budat = it_vbep-edatu.
    l_rf40d-bukrs = p_bukrs.
    l_rf40d-koart = 'D'.              "customer
    l_rf40d-shkzg = 'S'.              "receivable

    call function 'FI_TERMS_OF_PAYMENT_PROPOSE'
         exporting
              i_bldat         = it_vbep-edatu
              i_budat         = it_vbep-edatu
              i_zterm         = it_vbep-zterm
         importing
              e_zfbdt         = l_rf40d-zfbdt
              e_zbd1t         = l_rf40d-zbd1t
              e_zbd2t         = l_rf40d-zbd2t
              e_zbd3t         = l_rf40d-zbd3t
              e_zbd1p         = l_rf40d-zbd1p
              e_rebzg         = l_rf40d-rebzg
              e_zlsch         = l_rf40d-zlsch
         exceptions
              terms_not_found = 1
              others          = 2.

    call function 'PAYDAY_DETERMINATION'
         exporting
              i_rf40d         = l_rf40d
              payment_history = 'X'
         importing
              payday          = it_vbep-datum.
  else.
    it_vbep-datum  = it_vbep-edatu.
  endif.

  if t001-waers <> it_vbep-waerk.

    call function 'CONVERT_TO_LOCAL_CURRENCY'
         exporting
              date             = it_vbep-edatu
              foreign_amount   = it_vbep-bamnt
              foreign_currency = it_vbep-waerk
              local_currency   = t001-waers
         importing
              local_amount     = it_vbep-dmshb.

  else.
    it_vbep-dmshb = it_vbep-bamnt.
  endif.

endform.                    " get_paydate
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form DISPLAY_ALV .


  g_repid = sy-repid.

* ALV HEADER & FIELD SETTING
  perform : alvprn_basic01,
            alvprn_field01 using 'IT_VBEP',
            alvprn_list    tables it_vbep.
*
endform.                    " DISPLAY_ALV
