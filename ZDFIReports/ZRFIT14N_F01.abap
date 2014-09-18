*&---------------------------------------------------------------------*
*&  Include           ZRFIT14N_F01
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

  DATA : lt_ztvh TYPE TABLE OF ZTPP_PMT07JB_A,
         ls_ztvh TYPE ZTPP_PMT07JB_A.
  DATA : ls_data TYPE t_data.
  DATA : l_datum TYPE datum.

*** : For vehicle
  select *
    into table lt_ztvh
    from ZTPP_PMT07JB_A
   where gubb = 'A'.

  LOOP AT lt_ztvh INTO ls_ztvh.

    CLEAR : ls_data.

    CONCATENATE ls_ztvh-moye ls_ztvh-dist+0(3) ls_ztvh-dist+4(1)
                ls_ztvh-bmdl ls_ztvh-ocnn INTO ls_data-matnr.

    ls_data-kunnr = ls_ztvh-dist.
    ls_data-sqdt = ls_ztvh-sqdt.
    ls_data-gubun = 'V'.
    ls_data-plnmg = 1.

    COLLECT ls_data INTO gt_data.

  ENDLOOP.

*** : For Engine
  DATA : lt_zteg TYPE TABLE OF ZTPP_ENG_PIR,
         ls_zteg TYPE ZTPP_ENG_PIR.
  DATA : l_mandt TYPE mandt.

  SELECT * INTO TABLE lt_zteg
    FROM ZTPP_ENG_PIR
   WHERE WDATU IN ( SELECT MAX( WDATU )
                      FROM ZTPP_ENG_PIR ).

  LOOP AT lt_zteg INTO ls_zteg.

    CLEAR : ls_data.

    CHECK ls_zteg-matnr+0(1) <> '2'.
    CHECK ls_zteg-matnr+0(1) <> '3'.
    CHECK ls_zteg-plnmg <> 0.

    SELECT SINGLE mandt INTO l_mandt
      FROM knmt
     WHERE kunnr = 'AKNH'.   "" Kia Motor Manu. Georgia

    CHECK sy-subrc = 0.

    ls_data-matnr = ls_zteg-matnr.

    ls_data-kunnr = 'AKNH'.
    ls_data-sqdt = ls_zteg-PDATU.
    ls_data-gubun = 'E'.
    ls_data-plnmg = ls_zteg-plnmg.

    COLLECT ls_data INTO gt_data.

  ENDLOOP.

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

  DATA: ls_knb1 TYPE knb1,
        lt_knb1 TYPE TABLE OF knb1.
  DATA: ls_a004 TYPE a004,
        lt_a004 TYPE TABLE OF a004.
  DATA: ls_a005 TYPE a005,
        lt_a005 TYPE TABLE OF a005.
  DATA: ls_konp TYPE konp,
        lt_konp TYPE TABLE OF konp.
  DATA: ls_data TYPE t_data,
        ls_list TYPE t_list.
  DATA: l_zterm TYPE DZTERM.
  DATA: l_datum TYPE datum.

  CHECK gt_data[] IS NOT INITIAL.

  SELECT * INTO TABLE lt_knb1
    FROM knb1
    FOR ALL ENTRIES IN gt_data
   WHERE kunnr = gt_data-kunnr.

  SELECT * INTO TABLE lt_a004
    FROM a004
    FOR ALL ENTRIES IN gt_data
   WHERE kappl = 'V'
     AND kschl = 'ZV00'
     AND matnr = gt_data-matnr
     AND datab <= gt_data-sqdt
     AND datbi >= gt_data-sqdt.

  IF lt_a004[] IS NOT INITIAL.
    SELECT * INTO TABLE lt_konp
      FROM konp
      FOR ALL ENTRIES IN lt_a004
     WHERE knumh = lt_a004-knumh.
  ENDIF.

** : For Vehicle
  LOOP AT gt_data INTO ls_data WHERE gubun = 'V'.

    CLEAR : ls_a004, ls_konp, l_zterm, wk_fdes.

    READ TABLE lt_a004 INTO ls_a004 WITH KEY matnr = ls_data-matnr.

    IF sy-subrc = 0.
      READ TABLE lt_konp INTO ls_konp WITH KEY knumh = ls_a004-knumh.
      IF sy-subrc = 0.
        wk_fdes-wrshb = ls_konp-kbetr.
        wk_fdes-dmshb = ls_konp-kbetr * ls_data-plnmg.
        wk_fdes-dispw = ls_konp-konwa.
      ENDIF.
    ENDIF.

    l_datum = ls_data-sqdt + 3. "" Vehicle Leadtime

    READ TABLE lt_knb1 INTO ls_knb1 WITH KEY kunnr = ls_data-kunnr.

    IF sy-subrc = 0.
      wk_fdes-grupp = ls_knb1-fdgrv.
      l_zterm = ls_knb1-zterm.
      PERFORM get_paydate USING l_datum l_zterm wk_fdes-datum.
    ENDIF.

    wk_fdes-bukrs = p_bukrs.
    wk_fdes-EBENE = c_sptype.

    CONCATENATE '(Vehicle) PIR to Cash flow conversion(' sy-datum ')'
               INTO wk_fdes-sgtxt.

    COLLECT wk_fdes.

    ls_list-gubun  = 'V'.
    ls_list-matnr  = ls_data-matnr.
    ls_list-kunnr  = ls_data-kunnr.
    ls_list-fdgrv  = ls_knb1-fdgrv.
    ls_list-sqdt   = ls_data-sqdt.
    ls_list-paydt  = wk_fdes-datum.
    ls_list-zterm  = l_zterm.
    ls_list-plnmg  = ls_data-plnmg.
    ls_list-dmshb  = wk_fdes-dmshb.

    APPEND ls_list TO gt_list.

  ENDLOOP.

** : For Engine
  REFRESH : lt_konp.
  CLEAR : l_datum.

  SELECT * INTO TABLE lt_a005
    FROM a005
    FOR ALL ENTRIES IN gt_data
   WHERE kappl = 'V'
     AND kschl = 'ZE00'
     AND kunnr = 'AKNH'
     AND matnr = gt_data-matnr
     AND datab <= gt_data-sqdt
     AND datbi >= gt_data-sqdt.

  IF lt_a005[] IS NOT INITIAL.
    SELECT * INTO TABLE lt_konp
      FROM konp
      FOR ALL ENTRIES IN lt_a005
     WHERE knumh = lt_a005-knumh.
  ENDIF.

  LOOP AT gt_data INTO ls_data WHERE gubun = 'E'.

    CLEAR : ls_a005, ls_konp, l_zterm, wk_fdes.

    READ TABLE lt_a005 INTO ls_a005 WITH KEY matnr = ls_data-matnr.

    IF sy-subrc = 0.
      READ TABLE lt_konp INTO ls_konp WITH KEY knumh = ls_a005-knumh.
      IF sy-subrc = 0.
        wk_fdes-wrshb = ls_konp-kbetr.
        wk_fdes-dmshb = ls_konp-kbetr * ls_data-plnmg.
        wk_fdes-dispw = ls_konp-konwa.
      ENDIF.
    ENDIF.

    l_datum = ls_data-sqdt + 1. "" Engine Leadtime

    READ TABLE lt_knb1 INTO ls_knb1 WITH KEY kunnr = ls_data-kunnr.

    IF sy-subrc = 0.
      wk_fdes-grupp = ls_knb1-fdgrv.
      l_zterm = ls_knb1-zterm.
      PERFORM get_paydate USING l_datum l_zterm wk_fdes-datum.
    ENDIF.

    wk_fdes-bukrs = p_bukrs.
    wk_fdes-EBENE = c_sptype.

    CONCATENATE '(Engine) PIR to Cash flow conversion(' sy-datum ')'
               INTO wk_fdes-sgtxt.

    COLLECT wk_fdes.

    ls_list-gubun  = 'E'.
    ls_list-matnr  = ls_data-matnr.
    ls_list-kunnr  = ls_data-kunnr.
    ls_list-fdgrv  = ls_knb1-fdgrv.
    ls_list-sqdt   = ls_data-sqdt.
    ls_list-paydt  = wk_fdes-datum.
    ls_list-zterm  = l_zterm.
    ls_list-plnmg  = ls_data-plnmg.
    ls_list-dmshb  = wk_fdes-dmshb.

    APPEND ls_list TO gt_list.

  ENDLOOP.

  if p_test eq space.
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
form get_paydate USING p_datum p_zterm p_paydt.

  data: l_zfbdt like fdes-datum,
        l_rf40d like rf40d.

* LFDSDF00 - form zaverh_det.
* it_vbep-edatu
* currency...
  select single * from  t052
           where zterm eq p_zterm.

  if sy-subrc = 0.
    l_rf40d-budat = p_datum.
    l_rf40d-bukrs = p_bukrs.
    l_rf40d-koart = 'D'.              "customer
    l_rf40d-shkzg = 'S'.              "receivable

    call function 'FI_TERMS_OF_PAYMENT_PROPOSE'
         exporting
              i_bldat         = p_datum
              i_budat         = p_datum
              i_zterm         = p_zterm
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
              payday          = p_paydt.
  else.
    p_paydt  = p_datum.
  endif.

*  if t001-waers <> it_vbep-waerk.
*
*    call function 'CONVERT_TO_LOCAL_CURRENCY'
*         exporting
*              date             = l_datum
*              foreign_amount   = it_vbep-bamnt
*              foreign_currency = it_vbep-waerk
*              local_currency   = t001-waers
*         importing
*              local_amount     = it_vbep-dmshb.
*
*  else.
*    it_vbep-dmshb = it_vbep-bamnt.
*  endif.

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

  DATA : lt_list LIKE gt_list.

  g_repid = sy-repid.

* ALV HEADER & FIELD SETTING
  perform : alvprn_basic01,
            alvprn_field01,
            alvprn_list    tables gt_list.
*
endform.                    " DISPLAY_ALV
*&---------------------------------------------------------------------*
*&      Form  ALVPRN_FIELD01
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ALVPRN_FIELD01 .

  perform field_setting USING : 'S' 'FIELDNAME'   'GUBUN',
                                ' ' 'DDICTXT'     'M',
                                ' ' 'OUTPUTLEN'   '2',
                                ' ' 'JUST'        'C',
                                'E' 'SELTEXT_M'   'Class.',

                                'S' 'FIELDNAME'   'SQDT',
                                ' ' 'DDICTXT'     'M',
                                ' ' 'OUTPUTLEN'   '10',
                                ' ' 'JUST'        'C',
                                'E' 'SELTEXT_M'   'Pln.Dat',

                                'S' 'FIELDNAME'   'PAYDT',
                                ' ' 'DDICTXT'     'M',
                                ' ' 'OUTPUTLEN'   '10',
                                ' ' 'JUST'        'C',
                                'E' 'SELTEXT_M'   'Pay.Dat',

                                'S' 'FIELDNAME'   'MATNR',
                                ' ' 'DDICTXT'     'M',
                                ' ' 'OUTPUTLEN'   '18',
                                ' ' 'KEY'         ' ',
                                'E' 'SELTEXT_M'   'Material',

                                'S' 'FIELDNAME'   'KUNNR',
                                ' ' 'DDICTXT'     'M',
                                ' ' 'OUTPUTLEN'   '10',
                                ' ' 'KEY'         ' ',
                                'E' 'SELTEXT_M'   'Customer',

                                'S' 'FIELDNAME'   'FDGRV',
                                ' ' 'DDICTXT'     'M',
                                ' ' 'OUTPUTLEN'   '10',
                                'E' 'SELTEXT_M'   'Pln.Grp',

                                'S' 'FIELDNAME'   'ZTERM',
                                ' ' 'DDICTXT'     'M',
                                ' ' 'OUTPUTLEN'   '4',
                                'E' 'SELTEXT_M'   'Payment Key',

                                'S' 'FIELDNAME'   'PLNMG',
                                ' ' 'DDICTXT'     'R',
                                ' ' 'OUTPUTLEN'   '10',
                                ' ' 'REF_TABNAME'   'ZTPP_ENG_PIR',
                                ' ' 'REF_FIELDNAME' 'PLNMG',
                                ' ' 'DO_SUM'      'X',
                                'E' 'SELTEXT_M'   'Qty',

                                'S' 'FIELDNAME'   'DMSHB',
                                ' ' 'DDICTXT'     'R',
                                ' ' 'OUTPUTLEN'   '15',
                                ' ' 'REF_TABNAME'   'KONP',
                                ' ' 'REF_FIELDNAME'   'KBETR',
                                ' ' 'DO_SUM'      'X',
                                'E' 'SELTEXT_M'   'Amount'.

ENDFORM.                    " ALVPRN_FIELD01
*&---------------------------------------------------------------------*
*&      Form  FIELD_SETTING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_FIELDCAT  text
*      -->P_0787   text
*      -->P_0788   text
*      -->P_0789   text
*----------------------------------------------------------------------*
FORM FIELD_SETTING  USING  p_gub  p_fname  p_value.

*- 'S' -> Start
*- 'E' -> End

  IF p_gub = 'S'.

    CLEAR gs_fieldcat.

  ENDIF.

*-
  DATA l_fname(40).
  FIELD-SYMBOLS <fs> TYPE ANY.
  CONCATENATE 'GS_FIELDCAT-' p_fname INTO l_fname.

  ASSIGN (l_fname) TO <fs>.
  <fs> = p_value.

  IF p_gub = 'E'.

    APPEND gs_fieldcat TO gt_fieldcat.

  ENDIF.
ENDFORM.                    " FIELD_SETTING
