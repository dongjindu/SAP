*&---------------------------------------------------------------------*
*&  Include           ZRFIT13N_F01
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
      PERFORM dele_data.
      PERFORM save_data.
  ENDCASE.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  alvprn_list
*&---------------------------------------------------------------------*
FORM alvprn_list TABLES p_tab.
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
          t_outtab                 =  p_tab.
ENDFORM.                    " alvprn_list
*&---------------------------------------------------------------------*
*&      Form  alvprn_basic01
*&---------------------------------------------------------------------*
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

ENDFORM.                    " alvprn_basic01
*&-------------------------------------------------------------------
*&      Form  alvprn_field01_monthly
*&-------------------------------------------------------------------
FORM alvprn_field01_monthly USING p_intab.
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
                                'S' 'MATNR'       ' ',
                                ' ' 'DDICTXT'     'M',
                                ' ' 'OUTPUTLEN'   '18',
                                ' ' 'KEY'         ' ',
                                'E' 'SELTEXT_M'   'Material',

                                'S' 'ACTYP'       ' ',
                                ' ' 'DDICTXT'     'S',
                                ' ' 'OUTPUTLEN'   '02',
                                ' ' 'KEY'         ' ',
                                'E' 'SELTEXT_S'   'AT',

                                'S' 'KUNNR'       ' ',
                                ' ' 'DDICTXT'     'M',
                                ' ' 'OUTPUTLEN'   '10',
                                ' ' 'KEY'         ' ',
                                'E' 'SELTEXT_M'   'Cust/Vnd',

                                'S' 'GRUPP'       ' ',
                                ' ' 'DDICTXT'     'M',
                                ' ' 'OUTPUTLEN'   '6',
                                ' ' 'KEY'         ' ',
                                'E' 'SELTEXT_M'   'PlnGrp',

                                'S' 'PDATU'       ' ',
                                ' ' 'DDICTXT'     'M',
                                ' ' 'JUST'        'C',
                                ' ' 'KEY'         ' ',
                                ' ' 'OUTPUTLEN'   '10',
                                'E' 'SELTEXT_M'   'Org.Date',

                                'S' 'DATUM'       ' ',
                                ' ' 'DDICTXT'     'M',
                                ' ' 'JUST'        'C',
                                ' ' 'KEY'         ' ',
                                ' ' 'OUTPUTLEN'   '10',
                                'E' 'SELTEXT_M'   'Pln.Date',

                                'S' 'PLNMG'       ' ',
                                ' ' 'DDICTXT'     'M',
                                ' ' 'OUTPUTLEN'   '15',
                                ' ' 'ROUND'       '0',
                                ' ' 'DO_SUM'      'X',
                                'E' 'SELTEXT_M'   'Qty.',

                                'S' 'NETPR'       ' ',
                                ' ' 'DDICTXT'     'M',
                                ' ' 'OUTPUTLEN'   '10',
                                ' ' 'CURRENCY'    it_alv-waers,
                                'E' 'SELTEXT_M'   'Net Price',

                                'S' 'WAERS'       ' ',
                                ' ' 'DDICTXT'     'M',
                                ' ' 'OUTPUTLEN'   '05',
                                'E' 'SELTEXT_M'   'Curr',

                                'S' 'KBETR'       ' ',
                                ' ' 'DDICTXT'     'M',
                                ' ' 'OUTPUTLEN'   '18',
                                ' ' 'DO_SUM'      'X',
                                ' ' 'CURRENCY'    it_alv-waers,
                                'E' 'SELTEXT_M'   'Amount',

                                'S' 'DMSHB'       ' ',
                                ' ' 'DDICTXT'     'M',
                                ' ' 'OUTPUTLEN'   '18',
                                ' ' 'DO_SUM'      'X',
                                ' ' 'CURRENCY'    sv_waers,
                                'E' 'SELTEXT_M'   'Amount',

                                'S' 'STTS'        ' ',
                                ' ' 'DDICTXT'     'S',
                                ' ' 'OUTPUTLEN'   '02',
                                'E' 'SELTEXT_S'   'S'.
ENDFORM.
*&-------------------------------------------------------------------
*&      Form  form_setting
*&-------------------------------------------------------------------
FORM form_setting TABLES p_events_t LIKE gt_events
                   USING p_com   p_form.

  DATA : l_event_s    TYPE  slis_alv_event.

  READ TABLE  p_events_t  WITH KEY  name = p_com
                            INTO l_event_s.
  IF   sy-subrc EQ 0.
    MOVE     p_form      TO   l_event_s-form.
    MODIFY   p_events_t  FROM l_event_s INDEX sy-tabix.
  ENDIF.
ENDFORM.                    " form_setting
*&-------------------------------------------------------------------
*&      Form  field_setting
*&-------------------------------------------------------------------
FORM field_setting TABLES p_fieldcat_t LIKE gt_fieldcat
                    USING p_gub p_fname p_con.

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

  APPEND g_fieldcat_s TO p_fieldcat_t.
ENDFORM.                    " field_setting
*&---------------------------------------------------------------------*
*&      Form  dele_data
*&---------------------------------------------------------------------*
FORM dele_data.
  CLEAR: wk_dele[], wk_dele.
* IF p_mhly EQ 'X'.
  SELECT * FROM fdes
           INTO CORRESPONDING FIELDS OF wk_dele
           WHERE bukrs EQ p_bukrs
           AND   archk EQ space
           AND ( dsart EQ c_sftyp OR
                 dsart EQ c_mftyp ).
    APPEND wk_dele.

    wk_dele-archk = 'X'.
    wk_dele-aenus = sy-uname.
    wk_dele-aendt = sy-datum.
    wk_dele-avdat = sy-datum.
    APPEND wk_dele.  CLEAR wk_dele.
  ENDSELECT.

*  ELSEIF p_yrly EQ 'X'.
*    SELECT * FROM fdes
*             INTO CORRESPONDING FIELDS OF wk_dele
*             WHERE bukrs EQ p_bukrs
*             AND   archk EQ space
*             AND   refer EQ c_refer.  "refer: COPA
*      wk_dele-archk = 'X'.
*      wk_dele-aenus = sy-uname.
*      wk_dele-aendt = sy-datum.
*      wk_dele-avdat = sy-datum.
*      APPEND wk_dele.  CLEAR wk_dele.
*    ENDSELECT.
*  ENDIF.

  IF NOT wk_dele[] IS INITIAL.
    CALL FUNCTION 'CASH_FORECAST_MEMO_RECORD_UPD'
         EXPORTING
              aktion   = '2'
         TABLES
              tab_fdes = wk_dele.
*fixing problem (Hakchin)
    COMMIT WORK.
  ENDIF.
ENDFORM.                    " dele_data
*&---------------------------------------------------------------------*
*&      Form  save_data
*&---------------------------------------------------------------------*
FORM save_data.
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
ENDFORM.                    " save_data
*&---------------------------------------------------------------------*
*&      Form  get_customer_alloc
*&---------------------------------------------------------------------*
FORM get_customer_alloc.
  CLEAR: it_kun[], it_kun.
*-- customer allocation data
  SELECT * FROM ztfi_map3
           INTO CORRESPONDING FIELDS OF TABLE it_kun
           WHERE bukrs EQ p_bukrs.
ENDFORM.                    " get_customer_alloc
*&---------------------------------------------------------------------*
*&      Form  get_kunnr_matnr
*&---------------------------------------------------------------------*
FORM get_kunnr_matnr.
  DATA:   l_subrc LIKE sy-subrc.

  CLEAR: it_mat[], it_mat.

  PERFORM read_production_plan.

*-- assumption : material master code determine customer.
*--              Vehicle FSC include customer code.
*--              A/S parts will be sold to MOBIS only
*-- material + customer price data
  LOOP AT it_mat.
    READ TABLE i_matnr WITH KEY matnr = it_mat-matnr.
    CHECK sy-subrc = 0.

    CLEAR l_subrc.
    IF i_matnr-kkref = '0009'.
* skip special purpose vehicle...
      CHECK it_mat-matnr+4(1) <> 'X'.
      PERFORM get_sales_price_v USING l_subrc.

    ELSEIF i_matnr-kkref = '0008'.
      PERFORM get_sales_price_cm USING l_subrc.
    ELSE.
      CONTINUE.
    ENDIF.

    CHECK l_subrc = 0.

* Andy -- start
    MOVE-CORRESPONDING it_mat TO it_kun.
*----- amount = net price * qty
    it_kun-netpr = it_mat-kbetr.
    it_kun-kbetr = ( it_mat-kbetr * it_mat-plnmg ).         " * -1.
    it_kun-actyp = 'D'.   "Account type : D (Customer)

    it_kun-dsart = c_sftyp.        "planning type : SF
    APPEND it_kun.  CLEAR it_kun.
* Andy -- end.

  ENDLOOP.

* customer allocation...
*  LOOP AT it_kun.
*    READ TABLE it_mat WITH KEY kunnr = it_kun-kunnr.
*    IF sy-subrc EQ 0.
*      MOVE-CORRESPONDING it_mat TO it_kun.
**----- amount = ( net price * qty ) * allocation
*      it_kun-kbetr = ( it_mat-kbetr * it_mat-plnmg ) * it_kun-alloc.
*      it_kun-dsart = c_sftyp.   "planning type : SF
*      MODIFY it_kun.  CLEAR it_kun.
*    ENDIF.
*  ENDLOOP.
ENDFORM.                    " get_kunnr_matnr
*&---------------------------------------------------------------------*
*&      Form  determine_date_locamt
*&---------------------------------------------------------------------*
FORM determine_date_locamt.
  DATA: l_stts.

*--Day limit
  LOOP AT it_kun.
*   stat=> 0: ok, 1: Plan Group missing, 2: amount is zero, 9: other
*---list display
    MOVE-CORRESPONDING it_kun  TO it_alv.

    PERFORM get_cm_group USING it_alv-grupp
                               it_kun-zterm
                               it_kun-waers
                               l_stts.


* calc local currency
    PERFORM get_localamt.
    IF it_alv-dmshb IS INITIAL.
      it_alv-stts = '2'.
    ELSE.
      it_alv-stts = l_stts.  "status
    ENDIF.

* determine date.
    PERFORM get_paydate.

*....................................
    IF p_detail = 'X'.
      APPEND it_alv.
    ELSE.
      CLEAR it_alv-matnr.  COLLECT it_alv.
    ENDIF.

*---create memo record
    IF it_alv-stts < 2.
      IF it_alv-stts = 1.
        wk_fdes-sgtxt = 'Planning Group cannot be determined'.
      ENDIF.

      wk_fdes-dsart = it_alv-dsart.
      wk_fdes-bukrs = p_bukrs.
      wk_fdes-grupp = it_alv-grupp.
      wk_fdes-datum = it_alv-datum.
      wk_fdes-dispw = it_alv-waers.
      wk_fdes-kurst = 'M'.
      wk_fdes-wrshb = it_alv-kbetr.
      wk_fdes-dmshb = it_alv-dmshb.
      wk_fdes-refer = it_alv-pdatu.
      wk_fdes-zuonr = it_alv-matnr.
      wk_fdes-merkm = it_alv-kunnr.

      COLLECT wk_fdes.  CLEAR wk_fdes.
    ENDIF.
    CLEAR: it_alv.

  ENDLOOP.
ENDFORM.                    " determine_date_locamt
*&---------------------------------------------------------------------*
*&      Form  get_lifnr_matnr
*&---------------------------------------------------------------------*
FORM get_lifnr_matnr.
  DATA: l_peinh LIKE ekpo-peinh.

*-- vendor material
  SELECT matnr gsmng pedtr flief konnr plnum
*         INTO CORRESPONDING FIELDS OF TABLE it_plaf
         INTO CORRESPONDING FIELDS OF it_plaf
               FROM plaf
               FOR ALL ENTRIES IN r_matnr
                WHERE beskz IN ('X', 'F')  "procurement type
                  AND sobes IN ('0', '2')  "special proc.type
                  AND plscn EQ p_plscn     "planning scenario
*                 AND psttr IN r_datum
                  AND pedtr IN r_datum
                  AND matnr EQ r_matnr-low.
*                 AND matnr IN r_matnr.
    COLLECT it_plaf.

  ENDSELECT.

  LOOP AT it_plaf.
    CLEAR it_kun.
*---filter KD Material..
*(if PR req.tracking no = Planned order no, Skip) : NO
*(if PR del.date = planned date) : YES
*    IF it_plaf-matnr IN r_kdmat.
*      SELECT SINGLE bednr INTO eban-bednr
*                    FROM eban
*                    WHERE matnr = it_plaf-matnr
*                      AND lfdat = it_plaf-pedtr.
*      IF sy-subrc EQ 0.
*        DELETE it_plaf.  CONTINUE.
*      ENDIF.
*    ENDIF.

    it_kun-matnr = it_plaf-matnr.
*.......date...(fixit)
    it_kun-pdatu = it_plaf-pedtr.
*   it_kun-pdatu = it_plaf-psttr.

    PERFORM get_vendor_price  USING l_peinh.

*.......amount = ( net price * qty ) * -1
    IF l_peinh > 1. "if price unit > 1, (net price/price unit) * qty
      it_kun-netpr = ( it_kun-kbetr / l_peinh ).
      it_kun-kbetr = ( it_kun-kbetr / l_peinh ) * it_plaf-gsmng.
    ELSE.
      it_kun-netpr = it_kun-kbetr.
      it_kun-kbetr = it_kun-kbetr * it_plaf-gsmng.
    ENDIF.

    it_kun-kbetr = it_kun-kbetr * -1.
    it_kun-plnmg = it_plaf-gsmng.
    it_kun-actyp = 'K'.       "Account type : K (Vendor)
    it_kun-dsart = c_mftyp.   "planning type : MF

    APPEND it_kun.

  ENDLOOP.

ENDFORM.                    " get_lifnr_matnr
*&---------------------------------------------------------------------*
*&      Form  get_localamt
*&---------------------------------------------------------------------*
FORM get_localamt.
* LFDSDF00 - form zaverh_det.
* it_vbep-edatu
* currency...

*error : JPark, please correct it...
  IF it_alv-waers = space.
    it_alv-waers = sv_waers.
  ENDIF.

  IF it_alv-kbetr = 0.
    it_alv-dmshb = 0.

  ELSE.
    IF sv_waers = it_alv-waers.
      it_alv-dmshb = it_alv-kbetr.
    ELSE.
      CALL FUNCTION 'CONVERT_TO_LOCAL_CURRENCY'
           EXPORTING
                date             = it_alv-pdatu
                foreign_amount   = it_alv-kbetr
                foreign_currency = it_alv-waers
                local_currency   = sv_waers
           IMPORTING
                local_amount     = it_alv-dmshb.
    ENDIF.
  ENDIF.

*    SELECT * FROM t052
*             WHERE zterm EQ it_kun-zterm
*             AND   ztagg >= it_kun-pdatu+6(2).
*      wa052-amon = t052-zmona.  "(+)additional month
*      wa052-fday = t052-zfael.  "fixed day
*      EXIT.
*    ENDSELECT.
*    IF sy-subrc EQ 0.
*      IF NOT wa052-amon IS INITIAL.
*        CALL FUNCTION 'MONTH_PLUS_DETERMINE'
*             EXPORTING
*                  months  = wa052-amon
*                  olddate = it_kun-pdatu
*             IMPORTING
*                  newdate = it_kun-pdatu.
*      ENDIF.
*      CONCATENATE it_kun-pdatu(6) wa052-fday INTO wk_fdes-datum.
*    ELSE.
*      wk_fdes-datum = it_kun-pdatu.
*    ENDIF.


ENDFORM.                    " get_localamt
*&---------------------------------------------------------------------*
*&      Form  get_material_master
*&---------------------------------------------------------------------*
FORM get_material_master.
  DATA: l_matnr LIKE mara-matnr,
        l_profl LIKE mara-profl,
        l_bwkey LIKE mbew-bwkey,
        l_bklas LIKE mbew-bklas.
  RANGES: r_bklas FOR t025-bklas.

  SELECT * FROM t025 INTO TABLE i_t025
      WHERE kkref = '0001'  "Production Material
         OR kkref = '0002'  "Raw/Sub Material
         OR kkref = '0008'  "MIP
         OR kkref = '0009'. "Vehicle

  r_bklas-sign = 'I'.  r_bklas-option = 'EQ'.
  LOOP AT i_t025.
    MOVE i_t025-bklas TO r_bklas-low.
    APPEND r_bklas.
  ENDLOOP.

  CLEAR: r_matnr[], r_matnr, r_kdmat, r_kdmat.
  r_matnr-sign   = 'I'. r_matnr-option = 'EQ'.
  r_kdmat-sign   = 'I'. r_kdmat-option = 'EQ'.

  SELECT a~matnr a~profl b~bklas b~bwkey
      INTO (l_matnr, l_profl, l_bklas, l_bwkey)
      FROM mara AS a JOIN mbew AS b
        ON a~matnr = b~matnr
      WHERE bklas IN r_bklas
        AND a~matnr IN s_matnr.

    CASE l_profl.
      WHEN 'L'.    "LP
        MOVE l_matnr TO  r_matnr-low.
        APPEND r_matnr.
      WHEN 'K'.    "KD
        MOVE l_matnr TO  r_matnr-low.
        APPEND r_matnr.

        MOVE l_matnr TO  r_kdmat-low.
        APPEND r_kdmat.
      WHEN OTHERS.
        READ TABLE i_t025 WITH KEY bklas = l_bklas.
* MIP/Vehicle
        IF i_t025-kkref = '0008' OR i_t025-kkref = '0009'.
          i_matnr-matnr = l_matnr.
          i_matnr-bklas = l_bklas.
          i_matnr-kkref = i_t025-kkref.
          APPEND i_matnr.
        ELSE.
          MOVE l_matnr TO  r_matnr-low.
          APPEND r_matnr.
        ENDIF.
    ENDCASE.

    it_matpl-matnr = l_matnr.
    it_matpl-bwkey = l_bwkey.
    COLLECT it_matpl.
  ENDSELECT.

ENDFORM.                    " get_material_master
*&---------------------------------------------------------------------*
*&      Form  get_paydate
*&---------------------------------------------------------------------*
FORM get_paydate.
  DATA: l_zfbdt  LIKE fdes-datum,
        l_basedt LIKE fdes-datum,
        l_land1  LIKE t001-land1,
        l_rf40d  LIKE rf40d.

  IF it_kun-actyp = 'D'.
    l_basedt = it_alv-pdatu + p_dayc.
  ELSE.
    SELECT SINGLE land1 INTO sv_land1 FROM lfa1
                        WHERE lifnr = it_kun-kunnr.
    IF l_land1 = sv_land1.
      l_basedt = it_alv-pdatu.
    ELSE.
      l_basedt = it_alv-pdatu + p_dayv.
    ENDIF.
  ENDIF.

  IF it_kun-zterm = space.
    it_alv-datum = l_basedt.
  ELSE.
*    SELECT SINGLE * FROM  t052
*             WHERE zterm EQ it_kun-zterm.

    l_rf40d-budat = l_basedt.
    l_rf40d-bukrs = p_bukrs.
    l_rf40d-koart = 'K'.              "vendor
    l_rf40d-shkzg = 'H'.              "payable

    CALL FUNCTION 'FI_TERMS_OF_PAYMENT_PROPOSE'
         EXPORTING
              i_bldat         = it_alv-pdatu
              i_budat         = it_alv-pdatu
              i_zterm         = it_kun-zterm
         IMPORTING
              e_zfbdt         = l_rf40d-zfbdt
              e_zbd1t         = l_rf40d-zbd1t
              e_zbd2t         = l_rf40d-zbd2t
              e_zbd3t         = l_rf40d-zbd3t
              e_zbd1p         = l_rf40d-zbd1p
              e_rebzg         = l_rf40d-rebzg
              e_zlsch         = l_rf40d-zlsch
         EXCEPTIONS
              terms_not_found = 1
              OTHERS          = 2.


    CALL FUNCTION 'PAYDAY_DETERMINATION'
         EXPORTING
              i_rf40d         = l_rf40d
              payment_history = ' '
         IMPORTING
              payday          = it_alv-datum.


*  call function 'CASH_FORECAST_LEVEL_AND_DATE_2'

  ENDIF.

ENDFORM.                    " get_paydate
*&---------------------------------------------------------------------*
*&      Form  alvprn_sort_build
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM alvprn_sort_build.
  CLEAR ls_sort.
  ls_sort-fieldname = 'PDATU'.
  ls_sort-spos = 1.    "---> KEY
  ls_sort-up = 'X'.    "ascending/desending
  ls_sort-subtot = 'X'.
  APPEND ls_sort TO gt_sort.
ENDFORM.                    " alvprn_sort_build
*&---------------------------------------------------------------------*
*&      Form  init_data
*&---------------------------------------------------------------------*
FORM init_data.

  DATA: w_date LIKE sy-datum.
  DATA: g_mon_to(2) TYPE n,
        g_yea_to(4) TYPE n.
  CLEAR: it_alv[], it_alv, r_datum[].

  SELECT SINGLE waers land1  INTO (sv_waers, sv_land1)
                FROM t001
                WHERE bukrs EQ p_bukrs.

  rv_vkorg-sign = 'I'.
  rv_vkorg-option = 'EQ'.
  SELECT vkorg INTO rv_vkorg-low
                FROM tvko WHERE bukrs = p_bukrs.
    APPEND rv_vkorg.
  ENDSELECT.
* PERFORM get_customer_alloc.

* IF p_mhly EQ 'X'.

  g_mon_to = p_month + p_perid - 1.

  IF g_mon_to > 12.
    g_yea_to = p_gjahr + 1.
    g_mon_to = g_mon_to - 12.
  ELSE.
    g_yea_to = p_gjahr.
  ENDIF.

* DAY_IN_WEEK( today ).
* NEXT_WEEK( current week )
  w_date = sy-datum + 7.
  CALL FUNCTION 'GET_WEEK_INFO_BASED_ON_DATE'
       EXPORTING
            date   = w_date
       IMPORTING
            monday = w_date.

  g_seqfr = sy-datum.
  g_seqto = w_date - 1.
  g_mrpfr = w_date.
  g_mrpfr = w_date + 20.

  r_datum-sign = 'I'.  r_datum-option = 'BT'.
  r_ltpdt-sign = 'I'.  r_ltpdt-option = 'BT'.
  r_seqdt-sign = 'I'.  r_seqdt-option = 'BT'.

*  concatenate p_gjahr p_month '01' into r_datum-low.
  r_datum-low  = w_date.
  r_ltpdt-low  = g_seqto + 1.
  r_seqdt-low  = g_seqfr.
  r_seqdt-high = g_seqto.

  CONCATENATE g_yea_to g_mon_to '01' INTO r_datum-high.
  CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
       EXPORTING
            day_in            = r_datum-high
       IMPORTING
            last_day_of_month = r_datum-high
       EXCEPTIONS
            day_in_no_date    = 1
            OTHERS            = 2.

  r_ltpdt-high = r_datum-high.

  APPEND: r_datum, r_ltpdt, r_seqdt.

ENDFORM.                    " init_data
*&---------------------------------------------------------------------*
*&      Form  get_vendor_price
*&---------------------------------------------------------------------*
FORM get_vendor_price USING f_peinh.

  DATA: l_ebeln LIKE ekpo-ebeln.

*---- fixed vendor / agreement => scheduling agreement
  IF it_plaf-konnr NE space AND it_plaf-flief NE space.
    it_kun-kunnr = it_plaf-flief.
    SELECT SINGLE  b~netpr a~waers b~peinh   "currency key, price unit
           INTO (it_kun-kbetr, it_kun-waers, f_peinh)
           FROM ekko AS a JOIN ekpo AS b
           ON a~ebeln = b~ebeln
           WHERE a~ebeln = it_plaf-konnr
             AND a~bstyp EQ 'L'  "agreement
             AND a~kdatb <= it_plaf-pedtr
             AND a~kdate >= it_plaf-pedtr.

*              AND a~kdatb <= it_plaf-psttr  "value date
*              AND a~kdate >= it_plaf-psttr.

*      ENDSELECT.

  ELSE.
*---- info record  (T-Code: ME1M Trace)
*.....vendor, qty, price unit, effective price (not net price)
    SELECT SINGLE a~lifnr
                  b~effpr b~waers b~peinh
           INTO (it_kun-kunnr,
                 it_kun-kbetr, it_kun-waers, f_peinh)
           FROM eina AS a JOIN eine AS b
           ON a~infnr = b~infnr
           WHERE a~matnr =  it_plaf-matnr
             AND a~loekz = space           "DELETION
             AND b~loekz = space
             AND b~prdat >= it_plaf-pedtr.
*     ENDSELECT.

    IF sy-subrc <> 0.
      SELECT SINGLE a~lifnr INTO it_kun-kunnr
             FROM eina AS a JOIN eine AS b
             ON a~infnr = b~infnr
             WHERE a~matnr =  it_plaf-matnr
               AND a~loekz = space           "DELETION
               AND b~loekz = space.
* check po history
      IF sy-subrc <> 0.
        SELECT * FROM ekpo
            WHERE matnr = it_plaf-matnr
            ORDER BY prdat DESCENDING.
          l_ebeln = ekpo-ebeln.
          EXIT.
        ENDSELECT.

        SELECT SINGLE lifnr INTO it_kun-kunnr
            FROM ekko
            WHERE ebeln = l_ebeln.

      ENDIF.
    ENDIF.
  ENDIF.

*tables: mbew.
  IF it_kun-kbetr = 0.
    SELECT SINGLE * FROM mbew
       WHERE matnr = it_plaf-matnr
         AND stprs > 0.
*         bwkey = p_BWKEY.

    IF mbew-verpr = 0.
      it_kun-kbetr = mbew-stprs.
    ELSE.
      it_kun-kbetr = mbew-verpr.
    ENDIF.
    f_peinh = mbew-peinh.
    it_kun-waers = sv_waers.
  ENDIF.

ENDFORM.                    " get_vendor_price
*&---------------------------------------------------------------------*
*&      Form  read_production_plan
*&---------------------------------------------------------------------*
FORM read_production_plan.

  clear: it_mat, it_mat[].

* Vehicle
  SELECT a~matnr b~pdatu b~plnmg a~pbdnr b~entlu
         INTO CORRESPONDING FIELDS OF TABLE it_mat
         FROM pbim AS a JOIN pbed AS b
         ON a~bdzei = b~bdzei      "Independent requirements pointer
         WHERE a~versb EQ p_versb  "version '99'
*        and   a~pbdnr eq 'LTP_SUM'
         AND   b~pdatu IN r_ltpdt
         AND   a~matnr IN s_matnr
         AND   b~plnmg > 0.

* A/S part
  SELECT a~matnr b~pdatu b~plnmg  a~pbdnr b~entlu
         APPENDING CORRESPONDING FIELDS OF TABLE it_mat
         FROM pbim AS a JOIN pbed AS b
         ON a~bdzei = b~bdzei      "Independent requirements pointer
         WHERE a~versb EQ p_veras  "version 'AS'
*         AND   a~pbdnr EQ 'LTP_SUM'
         AND   b~pdatu IN r_datum
         AND   a~matnr IN s_matnr
         AND   b~plnmg > 0.

ENDFORM.                    " read_production_plan
*&---------------------------------------------------------------------*
*&      Form  get_sales_price_cm
*&---------------------------------------------------------------------*
FORM get_sales_price_cm  USING f_subrc LIKE sy-subrc.

* ZP00 - price(A005-customer/materil), ZP07 - margin (A307)
  DATA: l_knumh LIKE a005-knumh,
        l_kbetr LIKE konp-kbetr,
        l_matnr LIKE a004-matnr.

  DATA: BEGIN OF i_a004 OCCURS 0,
          matnr LIKE a004-matnr,
          knumh LIKE a004-knumh,
          kunnr LIKE a005-kunnr,
        END OF i_a004.
  DATA: i_a005 LIKE i_a004 OCCURS 0 WITH HEADER LINE.

  f_subrc = 1.
  CLEAR l_knumh.

  SELECT SINGLE knumh kunnr INTO (l_knumh, it_mat-kunnr)
         FROM a005
         WHERE kappl = 'V'
           AND kschl = 'ZP00'
           AND matnr EQ it_mat-matnr
           AND vkorg IN rv_vkorg
           AND datab <= it_mat-pdatu  "value date <= finish date
           AND datbi >= it_mat-pdatu.

  IF sy-subrc <> 0.
*  SELECT SINGLE kunnr INTO knb1-kunnr
*                FROM knb1
*                WHERE kunnr EQ it_mat-matnr+1(5).
*  IF sy-subrc EQ 0.
    REFRESH i_a005. CLEAR i_a005.
    CONCATENATE it_mat-matnr(13) '%' INTO l_matnr.
    SELECT matnr knumh INTO TABLE i_a005
           FROM a005
           WHERE kappl = 'V'
           AND   kschl = 'ZP00'
           AND   matnr LIKE l_matnr
           AND   vkorg IN rv_vkorg
           AND   datab <= it_mat-pdatu  "value date <= finish date
           AND   datbi >= it_mat-pdatu.
    SORT i_a005 BY matnr DESCENDING.
    READ TABLE i_a005 INDEX 1.
    l_knumh      = i_a005-knumh.
    it_mat-kunnr = i_a005-kunnr.
  ENDIF.

  CHECK l_knumh <> space.

  SELECT SINGLE kbetr konwa INTO (it_mat-kbetr, it_mat-waers)
              FROM konp
              WHERE knumh EQ l_knumh.

  SELECT SINGLE knumh INTO l_knumh FROM a307
       WHERE kappl = 'V'
         AND kschl = 'ZP07'
         AND vkorg IN rv_vkorg
         AND kunnr = it_mat-kunnr
         AND datab <= it_mat-pdatu  "value date <= finish date
         AND datbi >= it_mat-pdatu.

  SELECT SINGLE kbetr INTO l_kbetr
              FROM konp WHERE knumh EQ l_knumh.

  it_mat-kbetr = it_mat-kbetr * ( 1  + l_kbetr / 1000 ).
  CLEAR f_subrc.

ENDFORM.                    " get_sales_price_cm
*&---------------------------------------------------------------------*
*&      Form  get_sales_price_v
*&---------------------------------------------------------------------*
FORM get_sales_price_v  USING f_subrc.

  DATA: BEGIN OF i_a004 OCCURS 0,
          matnr LIKE a004-matnr,
          knumh LIKE a004-knumh,
          kunnr LIKE a005-kunnr,
        END OF i_a004.
  DATA: i_a005 LIKE i_a004 OCCURS 0 WITH HEADER LINE.

  DATA: l_knumh LIKE a005-knumh,
        l_kbetr LIKE konp-kbetr,
        l_matnr LIKE a004-matnr.

  f_subrc = 1.
  CLEAR l_knumh.

* A004 - Material : Vehicle
  SELECT SINGLE knumh INTO l_knumh FROM a004
         WHERE kappl = 'V'
           AND kschl = 'ZV00'
           AND matnr EQ it_mat-matnr
           AND vkorg IN rv_vkorg
           AND datab <= it_mat-pdatu  "value date <= finish date
           AND datbi >= it_mat-pdatu.
*****
* if not found, then skip it.
*     IF sy-subrc <> 0.  CONTINUE. ENDIF.
  IF sy-subrc <> 0.

    REFRESH i_a005.
    CONCATENATE it_mat-matnr(13) '%' INTO l_matnr.
    SELECT matnr knumh INTO TABLE i_a004
           FROM a004
           WHERE kappl = 'V'
             AND kschl = 'ZV00'
             AND matnr LIKE l_matnr
             AND vkorg IN rv_vkorg
             AND datab <= it_mat-pdatu  "value date <= finish date
             AND datbi >= it_mat-pdatu.
    SORT i_a004 BY matnr DESCENDING.
    READ TABLE i_a004 INDEX 1.
    l_knumh = i_a004-knumh.
  ENDIF.
  CHECK l_knumh <> space.

******
  SELECT SINGLE kbetr konwa
                INTO (l_kbetr, it_mat-waers)
                FROM konp
                WHERE knumh EQ l_knumh.
* it_mat-kbetr = it_mat-kbetr + l_kbetr.
  it_mat-kbetr = l_kbetr.


*FIXME for customer code determination
*3 or 5
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
       EXPORTING
            input  = it_mat-matnr+1(5)
       IMPORTING
            output = it_mat-kunnr.

  CLEAR f_subrc.

ENDFORM.                    " get_sales_price_v
*&---------------------------------------------------------------------*
*&      Form  get_cm_group
*&---------------------------------------------------------------------*
FORM get_cm_group  USING f_grp f_zterm f_curr f_stat.

  DATA: l_waers LIKE t001-waers.
  CLEAR f_stat.

  IF it_kun-dsart EQ 'SF'.
*.....sales plan(customer) : planning group = CM Group => Prev.acct.no.
    SELECT SINGLE a~fdgrv b~zterm b~waers
*      SELECT SINGLE a~altkn b~zterm b~waers
                   INTO (f_grp, f_zterm, l_waers)
                   FROM knb1 AS a JOIN knvv AS b
                   ON a~kunnr = b~kunnr
                   WHERE a~bukrs EQ p_bukrs
                   AND   a~kunnr EQ it_kun-kunnr
                   AND   b~vkorg IN rv_vkorg
                   AND   b~kunnr = it_kun-kunnr.
    IF sy-subrc <> 0.
      SELECT SINGLE fdgrv zterm INTO (f_grp, f_zterm)
                  FROM knb1
                  WHERE bukrs EQ p_bukrs
                    AND kunnr EQ it_kun-kunnr.
    ENDIF.
* if no data found, use default...
    IF f_grp = space.
      f_grp = 'A4999'.
      f_stat = '1'.
    ENDIF.

  ELSE.  "MF
*.....procurement plan(vendor)
    SELECT SINGLE a~fdgrv b~zterm b~waers
                  INTO (f_grp, f_zterm, l_waers)
                  FROM lfb1 AS a JOIN lfm1 AS b
                  ON a~lifnr = b~lifnr
                  WHERE a~bukrs EQ p_bukrs
                  AND   a~lifnr EQ it_kun-kunnr.
*                   AND   b~ekorg IN rv_vkorg
*                   AND   b~lifnr = it_kun-kunnr.
    IF f_grp = space.
      f_grp = 'B4999'.
      f_stat = '1'.
    ENDIF.
  ENDIF.

*use master data currency
  IF f_curr = space.
    f_curr = l_waers.
  ENDIF.

ENDFORM.                    " get_cm_group
*&---------------------------------------------------------------------*
*&      Form  GET_LIFNR_MATNR_NEW
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_LIFNR_MATNR_NEW .

  DATA: lt_mdkp LIKE mdkp OCCURS 0 WITH HEADER LINE.
  DATA: lv_dsdat like mdkp-dsdat.
  DATA: BEGIN OF lt_mdtbx OCCURS 100.
          INCLUDE STRUCTURE mdtb.
  DATA: END OF lt_mdtbx.
  DATA: BEGIN OF lt_mdtbx_e OCCURS 100,
          MATNR   TYPE MATNR,
          WERKS   TYPE WERKS_D.
          INCLUDE STRUCTURE mdtb.
  DATA: END OF lt_mdtbx_e.
  RANGES: r_delkz FOR mdtb-delkz.

  CLEAR : r_delkz[], r_delkz.

  r_delkz-sign = 'I'. r_delkz-option = 'EQ'.
  r_delkz-low = 'AR'. APPEND r_delkz.
  r_delkz-low = 'BA'. APPEND r_delkz.
  r_delkz-low = 'BE'. APPEND r_delkz.
  r_delkz-low = 'LA'. APPEND r_delkz.
  r_delkz-low = 'LE'. APPEND r_delkz.
  r_delkz-low = 'SA'. APPEND r_delkz.
  r_delkz-low = 'SB'. APPEND r_delkz.
  r_delkz-low = 'SM'. APPEND r_delkz.
  r_delkz-low = 'U2'. APPEND r_delkz.
  r_delkz-low = 'U3'. APPEND r_delkz.

*** : MBEW
  DATA : lt_mbew TYPE TABLE OF mbew,
         ls_mbew TYPE mbew.

  SELECT * INTO TABLE lt_mbew
    FROM mbew
    FOR ALL ENTRIES IN it_matpl
   WHERE matnr = it_matpl-matnr
     AND bwkey = it_matpl-bwkey
     AND salk3 > 0.

  SORT lt_mbew.

** : MRP
  SELECT  a~mandt
          a~DTART a~MATNR a~PLWRK a~PLSCN a~DTNUM a~DSDAT a~BDBKZ
          a~SLKZ1 a~SLKZ2 a~SLKZ3 a~SLKZ4 a~SLKZ5 a~SLKZ6 a~SLKZ7
          a~SLKZ8 a~VRKZ1 a~VRKZ2 a~VRKZ3 a~MTART a~MEINS a~DISST
          a~BESKZ a~SOBSL a~SOBES a~WRK02 a~DISMM a~DISVF a~DISPO
          a~PLDIS a~EKGRP a~MTWZT a~WEBAZ a~BEAZT a~FIXTR a~MFHKZ
          a~DISLS a~LOSVF a~LOSKZ a~PERAZ a~EISBE a~MINBE a~HOEBE
          a~BSTMI a~BSTMA a~BSTFX a~BSTRF a~SUM01 a~SUM02 a~SUM03
          a~SUM04 a~SUM05 a~NEGBS a~MSGID a~MSGAR a~MSGNR a~MSGV1
          a~MSGV2 a~MSGV3 a~MSGV4 a~DISGR a~PERIV a~MRPPP a~BDARF
          a~LFRHY a~RDPRF a~BERW1 a~BERW2 a~KZAUS a~AUSDT a~NFMAT
          a~AUSZ1 a~AUSZ2 a~AUSZ3 a~AUSZ4 a~AUSZ5 a~AUSZ6 a~AUSZ7
          a~AUSZ8 a~BEADA a~NAUKZ a~SAUFT a~KZPROMO a~SHFLG a~SHZET
          a~FABKZ a~MFXDT a~BSKFL a~MAABC a~CFLAG a~GRREL a~RWPRO
          a~SHPRO a~AHDIS a~BERW4
    INTO TABLE lt_mdkp
    FROM mdkp as a
      inner join marc as b
      on  b~matnr eq a~matnr
      and b~werks eq a~PLWRK
      FOR ALL ENTRIES IN lt_mbew
    WHERE a~dtart = 'MD'
      AND a~matnr = lt_mbew-matnr
      AND a~plwrk = lt_mbew-bwkey
      and b~DISMM NE 'ND'.

* LTP = Planned Order + Simulated Qty
  SELECT a~mandt
        a~DTART a~MATNR a~PLWRK a~PLSCN a~DTNUM a~DSDAT a~BDBKZ
        a~SLKZ1 a~SLKZ2 a~SLKZ3 a~SLKZ4 a~SLKZ5 a~SLKZ6 a~SLKZ7
        a~SLKZ8 a~VRKZ1 a~VRKZ2 a~VRKZ3 a~MTART a~MEINS a~DISST
        a~BESKZ a~SOBSL a~SOBES a~WRK02 a~DISMM a~DISVF a~DISPO
        a~PLDIS a~EKGRP a~MTWZT a~WEBAZ a~BEAZT a~FIXTR a~MFHKZ
        a~DISLS a~LOSVF a~LOSKZ a~PERAZ a~EISBE a~MINBE a~HOEBE
        a~BSTMI a~BSTMA a~BSTFX a~BSTRF a~SUM01 a~SUM02 a~SUM03
        a~SUM04 a~SUM05 a~NEGBS a~MSGID a~MSGAR a~MSGNR a~MSGV1
        a~MSGV2 a~MSGV3 a~MSGV4 a~DISGR a~PERIV a~MRPPP a~BDARF
        a~LFRHY a~RDPRF a~BERW1 a~BERW2 a~KZAUS a~AUSDT a~NFMAT
        a~AUSZ1 a~AUSZ2 a~AUSZ3 a~AUSZ4 a~AUSZ5 a~AUSZ6 a~AUSZ7
        a~AUSZ8 a~BEADA a~NAUKZ a~SAUFT a~KZPROMO a~SHFLG a~SHZET
        a~FABKZ a~MFXDT a~BSKFL a~MAABC a~CFLAG a~GRREL a~RWPRO
        a~SHPRO a~AHDIS a~BERW4
    APPENDING TABLE lt_mdkp
    FROM mdkp as a
      inner join marc as b
      on  b~matnr eq a~matnr
      and b~werks eq a~PLWRK
      FOR ALL ENTRIES IN lt_mbew
  WHERE a~dtart = 'LP'
    AND a~matnr = lt_mbew-matnr
    AND a~plwrk = lt_mbew-bwkey
    AND a~plscn = '900'
    and b~DISMM NE 'ND'.

  SORT lt_mdkp BY matnr plwrk.
  read table lt_mdkp index 1.
  lv_dsdat = lt_mdkp-dsdat.

  DATA: l_date LIKE sy-datum.

* V_HTNM Vendor-Material Relationships and Conditions
* MDTC
  TABLES: mdps.
  DATA: BEGIN OF lt_mdpsx OCCURS 100.
          INCLUDE STRUCTURE mdps.
  DATA: END OF lt_mdpsx.
  DATA : BEGIN OF lt_vmat OCCURS 0,
           matnr LIKE eord-matnr,
           lifnr LIKE eord-lifnr,
           name1 LIKE lfa1-name1,
         END OF lt_vmat.
  DATA : l_mng01 TYPE mng01.
  DATA: dtart LIKE mdkp-dtart.

*  if p_plscn is initial or p_plscn = '000'.
*    dtart = mddisp. " MRP
*  else.
*    dtart = lfplan. " LTP
*  endif.

  LOOP AT lt_mdkp.
    REFRESH lt_mdtbx.

    IF lt_mdkp-dtart = 'MD'.
*      REFRESH i_mdtbx.
*    IMPORT i_mdtbx FROM DATABASE MDTC(AR) ID i_mdkp-dtnum.
      CALL FUNCTION 'READ_MRP_LIST'
           EXPORTING
                idtnum = lt_mdkp-dtnum
                icflag = lt_mdkp-cflag
           TABLES
                mdtbx  = lt_mdtbx. "rec/req.
    ELSE.
      CALL FUNCTION 'DISPOBELEG_LESEN'
           EXPORTING
                dtnum = lt_mdkp-dtnum
                cflag = lt_mdkp-cflag
                mandt = lt_mdkp-mandt
           TABLES
                mdpsx = lt_mdpsx. "rec/req.
      LOOP AT lt_mdpsx.
        MOVE-CORRESPONDING lt_mdpsx TO lt_mdtbx.
        APPEND lt_mdtbx.
      ENDLOOP.

    ENDIF.

    LOOP AT lt_mdtbx.
      lt_mdtbx_e-matnr = lt_mdkp-matnr.
      lt_mdtbx_e-werks = lt_mdkp-plwrk.
      MOVE-CORRESPONDING lt_mdtbx TO lt_mdtbx_e.
      APPEND lt_mdtbx_e.
    ENDLOOP.
  ENDLOOP.

**** : MBEW
*  DATA : lt_mbew TYPE TABLE OF mbew,
*         ls_mbew TYPE mbew.
*
*  SELECT * INTO TABLE lt_mbew
*    FROM mbew
*    FOR ALL ENTRIES IN lt_mdtbx
*   WHERE matnr = lt_mdtbx-matnr
*     AND bwkey = lt_mdtbx-werks.

*  SORT lt_mbew.

*** : Vendor infor
  SELECT eord~matnr eord~lifnr lfa1~name1 INTO TABLE lt_vmat
    FROM eord
   INNER JOIN lfa1
          ON eord~lifnr = lfa1~lifnr
    FOR ALL ENTRIES IN lt_mdtbx_e
   WHERE eord~matnr = lt_mdtbx_e-matnr
     AND eord~vdatu <= lv_dsdat
     AND eord~bdatu >= lv_dsdat.

  SORT lt_vmat.

  DATA: l_peinh LIKE ekpo-peinh.
  DATA: l_netpr TYPE netpr.

  SORT lt_mdtbx_e BY dat00.

*summarize MRP/LTP using date range
  LOOP AT lt_mdtbx_e WHERE delkz IN r_delkz
                       AND dat00 IN r_datum.

    CLEAR it_kun.

    it_kun-matnr = lt_mdtbx_e-matnr.
* .......date...(fixit)
    it_kun-pdatu = lt_mdtbx_e-dat00.

    READ TABLE lt_vmat WITH KEY matnr = lt_mdtbx_e-matnr BINARY SEARCH.
    IF sy-subrc EQ 0.
      it_kun-kunnr = lt_vmat-lifnr.
    ENDIF.

    CLEAR : ls_mbew.

    READ TABLE lt_mbew INTO ls_mbew WITH KEY matnr = lt_mdtbx_e-matnr
                                             bwkey = lt_mdtbx_e-werks
                                     BINARY SEARCH.

    IF ls_mbew-verpr = 0.
      it_kun-kbetr = ls_mbew-stprs.
    ELSE.
      it_kun-kbetr = ls_mbew-verpr.
    ENDIF.

    l_peinh = ls_mbew-peinh.
    it_kun-waers = sv_waers.

    IF lt_mdtbx_e-plumi = '-'.  "+/-
      l_mng01 = - lt_mdtbx_e-mng01.
    ELSEIF lt_mdtbx_e-plumi = '+'.  "+/-
      l_mng01 = lt_mdtbx_e-mng01.
    ENDIF.

*.......amount = ( net price * qty ) * -1
    IF l_peinh > 1. "if price unit > 1, (net price/price unit) * qty
      l_netpr = ( it_kun-kbetr / l_peinh ).
      it_kun-kbetr = ( it_kun-kbetr / l_peinh ) * l_mng01.
    ELSE.
      l_netpr = it_kun-kbetr.
      it_kun-kbetr = it_kun-kbetr * l_mng01.
    ENDIF.

    AT END OF dat00.
      it_kun-netpr = l_netpr.
    ENDAT.

    it_kun-kbetr = it_kun-kbetr.
    it_kun-plnmg = l_mng01.
    it_kun-actyp = 'K'.       "Account type : K (Vendor)
    it_kun-dsart = c_mftyp.   "planning type : MF

    COLLECT it_kun.

  ENDLOOP.

ENDFORM.                    " GET_LIFNR_MATNR_NEW
