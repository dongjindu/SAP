* Notes: 498580
* if c/f, then reset to delete payment data
*
REPORT rffmdlpf_old.

INCLUDE: ififmcon_value_types,
         ififmcon_bool,
         ififmcon_appl,
         lfmauequ,
         rfeposc1.

TYPE-POOLS:
  fmfi,
  slis.

TABLES:
  fmifiit.

DATA:
  BEGIN OF g_t_augbl OCCURS 0,
    kunnr   LIKE bseg-kunnr,
    lifnr   LIKE bseg-lifnr,
    augdt   LIKE bseg-augdt,
    augbl   LIKE bseg-augbl,
    gjahr   LIKE bseg-gjahr,
    bukrs   LIKE bseg-bukrs,
    belnr   LIKE bseg-belnr,
  END   OF g_t_augbl,

  BEGIN OF g_t_ledger OCCURS 5,
    rldnr LIKE fmifiit-rldnr,
    payment_fdate,
  END   OF g_t_ledger,

  g_t_fmifiit_del     LIKE fmifiit         OCCURS 0 WITH HEADER LINE,
  g_t_dummy           LIKE fmifiit         OCCURS 0 WITH HEADER LINE,
  g_t_muster          LIKE fmifiit         OCCURS 0 WITH HEADER LINE,
  g_t_90_30           LIKE bkpf            OCCURS 0 WITH HEADER LINE,
  g_t_all             LIKE bkpf            OCCURS 0 WITH HEADER LINE,
  g_t_bkpf            LIKE bkpf            OCCURS 0 WITH HEADER LINE,
  g_t_bkpf_cc         LIKE bkpf            OCCURS 0 WITH HEADER LINE,
  g_t_postab          LIKE postab          OCCURS 0 WITH HEADER LINE,
  g_f_fm01d           LIKE fm01d,
  g_fikrs             LIKE fmifiit-fikrs,

  g_cnt_lines         TYPE i.

SELECTION-SCREEN BEGIN OF BLOCK selektion WITH FRAME TITLE text-020.
PARAMETER:
  p_bukrs LIKE fmifiit-bukrs OBLIGATORY,
  p_gjahr LIKE fmifiit-kngjahr OBLIGATORY.

SELECT-OPTIONS
  s_belnr FOR  fmifiit-knbelnr.
SELECTION-SCREEN END   OF BLOCK selektion.

SELECTION-SCREEN SKIP 1.

SELECTION-SCREEN BEGIN OF BLOCK steuerung WITH FRAME TITLE text-030.
PARAMETER:
  p_test LIKE lko74-testlauf  DEFAULT 'X'.
SELECTION-SCREEN END   OF BLOCK steuerung.

START-OF-SELECTION.

  CALL FUNCTION 'FMFK_GET_FIKRS_FROM_BUKRS'
    EXPORTING
      i_bukrs = p_bukrs
    IMPORTING
      e_fikrs = g_fikrs.

  if sy-subrc <> 0.
  endif.

*  select single fikrs into g_fikrs
*    from  t001
*    where bukrs = p_bukrs.

  CALL FUNCTION 'FMAU_AUTHORITY_FIFM'
       EXPORTING
            i_actvt       = fmau_ac_ini
            i_auth_object = 'F_FICB_FKR'
            i_fikrs       = g_fikrs
            i_msgty       = 'E'.

  if sy-subrc <> 0.
  endif.

*----- Die Daten zum Finanzkreis lesen
  CALL FUNCTION 'FMFK_FIKRS_READ'
    EXPORTING
      ip_fikrs            = g_fikrs
      ip_application_data = con_on
      ip_applc            = applc_ca
    IMPORTING
      f_fm01d             = g_f_fm01d.

  CHECK g_f_fm01d-fm_paym_s200_rc  = con_off AND
        g_f_fm01d-fm_paym_s200_nrc = con_on.

*--- Selektion
  SELECT * FROM fmifiit
    INTO TABLE g_t_fmifiit_del
   WHERE knbelnr IN s_belnr
     AND kngjahr  = p_gjahr
     AND   bukrs  = p_bukrs
     AND   vrgng <> fmfi_con_orgvg_dpclearing
     AND   vrgng <> fmfi_con_orgvg_dpc_transfer.

*---- Musterbeleg holen
  LOOP AT g_t_fmifiit_del.
    g_t_ledger-rldnr = g_t_fmifiit_del-rldnr.
    COLLECT g_t_ledger.
  ENDLOOP.

  PERFORM payment_muster(rffms200_old) TABLES g_t_ledger
                                              g_t_fmifiit_del
                                              g_t_muster
                                        USING 'I'.

*---- Nur Satze der neuen Fortschreibung uberarbeiten
  DELETE g_t_fmifiit_del WHERE stunr+15(1) = 'T'
                            OR ( btart <> fmfi_con_btart_reduction AND
                                 btart <> fmfi_con_btart_payed ).

*--- Nur die Satze des aktuellen Geschaftsjahres bearbeiten
  LOOP AT g_t_muster.
    LOOP AT g_t_fmifiit_del WHERE knbelnr = g_t_muster-knbelnr
                              AND kngjahr = g_t_muster-kngjahr
                              AND   bukrs = g_t_muster-bukrs
                              AND fmbuzei = g_t_muster-fmbuzei.
      IF g_t_muster-gjahr <> g_t_fmifiit_del-gjahr.
        DELETE g_t_fmifiit_del.
      ENDIF.
    ENDLOOP.
  ENDLOOP.

*---- Testbeleg holen ( gesplttet in CC und nicht CC )
  PERFORM split_cc_not_cc TABLES g_t_fmifiit_del
                                 g_t_all
                                 g_t_bkpf
                                 g_t_bkpf_cc.

*---- 90-30 Buchungen rauswerfen
  PERFORM delete_90_30_not_cc TABLES g_t_fmifiit_del
                                     g_t_90_30
                                     g_t_bkpf.

  PERFORM delete_90_30_cc TABLES g_t_fmifiit_del
                                 g_t_90_30
                                 g_t_bkpf_cc.

*--- alle restlichen Belege von DB loschen
  DESCRIBE TABLE g_t_fmifiit_del LINES sy-tfill.
  g_cnt_lines = sy-tfill.

  DELETE fmifiit FROM TABLE g_t_fmifiit_del.

*----- Ausgleiche holen
  LOOP AT g_t_all.
    CALL FUNCTION 'GET_CLEARED_ITEMS'
      EXPORTING
        i_belnr = g_t_all-belnr
        i_bukrs = g_t_all-bukrs
        i_gjahr = g_t_all-gjahr
        i_bvorg = g_t_all-bvorg
      TABLES
        t_items = g_t_postab
      EXCEPTIONS
        OTHERS  = 1.
    LOOP AT g_t_postab.
      g_t_augbl-belnr = g_t_postab-belnr.
      g_t_augbl-gjahr = g_t_postab-gjahr.
      g_t_augbl-bukrs = g_t_postab-bukrs.
      COLLECT g_t_augbl.
    ENDLOOP.
    REFRESH g_t_postab.
  ENDLOOP.

*----- Payflgs im alten Geschaftsjahr offnen
  SORT g_t_90_30 BY bukrs belnr gjahr.
  LOOP AT g_t_augbl.
    READ TABLE g_t_90_30
      WITH KEY bukrs = g_t_augbl-bukrs
               belnr = g_t_augbl-belnr
               gjahr = g_t_augbl-gjahr.
    IF sy-subrc <> 0.
      UPDATE fmifiit
         SET payflg = 'Y'
       WHERE vobelnr = g_t_augbl-belnr
         AND vogjahr = g_t_augbl-gjahr
         AND vobukrs = g_t_augbl-bukrs
         AND wrttp   = wrttp6
         AND stunr LIKE '%P'.
      IF sy-subrc <> 0.
        UPDATE fmifiit
           SET payflg = 'Y'
         WHERE vobelnr = g_t_augbl-belnr
           AND vogjahr = g_t_augbl-gjahr
           AND vobukrs = g_t_augbl-bukrs
           AND wrttp   = wrttp6.
      ENDIF.
    ELSE.
      UPDATE fmifiit
         SET payflg = 'Y'
       WHERE vobelnr = g_t_augbl-belnr
         AND vogjahr = g_t_augbl-gjahr
         AND vobukrs = g_t_augbl-bukrs
         AND ( knbelnr <> g_t_augbl-belnr
            OR kngjahr <> g_t_augbl-gjahr
            OR   bukrs <> g_t_augbl-bukrs )
         AND wrttp   = wrttp6
         AND stunr LIKE '%P'.
      IF sy-subrc <> 0.
        UPDATE fmifiit
           SET payflg = 'Y'
         WHERE vobelnr = g_t_augbl-belnr
           AND vogjahr = g_t_augbl-gjahr
           AND vobukrs = g_t_augbl-bukrs
         AND ( knbelnr <> g_t_augbl-belnr
            OR kngjahr <> g_t_augbl-gjahr
            OR   bukrs <> g_t_augbl-bukrs )
           AND wrttp   = wrttp6.
      ENDIF.
    ENDIF.
  ENDLOOP.

*--- Summensatze anpassen
*--- Vorzeichen wechseln
  CALL FUNCTION 'FM_CONVERT_SIGN_FI'
    TABLES
      t_fmifiit = g_t_fmifiit_del.

*---Summensatztabelle anpassen
  CALL FUNCTION 'FM_TOTALS_UPDATE_FI'
    TABLES
      t_fmifiit     = g_t_dummy
      t_fmifiit_del = g_t_fmifiit_del.

  IF p_test IS INITIAL.
    COMMIT WORK.
  ELSE.
    ROLLBACK WORK.
  ENDIF.

*----- Ausgabe
  PERFORM display_list TABLES g_t_fmifiit_del
                       USING  text-050.

*&---------------------------------------------------------------------*
*&      Form  split_cc_not_cc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_G_T_FMIFIIT_DEL  text
*      -->P_G_T_BKPF  text
*      -->P_G_T_BKPF_CC  text
*----------------------------------------------------------------------*
FORM split_cc_not_cc TABLES u_t_fmifiit_del STRUCTURE fmifiit
                            c_t_all         STRUCTURE bkpf
                            c_t_bkpf        STRUCTURE bkpf
                            c_t_bkpf_cc     STRUCTURE bkpf.
  DATA:
    l_t_bvor LIKE bvor OCCURS 0 WITH HEADER LINE.

*---- Belege auf CC testen
  LOOP AT u_t_fmifiit_del.
    c_t_bkpf-bukrs = g_t_fmifiit_del-vobukrs.
    c_t_bkpf-belnr = g_t_fmifiit_del-vobelnr.
    c_t_bkpf-gjahr = g_t_fmifiit_del-vogjahr.
    COLLECT c_t_bkpf.
  ENDLOOP.

  CHECK NOT c_t_bkpf[] IS INITIAL.

  APPEND LINES OF c_t_bkpf TO c_t_all.

  SELECT bukrs belnr gjahr bvorg FROM bkpf
    INTO CORRESPONDING FIELDS OF TABLE c_t_bkpf_cc
     FOR ALL ENTRIES IN c_t_bkpf
   WHERE bukrs = c_t_bkpf-bukrs
     AND belnr = c_t_bkpf-belnr
     AND gjahr = c_t_bkpf-gjahr
     AND bvorg <> space.
  IF NOT c_t_bkpf_cc[] IS INITIAL.

*----- Buchungskreisubergreifender Teil einlesen
    SELECT * FROM bvor
      INTO   TABLE l_t_bvor
       FOR   ALL ENTRIES IN c_t_bkpf_cc
     WHERE   bvorg = c_t_bkpf_cc-bvorg
       AND   xarch = space.

    SELECT * FROM bkpf
      INTO   TABLE c_t_bkpf_cc
       FOR   ALL ENTRIES IN l_t_bvor
     WHERE   bukrs = l_t_bvor-bukrs
       AND   belnr = l_t_bvor-belnr
       AND   gjahr = l_t_bvor-gjahr.

    SORT c_t_bkpf_cc BY bukrs belnr gjahr.
  ENDIF.

*----- Fur nicht buchungskreisubergreifende Buchungen mussen nur
*----- Belege mit identischem Zahlungs und Rechnungsbezug untersucht
*----- werden
  REFRESH: c_t_bkpf.

  LOOP AT u_t_fmifiit_del.
    IF u_t_fmifiit_del-bukrs   = u_t_fmifiit_del-vobukrs AND
       u_t_fmifiit_del-knbelnr = u_t_fmifiit_del-vobelnr AND
       u_t_fmifiit_del-kngjahr = u_t_fmifiit_del-vogjahr.

      READ TABLE c_t_bkpf_cc
        WITH KEY bukrs = u_t_fmifiit_del-vobukrs
                 belnr = u_t_fmifiit_del-vobelnr
                 gjahr = u_t_fmifiit_del-vogjahr.
      CHECK sy-subrc <> 0.

      c_t_bkpf-bukrs = u_t_fmifiit_del-vobukrs.
      c_t_bkpf-belnr = u_t_fmifiit_del-vobelnr.
      c_t_bkpf-gjahr = u_t_fmifiit_del-vogjahr.
      COLLECT c_t_bkpf.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " split_cc_not_cc
*&---------------------------------------------------------------------*
*&      Form  delete_90_30_not_cc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_G_T_BKPF  text
*----------------------------------------------------------------------*
FORM delete_90_30_not_cc TABLES c_t_fmifiit_del STRUCTURE fmifiit
                                c_t_90_30       STRUCTURE bkpf
                                c_t_bkpf        STRUCTURE bkpf.
  DATA:
    l_t_bseg  LIKE bseg   OCCURS 0 WITH HEADER LINE,

    l_f_fmfpo LIKE fmfpo,

    l_fikrs   LIKE fmifiit-fikrs,
    l_bukrs   LIKE fmifiit-bukrs.

  CHECK NOT c_t_bkpf[] IS INITIAL.

  SELECT bukrs belnr gjahr fipos lifnr kunnr xopvw
    FROM bseg
    INTO CORRESPONDING FIELDS OF TABLE l_t_bseg
     FOR ALL ENTRIES IN c_t_bkpf
   WHERE bukrs = c_t_bkpf-bukrs
     AND belnr = c_t_bkpf-belnr
     AND gjahr = c_t_bkpf-gjahr
     AND ktosl <> 'BUV'
     AND xref1 <> fmfi_con_euro_fi.

  LOOP AT l_t_bseg.

*----- Finnazkreis nachlesen
    IF l_bukrs <> l_t_bseg-bukrs.
      CALL FUNCTION 'FMFK_GET_FIKRS_FROM_BUKRS'
        EXPORTING
          i_bukrs = l_t_bseg-bukrs
        IMPORTING
          e_fikrs = l_fikrs.
      l_bukrs = l_t_bseg-bukrs.
    ENDIF.

*---- Finanzposition nachlesen
    CALL FUNCTION 'FMFPO_READ_QUICK'
      EXPORTING
        ip_flg_buffer_all = con_on
        ip_fikrs          = l_fikrs
        ip_gjahr          = l_t_bseg-gjahr
        ip_fipos          = l_t_bseg-fipos
      IMPORTING
        f_fmfpo           = l_f_fmfpo.

*---- Belege trennen
    IF l_f_fmfpo-fivor > '60'.
      DELETE c_t_fmifiit_del WHERE vobukrs = l_t_bseg-bukrs
                               AND vobelnr = l_t_bseg-belnr
                               AND vogjahr = l_t_bseg-gjahr
                               AND bukrs   = l_t_bseg-bukrs
                               AND knbelnr = l_t_bseg-belnr
                               AND kngjahr = l_t_bseg-gjahr.
      c_t_90_30-bukrs = l_t_bseg-bukrs.
      c_t_90_30-gjahr = l_t_bseg-gjahr.
      c_t_90_30-belnr = l_t_bseg-belnr.
      COLLECT c_t_90_30.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " delete_90_30_not_cc
*&---------------------------------------------------------------------*
*&      Form  delete_90_30_cc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_G_T_FMIFIIT_DEL  text
*      -->P_G_T_BKPF_CC  text
*----------------------------------------------------------------------*
FORM delete_90_30_cc TABLES c_t_fmifiit_del STRUCTURE fmifiit
                            c_t_90_30       STRUCTURE bkpf
                            c_t_bkpf        STRUCTURE bkpf.
  DATA:
    BEGIN OF l_t_bseg OCCURS 0,
      bvorg LIKE bkpf-bvorg,
      bukrs LIKE bseg-bukrs,
      belnr LIKE bseg-belnr,
      gjahr LIKE bseg-gjahr,
      fipos LIKE bseg-fipos,
      kunnr LIKE bseg-kunnr,
      lifnr LIKE bseg-lifnr,
      xopvw LIKE bseg-xopvw,
    END   OF l_t_bseg,

    l_t_payments LIKE l_t_bseg OCCURS 0 WITH HEADER LINE,

    l_f_fmfpo  LIKE fmfpo,
    l_lo_fivor LIKE fmfpo-fivor VALUE '90',
    l_hi_fivor LIKE fmfpo-fivor VALUE '00',

    l_fikrs   LIKE fmifiit-fikrs,
    l_bukrs   LIKE fmifiit-bukrs.

  CHECK NOT c_t_bkpf[] IS INITIAL.

  SELECT bukrs belnr gjahr fipos kunnr lifnr xopvw
    FROM bseg
    INTO CORRESPONDING FIELDS OF TABLE l_t_bseg
     FOR ALL ENTRIES IN c_t_bkpf
   WHERE bukrs = c_t_bkpf-bukrs
     AND belnr = c_t_bkpf-belnr
     AND gjahr = c_t_bkpf-gjahr
     AND ktosl <> 'BUV'
     AND xref1 <> fmfi_con_euro_fi.

*----- Gemeinsamer key einschreiben
  SORT l_t_bseg BY bukrs belnr gjahr.
  LOOP AT c_t_bkpf.
    READ TABLE l_t_bseg WITH KEY bukrs = c_t_bkpf-bukrs
                                 belnr = c_t_bkpf-belnr
                                 gjahr = c_t_bkpf-gjahr
    BINARY SEARCH.

    CHECK sy-subrc = 0.

    LOOP AT l_t_bseg FROM sy-tabix.
      IF l_t_bseg-bukrs <> c_t_bkpf-bukrs OR
         l_t_bseg-belnr <> c_t_bkpf-belnr OR
         l_t_bseg-gjahr <> c_t_bkpf-gjahr.
        EXIT.
      ENDIF.

      l_t_bseg-bvorg = c_t_bkpf-bvorg.
      MODIFY l_t_bseg TRANSPORTING bvorg.
    ENDLOOP.
  ENDLOOP.

  SORT l_t_bseg BY bvorg.

  LOOP AT l_t_bseg.

*----- Finnazkreis nachlesen
    IF l_bukrs <> l_t_bseg-bukrs.
      CALL FUNCTION 'FMFK_GET_FIKRS_FROM_BUKRS'
        EXPORTING
          i_bukrs = l_t_bseg-bukrs
        IMPORTING
          e_fikrs = l_fikrs.
      l_bukrs = l_t_bseg-bukrs.
    ENDIF.

*---- Finanzposition nachlesen
    CALL FUNCTION 'FMFPO_READ_QUICK'
      EXPORTING
        ip_flg_buffer_all = con_on
        ip_fikrs          = l_fikrs
        ip_gjahr          = l_t_bseg-gjahr
        ip_fipos          = l_t_bseg-fipos
      IMPORTING
        f_fmfpo           = l_f_fmfpo.

*---- Zahlungen merken
    IF l_f_fmfpo-fivor = '90'.
      l_t_payments-belnr = l_t_bseg-belnr.
      l_t_payments-gjahr = l_t_bseg-gjahr.
      l_t_payments-bukrs = l_t_bseg-bukrs.
      COLLECT l_t_payments.
    ENDIF.

    IF l_f_fmfpo-fivor < l_lo_fivor.
      l_lo_fivor = l_f_fmfpo-fivor.
    ENDIF.

    IF l_f_fmfpo-fivor > l_hi_fivor.
      l_hi_fivor = l_f_fmfpo-fivor.
    ENDIF.

    AT END OF bvorg.
      IF l_lo_fivor = '30' AND l_hi_fivor = '90'.
        LOOP AT l_t_payments.
          DELETE c_t_fmifiit_del WHERE vobukrs = l_t_payments-bukrs
                                   AND vobelnr = l_t_payments-belnr
                                   AND vogjahr = l_t_payments-gjahr.
          c_t_90_30-bukrs = l_t_payments-bukrs.
          c_t_90_30-gjahr = l_t_payments-gjahr.
          c_t_90_30-belnr = l_t_payments-belnr.
          COLLECT c_t_90_30.
        ENDLOOP.
      ENDIF.
      REFRESH: l_t_payments.

      l_lo_fivor = '90'.
      l_hi_fivor = '00'.
    ENDAT.
  ENDLOOP.
ENDFORM.                    " delete_90_30_cc
*---------------------------------------------------------------------*
*  FORM display_list
*---------------------------------------------------------------------*
*
*---------------------------------------------------------------------*
*  -->  C_T_OUTLIST
*  -->  U_TEXT
*---------------------------------------------------------------------*
FORM display_list TABLES c_t_outlist STRUCTURE fmifiit
                  USING  u_text      TYPE c.
  DATA:
    l_t_fieldcat TYPE slis_t_fieldcat_alv,
    l_f_layout   TYPE slis_layout_alv,
    l_text       TYPE lvc_title.

*----- Uberschrift zusammenbauen
  MOVE u_text TO l_text.

*----- Pfeil ausgeben
  l_f_layout-box_fieldname = 'INVISIBLE'.
  l_f_layout-info_fieldname = 'LCOL'.

*----- pass it to re-use function
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program          = 'RFFMDLPF_OLD'
      i_callback_html_top_of_page = 'HTML_TOP_OF_PAGE'
      i_structure_name            = 'FMIFIIT'
      i_grid_title                = l_text
      is_layout                   = l_f_layout
      it_fieldcat                 = l_t_fieldcat
    TABLES
      t_outtab                    = c_t_outlist.
ENDFORM.                    " display_list
*&---------------------------------------------------------------------*
*&      Form  html_top_of_page
*&---------------------------------------------------------------------*
*----- this routine is called back from function                       *
*----- REUSE_ALV_GRID_DISPLAY and creates a commentary block           *
*----- on top of the ALV list                                          *
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM html_top_of_page USING r_top  TYPE REF TO cl_dd_document.
  DATA:
    l_text   TYPE sdydo_text_element,
    s_table  TYPE REF TO cl_dd_table_element,
    col_key  TYPE REF TO cl_dd_area,
    col_info TYPE REF TO cl_dd_area,
    col_keyx TYPE REF TO cl_dd_area,
    col_infox TYPE REF TO cl_dd_area,
    l_date(10) TYPE c,
    l_time(10) TYPE c.

  WRITE sy-datum TO l_date.
  WRITE sy-uzeit TO l_time.

  CALL METHOD r_top->add_table
    EXPORTING
      no_of_columns = 3
      with_heading  = space
      border        = '0'
    IMPORTING
      table         = s_table.

  CALL METHOD s_table->add_column
    IMPORTING
      column = col_key.
  CALL METHOD s_table->add_column
    IMPORTING
      column = col_info.
  CALL METHOD s_table->add_column
    IMPORTING
      column = col_keyx.
  CALL METHOD s_table->add_column
    IMPORTING
      column = col_infox.

  l_text = 'Datum'(060).
  CALL METHOD col_keyx->add_text
    EXPORTING
      text         = l_text
      sap_emphasis = 'STRONG'.
  CALL METHOD col_infox->add_gap
    EXPORTING
      width = 6.
  l_text = l_date.
  CALL METHOD col_infox->add_text
    EXPORTING
      text      = l_text
      sap_style = 'KEY'.

  CALL METHOD s_table->new_row.

  l_text = 'Zeit'(070).
  CALL METHOD col_keyx->add_text
    EXPORTING
      text         = l_text
      sap_emphasis = 'STRONG'.
  CALL METHOD col_infox->add_gap
    EXPORTING
      width = 6.
  l_text = l_time.
  CALL METHOD col_infox->add_text
    EXPORTING
      text      = l_text
      sap_style = 'KEY'.
ENDFORM.                    " html_top_of_page
