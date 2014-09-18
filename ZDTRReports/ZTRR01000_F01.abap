*&---------------------------------------------------------------------*
*&  Include           ZTRR01000_F01
*&---------------------------------------------------------------------*

*---------------------------------------------------------------------*
*      Form  select_data
*---------------------------------------------------------------------*
FORM select_data.

  DATA: l_budat LIKE sy-datum,
        l_gjahr like bsis-gjahr.

  clear: r_hkont, r_hkont[], l_gjahr.
  l_gjahr = p_budat(4).

*// Limiting the scope of the data is imported.
  SELECT * INTO TABLE it_skb1 FROM skb1
   WHERE bukrs = p_bukrs
     AND ( fdlev LIKE 'B%' OR fdlev LIKE 'C%' )  "Planning Level
     AND saknr IN s_saknr.


  LOOP AT it_skb1.
    make_ranges:  it_skb1-saknr  r_hkont.
  ENDLOOP.


  SELECT * INTO TABLE it_flext
    FROM faglflext
   WHERE rbukrs  =  p_bukrs
     and ryear  =  l_gjahr
     AND racct  IN r_hkont.

  l_budat = p_budat.
  l_budat+6(2) = '01'.


  SELECT * INTO TABLE it_flexa
    FROM faglflexa
   WHERE rbukrs = p_bukrs
     AND budat BETWEEN l_budat AND p_budat
     AND rldnr = 'K0'
     AND racct IN r_hkont.



ENDFORM.                    " select_data
*---------------------------------------------------------------------*
*      FORM MAKE_IT_LIST
*---------------------------------------------------------------------*
FORM make_it_list.
  FIELD-SYMBOLS: <fs>.

  DATA: l_text(14).
  DATA: l_mon(2).
  DATA: l_month(2) TYPE n.

  l_mon = p_budat+4(2) - 1.
* Prior Month
  LOOP AT it_flext.
    it_list-racct = it_flext-racct.

    ADD it_flext-hslvt TO it_list-amt01.

    CLEAR l_month.
    DO l_mon TIMES.
      l_month = l_month + 1.

      CONCATENATE 'IT_FLEXT-HSL' l_month INTO l_text.

      ASSIGN (l_text) TO <fs>.

      ADD <fs> TO it_list-amt01.
    ENDDO.

    COLLECT it_list. CLEAR it_list.
  ENDLOOP.

* First Day of current month ~ Current
  LOOP AT it_flexa.
    it_list-racct = it_flexa-racct.

    IF it_flexa-budat < p_budat.
      it_list-amt01 = it_flexa-hsl.
    ELSE.

      CASE it_flexa-racct+9(1).
        WHEN '1'. it_list-amt05 = it_flexa-hsl.
        WHEN '2' OR '3'.
          IF it_flexa-drcrk = 'S'.
            CLEAR FAGLFLEXA.
            SELECT SINGLE * FROM faglflexa
             WHERE ryear = it_flexa-ryear
               AND docnr = it_flexa-docnr
               AND drcrk = 'H'.

            IF faglflexa-racct+0(5) = '00105'.
              it_list-amt05 = it_flexa-hsl.
            ELSE.
              it_list-amt03 = it_flexa-hsl.
            ENDIF.
          ELSE.
            CLEAR FAGLFLEXA.
            SELECT SINGLE * FROM faglflexa
              WHERE ryear = it_flexa-ryear
                AND docnr = it_flexa-docnr
                AND drcrk = 'S'.

            IF faglflexa-racct+0(5) = '00105'.
              it_list-amt03 = it_flexa-hsl.
            ELSE.
              it_list-amt05 = it_flexa-hsl.
            ENDIF.
*           it_list-amt05 = it_flexa-hsl.
          ENDIF.
        WHEN '6'. it_list-amt02 = it_flexa-hsl.

        WHEN OTHERS.
          IF it_flexa-drcrk = 'S'.
            it_list-amt02 = it_flexa-hsl.
          ELSE.
            it_list-amt04 = it_flexa-hsl.
          ENDIF.
      ENDCASE.
    ENDIF.

    COLLECT it_list. CLEAR it_list.
  ENDLOOP.


  LOOP AT it_list.
    SELECT SINGLE txt50 INTO it_list-txt50
      FROM skat
     WHERE spras = sy-langu
       AND ktopl = p_bukrs
       AND saknr = it_list-racct.

    READ TABLE it_skb1 WITH KEY bukrs = p_bukrs
                                saknr = it_list-racct.
    it_list-fdlev = it_skb1-fdlev.

    CLEAR t038.    "Cash Management: Grouping Structure
    SELECT SINGLE * FROM t038
     WHERE zeilt = 'E'         "Line Type
       AND selek = it_list-fdlev.  "Selection for Cash Management
*                                   (example: for cash position)

    it_list-glied = t038-glied.

    CLEAR t038t.      "Texts for Groupings
    SELECT SINGLE * FROM t038t
     WHERE spras = sy-langu
       AND glied = it_list-glied.  "Grouping for cash posn, liq.
*                                   forecast, concentration

    it_list-textu = t038t-textu.

    IF it_list-fdlev(1) = 'C'.
      it_list-glied = 'CASH'.
      it_list-textu = 'CASH'.
    ENDIF.

    it_list-bankl = it_list-racct+2(6).

    CASE it_list-bankl.         "Bank Keys
      WHEN '105101'. it_list-banka = ' Cash'.
      WHEN '105201'. it_list-banka = 'Comerica'.
      WHEN '105204'. it_list-banka = 'J.P Morgan'.
      WHEN '105205'. it_list-banka = 'CB & T'.
      WHEN '105206'. it_list-banka = 'Servis 1st'.
      WHEN '105207'. it_list-banka = 'Captial City'.
      WHEN '105208'. it_list-banka = 'First IC'.
      WHEN '105209'. it_list-banka = 'LaGrange'.
    ENDCASE.

    it_list-amt06 = it_list-amt01 + it_list-amt02 +
                    it_list-amt03 + it_list-amt04 +
                    it_list-amt05.

    MODIFY it_list. CLEAR it_list.
  ENDLOOP.

  IF p_rb1 = 'X'.
    DATA : lt_list LIKE it_list OCCURS 0 WITH HEADER LINE.

    lt_list[] = it_list[].
    CLEAR: it_list[].

    LOOP AT lt_list.
      MOVE-CORRESPONDING lt_list TO it_list.

      CLEAR: it_list-bankl, it_list-racct, it_list-txt50, it_list-fdlev,
             it_list-glied.

      COLLECT it_list.
      CLEAR it_list.
    ENDLOOP.

  ENDIF.

ENDFORM.                    "MAKE_IT_LIST

*---------------------------------------------------------------------*
*      Form  DISPLAY_LIST
*---------------------------------------------------------------------*
FORM display_list.

  g_repid = sy-repid.

  alv_layout-zebra             = 'X'.
  alv_layout-info_fieldname    = 'COLOR'.
  alv_layout-colwidth_optimize = 'X'.

  PERFORM alv_field_catalog.
  PERFORM alv_events.
  PERFORM alv_sort.
  PERFORM alv_comment.


  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program       = g_repid
*     i_callback_pf_status_set = 'STATUS'
      i_callback_user_command  = 'USER_COMMAND'
      it_sort                  = alv_sort[]
      is_layout                = alv_layout
      it_fieldcat              = alv_fieldcat[]
      i_save                   = 'A'
      it_events                = alv_event[]
    TABLES
      t_outtab                 = it_list.

ENDFORM.                    " write_data
*---------------------------------------------------------------------*
*      Form  build_field_catalog
*---------------------------------------------------------------------*
FORM alv_field_catalog.

  DATA: ls_fieldcat  TYPE  slis_fieldcat_alv.
  DATA: lt_fieldcat  TYPE  slis_t_fieldcat_alv.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_program_name     = g_repid
      i_internal_tabname = 'IT_LIST'
      i_inclname         = g_repid
    CHANGING
      ct_fieldcat        = alv_fieldcat[].

*  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
*    EXPORTING
*      i_program_name     = g_repid
*      i_internal_tabname = 'IT_BSEG'
*      i_inclname         = g_repid
*    CHANGING
*      ct_fieldcat        = lt_fieldcat[].
*
*  LOOP AT lt_fieldcat INTO ls_fieldcat.
*    CHECK ls_fieldcat-fieldname <> 'GJAHR' AND
*          ls_fieldcat-fieldname <> 'BELNR'.
*
*
*    INSERT ls_fieldcat INTO TABLE alv_fieldcat.
*  ENDLOOP.

*  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
*    EXPORTING
*      i_program_name     = g_repid
*      i_internal_tabname = 'IT_LIST'
*      i_inclname         = g_repid
*    CHANGING
*      ct_fieldcat        = alv_fieldcat[].


  LOOP AT alv_fieldcat INTO ls_fieldcat.

    ls_fieldcat-col_pos = sy-tabix.

    CASE ls_fieldcat-fieldname.
      WHEN 'AMT01'.
        ls_fieldcat-reptext_ddic = 'Balance(D-1)'.
      WHEN 'AMT02'.
        ls_fieldcat-reptext_ddic = 'Incoming(D)'.
      WHEN 'AMT03'.
        ls_fieldcat-reptext_ddic = 'Incoming Clearing(D)'.
*        ls_fieldcat-emphasize = 'C500'.
      WHEN 'AMT04'.
        ls_fieldcat-reptext_ddic = 'Outgoing(D)'.
*        ls_fieldcat-emphasize = 'C510'.

      WHEN 'AMT05'.
        ls_fieldcat-reptext_ddic = 'Outgoing Clearing(D)'.
      WHEN 'AMT06'.
        ls_fieldcat-reptext_ddic = 'Balance(D)'.
    ENDCASE.

    IF ls_fieldcat-datatype = 'CURR'.
      ls_fieldcat-do_sum = 'X'.
    ENDIF.

    ls_fieldcat-seltext_s  = ls_fieldcat-reptext_ddic.
    ls_fieldcat-seltext_m  = ls_fieldcat-reptext_ddic.
    ls_fieldcat-seltext_l  = ls_fieldcat-reptext_ddic.

    MODIFY alv_fieldcat FROM ls_fieldcat.
  ENDLOOP.

ENDFORM.                    " build_field_catalog
*---------------------------------------------------------------------*
*      Form  build_events
*---------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
FORM alv_events.

  DATA ll_events TYPE slis_alv_event.

  CLEAR alv_event[].

  ll_events-name   =  'TOP_OF_PAGE'.
  ll_events-form   =  'TOP_OF_PAGE'.
  APPEND ll_events TO alv_event.

ENDFORM.                    " build_events
*---------------------------------------------------------------------*
*      Form  build_comment
*---------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
FORM alv_comment.

  DATA: ll_line     TYPE slis_listheader.
  DATA: l_amount(15).

  CLEAR top_of_page[].


  CLEAR ll_line.
  ll_line-typ     =  'H'.
  ll_line-info    =  'Cash Position'.

  APPEND ll_line  TO top_of_page.

  CLEAR ll_line.
  ll_line-typ     =  'S'.
  CONCATENATE 'Date : ' p_budat INTO ll_line-info
  SEPARATED BY space.

  APPEND ll_line  TO top_of_page.
*
*
*  CLEAR ll_line.
*  ll_line-typ     =  'S'.
*  CONCATENATE 'End Time : ' etime INTO ll_line-info
*  SEPARATED BY space.
*
*  APPEND ll_line  TO top_of_page.
*
*  dtime = etime - stime.
*
*  CLEAR ll_line.
*  ll_line-typ     =  'S'.
*  CONCATENATE 'Duration Time : ' dtime INTO ll_line-info
*  SEPARATED BY space.
*
*  APPEND ll_line  TO top_of_page.

ENDFORM.                    " build_comment
*---------------------------------------------------------------------*
*      Form  TOP_OF_PAGE
*---------------------------------------------------------------------*

FORM top_of_page.

  IF p_rb1 = 'X'.
    SKIP 3.

    WRITE:  (135) 'Cash Position' CENTERED.
    WRITE:  (135) '=============' CENTERED.

    SKIP 2.

    WRITE:/(01) '',
           (06) 'Date :',
           (10) p_budat.

    ULINE.

    FORMAT COLOR  COL_HEADING.

    WRITE :/(01) '|',
            (25) '',
            (01) '|',
            (17) '' NO-GAP,
            (01) '|',
            (31) 'Incoming' CENTERED,
            (01) '|',
            (31) 'Outgoing' CENTERED,
            (01) '|',
            (17) '' NO-GAP,
             sy-vline.

    WRITE :/(01) '|',
            (25) 'Bank' CENTERED,
            (01) '|',
            (17) 'Balance' CENTERED NO-GAP,
            (01) '|',
            (31) 'Incoming' CENTERED,
            (01) '|',
            (31) 'Outgoing' CENTERED,
            (01) '|',
            (17) 'Balance' CENTERED NO-GAP,
             sy-vline.
    ULINE AT 49(68).

    WRITE :/(01) '|',
            (25) '',
            (01) '|',
            (17) '' NO-GAP,
            (01) '|',
            (15) 'In' CENTERED NO-GAP,
            (01) '|',
            (15) 'Clearing' CENTERED NO-GAP,
            (01) '|',
            (15) 'Out' CENTERED NO-GAP,
            (01) '|',
            (15) 'Clearing' CENTERED NO-GAP,
            (01) '|',
            (17) '' NO-GAP,
             sy-vline.
    ULINE.

  ELSE.
    CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
      EXPORTING
        i_logo             = 'ENJOYSAP_LOGO'
        it_list_commentary = top_of_page.
  ENDIF.
ENDFORM.                    " TOP_OF_PAGE
*---------------------------------------------------------------------*
*      Form  user_command
*---------------------------------------------------------------------*
FORM user_command   USING       r_ucomm     LIKE  sy-ucomm
                                rs_selfield TYPE  slis_selfield.

  CASE r_ucomm.

    WHEN '&IC1'.
      READ TABLE it_list INDEX rs_selfield-tabindex.

      CASE rs_selfield-sel_tab_field.      "
*        WHEN 'IT_LIST-BELNR'.

*          SET PARAMETER ID 'ACC' FIELD it_list-racct.
*          SET PARAMETER ID 'BUK' FIELD p_bukrs.
*          SET PARAMETER ID 'GJR' FIELD p_budat(4).
*
*          CALL TRANSACTION 'FAGLB03' AND SKIP FIRST SCREEN.

          SUBMIT rfitemgl
*          WITH FREE SELECTIONS l_texpr                      "#EC *)
            WITH sd_saknr-low = it_list-racct
            WITH sd_bukrs-low = p_bukrs
            WITH x_opsel  = space
            WITH x_clsel  = space              "ausgeglichene Posten
            WITH x_aisel  = 'X'              "alle Posten
            WITH so_budat-low = p_budat
            WITH pa_stida = '99991231'
            WITH x_norm   = 'X'                "normale Posten
            WITH x_merk   = 'X'              "Merkposten
            WITH x_park   = 'X'                "vorerfasste Posten
*            WITH pa_vari  = l_vari_alv
            AND RETURN.

      ENDCASE.
  ENDCASE.

ENDFORM.                    " user_command
*---------------------------------------------------------------------*
*      Form  status
*---------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
FORM status   USING extab TYPE slis_t_extab.
*SET PF-STATUS 'STANDARD'.  " EXCLUDING tab.
ENDFORM.                    " status
*---------------------------------------------------------------------*
*      Form  alv_sort
*---------------------------------------------------------------------*
FORM alv_sort.
  DATA ls_sort TYPE slis_t_sortinfo_alv WITH HEADER LINE.

  CLEAR alv_sort[].

  ls_sort-spos      = 1.

  ls_sort-fieldname = 'BANKA'.      "
  ls_sort-up        = 'X'.
  ls_sort-subtot    = 'X'.

  APPEND ls_sort TO alv_sort.


  ls_sort-spos      = 1.

  ls_sort-fieldname = 'TEXTU'.      "
  ls_sort-up        = 'X'.
  ls_sort-subtot    = 'X'.

  APPEND ls_sort TO alv_sort.


ENDFORM.                    " alv_sort
*&---------------------------------------------------------------------*
*&      Form  WRITE_LIST
*&---------------------------------------------------------------------*

FORM write_list .
  DATA : l_banka LIKE it_list-banka.

  SORT it_list BY banka textu.

  LOOP AT it_list.
    FORMAT COLOR OFF.
    AT NEW banka.
      l_banka = it_list-banka.
    ENDAT.

    WRITE : sy-vline,
            (12) l_banka,
            (01) '|',
            (10) it_list-textu,
            (01) '|',
            (17) it_list-amt01 NO-ZERO CURRENCY 'USD'
                               RIGHT-JUSTIFIED NO-GAP,
            (01) '|',
            (15) it_list-amt02 NO-GAP,
            (01) '|',
            (15) it_list-amt03 NO-GAP,
            (01) '|',
            (15) it_list-amt04 NO-GAP,
            (01) '|',
            (15) it_list-amt05 NO-GAP,
            (01) '|',
            (17) it_list-amt06 NO-GAP,
             sy-vline.

    WRITE :/ sy-vline.
    ULINE AT 16.
    CLEAR l_banka.

    AT END OF banka.
      SUM.
      WRITE :/ '|',
        (25) '',
        (01) '|'.
*      FORMAT COLOR COL_NORMAL.
      WRITE :

         (17) it_list-amt01 NO-ZERO CURRENCY 'USD'
                            RIGHT-JUSTIFIED NO-GAP,
         (01) '|',
         (15) it_list-amt02 NO-GAP,
         (01) '|',
         (15) it_list-amt03 NO-GAP,
         (01) '|',
         (15) it_list-amt04 NO-GAP,
         (01) '|',
         (15) it_list-amt05 NO-GAP,
         (01) '|',
         (17) it_list-amt06 NO-GAP,
          sy-vline.
      WRITE :/ sy-uline.
    ENDAT.

    AT LAST.
      SUM.
      FORMAT COLOR COL_TOTAL.
      WRITE :/ '|',
        (25) 'Total',
        (01) '|',
        (17) it_list-amt01 NO-ZERO CURRENCY 'USD'
                           RIGHT-JUSTIFIED NO-GAP,
        (01) '|',
        (15) it_list-amt02 NO-GAP,
        (01) '|',
        (15) it_list-amt03 NO-GAP,
        (01) '|',
        (15) it_list-amt04 NO-GAP,
        (01) '|',
        (15) it_list-amt05 NO-GAP,
        (01) '|',
        (17) it_list-amt06 NO-GAP,
         sy-vline.
      WRITE :/ sy-uline.
    ENDAT.

  ENDLOOP.

ENDFORM.                    " WRITE_LIST
*&---------------------------------------------------------------------*
*&      Form  INIT_SCREEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM INIT_SCREEN .

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


* & c????
  perform fi_wt_read_t001 using    p_bukrs
                          changing p_butxt.

ENDFORM.                    " INIT_SCREEN
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

ENDFORM.                    " FI_WT_READ_T001
