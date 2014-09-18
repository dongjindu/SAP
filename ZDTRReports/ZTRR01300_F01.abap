*&---------------------------------------------------------------------*
*&  Include           ZTRR01300_F01
*&---------------------------------------------------------------------*

*---------------------------------------------------------------------*
*      Form  select_data
*---------------------------------------------------------------------*
FORM select_data.

  DATA: BEGIN OF lt_belnr OCCURS 0,
          gjahr   LIKE  bseg-gjahr,
          belnr   LIKE  bseg-belnr,
          buzei   LIKE  bsis-buzei,
          budat   LIKE  bsis-budat,
          shkzg   LIKE  bsis-shkzg,
          dmbtr   LIKE  bsis-dmbtr,
        END OF lt_belnr.

  DATA: BEGIN OF lt_bsak OCCURS 0,
          lifnr LIKE bsak-lifnr,
          augbl LIKE bsak-augbl,
          augdt LIKE bsak-augdt,
        END OF lt_bsak.

  DATA: BEGIN OF lt_bsad OCCURS 0,
          kunnr LIKE bsad-kunnr,
          augbl LIKE bsad-augbl,
          augdt LIKE bsad-augdt,
        END OF lt_bsad.

  DATA : lt_bseg LIKE it_bseg OCCURS 0 WITH HEADER LINE.

  CLEAR: lt_belnr, lt_belnr[], it_skb1, it_skb1[], lt_bsak, lt_bsak[],
         lt_bsad, lt_bsad[], lt_bseg, lt_bseg[], it_bseg, it_bseg[],
         gv_fikrs, gv_gjahr.

*
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = '10'
      text       = 'Getting Data...'
    EXCEPTIONS
      OTHERS     = 1.

***  RANGES: lr_hkont   FOR skb1-saknr OCCURS 0.
*// G/L account master
  SELECT saknr fdgrv fdlev mitkz  INTO TABLE it_skb1
    FROM skb1
   WHERE bukrs = p_bukrs.


  SORT it_skb1 BY saknr.

*//  ==  (The need of having Planning Level)
  LOOP AT it_skb1.
    CHECK it_skb1-fdlev IS NOT INITIAL.        "Planning Level
    CHECK it_skb1-fdlev+1(1) <> '0'.           "Planning Level
    make_ranges: it_skb1-saknr  r_hkont1.
  ENDLOOP.

*// == Include ECA Loan
*// HMMA Account has not yet been determined
  IF p_eca = 'X'.
    make_ranges: '0000000000'  r_hkont1.
  ENDIF.

* 1. Get The document including g/l account having palnning level.

*// ==General Ledger: Actual Line Items
  SELECT gjahr belnr buzei budat   "shkzg dmbtr
         INTO CORRESPONDING FIELDS OF TABLE lt_belnr
    FROM bsis
   WHERE bukrs  = p_bukrs
     AND gjahr IN s_gjahr
     AND hkont IN r_hkont1
     AND budat IN s_budat
     AND belnr IN s_belnr
     AND bstat = ''.

*// G/L Accounts (Cleared Items)
  SELECT gjahr belnr buzei budat   "shkzg dmbtr
         APPENDING CORRESPONDING FIELDS OF TABLE lt_belnr
    FROM bsas
   WHERE bukrs  = p_bukrs
     AND gjahr IN s_gjahr
     AND hkont IN r_hkont1
     AND budat IN s_budat
     AND belnr IN s_belnr
     AND bstat = ''.

***  SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_belnr
***    FROM faglflexa
***   WHERE rbukrs = p_bukrs
***     AND ryear in s_gjahr
***     AND budat IN s_budat
***     AND belnr IN s_belnr
***     AND rldnr = 'K0'    "Ledger in General Ledger Accounting
***     AND racct IN r_hkont1.

  DELETE ADJACENT DUPLICATES FROM lt_belnr COMPARING gjahr belnr.

  CHECK NOT lt_belnr[] IS INITIAL.

* == 2.All item include bank account

  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_bkpf
    FROM bkpf FOR ALL ENTRIES IN lt_belnr
   WHERE bukrs =  p_bukrs        " Company Code
     AND gjahr = lt_belnr-gjahr  " Fiscal year
     AND belnr = lt_belnr-belnr.  "Document Number

  SORT it_bkpf BY bukrs gjahr belnr.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_bseg
    FROM bseg FOR ALL ENTRIES IN lt_belnr
   WHERE bukrs =  p_bukrs        " Company Code
     AND gjahr = lt_belnr-gjahr  " Fiscal year
     AND belnr = lt_belnr-belnr.   "Document Number


  LOOP AT it_bseg WHERE ( koart = 'K'
                     OR   koart = 'D' )
                    AND augbl <> ''.    "Clearing Document

    MOVE-CORRESPONDING it_bseg TO lt_bseg.
    APPEND lt_bseg.
    CLEAR lt_bseg.
    DELETE it_bseg.

  ENDLOOP.

  SORT lt_bseg BY gjahr belnr augdt augbl lifnr kunnr.

*// DUPLICATES document.No  delete.
  DELETE ADJACENT DUPLICATES FROM lt_bseg
                        COMPARING gjahr belnr augdt
                                  augbl lifnr kunnr.


  LOOP AT lt_bseg.
    MOVE-CORRESPONDING lt_bseg TO it_bseg.
    APPEND it_bseg.
    CLEAR it_bseg.
  ENDLOOP.


  SORT it_bseg BY gjahr belnr hkont.

  LOOP AT it_bseg.
    IF it_bseg-lifnr <> ''.      "Vendors
      lt_bsak-lifnr = it_bseg-lifnr.
      lt_bsak-augbl = it_bseg-augbl.
      lt_bsak-augdt = it_bseg-augdt.

      COLLECT lt_bsak.
      CLEAR lt_bsak.
    ELSEIF it_bseg-kunnr <> ''.  "Customers
      lt_bsad-kunnr = it_bseg-kunnr.
      lt_bsad-augbl = it_bseg-augbl.
      lt_bsad-augdt = it_bseg-augdt.

      COLLECT lt_bsad.
      CLEAR lt_bsad.
    ENDIF.
  ENDLOOP.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = '20'
      text       = 'Getting Data...'
    EXCEPTIONS
      OTHERS     = 1.

  IF lt_bsak[] IS NOT INITIAL.
*//                                         Vendors data
    SELECT * INTO CORRESPONDING FIELDS OF TABLE it_bsak
      FROM bsak FOR ALL ENTRIES IN lt_bsak
     WHERE bukrs =  p_bukrs        " Company Code
       AND lifnr = lt_bsak-lifnr
       AND augbl = lt_bsak-augbl   " Fiscal year
       AND augdt = lt_bsak-augdt.  "Document Number
  ENDIF.

  IF lt_bsad[] IS NOT INITIAL.
*//                                         Customers data
    SELECT * INTO CORRESPONDING FIELDS OF TABLE it_bsad
      FROM bsad FOR ALL ENTRIES IN lt_bsad
     WHERE bukrs =  p_bukrs        " Company Code
       AND kunnr = lt_bsad-kunnr
       AND augbl = lt_bsad-augbl   " Fiscal year
       AND augdt = lt_bsad-augdt.   "Document Number
  ENDIF.

  CLEAR: it_fmifiit, it_fmifiit[].

  gv_gjahr = s_gjahr-low.

* 3. For Investment (FI Line Item Table in Funds Management)
  SELECT * INTO TABLE it_fmifiit
    FROM fmifiit
   WHERE fikrs = gc_fikrs     "add item
     AND gjahr = gv_gjahr     "add item
     AND fonds <> ''
     AND bukrs = p_bukrs.     "add item


ENDFORM.                    " select_data
*---------------------------------------------------------------------*
*      FORM MAKE_IT_LIST
*---------------------------------------------------------------------*
FORM make_it_list.

  DATA: l_mod      TYPE i,
        l_line(6)  TYPE n,
        l_perc(10) TYPE n,
        l_text(50) TYPE c,
        l_tabix(6) TYPE n,
        l_continue.

  DATA : BEGIN OF lt_invest OCCURS 0,
           gjahr LIKE bseg-gjahr,
           belnr LIKE bseg-belnr,
           dmbtr LIKE bseg-dmbtr,
           wrbtr LIKE bseg-wrbtr,
         END OF lt_invest.

  DATA: ls_bseg LIKE bseg,
        ls_bkpf LIKE bkpf,
        ls_skb1 LIKE skb1.


  LOOP AT it_fmifiit.
    lt_invest-gjahr = it_fmifiit-kngjahr.
    lt_invest-belnr = it_fmifiit-knbelnr.
    lt_invest-wrbtr = it_fmifiit-trbtr.
    lt_invest-dmbtr = it_fmifiit-fkbtr.
    COLLECT lt_invest.
    CLEAR lt_invest.
  ENDLOOP.

  SORT lt_invest BY gjahr belnr.

*
  SORT it_bsak BY augdt augbl lifnr gjahr belnr buzei. " AUGGJ
  SORT it_bsad BY augdt augbl kunnr gjahr belnr buzei.

  DESCRIBE TABLE it_bseg LINES l_line.
*
  LOOP AT it_bseg.

    l_mod = sy-tabix  MOD 500.

    IF l_mod = 0.
      l_tabix = sy-tabix.

      CONCATENATE 'Data Processing......'  l_tabix '/' l_line
             INTO l_text.

      l_perc = 30 + ( l_tabix / l_line * 60 ).

      CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
        EXPORTING
          percentage = l_perc
          text       = l_text
        EXCEPTIONS
          OTHERS     = 1.

    ENDIF.

    READ TABLE it_bkpf WITH KEY bukrs = p_bukrs
                                gjahr = it_bseg-gjahr
                                belnr = it_bseg-belnr
                                BINARY SEARCH.
    MOVE-CORRESPONDING it_bkpf TO it_list.

    READ TABLE it_skb1 WITH KEY saknr = it_bseg-hkont.

*// mitkz: Account is reconciliation account
    CASE it_skb1-mitkz.
*//         A:  Assets
*//         D:  Customers
*//         K:  Vendors
*//         V:  Contract accounts receivable

      WHEN '' OR 'A'.
*//                          "Assets
        IF it_skb1-fdlev = ''.
          IF p_eca = 'X'.
            IF it_skb1-saknr = '0021002000'.   "ECA Loan
              CLEAR l_continue.
              LOOP AT it_bseg INTO ls_bseg WHERE gjahr = it_bseg-gjahr
                                             AND belnr = it_bseg-belnr
                                             AND hkont <> '0021002000'
.                                                         " ECA Loan
                READ TABLE it_skb1 INTO ls_skb1
                                   WITH KEY saknr = ls_bseg-hkont.

*//                Planning Level
                IF ls_skb1-fdlev = ''.
                  l_continue = 'X'.
                  EXIT.
                ENDIF.

              ENDLOOP.
            ENDIF.

            IF l_continue = 'X'.
              CHECK it_skb1-saknr <> '0021002000'.  "ECA Loan
            ENDIF.
          ENDIF.

          MOVE-CORRESPONDING it_bseg TO it_list.
          IF it_list-shkzg = 'S'.               "Debit
            it_list-wrbtr = it_list-wrbtr * -1.
            it_list-dmbtr = it_list-dmbtr * -1.
          ENDIF.
          it_list-augbl = it_list-belnr.
          it_list-augdt = it_list-budat.
          it_list-gjah1 = it_bseg-gjahr.
          it_list-beln1 = it_bseg-belnr.
          it_list-buze1 = it_bseg-buzei.
          it_list-buda1 = it_bkpf-budat.

          it_list-fdgrp = 'ZZZZZ'.

          IF it_list-aufnr <> ''.
            SELECT SINGLE b~fdgrv
              INTO it_list-fdgrp                  "Planning group
              FROM aufk AS a JOIN zttr0009  AS b  "Order master data
                ON a~anfaufnr = b~aufnr
             WHERE a~aufnr    =  it_list-aufnr.

          ENDIF.

          IF it_skb1-fdgrv <> ''.                  "Planning group
            it_list-fdgrp = it_skb1-fdgrv.
          ENDIF.

* Begin of TR - Posting by BP
          IF it_list-blart = 'TR'.
            CLEAR zttr0006.   "[TR-TM]Transaction Flow

            SELECT SINGLE * FROM zttr0006
             WHERE belnr = it_list-belnr
               AND gjahr = it_list-gjahr.

            IF sy-subrc = 0.
              CLEAR: zttr0004,       "[TR-TM]Transaction
                     zttr0001.       "[TR-TM] product type

              SELECT SINGLE * FROM zttr0004
               WHERE rfha = zttr0006-rfha.    "Financial Transaction

              SELECT SINGLE * FROM zttr0001
               WHERE gsart = zttr0004-gsart.  "Product Type

              IF zttr0006-sbewart = '1100' OR    "Flow Type
                 zttr0006-sbewart = '1105' OR
                 zttr0006-sbewart = '1120'.
                it_list-fdgrp = zttr0001-fdgrv1.  "Planning group
              ELSE.
                it_list-fdgrp = zttr0001-fdgrv2.  "Planning group
              ENDIF.
            ENDIF.
          ENDIF.
* End of TR
          APPEND it_list. CLEAR it_list.
        ENDIF.

      WHEN 'D'.
*//                          Customers
        IF it_bseg-augbl = ''.
          MOVE-CORRESPONDING it_bseg TO it_list.

          IF it_list-shkzg = 'S'.
            it_list-wrbtr = it_list-wrbtr * -1.
            it_list-dmbtr = it_list-dmbtr * -1.
          ENDIF.
          it_list-augbl = it_list-belnr.
          it_list-augdt = it_list-budat.

          it_list-gjah1 = it_bseg-gjahr.
          it_list-beln1 = it_bseg-belnr.
          it_list-buze1 = it_bseg-buzei.
          it_list-buda1 = it_bkpf-budat.

          SELECT SINGLE fdgrv INTO it_list-fdgrp
            FROM knb1
           WHERE bukrs = p_bukrs
             AND kunnr = it_list-kunnr.

* Begin of TR - Posting by BP
          IF it_list-blart = 'TR'.       "Document Type
            CLEAR zttr0006.
            SELECT SINGLE * FROM zttr0006
             WHERE belnr = it_list-belnr
               AND gjahr = it_list-gjahr.

            IF sy-subrc = 0.
              CLEAR: zttr0004, zttr0001.
              SELECT SINGLE * FROM zttr0004
               WHERE rfha = zttr0006-rfha.

              SELECT SINGLE * FROM zttr0001
               WHERE gsart = zttr0004-gsart.       "Product Type

              IF zttr0006-sbewart = '1100' OR      "Flow Type
                 zttr0006-sbewart = '1105' OR
                 zttr0006-sbewart = '1120'.
                it_list-fdgrp = zttr0001-fdgrv1.   "Planning group
              ELSE.
                it_list-fdgrp = zttr0001-fdgrv2.   "Planning group
              ENDIF.
            ENDIF.
          ENDIF.
* End of TR
          APPEND it_list.
          CLEAR it_list.
        ELSE.

          READ TABLE it_bsad WITH KEY augdt = it_bseg-augdt
                                      augbl = it_bseg-augbl
                                      kunnr = it_bseg-kunnr.

          LOOP AT it_bsad FROM sy-tabix.
            CHECK it_bsad-gjahr <> it_bseg-gjahr OR
                  it_bsad-belnr <> it_bseg-belnr.

            IF it_bsad-augdt <> it_bseg-augdt OR
               it_bsad-augbl <> it_bseg-augbl OR
               it_bsad-kunnr <> it_bseg-kunnr .
              EXIT.
            ENDIF.

            IF it_bsad-shkzg = 'H'.           "Credit Indicator
              it_bsad-dmbtr = it_bsad-dmbtr * -1.
              it_bsad-wrbtr = it_bsad-wrbtr * -1.
            ENDIF.

            MOVE-CORRESPONDING it_bsad TO it_list.
            it_list-gjah1 = it_bseg-gjahr.
            it_list-beln1 = it_bseg-belnr.
            it_list-buze1 = it_bseg-buzei.
            it_list-buda1 = it_bkpf-budat.

            SELECT SINGLE fdgrv INTO it_list-fdgrp FROM knb1
             WHERE bukrs = p_bukrs
               AND kunnr = it_list-kunnr.

* Begin of TR - Posting by BP
            IF it_list-blart = 'TR'.
              CLEAR zttr0006.
              SELECT SINGLE * FROM zttr0006
               WHERE belnr = it_list-belnr
                 AND gjahr = it_list-gjahr.

              IF sy-subrc = 0.
                CLEAR: zttr0004, zttr0001.

                SELECT SINGLE * FROM zttr0004
                 WHERE rfha = zttr0006-rfha.

                SELECT SINGLE * FROM zttr0001
                 WHERE gsart = zttr0004-gsart.       "Product Type

                IF zttr0006-sbewart = '1100' OR      "Flow Type
                   zttr0006-sbewart = '1105' OR
                   zttr0006-sbewart = '1120'.
                  it_list-fdgrp = zttr0001-fdgrv1.   "Planning group
                ELSE.
                  it_list-fdgrp = zttr0001-fdgrv2.   "Planning group
                ENDIF.
              ENDIF.
            ENDIF.
* End of TR
            APPEND it_list.
            CLEAR it_list.
          ENDLOOP.
        ENDIF.

      WHEN 'K'.
*//                           Vendors
        IF it_bseg-augbl = ''.
          MOVE-CORRESPONDING it_bseg TO it_list.

          IF it_list-shkzg = 'S'.
            it_list-wrbtr = it_list-wrbtr * -1.
            it_list-dmbtr = it_list-dmbtr * -1.
          ENDIF.

          it_list-augbl = it_list-belnr.
          it_list-augdt = it_list-budat.
          it_list-gjah1 = it_bseg-gjahr.
          it_list-beln1 = it_bseg-belnr.
          it_list-buze1 = it_bseg-buzei.
          it_list-buda1 = it_bkpf-budat.

          SELECT SINGLE fdgrv INTO it_list-fdgrp FROM lfb1
           WHERE bukrs = p_bukrs
             AND lifnr = it_list-lifnr.

          APPEND it_list. CLEAR it_list.
        ELSE.

          READ TABLE it_bsak WITH KEY augdt = it_bseg-augdt
                                      augbl = it_bseg-augbl
                                      lifnr = it_bseg-lifnr.

          LOOP AT it_bsak FROM sy-tabix.
            CHECK it_bsak-gjahr <> it_bseg-gjahr OR
                  it_bsak-belnr <> it_bseg-belnr.

            IF it_bsak-augdt <> it_bseg-augdt OR
               it_bsak-augbl <> it_bseg-augbl OR
               it_bsak-lifnr <> it_bseg-lifnr .
              EXIT.
            ENDIF.

            IF it_bsak-shkzg = 'H'.
              it_bsak-dmbtr = it_bsak-dmbtr * -1.
              it_bsak-wrbtr = it_bsak-wrbtr * -1.
            ENDIF.

            MOVE-CORRESPONDING it_bsak TO it_list.
            it_list-gjah1 = it_bseg-gjahr.
            it_list-beln1 = it_bseg-belnr.
            it_list-buze1 = it_bseg-buzei.
            it_list-buda1 = it_bkpf-budat.

            SELECT SINGLE fdgrv INTO it_list-fdgrp
              FROM lfb1
             WHERE bukrs = p_bukrs
               AND lifnr = it_list-lifnr.
* Investment
            READ TABLE it_fmifiit WITH KEY kngjahr = it_list-gjahr
                                           knbelnr = it_list-belnr.

            IF sy-subrc = 0.

              LOOP AT it_fmifiit WHERE kngjahr = it_list-gjahr
                                   AND knbelnr = it_list-belnr.

                CLEAR ls_bseg.
                SELECT SINGLE *  INTO ls_bseg
                  FROM bseg
                 WHERE bukrs = p_bukrs
                   AND gjahr = it_fmifiit-kngjahr
                   AND belnr = it_fmifiit-knbelnr
                   AND buzei = it_fmifiit-knbuzei.

                MOVE-CORRESPONDING ls_bseg TO it_list.
                MOVE-CORRESPONDING it_fmifiit TO it_list.

                it_list-buze2 = it_bsak-buzei.
                it_list-budat = it_bsak-budat.
                it_list-blart = it_bsak-blart.
                it_list-bldat = it_bsak-bldat.
                it_list-cpudt = it_bsak-cpudt.

                it_list-gjahr = it_fmifiit-kngjahr.
                it_list-belnr = it_fmifiit-knbelnr.
                it_list-buzei = it_fmifiit-knbuzei.
                it_list-augbl = it_bseg-augbl.
                it_list-augdt = it_bseg-augdt.
                it_list-gjah1 = it_bseg-gjahr.
                it_list-beln1 = it_bseg-belnr.
                it_list-buze1 = it_bseg-buzei.
                it_list-buda1 = it_bkpf-budat.

                READ TABLE lt_invest WITH KEY gjahr = it_list-gjahr
                                              belnr = it_list-belnr
                                              BINARY SEARCH.

                it_list-wrbtr = it_bsak-wrbtr * it_fmifiit-trbtr /
                                   lt_invest-wrbtr.

                it_list-dmbtr = it_bsak-dmbtr * it_fmifiit-fkbtr /
                                   lt_invest-dmbtr.

                it_list-aufnr = it_fmifiit-fonds.

                it_list-fdgr1 = it_list-fdgrp.

                SELECT SINGLE b~fdgrv INTO it_list-fdgrp
                  FROM aufk      AS a
                  JOIN zttr0009  AS b
                    ON a~anfaufnr = b~aufnr
                 WHERE a~aufnr    = it_list-aufnr.

                APPEND it_list. CLEAR it_list.

              ENDLOOP.
            ELSE.

* Payroll-check(?? ????)

              IF it_list-blart = 'HR'.
                READ TABLE it_bseg INTO ls_bseg
                                   WITH KEY gjahr = it_list-augdt(4)
                                            belnr = it_list-augbl
                                            hkont = '0010520111'.
*//                                     Comerica Bank Clearing-Check Out

*//                  HMMA Account has not yet been determined
                IF sy-subrc = 0.
                  it_list-fdgr1 = it_list-fdgrp.
                  it_list-fdgrp = 'C3120'.   "Planning Group
                ENDIF.
              ENDIF.

              APPEND it_list. CLEAR it_list.

            ENDIF.
          ENDLOOP.
        ENDIF.
    ENDCASE.

  ENDLOOP.

  SORT it_list BY gjahr belnr buzei buze2 gjah1 beln1 buze1..
  DELETE ADJACENT DUPLICATES FROM it_list
                  COMPARING gjahr belnr buzei buze2
                            gjah1 beln1 buzei.

  LOOP AT it_list.
    it_list-icon = gc_led_green.
    IF it_list-augbl <> it_list-beln1.  "Clearing Document <>
      it_list-icon = gc_led_yellow.
    ENDIF.

    SELECT SINGLE textl INTO it_list-textl
      FROM t035t
     WHERE spras = sy-langu
       AND grupp = it_list-fdgrp.

    MODIFY it_list TRANSPORTING icon textl.

  ENDLOOP.

ENDFORM.                    "MAKE_IT_LIST

*---------------------------------------------------------------------*
*      Form  DISPLAY_LIST
*---------------------------------------------------------------------*
FORM display_list.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = '90'
      text       = 'Displaying data...'
    EXCEPTIONS
      OTHERS     = 1.

  g_repid = sy-repid.

  alv_layout-zebra            = 'X'.
*  alv_layout-info_fieldname        = 'COLOR'.
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


  LOOP AT alv_fieldcat INTO ls_fieldcat.

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


ENDFORM.                    " build_comment
*---------------------------------------------------------------------*
*      Form  TOP_OF_PAGE
*---------------------------------------------------------------------*

FORM top_of_page.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      i_logo             = 'ENJOYSAP_LOGO'
      it_list_commentary = top_of_page.

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
        WHEN 'IT_LIST-BELNR'.
          SET PARAMETER ID : 'BLN' FIELD it_list-belnr,
                             'BUK' FIELD p_bukrs,
                             'GJR' FIELD it_list-gjahr.

          CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN .

        WHEN 'IT_LIST-AUGBL'.
          SET PARAMETER ID : 'BLN' FIELD it_list-augbl,
                             'BUK' FIELD p_bukrs,
                             'GJR' FIELD it_list-augdt(4).

          CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN .

        WHEN 'IT_LIST-BELN1'.
          SET PARAMETER ID : 'BLN' FIELD it_list-beln1,
                             'BUK' FIELD p_bukrs,
                             'GJR' FIELD it_list-gjah1.

          CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN .

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

ENDFORM.                    " alv_so
*&---------------------------------------------------------------------*
*&      Form  INIT_SCREEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM init_screen .

  GET PARAMETER ID 'BUK' FIELD p_bukrs.
  IF p_bukrs IS INITIAL .
    p_bukrs = 'H201'.
  ENDIF.

  LOOP AT SCREEN.
    IF screen-name = 'P_BUTXT'.
      screen-input  = 0.
      screen-intensified = '0'.
      screen-display_3d  = '0'.
      MODIFY SCREEN.
    ENDIF.
    IF screen-name = 'P_BUKRS'.
      screen-input = ' '.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

* text find
  PERFORM fi_wt_read_t001 USING    p_bukrs
                          CHANGING p_butxt.

ENDFORM.                    " INIT_SCREEN
*&---------------------------------------------------------------------*
*&      Form  FI_WT_READ_T001
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_BUKRS  text
*      <--P_P_BUTXT  text
*----------------------------------------------------------------------*
FORM fi_wt_read_t001  USING    pa_bukrs
                      CHANGING pa_butxt.

  DATA : it_t001 LIKE t001.

  CALL FUNCTION 'FI_WT_READ_T001'
    EXPORTING
      i_bukrs   = pa_bukrs
    IMPORTING
      t_t001    = it_t001
    EXCEPTIONS
      not_found = 1.

  CASE sy-subrc.
    WHEN 0.
      pa_butxt = it_t001-butxt.
    WHEN 1.
      MESSAGE s101(f5).
    WHEN OTHERS.
  ENDCASE.

ENDFORM.                    " FI_WT_READ_T001
*&---------------------------------------------------------------------*
*&      Form  EXPORT_LIST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form EXPORT_LIST .

  EXPORT it_list TO MEMORY ID 'ZTRR01300'.

endform.                    " EXPORT_LIST
