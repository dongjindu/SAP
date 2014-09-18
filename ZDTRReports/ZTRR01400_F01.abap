*&---------------------------------------------------------------------*
*&  Include           ZTRR01400_F01
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

  GET PARAMETER ID 'BUK' FIELD p_bukrs.
  IF p_bukrs IS INITIAL .
    p_bukrs = 'H201'.
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

* text find
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

*---------------------------------------------------------------------*
*      Form  select_data
*---------------------------------------------------------------------*
FORM select_data.

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

  DATA: BEGIN OF lt_bsas OCCURS 0,
          hkont LIKE bsas-hkont,
          augbl LIKE bsas-augbl,
          augdt LIKE bsas-augdt,
        END OF lt_bsas.

  DATA: BEGIN OF lt_belnr OCCURS 0,
          gjahr   LIKE  bseg-gjahr,
          belnr   LIKE  bseg-belnr,
        END OF lt_belnr.

  DATA : lt_bseg LIKE it_bseg OCCURS 0 WITH HEADER LINE.

  clear: it_skb1, it_skb1[], lt_bsak, lt_bsak[], lt_bsad, lt_bsad[],
         it_bseg, it_bseg[], r_hkont1, r_hkont1[], lt_bseg, lt_bseg[],
         lt_bsas, lt_bsas[], lt_belnr, lt_belnr[].

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = '5'
      text       = 'Getting Data...'
    EXCEPTIONS
      OTHERS     = 1.


  SELECT * INTO TABLE it_skb1 FROM skb1 WHERE bukrs = p_bukrs.
  SORT it_skb1 BY bukrs saknr.

  LOOP AT it_skb1.
    CHECK it_skb1-fdlev is not initial.
    make_ranges: it_skb1-saknr  r_hkont1.
  ENDLOOP.

*// == Include ECA Loan
*// HMMA Account has not yet been determined
  IF p_eca = 'X'.
    make_ranges: '0000000000'  r_hkont1.
  ENDIF.

* 1. Get The document including g/l account having palnning level.
*// ==General Ledger: Actual Line Items
  SELECT gjahr belnr buzei budat shkzg dmbtr
         INTO CORRESPONDING FIELDS OF TABLE lt_belnr
    FROM bsis
   WHERE bukrs  = p_bukrs
     AND gjahr in s_gjahr
     AND hkont IN r_hkont1
     AND budat IN s_budat
     AND belnr IN s_belnr
     and bstat = ''.

*//      G/L Accounts (Cleared Items)
  SELECT gjahr belnr buzei budat shkzg dmbtr
         appending CORRESPONDING FIELDS OF TABLE lt_belnr
    FROM bsas
   WHERE bukrs  = p_bukrs
     AND gjahr in s_gjahr
     AND hkont IN r_hkont1
     AND budat IN s_budat
     AND belnr IN s_belnr
     and bstat = ''.

***  SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_belnr
***    FROM faglflexa
***   WHERE rbukrs = p_bukrs
***     AND ryear IN s_gjahr
***     AND budat IN s_budat
***     AND belnr IN s_belnr
***     AND rldnr = 'K0'
***     AND racct IN lr_hkont.

  DELETE ADJACENT DUPLICATES FROM lt_belnr COMPARING gjahr belnr.

  CHECK NOT lt_belnr[] IS INITIAL.

* 2.All item include bank account

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
           AND belnr = lt_belnr-belnr.  "Document Number

  LOOP AT it_bseg WHERE ( koart = 'K'      "Vendors
                     OR   koart = 'D' )    "Customers
                    AND augbl <> ''.       "Clearing Document

    MOVE-CORRESPONDING it_bseg TO lt_bseg.
    APPEND lt_bseg.
    CLEAR lt_bseg.

    DELETE it_bseg.
  ENDLOOP.

  SORT lt_bseg BY gjahr belnr augdt augbl lifnr kunnr.
  DELETE ADJACENT DUPLICATES FROM lt_bseg COMPARING gjahr belnr augdt
                                                    augbl lifnr kunnr.


  LOOP AT lt_bseg.
    MOVE-CORRESPONDING lt_bseg TO it_bseg.
    APPEND it_bseg.
    CLEAR it_bseg.
  ENDLOOP.

  SORT it_bseg BY gjahr belnr hkont.

  LOOP AT it_bseg.
    IF it_bseg-lifnr <> ''.         "Vendors
      lt_bsak-lifnr = it_bseg-lifnr.
      lt_bsak-augbl = it_bseg-augbl.
      lt_bsak-augdt = it_bseg-augdt.
      COLLECT lt_bsak.
      CLEAR lt_bsak.

    ELSEIF it_bseg-kunnr <> ''.    "Customers
      lt_bsad-kunnr = it_bseg-kunnr.
      lt_bsad-augbl = it_bseg-augbl.
      lt_bsad-augdt = it_bseg-augdt.
      COLLECT lt_bsad.
      CLEAR lt_bsad.

*// ===== HMMA Account has not yet been determined
    ELSEIF it_bseg-hkont = '0000000000' OR   "ECA Loan
           it_bseg-hkont = '000000000'.
      lt_bsas-hkont = it_bseg-kunnr.
      lt_bsas-augbl = it_bseg-augbl.
      lt_bsas-augdt = it_bseg-augdt.
      COLLECT lt_bsas.
      CLEAR lt_bsas.
    ENDIF.
  ENDLOOP.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = '10'
      text       = 'Getting Data...'
    EXCEPTIONS
      OTHERS     = 1.

  IF NOT lt_bsak[] IS INITIAL.

    SELECT * INTO CORRESPONDING FIELDS OF TABLE it_bsak
      FROM bsak FOR ALL ENTRIES IN lt_bsak
     WHERE bukrs =  p_bukrs        " Company Code
       AND lifnr = lt_bsak-lifnr
       AND augbl = lt_bsak-augbl   " Fiscal year
       AND augdt = lt_bsak-augdt.  "Document Number
  ENDIF.

  IF NOT lt_bsad[] IS INITIAL.

    SELECT * INTO CORRESPONDING FIELDS OF TABLE it_bsad
      FROM bsad FOR ALL ENTRIES IN lt_bsad
     WHERE bukrs =  p_bukrs        " Company Code
       AND kunnr = lt_bsad-kunnr
       AND augbl = lt_bsad-augbl   " Fiscal year
       AND augdt = lt_bsad-augdt.   "Document Number
  ENDIF.

  IF NOT lt_bsas[] IS INITIAL.

    SELECT * INTO CORRESPONDING FIELDS OF TABLE it_bsas
      FROM bsas FOR ALL ENTRIES IN lt_bsas
     WHERE bukrs =  p_bukrs        " Company Code
       AND hkont = lt_bsas-hkont
       AND augbl = lt_bsas-augbl   " Fiscal year
       AND augdt = lt_bsas-augdt.   "Document Number
  ENDIF.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = '20'
      text       = 'Getting Data...'
    EXCEPTIONS
      OTHERS     = 1.

  CLEAR lt_belnr[].

  LOOP AT it_bsak.
    lt_belnr-gjahr = it_bsak-gjahr.
    lt_belnr-belnr = it_bsak-belnr.
    COLLECT lt_belnr.
    CLEAR lt_belnr.
  ENDLOOP.

  LOOP AT it_bsad.
    lt_belnr-gjahr = it_bsad-gjahr.
    lt_belnr-belnr = it_bsad-belnr.
    COLLECT lt_belnr.
    CLEAR lt_belnr.
  ENDLOOP.

  LOOP AT it_bsas.
    lt_belnr-gjahr = it_bsas-gjahr.
    lt_belnr-belnr = it_bsas-belnr.
    COLLECT lt_belnr.
    CLEAR lt_belnr.
  ENDLOOP.


  IF NOT lt_belnr[] IS INITIAL.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE gt_bseg
      FROM bseg FOR ALL ENTRIES IN lt_belnr
     WHERE bukrs =  p_bukrs        " Company Code
       AND gjahr = lt_belnr-gjahr  " Fiscal year
       AND belnr = lt_belnr-belnr. " ocument Number

    LOOP AT gt_bseg WHERE buzid = 'T'.
                         "Identification of the Line Item
      APPEND gt_bseg TO gt_btax.
      CLEAR gt_btax.
    ENDLOOP.

    SORT gt_btax BY bukrs gjahr belnr buzei.

    DELETE ADJACENT DUPLICATES FROM gt_btax
                               COMPARING bukrs gjahr belnr.
    DELETE gt_bseg WHERE buzid = 'T'.

  ENDIF.

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

  DATA : ls_bseg LIKE bseg,  ls_skb1 LIKE skb1.

  DATA : BEGIN OF lt_bseg OCCURS 0,
           gjahr LIKE bseg-gjahr,
           belnr LIKE bseg-belnr,
           dmbtr LIKE bseg-dmbtr,
           wrbtr LIKE bseg-wrbtr,
         END OF lt_bseg.

  DATA lt_btax LIKE TABLE OF lt_bseg WITH HEADER LINE.

  LOOP AT gt_bseg WHERE mwskz = 'U1'   "tax_code
                    AND txbfw = 0.
                           "Original Tax Base Amount in Document Curr
    CHECK gt_bseg-koart = 'S'  OR    "Account Type
          gt_bseg-koart = 'A'.
*                                    S: G/L accounts
*                                    A: Assets

    MOVE-CORRESPONDING gt_bseg TO lt_btax.
    COLLECT lt_btax.
    CLEAR lt_btax.
  ENDLOOP.

  SORT lt_btax BY gjahr belnr.

*  LOOP AT gt_bseg.
*    CHECK gt_bseg-koart = 'S' OR gt_bseg-koart = 'A'.
*    IF gt_bseg-mwskz = 'U1'.
*      IF gt_bseg-txbfw <> 0.
*        gt_bseg-wrbtr = gt_bseg-txbfw.
*        gt_bseg-dmbtr = gt_bseg-txbhw.
*      ELSE.
*        READ TABLE gt_btax WITH KEY bukrs = p_bukrs
*                                    gjahr = gt_bseg-gjahr
*                                    belnr = gt_bseg-belnr BINARY SEARCH
*.
*
*
*        READ TABLE lt_btax WITH KEY gjahr = gt_bseg-gjahr
*                                    belnr = gt_bseg-belnr BINARY SEARCH
*.
*
*
*        IF lt_btax-wrbtr <> 0.
*          gt_bseg-wrbtr = gt_bseg-wrbtr - ( ABS( gt_btax-fwbas ) *
*                          '0.07' ) *
*                          gt_bseg-wrbtr / lt_btax-wrbtr.
*        ENDIF.
*
*        IF lt_btax-dmbtr <> 0.
*          gt_bseg-dmbtr = gt_bseg-dmbtr - ( ABS( gt_btax-hwbas ) *
*                          '0.07' ) *
*                          gt_bseg-dmbtr / lt_btax-dmbtr.
*        ENDIF.
*
*      ENDIF.
*    ENDIF.
*    MOVE-CORRESPONDING gt_bseg TO lt_bseg.
*    COLLECT lt_bseg. CLEAR lt_bseg.
*  ENDLOOP.

  SORT lt_bseg BY gjahr belnr.

  DESCRIBE TABLE it_bseg LINES l_line.

  LOOP AT it_bseg.

    READ TABLE it_skb1 WITH KEY bukrs = p_bukrs
                                saknr = it_bseg-hkont.

*// mitkz: Account is reconciliation account
    IF it_skb1-mitkz = '' OR it_skb1-mitkz = 'A'.
*//         A:  Assets
*//         D:  Customers
*//         K:  Vendors
*//         V:  Contract accounts receivable
      CHECK it_skb1-fdlev = ''.

      IF p_eca = 'X'.
*//           HMMA Account has not yet been determined
        IF it_skb1-saknr = '0021002000'.   "ECA Loan
          CLEAR l_continue.
          LOOP AT it_bseg INTO ls_bseg WHERE gjahr = it_bseg-gjahr
                                         AND belnr = it_bseg-belnr
                                         AND hkont <> '0021002000'.
*                                                     "ECA Loan

            READ TABLE it_skb1 INTO ls_skb1 WITH KEY bukrs = p_bukrs
                                              saknr = ls_bseg-hkont.
            IF ls_skb1-fdlev = ''.
              l_continue = 'X'.
              EXIT.
            ENDIF.

          ENDLOOP.
        ENDIF.
        IF l_continue = 'X'.
*//                  HMMA Account has not yet been determined
          CHECK it_skb1-saknr <> '0021002000'.  "ECA Loan
        ENDIF.
      ENDIF.
*// Account is reconciliation account.
    ELSEIF it_skb1-mitkz = 'D' OR     "D:  Customers
           it_skb1-mitkz = 'K'.       "K:  Vendors
      CHECK it_bseg-augbl = ''.
    ENDIF.

    READ TABLE it_bkpf WITH KEY bukrs = p_bukrs
                                gjahr = it_bseg-gjahr
                                belnr = it_bseg-belnr
                                BINARY SEARCH.
    MOVE-CORRESPONDING it_bkpf TO it_list.

* HR Payroll
*    IF it_bseg-hkont = '0020554510' OR
*       it_bseg-hkont = '002054520'.
*
*        READ TABLE it_bsas WITH KEY augdt = it_bseg-augdt
*                                    augbl = it_bseg-augbl
*                                    hkont = it_bseg-hkont.
*
*        LOOP AT it_bsas FROM sy-tabix.
*          CHECK it_bsas-gjahr <> it_bseg-gjahr OR
*                it_bsas-belnr <> it_bseg-belnr.
*
*          IF it_bsas-augdt <> it_bseg-augdt OR
*             it_bsas-augbl <> it_bseg-augbl OR
*             it_bsas-hkont <> it_bseg-hkont .
*            EXIT.
*          ENDIF.
*
*       ENDLOOP.
*
*    ELSE.
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

    APPEND it_list.
    CLEAR it_list.
*    ENDIF.
  ENDLOOP.


  SORT it_bsak BY augdt augbl lifnr gjahr belnr buzei. " AUGGJ
  SORT it_bsad BY augdt augbl kunnr gjahr belnr buzei.

  LOOP AT it_bseg WHERE augbl <> ''.

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

    READ TABLE it_skb1 WITH KEY bukrs = p_bukrs
                                saknr = it_bseg-hkont.
    CASE it_skb1-mitkz.

      WHEN 'D'.
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

          READ TABLE gt_bseg WITH KEY gjahr = it_bsad-gjahr
                                      belnr = it_bsad-belnr.

          LOOP AT gt_bseg FROM sy-tabix.
            CHECK gt_bseg-buzei <> it_bsad-buzei.

            IF gt_bseg-gjahr <> it_bsad-gjahr OR
               gt_bseg-belnr <> it_bsad-belnr.
              EXIT.
            ENDIF.

            IF gt_bseg-mwskz = 'U1'.
              IF gt_bseg-txbfw <> 0.
                gt_bseg-wrbtr = gt_bseg-txbfw.
                gt_bseg-dmbtr = gt_bseg-txbhw.
              ELSE.
                READ TABLE gt_btax WITH KEY bukrs = p_bukrs
                                            gjahr = gt_bseg-gjahr
                                            belnr = gt_bseg-belnr
                                            BINARY SEARCH.


                READ TABLE lt_btax WITH KEY gjahr = gt_bseg-gjahr
                                            belnr = gt_bseg-belnr
                                            BINARY SEARCH.

*//  Calculate Taxes
                IF lt_btax-wrbtr <> 0.
                  gt_bseg-wrbtr = gt_bseg-wrbtr -
*//                                ( ABS( gt_btax-fwbas ) * '0.07' ) *
                                ( ABS( gt_btax-fwbas ) * '0.10' ) *
                                  gt_bseg-wrbtr / lt_btax-wrbtr.
                ENDIF.

                IF lt_btax-dmbtr <> 0.
                  gt_bseg-dmbtr = gt_bseg-dmbtr -
*//                                ( ABS( gt_btax-hwbas ) * '0.07' ) *
                                ( ABS( gt_btax-hwbas ) * '0.10' ) *
                                  gt_bseg-dmbtr / lt_btax-dmbtr.
                ENDIF.

              ENDIF.
            ENDIF.

            MOVE-CORRESPONDING gt_bseg TO it_list.

            it_list-buze2 = it_bsad-buzei.
            it_list-budat = it_bsad-budat.
            it_list-blart = it_bsad-blart.
            it_list-bldat = it_bsad-bldat.
            it_list-cpudt = it_bsad-cpudt.

            it_list-gjahr = gt_bseg-gjahr.
            it_list-belnr = gt_bseg-belnr.
            it_list-buzei = gt_bseg-buzei.
            it_list-augbl = it_bseg-augbl.
            it_list-augdt = it_bseg-augdt.
            it_list-gjah1 = it_bseg-gjahr.
            it_list-beln1 = it_bseg-belnr.
            it_list-buze1 = it_bseg-buzei.
            it_list-buda1 = it_bkpf-budat.

            IF it_list-shkzg = 'S'.
              it_list-wrbtr = it_list-wrbtr * -1.
              it_list-dmbtr = it_list-dmbtr * -1.
            ENDIF.

            if it_list-belnr is initial and
               it_list-wrbtr is initial and
               it_list-dmbtr is initial.
            else.
              APPEND it_list.
            endif.
            CLEAR: it_list.

***            APPEND it_list. CLEAR it_list.
          ENDLOOP.

          if it_list-belnr is initial and
             it_list-wrbtr is initial and
             it_list-dmbtr is initial.
          else.
            APPEND it_list.
          endif.
          CLEAR: it_list.
***          APPEND it_list. CLEAR it_list.
        ENDLOOP.

      WHEN 'K'.

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


          READ TABLE gt_bseg WITH KEY gjahr = it_bsak-gjahr
                                      belnr = it_bsak-belnr.

          LOOP AT gt_bseg FROM sy-tabix.
            CHECK gt_bseg-buzei <> it_bsak-buzei.

            IF gt_bseg-gjahr <> it_bsak-gjahr OR
               gt_bseg-belnr <> it_bsak-belnr.
              EXIT.
            ENDIF.

            IF gt_bseg-mwskz = 'U1'.  "Tax_code.
*//             "Original Tax Base Amount in Document Currency
              IF gt_bseg-txbfw <> 0.
                gt_bseg-wrbtr = gt_bseg-txbfw.
                gt_bseg-dmbtr = gt_bseg-txbhw.
              ELSE.
                READ TABLE gt_btax WITH KEY bukrs = p_bukrs
                                            gjahr = gt_bseg-gjahr
                                            belnr = gt_bseg-belnr
                                            BINARY SEARCH.


                READ TABLE lt_btax WITH KEY gjahr = gt_bseg-gjahr
                                            belnr = gt_bseg-belnr
                                            BINARY SEARCH.

*// tax (Calculate Taxes.)
                IF lt_btax-wrbtr <> 0.
                  gt_bseg-wrbtr = gt_bseg-wrbtr -
*//                                ( ABS( gt_btax-fwbas ) * '0.07' ) *
                                ( ABS( gt_btax-fwbas ) * '0.10' ) *
                                  gt_bseg-wrbtr / lt_btax-wrbtr.
                ENDIF.

                IF lt_btax-dmbtr <> 0.
                  gt_bseg-dmbtr = gt_bseg-dmbtr -
**                                ( ABS( gt_btax-hwbas ) * '0.07' ) *
                                ( ABS( gt_btax-hwbas ) * '0.10' ) *
                                  gt_bseg-dmbtr / lt_btax-dmbtr.
                ENDIF.

              ENDIF.
            ENDIF.

            MOVE-CORRESPONDING gt_bseg TO it_list.

            it_list-buze2 = it_bsak-buzei.
            it_list-budat = it_bsak-budat.
            it_list-blart = it_bsak-blart.
            it_list-bldat = it_bsak-bldat.
            it_list-cpudt = it_bsak-cpudt.

            it_list-gjahr = gt_bseg-gjahr.
            it_list-belnr = gt_bseg-belnr.
            it_list-buzei = gt_bseg-buzei.
            it_list-augbl = it_bseg-augbl.
            it_list-augdt = it_bseg-augdt.
            it_list-gjah1 = it_bseg-gjahr.
            it_list-beln1 = it_bseg-belnr.
            it_list-buze1 = it_bseg-buzei.
            it_list-buda1 = it_bkpf-budat.

*            CLEAR lt_bseg.
*            READ TABLE lt_bseg WITH KEY gjahr = it_list-gjahr
*                                        belnr = it_list-belnr
*                                        BINARY SEARCH.
*
*            IF lt_bseg-wrbtr <> 0.
*              it_list-wrbtr = it_bsak-wrbtr * gt_bseg-wrbtr /
*                                 lt_bseg-wrbtr.
*
*              it_list-dmbtr = it_bsak-dmbtr * gt_bseg-dmbtr /
*                                lt_bseg-dmbtr.
*            ENDIF.

            IF it_list-shkzg = 'S'.
              it_list-wrbtr = it_list-wrbtr * -1.
              it_list-dmbtr = it_list-dmbtr * -1.
            ENDIF.

            if it_list-belnr is initial and
               it_list-wrbtr is initial and
               it_list-dmbtr is initial.
            else.
              APPEND it_list.
            endif.
            CLEAR: it_list.

          ENDLOOP.

        ENDLOOP.

    ENDCASE.

  ENDLOOP.

  SORT it_list BY gjahr belnr buzei buze2 gjah1 beln1 buze1..
  DELETE ADJACENT DUPLICATES FROM it_list
                             COMPARING gjahr belnr buzei
                                       buze2 gjah1 beln1 buzei.

  LOOP AT it_list.
    it_list-icon = gc_led_green.

    IF it_list-augbl <> it_list-beln1.
      it_list-icon = gc_led_yellow.
    ENDIF.

    SELECT SINGLE txt50 INTO it_list-txt50
      FROM skat
     WHERE spras = sy-langu
       AND ktopl = gc_ktopl      "p_bukrs
       AND saknr = it_list-hkont.

    MODIFY it_list TRANSPORTING icon txt50.

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
*&      Form  EXPORT_LIST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form EXPORT_LIST .

  EXPORT it_list TO MEMORY ID 'ZTRR01400'.

endform.                    " EXPORT_LIST
