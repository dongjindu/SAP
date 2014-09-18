*----------------------------------------------------------------------*
***INCLUDE RFWT0020_EWT.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  vendor_document_check
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*

*--- extended w/tax data
FORM vendor_document_check
 USING wa_bsxk LIKE wa_bsak
 CHANGING h_added_with_item LIKE i_added_with_item[].

  DATA: l_bset LIKE bset OCCURS 1 WITH HEADER LINE,
        l_bkpf LIKE bkpf OCCURS 1 WITH HEADER LINE,
        l_bseg LIKE bseg OCCURS 1 WITH HEADER LINE,
        l_with_itemx LIKE with_itemx OCCURS 1 WITH HEADER LINE,
        h_with_itemx LIKE with_itemx OCCURS 1 WITH HEADER LINE.
  DATA: i_with_item LIKE with_item OCCURS 1 WITH HEADER LINE,
        h_with_item LIKE with_item OCCURS 1 WITH HEADER LINE,
        h_t059p TYPE t059p OCCURS 1 WITH HEADER LINE,
        h_t059z TYPE t059z OCCURS 1 WITH HEADER LINE,
        h_rc LIKE sy-subrc,
        lf_withcd LIKE with_item-wt_withcd.

  IF r_inv = 'X'.
    CHECK wa_bsxk-xzahl IS INITIAL.
  ELSE.
    CHECK wa_bsxk-xzahl = 'X'.
  ENDIF.

*---read wt info for vendor (belnr, gjahr, buzei, bukrs) from with_item
*---into i_x_with_item
  CALL FUNCTION 'FI_WT_READ_WT_INFO'
       EXPORTING
            i_bukrs     = wa_bsxk-bukrs
            i_belnr     = wa_bsxk-belnr
            i_gjahr     = wa_bsxk-gjahr
            i_buzei     = wa_bsxk-buzei
       TABLES
            t_with_item = i_with_item
       EXCEPTIONS
            not_found   = 1
            OTHERS      = 2.
  IF xcreate EQ 'X'.
    IF entry = 'X'.
      CHECK sy-subrc = 1.
    ELSE.
      CHECK sy-subrc = 0 OR sy-subrc = 1.
    ENDIF.
  ELSE.
    CHECK sy-subrc = 0.
  ENDIF.

*---select w/tax info where base amounts has to be filled into
*---                     h_with_item
*---using customizing data and table i_accit, t059p, t059z
  LOOP AT i_accit_wt.

*---read t059p for t059p-dopost
    CALL FUNCTION 'FI_WT_READ_T059P'
         EXPORTING
              i_bukrs   = wa_t001-bukrs
              i_type    = i_accit_wt-witht
         TABLES
              t_t059p   = h_t059p
         EXCEPTIONS
              not_found = 1
              OTHERS    = 2.

    IF sy-subrc <> 0.
      EXIT.
    ENDIF.

*--- w/tax type has no accumulation
    CHECK h_t059p-wt_accpt = '0'.
*--- w/tax type has no central invoice
   if not (   h_t059p-WITHT eq 'FE'  OR
            h_t059p-WITHT eq 'ST'  ).
    CHECK h_t059p-wt_ceinv = '0'.
    endif.
*--- w/tax type has no certification numbers
    CHECK h_t059p-wt_ctnbrl = 'X'.
*--- w/tax has to be posted
*    CHECK h_t059p-wt_dopost = 'X'.
*--- umsks <> 'W'
    CHECK wa_bsxk-umsks <> 'W'.

*---read t059z for t059z-qsatz, t059z-wt_ratez, t059z-xqfor
    CALL FUNCTION 'FI_WT_READ_T059Z'
         EXPORTING
              i_bukrs     = wa_t001-bukrs
              i_type      = i_accit_wt-witht
              i_wt_withcd = i_accit_wt-wt_withcd
         TABLES
              t_t059z     = h_t059z
         EXCEPTIONS
              not_found   = 1
              OTHERS      = 2.

    IF sy-subrc <> 0.
      EXIT.
    ENDIF.


*---- process items with w/tax 0%
    IF h_t059z-qsatz = 0
       AND h_t059z-wt_ratez = ' '
       AND h_t059z-xqfor = ' '.
      h_with_item-witht = i_accit_wt-witht.
      h_with_item-wt_withcd = i_accit_wt-wt_withcd.

*---- payments on account (xzahl='X', umsks=' ', ebzt=' ' and rebzg=' ')
*---- clear w/tax code
      IF   wa_bsxk-xzahl = 'X'
       AND wa_bsxk-umsks = ' '
       AND wa_bsxk-rebzt = ' '
     AND wa_bsxk-rebzg = ' '
     AND NOT wa_bsxk-augbl = wa_bsxk-belnr.
        CLEAR h_with_item-wt_withcd.
      ENDIF.

      APPEND h_with_item.
    ELSEIF h_t059z-qsatz = 0
       AND h_t059z-xqfor = 'X'.
      i_xqfor_with_item-witht = i_accit_wt-witht.
      i_xqfor_with_item-wt_withcd = i_accit_wt-wt_withcd.
      i_xqfor_with_item-wt_acco = wa_bsxk-lifnr.
      MOVE-CORRESPONDING wa_bsxk TO i_xqfor_with_item.
      APPEND i_xqfor_with_item.
    ENDIF.
  ENDLOOP.
*---- by now all data to be included in with_item are filled in
*---- h_x_with_item; however base amounts are not filled yet
*---- in h_x_with_item lines are deleted which are already
*---- in i_x_with_item.
  LOOP AT i_with_item.
    IF xcreate EQ 'X'.
      IF xchangeall = 'X'.
        IF NOT i_with_item-wt_qbshb IS INITIAL
           OR i_with_item-wt_basman = 'X'.
          DELETE h_with_item
                    WHERE witht = i_with_item-witht.
        ENDIF.
      ELSEIF xzerobase = 'X'.
        IF NOT i_with_item-wt_qsshb IS INITIAL
            OR i_with_item-wt_basman = 'X'.
          DELETE h_with_item
                    WHERE witht = i_with_item-witht.
        ENDIF.
      ELSE.
        DELETE h_with_item
                  WHERE witht = i_with_item-witht.
      ENDIF.
    ELSE.
      IF i_with_item-wt_withcd IS INITIAL.
        DELETE h_with_item
               WHERE witht = i_with_item-witht.
        CONTINUE.
      ENDIF.
      READ TABLE h_with_item WITH KEY witht = i_with_item-witht.
      IF sy-subrc EQ 0.
*---read t059p for t059p-dopost
        CALL FUNCTION 'FI_WT_READ_T059P'
             EXPORTING
                  i_bukrs   = wa_t001-bukrs
                  i_type    = i_with_item-witht
             TABLES
                  t_t059p   = h_t059p
             EXCEPTIONS
                  not_found = 1
                  OTHERS    = 2.

        IF sy-subrc <> 0.
          DELETE h_with_item
                 WHERE witht = i_with_item-witht.
          CONTINUE.
        ENDIF.

*--- w/tax type has no accumulation, no central invoice and
* no certification numbers
   if not ( h_t059p-WITHT ne 'FE' or
       h_t059p-WITHT ne 'ST' ).
        IF NOT ( h_t059p-wt_accpt = '0' ) OR
           NOT ( h_t059p-wt_ceinv = '0' ) OR
           NOT ( h_t059p-wt_ctnbrl = 'X' ).
          DELETE h_with_item
                 WHERE witht = i_with_item-witht.
          CONTINUE.
        ENDIF.
   endif.
*--- w/tax has to be posted
*    CHECK h_t059p-wt_dopost = 'X'.

*---read t059z for t059z-qsatz, t059z-wt_ratez, t059z-xqfor
        CALL FUNCTION 'FI_WT_READ_T059Z'
             EXPORTING
                  i_bukrs     = wa_t001-bukrs
                  i_type      = i_with_item-witht
                  i_wt_withcd = i_with_item-wt_withcd
             TABLES
                  t_t059z     = h_t059z
             EXCEPTIONS
                  not_found   = 1
                  OTHERS      = 2.

        IF sy-subrc <> 0.
          DELETE h_with_item
                WHERE witht = i_with_item-witht.
          CONTINUE.
        ENDIF.
        IF NOT ( h_t059z-qsatz = 0 AND
                h_t059z-wt_ratez = ' ' AND
                h_t059z-xqfor = ' ' ).
          DELETE h_with_item
                 WHERE witht = i_with_item-witht.
          CONTINUE.
        ENDIF.
        IF h_with_item-wt_withcd =
           i_with_item-wt_withcd.
          DELETE h_with_item
                 WHERE witht = i_with_item-witht.
          CONTINUE.
        ENDIF.
      ELSE.
        DELETE h_with_item
               WHERE witht = i_with_item-witht.
      ENDIF.
    ENDIF.
  ENDLOOP.
* Check if Withholding tax type is changed..If so changed
* UD1K921605


*---- read data from bseg
  REFRESH l_bseg. CLEAR l_bseg.
  SELECT * FROM bseg INTO TABLE l_bseg
          WHERE bukrs = wa_bsxk-bukrs
          AND   belnr = wa_bsxk-belnr
          AND   gjahr = wa_bsxk-gjahr
          AND   buzei = wa_bsxk-buzei.

  IF wa_bsxk-xzahl = 'X'.
    CLEAR h_with_itemx.
    REFRESH h_with_itemx.
    IF xcreate EQ 'X'.
      PERFORM fi_wt_check_calculation TABLES h_with_itemx
                                       USING wa_bsxk-bukrs
                                             wa_bsxk-gjahr
                                             wa_bsxk-belnr
                                             wa_bsxk-buzei
                                             h_rc.
      IF h_rc <> 0.
        MOVE-CORRESPONDING wa_bsxk TO i_unchangeable_bseg.
        APPEND i_unchangeable_bseg.
        EXIT.
      ENDIF.
    ENDIF.
  ELSE.
*---- read data from bkpf
    REFRESH l_bkpf. CLEAR l_bkpf.
    SELECT * FROM bkpf INTO TABLE l_bkpf
            WHERE bukrs = wa_bsxk-bukrs
            AND   belnr = wa_bsxk-belnr
            AND   gjahr = wa_bsxk-gjahr.

*---- fill header of l_bkpf.
    LOOP AT l_bkpf. EXIT. ENDLOOP.

*---- read data from tax table bset
    REFRESH l_bset. CLEAR l_bset.
    SELECT * FROM bset INTO TABLE l_bset
            WHERE bukrs = wa_bsxk-bukrs
            AND   belnr = wa_bsxk-belnr
            AND   gjahr = wa_bsxk-gjahr
            AND   buzei = wa_bsxk-buzei.
  ENDIF.

  LOOP AT h_with_item.
    IF xcreate EQ 'X'.
      IF wa_bsxk-xzahl = 'X'.
        READ TABLE h_with_itemx WITH KEY buzei = wa_bsxk-buzei
                                        witht = h_with_item-witht
                                      wt_withcd = h_with_item-wt_withcd.
        IF sy-subrc = 0.
          h_with_itemx-bukrs = wa_bsxk-bukrs.
          h_with_itemx-belnr = wa_bsxk-belnr.
          h_with_itemx-gjahr = wa_bsxk-gjahr.
          h_with_itemx-buzei = wa_bsxk-buzei.
          h_with_itemx-augdt = wa_bsxk-augdt.
          h_with_itemx-augbl = wa_bsxk-augbl.
          h_with_itemx-wt_acco = wa_bsxk-lifnr.
          h_with_itemx-koart = 'K'.
          MOVE-CORRESPONDING h_with_itemx TO h_with_item.
        ENDIF.
      ELSE.
        CLEAR l_with_itemx.
        REFRESH l_with_itemx.
*---- data from bsxk
        l_with_itemx-bukrs = wa_bsxk-bukrs.
        l_with_itemx-belnr = wa_bsxk-belnr.
        l_with_itemx-gjahr = wa_bsxk-gjahr.
        l_with_itemx-buzei = wa_bsxk-buzei.
        l_with_itemx-augdt = wa_bsxk-augdt.
        l_with_itemx-augbl = wa_bsxk-augbl.
        l_with_itemx-wt_acco = wa_bsxk-lifnr.
        l_with_itemx-koart = 'K'.

*---- data w/tax type and code
        l_with_itemx-witht = h_with_item-witht.
        l_with_itemx-wt_withcd = h_with_item-wt_withcd.
        APPEND l_with_itemx.

*---- calculation of w/tax base amounts, if w/tax code exists
        IF NOT l_with_itemx-wt_withcd IS INITIAL.
          PERFORM fi_wt_calc_withholding_base(saplfwtc)
                                              TABLES l_bseg
                                                     l_bset
                                                     l_with_itemx
                                               USING l_bkpf.
        ENDIF.
        MOVE-CORRESPONDING l_with_itemx TO h_with_item.
      ENDIF.
    ELSE.
      READ TABLE i_with_item WITH KEY witht = h_with_item-witht.
      lf_withcd = h_with_item-wt_withcd.
* begin of changes - * UD1K921605
* if Withholding
      if i_with_item-witht ne h_with_item-witht.
        move h_with_item-witht to i_with_item-witht.
      endif.
* End of changes

      MOVE-CORRESPONDING i_with_item TO h_with_item.

      h_with_item-wt_withcd = lf_withcd.
    ENDIF.

*---- add new lines to h_added_with_item.

    READ TABLE l_bseg WITH KEY
              bukrs  = h_with_item-bukrs
              belnr  = h_with_item-belnr
              gjahr  = h_with_item-gjahr
              buzei  = h_with_item-buzei.

    l_bseg-qsskz = 'XX'.
    APPEND l_bseg TO i_changed_bseg_xx.
*--- correct bseg-line with QSSKZ = 'XX'.
    CASE l_status.
      WHEN open_items.
        SELECT SINGLE  * FROM  bsik
                       WHERE  bukrs  = l_bseg-bukrs
                       AND    lifnr  = l_bseg-lifnr
                       AND    umsks  = l_bseg-umsks
                       AND    umskz  = l_bseg-umskz
                       AND    augdt  = l_bseg-augdt
                       AND    augbl  = l_bseg-augbl
                       AND    zuonr  = l_bseg-zuonr
                       AND    gjahr  = l_bseg-gjahr
                       AND    belnr  = l_bseg-belnr
                       AND    buzei  = l_bseg-buzei.
        CHECK sy-subrc = 0.
        bsik-qsskz = 'XX'.
        APPEND bsik TO i_changed_bsik_xx.

      WHEN closed_items.
        SELECT SINGLE * FROM  bsak
               WHERE  bukrs  = l_bseg-bukrs
               AND    lifnr  = l_bseg-lifnr
               AND    umsks  = l_bseg-umsks
               AND    umskz  = l_bseg-umskz
               AND    augdt  = l_bseg-augdt
               AND    augbl  = l_bseg-augbl
               AND    zuonr  = l_bseg-zuonr
               AND    gjahr  = l_bseg-gjahr
               AND    belnr  = l_bseg-belnr
               AND    buzei  = l_bseg-buzei.
        CHECK sy-subrc = 0.
        bsak-qsskz = 'XX'.
        APPEND bsak TO i_changed_bsak_xx.
    ENDCASE.
    IF h_t059p-wt_dopost IS INITIAL.
      h_with_item-wt_stat = 'X'.
    ENDIF.
    APPEND h_with_item TO h_added_with_item.
  ENDLOOP.

*---- h_added_with_item contains now all data which have to be added
*---- (or changed) to previous items!
ENDFORM.                               " vendor_document_check
*&---------------------------------------------------------------------*
*&      Form  customer_document_check
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_BSAD  text
*      <--P_I_ADDED_WITH_ITEM[]  text
*----------------------------------------------------------------------*
FORM customer_document_check  USING wa_bsxd LIKE wa_bsad
              CHANGING h_added_with_item LIKE i_added_with_item[].

  DATA: l_bset LIKE bset OCCURS 1 WITH HEADER LINE,
        l_bkpf LIKE bkpf OCCURS 1 WITH HEADER LINE,
        l_bseg LIKE bseg OCCURS 1 WITH HEADER LINE,
        l_with_itemx LIKE with_itemx OCCURS 1 WITH HEADER LINE,
        h_with_itemx LIKE with_itemx OCCURS 1 WITH HEADER LINE.
  DATA: i_with_item LIKE with_item OCCURS 1 WITH HEADER LINE,
        h_with_item LIKE with_item OCCURS 1 WITH HEADER LINE,
        h_t059p TYPE t059p OCCURS 1 WITH HEADER LINE,
        h_t059z TYPE t059z OCCURS 1 WITH HEADER LINE,
        h_rc LIKE sy-subrc.

  IF r_inv = 'X'.
    CHECK wa_bsxd-xzahl IS INITIAL.
  ELSE.
    CHECK wa_bsxd-xzahl = 'X'.
  ENDIF.

*---read wt info for vendor (belnr, gjahr, buzei, bukrs) from with_item
*---into i_x_with_item
  CALL FUNCTION 'FI_WT_READ_WT_INFO'
       EXPORTING
            i_bukrs     = wa_bsxd-bukrs
            i_belnr     = wa_bsxd-belnr
            i_gjahr     = wa_bsxd-gjahr
            i_buzei     = wa_bsxd-buzei
       TABLES
            t_with_item = i_with_item
       EXCEPTIONS
            not_found   = 1
            OTHERS      = 2.
  IF entry = 'X'.
    CHECK sy-subrc = 1.
  ELSE.
    CHECK sy-subrc = 0 OR sy-subrc = 1.
  ENDIF.

*---select w/tax info where base amounts has to be filled into
*---                     h_with_item
*---using customizing data and table i_accit, t059p, t059z
  LOOP AT i_accit_wt.

*---read t059p for t059p-dopost
    CALL FUNCTION 'FI_WT_READ_T059P'
         EXPORTING
              i_bukrs   = wa_t001-bukrs
              i_type    = i_accit_wt-witht
         TABLES
              t_t059p   = h_t059p
         EXCEPTIONS
              not_found = 1
              OTHERS    = 2.

    IF sy-subrc <> 0.
      EXIT.
    ENDIF.

*--- w/tax type has no accumulation
    CHECK h_t059p-wt_accpt = '0'.
*--- w/tax type has no central invoice
    CHECK h_t059p-wt_ceinv = '0'.
*--- w/tax type has no certification numbers
    CHECK h_t059p-wt_ctnbrl = 'X'.
*--- w/tax has to be posted
*    CHECK h_t059p-wt_dopost = 'X'.
*--- umsks <> 'W'
    CHECK wa_bsxd-umsks <> 'W'.

*---read t059z for t059z-qsatz, t059z-wt_ratez, t059z-xqfor
    CALL FUNCTION 'FI_WT_READ_T059Z'
         EXPORTING
              i_bukrs     = wa_t001-bukrs
              i_type      = i_accit_wt-witht
              i_wt_withcd = i_accit_wt-wt_withcd
         TABLES
              t_t059z     = h_t059z
         EXCEPTIONS
              not_found   = 1
              OTHERS      = 2.

    IF sy-subrc <> 0.
      EXIT.
    ENDIF.


*---- process items with w/tax 0%
    IF h_t059z-qsatz = 0
       AND h_t059z-wt_ratez = ' '
       AND h_t059z-xqfor = ' '.
      h_with_item-witht = i_accit_wt-witht.
      h_with_item-wt_withcd = i_accit_wt-wt_withcd.

*---- payments on account (xzahl='X', umsks=' ', ebzt=' ' and rebzg=' ')
*---- clear w/tax code
      IF   wa_bsxd-xzahl = 'X'
       AND wa_bsxd-umsks = ' '
       AND wa_bsxd-rebzt = ' '
       AND wa_bsxd-rebzg = ' '
       AND NOT wa_bsxd-augbl = wa_bsxd-belnr.
        CLEAR h_with_item-wt_withcd.
      ENDIF.

      APPEND h_with_item.
    ELSEIF h_t059z-qsatz = 0
       AND h_t059z-xqfor = 'X'.
      i_xqfor_with_item-witht = i_accit_wt-witht.
      i_xqfor_with_item-wt_withcd = i_accit_wt-wt_withcd.
      i_xqfor_with_item-wt_acco = wa_bsxd-kunnr.
      MOVE-CORRESPONDING wa_bsxd TO i_xqfor_with_item.
      APPEND i_xqfor_with_item.
    ENDIF.
  ENDLOOP.
*---- by now all data to be included in with_item are filled in
*---- h_x_with_item; however base amounts are not filled yet
*---- in h_x_with_item lines are deleted which are already
*---- in i_x_with_item.
  LOOP AT i_with_item.
    IF xchangeall = 'X'.
      IF NOT i_with_item-wt_qbshb IS INITIAL
         OR i_with_item-wt_basman = 'X'.
        DELETE h_with_item
                  WHERE witht = i_with_item-witht.
      ENDIF.
    ELSEIF xzerobase = 'X'.
      IF NOT i_with_item-wt_qsshb IS INITIAL
          OR i_with_item-wt_basman = 'X'.
        DELETE h_with_item
                  WHERE witht = i_with_item-witht.
      ENDIF.
    ELSE.
      DELETE h_with_item
                WHERE witht = i_with_item-witht.
    ENDIF.
  ENDLOOP.

*---- read data from bseg
  REFRESH l_bseg. CLEAR l_bseg.
  SELECT * FROM bseg INTO TABLE l_bseg
          WHERE bukrs = wa_bsxd-bukrs
          AND   belnr = wa_bsxd-belnr
          AND   gjahr = wa_bsxd-gjahr
          AND   buzei = wa_bsxd-buzei.

  IF wa_bsxd-xzahl = 'X'.
    CLEAR h_with_itemx.
    REFRESH h_with_itemx.
    PERFORM fi_wt_check_calculation TABLES h_with_itemx
                                     USING wa_bsxd-bukrs
                                           wa_bsxd-gjahr
                                           wa_bsxd-belnr
                                           wa_bsxd-buzei
                                           h_rc.
    IF h_rc <> 0.
      MOVE-CORRESPONDING wa_bsxd TO i_unchangeable_bseg.
      APPEND i_unchangeable_bseg.
      EXIT.
    ENDIF.

  ELSE.

*---- read data from bkpf
    REFRESH l_bkpf. CLEAR l_bkpf.
    SELECT * FROM bkpf INTO TABLE l_bkpf
            WHERE bukrs = wa_bsxd-bukrs
            AND   belnr = wa_bsxd-belnr
            AND   gjahr = wa_bsxd-gjahr.

*---- fill header of l_bkpf.
    LOOP AT l_bkpf. EXIT. ENDLOOP.

*---- read data from tax table bset
    REFRESH l_bset. CLEAR l_bset.
    SELECT * FROM bset INTO TABLE l_bset
            WHERE bukrs = wa_bsxd-bukrs
            AND   belnr = wa_bsxd-belnr
            AND   gjahr = wa_bsxd-gjahr
            AND   buzei = wa_bsxd-buzei.

  ENDIF.

  LOOP AT h_with_item.
    IF wa_bsxd-xzahl = 'X'.
      READ TABLE h_with_itemx WITH KEY buzei = wa_bsxd-buzei
                                      witht = h_with_item-witht
                                      wt_withcd = h_with_item-wt_withcd.
      IF sy-subrc = 0.
        h_with_itemx-bukrs = wa_bsxd-bukrs.
        h_with_itemx-belnr = wa_bsxd-belnr.
        h_with_itemx-gjahr = wa_bsxd-gjahr.
        h_with_itemx-buzei = wa_bsxd-buzei.
        h_with_itemx-augdt = wa_bsxd-augdt.
        h_with_itemx-augbl = wa_bsxd-augbl.
        h_with_itemx-wt_acco = wa_bsxd-kunnr.
        h_with_itemx-koart = 'K'.
        MOVE-CORRESPONDING h_with_itemx TO h_with_item.
      ENDIF.
    ELSE.
      CLEAR l_with_itemx.
      REFRESH l_with_itemx.
*---- data from bsxk
      l_with_itemx-bukrs = wa_bsxd-bukrs.
      l_with_itemx-belnr = wa_bsxd-belnr.
      l_with_itemx-gjahr = wa_bsxd-gjahr.
      l_with_itemx-buzei = wa_bsxd-buzei.
      l_with_itemx-augdt = wa_bsxd-augdt.
      l_with_itemx-augbl = wa_bsxd-augbl.
      l_with_itemx-wt_acco = wa_bsxd-kunnr.
      l_with_itemx-koart = 'K'.

*---- data w/tax type and code
      l_with_itemx-witht = h_with_item-witht.
      l_with_itemx-wt_withcd = h_with_item-wt_withcd.
      APPEND l_with_itemx.

*---- calculation of w/tax base amounts, if w/tax code exists
      IF NOT l_with_itemx-wt_withcd IS INITIAL.
        PERFORM fi_wt_calc_withholding_base(saplfwtc)
                                            TABLES l_bseg
                                                   l_bset
                                                   l_with_itemx
                                             USING l_bkpf.
      ENDIF.
      MOVE-CORRESPONDING l_with_itemx TO h_with_item.
    ENDIF.

    READ TABLE l_bseg WITH KEY
              bukrs  = h_with_item-bukrs
              belnr  = h_with_item-belnr
              gjahr  = h_with_item-gjahr
              buzei  = h_with_item-buzei.

    l_bseg-qsskz = 'XX'.
    APPEND l_bseg TO i_changed_bseg_xx.
*--- correct bseg-line with QSSKZ = 'XX'.
    CASE l_status.
      WHEN open_items.
        SELECT SINGLE  * FROM  bsid
                       WHERE  bukrs  = l_bseg-bukrs
                       AND    kunnr  = l_bseg-kunnr
                       AND    umsks  = l_bseg-umsks
                       AND    umskz  = l_bseg-umskz
                       AND    augdt  = l_bseg-augdt
                       AND    augbl  = l_bseg-augbl
                       AND    zuonr  = l_bseg-zuonr
                       AND    gjahr  = l_bseg-gjahr
                       AND    belnr  = l_bseg-belnr
                       AND    buzei  = l_bseg-buzei.
        CHECK sy-subrc = 0.
        bsid-qsskz = 'XX'.
        APPEND bsid TO i_changed_bsid_xx.

      WHEN closed_items.
        SELECT SINGLE * FROM  bsad
               WHERE  bukrs  = l_bseg-bukrs
               AND    kunnr  = l_bseg-kunnr
               AND    umsks  = l_bseg-umsks
               AND    umskz  = l_bseg-umskz
               AND    augdt  = l_bseg-augdt
               AND    augbl  = l_bseg-augbl
               AND    zuonr  = l_bseg-zuonr
               AND    gjahr  = l_bseg-gjahr
               AND    belnr  = l_bseg-belnr
               AND    buzei  = l_bseg-buzei.
        CHECK sy-subrc = 0.
        bsad-qsskz = 'XX'.
        APPEND bsad TO i_changed_bsad_xx.
    ENDCASE.
    IF h_t059p-wt_dopost IS INITIAL.
      h_with_item-wt_stat = 'X'.
    ENDIF.
    APPEND h_with_item TO h_added_with_item.
  ENDLOOP.

*---- h_added_with_item contains now all data which have to be added
*---- (or changed) to previous items!

ENDFORM.                               " customer_document_check
*---------------------------------------------------------------------*
*       FORM fi_wt_check_calculation                                  *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  i_with_item                                                   *
*  -->  i_bukrs                                                       *
*  -->  i_gjahr                                                       *
*  -->  i_belnr                                                       *
*---------------------------------------------------------------------*
FORM fi_wt_check_calculation TABLES i_with_item STRUCTURE with_itemx
                              USING i_bukrs LIKE bseg-bukrs
                                    i_gjahr LIKE bseg-gjahr
                                    i_belnr LIKE bseg-belnr
                                    i_buzei LIKE bseg-buzei
                                    i_rc LIKE sy-subrc.
*--- form to recalculate withholding tax for a document
*--- the calculated data will be given back in structure
*--- i_with_item

  TYPES: BEGIN OF wtax_trace_type,
             belnr LIKE with_itemx-belnr,
             gjahr LIKE with_itemx-gjahr,
             bukrs LIKE with_item-bukrs,
             buzei LIKE with_item-buzei,
             wt_acco LIKE with_item-wt_acco,
             witht LIKE with_item-witht,
             wt_withcd LIKE with_item-wt_withcd,
             text(8) TYPE c,
             dmbtr LIKE bseg-dmbtr,
             waers LIKE bkpf-waers,
             payment TYPE c,
          END OF wtax_trace_type.
  DATA: i_wtax_trace TYPE STANDARD TABLE OF wtax_trace_type
                     WITH HEADER LINE.
  DATA: wa_indx TYPE indx.

  DATA: i_aktyp TYPE  aktyp,
        i_dyncl TYPE  dyncl,
        i_case TYPE i,
        waers LIKE bkpf-waers,
        curtp LIKE t001a-curtp.

  DATA: l_bseg LIKE bseg OCCURS 1 WITH HEADER LINE,
        e_bseg LIKE bseg OCCURS 1 WITH HEADER LINE,
        l_bkpf LIKE bkpf OCCURS 1 WITH HEADER LINE,
        l_bset LIKE bset OCCURS 1 WITH HEADER LINE.

  DATA: x_with_item LIKE with_itemx OCCURS 1 WITH HEADER LINE,
        l_postab LIKE rfops OCCURS 1 WITH HEADER LINE,
        x_wtak LIKE wtak OCCURS 1 WITH HEADER LINE,
        x_wtad LIKE wtad OCCURS 1 WITH HEADER LINE.

  DATA: kontab_key LIKE kontab_1st,
        kontab_key1 LIKE kontab_1st OCCURS 1 WITH HEADER LINE,
        kontab_key2 LIKE kontab_1st.
  DATA: wt_kontab_id(30)    TYPE c,
        h_tfill LIKE sy-tfill.

  CLEAR i_rc.
  CLEAR i_with_item.
  REFRESH i_with_item.
*--- initialize global memory of with_item, wtak and wtad
  CALL FUNCTION 'FI_WT_DELETE_KONTAB'.
  CALL FUNCTION 'FI_WT_PUT_X_WTAK'
       TABLES
            t_wtak = x_wtak.
  CALL FUNCTION 'FI_WT_PUT_X_WTAD'
       TABLES
            t_wtad = x_wtad.
  CALL FUNCTION 'FI_WT_PUT_D_WTAK'
       TABLES
            t_wtak = x_wtak.
  CALL FUNCTION 'FI_WT_PUT_D_WTAD'
       TABLES
            t_wtad = x_wtad.


*--- read bkpf
  SELECT SINGLE * FROM  bkpf INTO l_bkpf
         WHERE  bukrs  = i_bukrs
         AND    belnr  = i_belnr
         AND    gjahr  = i_gjahr.
  APPEND l_bkpf.
  IF sy-subrc <> 0.
    i_rc = sy-subrc.
    EXIT.
  ENDIF.

*--- read bseg
  i_case = 2.
  SELECT * FROM  bseg INTO TABLE l_bseg
         WHERE  bukrs  = i_bukrs
         AND    belnr  = i_belnr
         AND    gjahr  = i_gjahr
         AND   ( koart = 'K' OR koart = 'D' )
         AND    umsks NE 'A'
         AND    umsks NE 'P'.
  SORT l_bseg BY buzei.
  READ TABLE l_bseg WITH KEY buzei = i_buzei.
  l_bseg-qsskz = sy-tabix.
  IF sy-subrc <> 0.
    i_rc = sy-subrc.
    EXIT.
  ENDIF.

  IF l_bseg-xzahl = 'X'.
    i_case = 1.
  ENDIF.

  CASE i_case.
    WHEN '1'.
*--- fb05: simulate payment
      i_aktyp = 'H'.
      i_dyncl = 'Z'.
      SELECT * FROM  with_item INTO i_with_item
             WHERE  bukrs  = i_bukrs
             AND    belnr  = i_belnr
             AND    gjahr  = i_gjahr.
        APPEND i_with_item.
      ENDSELECT.
      IF sy-subrc = 0.
        LOOP AT i_with_item.
          IF i_with_item-koart = 'K'.
            i_wtax_trace-text = 'ACCUMK'.
          ELSE.
            i_wtax_trace-text = 'ACCUMD'.
          ENDIF.
          i_wtax_trace-belnr = i_with_item-belnr.
          i_wtax_trace-gjahr = i_with_item-gjahr.
          i_wtax_trace-bukrs = i_with_item-bukrs.
          i_wtax_trace-buzei = i_with_item-buzei.
          i_wtax_trace-witht = i_with_item-witht.
          i_wtax_trace-wt_withcd = i_with_item-wt_withcd.
          i_wtax_trace-wt_acco = i_with_item-wt_acco.
          DO 4 TIMES.
            CASE sy-index.
              WHEN '1'. i_wtax_trace-dmbtr = i_with_item-wt_accbs.
              WHEN '2'. i_wtax_trace-dmbtr = i_with_item-wt_accwt.
              WHEN '3'. i_wtax_trace-dmbtr = i_with_item-wt_accwta.
              WHEN '4'. i_wtax_trace-dmbtr = i_with_item-wt_accwtha.
            ENDCASE.
            APPEND i_wtax_trace.
          ENDDO.
        ENDLOOP.
      ENDIF.

*--- export data
      wa_indx-mandt = '000'.
      wa_indx-srtfd = 'Withholding Tax'.
      wa_indx-pgmid = 'RFWT1000'.
      wa_indx-usera = sy-uname.

      EXPORT i_wtax_trace FROM i_wtax_trace
            TO SHARED BUFFER indx(wt) FROM wa_indx ID wa_indx-srtfd.

*--- get cleared items
      REFRESH postab.
      CALL FUNCTION 'GET_CLEARED_ITEMS'
           EXPORTING
                i_belnr                = i_belnr
                i_bukrs                = i_bukrs
                i_gjahr                = i_gjahr
                i_bvorg                = l_bkpf-bvorg
           TABLES
                t_items                = postab
           EXCEPTIONS
                not_found              = 1
                error_cleared_accounts = 2
                OTHERS                 = 3.

      IF sy-subrc <> 0.
        EXIT.
      ENDIF.

*--- delete some fields in order to make it possible to
*--- recalculate w/tax for these items
      LOOP AT postab where ( koart eq 'D' or koart eq 'K' ).
        MOVE-CORRESPONDING postab TO l_postab.
        IF NOT postab-belnr = postab-augbl.
          SELECT SINGLE * FROM  bseg INTO e_bseg
                 WHERE  bukrs  = postab-bukrs
                 AND    belnr  = postab-belnr
                 AND    gjahr  = postab-gjahr
                 AND    buzei  = postab-buzei.

          MOVE-CORRESPONDING e_bseg TO l_postab.
          l_postab-xaktp = 'X'.
          l_postab-xakts = 'X'.
          l_postab-konko = postab-konto.
          APPEND l_postab.
        ENDIF.
      ENDLOOP.

*--- recalculate payment
      CALL FUNCTION 'FI_WT_FB05_CALCULATE_WT'
           EXPORTING
                i_aktyp  = i_aktyp
                i_dyncl  = i_dyncl
           TABLES
                i_postab = l_postab
                i_bkpf   = l_bkpf.

      LOOP AT l_postab WHERE umsks NE 'A'
                         AND umsks NE 'P'.
        CLEAR kontab_key.
        CLEAR kontab_key1.
        CLEAR l_postab-qsznr.
        MOVE-CORRESPONDING l_postab TO kontab_key1.
        MOVE-CORRESPONDING l_postab TO kontab_key.
*--- allow COLLECT also for different MWSZ        "655513
        CLEAR kontab_key1-mwskz.                            "655513
        COLLECT kontab_key1.
        CONCATENATE sy-datum sy-uname INTO wt_kontab_id.
        CALL FUNCTION 'FI_WT_COLLECT_KONTAB'
             EXPORTING
                  i_bukrs         = l_postab-bukrs
                  i_belnr         = l_postab-belnr
                  i_buzei         = l_postab-buzei
                  i_gjahr         = l_postab-gjahr
                  i_clearing_date = l_bkpf-budat
                  i_kontab_key    = kontab_key
                  i_wt_kontab_id  = wt_kontab_id
             IMPORTING
                  e_qsskz         = l_postab-qsskz
             EXCEPTIONS
                  OTHERS          = 1.
      ENDLOOP.
*---- create WT info for payment document line item
      DESCRIBE TABLE kontab_key1 LINES h_tfill.
      DESCRIBE TABLE l_bseg.
      IF sy-tfill <> h_tfill.
        i_rc = 1.
        EXIT.
      ENDIF.
      IF sy-tfill = 1.
        kontab_key2-qsskz = '1'.
      ENDIF.
      MOVE-CORRESPONDING l_bseg TO kontab_key2.
      IF kontab_key2-shkzg = 'S'.
        kontab_key2-shkzg = 'H'.
      ELSE.
        kontab_key2-shkzg = 'S'.
      ENDIF.
      IF kontab_key2-koart = 'K'.
        kontab_key2-konko = l_bseg-lifnr.
      ELSE.
        kontab_key2-shkzg = l_bseg-kunnr.
      ENDIF.

      CALL FUNCTION 'FI_WT_PREPARE_PAYMENT'
           EXPORTING
                i_gjahr  = i_gjahr
                i_buzei  = i_buzei
                i_kontab = kontab_key2
           IMPORTING
                e_qsskz  = bseg-qsskz
           EXCEPTIONS
                OTHERS   = 1.

      CALL FUNCTION 'FI_WT_GET_P_WITH_ITEM'
           TABLES
                t_with_item = i_with_item
           EXCEPTIONS
                OTHERS      = 1.
      IF sy-subrc <> 0.
        EXIT.
      ENDIF.
      LOOP AT i_with_item WHERE buzei NE i_buzei.
        DELETE i_with_item.
      ENDLOOP.
  ENDCASE.
ENDFORM.
