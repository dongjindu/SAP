*&---------------------------------------------------------------------*
*& Report  ZPSFC174                                                    *
*&---------------------------------------------------------------------*
*& This report corrects negative values for the following fields       *
*& in process order / network / maintenance order / production order   *
*& reservations or product cost collector requirements.                *
*& BDMNG - Requirement Quantity                                        *
*& ERFMG - Quantity in unit of entry                                   *
*& VMENG - Confirmed quantity for availability check in SKU            *
*& ROMEN - Quantity of variable-size item                              *
*& ESMNG - Usage quantity per order unit of measure                    *
*& This report is related to report PPSFC106 (see note 406522), but    *
*& works for all reservations, not only co-products and by products.   *
*&---------------------------------------------------------------------*
*& Version:      1.10                                                  *
*& Author:       SAP                                                   *
*& Date:         31.01.2005                                            *
*& Last Changed: 14.03.2005                                            *
*&---------------------------------------------------------------------*

** Modified by Haseeb Mohammad on 07-27-2006, on request of Kevin Able**
** to use aufnr for selecting from RESB table in place of rsnum       **
** Helpdesk 67S9512427 and Transport number UD1K921519

REPORT  zpsfc174.

INCLUDE lcokotyp.

TABLES: caufv, resb.

DATA: BEGIN OF lt_orders OCCURS 0,
        aufnr LIKE caufv-aufnr,
        rsnum LIKE caufv-rsnum,
      END OF lt_orders.
DATA: BEGIN OF ls_resb OCCURS 0,
        rsnum LIKE resb-rsnum,
        rspos LIKE resb-rspos,
        rsart LIKE resb-rsart,
        aufnr LIKE resb-aufnr,
        bdmng LIKE resb-bdmng,
        erfmg LIKE resb-erfmg,
        vmeng LIKE resb-vmeng,
        romen LIKE resb-romen,
        esmng LIKE resb-esmng,
      END OF ls_resb.
DATA: lt_resb LIKE TABLE OF ls_resb.

SELECTION-SCREEN COMMENT /1(80) text1.
SELECTION-SCREEN COMMENT /1(80) text2.
SELECTION-SCREEN COMMENT /1(80) text3.
SELECTION-SCREEN ULINE.
SELECTION-SCREEN COMMENT /1(80) text_a.
SELECTION-SCREEN COMMENT /1(80) text_b.
SELECT-OPTIONS r_orders FOR caufv-aufnr.
SELECTION-SCREEN COMMENT /1(80) text_c.
SELECTION-SCREEN COMMENT /1(80) text_d.
SELECTION-SCREEN ULINE.
SELECTION-SCREEN BEGIN OF BLOCK block WITH FRAME TITLE title.
SELECTION-SCREEN COMMENT /1(72) text_e.
SELECTION-SCREEN BEGIN OF BLOCK block2 WITH FRAME.
PARAMETERS: testmode RADIOBUTTON GROUP mode DEFAULT 'X'.
PARAMETERS: update RADIOBUTTON GROUP mode.
SELECTION-SCREEN END OF BLOCK block2.
SELECTION-SCREEN COMMENT /1(72) text_f.
SELECTION-SCREEN END OF BLOCK block.

INITIALIZATION.

  text1  = 'This report corrects negative values for quantity fields'.
  text2  = 'in production order, network, maintenance order, process'.
  text3  = 'order reservations or product cost collector requirements.'.
  text_a = 'Please specify order numbers of orders'.
  text_b = '[or networks, product cost collectors] to be processed.'.
  text_c = 'With initial number range, all orders will be selected!'.
  text_d = 'Please consider that this might result in a long runtime!'.
  text_e = 'If TESTMODE is set, no database update will occur.'.
  text_f = 'Set radio button UPDATE for updating the database entries.'.
  title  = 'Mode'.

START-OF-SELECTION.

* Read production orders / process orders
  SELECT aufnr rsnum FROM caufv
                     INTO TABLE lt_orders
                     WHERE aufnr IN r_orders
                     AND ( autyp = auftragstyp-corp OR
                           autyp = auftragstyp-fert OR
                           autyp = auftragstyp-netw OR
                           autyp = auftragstyp-inst OR
                           autyp = auftragstyp-bord ).

  IF lt_orders[] IS INITIAL.
    WRITE: /'No orders/networks/cost collectors in the given range.'.
    EXIT.
  ENDIF.

* Read reservations with negative quantity fields
  SELECT rsnum rspos rsart aufnr bdmng
         erfmg vmeng romen esmng FROM resb
                                 INTO TABLE lt_resb
                                 FOR ALL ENTRIES IN lt_orders
**Modified by Haseeb Mohammad on 07-27-2006, Helpdesk 67S9512427
**to use aufnr for selecting from RESB table in place of rsnum
**Initiated by Able Kevin.  Transport no: UD1K921519

**                               WHERE rsnum = lt_orders-rsnum AND
                                 WHERE aufnr = lt_orders-aufnr AND
** End of change by Haseeb Mohammad on 07-27-2006,
** Transport number UD1K921519

                                 ( bdmng < 0 OR
                                   erfmg < 0 OR
                                   vmeng < 0 OR
                                   romen < 0 OR
                                   esmng < 0 ).

  IF lt_resb[] IS INITIAL.
    WRITE: /'No inconsistent components found.'.
    EXIT.
  ENDIF.

* Correct the values of quantity fields
  LOOP AT lt_resb INTO ls_resb.
    IF ls_resb-bdmng < 0.
      ls_resb-bdmng = ls_resb-bdmng * ( -1 ).
    ENDIF.
    IF ls_resb-erfmg < 0.
      ls_resb-erfmg = ls_resb-erfmg * ( -1 ).
    ENDIF.
    IF ls_resb-vmeng < 0.
      ls_resb-vmeng = 0.
    ENDIF.
    IF ls_resb-romen < 0.
      ls_resb-romen = ls_resb-romen * ( -1 ).
    ENDIF.
    IF ls_resb-esmng < 0.
      ls_resb-esmng = ls_resb-esmng * ( -1 ).
    ENDIF.
    MODIFY lt_resb FROM ls_resb.
    WRITE: / 'Reservation',
             ls_resb-rsnum, ls_resb-rspos,
             'of order', ls_resb-aufnr,
             'is', 'inconsistent' COLOR COL_NEGATIVE.
  ENDLOOP.

* Update database, if requested
  IF testmode IS INITIAL.
    LOOP AT lt_resb INTO ls_resb.
      UPDATE resb SET bdmng = ls_resb-bdmng
                      erfmg = ls_resb-erfmg
                      vmeng = ls_resb-vmeng
                      romen = ls_resb-romen
                      esmng = ls_resb-esmng
                  WHERE rsnum = ls_resb-rsnum
                  AND   rspos = ls_resb-rspos
                  AND   rsart = ls_resb-rsart.
    ENDLOOP.
    ULINE.
    WRITE: /'Reservations have been updated!' COLOR COL_POSITIVE.
    ULINE.
  ENDIF.
