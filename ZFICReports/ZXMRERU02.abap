*
* Andy
* MRER runing option should be '4' delivery
*----------------------------------------------------------------------*
*   INCLUDE ZXMRERU02                                                  *
*----------------------------------------------------------------------*
*"  IMPORTING
*"     VALUE(IS_RBKPV) TYPE  MRM_RBKPV
*"  EXPORTING
*"     REFERENCE(ES_RBKPV) TYPE  MRM_RBKPV
*"  TABLES
*  "      CT_FRSEG TYPE  MMCR_TFRSEG
*"  EXCEPTIONS
*"      ERROR_MESSAGE_RECEIVED

TABLES: lfb1, ztfi_ctl.
TABLES: likp, ekko, ekpo, konp.  ", mkpf, mseg.
DATA: l_gpres1 LIKE eine-netpr,
      l_gpres2 LIKE eine-netpr,
      l_zlspr  LIKE es_rbkpv-zlspr.

DATA: BEGIN OF it_knumh OCCURS 0,
        knumh  LIKE konh-knumh,
        datab  LIKE konh-datab, "Valid-from date
        datbi  LIKE konh-datbi,
        lifnr  LIKE lfa1-lifnr,
        kstbmt TYPE kstbmt,                                 "3 digit
        kbetr  LIKE konp-kbetr, "rate
        kpein  LIKE konp-kpein, "pricing unit
        ekorg  LIKE eine-ekorg,
        infnr  LIKE eina-infnr,
      END   OF it_knumh.

DATA: l_chk(1)     TYPE c,
      lf_tabix     LIKE  sy-tabix,
      ls_sgtxt     LIKE  rseg-sgtxt.

CHECK is_rbkpv-ivtyp = '1'.
*1  ERS
*2  ERS zero document

** Furong on 06/05/14 (
DATA:  l_kidno LIKE likp-lifex,
       l_xblnr LIKE mkpf-xblnr.
LOOP AT ct_frseg.
  lf_tabix = sy-tabix.
  SELECT SINGLE xblnr INTO l_xblnr
    FROM mkpf
    WHERE mblnr = ct_frseg-lfbnr.
  IF sy-subrc = 0.
    es_rbkpv-kidno = l_xblnr.
    SELECT SINGLE lifex INTO l_kidno
      FROM likp
      WHERE vbeln = l_xblnr.
    IF sy-subrc = 0.
      es_rbkpv-kidno = l_kidno.
      EXIT.
    ENDIF.
  ENDIF.
ENDLOOP.
** )

CLEAR ztfi_ctl.
SELECT SINGLE * FROM ztfi_ctl
   WHERE bukrs = es_rbkpv-bukrs AND categ = 'MRER' AND flag  = 'X'.

* get default house bank, first payment method and payment term.
SELECT SINGLE * FROM lfb1 WHERE lifnr = is_rbkpv-lifnr
                            AND bukrs = is_rbkpv-bukrs.
IF sy-subrc = 0.
  es_rbkpv-hbkid = lfb1-hbkid.    "house bank
  es_rbkpv-zlsch = lfb1-zwels(1). "pay method.(first one)
  es_rbkpv-zterm = lfb1-zterm.
ENDIF.

DATA: w_matnr TYPE matnr,
      w_peinh TYPE epein,
      w_netwr TYPE bwert,
      w_bstyp TYPE ebstyp.

LOOP AT ct_frseg.
  lf_tabix = sy-tabix.

** check info price = sa price
  IF ztfi_ctl-flag = 'X'.
    CLEAR l_zlspr.

    SELECT SINGLE matnr peinh netwr bstyp
       INTO (w_matnr, w_peinh, w_netwr, w_bstyp)
       FROM ekpo
       WHERE ebeln = ct_frseg-ebeln
         AND ebelp = ct_frseg-ebelp.

*    ekko-Knumv

    IF w_bstyp = 'F'. "Purchase order
      l_gpres1 = w_netwr / w_peinh.
      l_gpres2 = ct_frseg-wrbtr / w_peinh. "ct_frseg-menge.
      IF l_gpres1 <> l_gpres2.

        IF ct_frseg-ebeln(2) EQ '43'.
          l_zlspr = 'E'.          "Price Mismatch
        ENDIF.

      ENDIF.

    ELSE.              "scheduling agreement

      SELECT knumh datab lifnr ekorg
        INTO CORRESPONDING FIELDS OF TABLE it_knumh
        FROM a018
       WHERE kappl =  'M'
         AND kschl =  'PB00'
         AND matnr =  w_matnr
         AND lifnr =  is_rbkpv-lifnr
*      and ekorg =  c_ekorg
         AND esokz =  '0'
         AND datbi >=  es_rbkpv-budat   "Valid To
         AND datab <=  es_rbkpv-budat.  "Valid from

      READ TABLE it_knumh INDEX 1.

      SELECT SINGLE * FROM konp
       WHERE knumh = it_knumh-knumh
         AND kappl = 'M'
         AND kschl = 'PB00'.

      IF sy-subrc <> 0 AND NOT it_knumh-knumh IS INITIAL.
        l_zlspr = 'F'.           "Fatal error
      ELSE.
        l_gpres1 = konp-kbetr / w_peinh.
        l_gpres2 = ct_frseg-wrbtr / w_peinh. "ct_frseg-menge.
        IF l_gpres1 <> l_gpres2.
          IF ct_frseg-ebeln(2) EQ '43'.
            l_zlspr = 'E'.          "Price Mismatch
          ENDIF.
        ENDIF.
      ENDIF.

    ENDIF.

  ENDIF.

  IF ct_frseg-ebeln(2) EQ '46'.
    CLEAR l_zlspr.
  ENDIF.

  IF l_zlspr = space.
    CONCATENATE ct_frseg-lfgja '-' ct_frseg-lfbnr INTO ls_sgtxt.
  ELSE.
    CONCATENATE ct_frseg-lfgja '-' ct_frseg-lfbnr '(' l_zlspr ')'
           INTO ls_sgtxt.
  ENDIF.

* move back to lineitem text
  ct_frseg-sgtxt  = ls_sgtxt.
  MODIFY ct_frseg  FROM ct_frseg INDEX lf_tabix.

  IF l_zlspr <> space.
    es_rbkpv-zlspr = l_zlspr.
  ENDIF.
ENDLOOP.

*TABLES: ztfi_ctl.
*SELECT SINGLE * FROM ztfi_ctl where bukrs = 'H201'.

* Get Information ASN#
*DATA: lf_tabix     LIKE  sy-tabix,
*      ls_rseg      TYPE  mmcr_tfrseg,
*      ls_borgr     LIKE  likp-borgr_grp,
*      ls_lifex     LIKE  likp-lifex,
*      ls_lfdat     LIKE  likp-lfdat,
*      ls_fkdat     LIKE  likp-fkdat,
*      ls_verur     LIKE  likp-verur,
*      ls_grund     LIKE  mseg-grund,
*      ls_xblnr     LIKE  mkpf-xblnr,
*      ls_vbeln     LIKE  likp-vbeln,
*      ls_lfimg     LIKE  lips-lfimg,
*      ls_bwart     LIKE  mseg-bwart,
*      ls_sgtxt     LIKE  rseg-sgtxt,
*      ls_bsart     LIKE  ekko-bsart,
*      ls_rc(1)     TYPE c,
*      ls_diffd     TYPE i.


*  CLEAR: ls_borgr, ls_xblnr, ls_rc, ls_sgtxt, ls_lfimg.

** check po type
*  SELECT SINGLE bsart INTO ls_bsart FROM ekko
*    WHERE ebeln = ct_frseg-ebeln.
*
*  SELECT SINGLE xblnr       FROM mkpf  INTO ls_xblnr
*    WHERE mblnr = ct_frseg-lfbnr
*      AND mjahr = ct_frseg-lfgja.
*
**...JIS
*  IF ls_xblnr(6) = 'JIS-GR' OR ls_bsart = 'JIS'.
*    ls_sgtxt  = ls_xblnr.
*
**...JIT/Manual (No inbound delivery)
*  ELSEIF ls_xblnr IS INITIAL.
*    ls_rc     = '1'.
*    ls_sgtxt = ct_frseg-lfbnr.
*  ELSE.
*    ls_vbeln  = ls_xblnr(10).
*    SELECT SINGLE borgr_grp lifex lfdat fkdat
*      INTO (ls_borgr, ls_lifex, ls_lfdat, ls_fkdat)
*      FROM likp  WHERE vbeln = ls_vbeln.
*
**...Inbound delivery found
*    IF sy-subrc = 0.
*      ls_diffd = abs( ls_lfdat - ls_fkdat ).
**...ASN# or External identification of delivery note missing
*      IF ls_borgr = space OR ls_lifex = space.
*        ls_rc = '1'.
*        CONCATENATE ct_frseg-lfbnr ';' ls_xblnr ';#'
*               INTO ls_sgtxt.
*      ELSEIF ls_diffd > ztfi_ctl-ersday.     "check tolerance
*        ls_rc = '2'.  "Date Check
*        CONCATENATE ct_frseg-lfbnr ';' ls_borgr INTO ls_sgtxt.
*      ELSE.
*        CONCATENATE ct_frseg-lfbnr ';' ls_borgr INTO ls_sgtxt.
*      ENDIF.
**...No inbound delivery
*    ELSE.
*      ls_rc = '1'.
*      CONCATENATE ct_frseg-lfbnr ';' ls_xblnr ';#'
*             INTO ls_sgtxt.
*    ENDIF.
*  ENDIF.

* message always red
*        message s001(zfi) with
*          'Supplier shipping number is missing'.
*        raise error_message_received.
* search reason code...
*  select single grund into ls_grund from mseg
*     where mblnr = ct_frseg-LFBNR
*       and MJAHR = ct_frseg-LFGJA
*       and ZEILE = ct_frseg-lfpos.

*-------check qty....start
*        if is_rbkpv-xrechl = 'S'.
*          clear ls_lfimg.
*          select single lfimg into ls_lfimg  from lips
**           where vbeln = ls_xblnr and posnr = ct_frseg-lfpos.
*           where vbeln = ls_xblnr
*             and matnr = ct_frseg-matnr.
*          if ct_frseg-menge <> ls_lfimg.
*            ls_rc = '2'.   "Qty Diff
*            concatenate ct_frseg-lfbnr ';' ls_borgr ';qty chk'
*                   into ls_sgtxt.
*          endif.
*        endif.
*-------check qty....end
