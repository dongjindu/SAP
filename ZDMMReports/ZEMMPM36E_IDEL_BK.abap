************************************************************************
* Program Name      : ZEMMPM36E_IDEL
* Author            : Sung-Tae, Lim
* Creation Date     : 2003.10.24.
* Specifications By : Sung-Tae, Lim
* Development Request No : UD1K902658
* Addl Documentation:
* Description       : Inbound Delivery Create - KDWeb ASN
*
* Modification Logs
* Date            Developer        RequestNo      Description
* 2003.10.24.     Sung-Tae Lim     UD1K902658     Initial Coding
* 03/17/2005      Shiva            UD1K915000     Update error message
*                                                 if error occurs while
*                                                 creating delivery.
* 03/17/2005      Shiva            UD1K915032     Update error message
*                                                 if error occurs while
*                                               reading PO information.
* 03/21/2005      Shiva            UD1K915099   Type conflict for BAPI
*                                               reading error message.
************************************************************************

REPORT zemmpm36e_idel NO STANDARD PAGE HEADING
                      LINE-SIZE 132
                      LINE-COUNT 64(1)
                      MESSAGE-ID zmmm.

**---
INCLUDE : zrmmpmxxr_incl.


**--- Tables, Views & Structures
DATA : BEGIN OF it_itab OCCURS 0.
        INCLUDE STRUCTURE ztmm_kd_asn.
DATA :   ebelp LIKE ekpo-ebelp,
         linecolor(4),     " ALV Color
       END OF it_itab.

DATA: BEGIN OF it_ekpo_short OCCURS 0,
        ebeln LIKE ekpo-ebeln,
        ebelp LIKE ekpo-ebelp,
        werks LIKE ekpo-werks,
        lgort LIKE ekpo-lgort,
        menge LIKE ekpo-menge,
        meins LIKE ekpo-meins,
        lmein LIKE ekpo-lmein,
        umrez LIKE ekpo-umrez,
        umren LIKE ekpo-umren,
        matnr LIKE ekpo-matnr,
        ematn LIKE ekpo-ematn,                              "386409
        mfrnr LIKE ekpo-mfrnr,
        mfrpn LIKE ekpo-mfrpn,
        emnfr LIKE ekpo-emnfr,
        cuobj LIKE ekpo-cuobj,
        uebto LIKE ekpo-uebto,
        untto LIKE ekpo-untto,
        uebtk LIKE ekpo-uebtk,
        bwtar LIKE ekpo-bwtar,
        idnlf LIKE ekpo-idnlf,
        txz01 LIKE ekpo-txz01,
        mfrgr LIKE ekpo-mfrgr,
        gewei LIKE ekpo-gewei,
        voleh LIKE ekpo-voleh,
        ntgew LIKE ekpo-ntgew,
        brgew LIKE ekpo-brgew,
        volum LIKE ekpo-volum,
        ean11 LIKE ekpo-ean11,
        aktnr LIKE ekpo-aktnr,
        abeln LIKE ekpo-abeln,
        abelp LIKE ekpo-abelp,
        aurel LIKE ekpo-aurel,
        matkl LIKE ekpo-matkl,
        upvor LIKE ekpo-upvor,
        uptyp LIKE ekpo-uptyp,
        uebpo LIKE ekpo-uebpo,
        bstae LIKE ekpo-bstae,
        wepos LIKE ekpo-wepos,
        anlmg LIKE ekpo-menge,
        insmk LIKE ekpo-insmk,
        pstyp LIKE ekpo-pstyp,
        sobkz LIKE ekpo-sobkz,
        kzvbr LIKE ekpo-kzvbr,            "note 384051
        knttp LIKE ekpo-knttp,
        kzfme LIKE ekpo-kzfme,
      END OF it_ekpo_short.

DATA : st_vbsk LIKE vbsk.

DATA : it_komdlgn LIKE komdlgn OCCURS 0 WITH HEADER LINE,
       it_vbfs    LIKE vbfs    OCCURS 0 WITH HEADER LINE,
       it_vbls    LIKE vbls    OCCURS 0 WITH HEADER LINE,
       it_lips    LIKE lips    OCCURS 0 WITH HEADER LINE.

DATA : it_ekkn LIKE ekkn OCCURS 0 WITH HEADER LINE.

DATA : wa_itab LIKE it_itab.

DATA : it_xeket LIKE beket OCCURS 0 WITH HEADER LINE.


*----- BDC
DATA : BEGIN OF it_bdc OCCURS 0.
        INCLUDE STRUCTURE bdcdata.
DATA : END OF it_bdc.

DATA : BEGIN OF it_mess OCCURS 0.
        INCLUDE STRUCTURE bdcmsgcoll.
DATA : END OF it_mess.

DATA : it_message LIKE it_mess OCCURS 0 WITH HEADER LINE.


**--- Variables
DATA : w_mode LIKE ctu_params-dismode VALUE 'A'.

DATA:      gf_dlv_type LIKE likp-lfart. "Delivery Type
DATA:      gf_ebtyp    LIKE t163d-ebtyp.                    "386409

DATA : w_qty_sum LIKE it_komdlgn-lfimg.

CONSTANTS : c_ibtyp LIKE t163d-ibtyp VALUE '2'.

**--- Macro
DEFINE append_fieldcat.
  &1 = &1 + 1.
  w_fieldcat-col_pos    = &1.
  w_fieldcat-fieldname  = &2.
  w_fieldcat-outputlen  = &3.
  w_fieldcat-seltext_l  = &4.
  w_fieldcat-seltext_m  = &4.
  w_fieldcat-seltext_s  = &4.
  w_fieldcat-datatype   = &5.
  w_fieldcat-key        = &6.
  w_fieldcat-qfieldname = &7.
  w_fieldcat-cfieldname = &8.
  append w_fieldcat.
  clear : w_fieldcat.
END-OF-DEFINITION.

DEFINE append_sortcat.
  w_sortcat-spos      = &1.
  w_sortcat-fieldname = &2.
  w_sortcat-tabname   = &3.
  w_sortcat-up        = &4.
  w_sortcat-subtot    = &5.
  append w_sortcat.
  clear : w_sortcat.
END-OF-DEFINITION.


**---
SELECTION-SCREEN BEGIN OF BLOCK block1 WITH FRAME TITLE text-001.
SELECT-OPTIONS : s_zslno FOR ztmm_kd_asn_main-zslno OBLIGATORY
                             NO INTERVALS NO-EXTENSION.
SELECTION-SCREEN SKIP.
PARAMETER:       p_update AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK block1.

SELECTION-SCREEN BEGIN OF BLOCK block2 WITH FRAME TITLE text-002.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 5.
PARAMETERS       r1 RADIOBUTTON GROUP radi DEFAULT 'X'.
SELECTION-SCREEN POSITION 7.
SELECTION-SCREEN COMMENT 8(10) text-003 FOR FIELD r1.
SELECTION-SCREEN POSITION 25.
PARAMETERS       r2 RADIOBUTTON GROUP radi.
SELECTION-SCREEN POSITION 26.
SELECTION-SCREEN COMMENT 27(25) text-004 FOR FIELD r2.
SELECTION-SCREEN POSITION 55.
PARAMETERS       r3 RADIOBUTTON GROUP radi.
SELECTION-SCREEN POSITION 56.
SELECTION-SCREEN COMMENT 57(10) text-005 FOR FIELD r3.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK block2.

PARAMETERS : p_mode LIKE ctu_params-dismode DEFAULT 'N'.


**---
INITIALIZATION.
  PERFORM event_build USING w_eventcat[].


**---
TOP-OF-PAGE.
  PERFORM top_of_page.


**---
START-OF-SELECTION.
  PERFORM get_data.


**---
END-OF-SELECTION.
  IF it_itab[] IS INITIAL.
    MESSAGE s999 WITH text-m01.
  ELSE.
    IF p_update EQ space.
      PERFORM create_delivery.
      PERFORM update_table.
    ENDIF.
    PERFORM comment_build.     " USING w_top_of_page[].
    PERFORM make_alv_grid.
  ENDIF.


**---





*&---------------------------------------------------------------------*
*&      Form  get_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data.
*---
  CLEAR : it_itab, it_itab[].

*--- Processing
  IF r1 NE space.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE it_itab
             FROM ztmm_kd_asn_main
            WHERE zslno IN s_zslno
              AND zresult EQ space.
  ENDIF.

*--- Re-Processing(Error)
  IF r2 NE space.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE it_itab
             FROM ztmm_kd_asn_main
            WHERE zslno IN s_zslno
              AND zresult EQ 'E'.
  ENDIF.

*--- Success
  IF r3 NE space.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE it_itab
             FROM ztmm_kd_asn_main
            WHERE zslno IN s_zslno
              AND zresult EQ 'S'.
  ENDIF.
ENDFORM.                    " get_data

*&---------------------------------------------------------------------*
*&      Form  create_delivery
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_delivery.
*--- sort by Container No.(traid) & Case No.(ematn)
  SORT it_itab BY traid ematn.

  LOOP AT it_itab.
    CLEAR : wa_itab.
    MOVE : it_itab TO wa_itab.
    AT NEW ematn.     " Case Number
      CLEAR : it_komdlgn, it_komdlgn[].
    ENDAT.
*    PERFORM vbsk_fill.
    PERFORM get_inbound_delivery_type USING '2'.
    PERFORM it_ekpo_short_fill.
    PERFORM komdlgn_fill.
    AT END OF ematn.     " Case Number
      PERFORM call_function.
      PERFORM modify_itab.
    ENDAT.
  ENDLOOP.
ENDFORM.                    " create_delivery

*&---------------------------------------------------------------------*
*&      Form  vbsk_fill
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM vbsk_fill.
*---
  CLEAR : st_vbsk.

  MOVE : sy-mandt TO st_vbsk-mandt,
         sy-uname TO st_vbsk-ernam,
         sy-datum TO st_vbsk-erdat,
         sy-uzeit TO st_vbsk-uzeit,
         'L'      TO st_vbsk-smart.

*- Nummer VBSK vergeben -----------------------------------------------*
  SELECT SINGLE * FROM tvsa WHERE smart = st_vbsk-smart.

  IF sy-subrc EQ 0.
    CALL FUNCTION 'NUMBER_GET_NEXT'
         EXPORTING
              nr_range_nr = tvsa-numki
              object      = 'RV_SAMMG'
         IMPORTING
              number      = st_vbsk-sammg
         EXCEPTIONS
              OTHERS      = 01.
    IF sy-subrc NE 0.
      MESSAGE e700(me) WITH tvsa-numki text-007.
    ENDIF.
  ELSE.
    MESSAGE e700(me) WITH space text-007.
  ENDIF.
ENDFORM.                    " vbsk_fill

*&---------------------------------------------------------------------*
*&      Form  komdlgn_fill
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM komdlgn_fill.
*---
  LOOP AT it_ekpo_short.
    STATICS : h_grkor LIKE lips-grkor,      "Liefergruppe
              h_bsmng LIKE ekpo-menge.         "

    PERFORM read_ekko USING it_ekpo_short-ebeln.

*---
    IF NOT ekko-lifnr IS INITIAL.
      CALL FUNCTION 'VENDOR_MASTER_DATA_SELECT_12'
           EXPORTING
                pi_lifnr       = ekko-lifnr
                pi_ekorg       = ekko-ekorg
           IMPORTING
                pe_lfm1        = lfm1
           EXCEPTIONS
                no_entry_found = 1
                OTHERS         = 2.

      it_komdlgn-vsbed = lfm1-vsbed.     " Shipping conditions
    ELSE.
      CLEAR : it_komdlgn-vsbed.
    ENDIF.

*---
    it_komdlgn-lifnr = ekko-lifnr.
    it_komdlgn-inco1 = ekko-inco1.                          "363954
    it_komdlgn-inco2 = ekko-inco2.                          "363954
    it_komdlgn-exnum = ekko-exnum.                          "363954
    it_komdlgn-bukrs_best = ekko-bukrs.                     "363954

*---
    it_komdlgn-matnr   = it_ekpo_short-matnr.
    it_komdlgn-werks   = it_ekpo_short-werks.
    it_komdlgn-lgort   = it_ekpo_short-lgort.
*    xkomdlgn-charg     = ?
    it_komdlgn-vrkme   = it_ekpo_short-meins.
    it_komdlgn-meins   = it_ekpo_short-lmein.
    it_komdlgn-umvkz   = it_ekpo_short-umrez.
    it_komdlgn-umvkn   = it_ekpo_short-umren.

*---
    IF it_ekpo_short-matnr EQ space.                        "386409
      it_komdlgn-meins = it_ekpo_short-meins.
      it_komdlgn-umvkz = 1.
      it_komdlgn-umvkn = 1.
    ENDIF.

*---
    it_komdlgn-insmk = it_ekpo_short-insmk.
    it_komdlgn-kzfme = it_ekpo_short-kzfme.
    it_komdlgn-kzvbr = it_ekpo_short-kzvbr.           "note 384051

*--- get open qty
    CLEAR : it_xeket, it_xeket[], w_qty_sum.

    PERFORM get_open_qty USING it_ekpo_short-ebeln
                               it_ekpo_short-ebelp
*--- 2004/02/17 block by stlim
*                               wa_itab-zcreate.
*--- 2004/02/17
*--- 2004/02/17 add by stlim
*                               wa_itab-lfdat_la.
                               wa_itab-eta.
*--- 2004/02/17

*---
*    it_komdlgn-lfimg = wa_itab-lfimg.     "p_open_qty.

**--- block & insert by stlim (2004/05/13)
    IF wa_itab-lfimg EQ w_qty_sum.     " I/D qty = open qty
      MOVE : w_qty_sum TO it_komdlgn-lfimg.
    ELSEIF wa_itab-lfimg GT w_qty_sum.     " I/D qty > open qty
*      MOVE : w_qty_sum TO it_komdlgn-lfimg.
*
      MOVE : text-m03      TO it_itab-zmsg,
             'E'           TO it_itab-zresult,
             c_red         TO it_itab-linecolor.
*---
      MOVE : sy-datum TO it_itab-zbdat,     " BDC Execute Date
             sy-uzeit TO it_itab-zbtim,     " BDC Execute Time
             sy-uname TO it_itab-zbnam,     " User ID
             'C'      TO it_itab-zmode.     " Data Characteristic Flag

      MODIFY it_itab.
      EXIT.
    ELSEIF wa_itab-lfimg LT w_qty_sum.     " I/D qty < open qty
      MOVE : wa_itab-lfimg TO it_komdlgn-lfimg.
    ENDIF.
*    it_komdlgn-lfimg = w_qty_sum.     " open qty
**--- end of block & insert

*--- 2004/02/17 block by stlim
*    it_komdlgn-lfdat = wa_itab-zcreate.     "p_eindt.
*--- 2004/02/17
*--- 2004/02/17 add by stlim
*    it_komdlgn-lfdat = wa_itab-lfdat_la.

**--- blocked by stlim (2004/05/11)
    it_komdlgn-lfdat = wa_itab-eta.
    IF it_komdlgn-lfdat IS INITIAL.
      CLEAR : eket.
      SELECT SINGLE eindt INTO it_komdlgn-lfdat
                          FROM eket
                         WHERE ebeln EQ it_ekpo_short-ebeln
                           AND ebelp EQ it_ekpo_short-ebelp.
    ENDIF.
**--- end of block

**--- insert by stlim (2004/05/11)
*    CLEAR : eket.
*    SELECT SINGLE eindt INTO it_komdlgn-lfdat
*                        FROM eket
*                       WHERE ebeln EQ it_ekpo_short-ebeln
*                         AND ebelp EQ it_ekpo_short-ebelp.
**--- end of insert

*---
*    it_komdlgn-lfuhr = p_uzeit.
*    xkomdlgn-vstel = ?
*    xkomdlgn-vkorg = ?
*    xkomdlgn-vtweg = ?
*    xkomdlgn-spart = ?
    it_komdlgn-vgbel = it_ekpo_short-ebeln.
    it_komdlgn-vgpos = it_ekpo_short-ebelp.
    it_komdlgn-lfart = gf_dlv_type.
    it_komdlgn-vgtyp = 'V'.
    it_komdlgn-kzazu = 'X'.                "??? what's that for ????
    it_komdlgn-knttp = it_ekpo_short-knttp.
    it_komdlgn-sobkz = it_ekpo_short-sobkz.

*--- note 386409:
    SELECT * FROM t163g WHERE bstae EQ it_ekpo_short-bstae
                          AND ebtyp EQ gf_ebtyp.
      EXIT.
    ENDSELECT.
    IF sy-subrc = 0.
* Prüfen, ob Lieferavis WE-Zuordnung hat (Vorauss. für WE über VL32)
* und wepos prüfen
      IF t163g-wezuo EQ space OR it_ekpo_short-wepos EQ space.
        it_komdlgn-nowab = 'X'.
      ELSE.
        CLEAR it_komdlgn-nowab.
      ENDIF.
    ENDIF.

*---
    IF it_ekpo_short-matnr IS INITIAL OR it_ekpo_short-pstyp = '6'.
      it_komdlgn-posar = 'B'.
    ENDIF.

*---
    it_komdlgn-ematn = it_ekpo_short-ematn.
    it_komdlgn-mfrnr = it_ekpo_short-mfrnr.
    it_komdlgn-mfrpn = it_ekpo_short-mfrpn.
    it_komdlgn-emnfr = it_ekpo_short-emnfr.
    it_komdlgn-cuobj = it_ekpo_short-cuobj.
    it_komdlgn-uebto = it_ekpo_short-uebto.
    it_komdlgn-untto = it_ekpo_short-untto.
    it_komdlgn-uebtk = it_ekpo_short-uebtk.
*    it_komdlgn-lichn = p_licha.     " vendor batch number
*    it_komdlgn-charg = p_charg.
    it_komdlgn-bwtar = it_ekpo_short-bwtar.

*---
*    it_komdlgn-kdmat = it_ekpo_short-idnlf.
*    MOVE : wa_itab-ematn TO it_komdlgn-kdmat.
*    CONCATENATE wa_itab-ematn wa_itab-zzkdwebpo INTO
*                              it_komdlgn-kdmat.
*    CONCATENATE wa_itab-ebeln wa_itab-ematn INTO
*                              it_komdlgn-kdmat.
    CONCATENATE wa_itab-zzkdwebpo wa_itab-ematn INTO
                              it_komdlgn-kdmat.
*---

    it_komdlgn-arktx = it_ekpo_short-txz01.
    it_komdlgn-mfrgr = it_ekpo_short-mfrgr.
    it_komdlgn-gewei = it_ekpo_short-gewei.
    it_komdlgn-voleh = it_ekpo_short-voleh.
    it_komdlgn-ntgew = it_ekpo_short-ntgew * it_komdlgn-lfimg.
    it_komdlgn-brgew = it_ekpo_short-brgew * it_komdlgn-lfimg.
    it_komdlgn-volum = it_ekpo_short-volum * it_komdlgn-lfimg.
    it_komdlgn-ean11 = it_ekpo_short-ean11.
*    it_komdlgn-podrel = t163l-podrel.
    it_komdlgn-aktnr = it_ekpo_short-aktnr.
    it_komdlgn-abeln = it_ekpo_short-abeln.
    it_komdlgn-abelp = it_ekpo_short-abelp.
* xkomdlgn-ltssf = only sort criteria in vl31n
    it_komdlgn-aurel = it_ekpo_short-aurel.

*---
*    it_komdlgn-idnlf = it_ekpo_short-idnlf.
    MOVE : wa_itab-ematn TO it_komdlgn-idnlf.
*---

    it_komdlgn-matkl = it_ekpo_short-matkl.

*---
    CLEAR it_komdlgn-grkor.
    CLEAR it_komdlgn-kmpmg.
    CLEAR it_komdlgn-uepos.
    CLEAR it_komdlgn-uepvw.                                 "549736

*---
    IF it_ekpo_short-upvor CA '3X'.
      h_grkor = h_grkor + 1.
      it_komdlgn-grkor = h_grkor.
      h_bsmng = it_ekpo_short-menge.
    ENDIF.

    IF NOT it_ekpo_short-uebpo IS INITIAL AND
           it_ekpo_short-uptyp CA '3X'.
      it_komdlgn-uepvw = 'G'.                               "549736
      it_komdlgn-uepos = it_ekpo_short-uebpo.
      it_komdlgn-grkor = h_grkor.
      IF h_bsmng NE 0.
        it_komdlgn-kmpmg = it_ekpo_short-menge / h_bsmng.
      ENDIF.
    ENDIF.

*---
    IF it_ekpo_short-pstyp EQ '2'.
      it_komdlgn-sobkz = 'K'.
    ENDIF.
* Kontierungsfelder
    IF it_ekpo_short-sobkz EQ 'E' OR it_ekpo_short-sobkz EQ 'Q'.
      CALL FUNCTION 'MMPUR_EKKN_READ_EBELN_EBELP'
           EXPORTING
                pi_ebeln             = it_ekpo_short-ebeln
                pi_ebelp             = it_ekpo_short-ebelp
           TABLES
                pto_ekkn_po          = it_ekkn
           EXCEPTIONS
                no_records_requested = 1
                OTHERS               = 2.

      IF sy-subrc EQ 0.
        READ TABLE it_ekkn INDEX 1.
        it_komdlgn-ps_psp_pnr = it_ekkn-ps_psp_pnr.
        it_komdlgn-vbelv      = it_ekkn-vbeln.
        it_komdlgn-posnv      = it_ekkn-vbelp.
      ENDIF.
    ENDIF.

*--- others
    MOVE : '0005'        TO it_komdlgn-traty,
           wa_itab-traid TO it_komdlgn-traid,
           wa_itab-lifexpos TO it_komdlgn-lifexpos.

    MOVE : wa_itab-zinvoice TO it_komdlgn-bolnr.

*--- 2004/02/17 add by stlim
*--- add seal number
    MOVE : wa_itab-borgr_grp TO it_komdlgn-borgr_grp.
*---
    APPEND it_komdlgn.
    CLEAR : it_komdlgn.
  ENDLOOP.
ENDFORM.                    " komdlgn_fill

*&---------------------------------------------------------------------*
*&      Form  call_function
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM call_function.
*---
  CLEAR : it_vbfs, it_vbfs[], it_vbls, it_vbls[], it_lips, it_lips[].

  CALL FUNCTION 'GN_DELIVERY_CREATE'
       EXPORTING
            vbsk_i   = st_vbsk
       TABLES
            xkomdlgn = it_komdlgn
            xvbfs    = it_vbfs
            xvbls    = it_vbls
            xxlips   = it_lips.
ENDFORM.                    " call_function

*&---------------------------------------------------------------------*
*&      Form  read_ekko
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_ITAB_EBELN  text
*----------------------------------------------------------------------*
FORM read_ekko USING    p_it_itab_ebeln.
*---
  CALL FUNCTION 'ME_EKKO_SINGLE_READ'
       EXPORTING
            pi_ebeln         = p_it_itab_ebeln
       IMPORTING
            po_ekko          = ekko
       EXCEPTIONS
            no_records_found = 1
            OTHERS           = 2.
ENDFORM.                    " read_ekko

*&---------------------------------------------------------------------*
*&      Form  it_ekpo_short_fill
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM it_ekpo_short_fill.
*---
  CLEAR : it_ekpo_short, it_ekpo_short[].

  SELECT
         ebeln ebelp menge meins matnr werks lgort bstae loekz elikz
         lmein umrez umren insmk pstyp sobkz knttp kzfme kzvbr"384051
         ematn mfrnr mfrpn emnfr cuobj uebto untto uebtk bwtar idnlf
         txz01 mfrgr gewei voleh ntgew brgew volum ean11 aktnr abeln
         abelp aurel matkl upvor uptyp uebpo wepos          "386409
          INTO CORRESPONDING FIELDS OF TABLE it_ekpo_short
          FROM ekpo
           WHERE ebeln EQ wa_itab-ebeln
*             AND ebelp EQ wa_itab-ebelp
             AND matnr EQ wa_itab-matnr
*             AND werks IN s_werks
*             and matnr ne space                                 "386409
*             AND lgort IN s_lgort
*             AND bstae IN r_bstae
             AND loekz EQ space
             AND elikz EQ space
             AND retpo EQ space                             "327089
           ORDER BY ebeln ebelp.
  IF sy-subrc NE 0.
    CONCATENATE text-008 wa_itab-matnr INTO it_itab-zmsg.
    MOVE : 'E'           TO it_itab-zresult,
           c_red         TO it_itab-linecolor.
*---
    MOVE : sy-datum TO it_itab-zbdat,     " BDC Execute Date
           sy-uzeit TO it_itab-zbtim,     " BDC Execute Time
           sy-uname TO it_itab-zbnam,     " User ID
           'C'      TO it_itab-zmode.     " Data Characteristic Flag
*---
    MODIFY it_itab TRANSPORTING zbdat
                                zbtim
                                zbnam
                                zmode
                                zresult
                                zmsg
                                linecolor
                                         WHERE traid EQ wa_itab-traid
                                           AND ematn EQ wa_itab-ematn.
  ENDIF.

ENDFORM.                    " it_ekpo_short_fill

*&---------------------------------------------------------------------*
*&      Form  get_inbound_delivery_type
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0527   text
*----------------------------------------------------------------------*
FORM get_inbound_delivery_type USING    besttyp.
*--- get inbound delivery type from T163G
  DATA: h_ibtyp LIKE t163d-ibtyp.
* Lieferart für Grob-WE aus Bestätigungssteuerung ermitteln
  h_ibtyp = besttyp.
  CALL FUNCTION 'ME_CONFIRMATION_DELIVERY_TYPE'
       EXPORTING
            i_func              = '1'
       CHANGING
            c_ibtyp             = h_ibtyp
            c_ebtyp             = gf_ebtyp                  "386409
            c_lfart             = gf_dlv_type
       EXCEPTIONS
            function_not_valid  = 01
            param_value_missing = 02
            no_item_found       = 03.
  IF sy-subrc = 0.
    IF gf_dlv_type = space.
      gf_dlv_type = 'EL'.
    ENDIF.
  ELSE.
    gf_dlv_type = 'EL'.
  ENDIF.
ENDFORM.                    " get_inbound_delivery_type

*&---------------------------------------------------------------------*
*&      Form  modify_itab
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM modify_itab.
  DATA: w_err_msg LIKE bapiret2-message,
        wa_return LIKE bapiret2,
        w_msgno LIKE bapiret2-number.

  LOOP AT it_komdlgn.
    READ TABLE it_lips WITH KEY vgbel = it_komdlgn-vgbel
                                vgpos = it_komdlgn-vgpos
                                matnr = it_komdlgn-matnr
                                lfimg = it_komdlgn-lfimg.
    IF sy-subrc EQ 0.
      MOVE : it_lips-vbeln TO it_itab-zmsg,
             'S'           TO it_itab-zresult,
             c_green       TO it_itab-linecolor.

      MOVE : sy-datum TO it_itab-zbdat,     " BDC Execute Date
             sy-uzeit TO it_itab-zbtim,     " BDC Execute Time
             sy-uname TO it_itab-zbnam,     " User ID
             'C'      TO it_itab-zmode.     " Data Characteristic Flag

*---
      MODIFY it_itab TRANSPORTING zbdat   zbtim zbnam zmode
                                  zresult zmsg  linecolor
                            WHERE traid EQ wa_itab-traid
                              AND ematn EQ wa_itab-ematn
                              AND ebeln EQ it_komdlgn-vgbel
                              AND matnr EQ it_komdlgn-matnr.

**--- insert by stlim (2004/05/11)
      UPDATE likp SET zzdepdt = wa_itab-lfdat_la
                      zzarrdt = wa_itab-eta
                WHERE vbeln EQ it_lips-vbeln.
      COMMIT WORK.
    ELSE.
      READ TABLE it_vbfs WITH KEY vbeln = it_komdlgn-vgbel
                                  posnr = it_komdlgn-vgpos
                                  msgv1 = it_komdlgn-matnr.
      IF sy-subrc EQ 0.
        w_msgno = it_vbfs-msgno.
        CALL FUNCTION 'BAPI_MESSAGE_GETDETAIL'
          EXPORTING
            id                = it_vbfs-msgid
            number            = w_msgno
*       LANGUAGE          = SY-LANGU
            textformat        = 'NON'
*       LINKPATTERN       =
         message_v1        = it_vbfs-msgv1
         message_v2        = it_vbfs-msgv2
         message_v3        = it_vbfs-msgv3
         message_v4        = it_vbfs-msgv4
         IMPORTING
           message           = w_err_msg
           return            = wa_return .
*     TABLES
*       TEXT              =
        CLEAR: w_msgno.
        MOVE : w_err_msg     TO it_itab-zmsg,
               'E'           TO it_itab-zresult,
               c_red         TO it_itab-linecolor.
*---
        MOVE : sy-datum TO it_itab-zbdat,     " BDC Execute Date
               sy-uzeit TO it_itab-zbtim,     " BDC Execute Time
               sy-uname TO it_itab-zbnam,     " User ID
               'C'      TO it_itab-zmode.     " Data Characteristic Flag
      ELSE.
        CLEAR: w_msgno.
        MOVE : text-m02      TO it_itab-zmsg,
               'E'           TO it_itab-zresult,
               c_red         TO it_itab-linecolor.
*---
        MOVE : sy-datum TO it_itab-zbdat,     " BDC Execute Date
               sy-uzeit TO it_itab-zbtim,     " BDC Execute Time
               sy-uname TO it_itab-zbnam,     " User ID
               'C'      TO it_itab-zmode.     " Data Characteristic Flag
      ENDIF.

      MODIFY it_itab TRANSPORTING zbdat   zbtim zbnam zmode
                                  zresult zmsg  linecolor
                            WHERE traid EQ wa_itab-traid
                              AND ematn EQ wa_itab-ematn
                              AND ebeln EQ it_komdlgn-vgbel
                              AND matnr EQ it_komdlgn-matnr.
    ENDIF.
  ENDLOOP.



*
**---
*  READ TABLE it_lips INDEX 1.
*
**---
*  IF sy-subrc EQ 0.
*    MOVE : it_lips-vbeln TO it_itab-zmsg,
*           'S'           TO it_itab-zresult,
*           c_green       TO it_itab-linecolor.
*
*    MOVE : sy-datum TO it_itab-zbdat,     " BDC Execute Date
*           sy-uzeit TO it_itab-zbtim,     " BDC Execute Time
*           sy-uname TO it_itab-zbnam,     " User ID
*           'C'      TO it_itab-zmode.     " Data Characteristic Flag
*
**---
*    MODIFY it_itab TRANSPORTING zbdat
*                                zbtim
*                                zbnam
*                                zmode
*                                zresult
*                                zmsg
*                                linecolor
*                                          WHERE traid EQ wa_itab-traid
*                                            AND ematn EQ wa_itab-ematn.
*
***--- insert by stlim (2004/05/11)
*    UPDATE likp SET zzdepdt = wa_itab-lfdat_la
*                    zzarrdt = wa_itab-eta
*              WHERE vbeln EQ it_lips-vbeln.
*    COMMIT WORK.
***--- end of insert
*  ELSE.
*    LOOP AT it_vbfs WHERE msgty = 'E'.
*      w_msgno = it_vbfs-msgno.
*      CALL FUNCTION 'BAPI_MESSAGE_GETDETAIL'
*        EXPORTING
*          id                = it_vbfs-msgid
*          number            = w_msgno
**       LANGUAGE          = SY-LANGU
*          textformat        = 'NON'
**       LINKPATTERN       =
*       message_v1        = it_vbfs-msgv1
*       message_v2        = it_vbfs-msgv2
*       message_v3        = it_vbfs-msgv3
*       message_v4        = it_vbfs-msgv4
*       IMPORTING
*         message           = w_err_msg
*         return            = wa_return .
**     TABLES
**       TEXT              =
*      CLEAR: w_msgno.
*      MOVE : w_err_msg     TO it_itab-zmsg,
*             'E'           TO it_itab-zresult,
*             c_red         TO it_itab-linecolor.
**---
*      MOVE : sy-datum TO it_itab-zbdat,     " BDC Execute Date
*             sy-uzeit TO it_itab-zbtim,     " BDC Execute Time
*             sy-uname TO it_itab-zbnam,     " User ID
*             'C'      TO it_itab-zmode.     " Data Characteristic Flag
*
**---
*      MODIFY it_itab TRANSPORTING zbdat
*                                  zbtim
*                                  zbnam
*                                  zmode
*                                  zresult
*                                  zmsg
*                                  linecolor
*                                           WHERE traid EQ wa_itab-traid
*                                             AND ematn EQ wa_itab-ematn
  .
*    ENDLOOP.
*  ENDIF.
ENDFORM.                    " modify_itab

*&---------------------------------------------------------------------*
*&      Form  comment_build
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM comment_build.
*---
  CLEAR : w_line.
  w_line-typ  = 'H'.
  w_line-info = text-006.
  APPEND w_line TO w_top_of_page.

  CLEAR : w_line.
  APPEND INITIAL LINE TO w_top_of_page.
ENDFORM.                    " comment_build

*&---------------------------------------------------------------------*
*&      Form  make_alv_grid
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM make_alv_grid.
*---
  MOVE : 'LINECOLOR' TO w_layout-info_fieldname.

  PERFORM build_fieldcat.
  PERFORM build_sortcat.

  CLEAR : w_program.

  MOVE : sy-repid TO w_program.

*  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
  CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
       EXPORTING
            i_callback_program      = w_program
            i_callback_pf_status_set = 'SET_STATUS'
            i_callback_user_command = 'USER_COMMAND'
*            i_structure_name        = 'ZSMM_GR_LIST'
            is_layout               = w_layout
            it_fieldcat             = w_fieldcat[]
            it_events               = w_eventcat[]
            it_sort                 = w_sortcat[]
            i_save                  = 'A'
       TABLES
            t_outtab                = it_itab
       EXCEPTIONS
            program_error           = 1
            OTHERS                  = 2.
ENDFORM.                    " make_alv_grid

*---------------------------------------------------------------------*
*       FORM set_status                                               *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  RT_EXTAB                                                      *
*---------------------------------------------------------------------*
FORM set_status USING rt_extab TYPE slis_t_extab.
  SET PF-STATUS 'BASE'.
*  CASE wa_status_flag.
*    WHEN 'BASE'.
*      SET PF-STATUS 'BASE'.
*    WHEN 'DETAIL'.
*      SET PF-STATUS 'DETAIL'.
*  ENDCASE.
ENDFORM.                    "

*---------------------------------------------------------------------*
*       FORM user_command                                             *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  UCOMM                                                         *
*  -->  SELFIELD                                                      *
*---------------------------------------------------------------------*
FORM user_command USING ucomm LIKE sy-ucomm
                       selfield TYPE slis_selfield.
  CASE ucomm.
    WHEN '&EXEC'.
      PERFORM create_delivery.
      PERFORM update_table.

      SUBMIT (sy-repid) WITH s_zslno = s_zslno-low
                        WITH p_update = space
                        WITH r1       = r1
                        WITH r2       = r2
                        WITH r3       = r3
                        WITH p_mode   = p_mode.
  ENDCASE.
ENDFORM.                    "user_command

*&---------------------------------------------------------------------*
*&      Form  build_fieldcat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_fieldcat.
**--- &1 : position       &2 : field name       &3 : field length
**--- &4 : description    &5 : field type       &6 : key
**--- &7 : qty field      &8 : color
  append_fieldcat :
    w_col_pos 'TRAID'     15 'Container No.'  'CHAR' 'X' ''      '',
    w_col_pos 'EMATN'     18 'Case Number'    'CHAR' 'X' ''      '',
    w_col_pos 'MATNR'     18 'Material No.'   'CHAR' ''  ''      '',
*--- 2004/02/17 block by stlim
*    w_col_pos 'ZCREATE'   10 'ASN CrDate'     'DATS' ''  ''      '',
*--- 2004/02/17
*--- 2004/02/17 add by stlim
*    w_col_pos 'LFDAT_LA'   10 'ETD'            'DATS' ''  ''      '',
    w_col_pos 'ETA'        10 'ETA'            'DATS' ''  ''      '',
*---
    w_col_pos 'EBELN'     10 'PO No.'         'CHAR' ''  ''      '',
    w_col_pos 'ZZKDWEBPO' 10 'KDWeb PO'       'CHAR' ''  ''      '',
    w_col_pos 'LIFEXPOS'   6 'CaseLoc'        'NUMC' ''  ''      '',
    w_col_pos 'LFIMG'     12 'Quantity'       'QUAN' ''  'MEINS' '',
    w_col_pos 'MEINS'      3 'UoM'            'CHAR' ''  ''      '',
    w_col_pos 'BORGR_GRP' 35 'Seal Number'    'CHAR' ''  ''      '',
    w_col_pos 'EXPVZ'      1 'M'              'CHAR' ''  ''      '',
    w_col_pos 'ZRESULT'    1 'R'              'CHAR' ''  ''      '',
    w_col_pos 'ZMSG'      20 'Message'        'CHAR' ''  ''      ''.
ENDFORM.                    " build_fieldcat

*&---------------------------------------------------------------------*
*&      Form  build_sortcat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_sortcat.
**--- &1 : position     &2 : field name     &3 : tab name
**--- &4 : up           &5 : sub total
  append_sortcat : '1' 'TRAID' 'IT_ITAB' 'X' '',
                   '2' 'EMATN' 'IT_ITAB' 'X' ''.
ENDFORM.                    " build_sortcat

*&---------------------------------------------------------------------*
*&      Form  update_table
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM update_table.
*--- update table
  DATA : l_total TYPE i,
         l_succs TYPE i,
         l_error TYPE i.

  LOOP AT it_itab.
    CLEAR : ztmm_kd_asn_main.
    MOVE-CORRESPONDING it_itab TO ztmm_kd_asn_main.
    MODIFY ztmm_kd_asn_main.
    l_total = l_total + 1.
    IF it_itab-zresult EQ 'S'.
      l_succs = l_succs + 1.
    ELSE.
      l_error = l_error + 1.
    ENDIF.
  ENDLOOP.

*--- logging interface table
  DATA : st_ztca_if_log LIKE ztca_if_log.

  CLEAR : st_ztca_if_log.

  MOVE : sy-tcode TO st_ztca_if_log-tcode,
         l_total  TO st_ztca_if_log-total,
         l_succs  TO st_ztca_if_log-zsucc,
         l_error  TO st_ztca_if_log-error,
         sy-datum TO st_ztca_if_log-erdat,
         sy-uzeit TO st_ztca_if_log-erzet,
         sy-uname TO st_ztca_if_log-ernam,
         sy-datum TO st_ztca_if_log-aedat,
         sy-uzeit TO st_ztca_if_log-aezet,
         sy-uname TO st_ztca_if_log-aenam.

  CALL FUNCTION 'Z_FCA_EAI_INTERFACE_LOG'
    EXPORTING
      i_ztca_if_log              = st_ztca_if_log
*   IMPORTING
*     E_ZTCA_IF_LOG              =
   EXCEPTIONS
     update_failed              = 1
     number_range_error         = 2
     tcode_does_not_exist       = 3
     OTHERS                     = 4.
ENDFORM.                    " update_table

*&---------------------------------------------------------------------*
*&      Form  get_open_qty
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_EKPO_SHORT_EBELN  text
*      -->P_IT_EKPO_SHORT_EBELP  text
*----------------------------------------------------------------------*
FORM get_open_qty USING    p_it_ekpo_short_ebeln
                           p_it_ekpo_short_ebelp
                           p_wa_itab-zreate.
*---
  DATA : h-meng1 LIKE ekes-dabmg,
         h-menge LIKE ekes-dabmg.

*---
  CLEAR : t163d.
  SELECT SINGLE * FROM t163d
                 WHERE ibtyp EQ c_ibtyp.

*---
  CLEAR : h-menge.
  SELECT * FROM ekes WHERE ebeln EQ p_it_ekpo_short_ebeln
                       AND ebelp EQ p_it_ekpo_short_ebelp
                       AND ebtyp EQ t163d-ebtyp.
    IF ekes-estkz EQ '4'.
      h-menge = h-menge - ekes-menge.
    ELSE.
      h-menge = h-menge + ekes-menge.
    ENDIF.
  ENDSELECT.

*---
  CLEAR : h-meng1.
  IF NOT p_wa_itab-zreate IS INITIAL.
    SELECT * FROM eket WHERE ebeln EQ p_it_ekpo_short_ebeln
                         AND ebelp EQ p_it_ekpo_short_ebelp
                         AND eindt LE p_wa_itab-zreate.
      h-meng1 = h-meng1 + eket-menge.
    ENDSELECT.
*    IF sy-subrc NE 0.
*      CLEAR eket.
*      SELECT SINGLE * FROM eket WHERE ebeln EQ p_it_ekpo_short_ebeln
*                                AND   ebelp EQ p_it_ekpo_short_ebelp.
*    ENDIF.
  ELSE.
*    CLEAR eket.
*    SELECT SINGLE * FROM eket WHERE ebeln EQ ekpo-ebeln
*                                AND   ebelp EQ ekpo-ebelp.
  ENDIF.

*---
  IF h-meng1 GT h-menge.
    w_qty_sum = h-meng1 - h-menge.
  ELSE.
    w_qty_sum = it_ekpo_short-menge - h-menge.
  ENDIF.
  IF w_qty_sum LT 0.
    w_qty_sum = 0.
  ENDIF.

**---
*  CALL FUNCTION 'ME_CONFIRMATION_ANL_QTY'
*       EXPORTING
*            i_ebeln = p_it_ekpo_short_ebeln
*            i_ebelp = p_it_ekpo_short_ebelp
*            i_eindt = p_wa_itab-zreate
*       TABLES
*            xeket   = it_xeket.
*
**---
*  LOOP AT it_xeket.
*    w_qty_sum = w_qty_sum + it_xeket-menge.
*  ENDLOOP.
ENDFORM.                    " get_open_qty
