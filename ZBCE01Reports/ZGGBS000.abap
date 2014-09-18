PROGRAM rggbs000 .
*
* Andy Choi
*
*     : View: V_T80D
*     : RGUGBR00
*
*---------------------------------------------------------------------*
*                                                                     *
*   Substitutions: EXIT-Formpool for Uxxx-Exits                       *
*                                                                     *
*   Note: If you define a new user exit, you have to enter your       *
*         user exit in the form routine GET_EXIT_TITLES.              *
*                                                                     *
*---------------------------------------------------------------------*
INCLUDE fgbbgd00.              "Standard data types

*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!*
*    PLEASE INCLUDE THE FOLLOWING "TYPE-POOL"  AND "TABLES" COMMANDS  *
*        IF THE ACCOUNTING MODULE IS INSTALLED IN YOUR SYSTEM         *
TYPE-POOLS: gb002. " TO BE INCLUDED IN
TABLES: bkpf,      " ANY SYSTEM THAT
        bseg,      " HAS 'FI' INSTALLED
        cobl.
*        CSKS,
*        ANLZ,
*        GLU1.
*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!*


*----------------------------------------------------------------------*
*       FORM GET_EXIT_TITLES                                           *
*----------------------------------------------------------------------*
*       returns name and title of all available standard-exits         *
*       every exit in this formpool has to be added to this form.      *
*       You have to specify a parameter type in order to enable the    *
*       code generation program to determine correctly how to          *
*       generate the user exit call, i.e. how many and what kind of    *
*       parameter(s) are used in the user exit.                        *
*       The following parameter types exist:                           *
*                                                                      *
*       TYPE                Description              Usage             *
*    ------------------------------------------------------------      *
*       C_EXIT_PARAM_NONE   Use no parameter         Subst. and Valid. *
*                           except B_RESULT                            *
*       C_EXIT_PARAM_FIELD  Use one field as param.  Only Substitution *
*       C_EXIT_PARAM_CLASS  Use a type as parameter  Subst. and Valid  *
*                                                                      *
*----------------------------------------------------------------------*
*  -->  EXIT_TAB  table with exit-name and exit-titles                 *
*                 structure: NAME(5), PARAM(1), TITEL(60)
*----------------------------------------------------------------------*
FORM get_exit_titles TABLES etab.

  DATA: BEGIN OF exits OCCURS 50,
          name(5)   TYPE c,
          param     LIKE c_exit_param_none,
          title(60) TYPE c,
        END OF exits.

*  EXITS-NAME  = 'U100'.
*  EXITS-PARAM = C_EXIT_PARAM_NONE.
*  EXITS-TITLE = TEXT-100.             "Cost center from CSKS
*  APPEND EXITS.
*
*  EXITS-NAME  = 'U101'.
*  EXITS-PARAM = C_EXIT_PARAM_FIELD.
*  EXITS-TITLE = TEXT-101.             "Cost center from CSKS
*  APPEND EXITS.

*  EXITS-NAME  = 'U901'.
*  EXITS-PARAM = C_EXIT_PARAM_NONE.
*  EXITS-TITLE = 'Internal Order from Cost Center'.
*  APPEND EXITS.
*
*  EXITS-NAME  = 'U902'.
*  EXITS-PARAM = C_EXIT_PARAM_FIELD.
*  EXITS-TITLE = 'Internal Order from Asset'.
*  APPEND EXITS.

  exits-name  = 'U903'.   exits-param = c_exit_param_none.
  exits-title = 'Lineitem'.
  APPEND exits.

  exits-name  = 'U904'.   exits-param = c_exit_param_class.
  exits-title = 'Complete'.
  APPEND exits.

*  EXITS-NAME  = 'UPOTX'.
*  EXITS-PARAM = C_EXIT_PARAM_FIELD.
*  EXITS-TITLE = 'Read text of purchase order position'.
*  APPEND EXITS.

*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
* PLEASE DELETE THE FIRST '*' FORM THE BEGINING OF THE FOLLOWING LINES *
*        IF THE ACCOUNTING MODULE IS INSTALLED IN YOUR SYSTEM:         *
*  EXITS-NAME  = 'U102'.
*  EXITS-PARAM = C_EXIT_PARAM_CLASS.
*  EXITS-TITLE = TEXT-102.             "Sum is used for the reference.
*  APPEND EXITS.

  REFRESH etab.
  LOOP AT exits.
    etab = exits.
    APPEND etab.
  ENDLOOP.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM U901                                                     *
*---------------------------------------------------------------------*
*       Assign Internal ORder from Cost Center                        *
*---------------------------------------------------------------------*
FORM u901.
*
  MOVE cobl-kostl TO cobl-aufnr.
* COBL-AUFNR = '600025'.

ENDFORM.
*---------------------------------------------------------------------*
*       FORM U902                                                     *
*---------------------------------------------------------------------*
*       Assign Internal Order from Asset                              *
*---------------------------------------------------------------------*
*FORM u902 USING iorder.
*
** Invoice receipt
*  check bkpf-AWTYP = 'RMRP'
*    and bseg-ANLN1 <> space.
*  select single EAUFN into iorder
*     from ANLA
*     where BUKRS = bkpf-bukrs
*       and ANLN1 = bseg-ANLN1
*       and ANLN2 = bseg-ANLN2.
*ENDFORM.
* eject
*---------------------------------------------------------------------*
*       FORM U100                                                     *
*---------------------------------------------------------------------*
*       Reads the cost-center from the CSKS table .                   *
*---------------------------------------------------------------------*
*FORM U100.
*
*ENDFORM.

* eject
*---------------------------------------------------------------------*
*       FORM U101                                                     *
*---------------------------------------------------------------------*
*       Reads the cost-center from the CSKS table for accounting      *
*       area '0001'.                                                  *
*       This exit uses a parameter for the cost_center so it can      *
*       be used irrespective of the table used in the callup point.   *
*---------------------------------------------------------------------*
FORM u101 USING cost_center.

*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
* PLEASE DELETE THE FIRST '*' FORM THE BEGINING OF THE FOLLOWING LINES *
*        IF THE ACCOUNTING MODULE IS INSTALLED IN YOUR SYSTEM:         *
*  SELECT * FROM CSKS
*            WHERE KOSTL EQ COST_CENTER
*              AND KOKRS EQ '0001'.
*    IF CSKS-DATBI >= SY-DATUM AND
*       CSKS-DATAB <= SY-DATUM.
*
*      MOVE CSKS-ABTEI TO COST_CENTER .
*
*    ENDIF.
*  ENDSELECT.

ENDFORM.
*---------------------------------------------------------------------*
FORM upotx USING c_text.
  TABLES: ekpo.
  CHECK NOT bseg-ebeln IS INITIAL.
  CHECK NOT bseg-ebelp IS INITIAL.
  SELECT SINGLE txz01 FROM ekpo INTO c_text
         WHERE  ebeln  = bseg-ebeln
         AND    ebelp  = bseg-ebelp.
  IF sy-subrc NE 0.
    c_text = 'Error'.
  ENDIF.
ENDFORM.                    "UPOTX

* eject
*---------------------------------------------------------------------*
*       FORM U102                                                     *
*---------------------------------------------------------------------*
*       Inserts the sum of the posting into the reference field.      *
*       This exit can be used in FI for the complete document.        *
*       The complete data is passed in one parameter.                 *
*---------------------------------------------------------------------*
*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
* PLEASE DELETE THE FIRST '*' FORM THE BEGINING OF THE FOLLOWING LINES *
*        IF THE ACCOUNTING MODULE IS INSTALLED IN YOUR SYSTEM:         *
*FORM u102 USING bool_data TYPE gb002_015.
*DATA: SUM(10) TYPE C.
*
*    LOOP AT BOOL_DATA-BSEG INTO BSEG
*                    WHERE    SHKZG = 'S'.
*       BSEG-ZUONR = 'Test'.
*       MODIFY BOOL_DATA-BSEG FROM BSEG.
*       ADD BSEG-DMBTR TO SUM.
*    ENDLOOP.
*
*    BKPF-XBLNR = TEXT-001.
*    REPLACE '&' WITH SUM INTO BKPF-XBLNR.
*
*ENDFORM.


*ANDY

DATA: g_lifnr LIKE bseg-lifnr,
      g_baset LIKE bseg-dmbtr.
*&---------------------------------------------------------------------*
*&      Form  get_z11
*&---------------------------------------------------------------------*
FORM get_z11  USING p_chg.
  DATA: l_matnr LIKE mara-matnr,
        l_mtart LIKE mara-mtart.
  IF bseg-matnr <> space.
    bseg-zuonr = bseg-matnr.
    SELECT SINGLE matnr mtart INTO (l_matnr, l_mtart)
           FROM mara WHERE matnr = l_matnr.
    bseg-xref3 = l_mtart.

    p_chg = 'X'.

  ELSEIF bseg-zuonr <> space.
    l_matnr = bseg-zuonr.

    SELECT SINGLE matnr mtart INTO (l_matnr, l_mtart)
           FROM mara WHERE matnr = l_matnr.
    IF sy-subrc <> 0.
      bseg-xref3 = l_mtart.
    ELSE.
      CLEAR bseg-zuonr.
    ENDIF.
    p_chg = 'X'.
  ENDIF.

ENDFORM.                                                    " get_z11
*&---------------------------------------------------------------------*
*&      Form  get_z12
*&---------------------------------------------------------------------*
FORM get_z12  USING p_chg.
  bseg-zuonr        = bseg-ebeln.
  bseg-zuonr+10(5)  = bseg-ebelp.
  IF bseg-sgtxt = space.
    bseg-sgtxt        = bseg-matnr.
  ENDIF.
  bseg-xref3        = bseg-menge.

  p_chg = 'X'.
ENDFORM.                                                    " get_z11
*&---------------------------------------------------------------------*
*&      Form  get_z13
*&---------------------------------------------------------------------*
FORM get_z13  USING p_chg.
  DATA: l_zuonr LIKE bseg-zuonr.
  DATA: l_lifnr LIKE bseg-lifnr,
        l_price LIKE bseg-dmbtr,
        l_punit LIKE konp-kpein.

* only for goods issue
  CHECK bkpf-glvor = 'RMWA'.

  PERFORM get_vendor_pb00
             USING     bkpf-bukrs bkpf-bldat bseg-matnr bseg-werks
                       bkpf-budat bseg-meins
             CHANGING  l_lifnr    l_price    l_punit.

*info-record
*  clear l_zuonr.
*  select a~lifnr into l_zuonr
*         from eina as a join eine as b
*         on a~infnr = b~infnr
*         where a~matnr =  bseg-matnr
*           and a~loekz = space           "DELETION
*           and b~loekz = space
*           and b~prdat >= bkpf-bldat.
*  endselect.
*  if sy-dbcnt = 1.
*    bseg-zuonr = l_zuonr.
*  elseif sy-dbcnt > 1.
*    concatenate l_zuonr '...'  into bseg-zuonr.
*  endif.
  CLEAR bseg-sgtxt.

  bseg-zuonr     = l_lifnr.
*  bseg-sgtxt(10) = l_price.
*  bseg-sgtxt+11(1) = '/'.
*  bseg-sgtxt+12(3) = l_punit.
  bseg-xref3 = bseg-matnr.

  p_chg = 'X'.
ENDFORM.                                                    " get_z13
*&---------------------------------------------------------------------*
*&      Form  get_z14
*&---------------------------------------------------------------------*
FORM get_z14  USING p_chg.
  TABLES: ekko, ekkn, ckmlhd, likp.
  DATA: l_zterm LIKE ekko-zterm,
        l_aufnr LIKE ekkn-aufnr.


* only for invoice verification, goods receipt document
  CHECK bkpf-glvor = 'RMRP' OR bkpf-glvor = 'RMWE'.

* check gr/ir account posting (part, expense...)
  CHECK bseg-buzid = 'W'   "part
     OR bseg-buzid = 'F'   "freight/expense
     OR bseg-buzid = 'P'.  "variance


* check PO payment terms. (import)
  SELECT SINGLE * FROM  ekko
       WHERE ebeln = bseg-ebeln.
  CHECK sy-subrc = 0.

* check payment term for import
  l_zterm = ekko-zterm(2).
  CHECK l_zterm = 'DA' OR l_zterm = 'DP'
     OR l_zterm = 'TT' OR l_zterm = 'LC'.

* bill of lading from invoice document line text: XREF3
  IF bkpf-awtyp = 'RMRP'.
    bseg-xref3 = bseg-sgtxt.
    CLEAR bseg-sgtxt.
  ELSE.
** ... xblnr after posting is done... so following is unvalid.
*      select single BOLNR into T_BSEGSUB-xref3
*         from likp where vbeln = bkpf-XBLNR.
  ENDIF.

* Get HS Code : REF2
  IF bseg-ktosl = 'FR2' OR bseg-ktosl = 'WRX'.  "Duty, MIT
    CLEAR: bseg-xref2.
    SELECT SINGLE stawn FROM marc INTO bseg-xref2
       WHERE matnr = bseg-matnr
         AND werks = bseg-werks.
  ENDIF.

* Get duty rate : REF1
  CASE bseg-ktosl.
    WHEN 'FR2'.  "Duty
      TABLES: konh, konp.
      DATA: l_rate(5) TYPE c.
      SELECT SINGLE * FROM konh
         WHERE kschl = 'ZOA1'
           AND vakey = bseg-xref2.
*        and datab => sy-datum.
      SELECT SINGLE * FROM konp WHERE knumh = konh-knumh.
      l_rate = konp-kbetr / 10.
      CONCATENATE l_rate '%' INTO bseg-xref1.

    WHEN 'WRX'.
      bseg-xref1 = bseg-menge.  "Qty
  ENDCASE.

* material/asset/order into lineitem text
  SELECT SINGLE aufnr INTO l_aufnr
      FROM ekkn
      WHERE ebeln = bseg-ebeln
        AND ebelp = bseg-ebelp.
  IF l_aufnr = space.
    IF bseg-matnr <> space.
*...... get Cost est number
*      select single KALNR into BSEG-xref1 from ckmlhd
      bseg-sgtxt = bseg-matnr.
    ENDIF.
  ELSE.
    bseg-sgtxt = l_aufnr.
  ENDIF.

  p_chg = 'X'.
ENDFORM.                                                    " get_z14
*&---------------------------------------------------------------------*
*&      Form  get_z21
*&---------------------------------------------------------------------*
FORM get_z21  USING p_chg.
  DATA: l_baset LIKE bseg-dmbtr.

* check withholding
  CHECK bseg-ktosl = 'WIT'.

  l_baset = bseg-dmbtr + g_baset.

  bseg-zuonr     = g_lifnr.
  bseg-xref3     = bseg-qsskz.
  bseg-sgtxt(20) = l_baset.

  p_chg = 'X'.
ENDFORM.                                                    " get_z21
*&---------------------------------------------------------------------*
*&      Form  get_skey
*&---------------------------------------------------------------------*
FORM get_skey USING    p_zuawa.
  SELECT SINGLE zuawa INTO p_zuawa
    FROM skb1
    WHERE bukrs = bkpf-bukrs
      AND saknr = bseg-hkont.
ENDFORM.                    " get_skey
*&---------------------------------------------------------------------*
*&      Form  get_vendor_pb00
*&---------------------------------------------------------------------*
FORM get_vendor_pb00 USING    p_bkpf_bukrs
                              p_bkpf_bldat
                              p_bseg_matnr
                              p_bseg_werks
                              p_budat
                              p_bseg_meins
                     CHANGING p_l_lifnr
                              p_l_price
                              p_l_punit.
  DATA: BEGIN OF it_knumh OCCURS 0,
          knumh LIKE konh-knumh,
          datab LIKE konh-datab,
          datbi LIKE konh-datbi,
          lifnr LIKE lfa1-lifnr,
        END   OF it_knumh.
  RANGES: r_ekorg  FOR t024e-ekorg.
  TABLES: t024e.
  CLEAR: p_l_lifnr, p_l_price, p_l_punit.

*---- source of supply
  DATA:  lf_bqpim LIKE bqpim,
         lf_bqpex LIKE bqpex.
  CLEAR lf_bqpim.
  lf_bqpim-matnr = p_bseg_matnr.
  lf_bqpim-werks = p_bseg_werks.
  lf_bqpim-nemng = 1.
  lf_bqpim-meins = p_bseg_meins.
  lf_bqpim-lmein = p_bseg_meins.
  lf_bqpim-nedat = p_budat.
  lf_bqpim-bstyp = 'B'.
  lf_bqpim-pstyp = '0'.
  lf_bqpim-vorga = 'B'.
  lf_bqpim-bqpra = '1'.
  lf_bqpim-noaus = space.
  lf_bqpim-liste = 'X'.
  lf_bqpim-beskz = 'F'.
  lf_bqpim-msgno = 'X'.
  lf_bqpim-noquu = 'X'.
  lf_bqpim-novrt = 'X'.
  lf_bqpim-novrt_ord = 'X'.
  lf_bqpim-nomei = 'X'.
  lf_bqpim-matnl = 'X'.
  lf_bqpim-noqum = 'X'.

  CALL FUNCTION 'ME_SEARCH_SOURCE_OF_SUPPLY'
       EXPORTING
            comim = lf_bqpim
       IMPORTING
            comex = lf_bqpex.

  IF lf_bqpex-flief IS INITIAL.

  ELSE.
    p_l_lifnr = lf_bqpex-flief.
  ENDIF.

**----- Read suitable Price
*  r_ekorg-sign = 'I'.  r_ekorg-option = 'EQ'.
*  SELECT * FROM t024e
*     WHERE bukrs = p_bkpf_bukrs.
*    r_ekorg-low = t024e-ekorg. APPEND r_ekorg.
*  ENDSELECT.
*
*  CLEAR: it_knumh, it_knumh[].
*  SELECT knumh datab matnr lifnr
*    INTO CORRESPONDING FIELDS OF TABLE it_knumh
*    FROM a018
*   WHERE kappl =  'M'
*     AND kschl =  'PB00'
*     AND matnr =  p_bseg_matnr
*     AND ekorg IN r_ekorg
*     AND esokz =  '0'
*     AND datab <= p_bkpf_bldat
*     AND datbi >= p_bkpf_bldat.
*
**----- Check Info Record Deletion Mark
*  DATA: l_cnt TYPE i.
*  LOOP AT it_knumh.
*    SELECT COUNT( * )   INTO l_cnt
*      FROM eina AS a INNER JOIN eine AS b
*        ON a~infnr = b~infnr
*     WHERE a~matnr = p_bseg_matnr
*       AND a~lifnr = it_knumh-lifnr
*       AND a~loekz = ' '
*       AND b~werks = ' '
*       AND b~ekorg IN r_ekorg
*       AND b~loekz = ' '.
*    IF sy-subrc NE 0.   DELETE it_knumh.  ENDIF.
*  ENDLOOP.
*
*  READ TABLE it_knumh INDEX 1.
*  CHECK sy-subrc = 0.
*  p_l_lifnr = it_knumh-lifnr.
*
*  SELECT SINGLE b~kbetr b~kpein
*      INTO (p_l_price,p_l_punit)
*      FROM konh AS a INNER JOIN konp AS b
*        ON a~knumh = b~knumh
*      WHERE a~knumh    = it_knumh-knumh
*        AND b~loevm_ko = space.

ENDFORM.                    " get_vendor_pb00
*---------------------------------------------------------------------*
*       FORM U904                                                     *
*---------------------------------------------------------------------*
*       Complete - MIRO, FI
*---------------------------------------------------------------------*
FORM u904  USING bool_data TYPE gb002_015.
  DATA: l_zuawa LIKE skb1-zuawa,
        l_chg(1) TYPE c.


  LOOP AT bool_data-bseg INTO bseg.
    PERFORM get_skey  USING l_zuawa.

*set default value
    IF bseg-koart = 'K'.
      g_lifnr = bseg-lifnr.
      g_baset = bseg-dmbtr.
    ENDIF.

    CLEAR l_chg.
    CASE l_zuawa.
** material -> assignment : sort key
*      WHEN 'Z11'.   PERFORM get_z11   USING l_chg.
*      WHEN 'Z12'.   PERFORM get_z12   USING l_chg.
** material, vendor, ...
*      WHEN 'Z13'.   PERFORM get_z13   USING l_chg.
** import
*      WHEN 'Z14'.   PERFORM get_z14   USING l_chg.
* withholding
      WHEN 'Z21'.   PERFORM get_z21   USING l_chg.
      WHEN OTHERS.
    ENDCASE.

    IF l_chg = 'X'.  MODIFY bool_data-bseg FROM bseg.  ENDIF.
  ENDLOOP.

ENDFORM.
*---------------------------------------------------------------------*
*       FORM U903                                                     *
*---------------------------------------------------------------------*
*       Lineitem - MM, FI
*---------------------------------------------------------------------*
FORM u903.
  DATA: l_zuawa LIKE skb1-zuawa,
        l_chg(1) TYPE c.
  CLEAR l_chg.

  PERFORM get_skey  USING l_zuawa.

  CASE l_zuawa.
* material -> assignment : sort key
    WHEN 'Z11'.   PERFORM get_z11   USING l_chg.
    WHEN 'Z12'.   PERFORM get_z12   USING l_chg.
* material, vendor, ...
    WHEN 'Z13'.   PERFORM get_z13   USING l_chg.
* import
    WHEN 'Z14'.   PERFORM get_z14   USING l_chg.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.
