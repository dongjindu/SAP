*&---------------------------------------------------------------------*
*& Program: ZRFIF07                                                    *
*& Type   : Report                                                     *
*& Author :
*& Title  : Expense Budget Report (FM)                                 *
*&---------------------------------------------------------------------*
*   Requested by:        Andy Choi                                     *
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------
*  Modification Log
*  Date        Developer  CTS No.    Description
*======================================================================
*& 06/20/2013  T00303    UD1K957468  U1: Apply Archiving
*----------------------------------------------------------------------

* RFFMEP1A

REPORT zrfif07 LINE-SIZE 132 LINE-COUNT 65
                             NO STANDARD PAGE HEADING MESSAGE-ID db .

*-------------------------------------------------------------*
* Tables
*-------------------------------------------------------------*
TABLES : fmioi,
         ekbe,
         eban,
         ebkn, fmhictr, fmfctr, bsis ,tbpfm, fmfpo.

DEFINE __cls.                          " clear & refresh
  clear &1.refresh &1.
END-OF-DEFINITION.


*-------------------------------------------------------------*
* Data Declarations
*-------------------------------------------------------------*
DATA : BEGIN OF it_tab OCCURS 0,
        gjahr LIKE fmioi-gjahr,
        poper TYPE co_perio,

        fistl LIKE fmioi-fistl,
        fonds LIKE fmioi-fonds,
        fipex LIKE fmioi-fipex,

*------ for PM
        objnrz TYPE j_objnr,
        awtyp  TYPE awtyp,

        btart LIKE fmioi-btart,
        wrttp LIKE fmioi-wrttp,

        fkbtr LIKE fmioi-fkbtr,  "AMOUNT

        hkont LIKE fmioi-hkont,
        budat LIKE fmioi-budat,

        lifnr LIKE fmioi-lifnr,

        vrefbt LIKE fmioi-vrefbt, "Predecessor document
        vrefbn LIKE fmioi-vrefbn,
        vrfpos LIKE fmioi-vrfpos,

        refbn  LIKE fmioi-refbn,
        rfpos  LIKE fmioi-rfpos,
        refbt  LIKE fmioi-refbt,   "Reference document category

*------ for PM
        aufnr  TYPE aufnr,
        txz01  LIKE ekpo-txz01,
*        txz01 TYPE txz01,
       END OF it_tab.
DATA: it_tab_fi LIKE it_tab OCCURS 0 WITH HEADER LINE.

DATA : BEGIN OF it_fmfpo OCCURS 0,
         fipos    LIKE fmfpo-fipos,
         bezeich  LIKE fmfpot-bezeich,
         posit    LIKE fmep-posit,
       END OF it_fmfpo.

DATA : BEGIN OF it_pr OCCURS 0,
         banfn   LIKE eban-banfn,
         bnfpo   LIKE eban-bnfpo,
       END OF it_pr.

DATA : BEGIN OF it_po OCCURS 0,
         ebeln   LIKE ekpo-ebeln,
         ebelp   LIKE ekpo-ebelp,
       END OF   it_po.

DATA e_level(3) TYPE c.
DATA l_cousertype(3) TYPE c.
DATA $ix TYPE i.

TYPE-POOLS: slis.

DATA  : BEGIN OF wa_output,
         poper   TYPE poper,

         fonds   LIKE fmioi-fonds ,
         fistl   LIKE fmioi-fistl ,
         fipex   LIKE fmioi-fipex,
         hkont   LIKE fmioi-hkont,

         kostl   LIKE ebkn-kostl,
         aufnr   LIKE ebkn-aufnr,
         lifnr   LIKE fmioi-lifnr,

         refbt LIKE fmioi-refbt,
         refbn LIKE fmioi-refbn,
         rfpos LIKE fmioi-rfpos,

         vrefbt LIKE fmioi-vrefbt, "Predecessor document
         vrefbn LIKE fmioi-vrefbn, "PR document
         vrfpos LIKE fmioi-vrfpos,
         pr_amt  TYPE dmbtr,       "PR.original Amount

         fm_orig LIKE fmioi-fkbtr,
         fm_cf   LIKE fmioi-fkbtr,
         fm_rca  LIKE fmioi-fkbtr,
         fm_bal  LIKE fmioi-fkbtr,
         fm_gr   LIKE fmioi-fkbtr,
         fm_iv   LIKE fmioi-fkbtr,
         fm_dp   LIKE fmioi-fkbtr,

         knbelnr TYPE fm_knbelnr, "I/V document
         fm_chg  LIKE fmioi-fkbtr,
         fm_rdc  LIKE fmioi-fkbtr,
         fm_flw  LIKE fmioi-fkbtr,
         txz01   LIKE ekpo-txz01,

         typetxt(4),

         iv_no   TYPE mblnr,
         iv_amt  TYPE dmbtr,
         flag,
         END OF wa_output.


DATA : it_output LIKE STANDARD TABLE OF wa_output  WITH HEADER LINE
       INITIAL SIZE 0.


DATA: gt_fieldcat TYPE slis_t_fieldcat_alv,
      w_eventcat TYPE slis_t_event,
      gs_layout   TYPE slis_layout_alv,
      gt_sp_group TYPE slis_t_sp_group_alv,
      gt_events   TYPE slis_t_event,
      gt_sorts    TYPE slis_t_sortinfo_alv WITH HEADER LINE,
      gs_prnt     TYPE slis_print_alv,
      g_repid     LIKE sy-repid,
      gs_variant  TYPE disvariant.

DATA: BEGIN OF it_ebkn OCCURS 0,
        banfn LIKE ebkn-banfn,
        bnfpo LIKE ebkn-bnfpo,
        aufnr LIKE ebkn-aufnr,
        kostl LIKE ebkn-kostl,
        sakto LIKE ebkn-sakto,
        fistl LIKE ebkn-fistl,
        geber LIKE ebkn-geber,
        fipos LIKE ebkn-fipos,
        lifnr LIKE ekko-lifnr,
        bedat LIKE ekko-bedat,
      END OF it_ebkn.
DATA: BEGIN OF it_ekkn OCCURS 0,
        ebeln LIKE ekkn-ebeln,
        ebelp LIKE ekkn-ebelp,
        aufnr LIKE ekkn-aufnr,
        kostl LIKE ekkn-kostl,
        sakto LIKE ekkn-sakto,
        fistl LIKE ekkn-fistl,
        geber LIKE ekkn-geber,
        fipos LIKE ekkn-fipos,
      END OF it_ekkn.

DATA: BEGIN OF it_aufk OCCURS 0,
        aufnr LIKE aufk-aufnr,
        kostl LIKE aufk-kostl,
        kostv LIKE aufk-kostv,
      END OF it_aufk.

DATA: BEGIN OF it_ekpo OCCURS 0,
        ebeln LIKE ekpo-ebeln,
        ebelp LIKE ekpo-ebelp,
        erekz LIKE ekpo-erekz,  "Final invoice indicator
        netpr LIKE ekpo-netpr,
        peinh LIKE ekpo-peinh,
        pstyp LIKE ekpo-pstyp,  "Item category in purchasing document
        lifnr LIKE ekko-lifnr,
        bedat LIKE ekko-bedat,
      END OF it_ekpo.

*- U1 start
DATA: gt_ekko_a TYPE TABLE OF ekko WITH HEADER LINE,
      gt_ekpo_a TYPE TABLE OF ekpo WITH HEADER LINE,
      gt_ebkn_a TYPE TABLE OF ebkn WITH HEADER LINE,
      gt_ekkn_a TYPE TABLE OF ekkn WITH HEADER LINE,
      gt_ekbe_a TYPE TABLE OF ekbe WITH HEADER LINE,
      gt_eban_a TYPE TABLE OF eban WITH HEADER LINE,
      gt_aufk_a TYPE TABLE OF aufk WITH HEADER LINE.

DATA: BEGIN OF gt_pr_a OCCURS 0,
      refbn LIKE it_tab-vrefbn,
      rfpos LIKE it_tab-vrfpos,
      END OF gt_pr_a.
*- U1 End

*-------------------------------------------------------------*
* Selection Screen
*--------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE  text-001.

PARAMETERS: p_fikrs LIKE fmioi-fikrs OBLIGATORY MEMORY ID fik.
SELECT-OPTIONS  s_gjahr FOR fmioi-gjahr OBLIGATORY MEMORY ID gjr NO
INTERVALS.
SELECT-OPTIONS: s_perio  FOR fmioi-perio,
                s_zhldt  FOR fmioi-zhldt.

SELECT-OPTIONS: s_refbt  FOR fmioi-refbt.
SELECT-OPTIONS : s_fonds  FOR fmioi-fonds MEMORY ID fic,
                 s_fistl  FOR fmioi-fistl MEMORY ID fis,
                 s_fipex  FOR fmioi-fipex MEMORY ID fps.

SELECTION-SCREEN END OF BLOCK b1.

SELECT-OPTIONS :
                p_prof  FOR tbpfm-profil,
                p_knz   FOR fmfpo-knzaepo DEFAULT '3'.  " item category

SELECT-OPTIONS : s_lifnr  FOR fmioi-lifnr,
                 s_hkont  FOR fmioi-hkont,
                 s_refbn  FOR fmioi-refbn,
                 s_rfpos  FOR fmioi-rfpos,

                 s_vrefbt FOR fmioi-vrefbt,
                 s_vrefbn FOR fmioi-vrefbn,
                 s_vrfpos FOR fmioi-vrfpos,
                 s_budat  FOR fmioi-budat,
                 s_aufnr FOR ebkn-aufnr.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE  text-002.

PARAMETERS : p_fi DEFAULT 'X' NO-DISPLAY.
PARAMETERS : p_fi_all DEFAULT 'X' NO-DISPLAY.

PARAMETERS p_op0   RADIOBUTTON GROUP radi. "Summary
PARAMETERS p_op1   RADIOBUTTON GROUP radi. "Detail

SELECTION-SCREEN END OF BLOCK b2.

* Layout
SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME TITLE text-010.
PARAMETER p_vari TYPE slis_vari.
SELECTION-SCREEN END OF BLOCK b4.

RANGES: s_ebeln FOR ekpo-ebeln,
        s_banfn FOR ebkn-banfn,
        s_bnfpo FOR ebkn-bnfpo,
        s_ebelp FOR ekpo-ebelp.

*- U1 Start
INCLUDE ziarch_comm01.
*- U1 End

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_vari.
  PERFORM alv_variant_f4 CHANGING p_vari.

*-------------------------------------------------------------*
INITIALIZATION.
*  s_refbt-option = 'BT'.
*  s_refbt-sign   = 'I'.
*  s_refbt-low   = '10'.
*  s_refbt-high  = '20'.
*  APPEND s_refbt.
*
*  GET PARAMETER ID 'ZCOLV1' FIELD l_cousertype.
*  e_level = l_cousertype.
*
*  s_fonds = 'IEQ'.
*  s_fonds-low = space.
*  s_fonds-high = space.
*  APPEND s_fonds.
*
** default budget profile
*  REFRESH p_prof.
*  p_prof-option = 'EQ'. p_prof-sign = 'I'.
*  p_prof-low = 'M'. APPEND p_prof.
*  p_prof-low = 'Q'. APPEND p_prof.
*  p_prof-low = 'H'. APPEND p_prof.
*  p_prof-low = 'Y'. APPEND p_prof.

*-------------------------------------------------------------*
START-OF-SELECTION.
*--------------------------------------------------------------*
* Select Data from  FMIOI
  PERFORM get_data.

* Process PO & PR Data
  PERFORM process_data.

  LOOP AT it_output WHERE refbt EQ '910'.
    $ix = sy-tabix.
    CLEAR  it_output-fm_bal.
    MODIFY it_output INDEX $ix.
  ENDLOOP.

  IF  p_op0 EQ 'X'.
    PERFORM sumup_it_output.
  ENDIF.

*-------------------------------------------------------------*
END-OF-SELECTION.
*--------------------------------------------------------------*
* Display ALV
  PERFORM display_alv_ouput.

*&---------------------------------------------------------------------*
*&      Form  get_data
*&---------------------------------------------------------------------*
FORM get_data.

  p_fi = 'X'.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_fmfpo
     FROM fmfpo
     WHERE fikrs =  'H201'
       AND fipos IN s_fipex
       AND knzaepo IN p_knz.

* select pr/po details.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_tab
           FROM  fmioi
            WHERE fonds  IN s_fonds   AND
                  fistl  IN s_fistl   AND
                  fipex  IN s_fipex   AND
                  fikrs  =  p_fikrs   AND
                  gjahr  IN s_gjahr   AND
                  perio  IN s_perio   AND
                  zhldt  IN s_zhldt   AND
                  refbt  IN s_refbt   AND
                  refbn  IN s_refbn   AND
                  rfpos  IN s_rfpos   AND
                  vrefbt IN  s_vrefbt AND
                  vrefbn IN  s_vrefbn AND
                  vrfpos IN  s_vrfpos AND
                  budat  IN s_budat   AND
                  hkont  IN s_hkont   AND
                  fkbtr <> 0.

  DATA $ix TYPE i.
  DATA $profil  LIKE tbpfm-profil.

  LOOP AT it_tab.
    $ix = sy-tabix.
    CLEAR $profil.
    PERFORM get_budget_period USING it_tab-fistl
                                    it_tab-fipex
                                    it_tab-fonds
                                    it_tab-gjahr
                              CHANGING $profil.
    IF NOT $profil IN p_prof.
      DELETE it_tab INDEX $ix.
    ENDIF.

  ENDLOOP.

  IF p_fi = 'X'.

*0100	Original
*0110	Approved in Workflow        (Workflow ledger 9D only)
*0120	Not approved in Workflow    (Workflow ledger 9D only)
*0150	Change
*0200	Reduce
*0250	Paid
*0300	Previous year commt carryforward
*0350	Following year commt carryforward
*0360	Following year actual carryforward
*0400	Lock entry
*0500	Adjustment by follow-on document

    DATA : lt_fi LIKE it_tab OCCURS 0 WITH HEADER LINE.
    REFRESH: lt_fi.

    SELECT gjahr perio
           fistl fonds fipex
           objnrz awtyp
           btart wrttp fkbtr hkont budat
           lifnr
           vrefbt
           vrefbn vrfpos
           knbelnr knbuzei

        INTO TABLE lt_fi
             FROM  v_fmifi
              WHERE
                    fikrs  =  p_fikrs   AND
                    gjahr  IN  s_gjahr   AND
                    btart  = '0100'     AND

                    fonds  IN s_fonds   AND
                    fistl  IN s_fistl   AND
                    fipex  IN s_fipex   AND
                    perio  IN s_perio   AND
                    zhldt  IN s_zhldt   AND
*                 refbt  in s_refbt   and
                    knbelnr  IN s_refbn   AND
                    knbuzei  IN s_rfpos   AND
                    vrefbt IN  s_vrefbt AND
                    vrefbn IN  s_vrefbn AND
                    vrfpos IN  s_vrfpos AND
                    budat  IN s_budat   AND
                    hkont  IN s_hkont   AND
                    fkbtr <> 0
%_HINTS ORACLE 'FIRST_ROWS(10)'.

    LOOP AT lt_fi.
      it_tab_fi = lt_fi.

*for PM request.
      IF lt_fi-awtyp = 'MKPF'.
        SELECT SINGLE aufnr INTO it_tab_fi-aufnr FROM bseg
           WHERE bukrs = p_fikrs
             AND belnr = lt_fi-refbn
             AND hkont = lt_fi-hkont.
      ENDIF.

      COLLECT it_tab_fi.
    ENDLOOP.
  ENDIF.

*--not valid...due to performance..
  IF 1 = 2.
    IF NOT s_ebeln[] IS INITIAL.
* Select PO Details for Given PO
      SELECT
      a~refbn
      a~rfpos
      a~btart
      a~gjahr
      a~fkbtr
      a~fistl
      a~fonds
      a~fipex
      a~wrttp
      a~hkont
      a~fikrs
      a~budat
      a~lifnr
      a~vrefbn
      a~vrfpos
      INTO TABLE it_tab
     FROM  fdm1 AS b INNER JOIN fmioi AS a ON
           b~ebeln = a~refbn  AND
           b~rforg = a~rforg  AND
           b~ebelp = a~rfpos
      WHERE b~bltyp EQ '2'     AND
            b~rftyp EQ 'PORD'  AND
            fonds IN s_fonds   AND
            fipex IN s_fipex   AND
            fistl IN s_fistl   AND
            fikrs EQ p_fikrs   AND
            a~lifnr IN s_lifnr AND
            a~refbt EQ '020'   AND   "PO
            hkont IN s_hkont   AND
            refbn IN s_ebeln   AND
            rfpos IN s_ebelp   AND
            fkbtr  <> '0.00'
%_HINTS ORACLE 'FIRST_ROWS(10)'.

      LOOP AT it_tab.
        it_pr-banfn  = it_tab-vrefbn.
        it_pr-bnfpo  = it_tab-vrfpos.
        COLLECT it_pr.
      ENDLOOP.

* Select PR Details for the above selected PO's
      IF NOT it_pr[] IS INITIAL.
        SELECT
        a~refbn
        a~rfpos
        a~btart
        a~gjahr
        a~fkbtr
        a~fistl
        a~fonds
        a~fipex
        a~wrttp
        a~hkont
        a~fikrs
        a~budat
        a~lifnr
        a~vrefbn
        a~vrfpos
         APPENDING  TABLE it_tab
           FROM  fmioi AS a  FOR ALL ENTRIES IN it_pr
            WHERE refbn EQ it_pr-banfn   AND
                  refbt EQ '010'         AND   "PR
                  rfpos EQ it_pr-bnfpo   AND
                  fonds IN s_fonds       AND                "index 3
                  fipex IN s_fipex       AND                "index 3
                  fistl IN s_fistl       AND                "index 3
                  fikrs EQ p_fikrs   AND
                  lifnr IN s_lifnr   AND
                  hkont IN s_hkont   AND
                  budat IN s_budat   AND
                  fkbtr  <> '0.00' .
      ENDIF.

    ELSEIF NOT s_banfn[] IS INITIAL.
* Select PR Details for the given  PR Number
      SELECT
      a~refbn
      a~rfpos
      a~btart
      a~gjahr
      a~fkbtr
      a~fistl
      a~fonds
      a~fipex
      a~wrttp
      a~hkont
      a~fikrs
      a~budat
      a~lifnr
      a~vrefbn
      a~vrfpos
      INTO TABLE it_tab
      FROM   fdm2 AS b INNER JOIN fmioi AS a ON
           b~banfn = a~refbn  AND
           b~rforg = a~rforg  AND
           b~bnfpo = a~rfpos
      WHERE b~rftyp EQ 'PREQ'  AND
            a~refbt EQ '010'   AND   "PR
            refbn   IN s_banfn   AND
            rfpos   IN s_bnfpo   AND
            fonds   IN s_fonds   AND
            fipex   IN s_fipex   AND
            fistl   IN s_fistl   AND
            fikrs   EQ p_fikrs   AND
            a~lifnr IN s_lifnr AND
            hkont IN s_hkont   AND
            fkbtr  <> '0.00' .

* Select PO Details for the above selected PR Numbers
      SELECT
      a~refbn
      a~rfpos
      a~btart
      a~gjahr
      a~fkbtr
      a~fistl
      a~fonds
      a~fipex
      a~wrttp
      a~hkont
      a~fikrs
      a~budat
      a~lifnr
      a~vrefbn
      a~vrfpos
      APPENDING  TABLE  it_tab
     FROM  fmioi AS a
      WHERE refbt EQ '020'     AND   "PO
            fonds IN s_fonds   AND
            fipex IN s_fipex   AND
            fistl IN s_fistl   AND
            fikrs EQ p_fikrs   AND
            lifnr IN s_lifnr   AND
            hkont IN s_hkont   AND
            vrefbn IN s_banfn   AND
            vrfpos IN s_bnfpo   AND
            fkbtr  <> '0.00' .

    ENDIF.
  ENDIF.
  LOOP AT it_tab.
    $ix = sy-tabix.
    SELECT SINGLE * FROM fmfctr
        WHERE fikrs = 'H201'
          AND fictr = it_tab-fistl
          AND datbis >= sy-datum.

    IF sy-uname <> '103569' AND sy-uname <> 'HIS20069'.
      AUTHORITY-CHECK OBJECT 'Z_FICTR'
               ID 'FM_FIKRS'   FIELD 'H201'
               ID 'FM_FICTR'   FIELD fmfctr-fictr.

      IF sy-subrc <> 0.
        IF e_level <> 'ADM'.
          DELETE it_tab INDEX $ix.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.

  LOOP AT it_tab.
    $ix = sy-tabix.
    CLEAR it_tab-txz01.
    SELECT SINGLE txz01 INTO it_tab-txz01
    FROM ekpo WHERE ebeln EQ it_tab-refbn
    AND ebelp EQ it_tab-rfpos.
*- U1 Start
    IF p_arch EQ 'X' AND sy-subrc <> 0.
      PERFORM archive_read_ekpo_2.
    ENDIF.
*- U1 End
    MODIFY it_tab INDEX $ix TRANSPORTING txz01.
  ENDLOOP.

ENDFORM.                    " get_data
*&---------------------------------------------------------------------*
*&      Form  process_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM process_data.

  PERFORM collect_fm_to_itab.
  PERFORM collect_fi_to_itab.

* if Commited Amount Balance only Option is choosen
*  IF p_hist = 'X'.
*    DELETE  it_output WHERE
*                    fm_orig EQ 0 AND
*                    fm_cf EQ 0 AND
*                    fm_rca EQ 0 AND
*                    fm_bal EQ 0 AND
*                    fm_gr EQ 0 AND
*                    fm_iv EQ 0 AND
*                    fm_dp EQ 0 AND
*                    fm_chg EQ 0 AND
*                    fm_rdc EQ 0 AND
*                    fm_flw EQ 0.
*  ENDIF.

  PERFORM get_ebkn_order_info.
  PERFORM fill_cost_obj_info.


* PO -> get GR amount
  PERFORM get_gr_amount.

* Get PR Details
  IF 1 = 2.
    PERFORM get_pr_details.
  ENDIF.


ENDFORM.                    " process_data
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_ALV_OUPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_alv_ouput.

  IF p_op0 EQ 'X'.
    PERFORM field_setting TABLES gt_fieldcat USING :
*    'POPER'     'Period'        '03' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
    'FONDS'     'Fund'          '10' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
    'FISTL'     'FundCtr'       '10' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
    'FIPEX'     'CommitItm'     '10' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',

    'HKONT'     'G/LAccount'    '10' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
    'KOSTL'     'Cost Ctr'      '10' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
    'AUFNR'     'Order'         '10' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
    'LIFNR'     'Vendor'        '10' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',

*    'REFBT'     'Type'          '02' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
    'REFBN'     'Document'      '10' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
    'RFPOS'     'Itm'           ' 6' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
    'TXZ01'     'Text'          '40' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
* 'AEDAT'     'Date'          '10' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
    'FM_ORIG'   'CI-Org'        '16' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
    'FM_CF'     'CI-C/F'        '16' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',

    'FM_CHG'    'Change'     '16' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
    'FM_RDC'    'Reduce'     '16' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
    'FM_FLW'    'Flw-on'     '16' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',

    'FM_RCA'    'CI-Reduce'     '16' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
    'FM_BAL'    'CI-Balance'    '16' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
    'FM_GR'     'GR AMT'        '16' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
    'FM_DP'     'DP AMT'        '16' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
*    'FM_IV'     'IV AMT'        '16' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',

    'VREFBT'    'Pr.T'          '02' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
    'VREFBN'    'Pr.Doc.'       '10' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
    'PR_AMT'    'Pr.Amt.'       '16' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
    'VRFPOS'    'Pr.Itm'        ' 6' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',

    'KNBELNR'   'IV.Doc.'       '10' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
    'TYPETXT'   'Type'          '4' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',

    'IV_NO'     'I/V No'        '10' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
    'IV_AMT'    'IV Amt.'       '16' ' ' 'R'  ' '  ' '  '  ' ' '  ' '.

  ELSE.

    PERFORM field_setting TABLES gt_fieldcat USING :
    'POPER'     'Period'        '03' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
    'FONDS'     'Fund'          '10' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
    'FISTL'     'FundCtr'       '10' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
    'FIPEX'     'CommitItm'     '10' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',

    'HKONT'     'G/LAccount'    '10' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
    'KOSTL'     'Cost Ctr'      '10' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
    'AUFNR'     'Order'         '10' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
    'LIFNR'     'Vendor'        '10' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',

    'REFBT'     'Type'          '02' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
    'REFBN'     'Document'      '10' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
    'RFPOS'     'Itm'           ' 6' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
    'TXZ01'     'Text'          '40' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
* 'AEDAT'     'Date'          '10' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
    'FM_ORIG'   'CI-Org'        '16' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
    'FM_CF'     'CI-C/F'        '16' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',

    'FM_CHG'    'Change'     '16' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
    'FM_RDC'    'Reduce'     '16' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
    'FM_FLW'    'Flw-on'     '16' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',

    'FM_RCA'    'CI-Reduce'     '16' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
    'FM_BAL'    'CI-Balance'    '16' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
    'FM_GR'     'GR AMT'        '16' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
    'FM_DP'     'DP AMT'        '16' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
    'FM_IV'     'IV AMT'        '16' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',

    'VREFBT'    'Pr.T'          '02' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
    'VREFBN'    'Pr.Doc.'       '10' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
    'VRFPOS'    'Pr.Itm'        ' 6' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',

    'KNBELNR'   'IV.Doc.'       '10' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
    'TYPETXT'   'Type'          '4' ' ' 'L'  ' '  ' '  '  ' ' '  ' '.

  ENDIF.
  g_repid = gs_variant-report = sy-repid.
  gs_variant-variant = p_vari.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = g_repid
      it_fieldcat        = gt_fieldcat[]
      is_variant         = gs_variant
      it_events          = w_eventcat
      i_save             = 'A'
    TABLES
      t_outtab           = it_output[]
    EXCEPTIONS
      program_error      = 1
      OTHERS             = 2.


ENDFORM.                    " DISPLAY_ALV_OUPUT
*&---------------------------------------------------------------------*
*&      Form  field_setting
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_FIELDCAT  text
*      -->P_0482   text
*      -->P_0483   text
*      -->P_0484   text
*      -->P_0485   text
*      -->P_0486   text
*      -->P_0487   text
*      -->P_0488   text
*      -->P_0489   text
*      -->P_0490   text
*      -->P_0491   text
*----------------------------------------------------------------------*
FORM field_setting TABLES   p_fieldcat_t LIKE gt_fieldcat USING
                            p_fieldname       " FIELD name
                            p_title           " field titlw
                            p_outputlen       " length
                            p_key             "
                            p_just            "
                            p_noout           "
                            p_round           "
                            p_cfield          " currency field nam
                            p_qfield          " quantity field nam
                            p_dosum.           " make sum


  DATA: ls_fieldcat TYPE slis_fieldcat_alv.
  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname  = p_fieldname.
*  ls_fieldcat-seltext_s = p_title.
*  ls_fieldcat-seltext_m = p_title.
  ls_fieldcat-seltext_l  = p_title.
  ls_fieldcat-outputlen  = p_outputlen.
  ls_fieldcat-key        = p_key.
  ls_fieldcat-just       = p_just.
  ls_fieldcat-edit       = ''.   "p_edit.
  ls_fieldcat-no_out     = p_noout.
  ls_fieldcat-decimals_out   = p_round.
*  ls_fieldcat-cfieldname = p_cfield.
  ls_fieldcat-currency   = p_cfield.
  ls_fieldcat-qfieldname = p_qfield.
  ls_fieldcat-do_sum     = p_dosum.

  APPEND ls_fieldcat TO gt_fieldcat.




ENDFORM.                    " field_setting
*&---------------------------------------------------------------------*
*&      Form  get_GR_Amount
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_gr_amount.
  DATA : l_idx   LIKE sy-tabix.
  DATA : l_wrbtr LIKE ekbe-wrbtr,
         l_menge LIKE ekbe-menge.

  LOOP AT it_output WHERE refbt = '020'.
    l_idx = sy-tabix.

    READ TABLE it_ekpo WITH KEY ebeln = it_output-refbn
                                ebelp = it_output-rfpos
                       BINARY SEARCH.

    IF sy-subrc EQ 0
    AND it_ekpo-erekz  EQ ''    "Final invoice indicator
    AND it_ekpo-pstyp  NE '9' . "exclude service PO
      SELECT SUM( wrbtr ) SUM( menge )  INTO
            (l_wrbtr,l_menge ) FROM ekbe
                  WHERE ebeln = it_output-refbn AND
                        ebelp = it_output-rfpos AND
                        bewtp = 'E'.
*- U1 Start
      DATA: l_subrc LIKE sy-subrc.
      CLEAR l_subrc.
      l_subrc = sy-subrc.
      IF p_arch EQ 'X'.
        PERFORM archive_read_ekbe CHANGING l_wrbtr l_menge.
      ENDIF.
*- U1 End

*      IF sy-subrc EQ 0 AND l_wrbtr > 0.  "U1-
      IF l_subrc EQ 0 AND l_wrbtr > 0.   "U1+
        it_output-fm_gr  = l_wrbtr.
      ELSEIF l_wrbtr <= 0.
        it_output-fm_gr  = l_menge * it_ekpo-netpr / it_ekpo-peinh.
      ENDIF.


      MODIFY it_output INDEX l_idx TRANSPORTING fm_gr.
    ENDIF.

  ENDLOOP.

ENDFORM.                    " get_GR_Amount
*&---------------------------------------------------------------------*
*&      Form  fill_key_fields
*&---------------------------------------------------------------------*
FORM fill_key_fields.
  it_output-poper = it_tab-poper.

  it_output-fonds = it_tab-fonds.
  it_output-fistl = it_tab-fistl.
  it_output-fipex = it_tab-fipex.
  it_output-hkont = it_tab-hkont.

  it_output-refbt = it_tab-refbt.
  it_output-refbn = it_tab-refbn.
  it_output-rfpos = it_tab-rfpos.

  it_output-vrefbt = it_tab-vrefbt.
  it_output-vrefbn = it_tab-vrefbn.
  it_output-vrfpos = it_tab-vrfpos.

* it_output-aedat = it_tab-budat.

ENDFORM.                    " fill_key_fields
*&---------------------------------------------------------------------*
*&      Form  collect_fm_to_itab
*&---------------------------------------------------------------------*
FORM collect_fm_to_itab.
* amt type
*0100     Original
*0110     Approved in Workflow        (Workflow
*0120     Not approved in Workflow    (Workflow
*0150     Change
*0200     Reduce
*0250     Paid
*0300     Previous year commt carryforward
*0350     Following year commt carryforward
*0360     Following year actual carryforward
*0400     Lock entry
*0500     Adjustment by follow-on document

  LOOP AT it_tab.
    CHECK it_tab-gjahr IN s_gjahr.
    CHECK it_output-aufnr IN s_aufnr.

* Change the Amount Sign ( '+' -> '-', '-' -> '+' )
    it_tab-fkbtr = it_tab-fkbtr * -1 .

    CASE it_tab-btart.
      WHEN '0100'.    "Original
        it_output-fm_orig = it_tab-fkbtr .
*         it_output-fm_rca  = it_tab-fkbtr .

*       Prev+Following year commt carryforward
      WHEN '0300' OR '0350'.    "C/F
        it_output-fm_cf   = it_tab-fkbtr.

      WHEN '0150'.    "Change
        it_output-fm_rca  = it_tab-fkbtr.
        it_output-fm_chg  = it_tab-fkbtr.

      WHEN '0200'.    "Reduce  --> INVOICE
        it_output-fm_rca  = it_tab-fkbtr.
        it_output-fm_rdc  = it_tab-fkbtr.

      WHEN '0500'.    "Adjustment by follow-on document
        it_output-fm_rca  = it_tab-fkbtr.
        it_output-fm_flw  = it_tab-fkbtr.

      WHEN OTHERS.
        CONTINUE.
    ENDCASE.
    PERFORM fill_key_fields.

    it_output-fm_bal = it_tab-fkbtr.
    COLLECT it_output. CLEAR it_output.

  ENDLOOP.

ENDFORM.                    " collect_fm_to_itab
*&---------------------------------------------------------------------*
*&      Form  get_PR_DETAILS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_pr_details.

  SORT it_output BY refbn rfpos.

  LOOP AT it_tab WHERE wrttp = '50'.

*-- search PR?
    READ TABLE it_output WITH KEY refbn = it_tab-refbn
                                  rfpos = it_tab-rfpos
                         BINARY SEARCH.
    IF  sy-subrc EQ 0.
      PERFORM pr_exists. " if PR has PO
    ELSE.
      PERFORM no_pr .    " IF PR has no PO
    ENDIF.

    CLEAR : it_output,wa_output .
  ENDLOOP.

ENDFORM.                    " get_PR_DETAILS
*&---------------------------------------------------------------------*
*&      Form  PR_EXISTS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM pr_exists.
  MOVE-CORRESPONDING it_output TO wa_output.
* Change the Amount Sign ( '+' -> '-', '-' -> '+' )
  it_tab-fkbtr = it_tab-fkbtr * -1 .

  CASE it_tab-btart.

    WHEN '0100'.    "Original
      wa_output-fm_orig = it_output-fm_orig + it_tab-fkbtr.
      IF it_tab-gjahr IN s_gjahr.
        wa_output-fm_cf  = it_output-fm_cf + it_tab-fkbtr .
      ELSE.
        wa_output-fm_cf = 0.
        it_tab-fkbtr = 0.
      ENDIF.


    WHEN '0150'.    "Change
      IF it_tab-gjahr IN s_gjahr.
        wa_output-fm_rca =  it_output-fm_rca + it_tab-fkbtr.
      ELSE.
        wa_output-fm_rca = 0.
        it_tab-fkbtr     = 0.
      ENDIF.

    WHEN '0200'.    "Reduce
      IF it_tab-gjahr IN s_gjahr.
        wa_output-fm_rca = it_output-fm_rca + it_tab-fkbtr.
      ELSE.
        wa_output-fm_rca = 0.
        it_tab-fkbtr     = 0.
      ENDIF.

* Current
    WHEN '0300' OR '0350'.    "Prev+Following year Commt carryforward
      IF it_tab-gjahr IN s_gjahr.
        wa_output-fm_cf = it_output-fm_cf + it_tab-fkbtr.
      ELSE.
        wa_output-fm_cf = 0.
        it_tab-fkbtr     = 0.
      ENDIF.

    WHEN '0500'.    "Adjustment by follow-on document
      IF it_tab-gjahr IN s_gjahr.
        wa_output-fm_rca = wa_output-fm_rca + it_tab-fkbtr.
      ELSE.
        wa_output-fm_rca = 0.
        it_tab-fkbtr     = 0.
      ENDIF.


    WHEN OTHERS.
      EXIT.

  ENDCASE.

  wa_output-fm_bal = it_output-fm_bal + it_tab-fkbtr.
*  wa_output-budat = it_tab-budat.

  MODIFY it_output FROM wa_output
    TRANSPORTING fm_orig fm_cf fm_cf fm_rca fm_bal
                        WHERE  refbn = it_tab-refbn
                          AND  rfpos = it_tab-rfpos.

ENDFORM.                    " PR_EXISTS
*&---------------------------------------------------------------------*
*&      Form  NO_PR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM no_pr.

  MOVE-CORRESPONDING it_tab TO it_output.
  it_output-refbn = it_tab-refbn.
  it_output-rfpos = it_tab-rfpos.

* Change the Amount Sign ( '+' -> '-', '-' -> '+' )
  it_tab-fkbtr = it_tab-fkbtr * -1 .

  CASE it_tab-btart.

    WHEN '0100'.    "Original
      it_output-fm_orig =  it_tab-fkbtr.
      IF it_tab-gjahr IN s_gjahr.
        it_output-fm_cf  =  it_tab-fkbtr .
      ELSE.
        it_output-fm_cf = 0.
        it_tab-fkbtr = 0.
      ENDIF.

    WHEN '0150'.    "Change
      IF it_tab-gjahr IN s_gjahr.
        it_output-fm_rca =   it_tab-fkbtr.
      ELSE.
        it_output-fm_rca = 0.
        it_tab-fkbtr     = 0.
      ENDIF.

    WHEN '0200'.    "Reduce
      IF it_tab-gjahr IN s_gjahr.
        it_output-fm_rca =  it_tab-fkbtr.
      ELSE.
        it_output-fm_rca = 0.
        it_tab-fkbtr     = 0.
      ENDIF.

* Current
    WHEN '0300' OR '0350'.    "Prev+Following year Commt carryforward
      IF it_tab-gjahr IN s_gjahr.
        it_output-fm_cf =  it_tab-fkbtr.
      ELSE.
        wa_output-fm_cf = 0.
        it_tab-fkbtr     = 0.
      ENDIF.

    WHEN '0500'.    "Adjustment by follow-on document
      IF it_tab-gjahr IN s_gjahr.
        it_output-fm_rca = wa_output-fm_rca + it_tab-fkbtr.
      ELSE.
        it_output-fm_rca = 0.
        it_tab-fkbtr     = 0.
      ENDIF.


    WHEN OTHERS.
      EXIT.

  ENDCASE.

  it_output-fm_bal = it_output-fm_bal + it_tab-fkbtr.
*  it_output-budat  = it_tab-budat.
  APPEND it_output. CLEAR it_output.

ENDFORM.                    " NO_PR
*&---------------------------------------------------------------------*
*&      Form  get_ebkn_order_info
*&---------------------------------------------------------------------*
FORM get_ebkn_order_info.
  DATA: BEGIN OF lt_pr OCCURS 0,
          refbn LIKE it_tab-vrefbn,
          rfpos LIKE it_tab-vrfpos,
        END OF lt_pr.
  DATA: lt_po LIKE lt_pr OCCURS 0 WITH HEADER LINE.

  REFRESH: lt_pr, lt_po.

  LOOP AT it_output.
    IF it_output-refbt = '010'.
      lt_pr-refbn =   it_output-refbn .
      lt_pr-rfpos =   it_output-rfpos .
      APPEND lt_pr.
    ELSEIF it_output-refbt = '020'.
      lt_po-refbn =   it_output-refbn .
      lt_po-rfpos =   it_output-rfpos .
      APPEND lt_po.
    ENDIF.
  ENDLOOP.

*PR
  DESCRIBE TABLE lt_pr LINES sy-tabix.
  IF sy-tabix > 0.
    SELECT a~banfn a~bnfpo
           a~aufnr a~kostl a~sakto a~fistl a~geber a~fipos
           b~lifnr b~bedat
         INTO TABLE it_ebkn
         FROM  ebkn AS a
         INNER JOIN eban AS b
                 ON a~banfn = b~banfn
                AND a~bnfpo = b~bnfpo
         FOR ALL ENTRIES IN lt_pr
         WHERE   a~banfn =   lt_pr-refbn AND
                 a~bnfpo =   lt_pr-rfpos .
*- U1 Start
    IF p_arch EQ 'X'.
      PERFORM archive_read_ebkn TABLES lt_pr.
    ENDIF.
*- U1 End

    SORT it_ebkn BY banfn bnfpo.
  ENDIF.

* PO
  DESCRIBE TABLE lt_po LINES sy-tabix.
  IF sy-tabix > 0.
    SELECT a~ebeln a~ebelp
           a~erekz a~netpr a~peinh a~pstyp
           b~lifnr b~bedat
       INTO TABLE it_ekpo
       FROM ekpo AS a
            INNER JOIN ekko AS b
               ON a~ebeln = b~ebeln
       FOR ALL ENTRIES IN lt_po
       WHERE a~ebeln = lt_po-refbn
         AND a~ebelp = lt_po-rfpos.
*- U1 Start
    IF p_arch EQ 'X'.
      PERFORM archive_read_ekpo TABLES lt_po.
    ENDIF.
*- U1 End
    SORT it_ekpo BY ebeln ebelp.

    SELECT ebeln ebelp
           aufnr kostl sakto fistl geber fipos
       INTO TABLE it_ekkn
       FROM ekkn
       FOR ALL ENTRIES IN lt_po
       WHERE ebeln = lt_po-refbn AND
             ebelp = lt_po-rfpos.
*- U1 Start
    IF p_arch EQ 'X'.
      PERFORM archive_read_ekkn TABLES lt_po.
    ENDIF.
*- U1 End

    SORT it_ekkn BY ebeln ebelp.
  ENDIF.

* GET ORDER
  DESCRIBE TABLE it_ebkn LINES sy-tabix.
  IF sy-tabix > 0.
    SELECT aufnr kostl kostv INTO TABLE it_aufk
      FROM aufk
      FOR ALL ENTRIES IN it_ebkn
      WHERE aufnr = it_ebkn-aufnr.
*- U1 Start
    IF p_arch EQ 'X'.
      PERFORM archive_read_aufk USING 'B'.
    ENDIF.
*- U1 End
  ENDIF.

  DESCRIBE TABLE it_ekkn LINES sy-tabix.
  IF sy-tabix > 0.
    SELECT aufnr kostl kostv APPENDING TABLE it_aufk
      FROM aufk
      FOR ALL ENTRIES IN it_ekkn
      WHERE aufnr = it_ekkn-aufnr.
*- U1 Start
    IF p_arch EQ 'X'.
      PERFORM archive_read_aufk USING 'K'.
    ENDIF.
*- U1 End
  ENDIF.

  SORT it_aufk BY aufnr.
  DELETE ADJACENT DUPLICATES FROM it_aufk.

ENDFORM.                    " get_ebkn_order_info
*&---------------------------------------------------------------------*
*&      Form  fill_cost_obj_info
*&---------------------------------------------------------------------*
FORM fill_cost_obj_info.

  DATA: l_idx LIKE sy-tabix.
  DATA $cnt TYPE i.

  LOOP AT it_output.
    l_idx = sy-tabix.

* determine cc from PO
    IF it_output-refbt = '020'.
      it_output-typetxt = 'PO'.

      READ TABLE it_ekpo WITH KEY ebeln = it_output-refbn
                                  ebelp = it_output-rfpos
                         BINARY SEARCH.
      it_output-lifnr = it_ekpo-lifnr.
      it_output-poper = it_ekpo-bedat+4(2).

      READ TABLE it_ekkn WITH KEY ebeln = it_output-refbn
                                  ebelp = it_output-rfpos
                         BINARY SEARCH.
      IF sy-subrc = 0.
        it_output-kostl = it_ekkn-kostl.
        it_output-aufnr = it_ekkn-aufnr.
      ENDIF.

* determine cc from PR
    ELSEIF it_output-refbt = '010'.
      it_output-typetxt = 'PR'.

      READ TABLE it_ebkn WITH KEY banfn = it_output-refbn
                                  bnfpo = it_output-rfpos
                           BINARY SEARCH.
*      read table it_ebkn with key banfn = it_output-refbn
*                                  bnfpo = it_output-rfpos
*                         binary search.
      IF sy-subrc = 0.
        it_output-lifnr = it_ebkn-lifnr.
        it_output-kostl = it_ebkn-kostl.
        it_output-aufnr = it_ebkn-aufnr.
        it_output-poper = it_ebkn-bedat+4(2).
      ENDIF.
    ENDIF.

    IF it_output-refbt = '910'.
      it_output-typetxt = 'AP'.

      READ TABLE it_ekkn WITH KEY ebeln = it_output-vrefbn
                                  ebelp = it_output-vrfpos
                         BINARY SEARCH.
      IF sy-subrc = 0.
        it_output-kostl = it_ekkn-kostl.
        it_output-aufnr = it_ekkn-aufnr.
      ELSE.
* by ig.moon 10/05/2008 {

        SELECT SINGLE kostl aufnr
        INTO (it_output-kostl,it_output-aufnr) FROM ekkn
        WHERE ebeln EQ it_output-vrefbn
          AND ebelp EQ it_output-vrfpos .
*- U1 Start
        IF p_arch EQ 'X' AND sy-subrc <> 0.
          PERFORM archive_read_ekkn_2 CHANGING it_output-kostl it_output-aufnr.
        ENDIF.
*- U1 End
        IF it_output-kostl IS INITIAL.
          SELECT COUNT( DISTINCT kostl )
          FROM bsis INTO $cnt
          WHERE bukrs EQ 'H201'
            AND belnr EQ it_output-refbn
            AND gjahr IN s_gjahr
            AND hkont EQ it_output-hkont.

          IF $cnt > 1.
            it_output-kostl = '*'.
          ELSE.
            SELECT SINGLE kostl aufnr
            FROM bsis INTO (it_output-kostl,bsis-aufnr)
            WHERE bukrs EQ 'H201'
              AND belnr EQ it_output-refbn
              AND gjahr IN s_gjahr
              AND hkont EQ it_output-hkont.
            IF sy-subrc EQ 0.
              IF it_output-aufnr IS INITIAL.
                it_output-aufnr = bsis-aufnr.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.

        IF it_output-lifnr IS INITIAL.
          SELECT SINGLE lifnr
          FROM bsak INTO (it_output-lifnr)
          WHERE bukrs EQ 'H201'
            AND belnr EQ it_output-refbn.
        ENDIF.
* }

      ENDIF.

*      it_output-knbelnr = it_output-refbn.
*      it_output-refbn = it_output-vrefbn.
*      it_output-rfpos = it_output-vrfpos.

    ENDIF.

    IF it_output-kostl IS INITIAL AND NOT it_output-aufnr IS INITIAL.
      READ TABLE it_aufk WITH KEY aufnr = it_output-aufnr BINARY SEARCH.
      IF it_aufk-kostl IS INITIAL.
        it_output-kostl = it_aufk-kostv.
      ELSE.
        it_output-kostl = it_aufk-kostl.
      ENDIF.
    ENDIF.

    IF it_output-vrefbt EQ '20'.
      SELECT SINGLE lifnr INTO it_output-lifnr FROM ekko
      WHERE ebeln EQ it_output-vrefbn.
*- U1 Start
      IF p_arch EQ 'X' AND sy-subrc <> 0.
        PERFORM archive_read_ekko.
      ENDIF.
*- U1 End
      SELECT SINGLE kostl aufnr INTO (it_output-kostl,it_output-aufnr)
                       FROM ekkn
                       WHERE ebeln EQ it_output-vrefbn
                         AND ebelp EQ it_output-vrfpos.
*- U1 Start
      IF p_arch EQ 'X' AND sy-subrc <> 0.
        PERFORM archive_read_ekkn_2 CHANGING it_output-kostl it_output-aufnr.
      ENDIF.
*- U1 End
    ENDIF.


    MODIFY it_output INDEX l_idx TRANSPORTING poper kostl aufnr lifnr
refbn rfpos knbelnr typetxt.
  ENDLOOP.
ENDFORM.                    " fill_cost_obj_info
*&---------------------------------------------------------------------*
*&      Form  collect_fi_to_itab
*&---------------------------------------------------------------------*
FORM collect_fi_to_itab.
*    if it_tab_fi-wrttp = '54'  "iv
*    or it_tab_fi-wrttp = '66'  "trf
*    or it_tab_fi-wrttp = '61'. "dp
  DATA: lt_output LIKE it_output OCCURS 0 WITH HEADER LINE.

  SORT it_output BY refbt refbn rfpos.

  LOOP AT it_tab_fi.
    lt_output-poper = it_tab_fi-poper.
    lt_output-fonds = it_tab_fi-fonds.
    lt_output-fistl = it_tab_fi-fistl.
    lt_output-fipex = it_tab_fi-fipex.
    lt_output-hkont = it_tab_fi-hkont.
    lt_output-aufnr = it_tab_fi-aufnr.

    IF it_tab_fi-wrttp = '61'.
      lt_output-fm_dp    = - it_tab_fi-fkbtr.
    ELSE.
      lt_output-fm_iv    = - it_tab_fi-fkbtr.
    ENDIF.

    lt_output-fm_bal = it_tab_fi-fkbtr.

    lt_output-refbt = '910'.
    lt_output-refbn = it_tab_fi-refbn.
    lt_output-rfpos = it_tab_fi-rfpos.
    lt_output-vrefbt = it_tab_fi-vrefbt.
    lt_output-vrefbn = it_tab_fi-vrefbn.
    lt_output-vrfpos = it_tab_fi-vrfpos.



*    CASE it_tab_fi-btart.
*      WHEN '0100'.    "Original
*        lt_output-fm_orig = it_tab_fi-fkbtr .
*        lt_output-fm_rca  = it_tab_fi-fkbtr .
*
**       Prev+Following year commt carryforward
*      WHEN '0300' OR '0350'.    "C/F
*        lt_output-fm_cf   = it_tab_fi-fkbtr.
*
*      WHEN '0150'.    "Change
*        lt_output-fm_rca  = it_tab_fi-fkbtr.
*        lt_output-fm_chg  = it_tab_fi-fkbtr.
*
*      WHEN '0200'.    "Reduce  --> INVOICE
*        lt_output-fm_rca  = it_tab_fi-fkbtr.
*        lt_output-fm_rdc  = it_tab_fi-fkbtr.
*
*      WHEN '0500'.    "Adjustment by follow-on document
*        lt_output-fm_rca  = it_tab_fi-fkbtr.
*        lt_output-fm_flw  = it_tab_fi-fkbtr.
*
*      WHEN OTHERS.
*        CONTINUE.
*    ENDCASE.



    COLLECT lt_output.  CLEAR lt_output.
*    ELSE.
*      IF it_tab_fi-vrefbt = space.
*        COLLECT lt_output.  CLEAR lt_output.
*
*      ELSE.
*        READ TABLE it_output WITH KEY
*                   refbt = it_tab_fi-vrefbt
*                   refbn = it_tab_fi-vrefbn
*                   rfpos = it_tab_fi-vrfpos
*             BINARY SEARCH.
*        IF sy-subrc = 0.
*          it_output-fm_dp = it_output-fm_dp + lt_output-fm_dp.
*          it_output-fm_iv = it_output-fm_iv + lt_output-fm_iv.
*          MODIFY it_output INDEX sy-tabix TRANSPORTING fm_dp fm_iv.
*        ELSE.
*          COLLECT lt_output.  CLEAR lt_output.
*        ENDIF.
*
*      ENDIF.
*    ENDIF.

  ENDLOOP.

  APPEND LINES OF lt_output TO it_output.
ENDFORM.                    " collect_fi_to_itab
*&---------------------------------------------------------------------*
*&      Form  ALV_VARIANT_F4
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_P_VARI  text
*----------------------------------------------------------------------*
FORM alv_variant_f4 CHANGING p_vari.
  DATA: rs_variant LIKE disvariant,
        lv_nof4 TYPE c.

  CLEAR lv_nof4.
  LOOP AT SCREEN.
    IF screen-name = 'PA_VARI'.
      IF screen-input = 0.
        lv_nof4 = 'X'.
      ENDIF.
    ENDIF.
  ENDLOOP.

  CLEAR rs_variant.
  rs_variant-report   = sy-repid.
  rs_variant-username = sy-uname.

  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
    EXPORTING
      is_variant = rs_variant
      i_save     = 'A'
    IMPORTING
      es_variant = rs_variant
    EXCEPTIONS
      OTHERS     = 1.

  IF sy-subrc = 0 AND lv_nof4 = space.
    p_vari = rs_variant-variant.
  ENDIF.

ENDFORM.                    " ALV_VARIANT_F4
*&---------------------------------------------------------------------*
*&      Form  get_budget_period
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_TAB_FISTL  text
*      -->P_IT_TAB_FIPEX  text
*      -->P_IT_TAB_FONDS  text
*      <--P_$PROFIL  text
*----------------------------------------------------------------------*
FORM get_budget_period USING    f_fictr f_fipos f_geber f_gjahr
                       CHANGING f_profil .

  READ TABLE it_fmfpo  WITH KEY fipos = f_fipos.
  IF sy-subrc = 0.
    PERFORM determine_profile_fs USING    'H201'
                                          f_fictr
                                          it_fmfpo-posit
                                          f_geber
                                          f_gjahr
                                 CHANGING f_profil.
  ELSE.
    f_profil = ' '.
  ENDIF.

ENDFORM.                    " get_budget_period
*&---------------------------------------------------------------------*
*&      Form  determine_profile_fs
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_FIK  text
*      -->P_F_FICTR  text
*      -->P_IT_FMFPO_POSIT  text
*      -->P_F_GEBER  text
*      -->P_P_GJR  text
*      <--P_F_PROFIL  text
*----------------------------------------------------------------------*
FORM determine_profile_fs USING    l_fikrs
                                   l_fictr
                                   l_posit
                                   l_geber
                                   l_gjahr
                          CHANGING l_bprof.
  DATA: l_objnr LIKE fmfctr-ctr_objnr.
  DATA: l_farea LIKE  bpja-farea.


  l_objnr(2) = 'FS'.
  l_objnr+2(4) = l_fikrs.
  l_objnr+6  = l_fictr.

* Profile from TBPFM table.
  CALL FUNCTION 'KBPA_FIFM_GET_PROFIL'
    EXPORTING
      i_objnr         = l_objnr
      i_posit         = l_posit
      i_geber         = l_geber
      i_gjahr         = l_gjahr
      i_farea         = l_farea
    IMPORTING
      e_profil        = l_bprof
    EXCEPTIONS
      no_profil_found = 01.

  IF NOT sy-subrc IS INITIAL.
*   Profile from FundMgt Area
    CALL FUNCTION 'FM5B_GET_PROFILE'
      EXPORTING
        i_fikrs           = l_fikrs
        i_fincode         = l_geber
      IMPORTING
        e_profil          = l_bprof
      EXCEPTIONS
        fm_area_not_found = 1
        OTHERS            = 2.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDIF.

ENDFORM.                    "determine_profile_fs
*&---------------------------------------------------------------------*
*&      Form  sumup_it_output
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM sumup_it_output.

  DATA : $it_output LIKE it_output OCCURS 0 WITH HEADER LINE,
         $it_tab LIKE it_output OCCURS 0 WITH HEADER LINE,
         $it_iv LIKE it_output OCCURS 0 WITH HEADER LINE,
         $total_iv TYPE dmbtr,
         $total_dp TYPE dmbtr,
         $ix TYPE i,
         $fr TYPE i,
         $iix TYPE i,
         $line TYPE i.

  __cls $it_tab.

  LOOP AT it_output WHERE refbt EQ '020'.
    $ix = sy-tabix.
    $it_tab = it_output.
    APPEND $it_tab.
    DELETE it_output INDEX $ix.
  ENDLOOP.

  SORT it_output BY refbt vrefbn vrfpos.

  LOOP AT $it_tab.

    $ix = sy-tabix.
    READ TABLE it_output WITH KEY refbt = '910'
                                  vrefbn = $it_tab-refbn
                                  vrfpos = $it_tab-rfpos
                                  BINARY SEARCH.
    IF sy-subrc EQ 0.
      $fr = sy-tabix.
      __cls $it_iv.
      LOOP AT it_output FROM $fr.
        $iix = sy-tabix.
        IF it_output-refbt NE '910'
        OR it_output-vrefbn NE $it_tab-refbn
        OR it_output-vrfpos NE $it_tab-rfpos.
          EXIT.
        ENDIF.
        $it_iv = it_output.
        APPEND $it_iv.
        it_output-flag = 'X'.
        MODIFY it_output INDEX $iix TRANSPORTING flag.
      ENDLOOP.

      DESCRIBE TABLE $it_iv LINES $line.
      CLEAR $total_iv.
      IF $line EQ 1.
        $it_tab-iv_no  = $it_iv-refbn.
        $it_tab-iv_amt = $it_iv-fm_iv.
      ELSE.
        LOOP AT $it_iv.
          AT LAST.
            SUM.
            $total_iv = $it_iv-fm_iv.
            $total_dp = $it_iv-fm_dp.
          ENDAT.
        ENDLOOP .
        $it_tab-iv_amt = $total_iv.
        $it_tab-fm_dp = $total_dp.

      ENDIF.

      MODIFY $it_tab INDEX $ix TRANSPORTING iv_no iv_amt fm_dp.

    ENDIF.
  ENDLOOP.

  DELETE it_output WHERE flag EQ 'X'.
  APPEND LINES OF $it_tab TO it_output.

  __cls $it_tab.
  LOOP AT it_output WHERE refbt EQ '010'.
    $ix = sy-tabix.
    $it_tab = it_output.
    APPEND $it_tab.
    DELETE it_output INDEX $ix.
  ENDLOOP.

  SORT it_output BY refbt vrefbn vrfpos.

  LOOP AT $it_tab.
    $ix = sy-tabix.
    READ TABLE it_output WITH KEY refbt = '020'
                                  vrefbn = $it_tab-refbn
                                  vrfpos = $it_tab-rfpos
                                  BINARY SEARCH.
    IF sy-subrc EQ 0.
      DELETE $it_tab INDEX $ix.
      it_output-pr_amt = $it_tab-fm_orig.
      MODIFY it_output INDEX sy-tabix TRANSPORTING pr_amt.
    ENDIF.

  ENDLOOP.

  APPEND LINES OF $it_tab TO it_output.

  LOOP AT it_output.
    $ix = sy-tabix.
    IF it_output-refbt EQ '910'.
      it_output-iv_no = it_output-refbn.
      it_output-iv_amt = it_output-fm_iv.
      MODIFY it_output INDEX $ix.
    ENDIF.

  ENDLOOP.

ENDFORM.                    " sumup_it_output
*&---------------------------------------------------------------------*
*&      Form  ARCHIVE_READ_EBKN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_PR  text
*----------------------------------------------------------------------*
FORM archive_read_ebkn  TABLES  pt_pr STRUCTURE gt_pr_a.

  TYPES: BEGIN OF ty_ebkn,
         banfn TYPE banfn,
         bnfpo TYPE bnfpo,
         zebkn TYPE dzebkn,
           archivekey TYPE arkey,
           archiveofs TYPE admi_offst.
  TYPES: END OF ty_ebkn.

  DATA: l_handle    TYPE sytabix,
        lt_ebkn     TYPE TABLE OF ebkn WITH HEADER LINE,
        lt_eban     TYPE TABLE OF eban WITH HEADER LINE,
        l_archindex LIKE aind_str2-archindex,
        l_gentab    LIKE aind_str2-gentab.

  DATA: lt_inx_ebkn TYPE TABLE OF ty_ebkn,
        ls_inx_ebkn TYPE ty_ebkn.

* 1. Input the archive infostructure name
  CLEAR l_archindex.
  l_archindex = 'ZEBKN_001'.

* 2. Get the structure table using infostructure
  CLEAR l_gentab.
  SELECT SINGLE gentab INTO l_gentab FROM aind_str2
   WHERE archindex = l_archindex.

  CHECK sy-subrc = 0 AND NOT l_gentab IS INITIAL.

* 3. Get the archived data from structure table
  CLEAR lt_inx_ebkn[].
  SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_inx_ebkn
    FROM (l_gentab)
    FOR ALL ENTRIES IN pt_pr
   WHERE banfn = pt_pr-refbn
     AND bnfpo = pt_pr-rfpos.

  CHECK NOT lt_inx_ebkn[] IS INITIAL.

* 4. Get more archived data looping structure table
  CLEAR: gt_ebkn_a, gt_ebkn_a[], gt_eban_a, gt_eban_a[].
  LOOP AT lt_inx_ebkn INTO ls_inx_ebkn.
*  4.1 Read information from archivekey & offset
    CLEAR: lt_ebkn, lt_ebkn[], lt_eban, lt_eban[].
    CALL FUNCTION 'ASH_MM_EBAN_READ'
      EXPORTING
        i_archivekey           = ls_inx_ebkn-archivekey
        i_offset               = ls_inx_ebkn-archiveofs
      TABLES
        et_ebkn                = lt_ebkn
        et_eban                = lt_eban
      EXCEPTIONS
        not_in_infostructure   = 1
        not_in_archive         = 2
        no_instructure_defined = 3
        OTHERS                 = 4.

    CHECK NOT lt_ebkn[] IS INITIAL AND NOT lt_eban[] IS INITIAL.

* 5. Append archived data table to finally interal table
    INSERT LINES OF lt_ebkn INTO TABLE gt_ebkn_a.
    INSERT LINES OF lt_eban INTO TABLE gt_eban_a.
  ENDLOOP.

  CHECK NOT gt_ebkn_a[] IS INITIAL AND NOT gt_eban_a[] IS INITIAL.

  SORT: gt_ebkn_a, gt_eban_a.
  DELETE ADJACENT DUPLICATES FROM: gt_ebkn_a COMPARING ALL FIELDS,
                                   gt_eban_a COMPARING ALL FIELDS.
  LOOP AT gt_ebkn_a.
    CLEAR it_ebkn.
    MOVE-CORRESPONDING gt_ebkn_a TO it_ebkn.

    CLEAR gt_eban_a.
    READ TABLE gt_eban_a WITH KEY banfn = gt_ebkn_a-banfn
                                  bnfpo = gt_ebkn_a-bnfpo.
    CHECK sy-subrc = 0.
    "MOVE-CORRESPONDING gt_eban_a TO it_ebkn.
    it_ebkn-lifnr = gt_eban_a-lifnr.
    it_ebkn-bedat = gt_eban_a-bedat.

    APPEND it_ebkn.
  ENDLOOP.

ENDFORM.                    " ARCHIVE_READ_EBKN
*&---------------------------------------------------------------------*
*&      Form  ARCHIVE_READ_EKBE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_L_WRBTR  text
*      <--P_L_MENGE  text
*----------------------------------------------------------------------*
FORM archive_read_ekbe  CHANGING p_wrbtr p_menge.

  TYPES: BEGIN OF ty_ekbe,
         ebeln TYPE ebeln,
         ebelp TYPE ebelp,
         bewtp TYPE bewtp,
           archivekey TYPE arkey,
           archiveofs TYPE admi_offst.
  TYPES: END OF ty_ekbe.

  DATA: l_handle    TYPE sytabix,
        lt_ekbe     TYPE TABLE OF ekbe WITH HEADER LINE,
        l_archindex LIKE aind_str2-archindex,
        l_gentab    LIKE aind_str2-gentab.

  DATA: lt_inx_ekbe TYPE TABLE OF ty_ekbe,
        ls_inx_ekbe TYPE ty_ekbe.

* 1. Input the archive infostructure name
  CLEAR l_archindex.
  l_archindex = 'ZEKBE_001'.

* 2. Get the structure table using infostructure
  CLEAR l_gentab.
  SELECT SINGLE gentab INTO l_gentab FROM aind_str2
   WHERE archindex = l_archindex.

  CHECK sy-subrc = 0 AND NOT l_gentab IS INITIAL.

* 3. Get the archived data from structure table
  CLEAR lt_inx_ekbe[].
  SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_inx_ekbe
    FROM (l_gentab)
   WHERE ebeln = it_output-refbn
     AND ebelp = it_output-rfpos
     AND bewtp = 'E'.

  CHECK NOT lt_inx_ekbe[] IS INITIAL.

* 4. Get more archived data looping structure table
  CLEAR: gt_ekbe_a, gt_ekbe_a[].
  LOOP AT lt_inx_ekbe INTO ls_inx_ekbe.
*  4.1 Read information from archivekey & offset
    CLEAR: lt_ekbe, lt_ekbe[].
    CALL FUNCTION 'ASH_MM_EKKO_READ'
      EXPORTING
        i_archivekey           = ls_inx_ekbe-archivekey
        i_offset               = ls_inx_ekbe-archiveofs
      TABLES
        et_ekbe                = lt_ekbe
      EXCEPTIONS
        not_in_infostructure   = 1
        not_in_archive         = 2
        no_instructure_defined = 3
        OTHERS                 = 4.

    CHECK sy-subrc = 0 AND NOT lt_ekbe[] IS INITIAL.

* 5. Append archived data table to finally interal table
    INSERT LINES OF lt_ekbe INTO TABLE gt_ekbe_a.
  ENDLOOP.

  SORT gt_ekbe_a.
  DELETE ADJACENT DUPLICATES FROM gt_ekbe_a COMPARING ALL FIELDS.

  LOOP AT gt_ekbe_a.
    p_wrbtr = gt_ekbe_a-wrbtr.
    p_menge = gt_ekbe_a-menge.
  ENDLOOP.

ENDFORM.                    " ARCHIVE_READ_EKBE
*&---------------------------------------------------------------------*
*&      Form  ARCHIVE_READ_EKPO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_PO  text
*----------------------------------------------------------------------*
FORM archive_read_ekpo  TABLES  pt_po STRUCTURE gt_pr_a.

  TYPES: BEGIN OF ty_ekpo,
         ebeln TYPE ebeln,
         ebelp TYPE ebelp,
           archivekey TYPE arkey,
           archiveofs TYPE admi_offst.
  TYPES: END OF ty_ekpo.

  DATA: l_handle    TYPE sytabix,
        lt_ekko     TYPE TABLE OF ekko WITH HEADER LINE,
        lt_ekpo     TYPE TABLE OF ekpo WITH HEADER LINE,
        l_archindex LIKE aind_str2-archindex,
        l_gentab    LIKE aind_str2-gentab.

  DATA: lt_inx_ekpo TYPE TABLE OF ty_ekpo,
        ls_inx_ekpo TYPE ty_ekpo.

* 1. Input the archive infostructure name
  CLEAR l_archindex.
  l_archindex = 'ZEKPO_001'.

* 2. Get the structure table using infostructure
  CLEAR l_gentab.
  SELECT SINGLE gentab INTO l_gentab FROM aind_str2
   WHERE archindex = l_archindex.

  CHECK sy-subrc = 0 AND NOT l_gentab IS INITIAL.

* 3. Get the archived data from structure table
  CLEAR lt_inx_ekpo[].
  SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_inx_ekpo
    FROM (l_gentab)
    FOR ALL ENTRIES IN pt_po
   WHERE ebeln = pt_po-refbn
     AND ebelp = pt_po-rfpos.

  CHECK NOT lt_inx_ekpo[] IS INITIAL.

* 4. Get more archived data looping structure table
  CLEAR: gt_ekko_a, gt_ekko_a[], gt_ekpo_a, gt_ekpo_a[].
  LOOP AT lt_inx_ekpo INTO ls_inx_ekpo.
*  4.1 Read information from archivekey & offset
    CLEAR: lt_ekko, lt_ekko[], lt_ekpo, lt_ekpo[].
    CALL FUNCTION 'ASH_MM_EKKO_READ'
      EXPORTING
        i_archivekey           = ls_inx_ekpo-archivekey
        i_offset               = ls_inx_ekpo-archiveofs
      TABLES
        et_ekko                = lt_ekko
        et_ekpo                = lt_ekpo
      EXCEPTIONS
        not_in_infostructure   = 1
        not_in_archive         = 2
        no_instructure_defined = 3
        OTHERS                 = 4.

    CHECK sy-subrc = 0.

* 5. Append archived data table to finally interal table
    INSERT LINES OF lt_ekko INTO TABLE gt_ekko_a.
    INSERT LINES OF lt_ekpo INTO TABLE gt_ekpo_a.
  ENDLOOP.

  SORT: gt_ekko_a, gt_ekpo_a.
  DELETE ADJACENT DUPLICATES FROM: gt_ekko_a COMPARING ALL FIELDS,
                                   gt_ekpo_a COMPARING ALL FIELDS.

  LOOP AT gt_ekpo_a.
    CLEAR it_ekpo.
    MOVE-CORRESPONDING gt_ekpo_a TO it_ekpo.

    READ TABLE gt_ekko_a WITH KEY ebeln = gt_ekpo_a-ebeln.
    CHECK sy-subrc = 0.
    "MOVE-CORRESPONDING gt_ekko_a TO it_ekpo.
    it_ekpo-lifnr = gt_ekko_a-lifnr.
    it_ekpo-bedat = gt_ekko_a-bedat.

    APPEND it_ekpo.
  ENDLOOP.

ENDFORM.                    " ARCHIVE_READ_EKPO
*&---------------------------------------------------------------------*
*&      Form  ARCHIVE_READ_EKPO_2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM archive_read_ekpo_2 .

  TYPES: BEGIN OF ty_ekpo,
         ebeln TYPE ebeln,
         ebelp TYPE ebelp,
           archivekey TYPE arkey,
           archiveofs TYPE admi_offst.
  TYPES: END OF ty_ekpo.

  DATA: l_handle    TYPE sytabix,
        lt_ekpo     TYPE TABLE OF ekpo WITH HEADER LINE,
        l_archindex LIKE aind_str2-archindex,
        l_gentab    LIKE aind_str2-gentab.

  DATA: lt_inx_ekpo TYPE TABLE OF ty_ekpo,
        ls_inx_ekpo TYPE ty_ekpo.

* 1. Input the archive infostructure name
  CLEAR l_archindex.
  l_archindex = 'ZEKPO_001'.

* 2. Get the structure table using infostructure
  CLEAR l_gentab.
  SELECT SINGLE gentab INTO l_gentab FROM aind_str2
   WHERE archindex = l_archindex.

  CHECK sy-subrc = 0 AND NOT l_gentab IS INITIAL.

* 3. Get the archived data from structure table
  CLEAR ls_inx_ekpo.
  SELECT SINGLE * INTO CORRESPONDING FIELDS OF ls_inx_ekpo
    FROM (l_gentab)
   WHERE ebelp EQ it_tab-rfpos.

  CHECK sy-subrc = 0.

*  4.1 Read information from archivekey & offset
  CLEAR: lt_ekpo, lt_ekpo[].
  CALL FUNCTION 'ASH_MM_EKKO_READ'
    EXPORTING
      i_archivekey           = ls_inx_ekpo-archivekey
      i_offset               = ls_inx_ekpo-archiveofs
    TABLES
      et_ekpo                = lt_ekpo
    EXCEPTIONS
      not_in_infostructure   = 1
      not_in_archive         = 2
      no_instructure_defined = 3
      OTHERS                 = 4.

  CHECK sy-subrc = 0.

* 5. Append archived data table to finally interal table
  READ TABLE lt_ekpo INDEX 1.

  CHECK sy-subrc = 0.

  it_tab-txz01 = lt_ekpo-txz01.

ENDFORM.                    " ARCHIVE_READ_EKPO_2
*&---------------------------------------------------------------------*
*&      Form  ARCHIVE_READ_EKKN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_PO  text
*----------------------------------------------------------------------*
FORM archive_read_ekkn  TABLES  pt_po STRUCTURE  gt_pr_a.

  TYPES: BEGIN OF ty_ekkn,
         ebeln TYPE ebeln,
         ebelp TYPE ebelp,
           archivekey TYPE arkey,
           archiveofs TYPE admi_offst.
  TYPES: END OF ty_ekkn.

  DATA: l_handle    TYPE sytabix,
        lt_ekkn     TYPE TABLE OF ekkn WITH HEADER LINE,
        l_archindex LIKE aind_str2-archindex,
        l_gentab    LIKE aind_str2-gentab.

  DATA: lt_inx_ekkn TYPE TABLE OF ty_ekkn,
        ls_inx_ekkn TYPE ty_ekkn.

* 1. Input the archive infostructure name
  CLEAR l_archindex.
  l_archindex = 'ZEKPO_001'.

* 2. Get the structure table using infostructure
  CLEAR l_gentab.
  SELECT SINGLE gentab INTO l_gentab FROM aind_str2
   WHERE archindex = l_archindex.

  CHECK sy-subrc = 0 AND NOT l_gentab IS INITIAL.

* 3. Get the archived data from structure table
  CLEAR lt_inx_ekkn[].
  SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_inx_ekkn
    FROM (l_gentab)
    FOR ALL ENTRIES IN pt_po
   WHERE ebeln = pt_po-refbn
     AND ebelp = pt_po-rfpos.

  CHECK sy-subrc = 0.

* 4. Get more archived data looping structure table
  CLEAR: gt_ekkn_a, gt_ekkn_a[].
  LOOP AT lt_inx_ekkn INTO ls_inx_ekkn.
*  4.1 Read information from archivekey & offset
    CLEAR: lt_ekkn, lt_ekkn[].
    CALL FUNCTION 'ASH_MM_EKKO_READ'
      EXPORTING
        i_archivekey           = ls_inx_ekkn-archivekey
        i_offset               = ls_inx_ekkn-archiveofs
      TABLES
        et_ekkn                = lt_ekkn
      EXCEPTIONS
        not_in_infostructure   = 1
        not_in_archive         = 2
        no_instructure_defined = 3
        OTHERS                 = 4.

    CHECK sy-subrc = 0.

* 5. Append archived data table to finally interal table
    INSERT LINES OF lt_ekkn INTO TABLE gt_ekkn_a.
  ENDLOOP.

  SORT gt_ekkn_a.
  DELETE ADJACENT DUPLICATES FROM: gt_ekkn_a COMPARING ALL FIELDS.

  LOOP AT gt_ekkn_a.
    CLEAR it_ekkn.
    MOVE-CORRESPONDING gt_ekkn_a TO it_ekkn.
    APPEND it_ekkn.
  ENDLOOP.

ENDFORM.                    " ARCHIVE_READ_EKKN
*&---------------------------------------------------------------------*
*&      Form  ARCHIVE_READ_EKKN_2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_IT_OUTPUT_KOSTL  text
*      <--P_IT_OUTPUT_AUFNR  text
*----------------------------------------------------------------------*
FORM archive_read_ekkn_2  CHANGING p_kostl p_aufnr.

  TYPES: BEGIN OF ty_ekkn,
         ebeln TYPE ebeln,
         ebelp TYPE ebelp,
           archivekey TYPE arkey,
           archiveofs TYPE admi_offst.
  TYPES: END OF ty_ekkn.

  DATA: l_handle    TYPE sytabix,
        lt_ekkn     TYPE TABLE OF ekkn WITH HEADER LINE,
        l_archindex LIKE aind_str2-archindex,
        l_gentab    LIKE aind_str2-gentab.

  DATA: lt_inx_ekkn TYPE TABLE OF ty_ekkn,
        ls_inx_ekkn TYPE ty_ekkn.

* 1. Input the archive infostructure name
  CLEAR l_archindex.
  l_archindex = 'ZEKPO_001'.

* 2. Get the structure table using infostructure
  CLEAR l_gentab.
  SELECT SINGLE gentab INTO l_gentab FROM aind_str2
   WHERE archindex = l_archindex.

  CHECK sy-subrc = 0 AND NOT l_gentab IS INITIAL.

* 3. Get the archived data from structure table
  CLEAR ls_inx_ekkn.
  SELECT SINGLE * INTO CORRESPONDING FIELDS OF ls_inx_ekkn
    FROM (l_gentab)
   WHERE ebeln EQ it_output-vrefbn
     AND ebelp EQ it_output-vrfpos.

  CHECK sy-subrc = 0.

*  4.1 Read information from archivekey & offset
  CLEAR: lt_ekkn, lt_ekkn[].
  CALL FUNCTION 'ASH_MM_EKKO_READ'
    EXPORTING
      i_archivekey           = ls_inx_ekkn-archivekey
      i_offset               = ls_inx_ekkn-archiveofs
    TABLES
      et_ekkn                = lt_ekkn
    EXCEPTIONS
      not_in_infostructure   = 1
      not_in_archive         = 2
      no_instructure_defined = 3
      OTHERS                 = 4.

  CHECK sy-subrc = 0.

* 5. Append archived data table to finally interal table
  READ TABLE lt_ekkn INDEX 1.

  CHECK sy-subrc = 0.

  p_kostl = lt_ekkn-kostl.
  p_aufnr = lt_ekkn-aufnr.

ENDFORM.                    " ARCHIVE_READ_EKKN_2
*&---------------------------------------------------------------------*
*&      Form  ARCHIVE_READ_EKKO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM archive_read_ekko .

  TYPES: BEGIN OF ty_ekko,
         ebeln TYPE ebeln,
         ebelp TYPE ebelp,
           archivekey TYPE arkey,
           archiveofs TYPE admi_offst.
  TYPES: END OF ty_ekko.

  DATA: l_handle    TYPE sytabix,
        lt_ekko     TYPE TABLE OF ekko WITH HEADER LINE,
        l_archindex LIKE aind_str2-archindex,
        l_gentab    LIKE aind_str2-gentab.

  DATA: lt_inx_ekko TYPE TABLE OF ty_ekko,
        ls_inx_ekko TYPE ty_ekko.

* 1. Input the archive infostructure name
  CLEAR l_archindex.
  l_archindex = 'ZEKPO_001'.

* 2. Get the structure table using infostructure
  CLEAR l_gentab.
  SELECT SINGLE gentab INTO l_gentab FROM aind_str2
   WHERE archindex = l_archindex.

  CHECK sy-subrc = 0 AND NOT l_gentab IS INITIAL.

* 3. Get the archived data from structure table
  CLEAR ls_inx_ekko.
  SELECT SINGLE * INTO CORRESPONDING FIELDS OF ls_inx_ekko
    FROM (l_gentab)
   WHERE ebelp EQ it_tab-rfpos.

  CHECK sy-subrc = 0.

*  4.1 Read information from archivekey & offset
  CLEAR: lt_ekko, lt_ekko[].
  CALL FUNCTION 'ASH_MM_EKKO_READ'
    EXPORTING
      i_archivekey           = ls_inx_ekko-archivekey
      i_offset               = ls_inx_ekko-archiveofs
    TABLES
      et_ekko                = lt_ekko
    EXCEPTIONS
      not_in_infostructure   = 1
      not_in_archive         = 2
      no_instructure_defined = 3
      OTHERS                 = 4.

  CHECK sy-subrc = 0.

* 5. Append archived data table to finally interal table
  READ TABLE lt_ekko INDEX 1.

  CHECK sy-subrc = 0.

  it_output-lifnr = lt_ekko-lifnr.

ENDFORM.                    " ARCHIVE_READ_EKKO
*&---------------------------------------------------------------------*
*&      Form  ARCHIVE_READ_AUFK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM archive_read_aufk USING p_flag.

  TYPES: BEGIN OF ty_aufk,
         aufnr TYPE aufnr,
         auart TYPE aufart,
         erdat TYPE auferfdat,
         kostv TYPE aufkostv,
         objnr TYPE j_objnr,
           archivekey TYPE arkey,
           archiveofs TYPE admi_offst.
  TYPES: END OF ty_aufk.

  DATA: l_handle    TYPE sytabix,
        lt_aufk     TYPE TABLE OF aufk WITH HEADER LINE,
        l_archindex LIKE aind_str2-archindex,
        l_gentab    LIKE aind_str2-gentab.

  DATA: lt_inx_aufk TYPE TABLE OF ty_aufk,
        ls_inx_aufk TYPE ty_aufk.

* 1. Input the archive infostructure name
  CLEAR l_archindex.
  l_archindex = 'ZAUFK_001'.

* 2. Get the structure table using infostructure
  CLEAR l_gentab.
  SELECT SINGLE gentab INTO l_gentab FROM aind_str2
   WHERE archindex = l_archindex.

  CHECK sy-subrc = 0 AND NOT l_gentab IS INITIAL.

* 3. Get the archived data from structure table
  IF p_flag = 'B'.
    CLEAR lt_inx_aufk[].
    SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_inx_aufk
      FROM (l_gentab)
      FOR ALL ENTRIES IN it_ebkn
     WHERE aufnr = it_ebkn-aufnr.
  ELSEIF p_flag = 'K'.
    CLEAR lt_inx_aufk[].
    SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_inx_aufk
      FROM (l_gentab)
      FOR ALL ENTRIES IN it_ekkn
     WHERE aufnr = it_ekkn-aufnr.
  ENDIF.

  CHECK NOT lt_inx_aufk[] IS INITIAL.

* 4. Get more archived data looping structure table
  CLEAR: gt_aufk_a, gt_aufk_a[].
  LOOP AT lt_inx_aufk INTO ls_inx_aufk.
*  4.1 Read information from archivekey & offset
    CLEAR l_handle.
    CALL FUNCTION 'ARCHIVE_READ_OBJECT'
      EXPORTING
        object                    = 'MM_EKKO'
        archivkey                 = ls_inx_aufk-archivekey
        offset                    = ls_inx_aufk-archiveofs
      IMPORTING
        archive_handle            = l_handle
      EXCEPTIONS
        no_record_found           = 1
        file_io_error             = 2
        internal_error            = 3
        open_error                = 4
        cancelled_by_user         = 5
        archivelink_error         = 6
        object_not_found          = 7
        filename_creation_failure = 8
        file_already_open         = 9
        not_authorized            = 10
        file_not_found            = 11
        error_message             = 12
        OTHERS                    = 13.

    CHECK sy-subrc = 0.

*  4.2 Read table from information
    CLEAR: lt_aufk, lt_aufk[].
    CALL FUNCTION 'ARCHIVE_GET_TABLE'
      EXPORTING
        archive_handle          = l_handle
        record_structure        = 'AUFK'
        all_records_of_object   = 'X'
      TABLES
        table                   = lt_aufk
      EXCEPTIONS
        end_of_object           = 1
        internal_error          = 2
        wrong_access_to_archive = 3
        OTHERS                  = 4.

    CHECK sy-subrc = 0 AND NOT lt_aufk[] IS INITIAL.

* 5. Append archived data table to finally interal table
    INSERT LINES OF lt_aufk INTO TABLE gt_aufk_a.
  ENDLOOP.

  SORT gt_aufk_a.
  DELETE ADJACENT DUPLICATES FROM gt_aufk_a COMPARING ALL FIELDS.

  LOOP AT gt_aufk_a.
    CLEAR it_aufk.
    MOVE-CORRESPONDING gt_aufk_a TO it_aufk.
    APPEND it_aufk.
  ENDLOOP.

ENDFORM.                    " ARCHIVE_READ_AUFK
