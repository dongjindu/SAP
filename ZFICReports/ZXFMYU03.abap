*----------------------------------------------------------------------*
* Andy Choi
* EXIT_SAPLFMCH_001
* ZXFMYU03
*----------------------------------------------------------------------*
*"       IMPORTING
*"             VALUE(I_COBL) LIKE  COBL STRUCTURE  COBL
*"             VALUE(I_FMFPO) LIKE  FMFPO STRUCTURE  FMFPO
*"             VALUE(I_FMFCTR) LIKE  FMFCTR STRUCTURE  FMFCTR
*"       EXPORTING
*"             VALUE(E_FIPOS) LIKE  COBL-FIPOS
*"             VALUE(E_FISTL) LIKE  COBL-FISTL
*"             VALUE(E_FONDS) LIKE  COBL-GEBER
*----------------------------------------------------------------------*
CONSTANTS:
  con_on               LIKE fmdy-xfeld     VALUE 'X',
  con_off              LIKE fmdy-xfeld     VALUE ' '.
DATA: BEGIN OF i_po,
        fipos   LIKE  ekpo-fipos,
        fistl   LIKE  ekpo-fistl,
        geber   LIKE  ekpo-geber,
      END OF i_po.
TABLES: csks, aufk, imzo, impr.
DATA: l_scope LIKE aufk-scope,
      l_objnr LIKE fmii1-objnr,
      l_ccobj LIKE fmii1-objnr,
      l_kostl LIKE csks-kostl,
      l_kostv LIKE csks-kostl,
      l_autyp LIKE aufk-autyp,
      l_auart LIKE aufk-auart,
      l_posid LIKE impr-posid,
      l_xfeld LIKE fmdy-xfeld.
DATA: new_fistl LIKE cobl-fistl,
      new_geber LIKE cobl-geber,
      new_fipos LIKE cobl-fipos.
DATA: e_cobl LIKE cobl.

DATA: BEGIN OF l_f_fmhictr.          "/ New Ctr (Hierarchy)
        INCLUDE STRUCTURE fmhictr.
DATA: END   OF l_f_fmhictr.

CHECK i_cobl-fikrs = 'H201'.

*Initial Value
e_fipos = i_cobl-fipos.
e_fistl = i_cobl-fistl.
e_fonds = i_cobl-geber.



* goods movement
IF i_cobl-glvor = 'RMRU'    "PPCG,CO11,COGI
OR i_cobl-glvor = 'RMWF'    "GR prod.ord
OR i_cobl-glvor = 'RMWL'    "Delivery GI
OR i_cobl-glvor = 'RMWI'    "Phy.Inventory
OR i_cobl-glvor = 'RMM1'    "ML
OR i_cobl-glvor = 'RMM2'    "ML Init
OR i_cobl-glvor = 'RMBL'    "Mat D/C
OR i_cobl-glvor = 'RMPR'    "PrChg
OR i_cobl-blart = 'WD'.     "transfer
   e_fipos = '999500'.  EXIT.
ENDIF.

*MIRO - exception: ML, RI(import clearing)
IF i_cobl-glvor = 'RMRP'
AND ( i_cobl-blart = 'ML' or i_cobl-blart = 'RI' ).
  e_fipos = '999500'.  EXIT.
endif.

IF i_cobl-glvor = 'RMWA'.   "Material GI
  IF i_cobl-koart = 'M'.
    e_fipos = '999100'.  EXIT.
  ELSEIF i_cobl-hkont < '0000600000'.
    e_fipos = '999500'.  EXIT.
  ENDIF.
ENDIF.

*order settlement
if i_cobl-awtyp = 'AUAK'.
    e_fipos = '999500'.  EXIT.
endif.

IF i_cobl-awtyp = 'HRPAY'.
  e_fistl = 'HMMA'.
  e_fipos = '999200'.
  EXIT.
ENDIF.

*Overwrite Commitment Item Derivation ??? WHY???
CALL FUNCTION 'FM3R_GET_POSIT_FROM_ACCOUNT'
     EXPORTING
          i_bukrs = i_cobl-bukrs
          i_saknr = i_cobl-hkont
     IMPORTING
          e_fipos = e_fipos
     EXCEPTIONS
          OTHERS  = 4.

**MM invoice (GRIR no update, find original account)
IF i_cobl-glvor = 'RMRP' AND i_fmfpo-fivor IS INITIAL.
* TABLES: ekpo.  "PO Lineitem
  SELECT SINGLE * INTO CORRESPONDING FIELDS OF i_po FROM ekpo
      WHERE ebeln = i_cobl-ebeln
        AND ebelp = i_cobl-ebelp.
  IF i_po-fipos IS INITIAL.
    SELECT SINGLE * INTO CORRESPONDING FIELDS OF i_po FROM ekkn
        WHERE ebeln = i_cobl-ebeln
          AND ebelp = i_cobl-ebelp.
  ENDIF.
  e_fipos    = i_po-fipos.
  e_fistl    = i_po-fistl.
  e_fonds    = i_po-geber.
ENDIF.




*Get Order, CC info
IF i_cobl-aufnr <> space.
  SELECT SINGLE scope autyp auart objnr kostl kostv
     INTO (l_scope, l_autyp, l_auart, l_objnr, l_kostl, l_kostv)
     FROM aufk
     WHERE aufnr = i_cobl-aufnr.
  IF l_kostl IS INITIAL.
    l_kostl = l_kostv.
  ENDIF.
  SELECT SINGLE objnr
     INTO l_ccobj FROM csks
     WHERE kokrs = i_cobl-kokrs AND kostl = l_kostl.
ENDIF.

* order --> fund derivation
IF i_cobl-aufnr <> space AND e_fonds = space.
  CASE l_autyp.
    WHEN '01'.  "internal order
      IF sy-subrc = 0 AND l_scope = 'IV'.
*   CALL FUNCTION 'K_ORDER_READ'
        IF l_auart = 'Y'.   "Overhead Order

        ELSE.
*.........get Fund from Order-Fund assignment
          CALL FUNCTION 'FM_CO_ASSIGNMENT_READ_OBJECT'
               EXPORTING
                    i_kokrs            = i_cobl-kokrs
                    i_fikrs            = i_cobl-fikrs
                    i_objnr            = l_objnr
                    i_kstar            = i_cobl-hkont
                    i_datum            = i_cobl-budat
               IMPORTING
                    e_fonds            = new_geber
                    e_fictr            = new_fistl
                    e_flg_found_object = l_xfeld
               EXCEPTIONS
                    OTHERS             = 1.
          IF l_xfeld = 'X'.
            e_fonds = new_geber.
            e_fistl = new_fistl.
*.........Error: Internal Order is not assigned to Fund.
          ELSE.
            MESSAGE e102(zfi).
          ENDIF.
        ENDIF.
      ENDIF.
    WHEN '30'.  " Maintenance Order
      IF l_auart = 'PM05'.   "Capital Invest Order
* find investment Position. -> Fund
        SELECT SINGLE * FROM imzo
           WHERE objnr = l_objnr.
        IF sy-subrc = 0.
          SELECT SINGLE posid INTO (l_posid) FROM impr
             WHERE posnr EQ imzo-posnr.
          e_fonds = l_posid+1(10).   "fund
          e_fipos = i_cobl-fipos.    "commitment item
* derive fund center from master
          i_cobl-geber = e_fonds.
          i_cobl-fipos = e_fipos.
        ENDIF.
*      ELSE.
** maintenance order (derive from resp.cc)
*        DATA: l_cc_objnr LIKE csks-objnr.
*        SELECT SINGLE objnr INTO l_cc_objnr FROM csks
*          WHERE kokrs = i_cobl-kokrs
*            AND kostl = l_kostl.
*        CALL FUNCTION 'FM_CO_ASSIGNMENT_READ_OBJECT'
*             EXPORTING
*                  i_kokrs            = i_cobl-kokrs
*                  i_fikrs            = i_cobl-fikrs
*                  i_objnr            = l_cc_objnr
*                  i_kstar            = i_cobl-hkont
*                  i_datum            = i_cobl-budat
*             IMPORTING
*                  e_fictr            = new_fistl
*                  e_flg_found_object = l_xfeld
*             EXCEPTIONS
*                  OTHERS             = 1.
*        IF l_xfeld = 'X'. "Found
*          e_fistl = new_fistl.
*        ENDIF.
      ENDIF.
  ENDCASE.
ENDIF.

*--check top fund center, order....
IF NOT i_cobl-aufnr IS INITIAL and e_fistl <> space.
  CALL FUNCTION 'FICTR_READ_WITH_FICTR'
       EXPORTING
            ip_fictr     = e_fistl
            ip_fikrs     = i_cobl-fikrs
            ip_hierarchy = 'X'
       IMPORTING
            f_fmhictr    = l_f_fmhictr.
  IF l_f_fmhictr-parent_obj IS INITIAL.
    CLEAR e_fistl.
  ENDIF.
ENDIF.

*-------if no fund center, derive it from cost center
IF i_cobl-aufnr <> space AND e_fistl = space AND l_ccobj <> space.
  CALL FUNCTION 'FM_CO_ASSIGNMENT_READ_OBJECT'
       EXPORTING
            i_kokrs            = i_cobl-kokrs
            i_fikrs            = i_cobl-fikrs
            i_objnr            = l_ccobj
            i_kstar            = i_cobl-hkont
            i_datum            = i_cobl-budat
       IMPORTING
            e_fictr            = new_fistl
            e_flg_found_object = l_xfeld
       EXCEPTIONS
            OTHERS             = 1.
  IF l_xfeld = 'X'.
    e_fistl = new_fistl.
  ENDIF.
ENDIF.

*depreciation: asset item -> use 60
*not to post profit transfer
*Notice:this item carryforwarded.. so do not select open item..
IF i_cobl-blart = 'AF' AND i_cobl-koart = 'A'.
  e_fipos = '999100'.  "60


*payroll
*  b/s doc Dr) HR Split  Cr) accrual -> need not be recorded
ELSEIF i_cobl-awtyp = 'HRPAY'.
* if i_cobl-KTOSL = 'HRF'.
*  DATA: l_num TYPE i.
*  l_num = i_cobl-XBLNR.  "00001(P&L), 00002(B/S)

  e_fistl = 'HMMA'.
  e_fipos = '999200'.
  EXIT.
ENDIF.


**If not investment, then check default fund center from
**commitment item master
**FIXME... new commitment determined..how?
*IF e_fonds = space AND i_fmfpo-fictr <> space.
*   e_fistl = i_fmfpo-fictr.
*ENDIF.

*---------------------------------------------------------
* get default fund center from commitment item master
* that changed with user exit
*---------------------------------------------------------
IF NOT I_FMFPO-FICTR IS INITIAL AND I_FMFPO-FICTR <> E_FISTL.
  E_FISTL = I_FMFPO-FICTR.
ENDIF.

IF e_fistl = space.
  CLEAR new_fistl.
  SELECT SINGLE fictr INTO new_fistl
       FROM fmfpo
       WHERE fikrs  =  i_cobl-fikrs
         AND fipos  =  e_fipos
         AND datbis >= i_cobl-budat.
  IF new_fistl <> space.
    e_fistl = new_fistl.
  ENDIF.
ENDIF.

*Closing journal entry; Accrual posting -> NO NEED TO RECORD
*use document type???
if i_cobl-blart = 'AC'.
  clear e_fonds.
  e_fistl = 'HMMA'.
  e_fipos = '999900'.
endif.

*FIXME LATER...
* if asset sales/scrapping...
* gain/loss account contain FUND. No use!!!, clear it.
*  IF e_fistl = space.
*    l_objnr   = 'KS'.
*    l_objnr+2 = i_cobl-kokrs.
*    l_objnr+6 = i_cobl-kostl.
*
*    CALL FUNCTION 'FM_CO_ASSIGNMENT_READ_OBJECT'
*         EXPORTING
*              i_kokrs = i_cobl-kokrs
*              i_fikrs = i_cobl-fikrs
*              i_objnr = l_objnr
*              i_kstar = i_cobl-hkont
*              i_datum = i_cobl-budat
*         IMPORTING
**              e_fonds = e_fonds
*              e_fictr = e_fistl.
**              e_fipos = e_fipos.
**    clear: e_fonds.
*  ENDIF.



**********************************************************************
**********************************************************************
*old document ********************************************************
**********************************************************************
**********************************************************************
IF i_cobl-budat < '20031231'.

* except payment document, validate document
  CHECK i_fmfpo-fivor <> '90'.

  TABLES: bkpf.
  DATA: g_bktxt LIKE bkpf-bktxt,
        l_fivor LIKE fmfpo-fivor.

* XBLNR has a document number, select header text
  SELECT SINGLE bktxt INTO g_bktxt
     FROM bkpf
     WHERE bukrs = i_cobl-bukrs
       AND gjahr = i_cobl-gjahr
       AND belnr = i_cobl-xblnr(10).
* .................................................

* Do not create FM Document
  IF g_bktxt(4) = 'FMNO'.
    IF i_cobl-shkzg = 'H'.
      e_fipos = '999100'.                                   "60 credit
    ELSE.
      e_fipos = '999200'.  "60
    ENDIF.

* For invoice actuals
* Dr) Deposit   Cr) A/P
  ELSEIF g_bktxt(4) = 'FMIV' AND i_fmfpo-fivor <> '30'
                             AND i_cobl-bschl  <> '31'.
    e_fipos = '216900'.

* For payment actuals
* Dr) A/P(Employee)   Cr) Bank
  ELSEIF g_bktxt(4) = 'FMPY' AND i_cobl-hkont < '0000399999'.
    e_fipos = '216900'.
  ENDIF.

*conversion document ....................................
  IF i_cobl-budat < '20030601'.

* For payment actuals (eg:FMMN126060)
* Dr) A/P(Employee)   Cr) Bank
    IF g_bktxt(4) = 'FMMN'.
      SELECT SINGLE fivor INTO l_fivor FROM fmfpo
           WHERE fikrs = i_cobl-fikrs
             AND fipos = g_bktxt+4(6)
             AND datbis >= i_cobl-budat.
      IF l_fivor = '30'.       "make sure manually entered is '30'
        e_fipos = g_bktxt+4(6).
      ENDIF.

* 60 COMMIT for B/S account
* except internal order
    ELSEIF g_bktxt(4) = 'FMPL'.
      IF NOT i_cobl-aufnr IS INITIAL AND i_cobl-hkont < '0000399999'.
        IF i_cobl-shkzg = 'H'. "credit
          e_fipos = '999100'.  "60
        ELSE.
          e_fipos = '999200'.  "60
        ENDIF.
        CLEAR: e_fistl, e_fonds.
      ENDIF.

* reclass p&l to capitalize
    ELSEIF g_bktxt(4) = 'FMCX'.
      IF  i_cobl-hkont >= '0000600000'
      AND i_cobl-hkont <= '0000699999'.
        IF i_cobl-shkzg = 'H'. "credit
          e_fipos = '999100'.  "60
        ELSE.
          e_fipos = '999200'.  "60
        ENDIF.
        CLEAR: e_fistl, e_fonds.
      ENDIF.


* Dr) Advance   Cr) A/P    <-- Invoice / DP / Payment
* Dr) A/P       Cr) Bank   <-- Invoice / Payment
* Ex: FMBP160220100028
    ELSEIF g_bktxt(4) = 'FMBP'.
      SELECT SINGLE fivor INTO l_fivor FROM fmfpo
           WHERE fikrs = i_cobl-fikrs
             AND fipos = g_bktxt+4(6)
             AND datbis >= i_cobl-budat.

      CHECK sy-subrc = 0.
      DATA: l_aufnr LIKE bseg-aufnr,
            l_kstar LIKE bseg-hkont.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
           EXPORTING
                input  = g_bktxt+10(7)
           IMPORTING
                output = l_aufnr.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
           EXPORTING
                input  = g_bktxt+4(6)
           IMPORTING
                output = l_kstar.

      CALL FUNCTION 'K_ORDER_READ'
           EXPORTING
                aufnr     = l_aufnr
           IMPORTING
                objnr     = l_objnr
           EXCEPTIONS
                not_found = 1.

      CALL FUNCTION 'FM_CO_ASSIGNMENT_READ_OBJECT'
           EXPORTING
                i_kokrs            = i_cobl-kokrs
                i_fikrs            = i_cobl-fikrs
                i_objnr            = l_objnr
                i_kstar            = l_kstar
                i_datum            = i_cobl-budat
           IMPORTING
                e_fonds            = new_geber
                e_fictr            = new_fistl
                e_flg_found_object = l_xfeld
           EXCEPTIONS
                OTHERS             = 1.
      IF l_xfeld = 'X'.
        e_fipos = g_bktxt+4(6).
        e_fonds = new_geber.
        e_fistl = new_fistl.
      ENDIF.

    ENDIF.
  ENDIF. " end of conv document ( ~ 2002.12.31)
ENDIF.   " end of old document  ( ~ 2003.06.01)
