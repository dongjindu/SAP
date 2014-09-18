*----------------------------------------------------------------------*
*   INCLUDE ZXFMYU19                                                   *
*----------------------------------------------------------------------*
*"             VALUE(I_F_ACCIT) LIKE  ACCIT STRUCTURE  ACCIT
*"             VALUE(I_F_ACCHD) LIKE  ACCHD STRUCTURE  ACCHD
*"             VALUE(I_F_FMIFIIT) LIKE  FMIFIIT STRUCTURE  FMIFIIT
* by Andy Choi for HMMA

*Order category
*  01 - internal order (+investment order)
*  30 - maintenance order
*Budget profile
  data: l_bprof like T003O-bprof.
  clear l_bprof.

  data: l_objnrz like i_f_fmifiit-objnrz.
  l_objnrz = i_f_fmifiit-objnrz.

*CC+IO(stat)
  if l_objnrz(2) = 'KS' and NOT I_F_ACCIT-aufnr is initial.
    select single a~objnr b~bprof into (l_objnrz, l_bprof)
      from aufk as a
      inner join T003O as b
         on a~auart = b~auart
      where aufnr = I_F_ACCIT-aufnr.

*IO(real)
  elseif l_objnrz(2) = 'OR'.
    select single b~bprof into l_bprof
      from aufk as a
      inner join T003O as b
         on a~auart = b~auart
      where objnr = i_f_fmifiit-objnrz.
  endif.

* capture order interested...
  if l_bprof <> space.
    c_default_objnrz = l_objnrz.
  endif.


*OLD CONVERSION
  if i_f_accit-blart = 'BC'.

* Dr) Advance   Cr) A/P    <-- Invoice / DP / Payment
* Dr) A/P       Cr) Bank   <-- Invoice / Payment
* Ex: FMBP160220100028
    IF I_F_ACCHD-BKTXT(4) = 'FMBP'.
      DATA: l_aufnr LIKE bseg-aufnr,
            l_objnr LIKE AUFK-OBJNR.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
           EXPORTING
                input  = I_F_ACCHD-BKTXT+10(7)
           IMPORTING
                output = l_aufnr.
      CALL FUNCTION 'K_ORDER_READ'
           EXPORTING
                aufnr     = l_aufnr
           IMPORTING
                objnr     = l_objnr
           EXCEPTIONS
                not_found = 1.
      if sy-subrc = 0.
        c_default_objnrz = l_objnr.

      endif.
    endif.
  endif.


*TOO MANY LINE ITEM (Infolink Import Expense)
*NOT possible to summarize: PO+ITEM is key of invoice...
*FIXIT NEXT TIME
*  if I_F_ACCIT-BLART = 'KI'. "I_F_ACCIT-saknr = '0000138490'.
*    C_DEFAULT_SGTXT = space.
*  endif.


*IF i_cobl-budat < '20031231'.
*
** except payment document, validate document
*  CHECK i_fmfpo-fivor <> '90'.
*
*  TABLES: bkpf.
*  DATA: g_bktxt LIKE bkpf-bktxt,
*        l_fivor LIKE fmfpo-fivor.
*
** XBLNR has a document number, select header text
*  SELECT SINGLE bktxt INTO g_bktxt
*     FROM bkpf
*     WHERE bukrs = i_cobl-bukrs
*       AND gjahr = i_cobl-gjahr
*       AND belnr = i_cobl-xblnr(10).
** .................................................
*
** Do not create FM Document
*  IF g_bktxt(4) = 'FMNO'.
*    IF i_cobl-shkzg = 'H'.
*      e_fipos = '999100'.                                   "60 credit
*    ELSE.
*      e_fipos = '999200'.  "60
*    ENDIF.
*
** For invoice actuals
** Dr) Deposit   Cr) A/P
*  ELSEIF g_bktxt(4) = 'FMIV' AND i_fmfpo-fivor <> '30'
*                             AND i_cobl-bschl  <> '31'.
*    e_fipos = '216900'.
*
** For payment actuals
** Dr) A/P(Employee)   Cr) Bank
*  ELSEIF g_bktxt(4) = 'FMPY' AND i_cobl-hkont < '0000399999'.
*    e_fipos = '216900'.
*  ENDIF.
*
**conversion document ....................................
*  IF i_cobl-budat < '20030601'.
*
** For payment actuals (eg:FMMN126060)
** Dr) A/P(Employee)   Cr) Bank
*    IF g_bktxt(4) = 'FMMN'.
*      SELECT SINGLE fivor INTO l_fivor FROM fmfpo
*           WHERE fikrs = i_cobl-fikrs
*             AND fipos = g_bktxt+4(6)
*             AND datbis >= i_cobl-budat.
*      IF l_fivor = '30'.       "make sure manually entered is '30'
*        e_fipos = g_bktxt+4(6).
*      ENDIF.
*
** 60 COMMIT for B/S account
** except internal order
*    ELSEIF g_bktxt(4) = 'FMPL'.
*      IF NOT i_cobl-aufnr IS INITIAL AND i_cobl-hkont < '0000399999'.
*        IF i_cobl-shkzg = 'H'. "credit
*          e_fipos = '999100'.  "60
*        ELSE.
*          e_fipos = '999200'.  "60
*        ENDIF.
*        CLEAR: e_fistl, e_fonds.
*      ENDIF.
*
** reclass p&l to capitalize
*    ELSEIF g_bktxt(4) = 'FMCX'.
*      IF  i_cobl-hkont >= '0000600000'
*      AND i_cobl-hkont <= '0000699999'.
*        IF i_cobl-shkzg = 'H'. "credit
*          e_fipos = '999100'.  "60
*        ELSE.
*          e_fipos = '999200'.  "60
*        ENDIF.
*        CLEAR: e_fistl, e_fonds.
*      ENDIF.
*
*
** Dr) Advance   Cr) A/P    <-- Invoice / DP / Payment
** Dr) A/P       Cr) Bank   <-- Invoice / Payment
** Ex: FMBP160220100028
*    ELSEIF g_bktxt(4) = 'FMBP'.
*      SELECT SINGLE fivor INTO l_fivor FROM fmfpo
*           WHERE fikrs = i_cobl-fikrs
*             AND fipos = g_bktxt+4(6)
*             AND datbis >= i_cobl-budat.
*
*      CHECK sy-subrc = 0.
*      DATA: l_aufnr LIKE bseg-aufnr,
*            l_kstar LIKE bseg-hkont.
*
*      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*           EXPORTING
*                input  = g_bktxt+10(7)
*           IMPORTING
*                output = l_aufnr.
*      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*           EXPORTING
*                input  = g_bktxt+4(6)
*           IMPORTING
*                output = l_kstar.
*
*      CALL FUNCTION 'K_ORDER_READ'
*           EXPORTING
*                aufnr     = l_aufnr
*           IMPORTING
*                objnr     = l_objnr
*           EXCEPTIONS
*                not_found = 1.
*
*      CALL FUNCTION 'FM_CO_ASSIGNMENT_READ_OBJECT'
*           EXPORTING
*                i_kokrs            = i_cobl-kokrs
*                i_fikrs            = i_cobl-fikrs
*                i_objnr            = l_objnr
*                i_kstar            = l_kstar
*                i_datum            = i_cobl-budat
*           IMPORTING
*                e_fonds            = new_geber
*                e_fictr            = new_fistl
*                e_flg_found_object = l_xfeld
*           EXCEPTIONS
*                OTHERS             = 1.
*      IF l_xfeld = 'X'.
*        e_fipos = g_bktxt+4(6).
*        e_fonds = new_geber.
*        e_fistl = new_fistl.
*      ENDIF.
*
*    ENDIF.
*  ENDIF. " end of conv document ( ~ 2002.12.31)
*ENDIF.   " end of old document  ( ~ 2003.06.01)
