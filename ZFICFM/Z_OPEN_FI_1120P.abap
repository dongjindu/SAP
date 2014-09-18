FUNCTION Z_OPEN_FI_1120P.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(I_BKDF) TYPE  BKDF OPTIONAL
*"  TABLES
*"      T_BKPF STRUCTURE  BKPF
*"      T_BSEG STRUCTURE  BSEG
*"      T_BKPFSUB STRUCTURE  BKPF_SUBST
*"      T_BSEGSUB STRUCTURE  BSEG_SUBST
*"      T_BSEC STRUCTURE  BSEC OPTIONAL
*"  CHANGING
*"     REFERENCE(I_BKDFSUB) TYPE  BKDF_SUBST OPTIONAL
*"----------------------------------------------------------------------
  Tables: ekko, ekkn, ckmlhd, likp, bkpf.

  data: l_zterm like ekko-zterm,
        l_aufnr like ekkn-aufnr.

* only for invoice verification, goods receipt document
  read table t_bkpf index 1 into bkpf.
  check bkpf-awtyp = 'RMRP' or bkpf-BLART = 'WE'.

  loop at t_bseg.
* check gr/ir account posting
    check t_bseg-bschl = '96' or t_bseg-bschl = '86'.

* check PO payment terms. (import)
    select single * from  ekko
         where EBELN = t_bseg-ebeln.

* check payment term for import
    l_zterm = ekko-zterm(2).
    check l_zterm = 'DA' or l_zterm = 'DP'
       or l_zterm = 'TT' or l_zterm = 'LC'.

* field fill.
    READ TABLE T_BSEGSUB WITH KEY TABIX = sy-tabix.

* bill of lading from invoice document line text
    if bkpf-awtyp = 'RMRP'.
      T_BSEGSUB-xref3 = t_bseg-sgtxt.
    else.
** ... xblnr after posting is done... so following is unvalid.
*      select single BOLNR into T_BSEGSUB-xref3
*         from likp
*         where vbeln = bkpf-XBLNR.

    endif.

* material/asset/order
    select single aufnr into l_aufnr
        from ekkn
        where EBELN = t_bseg-ebeln
          and EBELP = t_bseg-EBELP.

    if l_AUFNR = space.
      if t_bseg-matnr <> space.
*...... get Cost est number
        select single KALNR into T_BSEGSUB-xref1
          from ckmlhd
          where matnr = t_bseg-matnr.
      endif.
    else.
      T_BSEGSUB-xref1 = l_aufnr.
    endif.

    MODIFY T_BSEGSUB INDEX SY-TABIX.

  endloop.

ENDFUNCTION.
