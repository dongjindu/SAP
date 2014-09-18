*----------------------------------------------------------------------*
*   INCLUDE ZXCKAU08                                                   *
*----------------------------------------------------------------------*
*"  IMPORTING
*"     VALUE(F_MATBW) LIKE  CKIMATBW STRUCTURE  CKIMATBW
*"     VALUE(IMP_KLVAR) LIKE  KEKO-KLVAR
*"     VALUE(IMP_VALUATION) LIKE  CKIBEW-VALUATION
*"     VALUE(IMP_BZOBJ) LIKE  KEKO-BZOBJ
*"     VALUE(IMP_EKORG) LIKE  CKKALKTAB-EKORG
*"     VALUE(IMP_LIFNR) LIKE  CKKALKTAB-LIFNR
*"     VALUE(IMP_TVERS) LIKE  CKKALKTAB-TVERS
*"     VALUE(IMP_CUOBJ) LIKE  CKKALKTAB-CUOBJ
*"     VALUE(IMP_VBELN) LIKE  KANZ-VBELN
*"     VALUE(IMP_POSNR) LIKE  KANZ-POSNR
*"     VALUE(IMP_AUFNR) LIKE  CKIBEW-AUFNR
*"  EXPORTING
*"     VALUE(EXP_PREIS) LIKE  CKKALKTAB-GPREIS
*"     VALUE(EXP_WAERS) LIKE  CKIBEW-OWAER
*"  CHANGING
*"     VALUE(EXP_PEINH) LIKE  CKKALKTAB-PEINH

tables: tck03.
data: l_bukrs like t001k-bukrs,
      l_yyyymm(6) type c.
data: l_poper type poper.
DATA: l_lifnr TYPE  lifnr,
      l_ekorg TYPE  ekorg,
      l_infnr TYPE  infnr,
      l_text  TYPE  tabname16.
DATA: l_kmein TYPE kmein.

select single * from tck03 where klvar = imp_klvar.
select single bukrs into l_bukrs from t001k
  where bwkey = f_matbw-werks.

*for Debugging------------------------
*SELECT count( * ) into sy-dbcnt FROM ztfi_ctl
*  WHERE categ = 'COPC_EXT' AND flag  = 'X' and zuonr = f_matbw-matnr.
*IF  sy-subrc = 0.
*  BREAK-POINT.
*ENDIF.
*for Debugging------------------------

*-APO WIP VALUATION
if imp_klvar = 'ZPRE'.
  include ZXCKAU08_ZPRE.

*-STD costing of MIP/FSC
elseif imp_klvar = 'PPC1'.
  include ZXCKAU08_ZPP.     "102 table
*-ABP costing of MIP/FSC
elseif imp_klvar = 'ZPCP'.
  include ZXCKAU08_ZBP.     "102 table

*-Raw costing for ABP
elseif imp_klvar = 'ZUBP'.
  include ZXCKAU08_ZPY.     "1) 102, 2) Info, 3)dummy

elseif imp_klvar = 'ZUNF'.  "NAFTA costing (MAP price)
  include ZXCKAU08_ZNF.

* Module, Unit Costing, Raw Costing
* INFO-record (A-quotation price via condition table)
* delivery cost - pb00, ztir, fra1, zoa1, zoth, zoti
elseif tck03-BWVAR(2) = 'ZU' or tck03-BWVAR(2) = 'ZM'
    or tck03-BWVAR(2) = 'ZR'.
  include ZXCKAU08_ZP1.     "1)Info, 2)Exit(Mn)

endif.
