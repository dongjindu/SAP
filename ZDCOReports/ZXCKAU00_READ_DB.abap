*----------------------------------------------------------------------*
*   INCLUDE ZXCKAU00_READ_DB                                           *
*----------------------------------------------------------------------*
*Commented by ANDY
*data: lt_keko  like keko occurs 0 with header line.
*data: lt_ckis  like ckis occurs 0 with header line.
*data: w_ckis  like ckis.
*data: w_wertn like kis1-wertn.
*data: w_ukaln like ckis-ukaln.
*
**if single item costing...
*describe table t_kis1 lines sy-index.
*if sy-index = 0. append t_kis1. endif.
*
**Component (inc. FSC) within plant
*select * into corresponding fields of table lt_keko
*     from keko
*     for all entries in t_kis1
*     where bzobj = f_ckiuser-bzobj
*       and kalnr = t_kis1-ukaln
*       and kalka = f_ckiuser-kalka
*       and kadky = f_ckiuser-kadky
*       and tvers = f_ckiuser-tvers
*       and bwvar = f_ckiuser-bwvar
*       and klvar = f_ckiuser-klvar
*       and werks = w_werks. "f_ckiuser-werks.
*sort lt_keko by kalnr.
*
**Component Item (exc. FSC)
*describe table lt_keko lines sy-tabix.
*if sy-tabix > 0.
*  select * into corresponding fields of table lt_ckis
*     from ckis
*     for all entries in lt_keko
*     where lednr = '00'             "Standard ledger
*       and bzobj = f_ckiuser-bzobj
*       and ( kalnr <> f_ckiuser-kalnr and kalnr = lt_keko-kalnr )
*       and kalka = f_ckiuser-kalka
*       and kadky = f_ckiuser-kadky
*       and tvers = f_ckiuser-tvers
*       and bwvar = f_ckiuser-bwvar
*       and ( typps = 'M' or typps = 'I' ). "FIXME; V
*  sort lt_ckis by kalnr hrkft.
*endif.



*get 1st level BOM
select idnrk maktx into corresponding fields of table lt_upg
  from stpo as s
  inner join makt as t
     on t~matnr = s~idnrk
    and t~spras = sy-langu
  where stlty = 'M'
    and stlnr =  f_ckiuser-stnum
    and datuv <= f_ckiuser-aldat.

check sy-subrc = 0.


include zxckau0z_ldc.


**read BOM explosion
** << Modify by Michelle on 02/10/2006
*
*clear l_capid.
*select single a~capid into l_capid
*  from tck19a as a
*  join tck03 as b
*    on b~aufkz = a~aufkz
* where b~klvar = f_ckiuser-klvar
*   and b~kalka = f_ckiuser-kalka
*   and b~bwvar = f_ckiuser-bwvar.
** Modify by Michelle on 02/10/2006 >>
*
*call function 'CS_BOM_EXPL_MAT_V2'
*     exporting
*          capid = l_capid
*          datuv = f_ckiuser-aldat
*          mtnrv = f_ckiuser-matnr
*          werks = f_ckiuser-werks
*          stlan = f_ckiuser-stlan
*          stlal = f_ckiuser-stalt
*          mehrs = 'X'  "Multi-level explosion
*          mmory = '1'  "Memory use On(1)
*          sanka = 'X'  "Only Costing Relevency(inc.Phantom)
*     tables
*          stb   = stb.
*loop at stb.
*  move-corresponding stb to lt_bom. append lt_bom.
*endloop.
*
**fill Itemization
*loop at t_kis1 where typps = 'M'.
*  clear itab.
*  move-corresponding t_kis1 to itab.
*
*  itab-kokrs  = f_ckiuser-kokrs.
*  itab-klvar  = f_ckiuser-klvar.
*  itab-artnr  = f_ckiuser-matnr.
*  itab-werks  = f_ckiuser-werks.
*  itab-bdatj  = f_ckiuser-bdatj.
*  itab-poper  = f_ckiuser-poper.
*  itab-verid  = f_ckiuser-verid. "Production version
*
*  itab-bwdat  = f_ckiuser-bwdat. "Val.date
*  itab-aldat  = f_ckiuser-aldat. "Qty.date
*  itab-stalt  = f_ckiuser-stalt. "Alt.BOM
*
*  itab-compn = t_kis1-matnr.
*  itab-reqqt = t_kis1-menge.
*
**For Debugging------------------
**  SELECT COUNT( * ) INTO SY-DBCNT FROM ZTFI_CTL
**    WHERE CATEG = 'ZXCKAU03' AND FLAG = 'X' AND ZUONR = T_KIS1-MATNR.
**  IF SY-SUBRC = 0.
**    BREAK-POINT.
**  ENDIF.
**For Debugging------------------
*
** get component detail
*  if not t_kis1-ukaln is initial.
*    w_ukaln = t_kis1-ukaln.
*
**---Plant!!!
*    read table lt_keko with key kalnr = w_ukaln  binary search.
*    itab-splnt = lt_keko-werks.  "Def.Supply plant
*
**---Stock transfer, get source plant data
*    if lt_keko-sobes = '7'. "Stock Trf
*      itab-splnt = lt_keko-sowrk.   "Trf From Plant
*
**.... get detail info from supply plant
*      read table lt_ckis into w_ckis with key kalnr = t_kis1-ukaln.
*      w_ukaln    = w_ckis-ukaln.
*      itab-strat = w_ckis-strat.   "Pricing Strategy
*
*      read table lt_keko with key kalnr = w_ukaln.
**---- select missing items
*      if sy-subrc <> 0.
*        select * appending corresponding fields of table lt_keko
*             from keko
*             where bzobj = f_ckiuser-bzobj
*               and kalnr = w_ukaln
*               and kalka = f_ckiuser-kalka
*               and kadky = f_ckiuser-kadky
*               and tvers = f_ckiuser-tvers
*               and bwvar = f_ckiuser-bwvar
*               and klvar = f_ckiuser-klvar
*               and werks = itab-splnt.
*        select * appending corresponding fields of table lt_ckis
*             from ckis
*             where lednr = '00'             "Standard ledger
*               and bzobj = f_ckiuser-bzobj
*               and kalnr = w_ukaln
*               and kalka = f_ckiuser-kalka
*               and kadky = f_ckiuser-kadky
*               and tvers = f_ckiuser-tvers
*               and bwvar = f_ckiuser-bwvar.
*        read table lt_keko with key kalnr = w_ukaln.
*      endif.
*    endif.
*
**---if end-item, get price detail..(EndItem - BOM usage blank)
*    if lt_keko-stlan is initial.
*
*      clear w_wertn.
*      loop at lt_ckis where kalnr = w_ukaln.
*        if lt_ckis-lifnr <> space.
*          itab-lifnr = lt_ckis-lifnr.
*          itab-infnr = lt_ckis-infnr.
*        endif.
**-------- LDC rate / price unit (no BOM qty)
*        case lt_ckis-hrkft.
*          when 'KD-D'.
*            itab-amtdt = t_kis1-menge * lt_ckis-wertn / lt_ckis-peinh.
*          when 'KD-F'.
*            itab-amtft = t_kis1-menge * lt_ckis-wertn / lt_ckis-peinh.
*          when 'KD-O'.
*            itab-amtot = t_kis1-menge * lt_ckis-wertn / lt_ckis-peinh.
*          when others.
*        endcase.
*
*        w_wertn   = w_wertn + lt_ckis-wertn.
*      endloop.
*
**---- exeption case...why?..unknown.
*      if itab-wertn = 0.
*        itab-wertn  = itab-reqqt * w_wertn / lt_ckis-peinh.
*        itab-gpreis = w_wertn.
*      endif.
*    endif.
*  endif.
*
*  append itab.
*** end item
**  IF lt_keko-stlan IS INITIAL.
*** MIP - enhance me later
**  ELSE.
**  ENDIF.
*endloop.
*
**get vendor name
*select lifnr name1 into corresponding fields of table t_lfa1
*                   from lfa1
*                   for all entries in itab
*                   where lifnr = itab-lifnr.
*sort t_lfa1 by lifnr.
*
**get Item info
*
*sort lt_bom by stufe index posnr ascending hdnfo descending. "SAP
*sort itab by compn posnr.
*
** << Modified on 10.12.2006 by Michelle
*loop at lt_bom where dumps = space.
*  read table itab with key compn = lt_bom-idnrk
*                           upgvc = space
*                           indx = 0
*                           chk = space.
*
*  if sy-subrc = 0.
*    l_index = sy-tabix.
*    move-corresponding lt_bom to itab.
*    itab-indx  = lt_bom-index.
*
*    read table t_lfa1 with key lifnr = itab-lifnr.
*    if sy-subrc = 0.
*      itab-name1 = t_lfa1-name1.
*    endif.
*
*    itab-chk = 'X'.
*
*    modify itab index l_index transporting indx chk.
*  endif.
*endloop.
** Modified on 10.12.2006 by Michelle >>
*
**get UPG
*sort lt_bom by index stufe ascending.
*loop at lt_bom.
*  if lt_bom-stufe = 1.
*    w_upg = lt_bom.
*  endif.
*
*  read table itab with key indx = lt_bom-index.
*
*  if sy-subrc = 0.
*    itab-upgvc = w_upg-idnrk.
**...UPG text
*    read table lt_upg with key idnrk = w_upg-idnrk.
*
*    if sy-subrc = 0.
*      itab-upgtx = lt_upg-maktx.
*    endif.
*
*    modify itab transporting upgvc upgtx where indx = lt_bom-index.
*  endif.
*endloop.
** Modify by Michelle on 02/10/2006 >>
