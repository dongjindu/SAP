*----------------------------------------------------------------------
* Program ID        : ZACOU103
* Title             : [CO] Cost Roll-Up
* Created on        : 08/14/2006
* Created by        : Michelle Jeong
* Specifications By : Andy Choi
* Description       : Cost Roll-Up program.
*                     recalculate costing and save itemization.
*----------------------------------------------------------------------
report zacou103 no standard page heading message-id zmco.

INCLUDE ZACOUI00_OLD.
*include zacoui00.
INCLUDE ZACOU103_OLD_TOP.
*include zacou103_top.

data: l_cousertype(3) type c.
*----------------------------------------------------------------------*
* Select-Options & Parameters
*----------------------------------------------------------------------*
selection-screen begin of block b0 with frame.
parameters: p_kokrs like keko-kokrs obligatory memory id cac,
            p_year  like keko-bdatj obligatory memory id bdtj,
            p_poper like keko-poper obligatory memory id popr.
select-options: s_kalst for keko-kalst,           " Level
                s_matnr for keko-matnr,           " Material
                s_lifnr for ckis-lifnr.           " Vendor
selection-screen end of block b0.
select-options: s_bkmat for keko-matnr.           " break-point

* Block: Costing types
selection-screen begin of block b1 with frame title text-001.
select-options s_kalka for keko-kalka no intervals
                           obligatory memory id kka.
parameters:    p_init  as checkbox default 'X',  " Initialize with BOM
               p_src   as checkbox,              " Price from Cost Est,
               p_small as checkbox default 'X'.  " rollup only 100 entry
selection-screen end of block b1.

* Block: Target Version
selection-screen begin of block b2 with frame title text-006.
parameter: p_ver type ztcou102-ver,
           p_mode type char01 no-display.
selection-screen end of block b2.

parameter: p_break as checkbox    modif id dis.
*Batch update
PARAMETER P_BATCH TYPE CHAR01 NO-DISPLAY MODIF ID RP.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
at selection-screen output.
  perform check_screen.

at selection-screen on value-request for s_kalka-low.
  perform popup_kalka using s_kalka-low 'S_KALKA-LOW'.

initialization.
  get parameter id 'ZCOLV1' field l_cousertype.

*----------------------------------------------------------------------*
* Start of selection
*----------------------------------------------------------------------*
start-of-selection.
  perform get_gt_100.
  if p_mode <> 'L'.

    perform get_products.  " Get FSC / MIP / Module

    describe table f_ckiuser lines sy-subrc.
    if sy-subrc = 0.
      message s000 with 'No data found.'.
      exit.
    endif.

    perform get_t030.

    perform roll_up.
  endif.

*----------------------------------------------------------------------*
* End of selection
*----------------------------------------------------------------------*
end-of-selection.
  if p_mode <> 'L'.
    perform save_roll_up.

    loop at itab_all where kstar is initial.
      write: / itab_all-kalnr, itab_all-compn,
               itab_all-upgvc, itab_all-menge.
    endloop.
  endif.

*&---------------------------------------------------------------------*
*&      Form  get_products
*&---------------------------------------------------------------------*
*       Get Costing Information
*&---------------------------------------------------------------------*
form get_products.
  data lt_keko type table of ckiuser with header line.

  clear: f_ckiuser.
  refresh: f_ckiuser.

  clear  : lt_keko, f_ckiuser1.
  refresh: lt_keko, f_ckiuser1.

  read table s_kalka index 1.
* module costing
  if s_kalka-low = 'M1'.
    select * into corresponding fields of table lt_keko
      from keko
     for all entries in gt_100
     where matnr = gt_100-matnr
       and werks = gt_100-werks
       and matnr in s_matnr
       and kalka in s_kalka
       and tvers = gc_tvers
       and bdatj = p_year
       and poper = p_poper
       and kokrs = p_kokrs.
  else.
    select * into corresponding fields of table lt_keko
      from keko
     where matnr in s_matnr
       and kalka in s_kalka
       and tvers = gc_tvers
       and bdatj = p_year
       and poper = p_poper
       and sobes <> '7'         "exclude stock trf.
       and ( beskz = 'E' or beskz = 'X' )
       and ( kalst >= 1 and kalst in s_kalst )
       and kokrs = p_kokrs.
  endif.


  if sy-subrc = 0.
*   Stock Trf.
    loop at lt_keko.
*     if locked... skip
      read table gt_100 with key matnr = lt_keko-matnr binary search.
      if sy-subrc = 0 and gt_100-lstat = 'X'.
        continue.
      endif.

*      IF lt_keko-sobes = '7'.
*        READ TABLE lt_keko WITH KEY matnr = lt_keko-matnr
*                                    werks = lt_keko-sowrk
*                                    klvar = lt_keko-klvar
*                                    kadky = lt_keko-kadky.
*        IF sy-subrc = 0.
*          MOVE-CORRESPONDING lt_keko TO f_ckiuser.
*
*          APPEND f_ckiuser.
*          CLEAR f_ckiuser.
*        ENDIF.
*
*      ELSE.
      move-corresponding lt_keko to f_ckiuser.

      append f_ckiuser.
      clear f_ckiuser.
*      ENDIF.

    endloop.
  endif.

  sort f_ckiuser.
  delete adjacent duplicates from f_ckiuser.

  sort f_ckiuser by disst descending
                    kalst ascending .    " Sort by level

endform.                    " get_products
*&---------------------------------------------------------------------*
*&      Form  GET_T_KIS1
*&---------------------------------------------------------------------*
*       Get FSC Items
*----------------------------------------------------------------------*
form get_t_kis1.
  clear t_kis1.
  refresh t_kis1.

  select kalka kadky tvers bwvar posnr typps kstar
         matnr gpreis peinh menge meeht ukaln
    into corresponding fields of table t_kis1
       from ckis
       where lednr = '00'             " Standard ledger
         and bzobj = f_ckiuser-bzobj
         and kalnr = f_ckiuser-kalnr
         and kalka = f_ckiuser-kalka
         and kadky = f_ckiuser-kadky
         and tvers = gc_tvers
         and bwvar = f_ckiuser-bwvar
         and ( typps = 'M' or typps = 'I' ).

endform.                    " GET_T_KIS1
*&---------------------------------------------------------------------*
*&      Form  GET_GT_KEKO
*&---------------------------------------------------------------------*
*       Get Component (inc. FSC)
*----------------------------------------------------------------------*
form get_gt_keko.
  clear gt_keko.
  refresh gt_keko.

  select kalnr werks bwdat stlan sobes sowrk kalst
    into corresponding fields of table gt_keko
       from keko
       for all entries in t_kis1
       where bzobj = '0'
         and kalnr = t_kis1-ukaln
         and kalka = t_kis1-kalka
         and tvers = t_kis1-tvers
         and kadky = t_kis1-kadky
         and bwvar = t_kis1-bwvar.

  sort gt_keko by kalnr.

endform.                    " GET_KEKO
*&---------------------------------------------------------------------*
*&      Form  GT_CKIS
*&---------------------------------------------------------------------*
*       Component Item (exc. FSC)
*----------------------------------------------------------------------*
form gt_ckis.
  if not gt_keko[] is initial.
    clear gt_ckis.
    refresh gt_ckis.

    select kalnr kstar hrkft wertn kstar ukaln
      appending corresponding fields of table  gt_ckis
         from ckis
       for all entries in gt_keko
         where lednr = '00'
           and bzobj = f_ckiuser-bzobj
     and ( kalnr <> f_ckiuser-kalnr and kalnr = gt_keko-kalnr )
           and kalka = f_ckiuser-kalka
           and kadky = f_ckiuser-kadky
           and tvers = gc_tvers
           and bwvar = f_ckiuser-bwvar
           and ( typps = 'M' or typps = 'I' ).
    sort gt_ckis by kalnr hrkft.
  endif.

endform.                    " GT_CKIS
*&---------------------------------------------------------------------*
*&      Form  GET_GT_BOM
*&---------------------------------------------------------------------*
*       Read BOM explosion
*----------------------------------------------------------------------*
form get_gt_bom.
  data l_capid type capid.

  check not gt_keko[] is initial.

  clear  : stb, gt_bom, l_capid.
  refresh: stb, gt_bom.

  select single a~capid into l_capid
    from tck19a as a
    join tck03 as b
      on b~aufkz = a~aufkz
   where b~klvar = f_ckiuser-klvar
     and b~kalka = f_ckiuser-kalka
     and b~bwvar = f_ckiuser-bwvar.

  call function 'CS_BOM_EXPL_MAT_V2'
       EXPORTING
            capid = l_capid
            datuv = f_ckiuser-aldat
            mtnrv = f_ckiuser-matnr
            werks = f_ckiuser-werks
            stlan = f_ckiuser-stlan
            stlal = f_ckiuser-stalt
            mehrs = 'X'  " Multi-level explosion
            mmory = '1'  " Memory use On(1)
            sanka = 'X'  " Only Costing Relevency(inc.Phantom)
       TABLES
            stb   = stb.

  loop at stb.
    move-corresponding stb to gt_bom.
    append gt_bom.
  endloop.

endform.                    " GET_GT_BOM
*&---------------------------------------------------------------------*
*&      Form  FILL_ITEMIZATION
*&---------------------------------------------------------------------*
*       Itemization
*----------------------------------------------------------------------*
form fill_itemization.
  data l_idx type sytabix.

  clear l_idx.

  loop at t_kis1.
    l_idx = sy-tabix.

    t_kis1-kalst = f_ckiuser-kalst.       " Level
    t_kis1-bwdat = f_ckiuser-bwdat.       " Valuation date

    modify t_kis1 index l_idx transporting kalst bwdat.
    if t_kis1-ukaln <> space and t_kis1-typps = 'M'.
      read table gt_keko with key kalnr = t_kis1-ukaln binary search.

      if sy-subrc = 0.
        t_kis1-stlan = gt_keko-stlan.      " BOM usage
        modify t_kis1 index l_idx transporting stlan.
      endif.
    endif.

  endloop.

  refresh itab.

  loop at t_kis1 where typps = 'M'.
    clear itab.

*   move header info
    perform move_f_ckiuser_to_itab using f_ckiuser.

*   get component detail
    perform get_compn_info using t_kis1.
  endloop.

endform.                    " FILL_ITEMIZATION
*&---------------------------------------------------------------------*
*&      Form  GET_ITEM_INFO
*&---------------------------------------------------------------------*
*       Get Item info
*----------------------------------------------------------------------*
form get_item_info.
  data l_index type sytabix.

  sort: gt_bom by stufe index posnr ascending hdnfo descending,
        itab by compn posnr.

  loop at gt_bom where dumps = space.
    read table itab with key compn = gt_bom-idnrk
                             upgvc = space
                             indx = 0
                             chk = space.

    if sy-subrc = 0.
      l_index = sy-tabix.

      move-corresponding gt_bom to itab.
      itab-indx  = gt_bom-index.
      itab-chk = 'X'.

      modify itab index l_index transporting indx chk.

    endif.
  endloop.

endform.                    " GET_ITEM_INFO
*&---------------------------------------------------------------------*
*&      Form  GET_UPG
*&---------------------------------------------------------------------*
*       Get UPG
*----------------------------------------------------------------------*
form get_upg.
  data l_key(60).

* get UPG
  sort gt_bom by index stufe ascending.
  loop at gt_bom.
    if gt_bom-stufe = 1.
      w_upg = gt_bom.
    endif.

    read table itab with key indx = gt_bom-index.

    if sy-subrc = 0.
      itab-stgb  = gt_bom-stgb.

*FIXME
*     itab-upgvc = w_upg-idnrk+0(7).
      data: ln type i.
      ln = strlen( w_upg-idnrk ).
      if ln = 9.
        concatenate w_upg-idnrk(7) '0' w_upg-idnrk+7(2) into itab-upgvc.
      else.
        itab-upgvc = w_upg-idnrk.
      endif.

      concatenate itab-artnr itab-werks itab-upgvc itab-compn
                                       into itab-key.
      modify itab transporting upgvc key stgb where indx = gt_bom-index.
    endif.
  endloop.

endform.                    " GET_UPG
*&---------------------------------------------------------------------*
*&      Form  SAVE_ROLL_UP
*&---------------------------------------------------------------------*
form save_roll_up.
  data: l_cnt type i,
        l_text1(60),
        l_text2(60),
        l_chk.

  clear: l_cnt, l_text1, l_text2, l_chk.

  if s_kalka-low <> 'BP'.
    clear p_ver.
  endif.
  loop at f_ckiuser.
    delete from ztcou103
     where kalka = f_ckiuser-kalka
       and kokrs = f_ckiuser-kokrs
       and bdatj = f_ckiuser-bdatj
       and poper = f_ckiuser-poper
       and kalka = f_ckiuser-kalka
       and ver = p_ver
       and artnr = f_ckiuser-matnr
       and werks = f_ckiuser-werks.
  endloop.

  commit work.

  itab_all-aedat = sy-datum.
  itab_all-aenam = sy-uname.
  modify itab_all transporting aedat aenam where aenam = space.

  data: l_cnt1  type i,
        l_cnt2 type i.

  clear: l_cnt1, l_cnt2.

  data: l_idx like ztcou103-indx.
  loop at itab_all.
    at new artnr.
      clear l_idx.
    endat.

    if p_small = 'X'.
      read table gt_100 with key matnr = itab_all-artnr binary search.
      if sy-subrc <> 0.
        continue.
      endif.
    endif.

    l_idx = l_idx + 1.
    itab_all-indx = l_idx.
    move-corresponding itab_all to ztcou103.
    insert ztcou103.

    if sy-subrc = 0.
      l_cnt1 = l_cnt1 + 1.
    else.
      write:/ 'Error saving:',
              itab_all-artnr, itab_all-compn, itab_all-indx.
    endif.
  endloop.

  describe table itab_all lines l_cnt2.

  l_chk = 'X'.
  set parameter id 'ZRU' field l_chk.

  write: /'------------------------------------------------------',
         /'Date:', sy-datum, 'Time:', sy-uzeit, 'User:', sy-uname,
         /'------------------------------------------------------',
         /'Controling area:', p_kokrs,
         /'Piscal year:    ', p_year,
         /'Period:         ' , p_poper,
         /,/'Saved', l_cnt1, 'records.',
         /'------------------------------------------------------'.
  loop at gt_missing.
    at first.
      write:/ '*** Missing Component ***'.
    endat.
    write:/ gt_missing-matnr.
  endloop.

endform.                    " SAVE_ROLL_UP
*&---------------------------------------------------------------------*
*&      Form  MOVE_F_CKIUSER_TO_ITAB
*&---------------------------------------------------------------------*
*       Move Header Info
*----------------------------------------------------------------------*
form move_f_ckiuser_to_itab using f_ckiuser structure ckiuser.
  itab-kalka = f_ckiuser-kalka.   " Costing type
  itab-kokrs = f_ckiuser-kokrs.   " Controlling Area
  itab-bdatj = f_ckiuser-bdatj.   " Posting year
  itab-poper = f_ckiuser-poper.   " Posting period
  itab-werks = f_ckiuser-werks.   " Plant
  itab-artnr = f_ckiuser-matnr.   " Product number
  itab-kalnr = f_ckiuser-kalnr.   " Cost estimate number
  itab-kadky = f_ckiuser-kadky.   " Costing date (key)
  itab-tvers = f_ckiuser-tvers.   " Costing version
  itab-bwvar = f_ckiuser-bwvar.   " Valuation Variant in Costing
  itab-verid = f_ckiuser-verid.   " Production version
  itab-losgr = f_ckiuser-losgr.   " Costing lot size
  itab-stlan = f_ckiuser-stlan.   " BoM Usg

  if itab-kalka = 'BP'.
    itab-ver = p_ver.             " BP Version
  endif.

endform.                    " MOVE_F_CKIUSER_TO_ITAB
*&---------------------------------------------------------------------*
*&      Form  GET_COMPN_INFO
*&---------------------------------------------------------------------*
*       Get component detail
*----------------------------------------------------------------------*
form get_compn_info using p_ckis structure t_kis1.
  data lv_logsr type menge_kpf.

  if not t_kis1-ukaln is initial.
    w_ukaln = t_kis1-ukaln.

*   case of stock transfer
    perform get_stock_trnsf.

    move-corresponding t_kis1 to itab.

    itab-compn = t_kis1-matnr.         " Component
    itab-posnr = t_kis1-posnr.         " BOM Item No.
    itab-menge = t_kis1-menge.         " Qty
    itab-meeht = t_kis1-meeht.         " UoM
    itab-peinh = t_kis1-peinh.

    if not s_bkmat[] is initial and itab-compn in s_bkmat.
      break-point.
    endif.

*   price unit; default price unit = 1
    if t_kis1-peinh = space or
       t_kis1-peinh = 0 or
       t_kis1-peinh > 999.
      select single peinh into itab-peinh
        from mbew
       where matnr = itab-compn
         and bwkey = itab-werks.
    else.
      itab-peinh = t_kis1-peinh.
    endif.

    if itab-peinh = 0.
      itab-peinh = 1.
    endif.

*   get price
    read table v_ztcou103 with key artnr = t_kis1-matnr.
*   ...MIP
    if sy-subrc = 0.
      perform get_price_mip using t_kis1.       " MIP
    else.
*   ...End Part
      perform get_price_end_item.               " case of end item

    endif.

    append itab.
  endif.

endform.                    " GET_COMPN_INFO
*&---------------------------------------------------------------------*
*&      Form  GET_STOCK_TRNSF
*&---------------------------------------------------------------------*
form get_stock_trnsf.
  read table gt_keko with key kalnr = w_ukaln binary search.
  itab-splnt = gt_keko-werks.    " Def.Supply plant

* case of stock transfer, get source plant data
  if gt_keko-sobes = '7'. " Stock Trf
    itab-splnt = gt_keko-sowrk.   " Trf From Plant

* get detail info from supply plant
    read table gt_ckis into w_ckis with key kalnr = t_kis1-ukaln
                                   binary search.
    if sy-subrc = 0.
      w_ukaln = w_ckis-ukaln.
    endif.

    read table gt_keko with key kalnr = w_ukaln binary search.
    if sy-subrc <> 0.
      select * appending corresponding fields of table gt_keko
           from keko
           where bzobj = f_ckiuser-bzobj
             and kalnr = w_ukaln
             and kalka = f_ckiuser-kalka
             and kadky = f_ckiuser-kadky
             and tvers = f_ckiuser-tvers
             and bwvar = f_ckiuser-bwvar
             and klvar = f_ckiuser-klvar
             and werks = itab-splnt.
      sort gt_keko by kalnr.
    endif.

    read table gt_ckis with key kalnr = w_ukaln binary search.
    if sy-subrc <> 0.
      select kalnr kstar hrkft wertn ukaln
        appending corresponding fields of table gt_ckis
           from ckis
           where lednr = '00'             "Standard ledger
             and bzobj = f_ckiuser-bzobj
             and kalnr = w_ukaln
             and kalka = f_ckiuser-kalka
             and kadky = f_ckiuser-kadky
             and tvers = f_ckiuser-tvers
             and bwvar = f_ckiuser-bwvar
             and ( typps = 'M' or typps = 'I' ). "FIXME; V
      sort gt_ckis by kalnr.
    endif.

* replace with origin source
    read table gt_keko with key kalnr = w_ukaln binary search.
    t_kis1-stlan = gt_keko-stlan.
  endif.

endform.                    " GET_STOCK_TRNSF
*&---------------------------------------------------------------------*
*&      Form  GET_PRICE_END_ITEM
*&---------------------------------------------------------------------*
*       Get price of end item
*----------------------------------------------------------------------*
form get_price_end_item.
* price$
  clear gv_changed.

  if p_src = 'X'.
    perform get_price_from_ckis using w_ukaln.
  else.
    perform get_changed_price using t_kis1-matnr.
  endif.

endform.                    " GET_PRICE_END_ITEM
*&---------------------------------------------------------------------*
*&      Form  get_price_mip
*&---------------------------------------------------------------------*
*       Get price of upper level item
*----------------------------------------------------------------------*
form get_price_mip using p_ckis structure t_kis1.
  itab-gpreis = v_ztcou103-wertn.

  itab-wertn = itab-menge * v_ztcou103-wertn  / itab-peinh.
  itab-duty =  itab-menge * v_ztcou103-duty   / itab-peinh.
  itab-frg =   itab-menge * v_ztcou103-frg    / itab-peinh.
  itab-oth =   itab-menge * v_ztcou103-oth    / itab-peinh.

  itab-kstar = t_kis1-kstar.      " v_ztcou103-kstar.
  itab-stkkz = v_ztcou103-stkkz.  " Assy
  itab-stlan = v_ztcou103-stlan.  " BoM Usg

endform.                    " get_price_mip
*&---------------------------------------------------------------------*
*&      Form  roll_up
*&---------------------------------------------------------------------*
form roll_up.
* Rollup
  loop at f_ckiuser.
    if not s_bkmat[] is initial and f_ckiuser-matnr in s_bkmat.
      break-point.
    endif.

    perform get_t_kis1.             " FSC Items

    if not t_kis1[] is initial.
      perform get_gt_keko.            " Component (inc. FSC)
      perform gt_ckis.                " Component Item (exc. FSC)
      perform get_gt_bom.             " Read BOM explosion
      perform get_changed_price_item. " Changed price
      perform fill_itemization.       " Fill Itemization
      perform get_item_info.          " Item info
      perform get_upg.                " UPG

      perform process_module_color.

      perform calc_mip_sum.           " MIP
      perform get_itab_all.
    endif.

  endloop.

endform.                    " roll_up
*&---------------------------------------------------------------------*
*&      Form  calc_mip_sum
*&---------------------------------------------------------------------*
form calc_mip_sum.
  data: l_wertn type zwertn,
        l_duty  type zduty1,
        l_frg   type zfrg1,
        l_oth   type zoth1.

  clear: l_wertn, l_duty, l_frg, l_oth.

  loop at itab.
    l_wertn = l_wertn + itab-wertn.
    l_duty = l_duty + itab-duty.
    l_frg = l_frg + itab-frg.
    l_oth = l_oth + itab-oth.
  endloop.

* MIP
  v_ztcou103-artnr = f_ckiuser-matnr.
  v_ztcou103-wertn = l_wertn.
  v_ztcou103-stkkz = 'X'.               "Assy
  v_ztcou103-stlan = f_ckiuser-stlan.

  v_ztcou103-duty  = l_duty.
  v_ztcou103-frg   = l_frg.
  v_ztcou103-oth   = l_oth.

  append v_ztcou103.

endform.                    " calc_mip_sum
*&---------------------------------------------------------------------*
*&      Form  GET_CHANGED_PRICE
*&---------------------------------------------------------------------*
form get_changed_price using p_matnr type matnr.
  read table gt_102 with key matnr = p_matnr
       binary search.

  if sy-subrc <> 0.
    gt_missing-matnr = p_matnr.
    collect gt_missing.

*    concatenate 'Missing in 102; ' p_matnr into g_txt.
*    call function 'FI_PROGRESS_INDICATOR'
*         EXPORTING
*              text = g_txt.

    if p_break = 'X'.
      break-point.
    endif.

    perform get_price_from_ckis using w_ukaln.
  else.

    data: l_output(10) type p decimals 3,
          l_convsn     type i.

    clear: l_output, l_convsn.
*if 102 has problem, then 0...
    if gt_102-peinh = 0.
      gt_102-peinh = 1.
    endif.

    if gt_102-pmeht = itab-meeht.
      l_output = 1.
      l_convsn = 1.
    else.
      l_convsn = 100.
      call function 'UNIT_CONVERSION_SIMPLE'
           EXPORTING
                input                = gt_102-peinh
                unit_in              = gt_102-pmeht
                unit_out             = itab-meeht
           IMPORTING
                output               = l_output
           EXCEPTIONS
                conversion_not_found = 1
                division_by_zero     = 2
                input_invalid        = 3
                output_invalid       = 4
                overflow             = 5
                type_invalid         = 6
                units_missing        = 7
                unit_in_not_found    = 8
                unit_out_not_found   = 9.
      if sy-subrc <> 0.
        break-point.
      endif.
    endif.

*   Unit price
    itab-gpreis = l_convsn * ( gt_102-wertn +
                    gt_102-duty + gt_102-frg + gt_102-oth ) / l_output.
    itab-peinh  = l_convsn * itab-peinh.

*   if different price of end-item
    gv_changed = 'X'.

*   Price
    itab-wertn = itab-menge * itab-gpreis / itab-peinh.

*   Duty
    itab-duty = itab-menge * gt_102-duty / itab-peinh.

*   Freight
    itab-frg = itab-menge * gt_102-frg / itab-peinh.

*   Other
    itab-oth = itab-menge * gt_102-oth / itab-peinh.

    read table it_t030 with key bklas = gt_102-bklas.
    if sy-subrc = 0.
      itab-kstar = it_t030-konts.
    endif.

  endif.

endform.                    " GET_CHANGED_PRICE
*&---------------------------------------------------------------------*
*&      Form  GET_CHANGED_PRICE_ITEM
*&---------------------------------------------------------------------*
form get_changed_price_item.
  clear gt_102.
  refresh gt_102.

  select ver matnr bklas lifnr peinh pmeht wertn duty frg oth
    into table gt_102
    from ztcou102
   where bdatj = f_ckiuser-bdatj
     and poper = f_ckiuser-poper
     and kalka = f_ckiuser-kalka
     and ver = p_ver.

  sort gt_102 by matnr.

endform.                    " GET_CHANGED_PRICE_ITEM
*&---------------------------------------------------------------------*
*&      Form  get_price_from_ckis
*&---------------------------------------------------------------------*
form get_price_from_ckis using f_kaln.
  data l_wertn type ck_kwt.

  clear l_wertn.

  loop at gt_ckis where kalnr = f_kaln.
    case gt_ckis-hrkft.
      when 'KD-D'.
        itab-duty = t_kis1-menge * gt_ckis-wertn / itab-peinh.
      when 'KD-F'.
        itab-frg = t_kis1-menge * gt_ckis-wertn / itab-peinh.
      when 'KD-O'.
        itab-oth = t_kis1-menge * gt_ckis-wertn / itab-peinh.
      when others.
        l_wertn = t_kis1-menge * gt_ckis-wertn / itab-peinh.
    endcase.

    itab-wertn = l_wertn + itab-duty + itab-frg + itab-oth.

*   cost element
    itab-kstar = gt_ckis-kstar.
  endloop.

endform.                    " get_price_from_ckis
*&---------------------------------------------------------------------*
*&      Form  get_t030
*&---------------------------------------------------------------------*
form get_t030.
  select single ktopl into tka01-ktopl
    from tka01
     where kokrs = p_kokrs.

* pool table
  refresh it_t030.

  select bklas konts
    into table it_t030
    from t030
      where ktopl = tka01-ktopl
        and ktosl = 'GBB'
        and bwmod = '0001'
        and komok = 'VBR'.

  sort it_t030 by bklas.

endform.                                                    " get_t030
*&---------------------------------------------------------------------*
*&      Form  GET_GT_100
*&---------------------------------------------------------------------*
form get_gt_100.
  clear gt_100.
  refresh gt_100.

  select * into table gt_100
    from ztcou100
   where kokrs = p_kokrs
     and kalka in s_kalka
     and bdatj = p_year
     and poper = p_poper.
  if sy-dbcnt > 0.
    read table gt_100 with key lstat = space.
    if sy-subrc <> 0.
      p_mode = 'L'.
    endif.

    sort gt_100 by matnr.
  endif.

* get locked cost info.
  loop at gt_100 where lstat = 'X'.
    select artnr
           sum( wertn ) sum( duty ) sum( frg ) sum( oth )
       appending corresponding fields of table v_ztcou103
       from ztcou103
       where kokrs = p_kokrs
         and kalka in s_kalka
         and bdatj = p_year
         and poper = p_poper
         and artnr = gt_100-matnr
       group by artnr.
  endloop.

endform.                    " GET_GT_100
*&---------------------------------------------------------------------*
*&      Form  get_itab_all
*&---------------------------------------------------------------------*
form get_itab_all.
  data: l_menge type menge_pos,     " Qty
        l_wertn type zwertn,        " Price
        l_duty  type zduty1,        " Duty
        l_frg   type zfrg1,         " Freight
        l_oth   type zoth1.         " Other

  clear: l_menge, l_wertn, l_duty, l_frg, l_oth.

  sort itab by key.

  loop at itab.
    move-corresponding itab to itab_all.

    l_menge = l_menge + itab-menge.
    l_wertn = l_wertn + itab-wertn.
    l_duty  = l_duty + itab-duty.
    l_frg   = l_frg + itab-frg.
    l_oth   = l_oth + itab-oth.

    at end of key.
      itab_all-menge = l_menge.
      itab_all-wertn = l_wertn.
      itab_all-duty  = l_duty.
      itab_all-frg   = l_frg.
      itab_all-oth   = l_oth.

      append itab_all.
      clear: itab_all, l_menge, l_wertn, l_duty, l_frg, l_oth.
    endat.
  endloop.

endform.                    " get_itab_all
*&---------------------------------------------------------------------*
*&      Form  process_module_color
*&---------------------------------------------------------------------*
form process_module_color.

  check s_kalka-low = 'M1'.

  loop at itab where stgb = 'U'.
    data: lw_int_key(3).         " Internal Key Color
    lw_int_key = f_ckiuser-matnr+10(3).
    select count( * ) into sy-dbcnt from ztmm_cp_color
                                    where copit   eq f_ckiuser-matnr
                                      and inkey eq lw_int_key
                                      and submt eq itab-compn
                                      and datab <  f_ckiuser-aldat
                                      and datbi >= f_ckiuser-aldat.
    if sy-subrc <> 0.
      delete itab.
    endif.
  endloop.

endform.                    " process_module_color
*&---------------------------------------------------------------------*
*&      Form  check_screen
*&---------------------------------------------------------------------*
form check_screen.
  if l_cousertype <> 'ADM'.
    loop at screen.
      if screen-group1 = 'DIS'.
        screen-invisible = '1'.
        screen-active    = '0'.
        modify screen.
      endif.
    endloop.
  endif.
endform.                    " check_screen
