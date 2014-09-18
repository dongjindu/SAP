REPORT ZMMSTKIV .
*&--------------------------------------------------------------------&*
*&   Program: ZMMSTKIV.
*&   Author: Shiva.
*&   Specification:
*&
*&--------------------------------------------------------------------&*
*& Date        User      Transport         Description                &*
*& 10/29/2004  Shiva     UD1K912733         Inventory Balance Report.
*& 11/22/2004  Shiva     UD1K913140         Option to save the ALV
*&                               layout and restrict the year parameter.
*&--------------------------------------------------------------------&*
INCLUDE ZSTKBTOP.
data: wa_fieldcat1 type line of slis_t_fieldcat_alv,
      wa_layout1 type slis_layout_alv.
selection-screen begin of block bk1 with frame.
parameters: p_gjahr like bkpf-gjahr obligatory,
            p_monat like bkpf-monat obligatory.
select-options: s_matnr for wa_mara-matnr,
                s_matkl for wa_mara-matkl,
                s_mtart for wa_mara-mtart,
                s_profl for wa_mara-profl,
                s_dispo for wa_marc-dispo,
                s_ekgrp for wa_marc-ekgrp.
selection-screen end of block bk1.

at selection-screen.
  check sy-ucomm = 'ONLI'.
  concatenate p_gjahr p_monat '01' into w_cdate.
  w_cyear = sy-datum(4).
  w_yrdif = w_cyear - p_gjahr.
  if w_yrdif > 1 or w_yrdif < 0.
    message id 'ZMM01' type 'E' number '009'.
    exit.
  endif.
  if p_gjahr ne w_cyear.
    w_selflg = 2.
    concatenate p_gjahr '12' '31' into w_yedat.    "Month Date
    concatenate w_cyear '01' '01' into w_ybdat.
  else.
    w_selflg = 1.
  endif.
*&--------Need to change this later to run this report just for 3 months
*&--------interval including current month.
  w_3date = w_cdate.
*  concatenate sy-datum(6) '01' into  w_pdate .
*  w_3date = w_pdate - 2.
*  if w_cdate ne w_3date.
*    message id 'ZMM01' type 'E' number '008'.
*    exit.
*  endif.
  w_tdate = sy-datum.
  if s_matnr is initial.
    if s_dispo is initial.
      message id 'ZMM01' type 'E' number '007'.
      exit.
    endif.
  endif.
  if p_monat > 12.
    message id 'ZMM01' type 'E' number '006'.
    exit.
  endif.

start-of-selection.
  perform fill_mvt_ranges.

  if not s_matnr is initial.
    select matnr mtart matkl profl from mara
                                   into table it_mara
                                   where matnr in s_matnr
                                   and   mtart in s_mtart
                                   and   matkl in s_matkl
                                   and   profl in s_profl.
  endif.
  select matnr werks ekgrp dispo
                           from marc
                           into table it_marc
                           where matnr in s_matnr
                           and   ekgrp in s_ekgrp
                           and   dispo in s_dispo
                           and   lvorm eq space.
  if s_matnr is initial.
    select matnr mtart matkl profl from mara
                                   into table it_mara
                                   for all entries in it_marc
                                   where matnr = it_marc-matnr.
  endif.
  sort: it_mara by matnr,
        it_marc by matnr.
  w_idx = 1.
  loop at it_mara into wa_mara.
    loop at it_marc into wa_marc from w_idx.
      if wa_marc-matnr ne wa_mara-matnr.
        w_idx = sy-tabix.
        exit.
      endif.
      wa_matl-matnr = wa_mara-matnr.
      wa_matl-werks = wa_marc-werks.
      wa_matl-mtart = wa_mara-mtart.
      wa_matl-matkl = wa_mara-matkl.
      wa_matl-profl = wa_mara-profl.
      wa_matl-ekgrp = wa_marc-ekgrp.
      wa_matl-dispo = wa_marc-dispo.
      append wa_matl to it_matl.
    endloop.
  endloop.

  case w_selflg.
    when 1.
      select matnr werks bwart shkzg sum( menge ) into table it_mseg
                               from mseg as ta
                               inner join mkpf as tb
                               on ta~mblnr = tb~mblnr
                               and ta~mjahr = tb~mjahr
                               where  tb~mjahr = p_gjahr
                             and   tb~budat between w_3date and w_tdate
                               group by matnr werks bwart shkzg.

    when 2.
      select matnr werks bwart shkzg sum( menge ) into table it_mseg
                               from mseg as ta
                               inner join mkpf as tb
                               on ta~mblnr = tb~mblnr
                               and ta~mjahr = tb~mjahr
                               where ( tb~mjahr = p_gjahr
                           and   tb~budat between w_3date and w_yedat )
                               or   ( tb~mjahr = w_cyear
                           and   tb~budat between w_ybdat and w_tdate )
                               group by matnr werks bwart shkzg.
  endcase.
  delete it_mseg where matnr is initial.
  describe table it_mseg lines w_lines.
  if w_lines eq 0.
    message id 'ZMM01' type 'I' number '010'.
    exit.
  endif.
  sort: it_matl by matnr,
        it_mseg by matnr.

  loop at it_mseg assigning <fs_tst>.
    read table it_matl with key matnr = <fs_tst>-matnr
                                werks = <fs_tst>-werks
                                        binary search
                                        transporting no fields.
    if sy-subrc ne 0.
      <fs_tst>-mkdel = 'X'.
      continue.
    endif.
    wa_result-matnr = <fs_tst>-matnr.
    wa_result-werks = <fs_tst>-werks.
    collect wa_result into it_result.
*&-----------To calculate Goods Receipt and Issue.
    wa_sum_mat-matnr = <fs_tst>-matnr.
    wa_sum_mat-werks = <fs_tst>-werks.
    wa_sum_mat-shkzg = <fs_tst>-shkzg.
    wa_sum_mat-menge = <fs_tst>-menge.
    collect wa_sum_mat into it_sum_mat.
  endloop.
  delete it_mseg where mkdel = 'X'.
*&--------------------Get current period stock.
  select matnr bwkey lbkum stprs peinh
                           into table it_mbew
                           from mbew
                           for all entries in it_result
                           where matnr = it_result-matnr
                           and   bwkey = it_result-werks
                           and   lvorm = space.
*&----------Get Stock value.
  select matnr werks sum( labst ) sum( insme ) sum( speme )
                     sum( einme ) sum( retme )
                     into table it_mard
                     from mard
                     where matnr in s_matnr
                     group by matnr werks.
  perform sum_stk_qty.
  perform sum_mvt_qty.
  perform build_result.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
   EXPORTING
     I_PROGRAM_NAME               = 'ZMMSTKIV'
     I_INTERNAL_TABNAME           = 'WA_RESULT'
*   I_STRUCTURE_NAME             =
*   I_CLIENT_NEVER_DISPLAY       = 'X'
     I_INCLNAME                   = 'ZSTKBTOP'
*   I_BYPASSING_BUFFER           =
*   I_BUFFER_ACTIVE              =
    CHANGING
      CT_FIELDCAT                  = it_fieldcat
   EXCEPTIONS
     INCONSISTENT_INTERFACE       = 1
     PROGRAM_ERROR                = 2
     OTHERS                       = 3 .
  IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
  perform format_fieldcatlog.
  perform display_alv.

*&---------------------------------------------------------------------*
*&      Form  fill_mvt_ranges
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_mvt_ranges.

  perform build_ranges using 'r_gr_bwart' '101'.
  append r_gr_bwart.
  perform build_ranges using 'r_gr_bwart' '102'.
  append r_gr_bwart.
  perform build_ranges using 'r_gr_bwart' '122'.
  append r_gr_bwart.
  perform build_ranges using 'r_gr_bwart' '561'.
  append r_gr_bwart.
  perform build_ranges using 'r_gr_bwart' '562'.
  append r_gr_bwart.
  perform build_ranges using 'r_gip_bwart' '261'.
  append r_gip_bwart.
  perform build_ranges using 'r_gip_bwart' '262'.
  append r_gip_bwart.
  perform build_ranges using 'r_gis_bwart' '551'.
  append r_gis_bwart.
  perform build_ranges using 'r_gis_bwart' '552'.
  append r_gis_bwart.
  perform build_ranges using 'r_gis_bwart' '555'.
  append r_gis_bwart.
  perform build_ranges using 'r_gis_bwart' '556'.
  append r_gis_bwart.
  perform build_ranges using 'r_cct_bwart' '701'.
  append r_cct_bwart.
  perform build_ranges using 'r_cct_bwart' '702'.
  append r_cct_bwart.
  perform build_ranges using 'r_cct_bwart' '711'.
  append r_cct_bwart.
  perform build_ranges using 'r_cct_bwart' '712'.
  append r_cct_bwart.
  perform build_ranges using 'r_stk_bwart' '301'.
  append r_stk_bwart.
  perform build_ranges using 'r_stk_bwart' '302'.
  append r_stk_bwart.
  perform build_ranges using 'r_stk_bwart' '309'.
  append r_stk_bwart.
  perform build_ranges using 'r_stk_bwart' '310'.
  append r_stk_bwart.

  clear: r_gr_bwart, r_gip_bwart, r_gis_bwart, r_cct_bwart, r_stk_bwart.
ENDFORM.                    " fill_mvt_ranges
*&---------------------------------------------------------------------*
*&      Form  build_ranges
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0112   text
*      -->P_0113   text
*----------------------------------------------------------------------*
FORM build_ranges USING value(p_range)
                        value(p_bwart).

  field-symbols: <fs_ran> like line of r_gr_bwart.

  assign (p_range) to <fs_ran>.
  <fs_ran>-sign = 'I'.
  <fs_ran>-option = 'EQ'.
  <fs_ran>-low = p_bwart.
*&-------------Miscellaneous
  if r_mis_bwart[] is initial.
    r_mis_bwart-sign = 'E'.
    r_mis_bwart-option = 'EQ'.
    r_mis_bwart-low = '311'.
    append r_mis_bwart.
    r_mis_bwart-low = '312'.
    append r_mis_bwart.
  endif.
  r_mis_bwart-sign = 'E'.
  r_mis_bwart-option = 'EQ'.
  r_mis_bwart-low = p_bwart.
  append r_mis_bwart.

ENDFORM.                    " build_ranges
*&---------------------------------------------------------------------*
*&      Form  sum_mvt_qty
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM sum_mvt_qty.
  sort it_mseg by matnr werks.
*&-------Goods Receipt.
  loop at it_mseg into wa_mseg
                  where bwart in r_gr_bwart.
    perform calculate_qty using wa_mseg 'GR' space.
  endloop.
  perform calculate_qty using wa_mseg 'GR' 'L'.
*&------Goods Issue for Production
  loop at it_mseg into wa_mseg
                  where bwart in r_gip_bwart.
    perform calculate_qty using wa_mseg 'GIP' space.
  endloop.
  perform calculate_qty using wa_mseg 'GIP' 'L'.
*&------Goods Issue for scrap.
  loop at it_mseg into wa_mseg
                  where bwart in r_gis_bwart.
    perform calculate_qty using wa_mseg 'GIS' space.
  endloop.
  perform calculate_qty using wa_mseg 'GIS' 'L'.
*&---------Miscellaneous.
  loop at it_mseg into wa_mseg
                  where bwart in r_mis_bwart.
    perform calculate_qty using wa_mseg 'MIS' space.
  endloop.
  perform calculate_qty using wa_mseg 'MIS' 'L'.
*&-----Cycle count.
  loop at it_mseg into wa_mseg
                  where bwart in r_cct_bwart.
    perform calculate_qty using wa_mseg 'CCT' space.
  endloop.
  perform calculate_qty using wa_mseg 'CCT' 'L'.
*&-----Stock transfer
  loop at it_mseg into wa_mseg
                  where bwart in r_stk_bwart.
    perform calculate_qty using wa_mseg 'STK' space.
  endloop.
  perform calculate_qty using wa_mseg 'STK' 'L'.
ENDFORM.                    " sum_mvt_qty
*&---------------------------------------------------------------------*
*&      Form  build_result
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_result.

  data: w_goods like mseg-menge,
        w_gdsrp like mseg-menge,
        w_gdsis like mseg-menge,
        w_stock like mard-labst,
        w_price_per like mbew-stprs.

  field-symbols: <fs_result> like line of it_result.

  loop at it_grp_mat into wa_grp_mat
                     where mvgrp  = 'GR'.
    wa_result-gr_menge = wa_grp_mat-menge.
    modify it_result from wa_result transporting gr_menge
                                    where matnr = wa_grp_mat-matnr
                                    and   werks = wa_grp_mat-werks.
  endloop.
  loop at it_grp_mat into wa_grp_mat
                     where mvgrp  = 'GIP'.
    wa_result-gip_menge = wa_grp_mat-menge.
    modify it_result from wa_result transporting gip_menge
                                    where matnr = wa_grp_mat-matnr
                                    and   werks = wa_grp_mat-werks.
  endloop.
  loop at it_grp_mat into wa_grp_mat
                     where mvgrp  = 'GIS'.
    wa_result-gis_menge = wa_grp_mat-menge.
    modify it_result from wa_result transporting gis_menge
                                    where matnr = wa_grp_mat-matnr
                                    and   werks = wa_grp_mat-werks.
  endloop.
  loop at it_grp_mat into wa_grp_mat
                     where mvgrp  = 'MIS'.
    wa_result-gim_menge = wa_grp_mat-menge.
    modify it_result from wa_result transporting gim_menge
                                    where matnr = wa_grp_mat-matnr
                                    and   werks = wa_grp_mat-werks.
  endloop.
  loop at it_grp_mat into wa_grp_mat
                     where mvgrp  = 'CCT'.
    wa_result-cct_menge = wa_grp_mat-menge.
    modify it_result from wa_result transporting cct_menge
                                    where matnr = wa_grp_mat-matnr
                                    and   werks = wa_grp_mat-werks.
  endloop.
  loop at it_grp_mat into wa_grp_mat
                     where mvgrp  = 'STK'.
    wa_result-stk_menge = wa_grp_mat-menge.
    modify it_result from wa_result transporting stk_menge
                                    where matnr = wa_grp_mat-matnr
                                    and   werks = wa_grp_mat-werks.
  endloop.

  sort: it_stk_info by matnr werks,
        it_sum_mat  by matnr werks shkzg,
        it_mbew     by matnr bwkey.

  loop at it_result assigning <fs_result>.
    clear: w_goods,w_gdsrp, w_gdsis, w_stock.
    read table it_stk_info into wa_stk_info
                           with key matnr = <fs_result>-matnr
                                    werks = <fs_result>-werks
                                    binary search
                                    transporting menge.
    if sy-subrc ne 0.
      w_stock = 0.
    else.
      w_stock = wa_stk_info-menge.
    endif.
    read table it_sum_mat into wa_sum_mat
                          with key matnr = <fs_result>-matnr
                                   werks = <fs_result>-werks
                                   shkzg = 'S'
                                   binary search
                                   transporting menge.
    if sy-subrc ne 0.
      w_gdsrp = 0.
    else.
      w_gdsrp = wa_sum_mat-menge.
    endif.
    read table it_sum_mat into wa_sum_mat
                          with key matnr = <fs_result>-matnr
                                   werks = <fs_result>-werks
                                   shkzg = 'H'
                                   binary search
                                   transporting menge.
    if sy-subrc ne 0.
      w_gdsis = 0.
    else.
      w_gdsis = wa_sum_mat-menge.
    endif.

    <fs_result>-bs_menge = w_stock - ( w_gdsrp - w_gdsis ).
    read table it_mbew into wa_mbew with key matnr = <fs_result>-matnr
                                             bwkey = <fs_result>-werks
                                             binary search
                                         transporting lbkum stprs peinh.
    if sy-subrc ne 0.
      <fs_result>-cs_menge  = 0.
      <fs_result>-stprs     = 0.
      <fs_result>-bs_netpr  = 0.
      <fs_result>-gr_netpr  = 0.
      <fs_result>-gip_netpr = 0.
      <fs_result>-gis_netpr = 0.
      <fs_result>-gim_netpr = 0.
      <fs_result>-cct_netpr = 0.
      <fs_result>-stk_netpr = 0.
      <fs_result>-cs_netpr  = 0.
    else.
      <fs_result>-stprs     = wa_mbew-stprs.
      <fs_result>-cs_menge  = wa_mbew-lbkum.
      w_price_per = wa_mbew-stprs / wa_mbew-peinh.
      <fs_result>-bs_netpr  = <fs_result>-bs_menge * w_price_per.
      <fs_result>-gr_netpr  = <fs_result>-gr_menge * w_price_per.
      <fs_result>-gip_netpr = <fs_result>-gip_menge * w_price_per.
      <fs_result>-gis_netpr = <fs_result>-gis_menge * w_price_per.
      <fs_result>-gim_netpr = <fs_result>-gim_menge * w_price_per.
      <fs_result>-cct_netpr = <fs_result>-cct_menge * w_price_per.
      <fs_result>-stk_netpr = <fs_result>-stk_menge * w_price_per.
      <fs_result>-cs_netpr  = <fs_result>-cs_menge * w_price_per.
    endif.
  endloop.
ENDFORM.                    " build_result
*&---------------------------------------------------------------------*
*&      Form  format_fieldcatlog
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM format_fieldcatlog.
  wa_fieldcat1-key = 'X'.
  modify it_fieldcat from wa_fieldcat1
                     transporting key
                     where fieldname = 'MATNR'
                     and   tabname   = 'WA_RESULT'.
  modify it_fieldcat from wa_fieldcat1
                     transporting key
                     where fieldname = 'WERKS'
                     and   tabname   = 'WA_RESULT'.
  clear wa_fieldcat1.
  wa_fieldcat1-seltext_l = 'Begin Stock Balance'.
  wa_fieldcat1-ddictxt   = 'L'.
  wa_fieldcat1-do_sum    = 'X'.
  modify it_fieldcat from wa_fieldcat1
                     transporting seltext_l ddictxt do_sum
                     where fieldname = 'BS_MENGE'
                     and   tabname   = 'WA_RESULT'.
  clear wa_fieldcat1.
  wa_fieldcat1-seltext_l = 'Goods Receipt'.
  wa_fieldcat1-ddictxt   = 'L'.
  wa_fieldcat1-do_sum    = 'X'.
  modify it_fieldcat from wa_fieldcat1
                     transporting seltext_l ddictxt do_sum
                     where fieldname = 'GR_MENGE'
                     and   tabname   = 'WA_RESULT'.
  clear wa_fieldcat1.
  wa_fieldcat1-seltext_l = 'GI for Production'.
  wa_fieldcat1-ddictxt   = 'L'.
  wa_fieldcat1-do_sum    = 'X'.
  modify it_fieldcat from wa_fieldcat1
                     transporting seltext_l ddictxt do_sum
                     where fieldname = 'GIP_MENGE'
                     and  tabname   = 'WA_RESULT'.
  clear wa_fieldcat1.
  wa_fieldcat1-seltext_l = 'GI Miscellaneous'.
  wa_fieldcat1-ddictxt   = 'L'.
  wa_fieldcat1-do_sum    = 'X'.
  modify it_fieldcat from wa_fieldcat1
                     transporting seltext_l ddictxt do_sum
                     where fieldname = 'GIM_MENGE'
                     and  tabname   = 'WA_RESULT'.

  clear wa_fieldcat1.
  wa_fieldcat1-seltext_l = 'GI for Scrap'.
  wa_fieldcat1-ddictxt   = 'L'.
  wa_fieldcat1-do_sum    = 'X'.
  modify it_fieldcat from wa_fieldcat1
                     transporting seltext_l ddictxt do_sum
                     where fieldname = 'GIS_MENGE'
                     and   tabname   = 'WA_RESULT'.
  clear wa_fieldcat1.
  wa_fieldcat1-seltext_l = 'Cycle count'.
  wa_fieldcat1-ddictxt   = 'L'.
  wa_fieldcat1-do_sum    = 'X'.
  modify it_fieldcat from wa_fieldcat1
                     transporting seltext_l ddictxt do_sum
                     where fieldname = 'CCT_MENGE'
                     and  tabname   = 'WA_RESULT'.
  clear wa_fieldcat1.
  wa_fieldcat1-seltext_l = 'Stock Transfer'.
  wa_fieldcat1-ddictxt   = 'L'.
  wa_fieldcat1-do_sum    = 'X'.
  modify it_fieldcat from wa_fieldcat1
                     transporting seltext_l ddictxt do_sum
                     where fieldname = 'STK_MENGE'
                     and   tabname   = 'WA_RESULT'.

  clear wa_fieldcat1.
  wa_fieldcat1-seltext_l = 'End Stock Balance'.
  wa_fieldcat1-ddictxt   = 'L'.
  wa_fieldcat1-do_sum    = 'X'.
  modify it_fieldcat from wa_fieldcat1
                     transporting seltext_l ddictxt do_sum
                     where fieldname = 'CS_MENGE'
                     and   tabname   = 'WA_RESULT'.

  clear wa_fieldcat1.
  wa_fieldcat1-seltext_l = 'Begin Stock Netprice'.
  wa_fieldcat1-ddictxt   = 'L'.
  wa_fieldcat1-no_out    = 'X'.
  modify it_fieldcat from wa_fieldcat1
                     transporting seltext_l ddictxt no_out
                     where fieldname = 'BS_NETPR'
                     and   tabname   = 'WA_RESULT'.

  clear wa_fieldcat1.
  wa_fieldcat1-seltext_l = 'GR Netprice'.
  wa_fieldcat1-ddictxt   = 'L'.
  wa_fieldcat1-no_out    = 'X'.
  modify it_fieldcat from wa_fieldcat1
                     transporting seltext_l ddictxt no_out
                     where fieldname = 'GR_NETPR'
                     and   tabname   = 'WA_RESULT'.

  clear wa_fieldcat1.
  wa_fieldcat1-seltext_l = 'GIP Netprice'.
  wa_fieldcat1-ddictxt   = 'L'.
  wa_fieldcat1-no_out    = 'X'.
  modify it_fieldcat from wa_fieldcat1
                     transporting seltext_l ddictxt no_out
                     where fieldname = 'GIP_NETPR'
                     and   tabname   = 'WA_RESULT'.

  clear wa_fieldcat1.
  wa_fieldcat1-seltext_l = 'GIS Netprice'.
  wa_fieldcat1-ddictxt   = 'L'.
  wa_fieldcat1-no_out    = 'X'.
  modify it_fieldcat from wa_fieldcat1
                     transporting seltext_l ddictxt no_out
                     where fieldname = 'GIS_NETPR'
                     and   tabname   = 'WA_RESULT'.
  clear wa_fieldcat1.
  wa_fieldcat1-seltext_l = 'GIM Netprice'.
  wa_fieldcat1-ddictxt   = 'L'.
  wa_fieldcat1-no_out    = 'X'.
  modify it_fieldcat from wa_fieldcat1
                     transporting seltext_l ddictxt no_out
                     where fieldname = 'GIM_NETPR'
                     and   tabname   = 'WA_RESULT'.

  clear wa_fieldcat1.
  wa_fieldcat1-seltext_l = 'Cycle count Netprice'.
  wa_fieldcat1-ddictxt   = 'L'.
  wa_fieldcat1-no_out    = 'X'.
  modify it_fieldcat from wa_fieldcat1
                     transporting seltext_l ddictxt no_out
                     where fieldname = 'CCT_NETPR'
                     and   tabname   = 'WA_RESULT'.
  clear wa_fieldcat1.
  wa_fieldcat1-seltext_l = 'Stock transfer Netprice'.
  wa_fieldcat1-ddictxt   = 'L'.
  wa_fieldcat1-no_out    = 'X'.
  modify it_fieldcat from wa_fieldcat1
                     transporting seltext_l ddictxt no_out
                     where fieldname = 'STK_NETPR'
                     and   tabname   = 'WA_RESULT'.

  clear wa_fieldcat1.
  wa_fieldcat1-seltext_l = 'End Stock Netprice'.
  wa_fieldcat1-ddictxt   = 'L'.
  wa_fieldcat1-no_out    = 'X'.
  modify it_fieldcat from wa_fieldcat1
                     transporting seltext_l ddictxt no_out
                     where fieldname = 'CS_NETPR'
                     and   tabname   = 'WA_RESULT'.

ENDFORM.                    " format_fieldcatlog
*&---------------------------------------------------------------------*
*&      Form  sum_stk_qty
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM sum_stk_qty.

  data: w_stk_qty like mard-labst.
  field-symbols: <fs_stk> like line of it_mard.

  loop at it_mard assigning <fs_stk>.
    w_stk_qty = <fs_stk>-labst + <fs_stk>-insme + <fs_stk>-speme
                + <fs_stk>-einme + <fs_stk>-retme.
    wa_stk_info-matnr = <fs_stk>-matnr.
    wa_stk_info-werks = <fs_stk>-werks.
    wa_stk_info-menge = w_stk_qty.
    collect wa_stk_info into it_stk_info.
  endloop.

ENDFORM.                    " sum_stk_qty
*&---------------------------------------------------------------------*
*&      Form  calculate_qty
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM calculate_qty using p_mseg structure wa_mseg p_mvgrp p_recflg.

  statics: w_prev_matnr like mseg-matnr,
           w_prev_werks like mseg-werks,
           w_calc_qty   like mseg-menge.
  case p_recflg.
    when space.
      if w_prev_matnr ne p_mseg-matnr or w_prev_werks ne p_mseg-werks.
        if sy-tabix eq 1.
        else.
          wa_grp_mat-matnr =  w_prev_matnr.
          wa_grp_mat-werks = w_prev_werks.
          wa_grp_mat-mvgrp = p_mvgrp.
          wa_grp_mat-menge = w_calc_qty.
          collect wa_grp_mat into it_grp_mat.
        endif.
        clear: w_prev_matnr, w_prev_werks, w_calc_qty.
        w_prev_matnr = p_mseg-matnr.
        w_prev_werks = p_mseg-werks.
      endif.
      case p_mseg-shkzg.
        when 'S'.
          w_calc_qty = w_calc_qty +  p_mseg-menge.
        when 'H'.
          w_calc_qty = w_calc_qty - p_mseg-menge.
      endcase.
    when 'L'.
*To handle the last record of the loop.
      wa_grp_mat-matnr =  w_prev_matnr.
      wa_grp_mat-werks = w_prev_werks.
      wa_grp_mat-mvgrp = p_mvgrp.
      wa_grp_mat-menge = w_calc_qty.
      collect wa_grp_mat into it_grp_mat.
      clear: wa_grp_mat,w_prev_matnr, w_prev_werks, w_calc_qty.
  endcase.

ENDFORM.                    " calculate_qty
*&---------------------------------------------------------------------*
*&      Form  display_alv
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_alv.
  data: wa_fieldsort type line of slis_t_sortinfo_alv.

  wa_layout1-get_selinfos = 'X'.
  wa_layout1-totals_text = 'Tot'.
  wa_layout1-header_text = 'Testing Header'.

  wa_fieldsort-spos = 1.
  wa_fieldsort-fieldname = 'MATNR'.
  wa_fieldsort-tabname   = 'WA_RESULT'.
  wa_fieldsort-spos = 2.
  wa_fieldsort-fieldname = 'WERKS'.
  wa_fieldsort-tabname   = 'WA_RESULT'.
  append wa_fieldsort to it_fieldsort.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
   EXPORTING
*   I_INTERFACE_CHECK                 = ' '
    I_BYPASSING_BUFFER                = 'X'
*   I_BUFFER_ACTIVE                   = ' '
     I_CALLBACK_PROGRAM                = 'ZMMSTKIN'
*   I_CALLBACK_PF_STATUS_SET          = ' '
*   I_CALLBACK_USER_COMMAND           = ' '
*   I_CALLBACK_TOP_OF_PAGE            = ' '
*   I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*   I_CALLBACK_HTML_END_OF_LIST       = ' '
    I_STRUCTURE_NAME                  = 'WA_RESULT'
*   I_BACKGROUND_ID                   = ' '
*   I_GRID_TITLE                      =
*   I_GRID_SETTINGS                   =
     IS_LAYOUT                         = wa_layout1
     IT_FIELDCAT                       = it_fieldcat[]
*   IT_EXCLUDING                      =
*   IT_SPECIAL_GROUPS                 =
      IT_SORT                           = it_fieldsort
*   IT_FILTER                         =
*   IS_SEL_HIDE                       =
*   I_DEFAULT                         = 'X'
    I_SAVE                            = 'A'
*   IS_VARIANT                        =
*   IT_EVENTS                         =
*   IT_EVENT_EXIT                     =
*   IS_PRINT                          =
*   IS_REPREP_ID                      =
*   I_SCREEN_START_COLUMN             = 0
*   I_SCREEN_START_LINE               = 0
*   I_SCREEN_END_COLUMN               = 0
*   I_SCREEN_END_LINE                 = 0
*   IT_ALV_GRAPHICS                   =
*   IT_ADD_FIELDCAT                   =
*   IT_HYPERLINK                      =
*   I_HTML_HEIGHT_TOP                 =
*   I_HTML_HEIGHT_END                 =
*   IT_EXCEPT_QINFO                   =
* IMPORTING
*   E_EXIT_CAUSED_BY_CALLER           =
*   ES_EXIT_CAUSED_BY_USER            =
    TABLES
      T_OUTTAB                          = it_result[]
   EXCEPTIONS
     PROGRAM_ERROR                     = 1
     OTHERS                            = 2 .
  IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " display_alv
