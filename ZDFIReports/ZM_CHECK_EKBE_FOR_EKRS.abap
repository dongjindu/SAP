report zm_check_ekbe_for_ekrs.

*---------------------------------------------------------------------*
* select options:
* SO_BELNR = MM invoice document
* SO_GJAHR = Document year
* SO_BUDAT = Posting date of MM invoice document
* SO_BUKRS = Company Code of Purchase Order
*---------------------------------------------------------------------*

tables: ekko, ekbe, ekpo, rbkp, rseg, ekrs.

data: tab_rbkp like rbkp occurs 10,
      s_rbkp like rbkp.

data: tab_rseg like rseg occurs 10,
      s_rseg like rseg.

data: begin of tab_check_ebeln occurs 10,
      ebeln like ekpo-ebeln,
      ebelp like ekpo-ebelp.
data: end of tab_check_ebeln.

data: tab_ekko like ekko occurs 10 with header line,
      s_ekko like ekko.

data: tab_ekpo like ekpo occurs 10,
      s_ekpo like ekpo.

data: tab_ek08rn like ek08rn occurs 10.
data: s_ek08rn like ek08rn.

data: begin of tab_hist_error occurs 10,
       ebeln like ekko-ebeln,
       ebelp like ekpo-ebelp,
       lfgja like ek08rn-lfgja,
       lfbnr like ek08rn-lfbnr,
       lfpos like ek08rn-lfpos,
       wemng like ek08rn-wemng,
       remng like ek08rn-remng,
       wewrt like ek08rn-wewrt,
       arewr like ek08rn-arewr.
data: end of tab_hist_error.

data: tab_ekbe like ekbe occurs 0 with header line,
      tab_ekrs like ekrs occurs 0 with header line.

data: f_lines type i,
      f_diff_menge like ek08rn-remng,
      f_diff_value like ek08rn-wewrt,
      f_diff_theoretical_value like ek08rn-wewrt,
      f_quot_we type p decimals 2,
      f_quot_re type p decimals 2,
      f_quot_compare type p decimals 2,
      f_quot type p decimals 2 value 5,
      f_add_check type boole-boole.

parameters: p_ebeln like ekbe-ebeln,
            p_ebelp like ekbe-ebelp.

select-options: so_belnr for rbkp-belnr,
                so_gjahr for rbkp-gjahr,
                so_cpudt for rbkp-cpudt,
                so_budat for rbkp-budat,
                so_bukrs for ekko-bukrs.
parameters: p_test type xfeld default 'X'.
* ---- selection ------------------------------------------------------*

select ebeln ebelp into table tab_check_ebeln
  from ekpo
  where XERSY = 'X'. "ERS
if sy-subrc ne 0.
  write: 'No S/A entries found'.
  exit.
endif.

*select * from  rbkp  into table tab_rbkp
*       where  belnr  in so_belnr
*       and    gjahr  in so_gjahr
*       and    budat  in so_budat
*       and    cpudt  in so_cpudt.
*
*if sy-subrc ne 0.
*  write: 'No RBKP entries found'.
*  exit.
*endif.
*
*select * from rseg into table tab_rseg
*                   for all entries in tab_rbkp
*       where  belnr  = tab_rbkp-belnr
*       and    gjahr  = tab_rbkp-gjahr.
*
*if sy-subrc ne 0.
*  write: 'No RSEG entries found'.
*  exit.
*endif.
*
** ---- fill tab_check_ebeln
*-------------------------------------------*
*loop at tab_rseg into s_rseg.
*  tab_check_ebeln-ebeln = s_rseg-ebeln.
*  tab_check_ebeln-ebelp = s_rseg-ebelp.
*  collect tab_check_ebeln.
*endloop.

select * from ekko into table tab_ekko
                   for all entries in tab_check_ebeln
       where  ebeln  = tab_check_ebeln-ebeln
       and    bukrs  in so_bukrs.

if sy-subrc ne 0.
  write: 'No EKKO entries found'.
  exit.
endif.

select * from ekpo into table tab_ekpo
         for all entries in tab_check_ebeln
         where  ebeln  = tab_check_ebeln-ebeln
         and    ebelp  = tab_check_ebeln-ebelp
         and    bukrs  in so_bukrs
         and    pstyp  ne 9
         and    repos ne space
         and    fplnr = space
         and    webre = 'X'
         and    xersy = 'X'.

if sy-subrc ne 0.
  write: 'No EKPO entries found'.
  exit.
endif.

* ---- analyzation ----------------------------------------------------*

loop at tab_ekpo into s_ekpo.
  check not s_ekpo-wepos is initial.
  check s_ekpo-weunb is initial.
  read table tab_ekko into s_ekko with key ebeln = s_ekpo-ebeln.
  call function 'ME_READ_ITEM_INVOICE'
       exporting
            display        = 'X'
            ebelp          = s_ekpo-ebelp
            iekko          = s_ekko
            re_kursf       = s_ekko-wkurs
            re_waers       = s_ekko-waers
            re_wwert       = sy-datum
       tables
            xek08rn        = tab_ek08rn
       exceptions
            not_found_any  = 1
            not_found_one  = 2
            not_valid_any  = 3
            not_valid_one  = 4
            enqueue_failed = 5
            others         = 6.
  if sy-subrc <> 0.
    continue.
  endif.

  loop at tab_ek08rn into s_ek08rn.
    perform check_differences.
  endloop.
endloop.


*-------- List of wrong PO histories ---------------------------------*
clear f_lines.
describe table tab_hist_error lines f_lines.
write: /  'PO history with difference for qty in GRs and Invoices'.
uline .
if f_lines eq 0.
  write: / 'No PO items with a wrong PO history found' .
else.
  perform write_header.
  refresh tab_ekrs.
  loop at tab_hist_error.
    at new ebelp.
      refresh tab_ekbe.
      call function 'ME_READ_HISTORY'
           exporting
                ebeln = tab_hist_error-ebeln
                ebelp = tab_hist_error-ebelp
                webre = 'X'
           tables
                xekbe = tab_ekbe.
    endat.
    write:  /1  tab_hist_error-ebeln ,
             11  tab_hist_error-ebelp ,
             17  tab_hist_error-lfbnr ,
             28  tab_hist_error-wemng ,
             43  tab_hist_error-remng ,
             58  tab_hist_error-wewrt ,
             73  tab_hist_error-arewr.
*   Preparation of EKRS entries:
    loop at tab_ekbe where ebeln = tab_hist_error-ebeln
                     and   ebelp = tab_hist_error-ebelp
                     and   vgabe = '1'
                     and   lfgja = tab_hist_error-lfgja
                     and   lfbnr = tab_hist_error-lfbnr
                     and   lfpos = tab_hist_error-lfpos.
      clear tab_ekrs.
      move-corresponding tab_ekbe to tab_ekrs.
      if tab_ekbe-zekkn ne 0  and  tab_ekbe-packno is initial.
        clear tab_ekrs-lfpos.
      endif.
      read table tab_ekko with key ebeln = tab_hist_error-ebeln.
      if sy-subrc = 0.
        tab_ekrs-lifnr = tab_ekko-lifnr.
        tab_ekrs-bukrs = tab_ekko-bukrs.
      endif.
*     Check against existence
      select single * from ekrs where budat = tab_ekrs-budat
                                and   lifnr = tab_ekrs-lifnr
                                and   belnr = tab_ekrs-belnr
                                and   buzei = tab_ekrs-buzei
                                and   gjahr = tab_ekrs-gjahr.
      if sy-subrc <> 0.
        append tab_ekrs.
      endif.
    endloop.
  endloop.
* Creation of EKRS entries
  if p_test is initial.
    call function 'ME_UPDATE_EKRS'
         exporting
              i_insert = 'X'
              i_delete = ' '
         tables
              t_ekrs   = tab_ekrs.
    refresh tab_ekrs.
  endif.
endif.
uline.
write:  / , / , / .
uline.

*---------------------------------------------------------------------*

form check_differences.

*------ calculate differences
  f_diff_menge = s_ek08rn-wemng - s_ek08rn-remng.
*------ check and fill error_tables---------------------------------*
  check f_diff_menge <> 0.
  if f_diff_menge lt 0 or f_diff_menge gt 0.
    tab_hist_error-ebeln = s_ekpo-ebeln.
    tab_hist_error-ebelp = s_ekpo-ebelp.
    tab_hist_error-lfgja = s_ek08rn-lfgja.
    tab_hist_error-lfbnr = s_ek08rn-lfbnr.
    tab_hist_error-lfpos = s_ek08rn-lfpos.
    tab_hist_error-wemng = s_ek08rn-wemng.
    tab_hist_error-remng = s_ek08rn-remng.
    tab_hist_error-wewrt = s_ek08rn-wewrt.
    tab_hist_error-arewr = s_ek08rn-arewr.
    append tab_hist_error.
  endif.
endform.                        "check_differences.

*---------------------------------------------------------------------*
*       FORM write_header                                             *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
form write_header.

  write:  /1   'PO number' ,
           11  'item' ,
           17  'Material doc' ,
           30  'Delivered qty' ,
           46  'Invoiced qty' ,
           61  'Deliv. value' ,
           74  'Invoices value' .

  write:  /1   'EBELN' ,
           11  'EBELP' ,
           17  'LFBNR' ,
           30  'EK08RN-WEMNG' ,
           46  'EK08RN-REMNG' ,
           61  'EK08RN-WEWRT' ,
           74  'EK08RN-AREWR' .
endform.                        "write_header.
