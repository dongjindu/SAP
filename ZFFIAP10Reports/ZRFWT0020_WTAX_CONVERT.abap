*&---------------------------------------------------------------------*
*& Report  RFWT0020                                                    *
*&                                                                     *
*&---------------------------------------------------------------------*
*& Report to reproduce w/tax information when w/tax base amount is     *
*&   needed for reporting purposes.                                    *
*&                                                                     *
*& DATE      Developer    Request       Desc
*& 05/26/06  Manju       UD1K920899     Program changes for Tax wholding
*& 08/01/06  Manju       UD1K921605     If Withholding tax code is
*                                       changed only retian latest with
*                                       holding tax code record in
*                                       with_item
*&---------------------------------------------------------------------*

report  rfwt0020                                                .
tables : lfbw,t059p.

include rfwt0020_top.
include mf05acom.
include rfeposc1.

tables: bsec.

selection-screen begin of block one with frame title text-001.

selection-screen skip 1.
select-options: i_lifnr for lfb1-lifnr.
select-options: i_stcd1 for lfa1-stcd1.
select-options: i_stcd2 for lfa1-stcd2.
select-options: i_kunnr for kna1-kunnr.
select-options: i_kstcd1 for kna1-stcd1.
select-options: i_kstcd2 for kna1-stcd2.
select-options: i_bukrs for t001-bukrs  memory id buk,
                i_belnr for bkpf-belnr.
select-options: i_time  for bkpf-budat,
                s_augdt for bsak-augdt.

selection-screen end of block one.

selection-screen begin of block two with frame title text-002.

selection-screen: begin of line,
  comment 4(60) text-003 for field xcreate, position 2.
parameters: xcreate radiobutton group uli default 'X'.
selection-screen end of line.
selection-screen: begin of line,
  comment 4(60) text-004 for field xmodify, position 2.
parameters: xmodify radiobutton group uli.
selection-screen end of line.

selection-screen begin of block three with frame title text-005.
selection-screen begin of line.
parameters: entry as checkbox default 'X'.
selection-screen comment 4(70) text-024.
selection-screen end of line.
selection-screen begin of line.
parameters: r_inv radiobutton group zahl.
selection-screen comment 4(30) text-028.
selection-screen end of line.
selection-screen begin of line.
parameters: r_pay radiobutton group zahl.
selection-screen comment 4(15) text-029.
selection-screen end of line.

selection-screen begin of line.
parameters: test as checkbox default 'X'.
selection-screen comment 4(15) text-008.
selection-screen end of line.

selection-screen end of block three.

selection-screen end of block two.

*------------------------------------------------------------
* MAIN PROGRAM
*------------------------------------------------------------
start-of-selection.

  data: l_idx like sy-tabix.

*----select data with tax codes stdd1
  refresh i_lfa1.
  select * from lfa1 into table i_lfa1
        where lifnr in i_lifnr.

  loop at i_lfa1 into wa_lfa1.
    l_idx = sy-tabix.

    if wa_lfa1-xcpdk = 'X'. "one-time
    else.
      if wa_lfa1-stcd1 in i_stcd1 and wa_lfa1-stcd2 in i_stcd2.
      else.
        delete i_lfa1 index l_idx.
        continue.
      endif.
    endif.

    i_lifnr-sign = 'I'.
    i_lifnr-option = 'EQ'.
    i_lifnr-low = wa_lfa1-lifnr.
    append i_lifnr.
  endloop.

  describe table i_lifnr lines h_line3.
  if h_line3 eq 0.
    clear i_lifnr.
    refresh i_lifnr.
    i_lifnr-sign = 'I'.
    i_lifnr-option = 'EQ'.
    append i_lifnr.
  endif.

  sort i_lifnr by low.
  delete adjacent duplicates from i_lifnr.

  sort i_lfa1  by lifnr.

*---customer---
  describe table i_kstcd1 lines h_line1.
  describe table i_kstcd2 lines h_line2.
  if h_line1 ne 0
   or h_line2 ne 0.
    refresh i_kna1.
    select * from kna1 into table i_kna1
          where stcd1 in i_kstcd1
             and stcd2 in i_kstcd2.

    loop at i_kna1 into wa_kna1.
      i_kunnr-sign = 'I'.
      i_kunnr-option = 'EQ'.
      i_kunnr-low = wa_kna1-kunnr.
      append i_kunnr.
    endloop.
  endif.
  describe table i_kunnr lines h_line3.
  if h_line3 eq 0.
    clear i_kunnr.
    refresh i_kunnr.
    i_kunnr-sign = 'I'.
    i_kunnr-option = 'EQ'.
    append i_kunnr.
  endif.

* read company code data / set lock entries to BKPF table
  refresh i_t001.
  select * from t001 into table i_t001
        where bukrs in i_bukrs.
  data :l_wt_tpnr like lfbw-wt_withcd,
        l_wt_postm like t059p-wt_postm.
  sort: i_t001.
  loop at i_t001 into wa_t001.

*-----------------------------------------------------------------------
*-- process vendor
*-----------------------------------------------------------------------

    refresh i_lfb1.
    select * from lfb1 into table i_lfb1
          where bukrs = wa_t001-bukrs and
                lifnr in i_lifnr.

    loop at i_lfb1 into wa_lfb1.

*---- checked lock entries from payment program
      select * from regus where koart = 'K'
                         and   bukrs = wa_t001-bukrs
                         and   konko = wa_lfb1-lifnr.
      endselect.
      if sy-subrc = 0.            " Account is locked (payment proposal)
        append regus to locked_entries.
      else.
        if test = ' '.
          call function 'ENQUEUE_EFLFB1A'
               exporting
                    lifnr          = wa_lfb1-lifnr
                    bukrs          = wa_t001-bukrs
               exceptions
                    foreign_lock   = 1
                    system_failure = 2
                    others         = 3.
          if sy-subrc <> 0.
            clear locked_entries.
            locked_entries-koart = 'K'.
            locked_entries-bukrs = wa_t001-bukrs.
            locked_entries-konko = wa_lfb1-lifnr.
            append locked_entries.
            continue.
          endif.
        endif.

        call function 'FI_CHECK_EXTENDED_WT'
             exporting
                  i_bukrs              = wa_t001-bukrs
             exceptions
                  component_not_active = 1
                  not_found            = 2
                  others               = 3.
        if sy-subrc = 0.
          extended_wt_active = 'X'.

          select single * from lfbw where lifnr eq   wa_lfb1-lifnr.
          if sy-subrc eq 0.
            select single *  from t059p where land1 = 'US' and
                                       witht = lfbw-witht.
            l_wt_tpnr =    t059p-wt_postm.
            pack l_wt_tpnr    to l_wt_postm.
*         l_wt_postm =  l_WT_TPNR.
          endif.
*---- no lock entries from payment program for this vendor
          call function 'FI_WT_DETERM_RELEVANT_TYPES'
               exporting
                    i_bukrs          = wa_t001-bukrs
                    i_acct           = wa_lfb1-lifnr
                    i_koart          = 'K'
                    i_budat          = sy-datum
                    i_postingtime    = l_wt_postm
               tables
                    t_with           = i_accit_wt
               exceptions
                    nothing_selected = 1
                    others           = 2.
          check sy-subrc = 0 or sy-subrc = 1.
        else.
          clear extended_wt_active.
        endif.

*---- read vendor items
        refresh i_bsak.
        l_status = closed_items.
        data: gt_bsak like wa_bsak occurs 0 with header line.
        select * from bsak into corresponding fields of table gt_bsak
           where bukrs = wa_t001-bukrs and
                 lifnr = wa_lfb1-lifnr and
                 augdt in s_augdt and
                 belnr in i_belnr and
                 budat in i_time  .
        loop at gt_bsak into wa_bsak.

          read table i_lfa1 into wa_lfa1
               with key lifnr = wa_bsak-lifnr binary search.
          if wa_lfa1-xcpdk = 'X'.  "ONE-TIME
            select single * from bsec
              where bukrs = wa_bsak-bukrs
                and belnr = wa_bsak-belnr
                and gjahr = wa_bsak-gjahr
                and buzei = wa_bsak-buzei.

            if bsec-stcd1 in i_stcd1 and bsec-stcd2 in i_stcd2.
            else.
              continue.
            endif.
          endif.

          if test = ' '.
            call function 'ENQUEUE_EFBKPF'
                 exporting
                      bukrs          = wa_bsak-bukrs
                      belnr          = wa_bsak-belnr
                      gjahr          = wa_bsak-gjahr
                 exceptions
                      foreign_lock   = 1
                      system_failure = 2
                      others         = 3.
          endif.
          if sy-subrc <> 0.
            move-corresponding wa_bsak to locked_documents.
            append locked_documents.
          else.
            append wa_bsak to i_bsak.
          endif.
        endloop.

*---- process vendor items;  new items are in table
*----          i_added_bsak_with_item
        sort i_bsak.
        loop at i_bsak into wa_bsak.
          if extended_wt_active = 'X'.
            perform vendor_document_check using wa_bsak
             changing i_added_with_item[].
          else.
            if wa_bsak-augbl = wa_bsak-belnr.
              append wa_bsak to i_bsak_augbl.
            else.
              perform simple_wt using wa_bsak.
            endif.
          endif.
        endloop.                       "i_bsak

*---simple withholding tax: process vendor items where augbl = belnr
        if extended_wt_active = ' '.
          l_status = closed_items_augbl.
          loop at i_bsak_augbl into wa_bsak.
            perform simple_wt using wa_bsak.
          endloop.
          clear i_bsak_augbl.
          refresh i_bsak_augbl.
        endif.

*---- read open vendor items
        refresh i_bsik.
        l_status = open_items.
        select * from bsik into wa_bsik
           where bukrs = wa_t001-bukrs and
                 lifnr = wa_lfb1-lifnr and
                 belnr in i_belnr and
                 budat in i_time.
          if test = ' '.
            call function 'ENQUEUE_EFBKPF'
                 exporting
                      bukrs          = wa_bsik-bukrs
                      belnr          = wa_bsik-belnr
                      gjahr          = wa_bsik-gjahr
                 exceptions
                      foreign_lock   = 1
                      system_failure = 2
                      others         = 3.
          endif.
          if sy-subrc <> 0.
            move-corresponding wa_bsik to locked_documents.
            append locked_documents.
          else.
            append wa_bsik to i_bsik.
          endif.
        endselect.

*---- process open vendor items
        loop at i_bsik into wa_bsik.
          if extended_wt_active = 'X'.
            perform vendor_document_check
             using wa_bsik
             changing i_added_with_item[].
          else.
            perform simple_wt
              using wa_bsik.
          endif.
        endloop.                       " i_bsik
        if test = ' '.
          call function 'DEQUEUE_ALL'.
        endif.
      endif.
    endloop.

*-----------------------------------------------------------------------
*-- process customer
*-----------------------------------------------------------------------

    refresh i_knb1.
    select * from knb1 into table i_knb1
          where bukrs = wa_t001-bukrs and
                kunnr in i_kunnr.
    loop at i_knb1 into wa_knb1.

*---- checked lock entries from payment program
      select * from regus where koart = 'D'
                         and   bukrs = wa_t001-bukrs
                         and   konko = wa_knb1-kunnr.
      endselect.
      if sy-subrc = 0.            " Account is locked (payment proposal)
        append regus to locked_entries.
      else.
        if test = ' '.
          call function 'ENQUEUE_EFKNB1A'
               exporting
                    kunnr          = wa_knb1-kunnr
                    bukrs          = wa_t001-bukrs
               exceptions
                    foreign_lock   = 1
                    system_failure = 2
                    others         = 3.
          if sy-subrc <> 0.
            clear locked_entries.
            locked_entries-koart = 'D'.
            locked_entries-bukrs = wa_t001-bukrs.
            locked_entries-konko = wa_knb1-kunnr.
            append locked_entries.
          endif.
        endif.

        call function 'FI_CHECK_EXTENDED_WT'
             exporting
                  i_bukrs              = wa_t001-bukrs
             exceptions
                  component_not_active = 1
                  not_found            = 2
                  others               = 3.
        if sy-subrc = 0.
          extended_wt_active = 'X'.

*---- no lock entries from payment program for this vendor
          call function 'FI_WT_DETERM_RELEVANT_TYPES'
               exporting
                    i_bukrs          = wa_t001-bukrs
                    i_acct           = wa_knb1-kunnr
                    i_koart          = 'D'
                    i_budat          = sy-datum
                    i_postingtime    = '2'
               tables
                    t_with           = i_accit_wt
               exceptions
                    nothing_selected = 1
                    others           = 2.
          check sy-subrc = 0 or sy-subrc = 1.
        else.
          clear extended_wt_active.
        endif.

*---- read customer items
        refresh i_bsad.
        l_status = closed_items.
        select * from bsad into wa_bsad
           where bukrs = wa_t001-bukrs and
                 kunnr = wa_knb1-kunnr and
                 augdt in s_augdt and
                 belnr in i_belnr and
                 budat in i_time.

          if test = ' '.
            call function 'ENQUEUE_EFBKPF'
                 exporting
                      bukrs          = wa_bsad-bukrs
                      belnr          = wa_bsad-belnr
                      gjahr          = wa_bsad-gjahr
                 exceptions
                      foreign_lock   = 1
                      system_failure = 2
                      others         = 3.
          endif.
          if sy-subrc <> 0.
            move-corresponding wa_bsad to locked_documents.
            append locked_documents.
          else.
            append wa_bsad to i_bsad.
          endif.
        endselect.

*---- process customer items;  new items are in table
*----          i_added_bsad_with_item
        sort i_bsad.
        loop at i_bsad into wa_bsad.
          if extended_wt_active = 'X'.
            perform customer_document_check using wa_bsad
             changing i_added_with_item[].
          endif.
        endloop.                       "i_bsad


*---- read open customer items
        refresh i_bsid.
        l_status = open_items.
        select * from bsid into wa_bsid
               where bukrs = wa_t001-bukrs and
                     kunnr = wa_knb1-kunnr and
                     belnr in i_belnr and
                     budat in i_time.

          if test = ' '.
            call function 'ENQUEUE_EFBKPF'
                 exporting
                      bukrs          = wa_bsid-bukrs
                      belnr          = wa_bsid-belnr
                      gjahr          = wa_bsid-gjahr
                 exceptions
                      foreign_lock   = 1
                      system_failure = 2
                      others         = 3.
          endif.
          if sy-subrc <> 0.
            move-corresponding wa_bsid to locked_documents.
            append locked_documents.
          else.
            append wa_bsid to i_bsid.
          endif.
        endselect.

*---- process open customer items
        loop at i_bsid into wa_bsid.
          if extended_wt_active = 'X'.
            perform customer_document_check
             using wa_bsid
             changing i_added_with_item[].
          endif.
        endloop.                       " i_bsid
        if test = ' '.
          call function 'DEQUEUE_ALL'.
        endif.
      endif.
    endloop.
  endloop.
  if test = ' '.
    call function 'DEQUEUE_ALL'.
  endif.



*--- write data to dbase
  if test = ' '.
    loop at  i_added_with_item.
      delete from with_item  where bukrs = i_added_with_item-bukrs
                                  and   belnr = i_added_with_item-belnr
                                  and   gjahr = i_added_with_item-gjahr
                                  and   buzei = i_added_with_item-buzei.
    endloop.
    modify with_item from table i_added_with_item.
    commit work.

    if xcreate eq 'X'.
      modify bseg from table i_changed_bseg_xx.
      modify bsak from table i_changed_bsak_xx.
      modify bsik from table i_changed_bsik_xx.
      modify bsad from table i_changed_bsad_xx.
      modify bsid from table i_changed_bsid_xx.
      commit work.
    endif.

    modify bseg from table i_changed_bseg.
    modify bsak from table i_changed_bsak.
    modify bsik from table i_changed_bsik.
    modify bsad from table i_changed_bsad.
    modify bsid from table i_changed_bsid.
    commit work.
  endif.

*--- display data with list viewer
  g_repid = sy-repid.
  perform create_gt_excluding.
  perform print_with_item.


  include zrfwt0020_ewt.
*  INCLUDE rfwt0020_ewt.
*  INCLUDE rfwt0020_alvf.
  include  zrfwt0020_alvf.
  include rfwt0020_cwt.
