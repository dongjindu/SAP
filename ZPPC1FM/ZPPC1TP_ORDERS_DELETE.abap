function zppc1tp_orders_delete.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(IF_RESTIME) TYPE  PPC_RES_TIME DEFAULT 14
*"     VALUE(IF_DELCONF) TYPE  AS4FLAG DEFAULT 'X'
*"     VALUE(IF_COMMIT) TYPE  AS4FLAG DEFAULT 'X'
*"  EXPORTING
*"     VALUE(EF_PLAF_COUNT) TYPE  I
*"     VALUE(EF_RESB_COUNT) TYPE  I
*"  EXCEPTIONS
*"      UPDATE_ERROR
*"----------------------------------------------------------------------

  data:
    lf_date     type sy-datlo,
    lf_time     type sy-timlo,
    lf_timestmp type tzntstmps,
    lf_no_del   type c,
    lf_orderid  type ppc_orderid,
    lf_conftime type ppc_conftime,

    ls_head     type ppc_head,
    ls_resb     type ppc1tp_resb,
    ls_plaf     type ppc1tp_plaf,
    ls_return   type bapiret2,
    ls_conf_del type bapi_ppc_conf_orderid,

    lt_head     type table of ppc_head,
    lt_resb     type table of ppc1tp_resb,
    lt_plaf     type table of ppc1tp_plaf,
    lt_resb_del type table of ppc1tp_resb,
    lt_plaf_del type table of ppc1tp_plaf,
    lt_conf_del type table of bapi_ppc_conf_orderid.


* Initialization
  clear: ef_plaf_count, ef_resb_count.
  clear: lf_date, lf_time, lf_timestmp, ls_plaf.
  refresh: lt_plaf, lt_resb_del, lt_plaf_del, lt_conf_del.

  move sy-timlo to lf_time.
  move sy-datlo to lf_date.
  subtract if_restime from lf_date.
  convert date lf_date time lf_time
          into time stamp lf_timestmp
          time zone 'UTC   '.

*---
* Select completed orders from PLAF
  select *
      from ppc1tp_plaf as p
      into table lt_plaf
      where wemng = p~gsmng.


*---
* Filter the orders: completed if_restime days ago
  loop at lt_plaf into ls_plaf.


    clear: lf_orderid, lf_conftime, lf_no_del, ls_head, ls_conf_del.
    refresh: lt_head, lt_resb.
    move ls_plaf-plnum to lf_orderid.


*   read confirmation for the order
    select *
        from ppc_head
        into table lt_head
        where orderid = lf_orderid
        order by flg_gr_head descending
                 conftime descending.

*   because both wamng and wemng are completed, the first
*   line of the result table should be the 'final' backflush,
*   but we should check, just in case...
    read table lt_head index 1 into ls_head.
    if ls_head-flg_gr_head ne gc_true or
       ls_head-flg_reversal eq gc_true.
      " paranoia check. this should not happen
      continue.
    endif.

*   is the last confirmation before residence limit?
    if ls_head-conftime gt lf_timestmp.
      " the order was completed recently
      continue.
    endif.
    lf_conftime = ls_head-conftime.

*   any unprocessed confirmation?
    loop at lt_head into ls_head
        where ( flg_synch ne gc_true or
                flg_asynch ne gc_true or
                flg_asynch_a ne gc_true ).
      " no not delete this order. yet.
      lf_no_del = gc_true.
    endloop.
    if lf_no_del eq gc_true.
      continue.
    endif.

*   any confirmation after the 'latest' GR?
    loop at lt_head into ls_head
        where conftime gt lf_conftime.
      " another paranoia check: there should not be
      " any confirmation after the final GR
      lf_no_del = gc_true.
    endloop.
    if lf_no_del eq gc_true.
      continue.
    endif.

*   now, order seems to be relevant to deletion - in this case,
*   find its dependent requirements in RESB
    select *
        from ppc1tp_resb
        into table lt_resb
        where rsnum = ls_plaf-rsnum.
    " draconian check: all reservation were completed?
    " usually, if PPCGO ran for all confirmations (a previous check),
    " then dependent resb entries should be consistent
    read table lt_resb into ls_resb with key enmng = 0.
    if sy-subrc = 0.
      " do not delete strangely backflushed orders
      continue.
    endif.

*   since we've got here, the order must be deleted
    append ls_plaf to lt_plaf_del.
    append lines of lt_resb to lt_resb_del.
    refresh lt_resb.

*   when required, set deletion flag at confirmation level
    if if_delconf eq gc_true.
      move lf_orderid to ls_conf_del-orderid.
      append ls_conf_del to lt_conf_del.
    endif.

  endloop.

  refresh: lt_head, lt_resb, lt_plaf.


*---
* Set deletion flag for confirmations: they can be archived now
  if not lt_conf_del is initial.
    call function 'PPC1DC_CONF_SET_DEL_FLAG'
         tables
              it_conf_orderid = lt_conf_del
         exceptions
              update_error    = 1
              others          = 2.
    if sy-subrc <> 0.
      message id sy-msgid type sy-msgty number sy-msgno
              with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
              raising update_error.
    endif.
  endif.


*---
* Delete entries from PLAF and RESB
  call function 'ZPPC1TP_UPDATE_DELETE_PLAF_RES' in update task
       tables
            i_ppc1tp_plaf_del = lt_plaf_del
            i_ppc1tp_resb_del = lt_resb_del
       exceptions
            others            = 1.
  if sy-subrc <> 0.
    message id sy-msgid type sy-msgty number sy-msgno
            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
            raising update_error.
  endif.


*---
* Commit the changes to database
  if if_commit eq gc_true.
    commit work.  "no wait, as we delete
  endif.


  describe table lt_plaf_del lines ef_plaf_count.
  describe table lt_resb_del lines ef_resb_count.

  refresh: lt_conf_del, lt_plaf_del, lt_resb_del.


endfunction.
