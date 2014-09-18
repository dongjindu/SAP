FUNCTION zppc1tp_orders_delete_pl.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IF_RESTIME) TYPE  PPC_RES_TIME DEFAULT 14
*"     VALUE(IF_DELCONF) TYPE  AS4FLAG DEFAULT 'X'
*"     VALUE(IF_COMMIT) TYPE  AS4FLAG DEFAULT 'X'
*"     REFERENCE(S_LOW)
*"  EXPORTING
*"     VALUE(EF_PLAF_COUNT) TYPE  I
*"     VALUE(EF_RESB_COUNT) TYPE  I
*"  EXCEPTIONS
*"      UPDATE_ERROR
*"----------------------------------------------------------------------

  DATA:
    lf_date     TYPE sy-datlo,
    lf_time     TYPE sy-timlo,
    lf_timestmp TYPE tzntstmps,
    lf_no_del   TYPE c,
    lf_orderid  TYPE ppc_orderid,
    lf_conftime TYPE ppc_conftime,

    ls_head     TYPE ppc_head,
    ls_resb     TYPE ppc1tp_resb,
    ls_plaf     TYPE ppc1tp_plaf,
    ls_return   TYPE bapiret2,
    ls_conf_del TYPE bapi_ppc_conf_orderid,

    lt_head     TYPE TABLE OF ppc_head with header line,
    lt_resb     TYPE TABLE OF ppc1tp_resb,
    lt_plaf     TYPE TABLE OF ppc1tp_plaf,
    lt_resb_del TYPE TABLE OF ppc1tp_resb,
    lt_plaf_del TYPE TABLE OF ppc1tp_plaf,
    lt_conf_del TYPE TABLE OF bapi_ppc_conf_orderid.


* Initialization
  CLEAR: ef_plaf_count, ef_resb_count.
  CLEAR: lf_date, lf_time, lf_timestmp, ls_plaf.
  REFRESH: lt_plaf, lt_resb_del, lt_plaf_del, lt_conf_del.

  MOVE sy-timlo TO lf_time.
  MOVE sy-datlo TO lf_date.
  SUBTRACT if_restime FROM lf_date.
  CONVERT DATE lf_date TIME lf_time
          INTO TIME STAMP lf_timestmp
          TIME ZONE 'UTC   '.
  REFRESH s_plnum.

  s_plnum-low    = s_low.
  s_plnum-sign   = 'I'.
  s_plnum-option = 'EQ'.
  APPEND s_plnum.
  READ TABLE s_plnum INDEX 1.

*---
* Select completed orders from PLAF
  IF s_plnum-low IS INITIAL.
    SELECT *
        FROM ppc1tp_plaf AS p
        INTO TABLE lt_plaf
*        WHERE wamng = p~gsmng
        where wemng = p~gsmng.
  ELSEIF NOT s_plnum-low IS INITIAL.
    SELECT *
        FROM ppc1tp_plaf AS p
        INTO TABLE lt_plaf
*        WHERE wamng = p~gsmng
        Where wemng = p~gsmng
          AND plnum IN s_plnum.
  ENDIF.

*---
* Filter the orders: completed if_restime days ago
  LOOP AT lt_plaf INTO ls_plaf.


    CLEAR: lf_orderid, lf_conftime, lf_no_del, ls_head, ls_conf_del.
    REFRESH: lt_head, lt_resb.
    MOVE ls_plaf-plnum TO lf_orderid.


* by ig.moon 5/11/2011 {
**   read confirmation for the order
*    SELECT *
*        FROM ppc_head
*        INTO TABLE lt_head
*        WHERE orderid = lf_orderid
*        ORDER BY flg_gr_head DESCENDING
*                 conftime DESCENDING.

    select *
        from ppc_head
        into table lt_head
        where orderid = lf_orderid
          and flg_gr_head = 'X'
        order by flg_gr_head descending
                 conftime descending.

   loop at lt_head.
     if lt_head-flg_reversal = 'X'.
        lt_head-confquant = - lt_head-confquant.
        modify lt_head index sy-tabix transporting confquant.
     endif.
   endloop.
   data: lv_flag type c.
   lv_flag = ' '.
   loop at lt_head.
     at last.
     sum.
     if lt_head-confquant <> 1.
       lv_flag = 'X'.
     endif.
     endat.
   endloop.
   if lv_flag = 'X'. continue. endif.

*}

    READ TABLE lt_head INDEX 1 INTO ls_head.

*   because both wamng and wemng are completed, the first
*   line of the result table should be the 'final' backflush,
*   but we should check, just in case...
*    READ TABLE lt_head INDEX 1 INTO ls_head.
*    IF ls_head-flg_gr_head NE gc_true OR
*       ls_head-flg_reversal EQ gc_true.
*      " paranoia check. this should not happen
*      CONTINUE.
*    ENDIF.

*   is the last confirmation before residence limit?

    IF ls_head-conftime GT lf_timestmp.
      " the order was completed recently
      CONTINUE.
    ENDIF.
    lf_conftime = ls_head-conftime.

*   any unprocessed confirmation?
    LOOP AT lt_head INTO ls_head
        WHERE ( flg_synch NE gc_true OR
                flg_asynch NE gc_true OR
                flg_asynch_a NE gc_true ).
      " no not delete this order. yet.
      lf_no_del = gc_true.
    ENDLOOP.
    IF lf_no_del EQ gc_true.
      CONTINUE.
    ENDIF.

*   any confirmation after the 'latest' GR?
    LOOP AT lt_head INTO ls_head
        WHERE conftime GT lf_conftime.
      " another paranoia check: there should not be
      " any confirmation after the final GR
      lf_no_del = gc_true.
    ENDLOOP.
    IF lf_no_del EQ gc_true.
      CONTINUE.
    ENDIF.

*   now, order is relevant to deletion - in this case,
*   find its dependend requirements in RESB
    SELECT *
        FROM ppc1tp_resb
        INTO TABLE lt_resb
        WHERE rsnum = ls_plaf-rsnum.
    " draconian check: all reservation were completed?
    READ TABLE lt_resb INTO ls_resb WITH KEY enmng = 0.
    IF sy-subrc = 0.
      " do not delete strangely backflushed orders
      " usually plaf has wamng=1 only when all enmng<>0 in resb
      CONTINUE.
    ENDIF.

*   since we've got here, the order must be deleted
    APPEND ls_plaf TO lt_plaf_del.
    APPEND LINES OF lt_resb TO lt_resb_del.
    REFRESH lt_resb.

*   when required, set deletion flag at confirmation level
    IF if_delconf EQ gc_true.
      MOVE lf_orderid TO ls_conf_del-orderid.
      APPEND ls_conf_del TO lt_conf_del.
    ENDIF.

  ENDLOOP.

  REFRESH: lt_head, lt_resb, lt_plaf.

*---
* Set deletion flag for confirmations: they can be archived now
  IF NOT lt_conf_del IS INITIAL.
    CALL FUNCTION 'PPC1DC_CONF_SET_DEL_FLAG'
         TABLES
              it_conf_orderid = lt_conf_del
         EXCEPTIONS
              update_error    = 1
              OTHERS          = 2.
    IF sy-subrc <> 0.
** Furong on 04/02/12
   message s002 with sy-msgv1 '. Plan order:'
                     ls_plaf-PLNUM.
*      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
*              RAISING update_error.
** End
    ENDIF.
  ENDIF.


*---
* Delete entries from PLAF and RESB
  CALL FUNCTION 'PPC1TP_UPDATE_DELETE_PLAF_RESB' IN UPDATE TASK
       TABLES
            i_ppc1tp_plaf_del = lt_plaf_del
            i_ppc1tp_resb_del = lt_resb_del
       EXCEPTIONS
            OTHERS            = 1.
  IF sy-subrc <> 0.
** Furong on 04/02/12
   message s002 with 'Error: deletion of PLAF/RESB. ' sy-msgv1.
*    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
*            RAISING update_error.
** End
  ENDIF.


*---
* Commit the changes to database
  IF if_commit EQ gc_true.
    COMMIT WORK.  "no wait, as we delete
  ENDIF.


  DESCRIBE TABLE lt_plaf_del LINES ef_plaf_count.
  DESCRIBE TABLE lt_resb_del LINES ef_resb_count.

  REFRESH: lt_conf_del, lt_plaf_del, lt_resb_del.


ENDFUNCTION.
