FUNCTION z_ppc1tp_orders_delete.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IF_RESTIME) TYPE  PPC_RES_TIME DEFAULT 14
*"     VALUE(IF_DELCONF) TYPE  AS4FLAG DEFAULT 'X'
*"     VALUE(IF_COMMIT) TYPE  AS4FLAG OPTIONAL
*"     REFERENCE(IF_PLAF) TYPE  AS4FLAG DEFAULT 'X'
*"  EXPORTING
*"     VALUE(EF_PLAF_COUNT) TYPE  I
*"     VALUE(EF_RESB_COUNT) TYPE  I
*"  TABLES
*"      IT_CONF_ORDERID STRUCTURE  BAPI_PPC_CONF_ORDERID
*"  EXCEPTIONS
*"      UPDATE_ERROR
*"      DELETE_ERROR
*"----------------------------------------------------------------------
  TYPES:
    BEGIN OF ts_ord_del,
      orderid      TYPE ppc_orderid,
      flg_synch    TYPE ppc_sync_posted,
      flg_asynch   TYPE ppc_async_posted,
      flg_asynch_a TYPE ppc_act_posted,
    END OF ts_ord_del.


  DATA:
    ls_head     TYPE ppc_head,
    ls_resb     TYPE ppc1tp_resb,
    ls_plaf     TYPE ppc1tp_plaf,
    ls_return   TYPE bapiret2,
    ls_conf_del TYPE bapi_ppc_conf_orderid,

    lt_head     TYPE TABLE OF ppc_head,
    lt_resb     TYPE TABLE OF ppc1tp_resb,
    lt_plaf     TYPE TABLE OF ppc1tp_plaf,
    lt_resb_del TYPE TABLE OF ppc1tp_resb,
    lt_plaf_del TYPE TABLE OF ppc1tp_plaf,

    lt_conf_del  TYPE TABLE OF bapi_ppc_conf_orderid,

    ls_resb_del  TYPE ppc1tp_resb,
    ls_plaf_del  TYPE ppc1tp_plaf,
    ls_plnum_del TYPE plnum,

    ls_ord_del TYPE ts_ord_del,

    lt_ord     TYPE TABLE OF ts_ord_del,
    lt_ord_tmp TYPE SORTED TABLE OF ts_ord_del  WITH NON-UNIQUE KEY orderid,
    lt_ord_del TYPE SORTED TABLE OF ts_ord_del  WITH NON-UNIQUE KEY orderid.


  DESCRIBE TABLE it_conf_orderid LINES sy-tabix.
  CHECK sy-tabix > 0.

* Initialization
  REFRESH: lt_plaf, lt_resb_del, lt_plaf_del, lt_conf_del.


  SELECT orderid materialnr plant ordernr
    FROM ppc_ord_inf INTO TABLE lt_ord
     FOR ALL ENTRIES IN it_conf_orderid
     WHERE orderid   = it_conf_orderid-orderid.
*         zz_tmstp_final < iv_selordtime

  CHECK sy-dbcnt > 0.

* Check synchronous and asynchronous movements,
* asynchronous activity posting are made to order
* If already post goods movement synch./asynch., escape from deleting
  SELECT DISTINCT orderid flg_synch flg_asynch flg_asynch_a
    FROM ppc_head
    INTO TABLE lt_ord_del
     FOR ALL ENTRIES IN lt_ord
   WHERE orderid      = lt_ord-orderid AND
*        crtime       < iv_selconftime AND
         flg_del      = space.    "already deleted

  lt_ord_tmp = lt_ord_del.
  LOOP AT lt_ord_tmp INTO ls_ord_del
    WHERE ( flg_synch    IS INITIAL OR flg_synch = 'B' ) OR
          ( flg_asynch   IS INITIAL OR flg_asynch = 'B' ) OR
          ( flg_asynch_a IS INITIAL OR flg_asynch = 'B' ).
    READ TABLE lt_ord_del WITH KEY orderid = ls_ord_del-orderid
      TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      DELETE lt_ord_del WHERE orderid = ls_ord_del-orderid.
    ELSE.
      CONTINUE.
    ENDIF.
  ENDLOOP.

  LOOP AT lt_ord_del INTO ls_ord_del.
*-- check planned order if deletion is allowed
*--- qty, date
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = ls_ord_del-orderid
      IMPORTING
        output = ls_plnum_del.

    SELECT SINGLE * INTO ls_plaf FROM ppc1tp_plaf
      WHERE plnum = ls_plnum_del.

    IF ls_plaf_del-gsmng = ls_plaf_del-wemng.  " completed...
*    APPEND ls_ord_del-orderid TO lt_conf_del.  "FIXME
      APPEND ls_plaf  TO lt_plaf.
    ENDIF.

  ENDLOOP.

* now, order seems to be relevant to deletion - in this case,
* find its dependent requirements in RESB
  LOOP AT lt_plaf INTO ls_plaf_del.
    SELECT * FROM ppc1tp_resb
        INTO TABLE lt_resb
        WHERE rsnum = ls_plaf-rsnum.

    " draconian check: all reservation were completed?
    " usually, if PPCGO ran for all confirmations (a previous check),
    " then dependent resb entries should be consistent
    READ TABLE lt_resb INTO ls_resb WITH KEY enmng = 0.
    IF sy-subrc = 0.
      " do not delete strangely backflushed orders
      CONTINUE.
    ENDIF.


*   since we've got here, the order must be deleted
    APPEND ls_plaf TO lt_plaf_del.
    APPEND LINES OF lt_resb TO lt_resb_del.
    REFRESH lt_resb.

*   when required, set deletion flag at confirmation level
    IF if_delconf EQ gc_true.
      MOVE ls_plaf-plnum  TO ls_conf_del-orderid.
      APPEND ls_conf_del TO lt_conf_del.
    ENDIF.

  ENDLOOP.

*---
* Set deletion flag for confirmations: they can be archived now
* call function 'PPC1DC_CONF_SET_DEL_FLAG'
  IF lt_conf_del IS NOT INITIAL.

    CALL FUNCTION 'BAPI_MNFCTCONFRCVR_DELETE'
      IMPORTING
        return          = ls_return
      TABLES
        it_conf_orderid = lt_conf_del.

    IF NOT ls_return IS INITIAL.
      MESSAGE ID ls_return-id TYPE ls_return-type NUMBER ls_return-number
      WITH ls_return-message_v1 ls_return-message_v2 ls_return-message_v3
           ls_return-message_v4
           RAISING delete_error.
    ENDIF.
  ENDIF.


*---
* Delete entries from PLAF and RESB
  IF if_plaf = 'X'.
    CALL FUNCTION 'PPC1TP_UPDATE_DELETE_PLAF_RESB' IN UPDATE TASK
      TABLES
        i_ppc1tp_plaf_del = lt_plaf_del
        i_ppc1tp_resb_del = lt_resb_del
      EXCEPTIONS
        OTHERS            = 1.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
              RAISING update_error.
    ENDIF.
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
