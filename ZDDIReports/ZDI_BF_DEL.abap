*&---------------------------------------------------------------------*
*& Report  ZDI_BF_DEL
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
* testing purpose
* from CDP package
REPORT  zdi_bf_del.

TABLES: ppc_ord_inf, plaf.

TYPES:
  BEGIN OF gty_s_order,
*    headid     TYPE ppc_headid_int,
    orderid    TYPE ppc_orderid,
    materialnr TYPE matnr,
    plant      TYPE werks_d,
    ordernr    TYPE ppc_ordernr,
  END   OF gty_s_order,
  gty_t_order  TYPE STANDARD TABLE OF gty_s_order,

  gtr_ordid    TYPE RANGE OF ppc_orderid,
  gtr_ordnr    TYPE RANGE OF ppc_ordernr,
  gtr_plnum    TYPE RANGE OF plnum,
  gtr_matnr    TYPE RANGE OF matnr,
  gtr_werks    TYPE RANGE OF werks_d.

TYPES:
  BEGIN OF ts_ord_del,
    orderid      TYPE ppc_orderid,
    flg_synch    TYPE ppc_sync_posted,
    flg_asynch   TYPE ppc_async_posted,
    flg_asynch_a TYPE ppc_act_posted,
  END OF ts_ord_del.

DATA: gt_ordid         TYPE ppc_t_orderid.

* --------------------------------------------------------------------- *
* Selection-Screen
* --------------------------------------------------------------------- *
SELECT-OPTIONS:  so_ordid  FOR  ppc_ord_inf-orderid MEMORY ID paf,
                 so_ordnr  FOR  ppc_ord_inf-ordernr,
                 so_plord  FOR  plaf-plnum,
                 so_matnr  FOR  ppc_ord_inf-materialnr,
                 so_plant  FOR  ppc_ord_inf-plant.
PARAMETERS:
                 p_plaf    TYPE cftest AS CHECKBOX DEFAULT 'X',
                 p_test    TYPE cftest AS CHECKBOX DEFAULT 'X'.

* --------------------------------------------------------------------- *
DATA: h_dontpanic   LIKE sy-datlo.
AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    IF screen-name = 'P_TEST'.
      GET PARAMETER ID 'DONTPANIC' FIELD h_dontpanic.
      IF h_dontpanic = sy-datlo.
        screen-input = '1'.
      ELSE.
        screen-input = '0'.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

* --------------------------------------------------------------------- *
START-OF-SELECTION.
* --------------------------------------------------------------------- *

  PERFORM get_orders  USING    so_ordid[]
                               so_ordnr[]
                               so_plord[]
                               so_matnr[]
                               so_plant[]
*                              gv_selconftime
                      CHANGING gt_ordid .
  IF gt_ordid IS INITIAL.

  ELSE.
    PERFORM set_order_deletion USING gt_ordid.
  ENDIF.



*&---------------------------------------------------------------------*
*&      Form  GET_ORDERS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->IR_ORDID   Range table for OrderID
*      -->IR_PLORD   Range table for planned order number
*      -->IR_MATNR   Range table for material
*      -->IR_PLANT   Range table for plant
*      <--ET_ORD_INF Order info internal table
*----------------------------------------------------------------------*
FORM get_orders  USING    ir_ordid       TYPE gtr_ordid
                          ir_ordnr       TYPE gtr_ordnr
                          value(ir_plnum) TYPE gtr_plnum
                          ir_matnr       TYPE gtr_matnr
                          ir_plant       TYPE gtr_werks
*                         iv_selconftime TYPE ppc_conftime
                 CHANGING et_ordid       TYPE ppc_t_orderid.

  DATA:
    ls_plnum_r TYPE range_s_plnum,
    lt_ord     TYPE gty_t_order,
    ls_ord_del TYPE ts_ord_del,
    lt_ord_del TYPE SORTED TABLE OF ts_ord_del
      WITH NON-UNIQUE KEY orderid,
    lt_tmp_del TYPE SORTED TABLE OF ts_ord_del
      WITH NON-UNIQUE KEY orderid.

  CLEAR et_ordid.

* Convert planned order number output length for PPC
  LOOP AT ir_plnum INTO ls_plnum_r.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = ls_plnum_r-low
      IMPORTING
        output = ls_plnum_r-low.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = ls_plnum_r-high
      IMPORTING
        output = ls_plnum_r-high.
    MODIFY ir_plnum INDEX sy-tabix FROM ls_plnum_r
    TRANSPORTING low high.
  ENDLOOP.

* Get the orders before selection date
  SELECT orderid materialnr plant ordernr
    FROM ppc_ord_inf INTO TABLE lt_ord
   WHERE orderid    IN ir_ordid AND
         orderid    IN ir_plnum AND
         ordernr    IN ir_ordnr AND
         materialnr IN ir_matnr AND
         plant      IN ir_plant "AND
*         zz_tmstp_final > 0 AND
*         zz_tmstp_final < iv_selordtime
    .

  IF NOT sy-subrc IS INITIAL.
    RETURN.
  ENDIF.

  SORT lt_ord BY orderid.
  DELETE ADJACENT DUPLICATES FROM lt_ord.

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

  lt_tmp_del = lt_ord_del.
  LOOP AT lt_tmp_del INTO ls_ord_del
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
    APPEND ls_ord_del-orderid TO et_ordid.
  ENDLOOP.

ENDFORM.                    " GET_ORDERS
*&---------------------------------------------------------------------*
*&      Form  SET_ORDER_DELETION
*&---------------------------------------------------------------------*
FORM set_order_deletion   USING    it_conf_ordid TYPE ppc_t_orderid.

  DATA: ls_return   TYPE bapiret2.

  CHECK NOT it_conf_ordid IS INITIAL.

  IF p_test = space.
    CALL FUNCTION 'Z_PPC1TP_ORDERS_DELETE'
      EXPORTING
        if_plaf         = p_plaf
      TABLES
        it_conf_orderid = it_conf_ordid
      EXCEPTIONS
        update_error    = 1
        delete_error    = 2
        OTHERS          = 3.
  ENDIF.

  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ELSE.
    DATA: ls_ord_del TYPE ts_ord_del.
    LOOP AT it_conf_ordid INTO ls_ord_del.
      WRITE:/ ls_ord_del-orderid, '.. deleted'.
    ENDLOOP.
  ENDIF.


ENDFORM.                    " SET_ORDER_DELETION
