FUNCTION zsapbf_ppc_ord_inf_read.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IV_ORDERID) TYPE  PPC_ORDERID OPTIONAL
*"     REFERENCE(IV_ORDERNR) TYPE  PPC_ORDERNR OPTIONAL
*"  EXPORTING
*"     VALUE(ES_PPC_ORD_INF) TYPE  PPC_ORD_INF
*"     VALUE(ET_PPC_ORD_INF) TYPE  ZSAPBF_TT_PPC_ORD_INF
*"  EXCEPTIONS
*"      INSUFFICIENT_INPUT
*"      NO_RECORD_FOUND
*"----------------------------------------------------------------------

  IF iv_orderid    IS INITIAL AND
     iv_ordernr    IS INITIAL.
    RAISE insufficient_input.
  ENDIF.

  CLEAR: es_ppc_ord_inf, et_ppc_ord_inf.

  IF iv_orderid IS NOT INITIAL.
*1. read global buffer
    READ TABLE gt_ord_inf INTO es_ppc_ord_inf WITH TABLE KEY orderid = iv_orderid.
    IF sy-subrc EQ 0.
      APPEND es_ppc_ord_inf TO et_ppc_ord_inf.
    ELSE.
*2. select from DB
      SELECT * FROM ppc_ord_inf
               INTO TABLE et_ppc_ord_inf
              WHERE orderid = iv_orderid.
      READ TABLE et_ppc_ord_inf INTO es_ppc_ord_inf INDEX 1.
      INSERT LINES OF et_ppc_ord_inf INTO TABLE gt_ord_inf.
    ENDIF.
  ELSEIF iv_ordernr IS NOT INITIAL.
*1. read global buffer
    LOOP AT gt_ord_inf INTO es_ppc_ord_inf WHERE ordernr = iv_ordernr.
      APPEND es_ppc_ord_inf TO et_ppc_ord_inf.
      CLEAR  es_ppc_ord_inf.
    ENDLOOP.
    IF et_ppc_ord_inf IS NOT INITIAL.
      READ TABLE et_ppc_ord_inf INTO es_ppc_ord_inf INDEX 1.
    ELSE.
*2. select from DB
      SELECT * FROM ppc_ord_inf
               INTO TABLE et_ppc_ord_inf
              WHERE ordernr = iv_ordernr.
      IF sy-subrc EQ 0.
        INSERT LINES OF et_ppc_ord_inf INTO TABLE gt_ord_inf.
        READ TABLE et_ppc_ord_inf INTO es_ppc_ord_inf INDEX 1.
      ENDIF.
    ENDIF.
  ENDIF.

  IF et_ppc_ord_inf IS INITIAL.
    RAISE no_record_found.
  ENDIF.


ENDFUNCTION.
