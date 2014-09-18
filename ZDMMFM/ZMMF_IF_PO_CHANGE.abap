FUNCTION ZMMF_IF_PO_CHANGE.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(I_ZSMM_IF002) LIKE  ZSMM_IF002 STRUCTURE  ZSMM_IF002
*"     REFERENCE(I_CHECK) TYPE  CHAR1 OPTIONAL
*"  TABLES
*"      T_ITEM STRUCTURE  ZSMM_IF009
*"      T_CONDITION STRUCTURE  ZSMM_IF010
*"      T_SERVICE STRUCTURE  ZSMM_IF011 OPTIONAL
*"      E_RETURN STRUCTURE  BAPIRETURN
*"----------------------------------------------------------------------
*&------------------------------------------------------------------
*& Program ID     : ZMMF_IF_PO_CHANGE
*& Profram Name   : Purchase Order Change
*& Created by     : Seong Geon Cho
*& Created on     : 11.28.2005
*& Development ID : *
*& Reference Pgm. : *
*& Description    : *
*&
*& Modification Log
*&====================================================================
*& Date     Developer      Request ID      Description
*&
*&--------------------------------------------------------------------

*"----------------------------------------------------------------------
*-- t_item-zflag = D : Delivery Completed Indicator
*-- t_item-zflag = R : PO del.
  DATA: l_ebeln LIKE ekko-ebeln.
  DATA: l_message LIKE bapireturn-message.

  CLEAR: v_ztmm_if016, v_ztmm_if016[].
  CLEAR: l_ebeln, l_message.
  CLEAR: v_check,
         v_po, v_return,
         v_item, v_item[],
         v_condition, v_condition[],
         v_service, v_service[],
         v_purchaseorder.
*-- 2005.12.19??
*-- Modify, Delete
  CLEAR: v_item_change, v_item_change[].
  CLEAR: v_item_del,    v_item_del[].

  CLEAR v_flag.

  CLEAR: ret2, ret2[].

  MOVE: i_check            TO v_check,
        i_zsmm_if002       TO v_po,
        t_item[]           TO v_item[],
        t_condition[]      TO v_condition[],
        t_service[]        TO v_service[],
        i_zsmm_if002-ebeln TO v_purchaseorder.

  MOVE: v_check TO v_flag.

  PERFORM apply_conversion_rule.

  perform split_item.

  PERFORM save_if_table.


*-- PO exist Check
  SELECT SINGLE ebeln INTO l_ebeln FROM ekko
  WHERE ebeln = v_po-ebeln.

  IF sy-subrc NE 0.
    CONCATENATE 'Document ' v_po-ebeln
                ' does not exist' INTO l_message.
    MOVE: 'E'       TO v_return-Type,
          l_message TO v_return-message.
    APPEND v_return.
    MOVE: v_return[] TO e_return[].

    LOOP AT v_return.
      MOVE-CORRESPONDING v_return TO v_ztmm_if016.
      MOVE: v_seqno  TO v_ztmm_if016-zseq,
            sy-tabix TO v_ztmm_if016-zsen.
      APPEND v_ztmm_if016.
    ENDLOOP.
    INSERT ztmm_if016 FROM TABLE v_ztmm_if016.
    EXIT.
  ENDIF.

  PERFORM po_change.

  MOVE: v_return[] TO e_return[].

  LOOP AT v_return.
    MOVE-CORRESPONDING v_return TO v_ztmm_if016.
    MOVE: v_seqno  TO v_ztmm_if016-zseq,
          sy-tabix TO v_ztmm_if016-zsen.
    APPEND v_ztmm_if016.
  ENDLOOP.
  INSERT ztmm_if016 FROM TABLE v_ztmm_if016.

ENDFUNCTION.
