FUNCTION ZMMF_IF_PO_CREATE.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(I_ZSMM_IF002) LIKE  ZSMM_IF002 STRUCTURE  ZSMM_IF002
*"  TABLES
*"      T_ITEM STRUCTURE  ZSMM_IF009
*"      T_CONDITION STRUCTURE  ZSMM_IF010
*"      T_SERVICE STRUCTURE  ZSMM_IF011 OPTIONAL
*"      E_RETURN STRUCTURE  BAPIRETURN
*"----------------------------------------------------------------------
*&------------------------------------------------------------------
*& Program ID     : ZMMF_IF_PO_CREATE
*& Profram Name   : Purchase Order Create
*& Created by     : Seong Geon Cho
*& Created on     : 11.22.2005
*& Development ID : *
*& Reference Pgm. : *
*& Description    : *
*&
*& Modification Log
*&====================================================================
*& Date        Developer      Request ID   Description
*& 11/03/2006  Manju          UD1K922918   Fetch Cost center & G/L info
*                                          from PR instead
* 11/07/2006   Manju          UD1K922938   Get order number from PR
*&--------------------------------------------------------------------

*"----------------------------------------------------------------------

*-- v_flag : I -> Create, U -> Change
  CLEAR: v_po, v_return, v_return[],
         v_item, v_item[],
         v_condition, v_condition[],
         v_service, v_service[].

  CLEAR: v_ztmm_if016, v_ztmm_if016[].
  CLEAR v_flag.

  MOVE: i_zsmm_if002  TO v_po,
        t_item[]      TO v_item[],
        t_condition[] TO v_condition[],
        t_service[]   TO v_service[].

  MOVE: 'C' TO v_flag.

  PERFORM apply_conversion_rule.

  PERFORM save_if_table.
  READ TABLE v_return WITH KEY type = 'E'.
  IF sy-subrc EQ 0.
    EXIT.
  ENDIF.

  PERFORM check_parameters.

  PERFORM po_create.

  MOVE: v_return[] TO e_return[].
*  MOVE: v_return-type    TO ztmm_if004-type,
*        v_return-message TO ztmm_if004-message.
  LOOP AT v_return.
    MOVE-CORRESPONDING v_return TO v_ztmm_if016.
    MOVE: v_seqno  TO v_ztmm_if016-zseq,
          sy-tabix TO v_ztmm_if016-zsen.
    APPEND v_ztmm_if016.
  ENDLOOP.
  INSERT ztmm_if016 FROM TABLE v_ztmm_if016.

*  UPDATE ztmm_if004.





ENDFUNCTION.
