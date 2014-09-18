FUNCTION z_fpp_update_bfstatus_b01 .
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(L_ZSPPVR) LIKE  ZSPPVR STRUCTURE  ZSPPVR
*"----------------------------------------------------------------------
  DATA : l_atinn LIKE ausp-atinn.

  CLEAR : p_plan_ord, wa_ztpp_bfst,l_atinn.

  CONCATENATE  l_zsppvr-p_model l_zsppvr-p_body_serial INTO
               vin.
*Read plan order number
  CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
       EXPORTING
            input  = p_char
       IMPORTING
            output = l_atinn.

  SELECT atwrt INTO p_plan_ord UP TO 1 ROWS
      FROM ausp
        WHERE objek  EQ vin
          AND atinn  EQ l_atinn.
  ENDSELECT.

  IF l_zsppvr-p_status EQ 'B01' AND NOT p_plan_ord IS INITIAL.
*Initial Update in a backflush status table : In case RP 'B01'
    PERFORM initial_update_bfst USING l_zsppvr p_plan_ord.
  ENDIF.
*issue 20050218-001 requested by BHKIM , changed by wskim on 02/18/2005
*spec change backflush error : logic change
*-----start
  IF NOT p_plan_ord IS INITIAL.
    PERFORM plan_order_change USING p_plan_ord .
  ENDIF.
*-----end
ENDFUNCTION.
