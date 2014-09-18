FUNCTION z_fpp_bapi_plan_order_change.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(L_PLNUM) LIKE  ZTPP_BFST-PLAN_ORD
*"----------------------------------------------------------------------
  DATA:  l_header            LIKE bapiplaf_i2 ,
    l_headerx           LIKE bapiplaf_i2x,
    l_return            LIKE bapireturn1 ,
con_fix TYPE c VALUE 'X'.

  l_header-bom_exp_fix_ind = con_fix.
  l_headerx-bom_exp_fix_ind = con_fix.


  CALL FUNCTION 'BAPI_PLANNEDORDER_CHANGE'
        EXPORTING
            plannedorder = l_plnum
            headerdata   = l_header
            headerdatax  = l_headerx
           IMPORTING
                return       = l_return.
*       EXCEPTIONS
*            communication_failure       = 1
*            system_failure              = 2
*            RESOURCE_FAILURE            = 3 .
**          OTHERS                      = 4.

  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT' .


ENDFUNCTION.
