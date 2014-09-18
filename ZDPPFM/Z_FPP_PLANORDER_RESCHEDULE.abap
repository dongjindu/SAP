FUNCTION Z_FPP_PLANORDER_RESCHEDULE.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(P_PORDER) LIKE  PLAF-PLNUM
*"     VALUE(P_DATE) LIKE  SY-DATUM
*"     VALUE(P_FIN_DATE) LIKE  SY-DATUM
*"  EXCEPTIONS
*"      COMMUNICATION_FAILURE
*"      SYSTEM_FAILURE
*"      RESOURCE_FAILURE
*"----------------------------------------------------------------------
  DATA: l_header       like bapiplaf_i2 ,
        l_headerx      like bapiplaf_i2x,
        l_return       like bapireturn1 .
  data: wa_plaf like plaf.
  data: lw_message(50).

  clear: ZTPP_INPUT_PLAN, L_HEADER, L_HEADERX, L_RETURN.

  select single * into wa_plaf from plaf where PLNUM = p_porder.

*FIX - 7/31/12 to avoid incorrect processing
  if sy-subrc <> 0.
    message s000(zmpp) with P_PORDER ' not exist!'.
    exit.
  endif.

  SELECT SINGLE *
    FROM ZTPP_INPUT_PLAN
   WHERE PLNUM = P_PORDER.

*performance fix; if same date, skip to change planned order - 7/31/12
  if wa_plaf-PSTTR = p_date and wa_plaf-pedtr = p_fin_date.
    if p_date >= wa_plaf-pertr. "open date is old... no need to update

       move p_date to ztpp_input_plan-rsnum .
       modify ztpp_input_plan from ztpp_input_plan.
       message s000(zmpp) with P_PORDER ' is skipped(same plan date)'.
       exit.
    endif.
  endif.


  l_header-ORDER_FIN_DATE   = P_fin_DATE.
*  l_header-PLAN_OPEN_DATE   = P_DATE.
  l_header-ORDER_START_DATE = P_DATE.
  l_headerX-ORDER_FIN_DATE  = 'X'.
* requested by MY Hur changed by chris--don't change open date
*  l_headerX-PLAN_OPEN_DATE = 'X'  .
  l_headerx-ORDER_START_DATE = 'X'  .

** added by Furong
  if p_date < wa_plaf-pertr.
     l_header-PLAN_OPEN_DATE   = P_DATE.
     l_headerX-PLAN_OPEN_DATE = 'X'.
  endif.

  CALL FUNCTION 'BAPI_PLANNEDORDER_CHANGE'
     EXPORTING
       plannedorder              = P_porder
       headerdata                = l_header
       headerdatax               = l_headerx
     IMPORTING
       RETURN                    = l_return.

  IF L_RETURN-TYPE = 'E' OR L_RETURN-TYPE = 'A' .

** on 02/06/13
     CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

         CALL FUNCTION 'MESSAGE_TEXT_BUILD'
       EXPORTING
         msgid                     = l_return-id
         msgnr                     = l_return-number
         MSGV1                     = l_return-message_v1
         MSGV2                     = l_return-message_v2
         MSGV3                     = l_return-message_v3
         MSGV4                     = l_return-message_v4
      IMPORTING
         MESSAGE_TEXT_OUTPUT       = lw_message.

       update ztpp_input_plan
         set: zmsg       = lw_message
        where plnt       = ztpp_input_plan-plnt
          and line       = ztpp_input_plan-line
          and modl       = ztpp_input_plan-modl
          and body_ser   = ztpp_input_plan-body_ser
          and seq_date   = ztpp_input_plan-seq_date
          and seq_serial = ztpp_input_plan-seq_serial
          and seq_code   = ztpp_input_plan-seq_code.

        commit work and wait.

**
**     CALL FUNCTION 'MESSAGE_TEXT_BUILD'
**       EXPORTING
**         msgid                     = l_return-id
**         msgnr                     = l_return-number
**         MSGV1                     = l_return-message_v1
**         MSGV2                     = l_return-message_v2
**         MSGV3                     = l_return-message_v3
**         MSGV4                     = l_return-message_v4
**      IMPORTING
**         MESSAGE_TEXT_OUTPUT       = lw_message
**               .
*     write:/ 'Failed order: ', P_PORDER , P_DATE, l_return-MESSAGE_V1,
*        l_return-MESSAGE_V2 .
** End on 02/06/13

  ELSE.
*---start #2 wskim 02/26/2005
*change field attribute : numeric type -> date type
*     write p_date to ztpp_input_plan-rsnum .
     move p_date to ztpp_input_plan-rsnum .
*---end
     modify ztpp_input_plan from ztpp_input_plan.
     CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
       EXPORTING
         WAIT          = 'X' .
  ENDIF.










ENDFUNCTION.
