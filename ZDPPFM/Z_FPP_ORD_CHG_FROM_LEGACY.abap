FUNCTION z_fpp_ord_chg_from_legacy.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(WA_LOGFILE) TYPE  ZSPPVR
*"     VALUE(MSG) TYPE  ZPP_MESSAGE OPTIONAL
*"  TABLES
*"      MESSAGE STRUCTURE  ZSPP_FUNC_MESSAGE OPTIONAL
*"----------------------------------------------------------------------
* Modification Logs
* Date       Developer    RequestNo    Description
*04/26/2005  Chris       UD1K915744  For spec change, the original plan
*                                    order creation date should be keep
*                                   (finish date, start date, open date)
*
* 11/09/2006 Manju       UD1K922975  When spec change take place for VIN
*                                    number, update the same in
*                                    equipment master header data.
* 11/16/2006 Manju       UD1K923093  Update equipment master with
*                                    changed VIN number.
*&----------------------------------------------------------------------

  DATA: l_plnum_new TYPE plaf-plnum,  "Plan Order Number
        l_plnum_old TYPE plaf-plnum,
        l_result_flg .

  DATA: l_flag               TYPE c               ,
        L_FIRM               TYPE c               ,
        l_rp(2)              TYPE c               ,
        l_fsc                LIKE ztpp_wosum-fsc  ,
        l_sorder_new         LIKE ztpp_wosum-sales,
        l_sorder_old         LIKE ztpp_wosum-sales,
        l_version            LIKE mkal-verid      ,
        l_new_order          LIKE plaf-plnum      ,
        l_old_order          LIKE plaf-plnum      ,
        l_mat_new            LIKE mara-matnr      ,
        l_mat_old            LIKE mara-matnr      .
  DATA: l_it_vm_new LIKE TABLE OF zspp_vin_value WITH HEADER LINE .
  DATA: l_it_vm_old LIKE TABLE OF zspp_vin_value WITH HEADER LINE .

* Start The Process. (Clear the Internal Table & Variables)
  CLEAR: it_message, it_message[], it_logfile, it_logfile[].
  msg = 'Start of LS ' .
*
*  MOVE log_file[] TO it_logfile[].
  MOVE-CORRESPONDING wa_logfile TO it_logfile.
  APPEND it_logfile.
*
  CLEAR: it_msg_tab, it_msg_tab[], l_flag.
  LOOP AT it_logfile WHERE ( dvrs = 'A' OR dvrs = 'M' ) AND flag = 'LS'.
    PERFORM clear_internal_tables.
    CLEAR: WA_OVERSEQ,   WA_UNDERSEQ,  L_FLAG, MSG.
    CLEAR: wa_equip_new, wa_equip_old.  "<--- Equipment Number
    CLEAR: l_plnum_new, l_plnum_old.    "<--- Planned Order Number
**************************************
* Case1 : Swapping from MES.
**************************************
    IF it_logfile-dvrs = 'M'.  "For Swapping.
      " 0. Set the BASIC Step for the SWAPPING Processing...
      CONCATENATE it_logfile-model2   it_logfile-body_no2
        INTO wa_equip_old.
      CONCATENATE it_logfile-p_model  it_logfile-p_body_serial
        INTO wa_equip_new.

      " 1. READ the vehicle Information.
      CLEAR: it_vm_old, it_vm_new, it_vm_old[], it_vm_new[].
      CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
           EXPORTING
                object       = wa_equip_old
                DISPLAY      = 'X'
           TABLES
                val_table    = it_vm_old
      EXCEPTIONS
        NO_DATA            = 1
        ERROR_MODE         = 2
        ERROR_OBJECT       = 3
        ERROR_VALUE        = 4
        OTHERS             = 5 .

      CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
           EXPORTING
                object       = wa_equip_new
                DISPLAY      = 'X'
           TABLES
                val_table    = it_vm_new
      EXCEPTIONS
        NO_DATA            = 1
        ERROR_MODE         = 2
        ERROR_OBJECT       = 3
        ERROR_VALUE        = 4
        OTHERS             = 5 .

      " 2. Check the STATUS and Plan Order Information.
      PERFORM check_status   USING l_flag       .
      IF l_flag = 'E'.
         it_message-message = MSG = IT_MESSAGE-MESSAGE.
         MOVE 'E'                        TO it_message-result_type.
         APPEND it_message .
         EXIT.
      ENDIF.

      " Updating Vehicle B/F Status Table.
      PERFORM update_bf_status USING l_FLAG .
      IF l_flag = 'E'.
         it_message-message = MSG = IT_MESSAGE-MESSAGE.
         MOVE 'E'                        TO it_message-result_type.
         APPEND it_message .
         EXIT.
      ENDIF.

      " 3. Change the Wrok-Order Summary
      perform change_wosum_swapp  using l_flag.

      " 4. Change the Vehicle Information
      PERFORM change_vm_others.
      CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
           EXPORTING
                object       = wa_equip_old
                mode         = 'W'
           TABLES
                val_table    = it_vm_new
      EXCEPTIONS
        NO_DATA            = 1
        ERROR_MODE         = 2
        ERROR_OBJECT       = 3
        ERROR_VALUE        = 4
        OTHERS             = 5 .

      CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
           EXPORTING
                object       = wa_equip_new
                mode         = 'W'
           TABLES
                val_table    = it_vm_old
      EXCEPTIONS
        NO_DATA            = 1
        ERROR_MODE         = 2
        ERROR_OBJECT       = 3
        ERROR_VALUE        = 4
        OTHERS             = 5 .

      " 4. Vehicle Master Information Change..
      PERFORM CALL_BAPI_EQUIPHEADER   USING wa_equip_new
                                            wa_equip_OLD  L_FLAG.
      IF l_flag = 'E'.
         it_message-message = MSG = IT_MESSAGE-MESSAGE.
         MOVE 'E'                        TO it_message-result_type.
         APPEND it_message .
         EXIT.
      ENDIF.

      " 5. Create the Change Log in ZTPP_CHANGE Table...
      PERFORM write_veh_change_log_for_SWAP  USING l_flag  msg.


**************************************
* Case2 : Spec Change from ALC.
**************************************
    ELSEIF it_logfile-dvrs = 'A'.          "For Spec Change.
      " Check the following logic..
      " 0. Keep the Vehicle Body-Number...
      CONCATENATE it_logfile-p_model it_logfile-p_body_serial
             INTO wa_equip_new.

      PERFORM get_vmaster        USING  wa_equip_new  l_flag  msg.
      IF l_flag = 'E'.
        CLEAR it_message.
        MOVE 'ERR'                         TO it_message-vm_no.
        MOVE wa_equip_new                  TO it_message-woh_no.
        CONCATENATE text-103  wa_equip_new INTO it_message-message.
        MOVE 'E'                           TO it_message-result_type.
        APPEND it_message .
        EXIT.
      ENDIF.

      " 1. Check the MITU Information.....
      read table it_vm_NEW with key atnam = 'P_MITU' .
      if sy-subrc = 0 and it_vm_NEW-atwrt = 'Y'      .
         wa_mitu  = 'Y'.
      else.
         wa_mitu  = ' '.
      endif.

      " 2. Get the information for the New Work-Order from ZTPP_WOSUM
      " 3. New Work-Order(including the color information) is already
      "    exist in the ZTPP_WOSUM File with created sales order..
      "    ---> if not exist, Raise the error...
      CONCATENATE wa_logfile-p_work_order   wa_logfile-p_ext_color
                  wa_logfile-p_int_color    INTO  l_mat_new        .
      READ TABLE it_vm_NEW WITH KEY atnam = 'P_WORK_ORDER'         .
      l_mat_OLD = it_vm_NEW-atwrt .
      READ TABLE it_vm_NEW WITH KEY atnam = 'P_EXT_COLOR'          .
      CONCATENATE l_mat_OLD it_vm_NEW-atwrt  INTO  l_mat_OLD       .
      READ TABLE it_vm_NEW WITH KEY atnam = 'P_INT_COLOR'          .
      CONCATENATE l_mat_OLD it_vm_NEW-atwrt  INTO  l_mat_OLD       .
      PERFORM check_material USING l_mat_OLD l_mat_new l_flag  msg .
      IF l_flag = 'E'.   EXIT.    ENDIF.

      READ TABLE it_vm_NEW WITH KEY atnam = 'P_SALES_ORDER'        .
      l_sorder_OLD = it_vm_NEW-atwrt                               .
      PERFORM get_inform_wosum   USING  l_mat_new l_sorder_new
                                        l_fsc     l_version        .
      IF sy-subrc NE 0.
        CLEAR it_message.
        MOVE 'ERR'                      TO it_message-vm_no  .
        MOVE l_mat_new                  TO it_message-woc_no .
        msg = 'Error of Work Order Summary.. not found.'     .
        CONCATENATE text-100  l_mat_new INTO it_message-message.
        MOVE 'E'                        TO it_message-result_type.
        APPEND it_message .
        EXIT.
      ENDIF.

      " 4. Delete the Old Plan Order for the Vehicle Master..
      READ TABLE it_vm_NEW WITH KEY atnam = 'P_PLAN_ORDER'.
      IF SY-SUBRC = 0 AND IT_VM_NEW-ZFLAG = SPACE.
        L_OLD_ORDER = IT_VM_NEW-ATWRT.
*-->requested by MY HUR changed by chris
*    before deletion, read the origin date
       select single plnum PSTTR PEDTR PERTR
         into wa_plndate
         from plaf
         where plnum = l_old_order.
*-->end of change on 04/26/2005
        PERFORM DELETE_OLD_PORDER   USING L_OLD_ORDER L_FLAG .
        IF L_FLAG = 'X'.
          MSG = it_message-message.
          MOVE 'E'                        TO it_message-result_type.
          APPEND it_message .
          EXIT.
        ENDIF.
      ELSE.
        l_flag = 'E'.
        it_message-message = msg = TEXT-010 .
        MOVE 'E'                        TO it_message-result_type.
        APPEND it_message .
        EXIT.
      ENDIF.

      " 5. Create the New Plan Order with 3'rd information..
      SELECT SINGLE id_point INTO l_rp
        FROM ztpp_status
       WHERE id = wa_logfile-p_status .
      IF SY-SUBRC = 0 AND L_RP >= '01' .
         L_FIRM   = 'X' .
      ELSE.
         L_FIRM   = ' ' .
      ENDIF.
      PERFORM call_bdc_planned_order  USING  l_fsc         l_version
                                             sy-datum      l_sorder_new
                            CHANGING  l_new_order  L_FIRM  l_flag .
      IF l_flag = 'E'.
        it_message-message = msg = 'Error of create planned_order ..' .
        MOVE 'E'                        TO it_message-result_type.
        APPEND it_message .
        EXIT.
      ENDIF.

     " 6. Adjust sales order quentity with vehicle number's information.
      PERFORM call_function_for_sales_order USING l_sorder_old
                                                  l_mat_old
                                                  '-'
                                          CHANGING l_flag.
      IF WA_UNDERSEQ = 'E'.
        msg = 'Under Sequence of sales order (-).'    .
      ENDIF.
      IF l_flag = 'E'.
        it_message-message = msg = 'Error of update sales order (-).' .
        MOVE 'E'                        TO it_message-result_type.
        APPEND it_message .
        EXIT.
      ENDIF.

      PERFORM call_function_for_sales_order USING l_sorder_new
                                                  l_mat_new
                                                  '+'
                                            CHANGING l_flag.
      IF WA_OVERSEQ = 'E'.
        msg = 'Over Sequence of sales order (+).'    .
*       EXIT.
      ENDIF.
      IF l_flag = 'E'.
        it_message-message = msg = 'Error of update sales order (+).'
.
        MOVE 'E'                        TO it_message-result_type.
        APPEND it_message .
        EXIT.
      ENDIF.

      " 7. Adjust ZTPP_WOSUM file with old/new Work-Order information.
      READ TABLE it_vm_NEW WITH KEY atnam = 'P_RP_STATUS'          .
      l_rp = it_vm_NEW-atwrt.
      PERFORM change_wosum_revice USING  l_mat_new '+' l_rp .
      IF sy-subrc NE 0.
        CLEAR it_message.
        MOVE 'ERR'                      TO it_message-vm_no  .
        MOVE l_mat_new                  TO it_message-woc_no .
        CONCATENATE text-100  'PLUS'    INTO it_message-message.
        msg = 'Error of update WOSUM       (+).'     .
        MOVE 'E'                        TO it_message-result_type.
        APPEND it_message .
        EXIT.
      ENDIF.

      PERFORM change_wosum_revice USING  l_mat_old '-' l_rp .
      IF sy-subrc NE 0.
        CLEAR it_message.
        MOVE 'ERR'                      TO it_message-vm_no  .
        MOVE l_mat_new                  TO it_message-woc_no .
        CONCATENATE text-100  'MINUS'   INTO it_message-message.
        msg = 'Error of update WOSUM       (+).'     .
        MOVE 'E'                        TO it_message-result_type.
        APPEND it_message .
        EXIT.
      ENDIF.

      " 8. Create new record in the ZTPP_BFST table with New plan order.
      " 9. Copy the record information for old-record to new-record.
      "10. Set deletion-flag old record in the ZTPP_BFST table.
      READ TABLE it_vm_NEW WITH KEY atnam = 'P_PLAN_ORDER'         .
      l_old_order  = it_vm_NEW-atwrt                               .
      PERFORM create_bfst USING  l_old_order  l_new_order    l_flag
                            it_logfile-p_model it_logfile-p_body_serial.
      IF l_flag = 'E'.
        CLEAR it_message.
        MOVE 'ERR'                         TO it_message-vm_no.
        msg = 'Error of change BFST Table  ....'     .
        CONCATENATE text-105 'OLD :' l_old_order 'NEW :' l_new_order
                it_logfile-p_model   it_logfile-p_body_serial
               INTO it_message-message .
        it_message-result_type = 'E'             .
        APPEND it_message .
        EXIT.
      ENDIF.

      " 11. Change the Vehicle Master Information...
      PERFORM change_vm          USING  l_new_order  l_sorder_new
              wa_logfile-p_work_order     wa_logfile-p_ext_color
              wa_logfile-p_int_color      wa_logfile-p_vin
              wa_logfile-p_rp_actual_date wa_logfile-p_rp_actual_time
              wa_logfile-P_DEST_CODE      L_FLAG                     .

      IF l_flag = 'E'.
        it_message-message = msg = 'Error of Change Vehicle Master..'  .
        MOVE 'E'                        TO it_message-result_type.
        APPEND it_message .
        EXIT.
      ENDIF.

      " 12. Change the Work-Order Sequence Quantity Information...
      PERFORM change_workorder   USING  l_mat_old  '-'  l_flag.
      PERFORM change_workorder   USING  l_mat_new  '+'  l_flag.

      " 13. Marking the Oversequencing flag...
      PERFORM write_veh_change_log_for_spec  USING l_flag  msg.
    ENDIF.

    CLEAR it_message.
    msg = 'END of Function' .
    APPEND it_message .
  ENDLOOP.

  MOVE it_message[] TO message[] .
ENDFUNCTION.
