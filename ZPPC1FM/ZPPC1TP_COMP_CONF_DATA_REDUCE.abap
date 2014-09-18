function zppc1tp_comp_conf_data_reduce.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  TABLES
*"      IT_POSTED_CONFIRM TYPE  PPC_T_SEND_CONF_DATA
*"  EXCEPTIONS
*"      MISSING_INPUT
*"      UPDATE_ERROR
*"----------------------------------------------------------------------
*  OSSADDON1 UD1K909661: performance improvement: since WAMNG is not
*  computed here anymore, no need to read RESB data into internal tables


  data:
    lt_posted_confirm type tt_posted_confirm,
    lt_ppc1tp_plaf type table of ppc1tp_plaf,
    lt_ppc1tp_plau type table of ppc1tp_plaf.


* Check for input data
  if it_posted_confirm[] is initial.
    message e022(ppc1pr) raising missing_input.
  endif.
* Populate lt_posted_confirm - add additional info
  perform fill_lt_posted_confirm
                tables it_posted_confirm
                       lt_posted_confirm.
* Exit when lt_posted_confirm is empty (we had only activities)
  check not lt_posted_confirm is initial.


* Read planned order data for the posted confirmations
  perform determine_plaf
                tables lt_posted_confirm
                       lt_ppc1tp_plaf.

* Update internal table for PLAF with the confirmed quantities
* and modify the database table RESB
  perform update_resb_plaf
                tables lt_posted_confirm
                       lt_ppc1tp_plaf
                       lt_ppc1tp_plau.

* Modify database table PLAF
  call function 'ZPPC1TP_UPDATE_DELETE_PLAF_RES' in update task
       tables
            i_ppc1tp_plaf_upd = lt_ppc1tp_plau
       exceptions
            others            = 1.
  if sy-subrc <> 0.
    message id sy-msgid type sy-msgty number sy-msgno
            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
            raising update_error.
  endif.

endfunction.
