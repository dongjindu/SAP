*&---------------------------------------------------------------------*
*& Report  ZSAPBF_PPC0_OCD
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  zsapbf_ppc0_ocd MESSAGE-ID zsapbf_ppc0.
INCLUDE zsapbf_ppc0_ocdtop.
INCLUDE zsapbf_ppc0_ocdf01.



START-OF-SELECTION.

*  IF p_ordday > p_confds.
** Error message:
** 'Restriction for orders has priority over restriction for backflushes'
*    MESSAGE e610.
*  ENDIF.

  PERFORM init_data USING    "p_ordday
                             p_confds
                             sy-uzeit
                             sy-datum
*                             sy-zonlo
                    CHANGING "gv_selordtime
                             gv_selconftime.

  IF p_protok = gc_true.
    PERFORM log_create USING sy-datlo
                             sy-timlo
                             sy-uname
                             sy-tcode
                             sy-repid
                             gc_al_object
                             gc_al_subobj
                    CHANGING gv_log_handle.
  ENDIF.

  PERFORM get_orders  USING    so_ordid[]
                               so_plord[]
                               so_matnr[]
                               so_plant[]
                               "gv_selordtime
                               gv_selconftime
                      CHANGING gt_ordid .
  IF gt_ordid IS INITIAL.
    MESSAGE i613.
    EXIT.
  ENDIF.

  IF p_test = gc_true AND p_protok = gc_true.
    DATA: lv_orderid  TYPE ppc_orderid,
          lv_msg_text TYPE bapi_msg.
    LOOP AT gt_ordid INTO lv_orderid. "gs_ordid.
      CONCATENATE text-012 lv_orderid ')' INTO lv_msg_text
        SEPARATED BY space.
      PERFORM add_msg USING gc_freetext_msgid
                            gc_freetext_msgno
                            gc_msgty-info
                            '' '' '' ''
                            gc_true
                            lv_msg_text
                   CHANGING gv_log_exist.

** Confirmations for order can be deleted (xxx)
*      PERFORM add_msg USING    gc_msgid
*                               gc_msgno_942
*                               gc_msgty-soft
*                               gs_ordid
*                               '' '' ''
*                               '' ''
*                      CHANGING gv_log_exist.
    ENDLOOP.
  ENDIF.

  IF p_test IS INITIAL.
    PERFORM block_process USING    gt_ordid
                          CHANGING gv_log_exist.
  ENDIF.

  IF p_protok = gc_true AND
    ( NOT gv_log_exist IS INITIAL ).

    PERFORM log_show USING gv_log_handle
                           gc_true.
  ENDIF.

END-OF-SELECTION.
