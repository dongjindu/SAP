FUNCTION Z_MM_IF_GR_DELIVERY_POST .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(I_LIFEX) TYPE  LIFEX OPTIONAL
*"  EXPORTING
*"     VALUE(E_SUBRC) TYPE  SYSUBRC
*"     VALUE(E_MSG) TYPE  CHAR255
*"  TABLES
*"      PDA_MSG STRUCTURE  ZMMS_PDA003 OPTIONAL
*"----------------------------------------------------------------------

  CLEAR : g_lifex, g_vbeln, g_subrc, g_msg, g_type,
          e_subrc, e_msg,   g_vbtyp, g_budat.

  MOVE : i_lifex TO g_lifex.

*.. External Identification of Delivery Note?? ???? ???? ??
  IF g_lifex IS INITIAL.
    MOVE : 'E'      TO g_type,
           '99'     TO e_subrc,
           text-m16 TO e_msg.

    PERFORM error_log USING 'MMIF_PDA_02' 'PDA' 'US' 'I' ' ' 'S' g_type
                            e_msg ''  ''  ''.
    EXIT.
  ENDIF.

*.. Sales Document??
  PERFORM get_sales_document.

  IF g_vbeln IS INITIAL.
    MOVE : 'E'      TO g_type,
           '99'     TO g_subrc.
    PERFORM build_message USING g_msg 'ZMMPDA' '002'
                                '' '' '' ''.

  ELSE.
    IF sy-datum+6(2) = '01'.
      PERFORM changing_date USING    sy-datum
                                     sy-uzeit
                            CHANGING g_budat.
    ELSE.
      MOVE : sy-datum TO g_budat.
    ENDIF.

*.. Inbound delivery BDC? ???? ?? Data Input
    CLEAR : wa_option, bdcdata, bdcdata[],
            gt_bdcmsg, gt_bdcmsg[].

    PERFORM inbound_delivery_bdc.

*.. BDC Transaction??
    PERFORM call_inbound_delivery.

*.. BDC????
    PERFORM inbound_delivery_result.

  ENDIF.

  PERFORM error_log USING 'MMIF_PDA_02' 'PDA' 'US' 'I' ' ' 'S'
                          g_type g_msg
                          g_lifex g_lifex(4) g_vbeln.

  MOVE : g_subrc TO e_subrc,
         g_msg   TO e_msg.

ENDFUNCTION.
