FUNCTION z_mm_if_gr_box_post .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(I_LIFEX) TYPE  LIFEX OPTIONAL
*"  EXPORTING
*"     VALUE(E_SUBRC) TYPE  SYSUBRC
*"     VALUE(E_MSG) TYPE  CHAR255
*"  TABLES
*"      PDA_HEADER STRUCTURE  ZMMS_PDA001 OPTIONAL
*"      PDA_ITEM STRUCTURE  ZMMS_PDA002 OPTIONAL
*"      PDA_MSG STRUCTURE  ZMMS_PDA003 OPTIONAL
*"----------------------------------------------------------------------
  data: LT_REHANG TYPE TABLE OF HUM_REHANG_HU.

  CLEAR : g_lifex, g_msg,   g_subrc, g_type,
          e_subrc, e_msg,   g_vbeln, g_vbtyp,
          g_venum, g_posnr, g_werks, g_matnr,
          g_vemng, g_vemeh.

  CLEAR : it_zmms_pda001, it_zmms_pda001[],
          it_zmms_pda002, it_zmms_pda002[],
          it_zmms_pda003, it_zmms_pda003[].

  CLEAR : pda_header, pda_header[],
          pda_msg,    pda_msg[].

  CLEAR : vbkok_wa,
          vbpok_tab,  vbpok_tab[],
          prot,       prot[].
*          it_objects, it_objects[].

*.. Data Move
  MOVE : i_lifex TO g_lifex.
  MOVE : pda_item[] TO it_zmms_pda002[].

*.. External Identification of Delivery Note?? ???? ???? ??
  IF g_lifex IS INITIAL.
    MOVE : 'E'      TO g_type,
           '99'     TO e_subrc,
           text-m16 TO e_msg.

    PERFORM error_log USING 'MMIF_PDA_04' 'PDA' 'US' 'I' ' ' 'S' g_type
                            e_msg ''  ''  ''.
    EXIT.
  ENDIF.

*.. PDA_ITEM? ???? ???? ??
  IF it_zmms_pda002[] IS INITIAL.
    MOVE : 'E'  TO g_type,
           '88' TO e_subrc.
    PERFORM build_message USING e_msg 'ZMMPDA' '015'
                                '' '' '' ''.

    PERFORM error_log USING 'MMIF_PDA_04' 'PDA' 'US' 'I' ' ' 'S' g_type
                            e_msg  g_lifex g_lifex+0(4)  ''.
    EXIT.
  ENDIF.

*  DATA : lt_ytemp_pda LIKE ytemp_pda OCCURS 0 WITH HEADER LINE.
*  CLEAR : lt_ytemp_pda, lt_ytemp_pda[].
*  LOOP AT it_zmms_pda002.
*    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*      EXPORTING
*        input  = it_zmms_pda002-exidv
*      IMPORTING
*        output = it_zmms_pda002-exidv.

*    MOVE-CORRESPONDING it_zmms_pda002 TO lt_ytemp_pda.
*    MOVE : sy-datum TO lt_ytemp_pda-erdat,
*           sy-uzeit TO lt_ytemp_pda-erzet,
*           sy-uname TO lt_ytemp_pda-ernam.
*    APPEND lt_ytemp_pda.  CLEAR lt_ytemp_pda.
*    modify it_zmms_pda002.  clear it_zmms_pda002.
*  ENDLOOP.

*  INSERT ytemp_pda FROM TABLE lt_ytemp_pda.
*  COMMIT WORK AND WAIT.

*.. Sales Document??
  PERFORM get_sales_document.

  IF g_vbeln IS INITIAL.
    MOVE : 'E'      TO g_type,
           '99'     TO g_subrc,
           text-m17 TO g_msg.
  ELSE.
*.. Header Data Input.
    PERFORM input_delivery_header.

*.. Item Input
    PERFORM input_delivery_item   changing LT_REHANG .

*.. Delivery Update
    PERFORM delivery_update_rtn   using LT_REHANG .


  ENDIF.

  PERFORM error_log USING 'MMIF_PDA_04' 'PDA' 'US' 'I' ' ' 'S'
                          g_type g_msg
                          g_lifex g_lifex(4) g_vbeln.

  MOVE : g_subrc TO e_subrc,
         g_msg   TO e_msg.

ENDFUNCTION.
