FUNCTION z_mm_if_empty_kanban_w_s .
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(I_WERKS) TYPE  WERKS_D OPTIONAL
*"     VALUE(I_MATNR) TYPE  MATNR OPTIONAL
*"  EXPORTING
*"     VALUE(E_SUBRC) TYPE  SYSUBRC
*"     VALUE(E_MSG) TYPE  CHAR255
*"  TABLES
*"      PDA_TAB STRUCTURE  ZMMS_PDA009 OPTIONAL
*"----------------------------------------------------------------------

  CLEAR : g_werks, g_matnr, g_subrc, g_msg, g_type,
          e_subrc, e_msg.
  CLEAR : g_tempb, g_rksta, g_flag.

  CLEAR : pda_tab,         pda_tab[],
          it_work_station, it_work_station[].

  MOVE : i_werks TO g_werks,
         i_matnr TO g_matnr.

*.. Temperature conditions indicator Search.
  PERFORM get_tempb.
  if g_tempb is initial.
    MOVE : 'E' TO g_type,
           99  TO g_subrc.
    PERFORM build_message USING g_msg 'ZMMPDA' '017' '' '' '' ''.

  else.
*.. Supply Area? PDA? ???? ?? Data? ??
    PERFORM assort_select_supply.

*..
    IF g_flag IS INITIAL.
      PERFORM get_supply_area.

      IF it_work_station[] IS INITIAL.
        MOVE : 'E'    TO g_type,
               88     TO g_subrc.
        PERFORM build_message USING g_msg 'ZMMPDA' '000'
                                    'No control cycle exist' '' '' ''.
      ELSE.
        MOVE : 'S'      TO g_type,
               text-m15 TO g_msg.
      ENDIF.
    ELSE.
      MOVE : 'E' TO g_type,
             99  TO g_subrc.
      if g_tempb = '3'.  "event driven
      PERFORM build_message USING g_msg 'ZMMPDA' '100'
              'S2L: Kanban Type should be Event-Driven' '' '' ''.
      else.
            PERFORM build_message USING g_msg 'ZMMPDA' '100'
              'B/F cycle should be S2L or M/Kanban' '' '' ''.
      endif.
    ENDIF.
  endif.


  PERFORM error_log USING 'MMIF_PDA_11' 'PDA' 'US' 'I' ' ' 'S' g_type
                           g_msg        g_werks g_matnr ''.

  MOVE : g_subrc TO e_subrc,
         g_msg   TO e_msg.
  MOVE : it_work_station[] TO pda_tab[].

ENDFUNCTION.
