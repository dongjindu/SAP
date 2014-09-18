FUNCTION zl_quant_select.
*"----------------------------------------------------------------------
*"*"Global interface:
*"  IMPORTING
*"     VALUE(I_REPID) LIKE  SY-REPID
*"     VALUE(I_STATUS) LIKE  SY-PFKEY
*"     VALUE(I_TITLE) LIKE  SY-TITLE
*"     VALUE(I_HEADER) TYPE  SLIS_T_LISTHEADER
*"     VALUE(I_SICHT) LIKE  RL01S-SICHT OPTIONAL
*"     VALUE(I_VARIANT) LIKE  DISVARIANT STRUCTURE  DISVARIANT OPTIONAL
*"     VALUE(I_CALLBACK) LIKE  SY-REPID OPTIONAL
*"     REFERENCE(I_STR_NAME) LIKE  DD02L-TABNAME OPTIONAL
*"     REFERENCE(I_LISTV) LIKE  RL01S-LISTV OPTIONAL
*"  TABLES
*"      T_LIST STRUCTURE  ZST_RL034
*"----------------------------------------------------------------------

*........Initialisierung...............................................

  PERFORM initialization_rl034.

*........Events definieren.............................................

  PERFORM define_events_rl034.

*........Special-Groups definieren.....................................
  IF NOT i_repid = 'RLLT0G00'.
    PERFORM define_special_groups_rl034.
  ENDIF.
*........Layout definieren.............................................

  PERFORM define_layout_rl034.

*........Variante initialisieren.......................................

  PERFORM variant_init_rl034.

*........Feldkatalog aufbauen..........................................
  IF i_repid = 'RLS10034'.
*........Umlagern per Mausklick........................................
    PERFORM build_fieldcat_rl034.

*    PERFORM BUILD_FILEDCAT_R1034_V2.

  ELSEIF i_repid = 'RLLQ0200'.
*........Umbuchen von unten............................................
    PERFORM build_fieldcat_rl200.
  ELSEIF i_repid = 'RLLT0G00'.
*........Rücklagern zur Lieferung......................................
    PERFORM build_fieldcat_rl031.
  ELSEIF i_repid = 'RLLI2110'.
*........Differenzen ausbuchen.........................................
    PERFORM build_fieldcat_rl032.
  ENDIF.

*........ALV aufrufen..................................................

  PERFORM call_alv_rl034.

ENDFUNCTION.
