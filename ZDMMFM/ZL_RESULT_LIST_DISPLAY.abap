FUNCTION zl_result_list_display.
*"----------------------------------------------------------------------
*"*"Global interface:
*"       IMPORTING
*"             VALUE(I_REPID) LIKE  SY-REPID
*"             VALUE(I_TITLE) LIKE  SY-TITLE
*"             VALUE(I_LGNUM) LIKE  LAGP-LGNUM
*"       TABLES
*"              T_EMKPF STRUCTURE  EMKPF
*"              T_LQUA_PROT STRUCTURE  LQUA_PROT
*"              T_IDOC_PROT STRUCTURE  SWOTOBJID
*"----------------------------------------------------------------------


  DESCRIBE TABLE t_lqua_prot LINES cnt_quants.

  CALL SCREEN 500.

ENDFUNCTION.

*---------------------------------------------------------------------*
*        AT LINE-SELECTION                                            *
*---------------------------------------------------------------------*
AT LINE-SELECTION.

*........Doppelklick...................................................
  PERFORM doubleclick_ll01lu02.
