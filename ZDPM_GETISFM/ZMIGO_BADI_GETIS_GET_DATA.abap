FUNCTION zmigo_badi_getis_get_data.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     REFERENCE(ES_MIGO_BADI_SCREEN_FIELDS) TYPE  ZHKPMS0001
*"  TABLES
*"      T_ITEM STRUCTURE  ZHKPMS0002
*"----------------------------------------------------------------------

  MOVE-CORRESPONDING zhkpms0001 TO es_migo_badi_screen_fields .

  LOOP AT gt_item .
    MOVE-CORRESPONDING gt_item TO t_item .
    APPEND t_item .
  ENDLOOP .

ENDFUNCTION.
