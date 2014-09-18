FUNCTION zmigo_badi_getis_put_data.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IS_MIGO_BADI_SCREEN_FIELDS) TYPE  ZHKPMS0001
*"  TABLES
*"      T_ITEM STRUCTURE  ZHKPMS0002
*"----------------------------------------------------------------------

  MOVE-CORRESPONDING is_migo_badi_screen_fields TO zhkpms0001 .

  IF gt_item[] IS NOT INITIAL .
    CLEAR gt_item[] .
  ENDIF .

  LOOP AT t_item .
    MOVE-CORRESPONDING t_item TO gt_item .
    append gt_item .
  ENDLOOP .

ENDFUNCTION.
