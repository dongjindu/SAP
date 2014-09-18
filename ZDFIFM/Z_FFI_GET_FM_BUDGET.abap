FUNCTION z_ffi_get_fm_budget.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(GEBER) LIKE  BPPE-GEBER
*"  TABLES
*"      OUT STRUCTURE  ZFI_FM_BUDGET
*"----------------------------------------------------------------------

  DATA : it_bppe LIKE bppe OCCURS 0 WITH HEADER LINE.
  DATA : wa_t_cnt TYPE i,
         wa_parent_obj LIKE fmhictr-parent_obj.


  SELECT * INTO TABLE it_bppe
  FROM bppe
  WHERE geber EQ geber.

  DESCRIBE TABLE it_bppe LINES wa_t_cnt.
  IF wa_t_cnt > 0.
    LOOP AT it_bppe.
      CLEAR : wa_parent_obj.
      SELECT SINGLE parent_obj INTO wa_parent_obj
          FROM fmhictr
          WHERE ctr_objnr = it_bppe-objnr.
      IF wa_parent_obj <> space.
        MOVE-CORRESPONDING it_bppe TO out.
        out-tot = it_bppe-wtp01 + it_bppe-wtp02 + it_bppe-wtp03
                + it_bppe-wtp04 + it_bppe-wtp05 + it_bppe-wtp06
                + it_bppe-wtp07 + it_bppe-wtp08 + it_bppe-wtp09
                + it_bppe-wtp10 + it_bppe-wtp11 + it_bppe-wtp12.
        COLLECT OUT.
        CLEAR   OUT.
      ENDIF.
    ENDLOOP.
  ENDIF.
ENDFUNCTION.
