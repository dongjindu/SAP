FUNCTION z_bi_get_prd_summary_3c.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(I_CALDAY) TYPE  SY-DATUM DEFAULT SY-DATUM
*"     VALUE(I_AS) TYPE  ZRESULT DEFAULT 'E'
*"  TABLES
*"      I_3C_ALL STRUCTURE  ZSBIP_3C_ALL
*"      I_3C_ENG1 STRUCTURE  ZSBIP_3C_ENG1
*"      I_3C_ENG2 STRUCTURE  ZSBIP_3C_ENG2
*"----------------------------------------------------------------------

  RANGES r_model FOR zprdqtymodel-p_model .

  REFRESH: i_3c_all, i_3c_eng1, i_3c_eng2.

  SELECT eitem eassyid plant_cd erpid COUNT( * ) AS eqty  INTO
CORRESPONDING FIELDS OF TABLE i_3c_all
  FROM ztpperm
  WHERE prod_dt EQ i_calday
  AND zresult EQ 'S'
  AND ( erpid = 'E01' OR erpid = 'E02' OR erpid = 'E03' )
  GROUP by eitem eassyid plant_cd erpid.

  SORT i_3c_all BY eitem eassyid plant_cd erpid.

  DATA: $tabix LIKE sy-tabix.
  CLEAR $tabix.
  LOOP AT i_3c_all.

    $tabix = sy-tabix.

    CASE i_3c_all-erpid.
      WHEN 'E01'.
        i_3c_all-3cgrp = 'C/B'.
      WHEN 'E02'.
        i_3c_all-3cgrp = 'C/S'.
      WHEN 'E03'.
        i_3c_all-3cgrp = 'C/H'.
      WHEN OTHERS.
    ENDCASE.

    MODIFY i_3c_all INDEX $tabix.
    CLEAR i_3c_all.

  ENDLOOP.

* exclude AS part?
*  IF i_as EQ 'E'.
*    DELETE i_3c_all WHERE sleplant EQ 'AS'.
*  ENDIF.


* get summary table
  CLEAR i_3c_all.
  LOOP AT i_3c_all.
    CASE i_3c_all-plant_cd.
      WHEN 'ENG1'.
        MOVE-CORRESPONDING i_3c_all TO i_3c_eng1.
        COLLECT i_3c_eng1.
      WHEN 'ENG2'.
        MOVE-CORRESPONDING i_3c_all TO i_3c_eng2.
        COLLECT i_3c_eng2.
      WHEN OTHERS. " if plant code is blank
        IF i_3c_all-eitem(1) EQ '3'.
          MOVE-CORRESPONDING i_3c_all TO i_3c_eng1.
          COLLECT i_3c_eng1.
        ELSE.
          MOVE-CORRESPONDING i_3c_all TO i_3c_eng2.
          COLLECT i_3c_eng2.
        ENDIF.
    ENDCASE.
*    IF i_3c_all-plant_cd EQ 'ENG1'.
*      MOVE-CORRESPONDING i_3c_all TO i_3c_eng1.
*      COLLECT i_3c_eng1.
*    ENDIF.
*
*    IF i_3c_all-plant_cd EQ 'ENG2'.
*      MOVE-CORRESPONDING i_3c_all TO i_3c_eng2.
*      COLLECT i_3c_eng2.
*    ENDIF.

    CLEAR: i_3c_all, i_3c_eng1, i_3c_eng2.
  ENDLOOP.

ENDFUNCTION.
