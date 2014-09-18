FUNCTION z_bi_get_prd_summary_engine.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(I_CALDAY) TYPE  SY-DATUM DEFAULT SY-DATUM
*"     VALUE(I_AS) TYPE  ZRESULT DEFAULT 'E'
*"  TABLES
*"      I_ENG_ALL STRUCTURE  ZSBIP_ENG_ALL
*"      I_ENGPLANT STRUCTURE  ZSBIP_ENG_BY_ENGPLANT
*"      I_SLEPLANT STRUCTURE  ZSBIP_ENG_BY_SLEPLANT
*"----------------------------------------------------------------------

  RANGES r_model FOR zprdqtymodel-p_model .

  REFRESH: i_eng_all, i_engplant, i_sleplant.

  SELECT eitem eassyid plant_cd COUNT( * ) AS eqty  INTO
CORRESPONDING FIELDS OF TABLE i_eng_all
  FROM ztpperm
  WHERE prod_dt EQ i_calday
  AND zresult EQ 'S'
  AND erpid EQ 'E05'
  GROUP by eitem eassyid plant_cd.

  SORT i_eng_all BY eitem eassyid plant_cd.

  DATA: $tabix LIKE sy-tabix.
  CLEAR $tabix.
  LOOP AT i_eng_all.

    $tabix = sy-tabix.
    SELECT SINGLE ladgr INTO i_eng_all-ladgr
    FROM marc
    WHERE ( werks = 'E001' or werks = 'E002' )
      AND matnr = i_eng_all-eitem.

    IF i_eng_all-ladgr EQ 'P500'.
      i_eng_all-sleplant = 'KMMG'.
    ELSEIF i_eng_all-ladgr EQ 'P200'.
      i_eng_all-sleplant = 'AS'.
    ELSE.
      i_eng_all-sleplant = 'HMMA'.
    ENDIF.

* to delete duplicated records
    IF i_eng_all-eqty > 1.
      i_eng_all-eqty = 1.
    ELSE.
      i_eng_all-eqty = 1.
    ENDIF.

    MODIFY i_eng_all INDEX $tabix.
    CLEAR i_eng_all.
  ENDLOOP.

* exclude AS part?
  IF i_as EQ 'E'.
    DELETE i_eng_all WHERE sleplant EQ 'AS'.
  ENDIF.


* get summary table
  CLEAR i_eng_all.
  LOOP AT i_eng_all.
    MOVE-CORRESPONDING i_eng_all TO i_engplant.
    COLLECT i_engplant.

    MOVE-CORRESPONDING i_eng_all TO i_sleplant.
    COLLECT i_sleplant.

    CLEAR: i_engplant, i_sleplant, i_eng_all.
  ENDLOOP.

ENDFUNCTION.
