FUNCTION z_bi_get_inv_summary_engine.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(I_CALDAY) TYPE  SY-DATUM DEFAULT SY-DATUM
*"     VALUE(I_AS) TYPE  ZRESULT DEFAULT 'E'
*"  TABLES
*"      I_ENG_ALL STRUCTURE  ZSBIP_ENGINV_ALL
*"      I_ENGPLANT STRUCTURE  ZSBIP_ENG_BY_ENGPLANT
*"      I_SLEPLANT STRUCTURE  ZSBIP_ENG_BY_SLEPLANT
*"      I_JITPLANT STRUCTURE  ZSBIP_ENG_BY_ENGPLANT
*"----------------------------------------------------------------------

  RANGES r_model FOR zprdqtymodel-p_model .

  REFRESH: i_eng_all, i_engplant, i_sleplant, i_jitplant.

* get engine code
  SELECT matnr ladgr prctr INTO CORRESPONDING FIELDS OF TABLE i_eng_all
  FROM marc
  WHERE ( werks = 'E001' or werks = 'E002' )
*    AND mmsta = '12'
    AND sfepr = 'ENGI'
    AND fevor = 'SEA'.

  SORT i_eng_all BY matnr ladgr prctr.

  DATA: $ix LIKE sy-tabix,
        $amt LIKE mard-labst.

  CLEAR i_eng_all.
  LOOP AT i_eng_all.
    $ix = sy-tabix. $amt = 0.

    SELECT SUM( labst ) INTO $amt
    FROM mard
    WHERE matnr = i_eng_all-matnr
    GROUP by matnr.
    ENDSELECT.
* delete if stock = 0 or no records
    IF sy-subrc EQ 0.
      IF $amt EQ 0.
        DELETE i_eng_all INDEX $ix.
      ELSE.
        i_eng_all-eqty = $amt.
*get engine plant & sales plant
        CASE i_eng_all-ladgr.
          WHEN 'P500'.
            i_eng_all-sleplant = 'KMMG'.
          WHEN 'P200'.
            i_eng_all-sleplant = 'AS'.
          WHEN OTHERS.
            i_eng_all-sleplant = 'HMMA'.
        ENDCASE.

        CASE i_eng_all-prctr.
          WHEN 'MXEX' OR 'MXEZ'.
            i_eng_all-plant_cd = 'ENG2'.
          WHEN 'MXEY'.
            i_eng_all-plant_cd = 'ENG1'.
          WHEN OTHERS.
        ENDCASE.

        MODIFY i_eng_all INDEX $ix.
      ENDIF.
    ELSE.
      DELETE i_eng_all INDEX $ix.
    ENDIF.

  ENDLOOP.

  IF i_as EQ 'E'.
    DELETE i_eng_all WHERE sleplant = 'AS'.
  ENDIF.

  LOOP AT i_eng_all.
    MOVE-CORRESPONDING i_eng_all TO i_engplant.
    MOVE-CORRESPONDING i_eng_all TO I_SLEPLANT.

    COLLECT: i_engplant, I_SLEPLANT.
    CLEAR i_eng_all. CLEAR: i_engplant, I_SLEPLANT.
  ENDLOOP.
  SORT i_engplant BY plant_cd.
  SORT I_SLEPLANT BY SLEPLANT.
* Select JIT Call list for Today
  DATA: BEGIN OF lt_sa OCCURS 0,
          vbeln LIKE vbak-vbeln,
        END OF lt_sa.

  DATA: BEGIN OF lt_temp OCCURS 0,
       matnr LIKE jitma-matnr,
       exdat LIKE jitit-exdat,
       quant LIKE jitco-quant,
       datum LIKE sy-datum,
       END OF lt_temp.

  DATA: i_jitcall LIKE lt_temp OCCURS 0 WITH HEADER LINE..

  DATA: l_date_c(8),
        l_char14(14).

  REFRESH: lt_sa, i_jitcall.

  SELECT vbeln INTO TABLE lt_sa
  FROM vbak
  WHERE auart = 'ZPLZ'
    AND kunnr = 'AKNH'.

  REFRESH lt_temp.
*  SELECT MATNR EXDAT QUANT INTO TABLE LT_TEMP
  LOOP AT lt_sa.
    SELECT matnr rdate AS exdat quant INTO TABLE lt_temp
     FROM jitma AS a
    INNER JOIN jitco AS b
    ON a~matid = b~matid
     INNER JOIN jitit AS c
     ON b~posid = c~posid
     WHERE vbeln = lt_sa-vbeln
       AND c~intst = '0000'.
*       AND c~rdate = i_calday.
    APPEND LINES OF lt_temp TO i_jitcall.
*    COLLECT i_jitcall.
  ENDLOOP.

  LOOP AT i_jitcall.
    $ix = sy-tabix.
    l_char14 = i_jitcall-exdat.
    l_date_c = l_char14+0(8).
    i_jitcall-datum = l_date_c.

    MODIFY i_jitcall INDEX $ix.
  ENDLOOP.
*
  DELETE i_jitcall WHERE datum NE i_calday.

  LOOP AT i_jitcall.
    CLEAR i_eng_all.
    READ TABLE i_eng_all WITH KEY matnr = i_jitcall BINARY SEARCH.
    IF sy-subrc EQ 0.
      i_jitplant-plant_cd = i_eng_all-plant_cd.
      i_jitplant-eqty = i_jitcall-quant.
      COLLECT i_jitplant.
    ENDIF.
  ENDLOOP.



ENDFUNCTION.
