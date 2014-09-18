FUNCTION z_bi_get_rs_summary_by_model.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(CHECK_DATE) TYPE  SY-DATUM DEFAULT SY-DATUM
*"     VALUE(MODEL) TYPE  ZMODEL OPTIONAL
*"  TABLES
*"      ZPRDQTYMODEL STRUCTURE  ZPRDQTYMODEL
*"      ZPRDQTYNATION STRUCTURE  ZPRDQTYNATION
*"      ZPRDQTYENG STRUCTURE  ZPRDQTYENG
*"----------------------------------------------------------------------

  RANGES r_model FOR zprdqtymodel-p_model .

  IF model IS INITIAL.
  ELSE.
    r_model = 'IEQ'.
    r_model-low = model.
    APPEND r_model.
  ENDIF.

  REFRESH: zprdqtymodel, zprdqtynation, zprdqtyeng.

  TABLES: ausp.
  DATA: BEGIN OF i_body OCCURS 0,
         objek LIKE ausp-objek,
         p_model LIKE zprdqtymodel-p_model,
         p_dest_code LIKE zprdqtynation-p_dest_code,
         engine LIKE zprdqtyeng-engine,
         prdqty TYPE zprdqtyeng-prdqty,
        END OF i_body.

  REFRESH i_body.
* get 'B00'
  SELECT objek INTO CORRESPONDING FIELDS OF TABLE i_body
  FROM ausp
  WHERE atinn = '0000003352'
    AND klart = '002'
    AND atwrt = 'B00'.

  DATA: $ix LIKE sy-tabix.

  DATA: it_modl_val TYPE ztbm_model_val_n OCCURS 10 WITH HEADER LINE.
  DATA $zvalue TYPE zvalue.

  SELECT *
       FROM ztbm_model_val_n
       INTO TABLE it_modl_val
       WHERE zfield EQ '01'.

  SORT it_modl_val BY zvalue.

  LOOP AT i_body.
    $ix = sy-tabix.
    i_body-prdqty = 1.

* check production car
    SELECT SINGLE *
    FROM ausp
    WHERE objek = i_body-objek
      AND atinn = '0000003429'
      AND klart = '002'
      AND atwrt = 'P'.

    IF sy-subrc EQ 0.

*get model
      SELECT SINGLE atwrt INTO i_body-p_model
      FROM ausp
      WHERE objek = i_body-objek
        AND atinn = '0000002804'
        AND klart = '002'.


      $zvalue = i_body-p_model(2).
      READ TABLE it_modl_val WITH KEY zvalue = $zvalue BINARY SEARCH.
      IF sy-subrc EQ 0.
        i_body-p_model = it_modl_val-zvalnm(2).
      ENDIF.

*      CASE i_body-p_model(2).
*        WHEN 'EM'.
*          i_body-p_model = 'NF'.
*        WHEN 'IN'.
*          i_body-p_model = 'YF'.
*        WHEN 'CR'.
*          i_body-p_model = 'CM'.
*        WHEN 'TC'.
*          i_body-p_model = 'UD'.
*        WHEN OTHERS.
*      ENDCASE.

*get nation
      SELECT SINGLE atwrt INTO i_body-p_dest_code
      FROM ausp
      WHERE objek = i_body-objek
        AND atinn = '0000003364'
        AND klart = '002'.

      CASE i_body-p_dest_code.
        WHEN 'B28'.
          i_body-p_dest_code = 'HMA'.
        WHEN 'B06'.
          i_body-p_dest_code = 'HAC'.
        WHEN OTHERS.
          i_body-p_dest_code = 'OTH'.
      ENDCASE.

*get engine - 219_9
      SELECT SINGLE atwrt INTO i_body-engine
      FROM ausp
      WHERE objek = i_body-objek
        AND atinn = '0000002311'
        AND klart = '002'.

      CASE i_body-engine.
        WHEN 'B'.
          i_body-engine = 'ENG18'.
        WHEN 'S'.
          i_body-engine = 'ENG20'.
*        WHEN 'T'.
*          i_body-engine = 'ENG24'.
*        WHEN 'L'.
*          i_body-engine = 'ENG35'.
        WHEN OTHERS.
          i_body-engine = 'OTH'.
      ENDCASE.
      MODIFY i_body INDEX $ix.

    ELSE.
      DELETE i_body INDEX $ix.
    ENDIF.

    CLEAR i_body.
  ENDLOOP.

  SORT i_body BY objek p_model p_dest_code engine.

  LOOP AT i_body.
    MOVE-CORRESPONDING i_body TO zprdqtymodel.
    COLLECT zprdqtymodel. CLEAR zprdqtymodel.

    MOVE-CORRESPONDING i_body TO zprdqtynation.
    COLLECT zprdqtynation. CLEAR zprdqtynation.

    MOVE-CORRESPONDING i_body TO zprdqtyeng.
    COLLECT zprdqtyeng. CLEAR zprdqtyeng.

    CLEAR i_body.
  ENDLOOP.
* }


ENDFUNCTION.
