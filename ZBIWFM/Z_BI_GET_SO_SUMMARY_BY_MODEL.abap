FUNCTION z_bi_get_so_summary_by_model.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(CHECK_DATE) TYPE  SY-DATUM DEFAULT SY-DATUM
*"     VALUE(MODEL) TYPE  ZMODEL OPTIONAL
*"  TABLES
*"      ZPRDQTYMODEL STRUCTURE  ZPRDQTYMODEL
*"      ZPRDQTYNATION STRUCTURE  ZPRDQTYNATION
*"----------------------------------------------------------------------

  RANGES r_model FOR zprdqtymodel-p_model .

  IF model IS INITIAL.
  ELSE.
    r_model = 'IEQ'.
    r_model-low = model.
    APPEND r_model.
  ENDIF.

  REFRESH: zprdqtymodel, zprdqtynation.

* by model
  SELECT p_model COUNT( * ) INTO TABLE zprdqtymodel
  FROM ztppvr
  WHERE k04pdat EQ check_date
  AND ( p_status EQ 'V05' OR
        p_status EQ 'V07' )"Ship out
  AND zresult EQ 'S'
  AND p_model IN r_model
  AND NOT ( p_dest_code LIKE '%XX%' OR p_dest_code LIKE '%XY%' )
  GROUP by p_model.

* by nation
  SELECT p_dest_code COUNT( * ) INTO TABLE zprdqtynation
  FROM ztppvr
  WHERE k04pdat EQ check_date
  AND ( p_status EQ 'V05' OR
        p_status EQ 'V07' )"Ship out
  AND zresult EQ 'S'
*  AND p_model IN r_model
  AND NOT ( p_dest_code LIKE '%XX%' OR p_dest_code LIKE '%XY%' )
  GROUP by p_dest_code.


* by ig {
  DATA: it_modl_val TYPE ztbm_model_val_n OCCURS 10 WITH HEADER LINE.
  DATA $zvalue TYPE zvalue.
  DATA $ix TYPE i.
  SELECT *
       FROM ztbm_model_val_n
       INTO TABLE it_modl_val
       WHERE zfield EQ '01'.

  SORT it_modl_val BY zvalue.

  LOOP AT zprdqtymodel.
    $ix = sy-tabix.
    $zvalue = zprdqtymodel-p_model(2).
    READ TABLE it_modl_val WITH KEY zvalue = $zvalue BINARY SEARCH.
    IF sy-subrc EQ 0.
      zprdqtymodel-p_model = it_modl_val-zvalnm(2).
    ENDIF.
    MODIFY zprdqtymodel INDEX $ix.
  ENDLOOP.

  LOOP AT zprdqtynation.
    $ix = sy-tabix.
    CASE zprdqtynation-p_dest_code(3).
      WHEN 'B28'.
        zprdqtynation-p_dest_code = 'HMA'.
      WHEN 'B06'.
        zprdqtynation-p_dest_code = 'HAC'.
      WHEN OTHERS.
        zprdqtynation-p_dest_code = 'OTH'.
    ENDCASE.

    MODIFY zprdqtynation INDEX $ix.
  ENDLOOP.
* }


ENDFUNCTION.
