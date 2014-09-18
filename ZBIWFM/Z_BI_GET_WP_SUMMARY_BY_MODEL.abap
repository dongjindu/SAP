FUNCTION z_bi_get_wp_summary_by_model.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(CHECK_DATE) TYPE  SY-DATUM DEFAULT SY-DATUM
*"     VALUE(MODEL) TYPE  ZMODEL OPTIONAL
*"  TABLES
*"      ZPRDQTYWIP STRUCTURE  ZPRDQTYWIP
*"----------------------------------------------------------------------

  RANGES r_model FOR zprdqtymodel-p_model .

  IF model IS INITIAL.
  ELSE.
    r_model = 'IEQ'.
    r_model-low = model.
    APPEND r_model.
  ENDIF.

  REFRESH: zprdqtywip.

* by model
  SELECT p_status p_model COUNT( * ) INTO TABLE zprdqtywip
  FROM ztppvr
  WHERE k04pdat EQ check_date
  AND ( p_status EQ 'B01' OR
        p_status EQ 'P02' OR
        p_status EQ 'P39' OR
        p_status EQ 'T24' OR
        p_status EQ 'T25' OR
        p_status EQ 'T26' ) "WIP
  AND zresult EQ 'S'
  AND p_model IN r_model
  AND NOT ( p_dest_code LIKE '%XX%' OR p_dest_code LIKE '%XY%' )
  GROUP by p_status p_model.

  SORT zprdqtywip BY p_status p_model.
* by nation
*  SELECT p_dest_code COUNT( * ) INTO TABLE zprdqtynation
*  FROM ztppvr
*  WHERE k04pdat EQ check_date
*  AND ( p_status EQ 'V05' OR
*        p_status EQ 'V07' )"Ship out
*  AND zresult EQ 'S'
**  AND p_model IN r_model
*  AND NOT ( p_dest_code LIKE '%XX%' OR p_dest_code LIKE '%XY%' )
*  GROUP by p_dest_code.
*

* by ig {
  DATA: it_modl_val TYPE ztbm_model_val_n OCCURS 10 WITH HEADER LINE.
  DATA $zvalue TYPE zvalue.
  DATA $ix TYPE i.
  SELECT *
       FROM ztbm_model_val_n
       INTO TABLE it_modl_val
       WHERE zfield EQ '01'.

  SORT it_modl_val BY zvalue.

  LOOP AT zprdqtywip.
    $ix = sy-tabix.
    $zvalue = zprdqtywip-p_model(2).
    READ TABLE it_modl_val WITH KEY zvalue = $zvalue BINARY SEARCH.
    IF sy-subrc EQ 0.
      zprdqtywip-p_model = it_modl_val-zvalnm(2).
    ENDIF.

    CASE zprdqtywip-p_status.
      WHEN 'B01'.
        zprdqtywip-p_status = 'BI'.
      WHEN 'P02'.
        zprdqtywip-p_status = 'PI'.
      WHEN 'P39'.
        zprdqtywip-p_status = 'TI'.
      WHEN 'T24' OR 'T25' OR 'T26'.
        zprdqtywip-p_status = 'SO'.
    ENDCASE.

    MODIFY zprdqtywip INDEX $ix.
  ENDLOOP.

  DATA: i_itab LIKE zprdqtywip OCCURS 0 WITH HEADER LINE.
  REFRESH: i_itab.

  LOOP AT zprdqtywip.
    MOVE zprdqtywip TO i_itab.
    COLLECT i_itab.
    CLEAR: zprdqtywip, i_itab.
  ENDLOOP.

  REFRESH zprdqtywip. zprdqtywip[] = i_itab[].
* }


ENDFUNCTION.
