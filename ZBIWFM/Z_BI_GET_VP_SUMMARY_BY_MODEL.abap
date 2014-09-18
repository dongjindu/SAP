FUNCTION z_bi_get_vp_summary_by_model.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(CHECK_DATE) TYPE  SY-DATUM DEFAULT SY-DATUM
*"     VALUE(MODEL) TYPE  ZMODEL OPTIONAL
*"  TABLES
*"      ZVPCQTYMODEL STRUCTURE  ZVPCQTYMODEL
*"      ZVPCQTYNATION STRUCTURE  ZVPCQTYNATION
*"      ZVPCQTYSUM STRUCTURE  ZVPCQTYSUM
*"----------------------------------------------------------------------

  RANGES r_model FOR zprdqtymodel-p_model .

  IF model IS INITIAL.
  ELSE.
    r_model = 'IEQ'.
    r_model-low = model.
    APPEND r_model.
  ENDIF.

  REFRESH: zvpcqtymodel, zvpcqtynation, zvpcqtysum.

  DATA: BEGIN OF i_itab OCCURS 0,
        p_status LIKE ztppvr-p_status,
        p_model LIKE ztppvr-p_model,
        p_dest_code LIKE ztppvr-p_dest_code,
        qty TYPE i,
        END OF i_itab.

* get source
  SELECT p_status p_model p_dest_code COUNT( * ) INTO TABLE i_itab
  FROM ztppvr
  WHERE k04pdat EQ check_date
  AND ( p_status EQ 'T27' OR
        p_status EQ 'T28' OR
        p_status EQ 'V04' OR
        p_status EQ 'V06' OR
        p_status EQ 'V05' OR
        p_status EQ 'V07' )"
  AND zresult EQ 'S'
  AND p_model IN r_model
  AND NOT ( p_dest_code LIKE '%XX%' OR p_dest_code LIKE '%XY%' )
  GROUP by p_status p_model p_dest_code.

  SORT i_itab BY p_status p_model p_dest_code.

* by ig {
  DATA: it_modl_val TYPE ztbm_model_val_n OCCURS 10 WITH HEADER LINE.
  DATA $zvalue TYPE zvalue.
  DATA $ix TYPE i.
  SELECT *
       FROM ztbm_model_val_n
       INTO TABLE it_modl_val
       WHERE zfield EQ '01'.

  SORT it_modl_val BY zvalue.

  LOOP AT i_itab.
    $ix = sy-tabix.
    $zvalue = i_itab-p_model(2).
    READ TABLE it_modl_val WITH KEY zvalue = $zvalue BINARY SEARCH.
    IF sy-subrc EQ 0.
      i_itab-p_model = it_modl_val-zvalnm(2).
    ENDIF.

    CASE i_itab-p_dest_code(3).
      WHEN 'B28'.
        i_itab-p_dest_code = 'HMA'.
      WHEN 'B06'.
        i_itab-p_dest_code = 'HAC'.
      WHEN OTHERS.
        i_itab-p_dest_code = 'OTH'.
    ENDCASE.
    MODIFY i_itab INDEX $ix.
  ENDLOOP.


  LOOP AT i_itab.
    CASE i_itab-p_status.
      WHEN 'T27' OR 'T28'.
* model
        MOVE-CORRESPONDING i_itab TO zvpcqtymodel.

        zvpcqtymodel-in = i_itab-qty.
        zvpcqtymodel-diff = i_itab-qty.
        COLLECT zvpcqtymodel. CLEAR zvpcqtymodel.

* nation
        MOVE-CORRESPONDING i_itab TO zvpcqtynation.

        zvpcqtynation-in = i_itab-qty.
        zvpcqtynation-diff = i_itab-qty.
        COLLECT zvpcqtynation. CLEAR zvpcqtynation.

        zvpcqtysum-vpcin =  i_itab-qty.
        COLLECT zvpcqtysum. CLEAR zvpcqtysum.

      WHEN 'V05' OR 'V07'.
* sum
        zvpcqtysum-shipout =  i_itab-qty.
        COLLECT zvpcqtysum. CLEAR zvpcqtysum.
* model & nation
        MOVE-CORRESPONDING i_itab TO zvpcqtymodel.
        MOVE-CORRESPONDING i_itab TO zvpcqtynation.

        zvpcqtymodel-out = i_itab-qty.
        zvpcqtynation-out = i_itab-qty.

        i_itab-qty = i_itab-qty * - 1.

        zvpcqtymodel-diff = i_itab-qty.
        zvpcqtynation-diff = i_itab-qty.

        COLLECT zvpcqtymodel. CLEAR zvpcqtymodel.
        COLLECT zvpcqtynation. CLEAR zvpcqtynation.

* nation
*        MOVE-CORRESPONDING i_itab TO zvpcqtynation.

*        zvpcqtynation-out = i_itab-qty.
*        i_itab-qty = i_itab-qty * - 1.
*        zvpcqtynation-diff = i_itab-qty.
*        COLLECT zvpcqtynation. CLEAR zvpcqtynation.

*
*        zvpcqtysum-shipout =  i_itab-qty.
*        COLLECT zvpcqtysum. CLEAR zvpcqtysum.
*
      WHEN 'V04' OR 'V06'.
*
        zvpcqtysum-shipin =  i_itab-qty.
        COLLECT zvpcqtysum. CLEAR zvpcqtysum.

      WHEN OTHERS.

    ENDCASE.
    CLEAR i_itab.
  ENDLOOP.

*  LOOP AT zprdqtynation.
*    $ix = sy-tabix.
*    CASE zprdqtynation-p_dest_code(3).
*      WHEN 'B28'.
*        zprdqtynation-p_dest_code = 'HMA'.
*      WHEN 'B06'.
*        zprdqtynation-p_dest_code = 'HAC'.
*      WHEN OTHERS.
*        zprdqtynation-p_dest_code = 'OTH'.
*    ENDCASE.
*
*    MODIFY zprdqtynation INDEX $ix.
*  ENDLOOP.
* }


ENDFUNCTION.
