FUNCTION z_fmm_if_price.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  TABLES
*"      T_IF_PRICE STRUCTURE  ZTMM_IF_PRICE
************************************************************************
*  Date         Developer     Request            Description
* 02/26/2007    Manju         UD1K930873         Default values for
*                                                'KD','JIS' and 'JIT'
*                                                materials
*"----------------------------------------------------------------------
  DATA  :c_flag TYPE c.
  DATA : lt_price LIKE ztmm_if_price OCCURS 0 WITH HEADER LINE.
  CLEAR :c_flag,w_int.
  REFRESH lt_price.
*Check
  LOOP AT t_if_price.
    CLEAR c_flag.
*STATUS Field check : if the status of field is 'N', don't need update
*  table. Only send to Legacy return values 'MESSAGE'.
    IF t_if_price-use_g EQ 'N'.
      t_if_price-err_c = 'E'.
      t_if_price-msg_c = 'The record does not exist in SAP system.'.
      t_if_price-inf_d = sy-datum.
      t_if_price-inf_time = sy-timlo.
      MODIFY t_if_price FROM t_if_price.
      CONTINUE.
    ENDIF.
*vendor check
    PERFORM vendor_check USING t_if_price c_flag.
    IF c_flag EQ 'X'.
      MODIFY t_if_price FROM t_if_price.
      CONTINUE.
    ENDIF.
*Purchaing org / group / tax code Check
    PERFORM purchasing_org_check USING t_if_price  c_flag.
    IF c_flag EQ 'X'.
      MODIFY t_if_price FROM t_if_price.
      CONTINUE.
    ENDIF.
*etc check
    PERFORM etc_check USING t_if_price c_flag.
    IF c_flag EQ 'X'.
      MODIFY t_if_price FROM t_if_price.
      CONTINUE.
    ENDIF.
*Material master Check
    PERFORM material_master_check USING t_if_price c_flag.
    IF c_flag EQ 'X'.
      MODIFY t_if_price FROM t_if_price.
      CONTINUE.
    ENDIF.

**Create or Update "
*    PERFORM update_rpocessing USING t_if_price.
    t_if_price-err_c = 'S'.
*    t_if_price-msg_c = 'Executed Successfully '.
    t_if_price-inf_d = sy-datum.
    t_if_price-inf_time = sy-timlo.
    t_if_price-zresult = space.
    t_if_price-zbdat = space.
    t_if_price-zmsg = space.
    MODIFY t_if_price FROM  t_if_price.
  ENDLOOP.

*Check results : If results of same vendor,same material
*have a value 'error' in same ENTRY day
* all of data must send out 'error'
  LOOP AT t_if_price WHERE err_c EQ 'E'.

    t_if_price-err_c = 'E'.

    MODIFY t_if_price TRANSPORTING err_c
          WHERE matnr  EQ t_if_price-matnr
            AND lifnr  EQ t_if_price-lifnr
            AND isrt_d EQ t_if_price-isrt_d.
  ENDLOOP.

  LOOP AT t_if_price WHERE use_g <> 'N'.
    MOVE-CORRESPONDING t_if_price TO lt_price.
    APPEND lt_price.
    CLEAR :t_if_price,lt_price.
  ENDLOOP.

*Update results of interface : ztmm_if_price
  MODIFY ztmm_if_price  FROM TABLE lt_price.
  IF sy-subrc = 0.
    COMMIT WORK.
  ELSE.
    ROLLBACK WORK.
  ENDIF.

ENDFUNCTION.
