*----------------------------------------------------------------------*
*   INCLUDE ZXCKAU00_MODULE_COLOR                                      *
*----------------------------------------------------------------------*
IF f_ckiuser-klvar = 'ZM01' AND f_ckiuser-matnr+3(2) = 'CP'.
  LOOP AT itab WHERE stgb = 'U'.
    DATA: lw_int_key(3).         " Internal Key Color
    lw_int_key = f_ckiuser-matnr+10(3).
    SELECT COUNT( * ) INTO sy-dbcnt FROM ztmm_cp_color
                                    WHERE copit   EQ f_ckiuser-matnr
                                      AND inkey EQ lw_int_key
                                      AND submt EQ itab-compn
                                      AND datab <  f_ckiuser-aldat
                                      AND datbi >= f_ckiuser-aldat.
    IF sy-subrc <> 0.
      DELETE itab.
    ENDIF.
  ENDLOOP.
ENDIF.
