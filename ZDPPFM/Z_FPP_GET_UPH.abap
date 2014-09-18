FUNCTION z_fpp_get_uph.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(DATE) LIKE  SY-DATUM
*"     VALUE(SHIFT) LIKE  KAPA-SCHNR OPTIONAL
*"     VALUE(SHOP) LIKE  CRHD-ARBPL DEFAULT 'T'
*"  EXPORTING
*"     REFERENCE(UPH) LIKE  ZTPP_STATUS-UPH
*"----------------------------------------------------------------------
  DATA: p_date      LIKE sy-datum,
        p_shift     LIKE kapa-schnr,
        p_shop      LIKE crhd-arbpl,
        l_shift_cnt TYPE i.

* DATA: LT_LD LIKE ZVPP_LD OCCURS 0 WITH HEADER LINE.
  DATA: BEGIN OF lt_ld OCCURS 0.
          INCLUDE STRUCTURE zvpp_ld.
  DATA:   uph LIKE ztpp_status-uph,
        END   OF lt_ld.

  DATA: lw_ld LIKE zvpp_ld.

  clear: uph.

  p_date = date.
  p_shift = shift.
  p_shop = shop.

  IF p_shift IS INITIAL .
    SELECT * INTO TABLE lt_ld
      FROM zvpp_ld
     WHERE ld_perst <= p_date
       AND ld_pered >= p_date
       AND arbpl     = p_shop     .
    IF sy-subrc NE 0.
      uph = 0.
      EXIT.
    ENDIF.

  ELSE.
    SELECT * INTO TABLE lt_ld
      FROM zvpp_ld
     WHERE ld_perst <= p_date
       AND ld_pered >= p_date
       AND ld_shift  = p_shift
       AND arbpl     = p_shop    .
    IF sy-subrc NE 0.
      uph = 0.
      EXIT.
    ENDIF.

  ENDIF.

*---<< BS Bae. 12/20/2013. Bug fix
*  LOOP AT lt_ld.
*    lw_ld-lrate = lw_ld-lrate + lt_ld-lrate.
*    lw_ld-lantu = lw_ld-lantu + lt_ld-lantu.
*  ENDLOOP.
*
*  IF lw_ld-lantu = 0.
*    uph = 0 .
*  ELSE.
*    uph = lw_ld-lrate / lw_ld-lantu .
*  ENDIF.

  LOOP AT lt_ld.
    IF lt_ld-lantu NE 0.
      uph = uph + ( lt_ld-lrate / lt_ld-lantu ).
    ENDIF.

    l_shift_cnt = l_shift_cnt + 1.
  ENDLOOP.

  IF l_shift_cnt EQ 0.
    uph = 0.
  ELSE.
    uph = uph / l_shift_cnt.
  ENDIF.
*--->> BS Bae. 12/20/2013. Bug fix
ENDFUNCTION.
