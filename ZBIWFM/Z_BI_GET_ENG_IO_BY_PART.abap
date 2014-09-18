FUNCTION z_bi_get_eng_io_by_part.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(CHECK_DATE) TYPE  SY-DATUM DEFAULT SY-DATUM
*"  TABLES
*"      I_ENG STRUCTURE  ZSBW_ENGLIST1 OPTIONAL
*"      I_IO STRUCTURE  ZSBW_ENGINOUT1 OPTIONAL
*"----------------------------------------------------------------------

  REFRESH: i_io.

  SELECT *
  FROM ztpperm
  WHERE prod_dt = check_date
    AND eqty > 0.

    MOVE-CORRESPONDING ztpperm TO i_io.
    CASE ztpperm-en_reserve_02.
      WHEN 'E001'.
        i_io-werks = 'HEA1'.
      WHEN 'E002'.
        i_io-werks = 'HEA2'.
      WHEN OTHERS.
    ENDCASE.

    CASE ztpperm-erpid.
      WHEN 'E91' OR 'E92' OR 'E93' OR 'E04'.
        i_io-zinqty = ztpperm-eqty.
      WHEN 'E01' OR 'E02' OR 'E03' OR 'E05'.
        i_io-zoutqty = ztpperm-eqty.
      WHEN OTHERS.
        CONTINUE.
    ENDCASE.

    i_io-meins = 'EA'.

    COLLECT i_io.

  ENDSELECT.

ENDFUNCTION.
