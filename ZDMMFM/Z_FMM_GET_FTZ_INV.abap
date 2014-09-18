FUNCTION z_fmm_get_ftz_inv .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  TABLES
*"      T_INV_HD STRUCTURE  ZTMM_FTZ_INV_HD
*"      T_INV_ITM STRUCTURE  ZTMM_FTZ_INV_ITM
*"----------------------------------------------------------------------
  TABLES: ztmm_ftz_inv_hd.
  LOOP AT t_inv_hd.
    t_inv_hd-zsdat = sy-datum.
    t_inv_hd-zstim = sy-uzeit.
    SELECT SINGLE * FROM ztmm_ftz_inv_hd
      WHERE zfcivrn = t_inv_hd-zfcivrn
        AND bukrs  = t_inv_hd-bukrs
        AND zfcivno = t_inv_hd-zfcivno
        AND zfhblno = t_inv_hd-zfhblno.
    IF sy-subrc = 0.
      INSERT ztmm_ftz_inv_log FROM t_inv_hd.
      t_inv_hd-zzret    = 'E'.
      MODIFY t_inv_hd TRANSPORTING zzret.

      DELETE t_inv_itm
         WHERE zfcivrn = t_inv_hd-zfcivrn
         AND bukrs  = t_inv_hd-bukrs
         AND zfcivno = t_inv_hd-zfcivno.
    ELSE.
      t_inv_hd-zsdat = sy-datum.
      t_inv_hd-zstim = sy-uzeit.
      t_inv_hd-zedat = sy-datum.
      t_inv_hd-zetim = sy-uzeit.
      INSERT ztmm_ftz_inv_hd FROM t_inv_hd.
      IF sy-subrc = 0.
        t_inv_hd-zzret    = 'S'.
          COMMIT WORK.
      ELSE.
        t_inv_hd-zzret    = 'E'.
        ROLLBACK WORK.
      ENDIF.
      MODIFY t_inv_hd TRANSPORTING zzret.
    ENDIF.
  ENDLOOP.

  INSERT ztmm_ftz_inv_itm FROM TABLE t_inv_itm.
*  IF SY-SUBRC = 0.
    COMMIT WORK.
*  ENDIF.

ENDFUNCTION.
