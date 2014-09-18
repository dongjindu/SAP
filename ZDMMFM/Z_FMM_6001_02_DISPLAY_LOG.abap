FUNCTION z_fmm_6001_02_display_log.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  TABLES
*"      IMT_ZTLOG STRUCTURE  ZTLOG OPTIONAL
*"----------------------------------------------------------------------
  DATA:   l_mstring(480).
  TABLES: t100.
  FIELD-SYMBOLS: <fs_ztlog> LIKE LINE OF imt_ztlog.

  LOOP AT imt_ztlog ASSIGNING <fs_ztlog>
    WHERE ba_message = space.
    SELECT SINGLE * FROM t100 WHERE sprsl = <fs_ztlog>-bb_msgspra
                              AND   arbgb = <fs_ztlog>-bb_msgid
                              AND   msgnr = <fs_ztlog>-bb_msgnr.
    IF sy-subrc = 0.
      l_mstring = t100-text.
      IF l_mstring CS '&1'.
        REPLACE '&1' WITH <fs_ztlog>-bb_msgv1 INTO l_mstring.
        REPLACE '&2' WITH <fs_ztlog>-bb_msgv2 INTO l_mstring.
        REPLACE '&3' WITH <fs_ztlog>-bb_msgv3 INTO l_mstring.
        REPLACE '&4' WITH <fs_ztlog>-bb_msgv4 INTO l_mstring.
      ELSE.
        REPLACE '&' WITH <fs_ztlog>-bb_msgv1 INTO l_mstring.
        REPLACE '&' WITH <fs_ztlog>-bb_msgv2 INTO l_mstring.
        REPLACE '&' WITH <fs_ztlog>-bb_msgv3 INTO l_mstring.
        REPLACE '&' WITH <fs_ztlog>-bb_msgv4 INTO l_mstring.
      ENDIF.
      CONDENSE l_mstring.
**/If you don't view the log, then don't comment below 4 lines.
*      WRITE: / <fs_ztlog>-logno_h, <fs_ztlog>-logno_d,
*               <fs_ztlog>-bd_dyname,
*               <fs_ztlog>-bd_dynumb, <fs_ztlog>-bd_fldname.
*      WRITE: / <fs_ztlog>-zuname,    <fs_ztlog>-zdatum,
*               <fs_ztlog>-zuzeit,    <fs_ztlog>-bb_tcode.
*      WRITE: / <fs_ztlog>-bb_msgtyp, l_mstring(250).
*    ELSE.
*      WRITE: / <fs_ztlog>.
    ENDIF.
    <fs_ztlog>-ba_message = l_mstring. CLEAR: l_mstring.
  ENDLOOP.

***********************
  it_ztlog = imt_ztlog[].
  IF it_ztlog IS INITIAL.
    MESSAGE s999(zmmm) WITH 'There is no data!'.
  ELSE.
    CALL SCREEN 9100.                   " Go to Screen 9100
  ENDIF.

ENDFUNCTION.
