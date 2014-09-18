FUNCTION ZPM_DIE_MAINTENANCE.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  EXPORTING
*"     VALUE(ZZRET) TYPE  ZCHECK
*"  TABLES
*"      ZZPM_DIE_MAINT STRUCTURE  ZPM_DIE_MAINT
*"----------------------------------------------------------------------
  DATA: L_INDEX LIKE SY-TABIX.

  LOOP AT ZZPM_DIE_MAINT.
    L_INDEX = SY-TABIX.
    IF ZZPM_DIE_MAINT-ZDIE_NUMBER IS INITIAL.
      DELETE ZZPM_DIE_MAINT INDEX L_INDEX.
    ENDIF.
  ENDLOOP.

  MODIFY ZPM_DIE_MAINT FROM TABLE ZZPM_DIE_MAINT.

  IF SY-SUBRC = 0.
    ZZRET = 'S'.
  ELSE.
    ZZRET = 'E'.
  ENDIF.

ENDFUNCTION.
