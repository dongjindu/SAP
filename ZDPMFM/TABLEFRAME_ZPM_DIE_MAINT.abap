*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZPM_DIE_MAINT
*   generation date: 2007/03/13 at 13:04:05 by user 100565
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZPM_DIE_MAINT      .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
