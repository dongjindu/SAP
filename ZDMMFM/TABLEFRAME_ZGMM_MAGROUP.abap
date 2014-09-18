*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZGMM_MAGROUP
*   generation date: 12/11/2008 at 14:36:04 by user 101457
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZGMM_MAGROUP       .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
