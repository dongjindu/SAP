*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZGMM_MOD_DECOMP
*   generation date: 09/10/2008 at 11:07:42 by user 101457
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZGMM_MOD_DECOMP    .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
