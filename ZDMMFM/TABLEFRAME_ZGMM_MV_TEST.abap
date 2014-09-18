*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZGMM_MV_TEST
*   generation date: 12/05/2003 at 04:49:28 by user HAKCHIN
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZGMM_MV_TEST       .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
