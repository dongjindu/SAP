*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZGMM_CH_REAN_CD
*   generation date: 07-20-2003 at 19:41:27 by user HAKCHIN
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZGMM_CH_REAN_CD    .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
