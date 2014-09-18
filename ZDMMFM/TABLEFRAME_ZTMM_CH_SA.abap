*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZTMM_CH_SA
*   generation date: 06/07/2006 at 15:35:30 by user 101457
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZTMM_CH_SA         .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
