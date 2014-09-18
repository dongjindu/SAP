*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZFGMM_PR_LOG
*   generation date: 06/22/2006 at 08:09:31 by user 101457
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZFGMM_PR_LOG       .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
