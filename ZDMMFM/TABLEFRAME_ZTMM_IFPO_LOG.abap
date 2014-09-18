*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZTMM_IFPO_LOG
*   generation date: 06/19/2006 at 16:59:45 by user 101457
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZTMM_IFPO_LOG      .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
