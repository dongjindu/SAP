*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZGSD_VEH_LOG
*   generation date: 09/02/2008 at 18:32:30 by user 101457
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZGSD_VEH_LOG       .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
