*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZGMM_RISK_LEVEL
*   generation date: 09/04/2009 at 10:17:34 by user 101457
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZGMM_RISK_LEVEL    .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
