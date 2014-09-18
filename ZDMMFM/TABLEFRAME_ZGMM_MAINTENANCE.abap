*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZGMM_MAINTENANCE
*   generation date: 05/19/2005 at 16:21:29 by user 100471
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZGMM_MAINTENANCE   .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
