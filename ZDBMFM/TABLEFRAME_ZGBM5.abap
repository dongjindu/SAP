*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZGBM5
*   generation date: 05/15/2004 at 09:26:50 by user WSKIM
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZGBM5              .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
