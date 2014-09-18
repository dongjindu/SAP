*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZGPP_DAY_SUM
*   generation date: 02/03/2006 at 16:51:27 by user 101457
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZGPP_DAY_SUM       .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
