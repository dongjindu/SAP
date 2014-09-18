*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZTPP_ENG_STOCK
*   generation date: 09/20/2007 at 10:01:50 by user 101457
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZTPP_ENG_STOCK     .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
