*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZGPP_ENG_DB
*   generation date: 10/16/2009 at 11:36:07 by user 101457
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZGPP_ENG_DB        .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
