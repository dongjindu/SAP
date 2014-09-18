*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZGPP_TEST
*   generation date: 11/20/2009 at 10:04:53 by user 101457
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZGPP_TEST          .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
