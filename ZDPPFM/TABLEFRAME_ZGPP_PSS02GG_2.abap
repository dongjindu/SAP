*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZGPP_PSS02GG_2
*   generation date: 10/08/2008 at 11:04:19 by user 101457
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZGPP_PSS02GG_2     .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
