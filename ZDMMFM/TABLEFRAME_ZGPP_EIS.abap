*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZGPP_EIS
*   generation date: 05/19/2010 at 08:31:40 by user 101457
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZGPP_EIS           .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
