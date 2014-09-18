*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZGPP_PVV02AB_2
*   generation date: 10/13/2008 at 10:59:02 by user 101457
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZGPP_PVV02AB_2     .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
