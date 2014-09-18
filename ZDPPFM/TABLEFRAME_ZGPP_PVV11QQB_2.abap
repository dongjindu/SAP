*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZGPP_PVV11QQB_2
*   generation date: 10/16/2008 at 09:13:27 by user 101457
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZGPP_PVV11QQB_2    .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
