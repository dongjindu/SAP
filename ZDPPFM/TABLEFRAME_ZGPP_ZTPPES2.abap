*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZGPP_ZTPPES2
*   generation date: 06/02/2008 at 14:08:21 by user 101457
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZGPP_ZTPPES2       .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
