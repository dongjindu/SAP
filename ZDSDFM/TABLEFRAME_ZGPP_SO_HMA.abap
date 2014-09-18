*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZGPP_SO_HMA
*   generation date: 03/02/2006 at 10:55:16 by user 101457
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZGPP_SO_HMA        .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
