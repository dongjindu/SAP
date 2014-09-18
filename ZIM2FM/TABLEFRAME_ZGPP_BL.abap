*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZGPP_BL
*   generation date: 12/05/2008 at 11:27:36 by user 101457
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZGPP_BL            .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
