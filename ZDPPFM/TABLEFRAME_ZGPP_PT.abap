*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZGPP_PT
*   generation date: 07/07/2010 at 12:47:08 by user 101457
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZGPP_PT            .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
