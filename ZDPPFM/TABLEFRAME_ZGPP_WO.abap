*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZGPP_WO
*   generation date: 01/31/2006 at 12:55:37 by user 101457
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZGPP_WO            .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
