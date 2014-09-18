*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZGPP_PMT03FB_N
*   generation date: 04/28/2006 at 15:28:44 by user 101457
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZGPP_PMT03FB_N     .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
