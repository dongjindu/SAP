*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZGPP_PMT07JB_B
*   generation date: 09/11/2006 at 22:13:27 by user 101457
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZGPP_PMT07JB_B     .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
