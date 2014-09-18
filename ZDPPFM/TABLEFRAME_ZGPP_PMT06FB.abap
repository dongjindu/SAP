*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZGPP_PMT06FB
*   generation date: 12/25/2003 at 21:08:11 by user SOFTNT
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZGPP_PMT06FB       .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
