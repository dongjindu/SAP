*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZGPP_PMT01B01
*   generation date: 02/07/2006 at 13:09:42 by user 101457
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZGPP_PMT01B01      .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
