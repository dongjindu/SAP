*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZGPP_PMT05AB
*   generation date: 12/23/2003 at 21:04:38 by user SOFTNT
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZGPP_PMT05AB       .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
