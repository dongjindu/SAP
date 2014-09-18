*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZTPP_PMT07JB
*   generation date: 11/22/2005 at 07:30:47 by user 101457
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZTPP_PMT07JB       .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
