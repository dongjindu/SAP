*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZGPP_PMT07JB
*   generation date: 11/26/2003 at 06:04:45 by user JOKIM
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZGPP_PMT07JB       .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
