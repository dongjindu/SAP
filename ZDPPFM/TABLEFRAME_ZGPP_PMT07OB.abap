*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZGPP_PMT07OB
*   generation date: 12/03/2003 at 19:30:49 by user JOKIM
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZGPP_PMT07OB       .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
