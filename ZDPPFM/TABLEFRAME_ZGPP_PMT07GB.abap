*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZGPP_PMT07GB
*   generation date: 12/03/2003 at 19:36:39 by user JOKIM
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZGPP_PMT07GB       .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
