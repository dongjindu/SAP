*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZGPP_PMT06GB
*   generation date: 11/26/2003 at 17:25:28 by user BOBBY
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZGPP_PMT06GB       .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
