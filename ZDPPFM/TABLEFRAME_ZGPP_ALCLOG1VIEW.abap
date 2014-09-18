*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZGPP_ALCLOG1VIEW
*   generation date: 12/03/2003 at 20:07:38 by user EZLOVE
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZGPP_ALCLOG1VIEW   .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
