*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZESS
*   generation date: 07/24/2009 at 08:22:11 by user 103569
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZESS               .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
