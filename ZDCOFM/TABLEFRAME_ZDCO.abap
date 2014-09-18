*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZDCO
*   generation date: 07/25/2008 at 10:24:29 by user 103569
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZDCO               .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
