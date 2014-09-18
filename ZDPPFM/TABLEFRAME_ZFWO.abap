*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZFWO
*   generation date: 02/25/2005 at 14:13:15 by user 100701
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZFWO               .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
