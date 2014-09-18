*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZFTZ
*   generation date: 08/27/2008 at 16:43:46 by user 103569
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZFTZ               .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
