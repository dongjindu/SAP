*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZSEC
*   generation date: 02/19/2008 at 11:38:10 by user 100809
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZSEC               .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
