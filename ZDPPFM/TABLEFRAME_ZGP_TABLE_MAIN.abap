*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZGP_TABLE_MAIN
*   generation date: 11/19/2003 at 18:42:58 by user EZLOVE
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZGP_TABLE_MAIN     .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
