*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZTAB_MAIN
*   generation date: 23.02.2005 at 11:00:23 by user 100565
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZTAB_MAIN          .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
