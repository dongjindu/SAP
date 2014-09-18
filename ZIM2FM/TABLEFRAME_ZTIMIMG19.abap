*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZTIMIMG19
*   generation date: 2001/11/26 at 20:49:16 by user ESBKANG
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZTIMIMG19          .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
