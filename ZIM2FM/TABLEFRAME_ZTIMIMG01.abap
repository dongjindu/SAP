*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZTIMIMG01
*   generation date: 2002/02/18 at 11:18:46 by user ESBKANG
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZTIMIMG01          .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
