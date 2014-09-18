*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZEBOM
*   generation date: 09/17/2007 at 16:37:15 by user 103569
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZEBOM              .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
