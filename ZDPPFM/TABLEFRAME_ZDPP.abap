*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZDPP
*   generation date: 01/08/2009 at 14:34:53 by user 100809
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZDPP               .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
