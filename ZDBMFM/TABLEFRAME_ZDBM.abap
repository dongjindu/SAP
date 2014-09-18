*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZDBM
*   generation date: 09/02/2008 at 10:09:42 by user 100809
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZDBM               .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
