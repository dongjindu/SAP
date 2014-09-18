*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZFGMMKBN
*   generation date: 07/29/2005 at 13:43:31 by user 100471
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZFGMMKBN           .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
