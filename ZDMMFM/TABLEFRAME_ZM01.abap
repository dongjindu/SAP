*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZM01
*   generation date: 12/13/2007 at 14:18:18 by user P00246
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZM01               .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
