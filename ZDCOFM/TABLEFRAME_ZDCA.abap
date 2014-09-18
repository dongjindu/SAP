*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZDCA
*   generation date: 10/01/2004 at 17:05:08 by user BAE_BS
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZDCA               .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
