*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZGRF
*   generation date: 07/01/2004 at 10:46:44 by user BONGSOO
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZGRF               .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
