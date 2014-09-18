*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZGBM
*   generation date: 04/01/2004 at 10:01:18 by user BONGSOO
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZGBM               .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
