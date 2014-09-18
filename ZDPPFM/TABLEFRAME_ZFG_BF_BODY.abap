*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZFG_BF_BODY
*   generation date: 09/07/2006 at 15:23:34 by user 101457
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZFG_BF_BODY        .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
