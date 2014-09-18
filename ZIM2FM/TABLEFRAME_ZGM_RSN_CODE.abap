*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZGM_RSN_CODE
*   generation date: 09/07/2010 at 12:51:02 by user 101457
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZGM_RSN_CODE       .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
