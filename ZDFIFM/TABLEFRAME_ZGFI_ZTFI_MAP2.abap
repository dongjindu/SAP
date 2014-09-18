*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZGFI_ZTFI_MAP2
*   generation date: 11/17/2003 at 23:05:27 by user JIPARK
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZGFI_ZTFI_MAP2     .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
