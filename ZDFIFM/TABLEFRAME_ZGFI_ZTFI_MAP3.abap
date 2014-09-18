*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZGFI_ZTFI_MAP3
*   generation date: 01/09/2004 at 11:35:31 by user JIPARK
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZGFI_ZTFI_MAP3     .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
