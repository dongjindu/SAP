*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZGFI_ZTFI_MAP1
*   generation date: 04/14/2004 at 00:34:40 by user JIPARK
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZGFI_ZTFI_MAP1     .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
