*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZGFI_ZTFI_CMAP
*   generation date: 04/30/2004 at 01:28:27 by user JIPARK
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZGFI_ZTFI_CMAP     .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
