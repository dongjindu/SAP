*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZGFI_ZTFI_CMAD
*   generation date: 11/03/2003 at 18:17:21 by user JIPARK
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZGFI_ZTFI_CMAD     .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
