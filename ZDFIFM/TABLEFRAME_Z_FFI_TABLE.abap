*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_Z_FFI_TABLE
*   generation date: 09-02-2003 at 03:29:55 by user ZZANG4YOU
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_Z_FFI_TABLE        .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
