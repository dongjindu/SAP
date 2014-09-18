*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZGFI_ZTFI_FMAP
*   generation date: 12/03/2003 at 16:11:13 by user ANDY
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZGFI_ZTFI_FMAP     .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
