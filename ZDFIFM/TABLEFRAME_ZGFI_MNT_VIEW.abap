*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZGFI_MNT_VIEW
*   generation date: 08-12-2003 at 02:41:45 by user ZZANG4YOU
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZGFI_MNT_VIEW      .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
