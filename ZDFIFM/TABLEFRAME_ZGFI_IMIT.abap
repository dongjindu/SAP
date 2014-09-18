*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZGFI_IMIT
*   generation date: 10/05/2005 at 20:13:05 by user ANDY
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZGFI_IMIT          .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
