*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZGPM_TABLE
*   generation date: 05/13/2004 at 01:56:37 by user SHOXX
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZGPM_TABLE         .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
