*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZTFI_FMAL
*   generation date: 11/02/2012 at 10:07:22 by user HIS20094
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZTFI_FMAL          .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
