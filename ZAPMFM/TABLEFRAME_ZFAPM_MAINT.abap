*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZFAPM_MAINT
*   generation date: 03/17/2014 at 09:43:07 by user HIS20088
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZFAPM_MAINT        .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
