*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZGPM_LINE
*   generation date: 07.08.2003 at 23:04:43 by user SHOXX
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZGPM_LINE          .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
