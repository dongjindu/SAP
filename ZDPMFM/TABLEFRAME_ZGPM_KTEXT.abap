*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZGPM_KTEXT
*   generation date: 05/06/2004 at 18:58:44 by user SHOXX
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZGPM_KTEXT         .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
