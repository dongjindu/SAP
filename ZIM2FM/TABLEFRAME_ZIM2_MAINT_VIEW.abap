*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZIM2_MAINT_VIEW
*   generation date: 06/02/2005 at 20:25:03 by user ANDY
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZIM2_MAINT_VIEW    .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
