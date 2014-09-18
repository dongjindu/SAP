*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZGPP_MOD_BOM_HIS
*   generation date: 11/03/2010 at 14:51:30 by user 101457
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZGPP_MOD_BOM_HIS   .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
