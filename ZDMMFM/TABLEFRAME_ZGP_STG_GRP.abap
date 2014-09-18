*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZGP_STG_GRP
*   generation date: 12/18/2006 at 16:29:26 by user 101457
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZGP_STG_GRP        .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
