*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZGPP_EWS_1
*   generation date: 05/24/2010 at 13:51:22 by user 101457
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZGPP_EWS_1         .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
