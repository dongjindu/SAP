*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZGPP_EWS
*   generation date: 07/20/2010 at 16:20:56 by user 101457
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZGPP_EWS           .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
