*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZGPP_MDM_HMC
*   generation date: 12/11/2009 at 12:52:58 by user 101457
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZGPP_MDM_HMC       .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
