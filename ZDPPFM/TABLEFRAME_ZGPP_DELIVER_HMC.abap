*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZGPP_DELIVER_HMC
*   generation date: 08/03/2010 at 13:01:25 by user 101457
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZGPP_DELIVER_HMC   .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
