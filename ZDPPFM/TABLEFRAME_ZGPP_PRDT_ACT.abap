*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZGPP_PRDT_ACT
*   generation date: 08/03/2010 at 10:34:03 by user 101457
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZGPP_PRDT_ACT      .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
