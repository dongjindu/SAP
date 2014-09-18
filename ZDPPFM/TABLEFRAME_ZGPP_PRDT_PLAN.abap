*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZGPP_PRDT_PLAN
*   generation date: 10/17/2007 at 13:55:31 by user 101457
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZGPP_PRDT_PLAN     .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
