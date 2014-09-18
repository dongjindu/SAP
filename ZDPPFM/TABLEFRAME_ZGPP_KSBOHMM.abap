*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZGPP_KSBOHMM
*   generation date: 10/08/2007 at 16:19:45 by user 101457
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZGPP_KSBOHMM       .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
