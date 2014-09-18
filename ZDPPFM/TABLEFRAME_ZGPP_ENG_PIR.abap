*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZGPP_ENG_PIR
*   generation date: 07/30/2007 at 16:37:36 by user 101457
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZGPP_ENG_PIR       .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
