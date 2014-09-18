*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZGPP_COMM_VALS
*   generation date: 08/31/2007 at 14:47:26 by user 101457
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZGPP_COMM_VALS     .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
