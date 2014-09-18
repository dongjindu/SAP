*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZGPP_DCM_LIST
*   generation date: 09/09/2009 at 11:34:03 by user 101457
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZGPP_DCM_LIST      .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
