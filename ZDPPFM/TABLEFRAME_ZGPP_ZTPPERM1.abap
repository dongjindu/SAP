*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZGPP_ZTPPERM1
*   generation date: 08/05/2009 at 11:39:39 by user 101457
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZGPP_ZTPPERM1      .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
